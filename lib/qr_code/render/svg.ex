defmodule QRCode.Render.Svg do
  @moduledoc """
  Create SVG structure with settings.
  """

  alias MatrixReloaded.Matrix
  alias QRCode.QR
  alias QRCode.Render.SvgSettings

  @type t :: %__MODULE__{
          xmlns: String.t(),
          xlink: String.t(),
          width: ExMaybe.t(integer),
          height: ExMaybe.t(integer),
          body: String.t(),
          rank_matrix: ExMaybe.t(pos_integer),
          ecc_level: Atom.t()
        }

  defstruct xmlns: "http://www.w3.org/2000/svg",
            xlink: "http://www.w3.org/1999/xlink",
            width: nil,
            height: nil,
            body: nil,
            rank_matrix: nil,
            ecc_level: nil

  @doc """
  Create Svg structure from QR matrix as binary. This binary contains svg
  attributes and svg elements.
  """
  @spec create(Result.t(String.t(), QR.t()), SvgSettings.t()) :: Result.t(String.t(), binary())
  def create({:ok, %QR{} = qr}, settings) do
    qr
    |> create_svg(settings)
    |> Result.ok()
  end

  def create(error, _settings), do: error

  # Private

  defp create_svg(%QR{matrix: matrix, ecc_level: ecc_level}, settings) do
    %__MODULE__{ecc_level: ecc_level}
    |> construct_body(matrix, settings)
    |> construct_svg(settings, ecc_level)
  end

  defp construct_body(svg, matrix, %SvgSettings{scale: scale, image: image}) do
    {rank_matrix, _} = Matrix.size(matrix)
    size = rank_matrix * scale
    quiet_zone = trunc(size * 0.1)

    %{
      svg
      | body:
          matrix
          |> maybe_zero_elements_behind_image(image, rank_matrix, svg.ecc_level)
          |> find_nonzero_element()
          |> Enum.map(&create_rect(&1, scale, quiet_zone)),
        rank_matrix: rank_matrix
    }
  end

  defp construct_svg(
         %__MODULE__{
           xmlns: xmlns,
           xlink: xlink,
           body: body,
           rank_matrix: rank_matrix
         },
         %SvgSettings{
           background_opacity: background_opacity,
           background_color: background_color,
           image: image,
           qrcode_color: qrcode_color,
           scale: scale,
           structure: structure
         },
         ecc_level
       ) do
    size = rank_matrix * scale
    quiet_zone = trunc(size * 0.1)

    {:svg,
     %{
       xmlns: xmlns,
       xlink: xlink,
       width: size + quiet_zone * 2,
       height: size + quiet_zone * 2
     },
     [
       quiet_zone(),
       background_rect(background_color, background_opacity, quiet_zone),
       to_group(body, qrcode_color),
       put_image(image, rank_matrix, scale, ecc_level)
     ]}
    |> XmlBuilder.generate(format: format(structure))
  end

  # Helpers

  defp create_rect({x_pos, y_pos}, scale, quiet_zone) do
    {:rect,
     %{width: scale, height: scale, x: scale * x_pos + quiet_zone, y: scale * y_pos + quiet_zone},
     nil}
  end

  defp quiet_zone do
    {:rect, %{width: "100%", height: "100%", fill: "#ffffff"}, nil}
  end

  defp background_settings(color, quiet_zone) do
    %{
      width: quiet_zone * 10,
      height: quiet_zone * 10,
      x: quiet_zone,
      y: quiet_zone,
      fill: to_hex(color)
    }
  end

  defp background_rect(color, nil, quiet_zone) do
    {:rect, background_settings(color, quiet_zone), nil}
  end

  defp background_rect(color, opacity, quiet_zone)
       when 0.0 <= opacity and opacity <= 1.0 do
    bg_settings =
      background_settings(color, quiet_zone)
      |> Map.put(:"fill-opacity", opacity)

    {:rect, bg_settings, nil}
  end

  defp to_group(body, color) do
    {:g, %{fill: to_hex(color)}, body}
  end

  defp put_image(nil, _, _, _), do: ""

  defp put_image({path_to_image, fit}, rank_matrix, scale, ecc_level)
       when is_binary(path_to_image) and is_atom(fit) do
    image_width = ideal_image_block_width(rank_matrix, ecc_level)

    image_width =
      case fit do
        :fill -> image_width
        :inset -> image_width - 2
      end

    image_size = image_width * scale

    {:image,
     %{
       href: encode_embedded_image(path_to_image),
       height: image_size,
       width: image_size,
       x: "50%",
       y: "50%",
       transform: "translate(-#{image_size / 2}, -#{image_size / 2})"
     }, nil}
  end

  defp ideal_image_block_width(rank_matrix, ecc_level) do
    area = :math.pow(rank_matrix, 2)
    # don't include the three 9x9 finder patterns or required patterns in the calculation
    area = area - 9 * 9 * 3 - rank_matrix * 2

    # The percentages of the QR code that can be used for image data
    # Based on the error correction levels, adjusted by trial and error
    # low: 7%, medium: 15%, quartile: 25%, high: 30%
    [low: 0.05, medium: 0.10, quartile: 0.16, high: 0.18]

    # [low: 0.05, medium: 0.10, quartile: 0.16, high: 0.12]
    |> Enum.map(fn {level, percentage} ->
      # calculate the max size of the image that does not exceed the error correction
      size = :math.sqrt(area * percentage) |> :math.floor() |> trunc()
      # ensure the size is odd
      size = if rem(size, 2) == 0, do: max(1, size - 1), else: size
      {level, size}
    end)
    |> List.keyfind(ecc_level, 0)
    |> elem(1)
  end

  defp encode_embedded_image(path_to_image) do
    encoded_image =
      path_to_image
      |> File.read!()
      |> Base.encode64()

    [_image_name, image_type] =
      path_to_image
      |> Path.basename()
      |> String.split(".")

    "data:image/#{mime_type(image_type)}; base64, #{encoded_image}"
  end

  defp mime_type("png"), do: "png"
  defp mime_type("svg"), do: "svg+xml"

  defp mime_type(type) when type in ["jpg", "jpeg"] do
    "jpeg"
  end

  defp mime_type(_type), do: raise(ArgumentError, "Bad embedded image format!")

  # If an image exists, zero out all elements that will be behind the image
  defp maybe_zero_elements_behind_image(matrix, nil, _, _), do: matrix

  defp maybe_zero_elements_behind_image(matrix, _, rank_matrix, ecc_level) do
    image_block_width = ideal_image_block_width(rank_matrix, ecc_level)
    inset = (rank_matrix - image_block_width) / 2

    for {row, y} <- Enum.with_index(matrix),
        do:
          for(
            {element, x} <- Enum.with_index(row),
            do:
              if x >= inset and x <= rank_matrix - inset - 1 and
                   y >= inset and y <= rank_matrix - inset - 1 do
                0
              else
                element
              end
          )
  end

  defp find_nonzero_element(matrix) do
    for {row, i} <- Enum.with_index(matrix),
        {element, j} <- Enum.with_index(row),
        element == 1,
        do: {i, j}
  end

  defp format(:minify), do: :none
  defp format(:readable), do: :indent

  defp to_hex({r, g, b} = color) when is_tuple(color) do
    "##{encode_color(r)}#{encode_color(g)}#{encode_color(b)}"
  end

  defp to_hex(color) do
    color
  end

  defp encode_color(c) when is_integer(c) and 0 <= c and c <= 255 do
    c |> :binary.encode_unsigned() |> Base.encode16()
  end
end
