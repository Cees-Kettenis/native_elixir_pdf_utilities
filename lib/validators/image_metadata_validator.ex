defmodule NativeElixirPdfUtilities.Validators.ImageMetadataValidator do
  @moduledoc "Bounded orientation and resolution metadata from admitted raster image containers."
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Reads the first TIFF IFD's orientation and resolution, ignoring malformed optional metadata."
  @spec tiff(binary()) :: map()
  def tiff(bytes) do
    header =
      case bytes do
        <<"II", 42::little-16, offset::little-32, _::binary>> -> {:ok, :little, offset}
        <<"MM", 42::big-16, offset::big-32, _::binary>> -> {:ok, :big, offset}
        _ -> :error
      end

    with {:ok, endian, offset} when offset >= 8 <- header,
         {:ok, count} <- integer(bytes, offset, 2, endian),
         true <- offset + 2 + count * 12 + 4 <= byte_size(bytes) do
      HtmlValidator.reserve_render_resource(:max_entries, count, :style)
      HtmlValidator.reserve_render_resource(:max_work, 12 * count, :style)
      entries = binary_part(bytes, offset + 2, count * 12)

      values =
        for <<entry::binary-size(12) <- entries>>, reduce: %{} do
          values ->
            {:ok, tag} = integer(entry, 0, 2, endian)
            {:ok, type} = integer(entry, 2, 2, endian)
            {:ok, count} = integer(entry, 4, 4, endian)

            value =
              case {tag, type, count} do
                {tag, 3, 1} when tag in [0x112, 0x128] ->
                  integer(entry, 8, 2, endian)

                {tag, 5, 1} when tag in [0x11A, 0x11B] ->
                  with {:ok, at} <- integer(entry, 8, 4, endian),
                       {:ok, n} <- integer(bytes, at, 4, endian),
                       {:ok, d} when d > 0 <- integer(bytes, at + 4, 4, endian),
                       do: {:ok, n / d},
                       else: (_ -> :error)

                _ ->
                  :error
              end

            case value do
              {:ok, value} -> Map.put_new(values, tag, value)
              _ -> values
            end
        end

      orientation =
        case Map.get(values, 0x112) do
          n when n in 1..8 -> %{exif_orientation: n}
          _ -> %{}
        end

      unit =
        case Map.get(values, 0x128) do
          2 -> 1 / 96
          3 -> 2.54 / 96
          _ -> nil
        end

      resolution =
        case {unit, Map.get(values, 0x11A), Map.get(values, 0x11B)} do
          {unit, x, y}
          when is_number(unit) and is_number(x) and x > 0 and is_number(y) and y > 0 ->
            %{resolution_densities: {x * unit, y * unit}}

          _ ->
            %{}
        end

      Map.merge(orientation, resolution)
    else
      _ -> %{}
    end
  end

  @doc "Reads optional JFIF absolute density units without treating aspect-only units as a resolution."
  @spec jfif(binary()) :: map()
  def jfif(bytes) do
    case bytes do
      <<"JFIF", 0, _major, _minor, unit, x::16, y::16, _::binary>>
      when unit in [1, 2] and x > 0 and y > 0 ->
        scale = if unit == 1, do: 1 / 96, else: 2.54 / 96
        %{resolution_densities: {x * scale, y * scale}}

      _ ->
        %{}
    end
  end

  @doc "Reads the PNG pHYs pixels-per-metre resolution when its unit is absolute."
  @spec png_resolution(binary()) :: map()
  def png_resolution(bytes) do
    case bytes do
      <<x::32, y::32, 1>> when x > 0 and y > 0 ->
        %{resolution_densities: {x * 0.0254 / 96, y * 0.0254 / 96}}

      _ ->
        %{}
    end
  end

  defp integer(bytes, offset, size, endian) do
    # TIFF offsets are unsigned; callers use fixed two- or four-byte fields.
    if offset + size <= byte_size(bytes),
      do: {:ok, :binary.decode_unsigned(binary_part(bytes, offset, size), endian)},
      else: :error
  end
end
