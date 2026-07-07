defmodule NativeElixirPdfUtilities.HtmlToPdf.Font do
  @moduledoc """
  Font loading, fallback resolution, text measurement, and PDF text encoding.

  The renderer keeps font discovery explicit. Embedded fonts must be provided
  with the `:fonts` render option; no system font lookup is performed.
  """

  @type font_style :: :normal | :italic
  @type registry :: %{embedded: [embedded_font()]}
  @type built_in_font :: %{type: :built_in, family: String.t(), pdf_name: String.t()}
  @type embedded_font :: %{
          type: :embedded,
          family: String.t(),
          weight: number(),
          style: font_style(),
          id: String.t(),
          pdf_name: String.t(),
          data: binary(),
          units_per_em: pos_integer(),
          widths: [non_neg_integer()],
          default_width: non_neg_integer(),
          cmap: %{optional(non_neg_integer()) => non_neg_integer()},
          ascent: integer(),
          descent: integer(),
          bbox: {integer(), integer(), integer(), integer()}
        }
  @type font_face :: built_in_font() | embedded_font()

  @built_in_families ["Courier", "Helvetica", "Times-Roman"]

  @doc """
  Loads explicit TTF font options into a registry.

  Accepted font entries are maps, keyword lists, or `{family, path}` tuples. Map
  and keyword entries must include `:family` and `:path`; `:weight` and `:style`
  are optional.
  """
  @spec load_registry(keyword()) :: {:ok, registry()} | :error
  def load_registry(opts) do
    case Keyword.get(opts, :fonts, []) do
      fonts when is_list(fonts) ->
        Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, acc} ->
          case load_font(font) do
            {:ok, loaded} -> {:cont, {:ok, acc ++ [loaded]}}
            :error -> {:halt, :error}
          end
        end)
        |> case do
          {:ok, embedded} -> {:ok, %{embedded: embedded}}
          :error -> :error
        end

      _ ->
        :error
    end
  end

  @doc """
  Resolves a CSS font-family value or fallback list to a supported font face.
  """
  @spec resolve(String.t() | [String.t()], number(), font_style(), registry()) ::
          {:ok, [String.t()], font_face()} | :error
  def resolve(family_value, weight, style, registry) do
    families = font_families(family_value)

    case Enum.find_value(families, &resolve_family(&1, weight, style, registry)) do
      nil -> :error
      font -> {:ok, families, font}
    end
  end

  @doc """
  Returns the PDF resource key for a selected font face.
  """
  @spec pdf_name(font_face()) :: String.t()
  def pdf_name(font) do
    case font do
      %{type: :built_in, pdf_name: pdf_name} -> pdf_name
      %{type: :embedded, id: id} -> "Embedded-" <> id
    end
  end

  @doc """
  Measures text in PDF points for the selected font and size.
  """
  @spec text_width(String.t(), map(), number()) :: number()
  def text_width(text, font, font_size) do
    case font do
      %{type: :embedded, units_per_em: units_per_em} ->
        text
        |> String.to_charlist()
        |> Enum.reduce(0, fn codepoint, acc ->
          glyph_id = Map.get(font.cmap, codepoint, 0)
          acc + glyph_width(font, glyph_id)
        end)
        |> Kernel./(units_per_em)
        |> Kernel.*(font_size)

      _ ->
        text
        |> String.length()
        |> Kernel.*(font_size)
        |> Kernel.*(0.6)
    end
  end

  @doc """
  Encodes text for an embedded Type0 font content stream.
  """
  @spec encode_embedded_text(String.t(), embedded_font()) :: String.t()
  def encode_embedded_text(text, font) do
    text
    |> String.to_charlist()
    |> Enum.map_join("", fn codepoint ->
      glyph_id = Map.get(font.cmap, codepoint, 0)
      <<glyph_id::16>> |> Base.encode16(case: :upper)
    end)
  end

  @doc """
  Builds CID-to-Unicode mappings for all text shown with an embedded font.
  """
  @spec unicode_mappings([String.t()], embedded_font()) :: %{
          optional(non_neg_integer()) => non_neg_integer()
        }
  def unicode_mappings(texts, font) do
    Enum.reduce(texts, %{}, fn text, acc ->
      text
      |> String.to_charlist()
      |> Enum.reduce(acc, fn codepoint, mappings ->
        glyph_id = Map.get(font.cmap, codepoint, 0)

        case glyph_id do
          0 -> mappings
          glyph_id -> Map.put_new(mappings, glyph_id, codepoint)
        end
      end)
    end)
  end

  defp load_font(font) do
    with {:ok, family, path, weight, style} <- font_config(font),
         {:ok, data} <- File.read(path),
         {:ok, parsed} <- parse_ttf(data) do
      hash =
        :crypto.hash(:sha256, [family, data])
        |> Base.encode16(case: :lower)
        |> binary_part(0, 12)

      {:ok,
       parsed
       |> Map.merge(%{
         type: :embedded,
         family: family,
         weight: weight,
         style: style,
         id: hash,
         pdf_name: pdf_safe_name(family) <> "-" <> hash,
         data: data
       })}
    else
      _ -> :error
    end
  end

  defp font_config(font) do
    case font do
      {family, path} when is_binary(family) and is_binary(path) ->
        {:ok, family, path, 400, :normal}

      font when is_list(font) ->
        font_config(Map.new(font))

      font when is_map(font) ->
        family = Map.get(font, :family) || Map.get(font, "family")
        path = Map.get(font, :path) || Map.get(font, "path")
        weight = Map.get(font, :weight) || Map.get(font, "weight") || 400
        style = Map.get(font, :style) || Map.get(font, "style") || :normal

        with true <- is_binary(family) and String.trim(family) != "",
             true <- is_binary(path) and String.trim(path) != "",
             {:ok, weight} <- font_weight(weight),
             {:ok, style} <- font_style(style) do
          {:ok, String.trim(family), path, weight, style}
        end

      _ ->
        :error
    end
  end

  defp font_weight(weight) do
    case weight do
      weight when is_integer(weight) and weight >= 100 and weight <= 900 ->
        {:ok, weight}

      weight when is_float(weight) and weight >= 100 and weight <= 900 ->
        {:ok, weight}

      "normal" ->
        {:ok, 400}

      "bold" ->
        {:ok, 700}

      weight when is_binary(weight) ->
        weight |> String.trim() |> Integer.parse() |> parsed_weight()

      _ ->
        :error
    end
  end

  defp parsed_weight({weight, ""}) when weight >= 100 and weight <= 900, do: {:ok, weight}
  defp parsed_weight(_parsed), do: :error

  defp font_style(style) do
    case style do
      :normal -> {:ok, :normal}
      :italic -> {:ok, :italic}
      "normal" -> {:ok, :normal}
      "italic" -> {:ok, :italic}
      _ -> :error
    end
  end

  defp font_families(family_value) do
    case family_value do
      families when is_list(families) ->
        families
        |> Enum.filter(&is_binary/1)
        |> Enum.map(&normalize_family/1)
        |> Enum.reject(&(&1 == ""))

      family when is_binary(family) ->
        family
        |> String.split(",", trim: true)
        |> Enum.map(&normalize_family/1)
        |> Enum.reject(&(&1 == ""))

      _ ->
        []
    end
  end

  defp normalize_family(family) do
    family
    |> String.trim()
    |> String.trim("\"")
    |> String.trim("'")
    |> generic_family()
  end

  defp generic_family(family) do
    case String.downcase(family) do
      "sans-serif" -> "Helvetica"
      "serif" -> "Times-Roman"
      "monospace" -> "Courier"
      _ -> family
    end
  end

  defp resolve_family(family, weight, style, registry) do
    cond do
      family in @built_in_families ->
        built_in_font(family, weight, style)

      true ->
        embedded_family(family, weight, style, registry)
    end
  end

  defp built_in_font(family, weight, style) do
    pdf_name =
      case {family, weight >= 700, style} do
        {"Helvetica", true, :italic} -> "Helvetica-BoldOblique"
        {"Helvetica", true, _} -> "Helvetica-Bold"
        {"Helvetica", false, :italic} -> "Helvetica-Oblique"
        {"Helvetica", false, _} -> "Helvetica"
        {"Courier", true, :italic} -> "Courier-BoldOblique"
        {"Courier", true, _} -> "Courier-Bold"
        {"Courier", false, :italic} -> "Courier-Oblique"
        {"Courier", false, _} -> "Courier"
        {"Times-Roman", true, :italic} -> "Times-BoldItalic"
        {"Times-Roman", true, _} -> "Times-Bold"
        {"Times-Roman", false, :italic} -> "Times-Italic"
        {"Times-Roman", false, _} -> "Times-Roman"
      end

    %{type: :built_in, family: family, pdf_name: pdf_name}
  end

  defp embedded_family(family, weight, style, registry) do
    registry.embedded
    |> Enum.filter(&(&1.family == family))
    |> case do
      [] ->
        nil

      fonts ->
        Enum.min_by(fonts, fn font ->
          style_penalty = if font.style == style, do: 0, else: 1_000
          abs(font.weight - weight) + style_penalty
        end)
    end
  end

  defp parse_ttf(data) do
    with {:ok, tables} <- table_directory(data),
         {:ok, head} <- table(data, tables, "head"),
         {:ok, hhea} <- table(data, tables, "hhea"),
         {:ok, maxp} <- table(data, tables, "maxp"),
         {:ok, hmtx} <- table(data, tables, "hmtx"),
         {:ok, cmap} <- table(data, tables, "cmap"),
         {:ok, units_per_em, bbox} <- parse_head(head),
         {:ok, ascent, descent, hmetric_count} <- parse_hhea(hhea),
         {:ok, glyph_count} <- read_u16(maxp, 4),
         {:ok, widths} <- parse_hmtx(hmtx, glyph_count, hmetric_count),
         {:ok, cmap} <- parse_cmap(cmap) do
      {:ok,
       %{
         units_per_em: units_per_em,
         widths: widths,
         default_width: List.last(widths) || 600,
         cmap: cmap,
         ascent: ascent,
         descent: descent,
         bbox: bbox
       }}
    end
  end

  defp table_directory(data) do
    with {:ok, scaler_type} <- read_u32(data, 0),
         true <- scaler_type in [0x0001_0000, 0x7472_7565],
         {:ok, table_count} <- read_u16(data, 4),
         true <- byte_size(data) >= 12 + table_count * 16 do
      records =
        0..(table_count - 1)
        |> Enum.reduce(%{}, fn index, acc ->
          offset = 12 + index * 16
          tag = binary_part(data, offset, 4)
          <<_checksum::32, table_offset::32, length::32>> = binary_part(data, offset + 4, 12)
          Map.put(acc, tag, {table_offset, length})
        end)

      {:ok, records}
    else
      _ -> :error
    end
  end

  defp table(data, tables, tag) do
    case Map.get(tables, tag) do
      {offset, length} when offset >= 0 and length >= 0 and byte_size(data) >= offset + length ->
        {:ok, binary_part(data, offset, length)}

      _ ->
        :error
    end
  end

  defp parse_head(head) do
    with {:ok, units_per_em} <- read_u16(head, 18),
         {:ok, x_min} <- read_i16(head, 36),
         {:ok, y_min} <- read_i16(head, 38),
         {:ok, x_max} <- read_i16(head, 40),
         {:ok, y_max} <- read_i16(head, 42),
         true <- units_per_em > 0 do
      {:ok, units_per_em, {x_min, y_min, x_max, y_max}}
    else
      _ -> :error
    end
  end

  defp parse_hhea(hhea) do
    with {:ok, ascent} <- read_i16(hhea, 4),
         {:ok, descent} <- read_i16(hhea, 6),
         {:ok, hmetric_count} <- read_u16(hhea, 34),
         true <- hmetric_count > 0 do
      {:ok, ascent, descent, hmetric_count}
    else
      _ -> :error
    end
  end

  defp parse_hmtx(hmtx, glyph_count, hmetric_count) do
    cond do
      glyph_count <= 0 or hmetric_count <= 0 or byte_size(hmtx) < hmetric_count * 4 ->
        :error

      true ->
        metric_widths =
          0..(hmetric_count - 1)
          |> Enum.map(fn index ->
            {:ok, width} = read_u16(hmtx, index * 4)
            width
          end)

        last_width = List.last(metric_widths)
        extra_count = max(glyph_count - hmetric_count, 0)
        {:ok, metric_widths ++ List.duplicate(last_width, extra_count)}
    end
  end

  defp parse_cmap(cmap) do
    with {:ok, subtable_offsets} <- cmap_subtable_offsets(cmap) do
      subtable_offsets
      |> Enum.map(&parse_cmap_subtable(cmap, &1))
      |> Enum.find(&match?({:ok, _map}, &1))
      |> case do
        {:ok, map} when map_size(map) > 0 -> {:ok, map}
        _ -> :error
      end
    end
  end

  defp cmap_subtable_offsets(cmap) do
    with {:ok, count} <- read_u16(cmap, 2),
         true <- byte_size(cmap) >= 4 + count * 8 do
      offsets =
        0..(count - 1)
        |> Enum.map(fn index ->
          offset = 4 + index * 8
          {:ok, platform_id} = read_u16(cmap, offset)
          {:ok, encoding_id} = read_u16(cmap, offset + 2)
          {:ok, subtable_offset} = read_u32(cmap, offset + 4)
          {platform_id, encoding_id, subtable_offset}
        end)
        |> Enum.sort_by(fn {platform_id, encoding_id, _offset} ->
          cond do
            platform_id == 3 and encoding_id == 10 -> 0
            platform_id == 3 and encoding_id == 1 -> 1
            platform_id == 0 -> 2
            true -> 3
          end
        end)
        |> Enum.map(fn {_platform_id, _encoding_id, offset} -> offset end)

      {:ok, offsets}
    else
      _ -> :error
    end
  end

  defp parse_cmap_subtable(cmap, offset) do
    with true <- byte_size(cmap) >= offset + 2,
         {:ok, format} <- read_u16(cmap, offset) do
      case format do
        4 -> parse_cmap_format4(cmap, offset)
        _ -> :error
      end
    else
      _ -> :error
    end
  end

  defp parse_cmap_format4(cmap, offset) do
    with {:ok, length} <- read_u16(cmap, offset + 2),
         true <- byte_size(cmap) >= offset + length,
         {:ok, seg_count_x2} <- read_u16(cmap, offset + 6),
         true <- seg_count_x2 > 0 and rem(seg_count_x2, 2) == 0 do
      seg_count = div(seg_count_x2, 2)
      end_codes_offset = offset + 14
      start_codes_offset = end_codes_offset + seg_count * 2 + 2
      id_deltas_offset = start_codes_offset + seg_count * 2
      id_range_offsets_offset = id_deltas_offset + seg_count * 2

      0..(seg_count - 1)
      |> Enum.reduce_while({:ok, %{}}, fn index, {:ok, acc} ->
        with {:ok, end_code} <- read_u16(cmap, end_codes_offset + index * 2),
             {:ok, start_code} <- read_u16(cmap, start_codes_offset + index * 2),
             {:ok, id_delta} <- read_i16(cmap, id_deltas_offset + index * 2),
             {:ok, range_offset} <- read_u16(cmap, id_range_offsets_offset + index * 2),
             true <- start_code <= end_code do
          mappings =
            start_code..end_code
            |> Enum.reject(&(&1 == 0xFFFF))
            |> Enum.reduce(acc, fn codepoint, mappings ->
              glyph_id =
                cmap_format4_glyph_id(
                  cmap,
                  codepoint,
                  start_code,
                  id_delta,
                  range_offset,
                  id_range_offsets_offset + index * 2
                )

              case glyph_id do
                glyph_id when is_integer(glyph_id) and glyph_id > 0 ->
                  Map.put(mappings, codepoint, glyph_id)

                _ ->
                  mappings
              end
            end)

          {:cont, {:ok, mappings}}
        else
          _ -> {:halt, :error}
        end
      end)
    else
      _ -> :error
    end
  end

  defp cmap_format4_glyph_id(
         cmap,
         codepoint,
         start_code,
         id_delta,
         range_offset,
         range_word_offset
       ) do
    case range_offset do
      0 ->
        rem(codepoint + id_delta, 65_536)

      range_offset ->
        glyph_offset = range_word_offset + range_offset + 2 * (codepoint - start_code)

        case read_u16(cmap, glyph_offset) do
          {:ok, 0} -> 0
          {:ok, glyph_id} -> rem(glyph_id + id_delta, 65_536)
          :error -> 0
        end
    end
  end

  defp glyph_width(font, glyph_id) do
    Enum.at(font.widths, glyph_id, font.default_width)
  end

  defp pdf_safe_name(name) do
    name
    |> String.replace(~r/[^A-Za-z0-9_-]/u, "")
    |> case do
      "" -> "EmbeddedFont"
      safe -> safe
    end
  end

  defp read_u16(data, offset) do
    case byte_size(data) >= offset + 2 do
      true ->
        <<value::16>> = binary_part(data, offset, 2)
        {:ok, value}

      false ->
        :error
    end
  end

  defp read_i16(data, offset) do
    case byte_size(data) >= offset + 2 do
      true ->
        <<value::signed-16>> = binary_part(data, offset, 2)
        {:ok, value}

      false ->
        :error
    end
  end

  defp read_u32(data, offset) do
    case byte_size(data) >= offset + 4 do
      true ->
        <<value::32>> = binary_part(data, offset, 4)
        {:ok, value}

      false ->
        :error
    end
  end
end
