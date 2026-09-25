defmodule NativeElixirPdfUtilities.HtmlToPdf.Font do
  @moduledoc """
  Font loading, fallback resolution, text measurement, and PDF text encoding.

  The renderer loads explicitly configured fonts, discovers installed fonts on
  demand, and keeps the bundled DejaVu Sans faces as the final fallback.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.FontCache
  alias NativeElixirPdfUtilities.HtmlToPdf.SystemFontCache
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type font_style :: :normal | :italic
  @type registry :: %{
          embedded: [embedded_font()],
          fallback: [embedded_font()],
          system_font_discovery: boolean()
        }
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
          kerning: %{optional({non_neg_integer(), non_neg_integer()}) => integer()},
          ascent: integer(),
          descent: integer(),
          line_gap: integer(),
          bbox: {integer(), integer(), integer(), integer()},
          embedding_flags: non_neg_integer(),
          variable_font?: boolean(),
          source: :configured | :bundled | :system
        }
  @typedoc "A document-scoped mapping from Unicode code points to PDF CIDs and font glyphs."
  @type pdf_encoding :: %{
          required(:codepoint_to_cid) => %{optional(non_neg_integer()) => pos_integer()},
          required(:cid_to_gid) => %{optional(pos_integer()) => non_neg_integer()},
          required(:cid_to_unicode) => %{optional(pos_integer()) => non_neg_integer()}
        }
  @type font_face :: built_in_font() | embedded_font()

  @bundled_font_family "DejaVu Sans"

  @doc false
  @spec normalize_options(term()) ::
          {:ok, keyword()} | :error | {:error, {:resource_limit_exceeded, map()}}
  def normalize_options(opts) do
    case Keyword.keyword?(opts) do
      true ->
        case normalize_configs(Keyword.get(opts, :fonts, [])) do
          {:ok, fonts} -> {:ok, Keyword.put(opts, :fonts, fonts)}
          {:error, {_reason, _diagnostic}} = error -> error
          :error -> :error
        end

      false ->
        :error
    end
  end

  @doc false
  @spec normalize_configs(term()) ::
          {:ok, [map()]} | :error | {:error, {:resource_limit_exceeded, map()}}
  def normalize_configs(fonts) do
    FontValidator.with_budget(fn ->
      case is_list(fonts) do
        true ->
          FontValidator.check(:max_font_count, length(fonts))

          Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, prepared} ->
            case normalize_config(font) do
              {:ok, normalized} -> {:cont, {:ok, prepared ++ [normalized]}}
              :error -> {:halt, :error}
            end
          end)

        false ->
          :error
      end
    end)
  end

  @doc """
  Loads explicit TTF font options into a registry.

  Accepted font entries are maps, keyword lists, or `{family, path}` tuples. Map
  and keyword entries must include `:family` and one or more `:path` or `:data`
  candidates; `:weight` and `:style` are optional.
  """
  @spec load_registry(keyword()) ::
          {:ok, registry()}
          | :error
          | {:error, {:invalid_document | :resource_limit_exceeded, map()}}
  def load_registry(opts) do
    FontValidator.with_budget(fn ->
      with {:ok, prepared_opts} <- normalize_options(opts),
           :ok <- HtmlValidator.validate_font_configs(Keyword.fetch!(prepared_opts, :fonts)),
           {:ok, bundled_configs} <- normalize_configs(bundled_font_configs()),
           :ok <- HtmlValidator.validate_font_configs(bundled_configs),
           {:ok, configured} <- load_fonts(Keyword.fetch!(prepared_opts, :fonts), :configured),
           {:ok, bundled} <- load_fonts(bundled_configs, :bundled) do
        fallback = configured ++ bundled

        {:ok,
         %{
           embedded: Enum.uniq_by(fallback, &font_key/1),
           fallback: fallback,
           system_font_discovery: Keyword.get(prepared_opts, :system_font_discovery, true)
         }}
      else
        {:error, {_reason, _diagnostic}} = error -> error
        _ -> :error
      end
    end)
  end

  @doc """
  Resolves a CSS font-family value or fallback list to a supported font face.
  """
  @spec resolve(String.t() | [String.t()], number(), font_style(), registry()) ::
          {:ok, [String.t()], font_face()} | :error | {:error, {:resource_limit_exceeded, map()}}
  def resolve(family_value, weight, style, registry) do
    FontValidator.with_budget(fn ->
      families = font_families(family_value)

      case List.first(requested_faces(families, weight, style, registry)) ||
             embedded_family(@bundled_font_family, weight, style, registry) do
        nil -> :error
        font -> {:ok, families, font}
      end
    end)
  end

  @doc false
  @spec requested_faces(String.t() | [String.t()], number(), font_style(), registry()) ::
          [embedded_font()] | {:error, {:resource_limit_exceeded, map()}}
  def requested_faces(family_value, weight, style, registry) do
    FontValidator.with_budget(fn ->
      family_value
      |> font_families()
      |> Enum.map(&resolve_family(&1, weight, style, registry))
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq_by(&pdf_name/1)
    end)
  end

  @doc """
  Returns configured and bundled fallback faces in family declaration order,
  ordering each family's faces by the closest weight and style.
  """
  @spec fallback_faces(registry(), number(), font_style()) :: [embedded_font()]
  def fallback_faces(registry, weight, style) do
    registry
    |> Map.get(:fallback, [])
    |> Enum.group_by(&String.downcase(&1.family))
    |> then(fn grouped ->
      registry
      |> Map.get(:fallback, [])
      |> Enum.map(&String.downcase(&1.family))
      |> Enum.uniq()
      |> Enum.flat_map(fn family ->
        grouped
        |> Map.fetch!(family)
        |> Enum.sort_by(fn font ->
          style_penalty = if font.style == style, do: 0, else: 1_000
          abs(font.weight - weight) + style_penalty
        end)
      end)
    end)
  end

  @doc """
  Returns whether a font face can safely encode every codepoint in `text`.

  Built-in PDF fonts are limited to printable ASCII. Embedded fonts are
  checked against their Unicode character map.
  """
  @spec supports_text?(font_face(), String.t()) :: boolean()
  def supports_text?(font, text) do
    case {font, text} do
      {%{type: :built_in}, text} when is_binary(text) ->
        String.valid?(text) and
          text
          |> String.to_charlist()
          |> Enum.all?(&(&1 in 0x20..0x7E))

      {%{type: :embedded, cmap: cmap}, text} when is_map(cmap) and is_binary(text) ->
        String.valid?(text) and
          text
          |> String.to_charlist()
          |> Enum.all?(&(Map.get(cmap, &1, 0) != 0))

      _ ->
        false
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
  Measures glyph advances in PDF points for the selected font and size.
  Nonzero letter spacing disables optional ligatures; callers add spacing separately.
  """
  @spec text_width(String.t(), map(), number(), number()) :: number()
  def text_width(text, font, font_size, letter_spacing \\ 0) do
    case font do
      %{type: :embedded, units_per_em: units_per_em} ->
        {glyphs, maximum_glyph} =
          text
          |> shape_ligatures(font, letter_spacing)
          |> String.to_charlist()
          |> Enum.map_reduce(-1, fn codepoint, maximum ->
            glyph = Map.get(font.cmap, codepoint, 0)
            {glyph, max(glyph, maximum)}
          end)

        NativeElixirPdfUtilities.Validators.HtmlValidator.reserve_render_resource(
          :max_layout_text_work,
          maximum_glyph + 1,
          :layout
        )

        widths = font.widths |> Enum.take(maximum_glyph + 1) |> List.to_tuple()

        glyphs
        |> Enum.reduce({0, nil}, fn glyph_id, {acc, previous} ->
          adjustment = Map.get(Map.get(font, :kerning, %{}), {previous, glyph_id}, 0)

          width =
            if glyph_id < tuple_size(widths), do: elem(widths, glyph_id), else: font.default_width

          {acc + width + adjustment, glyph_id}
        end)
        |> elem(0)
        |> Kernel./(units_per_em)
        |> Kernel.*(font_size)

      _ ->
        text
        |> String.length()
        |> Kernel.*(font_size)
        |> Kernel.*(0.6)
    end
  end

  @doc "Shapes common Latin ligatures when the font provides them and letter spacing is zero."
  @spec shape_ligatures(String.t(), map(), number()) :: String.t()
  def shape_ligatures(text, font, letter_spacing \\ 0) do
    if letter_spacing == 0 do
      [{"ffi", "ﬃ"}, {"ffl", "ﬄ"}, {"ff", "ﬀ"}, {"fi", "ﬁ"}, {"fl", "ﬂ"}]
      |> Enum.reduce(text, fn {letters, ligature}, shaped ->
        case Map.get(font.cmap, hd(String.to_charlist(ligature)), 0) do
          0 -> shaped
          _glyph -> String.replace(shaped, letters, ligature)
        end
      end)
    else
      text
    end
  end

  @doc """
  Builds a document-scoped CID encoding for text shown with an embedded font.
  Entries may be strings or `{text, letter_spacing}` pairs for spaced glyph runs.
  """
  @spec pdf_encoding([String.t() | {String.t(), number()}], embedded_font()) :: pdf_encoding()
  def pdf_encoding(texts, font) do
    encoding = %{codepoint_to_cid: %{}, cid_to_gid: %{}, cid_to_unicode: %{}}

    {encoding, _next_cid} =
      Enum.reduce(texts, {encoding, 1}, fn entry, {encoding, next_cid} ->
        {text, spacing} =
          case entry do
            {text, spacing} -> {text, spacing}
            text -> {text, 0}
          end

        text
        |> shape_ligatures(font, spacing)
        |> String.to_charlist()
        |> Enum.reduce({encoding, next_cid}, fn codepoint, {encoding, next_cid} ->
          case Map.has_key?(encoding.codepoint_to_cid, codepoint) do
            true ->
              {encoding, next_cid}

            false ->
              case Map.get(font.cmap, codepoint, 0) do
                0 ->
                  {encoding, next_cid}

                glyph_id ->
                  encoding = %{
                    codepoint_to_cid: Map.put(encoding.codepoint_to_cid, codepoint, next_cid),
                    cid_to_gid: Map.put(encoding.cid_to_gid, next_cid, glyph_id),
                    cid_to_unicode: Map.put(encoding.cid_to_unicode, next_cid, codepoint)
                  }

                  {encoding, next_cid + 1}
              end
          end
        end)
      end)

    encoding
  end

  @doc """
  Encodes original text for an embedded Type0 font content stream.
  Applies the encoding's available ligatures unless letter spacing is nonzero.
  """
  @spec encode_embedded_text(String.t(), pdf_encoding() | embedded_font(), number()) :: String.t()
  def encode_embedded_text(text, encoding_or_font, letter_spacing \\ 0) do
    encoding =
      case encoding_or_font do
        %{codepoint_to_cid: mappings} = encoding when is_map(mappings) ->
          encoding

        %{type: :embedded} = font ->
          pdf_encoding([{text, letter_spacing}], font)
      end

    text
    |> shape_ligatures(%{cmap: encoding.codepoint_to_cid}, letter_spacing)
    |> String.to_charlist()
    |> Enum.map_join("", fn codepoint ->
      cid = Map.fetch!(encoding.codepoint_to_cid, codepoint)
      Base.encode16(<<cid::16>>, case: :upper)
    end)
  end

  @doc """
  Builds CID-to-Unicode mappings for all text shown with an embedded font.
  """
  @spec unicode_mappings([String.t()], embedded_font()) :: %{
          optional(non_neg_integer()) => non_neg_integer()
        }
  def unicode_mappings(texts, font) do
    pdf_encoding(texts, font).cid_to_unicode
  end

  defp load_fonts(fonts, source) do
    Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, acc} ->
      case load_font(Map.put(font, :source, source)) do
        {:ok, loaded} -> {:cont, {:ok, acc ++ [loaded]}}
        {:error, _reason} = error -> {:halt, error}
        :error -> {:halt, :error}
      end
    end)
  end

  defp normalize_config(font) do
    case font do
      {family, path} ->
        normalize_config(%{family: family, path: path})

      font when is_list(font) ->
        case Keyword.keyword?(font) do
          true -> normalize_config(Map.new(font))
          false -> :error
        end

      font when is_map(font) ->
        family = Map.get(font, :family) || Map.get(font, "family")
        path = Map.get(font, :path) || Map.get(font, "path")
        data = Map.get(font, :data) || Map.get(font, "data")
        weight = Map.get(font, :weight) || Map.get(font, "weight") || 400
        style = Map.get(font, :style) || Map.get(font, "style") || :normal

        with true <- is_binary(family) and String.trim(family) != "",
             {:ok, source} <- normalize_font_source(path, data),
             {:ok, weight} <- normalize_weight(weight),
             {:ok, style} <- normalize_style(style) do
          {:ok,
           source
           |> Map.merge(%{family: String.trim(family), weight: weight, style: style})}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp normalize_font_source(path, data) do
    case {path, data} do
      {path, nil} ->
        with {:ok, paths} <- normalize_paths(path), do: {:ok, %{path: paths}}

      {nil, data} ->
        case data do
          data when is_binary(data) and byte_size(data) > 0 ->
            {:ok, %{data: [data]}}

          candidates when is_list(candidates) ->
            FontValidator.check(:max_font_candidates, length(candidates))

            case candidates != [] and
                   Enum.all?(candidates, &(is_binary(&1) and byte_size(&1) > 0)) do
              true -> {:ok, %{data: candidates}}
              false -> :error
            end

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp normalize_paths(path) do
    case path do
      path when is_binary(path) ->
        case String.trim(path) do
          "" -> :error
          path -> {:ok, [path]}
        end

      paths when is_list(paths) ->
        FontValidator.check(:max_font_candidates, length(paths))

        case paths != [] and Enum.all?(paths, &(is_binary(&1) and String.trim(&1) != "")) do
          true -> {:ok, Enum.map(paths, &String.trim/1)}
          false -> :error
        end

      _ ->
        :error
    end
  end

  defp normalize_weight(weight) do
    case weight do
      weight when is_number(weight) and weight >= 100 and weight <= 900 ->
        {:ok, weight}

      "normal" ->
        {:ok, 400}

      "bold" ->
        {:ok, 700}

      weight when is_binary(weight) ->
        case Integer.parse(String.trim(weight)) do
          {weight, ""} when weight >= 100 and weight <= 900 -> {:ok, weight}
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp normalize_style(style) do
    case style do
      :normal -> {:ok, :normal}
      :italic -> {:ok, :italic}
      "normal" -> {:ok, :normal}
      "italic" -> {:ok, :italic}
      _ -> :error
    end
  end

  defp bundled_font_configs do
    font_directory =
      Application.app_dir(:native_elixir_pdf_utilities, "priv/fonts/dejavu")

    [
      %{
        family: @bundled_font_family,
        path: Path.join(font_directory, "DejaVuSans.ttf"),
        weight: 400,
        style: :normal
      },
      %{
        family: @bundled_font_family,
        path: Path.join(font_directory, "DejaVuSans-Bold.ttf"),
        weight: 700,
        style: :normal
      },
      %{
        family: @bundled_font_family,
        path: Path.join(font_directory, "DejaVuSans-Oblique.ttf"),
        weight: 400,
        style: :italic
      },
      %{
        family: @bundled_font_family,
        path: Path.join(font_directory, "DejaVuSans-BoldOblique.ttf"),
        weight: 700,
        style: :italic
      }
    ]
  end

  defp font_key(font) do
    {String.downcase(font.family), font.weight, font.style}
  end

  defp load_font(font) do
    FontValidator.reserve_face(font)

    result =
      case font do
        %{path: paths} ->
          load_first_supported_font(paths)

        %{data: candidates} ->
          load_first_supported_data(candidates)
      end

    family = Map.fetch!(font, :family)

    with {:ok, data, parsed} <- result,
         :ok <-
           HtmlValidator.validate_font_embedding(
             family,
             parsed.embedding_flags,
             parsed.variable_font?
           ) do
      hash =
        :crypto.hash(:sha256, [family, data])
        |> Base.encode16(case: :lower)
        |> binary_part(0, 12)

      {:ok,
       parsed
       |> Map.merge(%{
         type: :embedded,
         family: family,
         weight: Map.fetch!(font, :weight),
         style: Map.fetch!(font, :style),
         source: Map.fetch!(font, :source),
         id: hash,
         pdf_name: pdf_safe_name(family) <> "-" <> hash,
         data: data
       })}
    else
      {:error, {_reason, _diagnostic}} = error -> error
      _ -> :error
    end
  end

  defp load_first_supported_font(paths) do
    Enum.reduce_while(paths, :error, fn path, :error ->
      result =
        FontCache.fetch(path, fn data ->
          with {:ok, parsed} <- parse_ttf(data) do
            {:ok, {data, parsed}}
          else
            {:error, {_reason, _diagnostic}} = error -> error
            _ -> :error
          end
        end)

      case result do
        {:ok, {data, parsed}} -> {:halt, {:ok, data, parsed}}
        {:error, {_reason, _diagnostic}} = error -> {:halt, error}
        :error -> {:cont, :error}
      end
    end)
  end

  defp load_first_supported_data(candidates) do
    Enum.reduce_while(candidates, :error, fn data, :error ->
      FontValidator.reserve(:max_font_candidates, 1)
      FontValidator.reserve_source(data)

      case FontValidator.memo({:parsed, :crypto.hash(:sha256, data)}, fn -> parse_ttf(data) end) do
        {:ok, parsed} -> {:halt, {:ok, data, parsed}}
        {:error, {_reason, _diagnostic}} = error -> {:halt, error}
        :error -> {:cont, :error}
      end
    end)
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
  end

  defp resolve_family(family, weight, style, registry) do
    embedded_family(family, weight, style, registry) ||
      discover_system_family(family, weight, style, registry)
  end

  defp discover_system_family(family, weight, style, registry) do
    case Map.get(registry, :system_font_discovery, false) do
      true ->
        key = {String.downcase(family), weight, style}

        FontValidator.memo({:discovery, key}, fn ->
          FontValidator.reserve(:max_font_discoveries, 1)

          result =
            SystemFontCache.fetch(key, fn ->
              FontValidator.with_budget(fn ->
                with {:ok, discovered} <- ElixirFontDiscovery.resolve(family, weight, style),
                     {:ok, loaded} <-
                       load_font(%{
                         family: discovered.family,
                         data: [discovered.data],
                         weight: discovered.weight,
                         style:
                           if(discovered.style == :oblique, do: :italic, else: discovered.style),
                         source: :system
                       }) do
                  loaded
                else
                  {:error, {_reason, _diagnostic}} = error -> error
                  _ -> nil
                end
              end)
            end)

          FontValidator.discovery_result(result)
        end)

      false ->
        nil
    end
  end

  defp embedded_family(family, weight, style, registry) do
    normalized_family = String.downcase(family)

    registry.embedded
    |> Enum.filter(&(String.downcase(&1.family) == normalized_family))
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
         {:ok, ascent, descent, line_gap, hmetric_count} <- parse_hhea(hhea),
         {:ok, glyph_count} <- read_u16(maxp, 4),
         {:ok, widths} <- parse_hmtx(hmtx, glyph_count, hmetric_count),
         {:ok, cmap} <- parse_cmap(cmap),
         {:ok, kerning} <-
           FontValidator.prepare_kerning(
             case table(data, tables, "kern") do
               {:ok, kern} -> kern
               :error -> <<>>
             end,
             glyph_count
           ),
         {:ok, embedding_flags} <- font_embedding_flags(data, tables) do
      {:ok,
       %{
         units_per_em: units_per_em,
         widths: widths,
         default_width: List.last(widths) || 600,
         cmap: cmap,
         kerning: kerning,
         ascent: ascent,
         descent: descent,
         line_gap: line_gap,
         bbox: bbox,
         embedding_flags: embedding_flags,
         variable_font?: Map.has_key?(tables, "fvar")
       }}
    end
  end

  defp font_embedding_flags(data, tables) do
    case table(data, tables, "OS/2") do
      {:ok, os2} -> read_u16(os2, 8)
      :error -> {:ok, 0}
    end
  end

  defp table_directory(data) do
    with {:ok, scaler_type} <- read_u32(data, 0),
         true <- scaler_type in [0x0001_0000, 0x7472_7565],
         {:ok, table_count} <- read_u16(data, 4),
         true <- table_count > 0,
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
         {:ok, line_gap} <- read_i16(hhea, 8),
         {:ok, hmetric_count} <- read_u16(hhea, 34),
         true <- hmetric_count > 0 do
      {:ok, ascent, descent, line_gap, hmetric_count}
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
    with {:ok, subtables} <- FontValidator.prepare_cmap(cmap) do
      Enum.find_value(subtables, :error, fn segments ->
        mappings =
          Enum.reduce(segments, %{}, fn {first, last, delta, glyphs}, mappings ->
            first..last
            |> Enum.reject(&(&1 == 0xFFFF))
            |> Enum.reduce(mappings, fn codepoint, mappings ->
              glyph_id =
                case glyphs do
                  nil ->
                    rem(codepoint + delta, 65_536)

                  glyphs ->
                    case :binary.decode_unsigned(binary_part(glyphs, 2 * (codepoint - first), 2)) do
                      0 -> 0
                      glyph_id -> rem(glyph_id + delta, 65_536)
                    end
                end

              case glyph_id > 0 do
                true -> Map.put(mappings, codepoint, glyph_id)
                false -> mappings
              end
            end)
          end)

        if map_size(mappings) > 0, do: {:ok, mappings}
      end)
    end
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
