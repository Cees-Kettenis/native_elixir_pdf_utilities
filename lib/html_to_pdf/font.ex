defmodule NativeElixirPdfUtilities.HtmlToPdf.Font do
  @moduledoc """
  Font loading, fallback resolution, text measurement, and PDF text encoding.

  The renderer loads explicitly configured fonts, bundles DejaVu Sans fallback
  faces, and also discovers a small set of common system sans-serif fonts when
  they are available.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.FontCache
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type font_style :: :normal | :italic
  @type registry :: %{embedded: [embedded_font()], fallback: [embedded_font()]}
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
  @typedoc "A document-scoped mapping from Unicode code points to PDF CIDs and font glyphs."
  @type pdf_encoding :: %{
          required(:codepoint_to_cid) => %{optional(non_neg_integer()) => pos_integer()},
          required(:cid_to_gid) => %{optional(pos_integer()) => non_neg_integer()},
          required(:cid_to_unicode) => %{optional(pos_integer()) => non_neg_integer()}
        }
  @type font_face :: built_in_font() | embedded_font()

  @built_in_families ["Courier", "Helvetica", "Times-Roman"]
  @bundled_font_family "DejaVu Sans"
  @system_font_candidates [
    %{
      family: "Arial",
      path: "/usr/share/fonts/truetype/msttcorefonts/Arial.ttf",
      weight: 400,
      style: :normal
    },
    %{
      family: "Arial",
      path: "/usr/share/fonts/truetype/msttcorefonts/Arial_Bold.ttf",
      weight: 700,
      style: :normal
    },
    %{
      family: "Arial",
      path: "/usr/share/fonts/truetype/msttcorefonts/Arial_Italic.ttf",
      weight: 400,
      style: :italic
    },
    %{
      family: "Arial",
      path: "/usr/share/fonts/truetype/msttcorefonts/Arial_Bold_Italic.ttf",
      weight: 700,
      style: :italic
    },
    %{
      family: "Liberation Sans",
      path: [
        "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
        "/usr/share/fonts/truetype/liberation2/LiberationSans-Regular.ttf",
        "/usr/share/fonts/liberation/LiberationSans-Regular.ttf"
      ],
      weight: 400,
      style: :normal
    },
    %{
      family: "Liberation Sans",
      path: [
        "/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf",
        "/usr/share/fonts/truetype/liberation2/LiberationSans-Bold.ttf",
        "/usr/share/fonts/liberation/LiberationSans-Bold.ttf"
      ],
      weight: 700,
      style: :normal
    },
    %{
      family: "Liberation Sans",
      path: [
        "/usr/share/fonts/truetype/liberation/LiberationSans-Italic.ttf",
        "/usr/share/fonts/truetype/liberation2/LiberationSans-Italic.ttf",
        "/usr/share/fonts/liberation/LiberationSans-Italic.ttf"
      ],
      weight: 400,
      style: :italic
    },
    %{
      family: "Liberation Sans",
      path: [
        "/usr/share/fonts/truetype/liberation/LiberationSans-BoldItalic.ttf",
        "/usr/share/fonts/truetype/liberation2/LiberationSans-BoldItalic.ttf",
        "/usr/share/fonts/liberation/LiberationSans-BoldItalic.ttf"
      ],
      weight: 700,
      style: :italic
    },
    %{
      family: "DejaVu Sans",
      path: "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
      weight: 400,
      style: :normal
    },
    %{
      family: "DejaVu Sans",
      path: "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf",
      weight: 700,
      style: :normal
    },
    %{
      family: "DejaVu Sans",
      path: "/usr/share/fonts/truetype/dejavu/DejaVuSans-Oblique.ttf",
      weight: 400,
      style: :italic
    },
    %{
      family: "DejaVu Sans",
      path: "/usr/share/fonts/truetype/dejavu/DejaVuSans-BoldOblique.ttf",
      weight: 700,
      style: :italic
    },
    %{
      family: "Noto Sans",
      path: "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf",
      weight: 400,
      style: :normal
    },
    %{
      family: "Noto Sans",
      path: "/usr/share/fonts/truetype/noto/NotoSans-Bold.ttf",
      weight: 700,
      style: :normal
    }
  ]

  @doc false
  @spec normalize_options(term()) :: {:ok, keyword()} | :error
  def normalize_options(opts) do
    case Keyword.keyword?(opts) do
      true ->
        case normalize_configs(Keyword.get(opts, :fonts, [])) do
          {:ok, fonts} -> {:ok, Keyword.put(opts, :fonts, fonts)}
          :error -> :error
        end

      false ->
        :error
    end
  end

  @doc false
  @spec normalize_configs(term()) :: {:ok, [map()]} | :error
  def normalize_configs(fonts) do
    case is_list(fonts) do
      true ->
        Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, prepared} ->
          case normalize_config(font) do
            {:ok, normalized} -> {:cont, {:ok, prepared ++ [normalized]}}
            :error -> {:halt, :error}
          end
        end)

      false ->
        :error
    end
  end

  @doc """
  Loads explicit TTF font options into a registry.

  Accepted font entries are maps, keyword lists, or `{family, path}` tuples. Map
  and keyword entries must include `:family` and one or more `:path` or `:data`
  candidates; `:weight` and `:style` are optional.
  """
  @spec load_registry(keyword()) :: {:ok, registry()} | :error
  def load_registry(opts) do
    with {:ok, prepared_opts} <- normalize_options(opts),
         :ok <- HtmlValidator.validate_font_configs(Keyword.fetch!(prepared_opts, :fonts)),
         {:ok, bundled_configs} <- normalize_configs(bundled_font_configs()),
         :ok <- HtmlValidator.validate_font_configs(bundled_configs),
         {:ok, configured} <- load_fonts(Keyword.fetch!(prepared_opts, :fonts)),
         {:ok, bundled} <- load_fonts(bundled_configs) do
      fallback = configured ++ bundled
      embedded = Enum.uniq_by(fallback, &font_key/1)
      {:ok, %{embedded: embedded ++ system_fonts(embedded), fallback: fallback}}
    else
      _ -> :error
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
  Builds a document-scoped CID encoding for text shown with an embedded font.
  """
  @spec pdf_encoding([String.t()], embedded_font()) :: pdf_encoding()
  def pdf_encoding(texts, font) do
    encoding = %{codepoint_to_cid: %{}, cid_to_gid: %{}, cid_to_unicode: %{}}

    {encoding, _next_cid} =
      Enum.reduce(texts, {encoding, 1}, fn text, {encoding, next_cid} ->
        text
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
  Encodes text for an embedded Type0 font content stream.
  """
  @spec encode_embedded_text(String.t(), pdf_encoding() | embedded_font()) :: String.t()
  def encode_embedded_text(text, encoding_or_font) do
    encoding =
      case encoding_or_font do
        %{codepoint_to_cid: mappings} = encoding when is_map(mappings) ->
          encoding

        %{type: :embedded} = font ->
          pdf_encoding([text], font)
      end

    text
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

  defp load_fonts(fonts) do
    Enum.reduce_while(fonts, {:ok, []}, fn font, {:ok, acc} ->
      case load_font(font) do
        {:ok, loaded} -> {:cont, {:ok, acc ++ [loaded]}}
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
    result =
      case font do
        %{path: paths} ->
          load_first_supported_font(paths)

        %{data: candidates} ->
          load_first_supported_data(candidates)
      end

    with {:ok, data, parsed} <- result do
      family = Map.fetch!(font, :family)

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
         id: hash,
         pdf_name: pdf_safe_name(family) <> "-" <> hash,
         data: data
       })}
    else
      _ -> :error
    end
  end

  defp load_first_supported_font(paths) do
    Enum.reduce_while(paths, :error, fn path, :error ->
      result =
        FontCache.fetch(path, fn absolute_path ->
          with {:ok, data} <- File.read(absolute_path),
               {:ok, parsed} <- parse_ttf(data) do
            {:ok, {data, parsed}}
          else
            _ -> :error
          end
        end)

      case result do
        {:ok, {data, parsed}} -> {:halt, {:ok, data, parsed}}
        :error -> {:cont, :error}
      end
    end)
  end

  defp load_first_supported_data(candidates) do
    Enum.reduce_while(candidates, :error, fn data, :error ->
      case parse_ttf(data) do
        {:ok, parsed} -> {:halt, {:ok, data, parsed}}
        _ -> {:cont, :error}
      end
    end)
  end

  defp system_fonts(explicit_fonts) do
    explicit_keys =
      explicit_fonts
      |> Enum.map(&{String.downcase(&1.family), &1.weight, &1.style})
      |> MapSet.new()

    candidates =
      system_font_candidates()
      |> Enum.reject(fn font ->
        MapSet.member?(explicit_keys, {String.downcase(font.family), font.weight, font.style}) or
          not Enum.any?(List.wrap(font.path), &File.regular?/1)
      end)
      |> Enum.uniq_by(&{String.downcase(&1.family), &1.weight, &1.style})

    {:ok, prepared} = normalize_configs(candidates)
    :ok = HtmlValidator.validate_font_configs(prepared)

    Enum.reduce(prepared, [], fn font, acc ->
      loaded = load_font(font)
      if match?({:ok, _loaded}, loaded), do: acc ++ [elem(loaded, 1)], else: acc
    end)
  end

  defp system_font_candidates do
    @system_font_candidates ++ user_arial_font_candidates()
  end

  defp user_arial_font_candidates do
    with {:ok, home} <- System.fetch_env("HOME") do
      [
        %{
          family: "Arial",
          path: Path.join(home, ".local/share/fonts/Monotype/TrueType/Arial/Arial_Regular.ttf"),
          weight: 400,
          style: :normal
        },
        %{
          family: "Arial",
          path: Path.join(home, ".local/share/fonts/Monotype/TrueType/Arial/Arial_Bold.ttf"),
          weight: 700,
          style: :normal
        },
        %{
          family: "Arial",
          path: Path.join(home, ".local/share/fonts/Monotype/TrueType/Arial/Arial_Italic.ttf"),
          weight: 400,
          style: :italic
        },
        %{
          family: "Arial",
          path:
            Path.join(home, ".local/share/fonts/Monotype/TrueType/Arial/Arial_Bold_Italic.ttf"),
          weight: 700,
          style: :italic
        }
      ]
    else
      _ -> []
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
         true <- count > 0,
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
