defmodule NativeElixirPdfUtilities.HtmlToPdf.Font do
  @moduledoc """
  Font loading, fallback resolution, text measurement, and PDF text encoding.

  The renderer loads explicitly configured fonts, discovers installed fonts on
  demand, and keeps the bundled DejaVu Sans faces as the final fallback.
  """

  alias NativeElixirPdfUtilities.HtmlToPdf.FontCache
  alias NativeElixirPdfUtilities.HtmlToPdf.SystemFontCache
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type font_style :: :normal | :italic
  @type registry :: %{
          required(:embedded) => [embedded_font()],
          required(:fallback) => [embedded_font()],
          required(:system_font_discovery) => boolean(),
          optional(:scoped) => %{
            optional(reference()) => %{embedded: [embedded_font()], fallback: [embedded_font()]}
          },
          optional(:active_scope) => reference() | nil
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
  @typedoc "A document-scoped mapping from Unicode or selected glyph pairs to PDF CIDs and glyphs."
  @type pdf_encoding :: %{
          required(:codepoint_to_cid) => %{
            optional(non_neg_integer() | {non_neg_integer(), non_neg_integer()}) => pos_integer()
          },
          required(:cid_to_gid) => %{optional(pos_integer()) => non_neg_integer()},
          required(:cid_to_unicode) => %{
            optional(pos_integer()) => non_neg_integer() | String.t()
          },
          optional(:latin_ligatures) => %{optional(String.t()) => non_neg_integer()}
        }
  @type font_face :: built_in_font() | embedded_font()

  @bundled_font_family "DejaVu Sans"
  @measurement_cache_key {__MODULE__, :measurement_cache}
  @latin_ligatures [
    {"ffi", 0xFB03, "ﬃ"},
    {"ffl", 0xFB04, "ﬄ"},
    {"ff", 0xFB00, "ﬀ"},
    {"fi", 0xFB01, "ﬁ"},
    {"fl", 0xFB02, "ﬂ"}
  ]

  @doc """
  Reads the selected font's OpenType MATH constants, glyph corrections,
  height-dependent kerning, stretch variants, and assemblies in design units.
  The `:substitutions` map contains the default math language's `ssty`, `flac`,
  and `dtls` features. Each feature retains ordered lookup maps from glyph IDs
  to alternate glyph lists. `:substitution_kinds` retains each lookup's single
  or alternate kind, so a single substitution is not mistaken for a one-item
  alternate set. Scaling and positioning are separate layout work.

  Parsing is lazy and reused within the font-loading budget scope. Missing or
  malformed metrics return a diagnostic; another font's metrics are never used.
  """
  @spec math_metrics(font_face()) :: {:ok, map()} | {:error, {atom(), map()}}
  def math_metrics(font) do
    FontValidator.prepare_math_metrics(font)
  end

  @doc """
  Reads glyph ink bounds from the selected TrueType font in design units.

  Tuple positions are glyph IDs and contain `{xmin, ymin, xmax, ymax}`. Empty
  outlines have zero bounds. Parsing is lazy and reuses identical source bytes
  within the font budget; decoded geometry consumes `max_font_math_work`.
  """
  @spec glyph_bounds(font_face()) :: {:ok, tuple()} | {:error, {atom(), map()}}
  def glyph_bounds(font) do
    FontValidator.prepare_glyph_bounds(font)
  end

  @doc false
  @spec with_measurement_cache((-> result)) :: result when result: term()
  def with_measurement_cache(fun) do
    case Process.get(@measurement_cache_key) do
      nil ->
        table = :ets.new(:font_measurements, [:set, :private])
        Process.put(@measurement_cache_key, {table, 0, %{}})

        try do
          fun.()
        after
          Process.delete(@measurement_cache_key)
          :ets.delete(table)
        end

      _cache ->
        fun.()
    end
  end

  @doc false
  @spec normalize_options(term()) ::
          {:ok, keyword()} | :error | {:error, {:resource_limit_exceeded, map()}}
  def normalize_options(opts) do
    FontValidator.prepare_options(opts)
  end

  @doc false
  @spec normalize_configs(term()) ::
          {:ok, [map()]} | :error | {:error, {:resource_limit_exceeded, map()}}
  def normalize_configs(fonts) do
    FontValidator.prepare_configs(fonts)
  end

  @doc "Loads only explicit caller TrueType configurations, without bundled fonts or system discovery."
  @spec load_configured_faces(term()) :: {:ok, [embedded_font()]} | {:error, {atom(), map()}}
  def load_configured_faces(configs) do
    FontValidator.with_budget(fn ->
      with {:ok, normalized} <- normalize_configs(configs),
           {:ok, faces} <- load_fonts(normalized, :configured) do
        {:ok, faces}
      else
        {:error, _} = error ->
          error

        :error ->
          NativeElixirPdfUtilities.Diagnostics.error(
            :font,
            :invalid_font,
            "caller fonts require valid explicit TrueType configurations",
            module: __MODULE__,
            operation: :load_configured_faces
          )
      end
    end)
  end

  @doc "Loads validated embedded byte faces without bundled font loading or system discovery."
  @spec load_byte_faces(term()) :: {:ok, [embedded_font()]} | {:error, {atom(), map()}}
  def load_byte_faces(descriptors) do
    FontValidator.with_budget(fn ->
      with {:ok, configs} <- FontValidator.prepare_byte_faces(descriptors),
           {:ok, normalized} <- normalize_configs(configs),
           {:ok, faces} <- load_fonts(normalized, :configured) do
        {:ok, faces}
      else
        {:error, _} = error ->
          error

        _ ->
          NativeElixirPdfUtilities.Diagnostics.error(
            :font,
            :unsupported_font,
            "embedded source editor font could not be loaded",
            module: __MODULE__,
            operation: :load_byte_faces
          )
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
           {:ok, bundled_configs} <- normalize_configs(bundled_font_configs()),
           {:ok, registry} <-
             FontValidator.with_registry_loads(
               Keyword.fetch!(prepared_opts, :fonts),
               bundled_configs,
               fn ->
                 with {:ok, configured} <-
                        load_fonts(Keyword.fetch!(prepared_opts, :fonts), :configured),
                      {:ok, bundled} <- load_fonts(bundled_configs, :bundled) do
                   fallback = configured ++ bundled

                   {:ok,
                    %{
                      embedded: Enum.uniq_by(fallback, &font_key/1),
                      fallback: fallback,
                      system_font_discovery:
                        Keyword.get(prepared_opts, :system_font_discovery, true)
                    }}
                 end
               end
             ) do
        {:ok, registry}
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
    registry = scoped_registry(registry)

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
        String.valid?(text, :fast_ascii) and
          text
          |> String.to_charlist()
          |> Enum.all?(&(&1 in 0x20..0x7E))

      {%{type: :embedded, cmap: cmap}, text} when is_map(cmap) and is_binary(text) ->
        String.valid?(text, :fast_ascii) and supported_codepoints?(text, cmap)

      _ ->
        false
    end
  end

  # The caller has admitted UTF-8. Scan without allocating a charlist for
  # every fallback-font candidate and stop at the first missing glyph.
  defp supported_codepoints?(text, cmap) do
    case text do
      "" ->
        true

      <<codepoint::utf8, rest::binary>> ->
        case cmap do
          %{^codepoint => glyph} when glyph != 0 -> supported_codepoints?(rest, cmap)
          _ -> false
        end
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
  Standalone work-limit failures return the shared diagnostic; successful calls return a number.
  """
  @spec text_width(String.t(), map(), number(), number()) ::
          number() | {:error, {atom(), NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  def text_width(text, font, font_size, letter_spacing \\ 0) do
    measure_text_result(text, font, font_size, letter_spacing, :design)
  end

  @doc "Measures a validated selected font's CSS linear advances with fixed FreeType precision."
  @spec css_text_width(String.t(), font_face(), number(), number()) ::
          number() | {:error, {atom(), NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  @spec css_text_width(String.t(), font_face(), number()) ::
          number() | {:error, {atom(), NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  def css_text_width(text, font, font_size, letter_spacing \\ 0) do
    measure_text_result(text, font, font_size, letter_spacing, :css)
  end

  @doc "Returns selected-font CSS glyph advances for validated text, sharing shaping, caches and work budgets."
  @spec css_glyph_advances(String.t(), embedded_font(), number(), number()) ::
          [{non_neg_integer(), non_neg_integer(), number()}]
          | {:error, {atom(), NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  @spec css_glyph_advances(String.t(), embedded_font(), number()) ::
          [{non_neg_integer(), non_neg_integer(), number()}]
          | {:error, {atom(), NativeElixirPdfUtilities.Diagnostics.diagnostic()}}
  def css_glyph_advances(text, font, font_size, letter_spacing \\ 0) do
    measure_text_result(text, font, font_size, letter_spacing, :css_glyph_advances)
  end

  # The CSS and design-unit APIs share shaping, selected-font caches and work accounting.
  defp measure_text_result(text, font, font_size, letter_spacing, metrics) do
    result =
      HtmlValidator.with_render_budget(fn ->
        case font do
          %{type: :embedded, units_per_em: units_per_em} ->
            {width, work} =
              case Process.get(@measurement_cache_key) do
                {table, _bytes, _fonts} = cache ->
                  identity = measurement_font_identity(font, cache)
                  cache_key = {identity, font_size, letter_spacing, text, metrics}

                  case :ets.lookup(table, cache_key) do
                    [{^cache_key, width, work}] ->
                      {width, work}

                    [] ->
                      measured =
                        embedded_text_width(
                          text,
                          font,
                          units_per_em,
                          font_size,
                          letter_spacing,
                          metrics
                        )

                      {^table, bytes, fonts} = Process.get(@measurement_cache_key)

                      # Glyph plans retain a list, unlike scalar widths. Account
                      # for it through the shared font-cache byte measurement.
                      plan_bytes =
                        if is_list(elem(measured, 0)),
                          do: FontValidator.cache_bytes(elem(measured, 0)),
                          else: 0

                      allocated = byte_size(text) + plan_bytes

                      if :ets.info(table, :size) + map_size(fonts) <
                           Limits.get(:max_layout_measurement_cache_entries) and
                           bytes + allocated <= Limits.get(:max_layout_measurement_cache_bytes) do
                        :ets.insert(table, {cache_key, elem(measured, 0), elem(measured, 1)})
                        Process.put(@measurement_cache_key, {table, bytes + allocated, fonts})
                      end

                      measured
                  end

                nil ->
                  embedded_text_width(
                    text,
                    font,
                    units_per_em,
                    font_size,
                    letter_spacing,
                    metrics
                  )
              end

            HtmlValidator.reserve_render_resource(:max_layout_text_work, work, :layout)
            width

          _ ->
            text
            |> String.length()
            |> Kernel.*(font_size)
            |> Kernel.*(0.6)
        end
      end)

    case result do
      {:error, {reason, diagnostic}} ->
        operation =
          case metrics do
            :design -> :text_width
            :css -> :css_text_width
            :css_glyph_advances -> :css_glyph_advances
          end

        {:error,
         {reason,
          diagnostic
          |> Map.drop([:operation, :module])
          |> NativeElixirPdfUtilities.Diagnostics.with_context(
            operation: operation,
            module: __MODULE__,
            source: Map.get(font, :family)
          )}}

      result ->
        result
    end
  end

  defp embedded_text_width(text, font, units_per_em, font_size, letter_spacing, metrics) do
    {metric_mode, paint?} =
      case metrics do
        :css_glyph_advances -> {FontValidator.css_advance_scale(units_per_em, font_size), true}
        :css -> {FontValidator.css_advance_scale(units_per_em, font_size), false}
        :design -> {:design, false}
      end

    {glyphs, maximum_glyph} =
      text
      |> shape_ligatures(font, letter_spacing)
      |> String.to_charlist()
      |> Enum.map_reduce(-1, fn codepoint, maximum ->
        glyph = Map.get(font.cmap, codepoint, 0)
        {if(paint?, do: {codepoint, glyph}, else: glyph), max(glyph, maximum)}
      end)

    widths = measurement_widths(font, maximum_glyph)

    measured =
      if paint? do
        kerning = Map.get(font, :kerning, %{})

        {advances, pending} =
          glyphs
          |> Enum.reduce({[], nil}, fn {codepoint, glyph_id}, {advances, previous} ->
            width =
              if glyph_id < tuple_size(widths),
                do: elem(widths, glyph_id),
                else: font.default_width

            base = selected_glyph_advance(width, metric_mode)

            advances =
              case previous do
                nil ->
                  advances

                {previous_codepoint, previous_glyph, previous_base} ->
                  adjustment = Map.get(kerning, {previous_glyph, glyph_id}, 0)

                  combined =
                    if adjustment == 0,
                      do: base,
                      else: selected_glyph_advance(width + adjustment, metric_mode)

                  # Pair kerning moves this glyph's origin, so it belongs to
                  # the preceding pen advance. Emit that advance once known.
                  [
                    {previous_codepoint, previous_glyph,
                     (previous_base + combined - base) / 65_536 * metric_mode.correction * 0.75}
                    | advances
                  ]
              end

            {advances, {codepoint, glyph_id, base}}
          end)

        case pending do
          nil ->
            []

          {codepoint, glyph, base} ->
            Enum.reverse([
              {codepoint, glyph, base / 65_536 * metric_mode.correction * 0.75} | advances
            ])
        end
      else
        # Scalar layout and Math measurements keep their existing compact
        # integer reduction; only callers requesting a paint plan own a list.
        kerning = Map.get(font, :kerning, %{})

        width =
          glyphs
          |> Enum.reduce({0, nil}, fn glyph_id, {acc, previous} ->
            adjustment = Map.get(kerning, {previous, glyph_id}, 0)

            width =
              if glyph_id < tuple_size(widths),
                do: elem(widths, glyph_id),
                else: font.default_width

            {acc + selected_glyph_advance(width + adjustment, metric_mode), glyph_id}
          end)
          |> elem(0)

        case metric_mode do
          :design -> width / units_per_em * font_size
          %{correction: correction} -> width / 65_536 * correction * 0.75
        end
      end

    {measured, length(glyphs)}
  end

  # Measurement and paint share the exact signed FT_MulDiv operation; design
  # measurements retain their original integer advances and final normalization.
  @compile {:inline, selected_glyph_advance: 2}
  defp selected_glyph_advance(advance, metrics) do
    case metrics do
      :design ->
        advance

      %{scale: scale} ->
        scaled = div(abs(advance) * scale + 32, 64)
        if advance < 0, do: -scaled, else: scaled
    end
  end

  @compile {:inline, measurement_font_identity: 2}
  defp measurement_font_identity(font, {table, bytes, fonts}) do
    key = Map.get(font, :id) || font

    case Map.get(fonts, key) do
      {^font, identity, _widths, _complete, _allocated} ->
        identity

      previous ->
        if map_size(fonts) + :ets.info(table, :size) <
             Limits.get(:max_layout_measurement_cache_entries) do
          identity = make_ref()
          allocated = if previous, do: elem(previous, 4), else: 0
          fonts = Map.put(fonts, key, {font, identity, {}, false, 0})
          Process.put(@measurement_cache_key, {table, bytes - allocated, fonts})
          identity
        else
          font
        end
    end
  end

  @doc false
  @spec glyph_widths(embedded_font(), non_neg_integer()) :: tuple()
  def glyph_widths(font, maximum_glyph) do
    case Process.get(@measurement_cache_key) do
      nil -> :ok
      cache -> measurement_font_identity(font, cache)
    end

    measurement_widths(font, maximum_glyph)
  end

  defp measurement_widths(font, maximum_glyph) do
    requested = maximum_glyph + 1
    key = Map.get(font, :id) || font

    cached =
      case Process.get(@measurement_cache_key) do
        {_table, _bytes, fonts} -> Map.get(fonts, key)
        nil -> nil
      end

    case cached do
      {^font, _identity, widths, complete, _allocated}
      when complete or tuple_size(widths) >= requested ->
        widths

      _ ->
        HtmlValidator.reserve_render_resource(:max_layout_text_work, requested, :layout)
        widths = font.widths |> Enum.take(requested) |> List.to_tuple()

        case {cached, Process.get(@measurement_cache_key)} do
          {{^font, identity, _previous, _complete, allocated}, {table, bytes, fonts}} ->
            # Only the newly allocated tuple is owned here; font terms remain shared.
            tuple_bytes = (tuple_size(widths) + 1) * :erlang.system_info(:wordsize)

            if bytes - allocated + tuple_bytes <= Limits.get(:max_layout_measurement_cache_bytes) do
              fonts =
                Map.put(
                  fonts,
                  key,
                  {font, identity, widths, tuple_size(widths) < requested, tuple_bytes}
                )

              Process.put(@measurement_cache_key, {table, bytes - allocated + tuple_bytes, fonts})
            end

          _ ->
            :ok
        end

        widths
    end
  end

  @doc "Shapes common Latin ligatures authorized by the selected font's Latin GSUB liga feature when letter spacing is zero."
  @spec shape_ligatures(String.t(), map(), number()) :: String.t()
  def shape_ligatures(text, font, letter_spacing \\ 0) do
    if letter_spacing == 0 and :binary.match(text, "f") != :nomatch do
      @latin_ligatures
      |> Enum.reduce(text, fn {letters, codepoint, ligature}, shaped ->
        if Map.get(Map.get(font, :latin_ligatures, %{}), letters) == codepoint and
             Map.get(font.cmap, codepoint, 0) > 0,
           do: String.replace(shaped, letters, ligature),
           else: shaped
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
    encoding = %{
      codepoint_to_cid: %{},
      cid_to_gid: %{},
      cid_to_unicode: %{},
      latin_ligatures: Map.get(font, :latin_ligatures, %{})
    }

    # Repeated identical runs cannot introduce another CID.
    {encoding, _next_cid} =
      Enum.reduce(Enum.uniq(texts), {encoding, 1}, fn entry, {encoding, next_cid} ->
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
                    encoding
                    | codepoint_to_cid: Map.put(encoding.codepoint_to_cid, codepoint, next_cid),
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
  Older three-map encodings without `:latin_ligatures` retain their original
  shaping based on available Unicode ligature mappings. Explicit metadata,
  including an empty map, takes precedence.
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

    shaped =
      shape_ligatures(
        text,
        %{
          cmap: encoding.codepoint_to_cid,
          latin_ligatures:
            Map.get_lazy(encoding, :latin_ligatures, fn ->
              Map.new(@latin_ligatures, fn {letters, codepoint, _ligature} ->
                {letters, codepoint}
              end)
            end)
        },
        letter_spacing
      )

    encoded =
      for codepoint <- String.to_charlist(shaped), into: <<>> do
        <<Map.fetch!(encoding.codepoint_to_cid, codepoint)::16>>
      end

    Base.encode16(encoded, case: :upper)
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
    fonts
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {font, ordinal}, {:ok, acc} ->
      case load_font(Map.put(font, :source, source), ordinal) do
        {:ok, loaded} -> {:cont, {:ok, acc ++ [loaded]}}
        {:error, _reason} = error -> {:halt, error}
        :error -> {:halt, :error}
      end
    end)
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

  defp load_font(font, ordinal \\ nil) do
    FontValidator.reserve_face(font)

    result =
      case font do
        %{path: paths} ->
          load_first_supported_font(paths, Map.get(font, :source) == :bundled)

        %{data: candidates} ->
          load_first_supported_data(candidates)
      end

    family = Map.fetch!(font, :family)

    with {:ok, data, parsed, selected} <- result,
         :ok <-
           HtmlValidator.validate_font_embedding(
             family,
             parsed.embedding_flags,
             parsed.variable_font?
           ) do
      hash = FontCache.identity(family, data)

      loaded =
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
        })

      {:ok, FontValidator.observe_loaded_font(font, ordinal, selected, loaded)}
    else
      {:error, {_reason, _diagnostic}} = error -> error
      _ -> :error
    end
  end

  defp load_first_supported_font(paths, bundled?) do
    paths
    |> Enum.with_index()
    |> Enum.reduce_while(:error, fn {path, ordinal}, :error ->
      fetch = if bundled?, do: &FontCache.fetch_bundled/2, else: &FontCache.fetch/2

      result =
        fetch.(path, fn data ->
          with {:ok, parsed} <- FontValidator.prepare_ttf(data) do
            {:ok, {data, parsed}}
          else
            {:error, {_reason, _diagnostic}} = error -> error
            _ -> :error
          end
        end)

      case result do
        {:ok, {data, parsed}} -> {:halt, {:ok, data, parsed, ordinal}}
        {:error, {_reason, _diagnostic}} = error -> {:halt, error}
        :error -> {:cont, :error}
      end
    end)
  end

  defp load_first_supported_data(candidates) do
    candidates
    |> Enum.with_index()
    |> Enum.reduce_while(:error, fn {data, ordinal}, :error ->
      FontValidator.reserve(:max_font_candidates, 1)
      FontValidator.reserve_source(data)

      case FontValidator.memo({:parsed, :crypto.hash(:sha256, data)}, fn ->
             FontValidator.prepare_ttf(data)
           end) do
        {:ok, parsed} -> {:halt, {:ok, data, parsed, ordinal}}
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
      case String.downcase(family) do
        "stix two math" ->
          bundled_math_font()

        "math" ->
          embedded_family("STIX Two Math", weight, style, registry) || bundled_math_font()

        "dejavu sans mono" ->
          bundled_monospace_font(weight, style)

        "monospace" ->
          discover_system_family(family, weight, style, registry) ||
            bundled_monospace_font(weight, style)

        _ ->
          discover_system_family(family, weight, style, registry)
      end
  end

  defp bundled_math_font do
    FontValidator.memo(:bundled_math, fn ->
      load_bundled_font(%{
        family: "STIX Two Math",
        path: [
          Application.app_dir(
            :native_elixir_pdf_utilities,
            "priv/fonts/stix/STIXTwoMath-Regular.ttf"
          )
        ],
        weight: 400,
        style: :normal,
        source: :bundled
      })
    end)
  end

  defp bundled_monospace_font(weight, style) do
    weight = if weight >= 600, do: 700, else: 400

    FontValidator.memo({:bundled_monospace, weight, style}, fn ->
      suffix =
        case {weight, style} do
          {700, :italic} -> "-BoldOblique"
          {700, _} -> "-Bold"
          {_, :italic} -> "-Oblique"
          _ -> ""
        end

      load_bundled_font(%{
        family: "DejaVu Sans Mono",
        path: [
          Application.app_dir(
            :native_elixir_pdf_utilities,
            "priv/fonts/dejavu/DejaVuSansMono#{suffix}.ttf"
          )
        ],
        weight: weight,
        style: style,
        source: :bundled
      })
    end)
  end

  defp load_bundled_font(configuration) do
    case load_font(configuration) do
      {:ok, font} ->
        font

      {:error, {:resource_limit_exceeded, _diagnostic}} = error ->
        throw({:font_resource_limit, error})
    end
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

  defp scoped_registry(registry) do
    case registry do
      %{scoped: scopes, active_scope: scope} ->
        case Map.get(scopes, scope) do
          nil -> registry
          faces -> Map.merge(registry, faces)
        end

      _ ->
        registry
    end
  end

  defp embedded_family(family, weight, style, registry) do
    normalized_family = String.downcase(family)

    registry
    |> scoped_registry()
    |> Map.fetch!(:embedded)
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

  defp pdf_safe_name(name) do
    name
    |> String.replace(~r/[^A-Za-z0-9_-]/u, "")
    |> case do
      "" -> "EmbeddedFont"
      safe -> safe
    end
  end
end
