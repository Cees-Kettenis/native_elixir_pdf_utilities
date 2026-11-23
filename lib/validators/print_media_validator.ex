defmodule NativeElixirPdfUtilities.Validators.PrintMediaValidator do
  @moduledoc """
  Media predicates share one bounded grammar. Lengths are CSS pixels.
  Stylesheet media dimensions describe the requested page box before CSS @page
  size. The legacy responsive source-size context uses the effective printable
  area. Embedded media dimensions include gutters; client layout is separate.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @forward_range ~r/\A(width|height|inline-size|block-size|resolution|aspect-ratio|color|color-index|monochrome)\s*(<=|>=|<|>|=)\s*(.+)\z/s
  @reverse_range ~r/\A(.+?)\s*(<=|>=|<|>|=)\s*(width|height|inline-size|block-size|resolution|aspect-ratio|color|color-index|monochrome)(?:\s*(<=|>=|<|>)\s*(.+))?\z/s

  # Static print output uses sRGB, eight bits per component, no device session.
  @numeric_features %{"color" => 8, "color-index" => 0, "monochrome" => 0}
  @discrete_features %{
    "prefers-color-scheme" => {"light", ~w(light dark)},
    "prefers-contrast" => {"no-preference", ~w(no-preference more less custom)},
    "prefers-reduced-motion" => {"no-preference", ~w(no-preference reduce)},
    "prefers-reduced-transparency" => {"no-preference", ~w(no-preference reduce)},
    "prefers-reduced-data" => {"no-preference", ~w(no-preference reduce)},
    "forced-colors" => {"none", ~w(none active)},
    "inverted-colors" => {"none", ~w(none inverted)},
    "dynamic-range" => {"standard", ~w(standard high)},
    "color-gamut" => {"srgb", ~w(srgb p3 rec2020)},
    "update" => {"none", ~w(none slow fast)},
    "overflow-block" => {"paged", ~w(none scroll paged)},
    "overflow-inline" => {"none", ~w(none scroll)},
    "scripting" => {"none", ~w(none initial-only enabled)}
  }

  @page_candidate Regex.compile!(
                    "#{HtmlValidator.css_opaque_pattern()}(*SKIP)(*F)|@page\\b",
                    "ui"
                  )

  @type context :: %{
          required(:width) => float(),
          required(:height) => float(),
          required(:density) => float(),
          required(:type) => :print,
          optional(:source_font_options) => keyword(),
          optional(:source_metrics) => map()
        }

  @doc "Validates explicit print viewport and deterministic image density options."
  @spec validate_options(keyword()) :: :ok | {:error, {atom(), map()}}
  def validate_options(opts) do
    viewport = Keyword.get(opts, :print_viewport)
    density = Keyword.get(opts, :image_selection_density, 1)

    dimensions =
      if is_tuple(viewport) and tuple_size(viewport) == 2, do: Tuple.to_list(viewport), else: []

    for value <- [density | dimensions], is_number(value) and value > 0 do
      HtmlValidator.check_render_resource(:max_numeric_magnitude, value, :options)
    end

    valid_viewport? =
      case viewport do
        nil -> true
        {width, height} -> positive_number?(width) and positive_number?(height)
        _ -> false
      end

    if valid_viewport? and positive_number?(density) do
      :ok
    else
      Diagnostics.error(
        :options,
        :invalid_options,
        "print_viewport must be a positive {width, height} in CSS pixels and image_selection_density must be positive",
        module: __MODULE__,
        source: "print_viewport/image_selection_density"
      )
    end
  end

  @doc "Returns a validated print context from effective renderer options."
  @spec context(keyword()) :: {:ok, context()} | {:error, {atom(), map()}}
  def context(opts) do
    with :ok <- validate_options(opts) do
      viewport =
        case Keyword.get(opts, :print_viewport) do
          nil ->
            with {:ok, {width, height}} <-
                   PageGeometry.normalize_page_size(Keyword.get(opts, :page_size, :a4)),
                 {:ok, margins} <- PageGeometry.normalize_margins(Keyword.get(opts, :margin, 0)),
                 :ok <- HtmlValidator.validate_page_size({width, height}),
                 :ok <- HtmlValidator.validate_margins(margins),
                 :ok <- HtmlValidator.validate_printable_area({width, height}, margins) do
              {:ok,
               {(width - margins.left - margins.right) / 0.75,
                (height - margins.top - margins.bottom) / 0.75}}
            else
              {:error, _reason} ->
                Diagnostics.error(
                  :options,
                  :invalid_options,
                  "print media selection requires a positive printable page area",
                  module: __MODULE__
                )
            end

          viewport ->
            {:ok, viewport}
        end

      with {:ok, {width, height}} <- viewport do
        {:ok,
         %{
           width: width * 1.0,
           height: height * 1.0,
           density: Keyword.get(opts, :image_selection_density, 1) * 1.0,
           type: :print,
           source_font_options:
             Keyword.take(opts, [:default_font, :fonts, :system_font_discovery])
         }}
      end
    end
  end

  @doc "Returns stylesheet media context from the requested page box before CSS @page size."
  @spec renderer_context(keyword()) :: {:ok, context()} | {:error, {atom(), map()}}
  def renderer_context(opts) do
    with :ok <- validate_options(opts) do
      viewport =
        case Keyword.get(opts, :print_viewport) do
          nil ->
            with {:ok, {width, height}} <-
                   PageGeometry.normalize_page_size(Keyword.get(opts, :page_size, :a4)),
                 :ok <- HtmlValidator.validate_page_size({width, height}),
                 do: {:ok, {width / 0.75, height / 0.75}}

          viewport ->
            {:ok, viewport}
        end

      case viewport do
        {:ok, {width, height}} ->
          {:ok,
           %{
             width: width * 1.0,
             height: height * 1.0,
             density: Keyword.get(opts, :image_selection_density, 1) * 1.0,
             type: :print,
             source_font_options:
               Keyword.take(opts, [:default_font, :fonts, :system_font_discovery])
           }}

        {:error, reason} ->
          Diagnostics.error(:layout, reason, "layout failed: #{reason}", module: __MODULE__)
      end
    end
  end

  @doc "Builds media context from a nonnegative child viewport in points, including scrollbar gutters."
  @spec client_context(keyword(), {number(), number()}) ::
          {:ok, context()} | {:error, {atom(), map()}}
  def client_context(opts, {width, height}) do
    with :ok <- validate_options(opts),
         {:ok, {width, height}} <- HtmlValidator.prepare_viewport_dimensions(width, height) do
      {:ok,
       %{
         width: width / 0.75,
         height: height / 0.75,
         density: Keyword.get(opts, :image_selection_density, 1) * 1.0,
         type: :print,
         source_font_options: Keyword.take(opts, [:default_font, :fonts, :system_font_discovery])
       }}
    end
  end

  alias NativeElixirPdfUtilities.Validators.{
    CssAliasValidator,
    CssConditionalValueValidator,
    MediaConditionValidator
  }

  @doc "Returns the environment part of a stylesheet cache key, omitting invariant media types."
  @spec cache_context(String.t(), context() | nil | :unpositioned) ::
          context() | nil | :unpositioned | :print
  def cache_context(css, context) do
    if String.contains?(css, "@") and
         NativeElixirPdfUtilities.HtmlToPdf.RenderCache.fetch_stylesheet(
           {:media_context_required, css},
           fn -> media_context_required?(css) end
         ), do: context, else: :print
  end

  defp media_context_required?(css) do
    css
    |> NativeElixirPdfUtilities.Validators.CssSyntaxValidator.block_items(true)
    |> Enum.any?(fn rule ->
      if rule.kind == :rule and Map.get(rule, :block, false) do
        case HtmlValidator.trim_css_whitespace(rule.selectors) do
          <<"@", name::binary>> ->
            case HtmlValidator.parse_css_identifier(name) do
              {:ok, name, query} ->
                (String.downcase(name, :ascii) in ["media", "when", "else"] and
                   String.contains?(query, "(")) or
                  media_context_required?(rule.declarations)

              _ ->
                false
            end

          _ ->
            media_context_required?(rule.declarations)
        end
      else
        false
      end
    end)
  end

  @doc "Checks stylesheet applicability and diagnoses viewport-qualified page defaults during page preparation."
  @spec matches_stylesheet?(
          String.t(),
          String.t(),
          context() | nil | :unpositioned
        ) :: boolean()
  def matches_stylesheet?(query, css, context), do: matches_stylesheet?(query, css, context, %{})

  defp matches_stylesheet?(query, css, context, aliases) do
    if String.contains?(query, "(") and
         Regex.match?(@page_candidate, css) do
      throw(
        {:render_resource_limit,
         Diagnostics.error(
           :style,
           :not_implemented,
           "viewport-qualified @page defaults require page/media negotiation that is not implemented; keep page size and margins outside viewport conditions",
           module: __MODULE__,
           source: query
         )}
      )
    end

    matches?(query, context, aliases)
  end

  @doc "Selects bounded nested media blocks without changing their source order. Nil retains the legacy media-type-only API."
  @spec select_rules(String.t(), context() | nil | :unpositioned, map() | nil, map() | nil) ::
          {:ok, String.t()}
  def select_rules(css, context, aliases \\ nil, namespaces \\ nil) do
    aliases = aliases || CssAliasValidator.definitions(css, :media)

    with {:ok, selected, _records} <-
           select_media(css, context, 0, :active, 0, aliases, namespaces),
         do: {:ok, selected}
  end

  @doc false
  @spec project_source_rules(
          String.t(),
          context() | nil | :unpositioned,
          map() | nil,
          map() | nil
        ) ::
          {:ok, String.t(), [map()]}
  def project_source_rules(css, context, aliases \\ nil, namespaces \\ nil) do
    aliases = aliases || CssAliasValidator.definitions(css, :media)
    select_media(css, context, 0, :source, 0, aliases, namespaces)
  end

  defp select_media(css, context, depth, mode, origin, aliases, namespaces) do
    alias NativeElixirPdfUtilities.Validators.CssSyntaxValidator
    HtmlValidator.check_render_resource(:max_depth, depth, :css)
    HtmlValidator.reserve_render_resource(:max_work, byte_size(css), :css)

    # Projected callers supply the original bindings. Standalone stylesheet
    # callers retain their source offsets and only need bindings for qualified names.
    namespaces =
      namespaces ||
        if(String.contains?(css, "|"),
          do: elem(CssSyntaxValidator.stylesheet_projection(css), 1),
          else: %{}
        )

    {output, records, cursor, _chain} =
      css
      |> CssSyntaxValidator.block_items(origin == 0)
      |> Enum.reduce({[], [], 0, nil}, fn rule, {output, records, cursor, chain} ->
        selector =
          if rule.kind == :rule, do: HtmlValidator.trim_css_whitespace(rule.selectors), else: ""

        media =
          case selector do
            <<"@", name::binary>> ->
              case HtmlValidator.parse_css_identifier(name) do
                {:ok, name, query} ->
                  name = String.downcase(name, :ascii)
                  if name in ["media", "supports", "when", "else"], do: {name, query}

                _ ->
                  nil
              end

            _ ->
              nil
          end

        {replacement, new_records, next_chain} =
          if is_tuple(media) do
            {kind, query} = media
            body_at = rule.offset + byte_size(rule.selectors) + 1

            trimmed = HtmlValidator.trim_css_whitespace(query)

            active =
              rule.block and
                case kind do
                  "media" ->
                    trimmed != "" and
                      matches_stylesheet?(query, rule.declarations, context, aliases)

                  "supports" ->
                    trimmed != "" and
                      NativeElixirPdfUtilities.Validators.CssSupportsValidator.matches?(
                        query,
                        namespaces
                      )

                  "when" ->
                    CssConditionalValueValidator.matches?(
                      trimmed,
                      context,
                      aliases,
                      %{},
                      namespaces
                    )

                  "else" ->
                    chain == false and
                      (trimmed == "" or
                         CssConditionalValueValidator.matches?(
                           trimmed,
                           context,
                           aliases,
                           %{},
                           namespaces
                         ))
                end

            next_chain =
              if kind == "else" and is_nil(chain),
                do: nil,
                else: active or (kind == "else" and chain)

            {:ok, selected, children} =
              if rule.block and (active or mode == :source),
                do:
                  select_media(
                    rule.declarations,
                    context,
                    depth + if(kind == "media", do: 1, else: 0),
                    mode,
                    origin + body_at,
                    aliases,
                    namespaces
                  ),
                else: {:ok, "", []}

            if mode == :source do
              leading = byte_size(rule.selectors) - byte_size(String.trim_leading(rule.selectors))

              record = %{
                offset: origin + rule.offset + leading,
                length: byte_size(rule.source) - leading,
                query: query,
                active: active,
                body_offset: origin + body_at,
                body_length: byte_size(rule.declarations)
              }

              record = if kind == "supports", do: Map.put(record, :kind, :supports), else: record

              projected =
                if rule.block,
                  do:
                    String.duplicate(" ", byte_size(rule.selectors) + 1) <>
                      selected <> if(rule.closed, do: " ", else: ""),
                  else: String.duplicate(" ", byte_size(rule.source))

              {projected, [record | children], next_chain}
            else
              {selected, [], next_chain}
            end
          else
            if rule.kind == :rule and rule.block and not definition_rule?(selector) do
              body_at = rule.offset + byte_size(rule.selectors) + 1

              {:ok, body, children} =
                select_media(
                  rule.declarations,
                  context,
                  depth,
                  mode,
                  origin + body_at,
                  aliases,
                  namespaces
                )

              {rule.selectors <> "{" <> body <> if(rule.closed, do: "}", else: ""), children, nil}
            else
              {rule.source, [], nil}
            end
          end

        before = binary_part(css, cursor, rule.offset - cursor)

        {[replacement, before | output], [new_records | records],
         rule.offset + byte_size(rule.source), next_chain}
      end)

    tail = binary_part(css, cursor, byte_size(css) - cursor)

    {:ok, IO.iodata_to_binary(Enum.reverse([tail | output])),
     records |> Enum.reverse() |> List.flatten()}
  end

  defp definition_rule?(selector) do
    case selector do
      <<"@", tail::binary>> ->
        case HtmlValidator.parse_css_identifier(tail) do
          {:ok, name, _} -> String.downcase(name, :ascii) in ["function", "mixin"]
          _ -> false
        end

      _ ->
        false
    end
  end

  @doc "Evaluates a media query list against a validated print context. Unknown features do not match."
  @spec matches?(String.t(), context() | nil | :unpositioned) :: boolean()
  def matches?(query, context), do: matches?(query, context, %{})

  @doc "Matches media queries with the active stylesheet's custom media aliases."
  @spec matches?(String.t(), context() | nil | :unpositioned, map()) :: boolean()
  def matches?(query, context, aliases) do
    query =
      query
      |> NativeElixirPdfUtilities.HtmlToPdf.CssParser.strip_comments()
      |> HtmlValidator.trim_css_whitespace()

    query = if map_size(aliases) == 0, do: String.downcase(query, :ascii), else: query

    if map_size(aliases) > 0 and String.contains?(query, "--") do
      alias_truth(query, context, aliases, []) == true
    else
      ordinary_match?(String.downcase(query, :ascii), context)
    end
  end

  defp ordinary_match?(query, context) do
    if context in [nil, :unpositioned] do
      if context == :unpositioned and String.contains?(query, "(") do
        throw(
          {:render_resource_limit,
           Diagnostics.error(
             :style,
             :not_implemented,
             "viewport-dependent child styles require prepare_iframe_document and style_viewport with an explicit client viewport",
             module: __MODULE__,
             source: query
           )}
        )
      end

      query in ["", "print", "only print", "all", "only all"]
    else
      query == "" or Enum.any?(HtmlValidator.split_css_comma(query), &condition(&1, context, 0))
    end
  end

  defp alias_truth(query, context, aliases, stack) do
    HtmlValidator.check_render_resource(:max_depth, length(stack), :css)
    HtmlValidator.reserve_render_resource(:max_work, byte_size(query), :css)

    case String.downcase(String.trim(query), :ascii) do
      "true" when stack != [] ->
        true

      "false" when stack != [] ->
        false

      _ ->
        HtmlValidator.split_css_comma(query)
        |> Enum.reduce(false, fn condition, previous ->
          truth =
            MediaConditionValidator.truth(
              condition,
              context,
              fn value, context ->
                case HtmlValidator.parse_css_identifier(value) do
                  {:ok, "--" <> _ = name, ""} ->
                    with {:ok, alias_query} <- Map.fetch(aliases, name),
                         false <- name in stack do
                      alias_truth(alias_query, context, aliases, [name | stack])
                    else
                      _ -> :unknown
                    end

                  _ ->
                    if is_map(context), do: feature(value, context), else: :unknown
                end
              end,
              true
            )

          cond do
            previous == true or truth == true -> true
            previous == :unknown or truth == :unknown -> :unknown
            true -> false
          end
        end)
    end
  end

  @doc "Resolves a nonnegative CSS source-size length to CSS pixels, or returns :error."
  @spec source_length(String.t(), context()) ::
          {:ok, float()} | :error | {:error, {atom(), map()}}
  def source_length(value, context) do
    HtmlValidator.with_render_budget(fn -> evaluated_length(value, context, :nonnegative) end)
  end

  defp evaluated_length(value, context, sign_policy) do
    value = String.downcase(HtmlValidator.trim_css_whitespace(value), :ascii)

    context =
      case NativeElixirPdfUtilities.Validators.SourceSizeValidator.prepare_context(
             context,
             [value],
             []
           ) do
        {:ok, measured} -> measured
        {:error, _} = error -> throw({:render_resource_limit, error})
      end

    parsed =
      if length(NativeElixirPdfUtilities.Validators.CssMathValidator.component_terms(value, true)) ==
           1,
         do: NativeElixirPdfUtilities.Validators.CssMathValidator.evaluate(value, context),
         else: :error

    parsed =
      case parsed do
        {:ok, :nan, %{length: 1} = type} when map_size(type) == 1 ->
          {:ok, 0.0}

        {:ok, value, %{length: 1} = type} when is_number(value) and map_size(type) == 1 ->
          {:ok, value}

        {:ok, number, type} when number == 0 and map_size(type) == 0 ->
          {:ok, 0.0}

        {:ok, special, %{length: 1} = type}
        when special in [:infinity, :negative_infinity] and map_size(type) == 1 ->
          throw(
            {:render_resource_limit,
             Diagnostics.error(
               :style,
               :resource_limit_exceeded,
               "source-size arithmetic is outside the finite numeric range",
               module: __MODULE__,
               source: value
             )}
          )

        _ ->
          :error
      end

    case parsed do
      {:ok, value} when value >= 0 or sign_policy == :signed ->
        HtmlValidator.check_render_resource(:max_numeric_magnitude, abs(value), :style)
        HtmlValidator.normalize_geometry_number(value)

      _ ->
        :error
    end
  end

  defp positive_number?(value) do
    case HtmlValidator.normalize_geometry_number(value) do
      {:ok, value} -> value > 0
      _ -> false
    end
  end

  defp condition(query, context, _depth) do
    NativeElixirPdfUtilities.Validators.MediaConditionValidator.evaluate(
      query,
      context,
      &feature/2
    )
  end

  @doc "Evaluates the shared range grammar against a selected query container."
  @spec container_feature(String.t(), map()) :: boolean() | :unknown
  def container_feature(value, context), do: feature(value, context)

  defp feature(value, context) do
    value = HtmlValidator.trim_css_whitespace(value)

    value =
      value
      |> NativeElixirPdfUtilities.Validators.CssMathValidator.component_terms(:ranges)
      |> Enum.map_join(" ", fn token ->
        case HtmlValidator.parse_css_identifier(token) do
          {:ok, identifier, ""} -> String.downcase(identifier, :ascii)
          {:ok, identifier, ":" <> rest} -> String.downcase(identifier, :ascii) <> ":" <> rest
          _ -> token
        end
      end)

    case Regex.run(~r/\A([a-z-]+)\s*:\s*(.+)\z/s, value) do
      [_, "grid", expected] ->
        case NativeElixirPdfUtilities.Validators.CssMathValidator.scalar(expected, :integer) do
          {:ok, value} when value in [0, 1] -> value == 0
          _ -> :unknown
        end

      [_, "orientation", orientation] ->
        if orientation in ~w(landscape portrait),
          do:
            orientation ==
              if(
                Map.get(context, :query_width, context.width) >
                  Map.get(context, :query_height, context.height),
                do: "landscape",
                else: "portrait"
              ),
          else: :unknown

      [_, name, expected] when is_map_key(@discrete_features, name) ->
        {actual, allowed} = Map.fetch!(@discrete_features, name)

        case HtmlValidator.parse_css_identifier(HtmlValidator.trim_css_whitespace(expected)) do
          {:ok, expected, ""} ->
            expected = String.downcase(expected, :ascii)
            if expected in allowed, do: actual == expected, else: :unknown

          _ ->
            :unknown
        end

      [_, name, expected] ->
        {comparison, name} =
          cond do
            String.starts_with?(name, "min-") -> {:min, String.slice(name, 4..-1//1)}
            String.starts_with?(name, "max-") -> {:max, String.slice(name, 4..-1//1)}
            true -> {:equal, name}
          end

        with {:ok, actual, expected} <- feature_values(name, expected, context) do
          compare(actual, expected, comparison)
        else
          _ -> :unknown
        end

      _ ->
        range_feature(value, context)
    end
  end

  defp range_feature(value, context) do
    case Regex.run(@forward_range, value) do
      [_, name, operator, expected] ->
        with {:ok, actual, expected} <- feature_values(name, expected, context) do
          compare(actual, expected, operator)
        else
          _ -> :unknown
        end

      _ ->
        case Regex.run(@reverse_range, value) do
          [_, expected, operator, name, second_operator, second_expected] ->
            with true <-
                   (operator in ["<", "<="] and second_operator in ["<", "<="]) or
                     (operator in [">", ">="] and second_operator in [">", ">="]),
                 {:ok, actual, first} <- feature_values(name, expected, context),
                 {:ok, _, second} <- feature_values(name, second_expected, context) do
              compare(first, actual, operator) and compare(actual, second, second_operator)
            else
              _ -> :unknown
            end

          [_, expected, operator, name] ->
            with {:ok, actual, expected} <- feature_values(name, expected, context) do
              compare(expected, actual, operator)
            else
              _ -> :unknown
            end

          _ ->
            case value do
              "width" ->
                Map.get(context, :query_width, context.width) > 0

              "height" ->
                Map.get(context, :query_height, context.height) > 0

              "inline-size" when is_map_key(context, :query_inline_size) ->
                context.query_inline_size > 0

              "block-size" when is_map_key(context, :query_block_size) ->
                context.query_block_size > 0

              "aspect-ratio" ->
                Map.get(context, :query_width, context.width) > 0 and
                  Map.get(context, :query_height, context.height) > 0

              "grid" ->
                false

              "resolution" ->
                true

              name when is_map_key(@numeric_features, name) ->
                Map.fetch!(@numeric_features, name) > 0

              name when is_map_key(@discrete_features, name) ->
                elem(Map.fetch!(@discrete_features, name), 0) not in ["none", "no-preference"]

              _ ->
                :unknown
            end
        end
    end
  end

  defp feature_values(name, expected, context) do
    case name do
      name when is_map_key(@numeric_features, name) ->
        with {:ok, expected} <-
               NativeElixirPdfUtilities.Validators.CssMathValidator.scalar(expected, :integer),
             do: {:ok, Map.fetch!(@numeric_features, name), expected}

      name when name in ~w(width height) ->
        with {:ok, expected} <- evaluated_length(expected, context, :signed),
             do:
               {:ok,
                if(name == "width",
                  do: Map.get(context, :query_width, context.width),
                  else: Map.get(context, :query_height, context.height)
                ), expected}

      "inline-size" when is_map_key(context, :query_inline_size) ->
        with {:ok, expected} <- evaluated_length(expected, context, :signed),
             do: {:ok, context.query_inline_size, expected}

      "block-size" when is_map_key(context, :query_block_size) ->
        with {:ok, expected} <- evaluated_length(expected, context, :signed),
             do: {:ok, context.query_block_size, expected}

      "resolution" ->
        case HtmlValidator.parse_css_number(expected) do
          {value, unit} when value >= 0 and unit in ~w(dppx dpi dpcm x) ->
            scale =
              case unit do
                "dpi" -> 1 / 96
                "dpcm" -> 2.54 / 96
                _ -> 1
              end

            {:ok, context.density, value * scale}

          _ ->
            :error
        end

      "aspect-ratio" ->
        width = Map.get(context, :query_width, context.width)
        height = Map.get(context, :query_height, context.height)

        case String.split(expected, "/")
             |> Enum.map(&HtmlValidator.parse_css_number(String.trim(&1))) do
          [{numerator, ""}, {denominator, ""}]
          when numerator >= 0 and denominator > 0 and height > 0 ->
            {:ok, width / height, numerator / denominator}

          [{ratio, ""}] when ratio >= 0 and height > 0 ->
            {:ok, width / height, ratio}

          _ ->
            :error
        end

      _ ->
        :error
    end
  end

  defp compare(actual, expected, operator) do
    case operator do
      op when op in [:min, ">="] -> actual >= expected
      op when op in [:max, "<="] -> actual <= expected
      ">" -> actual > expected
      "<" -> actual < expected
      _ -> actual == expected
    end
  end
end
