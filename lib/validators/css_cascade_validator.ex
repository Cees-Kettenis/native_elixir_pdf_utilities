defmodule NativeElixirPdfUtilities.Validators.CssCascadeValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgStyleValidator}
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache

  @sides ~w(top right bottom left)
  @font_controls NativeElixirPdfUtilities.Validators.CssFontControlsValidator.properties()
  @simple NativeElixirPdfUtilities.Validators.CssPublishingValidator.properties() ++
            ~w(outline-width outline-style outline-color outline-offset box-shadow border-image-source border-image-slice border-image-width border-image-outset border-image-repeat image-orientation image-resolution image-rendering color-scheme print-color-adjust forced-color-adjust color accent-color appearance content-visibility visibility resize zoom field-sizing scrollbar-width scrollbar-color scrollbar-gutter box-sizing border-radius
    text-transform word-spacing text-indent text-align-last text-justify hanging-punctuation quotes content counter-reset counter-increment counter-set letter-spacing
    text-emphasis-style text-emphasis-color text-emphasis-position text-shadow text-underline-offset text-underline-position text-decoration-skip-ink initial-letter initial-letter-align initial-letter-wrap baseline-source alignment-baseline baseline-shift dominant-baseline width height min-width max-width min-height max-height orphans widows
    ruby-align ruby-position ruby-merge ruby-overhang writing-mode text-orientation text-combine-upright direction unicode-bidi position z-index isolation opacity mix-blend-mode contain contain-intrinsic-width contain-intrinsic-height container-name container-type object-fit object-position overflow-x overflow-y overflow-clip-margin text-overflow block-ellipsis max-lines continue aspect-ratio border-collapse
    border-spacing caption-side table-layout empty-cells column-count column-width column-fill column-span column-rule-width column-rule-style column-rule-color float clear shape-outside shape-inside shape-margin shape-image-threshold flex-direction flex-wrap row-gap column-gap
    justify-content align-items align-self justify-self justify-items align-content order reading-flow reading-order
    flex-grow flex-shrink flex-basis grid-template-columns grid-template-rows grid-auto-columns
    grid-auto-rows grid-template-areas grid-auto-flow grid-column-start grid-column-end grid-row-start grid-row-end
    white-space white-space-collapse white-space-trim text-wrap-mode text-wrap-style tab-size word-break overflow-wrap line-break math-style math-shift math-depth) ++
            NativeElixirPdfUtilities.Validators.CssHyphenationValidator.properties()
  @fields Map.new(@simple, fn name ->
            {name, [[String.to_atom(String.replace(name, "-", "_"))]]}
          end)
          |> Map.merge(%{
            "-webkit-text-fill-color" => [[:text_fill_color]],
            "-webkit-text-stroke-color" => [[:text_stroke_color]],
            "-webkit-text-stroke-width" => [[:text_stroke_width]],
            "row-gap" => [[:row_gap], [:_row_gap_normal]],
            "column-gap" => [[:column_gap], [:_column_gap_normal]],
            "font-size" => [[:font_size]],
            "font-weight" => [[:font_weight]],
            "font-style" => [[:font_style], [:font_oblique]],
            "font-width" => [[:font_width]],
            "font-family" => [
              [:font_families],
              [:font_family],
              [:font_face],
              [:_font_family_scope]
            ],
            "line-height" => [
              [:line_height],
              [:line_height_normal],
              [:line_height_multiplier],
              [:line_height_explicit],
              [:_line_height_parent],
              [:_line_height_root]
            ],
            "display" => [
              [:display],
              [:_math_display],
              [:_math_formatting],
              [:_ruby_annotation],
              [:_flow_root],
              [:_display_role],
              [:_display_outer],
              [:table_section]
            ],
            "text-align" => [[:text_align], [:_text_align_initial]],
            "shape-outside" => [[:shape_outside], [:shape_image_source], [:shape_image]],
            "border-top-left-radius" => [[:border_radii, :top_left]],
            "border-top-right-radius" => [[:border_radii, :top_right]],
            "border-bottom-right-radius" => [[:border_radii, :bottom_right]],
            "border-bottom-left-radius" => [[:border_radii, :bottom_left]],
            "background-origin" => [[:background_origins]],
            "background-clip" => [[:background_clips]],
            "background-attachment" => [[:background_attachments]],
            "background-color" => [[:background_color]],
            "background-blend-mode" => [[:background_blend_modes]],
            "background-image" => [
              [:background_image],
              [:background_image_source],
              [:background_images]
            ],
            "background-position" => [[:background_position], [:background_positions]],
            "background-size" => [[:background_size], [:background_sizes]],
            "background-repeat" => [[:background_repeat], [:background_repeats]],
            "list-style-type" => [[:list_marker_type]],
            "list-style-position" => [[:list_style_position]],
            "list-style-image" => [[:list_style_image], [:list_style_image_source]],
            "page" => [[:page_name]],
            "float" => [[:float], [:publishing_float]],
            "position" => [[:position], [:running_name], [:running_copy]],
            "running" => [[:running_name], [:running_copy]],
            "counter-increment" => [[:counter_increment], [:counter_increment_implicit]],
            "break-before" => [[:break_before]],
            "break-after" => [[:break_after]],
            "break-inside" => [[:break_inside]],
            "clip-path" => [[:css_clip_path]],
            "filter" => [[:css_filter]],
            "backdrop-filter" => [[:css_backdrop_filter]],
            "offset-path" => [[:css_offset_path]],
            "offset-position" => [[:css_offset_position]],
            "offset-anchor" => [[:css_offset_anchor]],
            "offset-distance" => [[:css_offset_distance]],
            "offset-rotate" => [[:css_offset_rotate]],
            "perspective" => [[:css_perspective]],
            "perspective-origin" => [[:css_perspective_origin]],
            "transform-style" => [[:css_transform_style]],
            "backface-visibility" => [[:css_backface_visibility]],
            "translate" => [[:css_translate], [:css_translate_metric_style]],
            "rotate" => [[:css_rotate], [:css_rotate_metric_style]],
            "scale" => [[:css_scale], [:css_scale_metric_style]],
            "transform-box" => [[:css_transform_box]],
            "transform" => [[:css_transform], [:css_transform_metric_style]],
            "transform-origin" => [[:transform_origin], [:transform_origin_metric_style]],
            "text-decoration-line" => [[:text_decoration_line]],
            "text-decoration-color" => [[:text_decoration_color]],
            "text-decoration-style" => [[:text_decoration_style]],
            "text-decoration-thickness" => [[:text_decoration_thickness]]
          })
          |> Map.merge(
            NativeElixirPdfUtilities.Validators.CssFontControlsValidator.cascade_fields()
          )
          |> Map.merge(NativeElixirPdfUtilities.Validators.CssMaskValidator.cascade_fields())
          |> then(fn fields ->
            Enum.reduce(@sides, fields, fn side, fields ->
              edge = String.to_atom(side)

              fields
              |> Map.put(
                "margin-" <> side,
                [[:margin, edge]] ++
                  case side do
                    "top" -> [[:_margin_before_em]]
                    "bottom" -> [[:margin_after], [:_margin_after_em]]
                    _ -> []
                  end
              )
              |> Map.put("padding-" <> side, [[:padding, edge]])
              |> Map.put("border-" <> side <> "-width", [[:border_widths, edge]])
              |> Map.put("border-" <> side <> "-color", [[:border_colors, edge]])
              |> Map.put("border-" <> side <> "-style", [[:border_styles, edge]])
              |> Map.put(side, [[:offsets, edge]])
            end)
          end)
  @shorthands %{
                "offset" =>
                  ~w(offset-path offset-position offset-anchor offset-distance offset-rotate),
                "mask" =>
                  ~w(mask-image mask-mode mask-repeat mask-position mask-size mask-origin mask-clip mask-composite mask-border-source mask-border-slice mask-border-width mask-border-outset mask-border-repeat mask-border-mode),
                "mask-border" =>
                  ~w(mask-border-source mask-border-slice mask-border-width mask-border-outset mask-border-repeat mask-border-mode),
                "outline" => ~w(outline-width outline-style outline-color),
                "-webkit-text-stroke" => ~w(-webkit-text-stroke-width -webkit-text-stroke-color),
                "vertical-align" => ~w(baseline-source alignment-baseline baseline-shift),
                "text-emphasis" => ~w(text-emphasis-style text-emphasis-color),
                "white-space" => ~w(white-space-collapse text-wrap-mode white-space-trim),
                "text-wrap" => ~w(text-wrap-mode text-wrap-style),
                "overflow" => ~w(overflow-x overflow-y),
                "line-clamp" => ~w(max-lines block-ellipsis continue),
                "margin" => Enum.map(@sides, &("margin-" <> &1)),
                "padding" => Enum.map(@sides, &("padding-" <> &1)),
                "border-image" =>
                  ~w(border-image-source border-image-slice border-image-width border-image-outset border-image-repeat),
                "border-radius" =>
                  ~w(border-top-left-radius border-top-right-radius border-bottom-right-radius border-bottom-left-radius),
                "border-width" => Enum.map(@sides, &("border-" <> &1 <> "-width")),
                "border-color" => Enum.map(@sides, &("border-" <> &1 <> "-color")),
                "border-style" => Enum.map(@sides, &("border-" <> &1 <> "-style")),
                "border" =>
                  for(side <- @sides, kind <- ~w(width color style), do: "border-#{side}-#{kind}") ++
                    ~w(border-image-source border-image-slice border-image-width border-image-outset border-image-repeat),
                "background" =>
                  ~w(background-color background-image background-position background-size background-repeat background-origin background-clip background-attachment),
                "font" =>
                  ~w(font-size line-height font-family font-weight font-style font-width) ++
                    NativeElixirPdfUtilities.Validators.CssFontControlsValidator.font_resets(),
                "font-variant" =>
                  NativeElixirPdfUtilities.Validators.CssFontControlsValidator.variant_properties(),
                "font-synthesis" =>
                  NativeElixirPdfUtilities.Validators.CssFontControlsValidator.synthesis_properties(),
                "list-style" => ~w(list-style-type list-style-position list-style-image),
                "text-decoration" =>
                  ~w(text-decoration-line text-decoration-color text-decoration-style text-decoration-thickness),
                "columns" => ~w(column-count column-width),
                "container" => ~w(container-name container-type),
                "contain-intrinsic-size" => ~w(contain-intrinsic-width contain-intrinsic-height),
                "column-rule" => ~w(column-rule-width column-rule-style column-rule-color),
                "gap" => ~w(row-gap column-gap),
                "place-content" => ~w(align-content justify-content),
                "place-items" => ~w(align-items justify-items),
                "place-self" => ~w(align-self justify-self),
                "flex-flow" => ~w(flex-direction flex-wrap),
                "flex" => ~w(flex-grow flex-shrink flex-basis),
                "grid-template" =>
                  ~w(grid-template-columns grid-template-rows grid-template-areas),
                "grid" =>
                  ~w(grid-template-columns grid-template-rows grid-template-areas grid-auto-columns grid-auto-rows grid-auto-flow),
                "grid-column" => ~w(grid-column-start grid-column-end),
                "grid-row" => ~w(grid-row-start grid-row-end),
                "grid-area" => ~w(grid-row-start grid-column-start grid-row-end grid-column-end)
              }
              |> Map.merge(
                Map.new(@sides, fn side ->
                  {"border-" <> side, Enum.map(~w(width color style), &"border-#{side}-#{&1}")}
                end)
              )
  @aliases %{
    "font-stretch" => "font-width",
    "word-wrap" => "overflow-wrap",
    "page-break-before" => "break-before",
    "page-break-after" => "break-after",
    "page-break-inside" => "break-inside"
  }
  @aliases Map.merge(
             @aliases,
             Map.new(
               ~w(background-clip background-origin background-size border-bottom-left-radius border-bottom-right-radius border-top-left-radius border-top-right-radius border-radius box-shadow box-sizing flex flex-basis flex-direction flex-flow flex-grow flex-shrink flex-wrap justify-content align-content align-items align-self order appearance column-count column-width column-gap column-rule column-rule-color column-rule-style column-rule-width columns transform transform-origin print-color-adjust hyphens text-emphasis text-emphasis-style text-emphasis-color text-emphasis-position writing-mode text-orientation line-clamp),
               &{"-webkit-" <> &1, &1}
             )
           )
  @aliases Map.merge(@aliases, %{
             "-webkit-box-align" => "align-items",
             "-webkit-box-flex" => "flex-grow",
             "-webkit-box-ordinal-group" => "order",
             "-webkit-box-orient" => "flex-direction",
             "-webkit-box-pack" => "justify-content"
           })

  @doc "Returns the standard owner for a selected static compatibility spelling."
  @spec canonical_property(String.t()) :: String.t()
  @compile {:inline, canonical_property: 1}
  def canonical_property(property) do
    case property do
      "-webkit-" <> _ -> Map.get(@aliases, property, property)
      _ -> property
    end
  end

  @doc "Maps legacy static box keywords before invoking the standard owner's validator."
  @spec normalize_declaration(String.t(), String.t()) :: {String.t(), String.t()}
  def normalize_declaration(property, value) do
    mapped =
      case {property, value} do
        {"display", value} ->
          Map.get(
            %{
              "-webkit-box" => "flex",
              "-webkit-flex" => "flex",
              "-webkit-inline-box" => "inline-flex",
              "-webkit-inline-flex" => "inline-flex"
            },
            value,
            value
          )

        {"-webkit-box-orient", value} ->
          Map.get(
            %{
              "horizontal" => "row",
              "inline-axis" => "row",
              "vertical" => "column",
              "block-axis" => "column"
            },
            value,
            value
          )

        {"-webkit-box-align", value} ->
          Map.get(%{"start" => "flex-start", "end" => "flex-end"}, value, value)

        {"-webkit-box-pack", value} ->
          Map.get(
            %{"start" => "flex-start", "end" => "flex-end", "justify" => "space-between"},
            value,
            value
          )

        _ ->
          value
      end

    {canonical_property(property), mapped}
  end

  @inherited ~w(scrollbar-color -webkit-text-fill-color -webkit-text-stroke-color -webkit-text-stroke-width image-orientation image-resolution image-rendering color-scheme print-color-adjust forced-color-adjust color accent-color font-size font-family font-style font-weight font-width line-height
    dominant-baseline letter-spacing word-spacing text-indent text-align-last text-justify hanging-punctuation text-align text-transform quotes white-space white-space-collapse text-wrap-mode text-wrap-style tab-size word-break overflow-wrap line-break
    text-emphasis-style text-emphasis-color text-emphasis-position text-shadow text-underline-offset text-underline-position text-decoration-skip-ink initial-letter-align initial-letter-wrap orphans widows caption-side border-collapse border-spacing empty-cells list-style-type list-style-position
    list-style-image visibility block-ellipsis ruby-align ruby-position ruby-merge ruby-overhang writing-mode text-orientation text-combine-upright direction math-style math-shift math-depth) ++
               @font_controls ++
               NativeElixirPdfUtilities.Validators.CssHyphenationValidator.properties()
  @inherited_fields @inherited
                    |> Enum.flat_map(fn property ->
                      Enum.map(Map.fetch!(@fields, property), &hd/1)
                    end)
                    |> Enum.uniq()
                    |> Enum.reject(&(&1 == :line_height_explicit))
                    |> Kernel.++([:_force_hidden])

  @doc "Returns inherited computed CSS fields from the shared property policy."
  @spec inherited_fields() :: [atom()]
  def inherited_fields, do: @inherited_fields

  @wide ~w(initial inherit unset revert revert-layer)

  @doc "Recognizes properties owned by the static cascade and its shorthand registry."
  @spec supported_property?(String.t()) :: boolean()
  def supported_property?(property) do
    property = Map.get(@aliases, property, property)

    String.starts_with?(property, "--") or property == "all" or
      Map.has_key?(@fields, property) or Map.has_key?(@shorthands, property) or
      NativeElixirPdfUtilities.Validators.CssLogicalValidator.logical?(property) or
      Map.has_key?(SvgStyleValidator.presentation_properties(), property)
  end

  @doc "Combines document registrations without admitting definitions from a shadow stylesheet."
  @spec registrations([map()]) :: map()
  def registrations(entries) do
    Enum.reduce(entries, %{}, fn entry, registrations ->
      if is_nil(Map.get(entry, :shadow_scope_id)) and
           Map.get(entry.prepared, :stylesheet_applicable, true),
         do:
           Map.merge(
             registrations,
             entry.prepared |> Map.get(:cascade, %{}) |> Map.get(:registrations, %{})
           ),
         else: registrations
    end)
  end

  @doc "Combines layer order across stylesheets while keeping author trees and anonymous layers distinct."
  @spec layer_rules([map()]) :: [map()]
  def layer_rules(entries) do
    {entries, orders} =
      entries
      |> Enum.with_index()
      |> Enum.map_reduce(%{}, fn {entry, index}, orders ->
        scope = Map.get(entry, :shadow_scope_id)

        qualify = fn path ->
          Enum.map(path, fn name ->
            case name do
              {:anonymous, offset} -> {:anonymous, index, offset}
              name -> name
            end
          end)
        end

        events =
          if Map.get(entry.prepared, :stylesheet_applicable, true),
            do: entry.prepared |> Map.get(:cascade, %{}) |> Map.get(:layers, []),
            else: []

        orders =
          Enum.reduce(events, orders, fn event, orders ->
            path = qualify.(event.path)
            HtmlValidator.check_render_resource(:max_depth, length(path), :css)

            Enum.reduce(1..length(path), orders, fn length, orders ->
              path = Enum.take(path, length)
              key = {scope, Enum.drop(path, -1)}
              siblings = Map.get(orders, key, %{})
              siblings = Map.put_new(siblings, List.last(path), map_size(siblings))
              Map.put(orders, key, siblings)
            end)
          end)

        rules =
          Enum.map(entry.prepared.rules, fn rule ->
            Map.put(rule, :layer, qualify.(Map.get(rule, :layer, [])))
            |> Map.merge(Map.take(entry, [:shadow_scope_id, :shadow_host_id, :shadow_depth]))
            |> Map.put(
              :implicit_scope_root_id,
              Map.get(entry, :scope_owner_id, get_in(entry, [:node, :parent_id]))
            )
          end)

        {rules, orders}
      end)

    entries
    |> List.flatten()
    |> Enum.with_index(fn rule, index ->
      scope = Map.get(rule, :shadow_scope_id)
      path = Map.get(rule, :layer, [])

      layer_order =
        path
        |> Enum.with_index()
        |> Enum.map(fn {name, index} ->
          orders |> Map.fetch!({scope, Enum.take(path, index)}) |> Map.fetch!(name)
        end)

      rule |> Map.put(:order, index) |> Map.put(:layer_order, layer_order)
    end)
  end

  @doc "Orders validated author declarations including encapsulation, layers and scope proximity."
  @spec order([map()]) :: [map()]
  def order(declarations) do
    Enum.sort_by(declarations, fn declaration ->
      important = declaration.important
      encapsulation = Map.get(declaration, :encapsulation, 0)

      layer =
        Enum.map(Map.get(declaration, :layer_order, []), fn rank ->
          if important, do: {1, -rank}, else: {0, rank}
        end) ++ [if(important, do: {0, 0}, else: {1, 0})]

      proximity =
        case Map.get(declaration, :scope_proximity) do
          nil -> {0, 0}
          distance -> {1, -distance}
        end

      {important, if(important, do: -encapsulation, else: encapsulation), declaration.inline,
       layer, declaration.specificity, proximity, declaration.order,
       declaration.declaration_index}
    end)
  end

  @doc "Returns longhand assignments once for each shorthand without interpreting its value twice."
  @spec targets(String.t(), map()) :: [String.t()]
  def targets(property, context \\ %{}) do
    property = Map.get(@aliases, property, property)

    cond do
      property == "all" ->
        (Map.keys(@fields) -- ["direction", "unicode-bidi"]) ++
          if(Map.get(context, :_svg_element, false),
            do: Map.keys(SvgStyleValidator.presentation_properties()),
            else: []
          )

      Map.get(context, :_svg_element, false) and
          Map.has_key?(SvgStyleValidator.presentation_properties(), property) ->
        [property]

      true ->
        case NativeElixirPdfUtilities.Validators.CssLogicalValidator.physical_properties(
               property,
               context
             ) do
          nil -> Map.get(@shorthands, property, [property])
          physical -> Enum.flat_map(physical, &targets(&1, context))
        end
    end
  end

  @doc "Selects winning longhand assignments and rolls back origins and layers before computation."
  @spec plan([term()], map(), map()) :: [map()]
  def plan(declarations, context, keywords \\ %{}) do
    # Only winner selection is shared. Variables, fonts and resources retain each element's context.
    RenderCache.fetch_stylesheet(
      {__MODULE__, declarations, Map.get(context, :_svg_element, false),
       Map.get(context, :writing_mode, :horizontal_tb), Map.get(context, :direction, :ltr),
       Map.get(context, :text_orientation, :mixed), keywords},
      fn ->
        records =
          Enum.with_index(declarations, fn declaration, index ->
            record = if is_map(declaration), do: declaration, else: %{declaration: declaration}

            {property, literal_keyword} =
              case NativeElixirPdfUtilities.Validators.CssValueValidator.declaration_pair(record) do
                {property, value} -> {property, keyword(value)}
                nil -> {nil, nil}
              end

            record = Map.drop(record, [:cascade_keyword, :cascade_resolved])

            Map.merge(record, %{
              cascade_original: Map.get(record, :cascade_original, declaration),
              cascade_keyword: Map.get(keywords, index),
              cascade_literal_keyword: literal_keyword,
              cascade_index: index,
              cascade_targets: if(is_nil(property), do: [], else: targets(property, context))
            })
          end)

        case records do
          [record] ->
            if is_nil(winner([record])),
              do: [Map.put(record, :cascade_targets, [])],
              else: records

          [] ->
            []

          _ ->
            candidates =
              Enum.reduce(records, %{}, fn record, candidates ->
                Enum.reduce(record.cascade_targets, candidates, fn property, candidates ->
                  Map.update(candidates, property, [record], &[record | &1])
                end)
              end)

            winners =
              Map.new(candidates, fn {property, candidates} -> {property, winner(candidates)} end)

            Enum.map(records, fn record ->
              Map.put(
                record,
                :cascade_targets,
                Enum.filter(record.cascade_targets, &(winners[&1] == record.cascade_index))
              )
            end)
        end
      end
    )
  end

  defp winner(candidates) do
    case candidates do
      [] ->
        nil

      [candidate | rest] ->
        case Map.get(candidate, :cascade_keyword) || candidate.cascade_literal_keyword do
          "revert" ->
            nil

          "revert-layer" ->
            inline = Map.get(candidate, :inline, false)

            important =
              NativeElixirPdfUtilities.Validators.CssValueValidator.important_declaration?(
                candidate
              )

            winner(
              Enum.reject(rest, fn item ->
                same_tree =
                  Map.get(item, :definition_scope) == Map.get(candidate, :definition_scope)

                cond do
                  not same_tree ->
                    false

                  inline ->
                    Map.get(item, :inline, false)

                  important ->
                    NativeElixirPdfUtilities.Validators.CssValueValidator.important_declaration?(
                      item
                    ) or
                      normal_layer_key(item) >= normal_layer_key(candidate)

                  true ->
                    Map.get(item, :layer) == Map.get(candidate, :layer)
                end
              end)
            )

          _ ->
            candidate.cascade_index
        end
    end
  end

  defp normal_layer_key(declaration),
    do: Enum.map(Map.get(declaration, :layer_order, []), &{0, &1}) ++ [{1, 0}]

  @doc "Prepares substituted values once and selects any computed CSS-wide rollback."
  @spec prepare_values([map()], map(), :atomics.atomics_ref()) ::
          {:ok, [map()]} | {:error, term()}
  def prepare_values(declarations, context, budget) do
    prepared =
      Enum.map(declarations, fn declaration ->
        case NativeElixirPdfUtilities.Validators.CssValueValidator.declaration_pair(declaration) do
          {_property, value} ->
            NativeElixirPdfUtilities.Validators.CssSubstitutionValidator.resolve_value(
              value,
              context,
              budget
            )

          nil ->
            {:error, :invalid_document}
        end
      end)

    case Enum.find(prepared, &match?({:error, _}, &1)) do
      nil ->
        keywords =
          Enum.zip(declarations, prepared)
          |> Enum.with_index()
          |> Map.new(fn {{declaration, result}, index} ->
            computed_keyword =
              case result do
                {:ok, value} ->
                  if value == elem(declaration.declaration, 1), do: nil, else: keyword(value)

                _ ->
                  nil
              end

            {index, computed_keyword}
          end)

        controls_changed =
          Enum.with_index(declarations)
          |> Enum.any?(fn {declaration, index} ->
            not Map.has_key?(declaration, :cascade_targets) or
              Map.get(declaration, :cascade_keyword) != keywords[index]
          end)

        planned =
          if controls_changed, do: plan(declarations, context, keywords), else: declarations

        {:ok, Enum.zip_with(planned, prepared, &Map.put(&1, :cascade_resolved, &2))}

      error ->
        error
    end
  end

  @doc "Computes custom properties, including CSS-wide values produced by substitution, within the shared work budget."
  @spec resolve_custom([map()], map(), [map()], map(), :atomics.atomics_ref()) ::
          {:ok, map(), map(), [map()]} | {:error, term()}
  def resolve_custom(custom, context, ordinary, baseline, budget) do
    if custom == [] and context._custom_properties == %{} and
         Map.get(context, :_property_registrations, %{}) == %{} do
      with {:ok, prepared} <- prepare_values(ordinary, context, budget),
           do: {:ok, %{}, %{}, prepared}
    else
      resolve_custom_loop(
        custom,
        context,
        ordinary,
        baseline,
        context._custom_properties,
        budget,
        false
      )
    end
  end

  defp resolve_custom_loop(custom, context, ordinary, baseline, raw, budget, started) do
    HtmlValidator.reserve_render_resource(:max_work, 1, :css)
    alias NativeElixirPdfUtilities.Validators.CssPropertyRegistrationValidator

    with {:ok, values, sanitized} <-
           CssPropertyRegistrationValidator.resolve(raw, context, ordinary, budget, :provisional),
         actual = Map.put(context, :_custom_properties, values),
         {:ok, prepared} <- prepare_values(ordinary, actual, budget) do
      needs_control =
        started or
          Map.get(context, :_property_registrations, %{}) != %{} or
          Enum.any?(values, fn {_name, value} ->
            is_binary(value) and not is_nil(keyword(value))
          end)

      if not needs_control do
        {:ok, values, sanitized, prepared}
      else
        with {:ok, controls} <- prepare_values(custom, actual, budget) do
          next =
            Enum.reduce(controls, baseline, fn declaration, properties ->
              case declaration.cascade_targets do
                [] ->
                  properties

                [property] ->
                  value = declaration.cascade_keyword || elem(declaration.declaration, 1)
                  Map.put(properties, property, custom_value(context, property, value))
              end
            end)

          same_targets =
            Enum.map(ordinary, & &1.cascade_targets) == Enum.map(prepared, & &1.cascade_targets)

          if next == raw and
               (same_targets or Map.get(context, :_property_registrations, %{}) == %{}),
             do: {:ok, values, sanitized, prepared},
             else: resolve_custom_loop(custom, context, prepared, baseline, next, budget, true)
        end
      end
    end
  end

  @doc "Applies CSS-wide lifecycle rules to a custom property's raw token value."
  @spec custom_value(map(), String.t(), String.t()) :: String.t() | nil
  def custom_value(context, property, value) do
    registration = Map.get(Map.get(context, :_property_registrations, %{}), property)
    initial = if registration, do: registration.initial

    inherited =
      Map.get(
        Map.get(Map.get(context, :_cascade_inherited, context), :_custom_properties, %{}),
        property,
        initial
      )

    case keyword(value) do
      "initial" ->
        initial

      "inherit" ->
        inherited

      word when word in ["unset", "revert", "revert-layer"] ->
        if registration && not registration.inherits, do: initial, else: inherited

      nil ->
        value
    end
  end

  @doc "Recognizes a CSS-wide keyword as a single decoded CSS identifier."
  @spec keyword(String.t()) :: String.t() | nil
  def keyword(value) do
    source = HtmlValidator.trim_css_whitespace(value)

    case source do
      keyword when keyword in @wide ->
        keyword

      <<first, _::binary>> when first in ~c"iIuUrR\\" ->
        if String.contains?(source, "\\") do
          case HtmlValidator.parse_css_identifier(source) do
            {:ok, name, ""} ->
              name = String.downcase(name, :ascii)
              if name in @wide, do: name, else: nil

            _ ->
              nil
          end
        else
          normalized = String.downcase(source, :ascii)
          if normalized in @wide, do: normalized, else: nil
        end

      _ ->
        nil
    end
  end

  @doc "Copies only a declaration's winning longhands from its once-validated shorthand result."
  @spec project(map(), map(), [String.t()]) :: map()
  def project(context, validated, targets) do
    baselines? =
      Enum.any?(targets, &(&1 in ~w(baseline-source alignment-baseline baseline-shift)))

    validated =
      if baselines?,
        do: NativeElixirPdfUtilities.Validators.CssBaselineValidator.initialize(validated),
        else: validated

    projected =
      Enum.reduce(targets, context, fn property, result ->
        fields = property_fields(property, context)

        result =
          case fields do
            nil ->
              validated

            fields ->
              Enum.reduce(fields, result, fn path, result ->
                copy_path(result, validated, path)
              end)
          end

        if Map.get(context, :_svg_element, false) and
             Map.has_key?(Map.get(validated, :_svg_specified, %{}), property),
           do:
             Map.update(
               result,
               :_svg_specified,
               %{property => validated._svg_specified[property]},
               &Map.put(&1, property, validated._svg_specified[property])
             ),
           else: result
      end)

    if baselines?,
      do: NativeElixirPdfUtilities.Validators.CssBaselineValidator.synchronize(projected),
      else: projected
  end

  @doc "Applies CSS-wide defaults using the actual inherited or user-agent context."
  @spec wide(map(), String.t(), String.t()) :: {:ok, map()} | :unhandled
  def wide(context, property, value) do
    keyword = keyword(value)

    if keyword &&
         ((Map.has_key?(@fields, Map.get(@aliases, property, property)) ||
             Map.has_key?(@shorthands, property) || property == "all" ||
             NativeElixirPdfUtilities.Validators.CssLogicalValidator.logical?(property)) or
            (Map.get(context, :_svg_element, false) and
               Map.has_key?(SvgStyleValidator.presentation_properties(), property))) do
      initial = initial(context)

      inherited =
        Map.get_lazy(context, :_cascade_inherited, fn ->
          context
          |> Map.merge(Map.get(context, :_inherited_font, %{}))
          |> Map.put(
            :color,
            Map.get(context, :_inherited_color, Map.get(context, :color, {0, 0, 0}))
          )
        end)

      ua = Map.get(context, :_cascade_ua, context)

      result =
        Enum.reduce(targets(property, context), context, fn target, result ->
          source =
            case keyword do
              "initial" ->
                initial

              "inherit" ->
                inherited_property_context(initial, inherited, target, context)

              "unset" ->
                if inherited?(target, context),
                  do: inherited_property_context(initial, inherited, target, context),
                  else: initial

              _ ->
                ua
            end

          project(result, source, [target])
        end)

      {:ok, result}
    else
      :unhandled
    end
  end

  @doc "Returns whether a supported longhand inherits by default."
  @spec inherited?(String.t(), map()) :: boolean()
  def inherited?(property, context) do
    key = Map.get(SvgStyleValidator.presentation_properties(), property)

    if Map.get(context, :_svg_element, false) and key,
      do: SvgStyleValidator.inherited_property?(key),
      else: Map.get(@aliases, property, property) in @inherited
  end

  @doc "Returns the initial computed values for supported static properties."
  @spec initial(map()) :: map()
  def initial(context \\ %{}) do
    base =
      %{
        color:
          case context do
            %{color_scheme: :dark} ->
              {:ok, color} = HtmlValidator.parse_css_color("CanvasText", context)
              color

            _ ->
              {0, 0, 0}
          end,
        color_scheme: :light,
        image_orientation: :from_image,
        image_resolution: %{density: 1.0, from_image?: false, snap?: false},
        image_rendering: :auto,
        print_color_adjust: :economy,
        forced_color_adjust: :auto,
        font_size: 12.0,
        font_families: ["serif"],
        font_weight: 400,
        font_style: :normal,
        font_width: 100,
        font_oblique: nil,
        font_controls: %{},
        font_family: nil,
        font_face: nil,
        letter_spacing: 0.0,
        line_height: 14.4,
        line_height_normal: true,
        display: :inline,
        writing_mode: :horizontal_tb,
        direction: :ltr,
        unicode_bidi: :normal,
        text_align: :left,
        _text_align_initial: true,
        text_transform: :none,
        quotes: :auto,
        white_space: :normal,
        word_break: :normal,
        overflow_wrap: :normal,
        line_break: :auto,
        width: :auto,
        height: :auto,
        min_width: :auto,
        min_height: :auto,
        max_width: :none,
        max_height: :none,
        vertical_align: :baseline,
        orphans: 2,
        widows: 2,
        appearance: :none,
        content_visibility: :visible,
        visibility: :visible,
        resize: :none,
        zoom: 1.0,
        field_sizing: :fixed,
        scrollbar_width: :auto,
        scrollbar_color: :auto,
        scrollbar_gutter: :auto,
        content: :none,
        counter_reset: [],
        counter_increment: [],
        counter_set: [],
        isolation: :auto,
        opacity: 1.0,
        mix_blend_mode: :Normal,
        background_blend_modes: [:Normal],
        contain: [],
        container_name: [],
        container_type: :normal,
        contain_intrinsic_width: 0.0,
        contain_intrinsic_height: 0.0,
        overflow_x: :visible,
        overflow_y: :visible,
        overflow_clip_margin: {:padding_box, 0.0},
        text_overflow: :clip,
        block_ellipsis: :clip,
        max_lines: :none,
        continue: :auto,
        overflow: :visible,
        aspect_ratio: :auto,
        object_fit: :fill,
        object_position: {{:percent, 0.5}, {:percent, 0.5}},
        border_collapse: :separate,
        border_spacing: {0.0, 0.0},
        caption_side: :top,
        table_layout: :auto,
        empty_cells: :show,
        column_count: :auto,
        column_width: :auto,
        column_fill: :balance,
        column_span: :none,
        column_rule_width: 2.25,
        column_rule_style: :none,
        column_rule_color: :current_color,
        _row_gap_normal: true,
        _column_gap_normal: true,
        shape_outside: :none,
        shape_inside: :auto,
        shape_margin: 0.0,
        shape_image_threshold: 0.0,
        shape_image_source: nil,
        shape_image: nil,
        float: :none,
        clear: :none,
        flex_direction: :row,
        flex_wrap: :nowrap,
        row_gap: 0.0,
        column_gap: 0.0,
        flex_grow: 0.0,
        flex_shrink: 1.0,
        flex_basis: :auto,
        order: 0,
        reading_flow: :normal,
        reading_order: 0,
        justify_content: :none,
        align_items: :stretch,
        align_self: :auto,
        justify_self: :auto,
        justify_items: :stretch,
        align_content: :none,
        grid_template_columns: [],
        grid_template_rows: [],
        grid_template_areas: [],
        grid_auto_flow: :row,
        grid_auto_columns: :auto,
        grid_auto_rows: :auto,
        grid_column_start: :auto,
        grid_column_end: :auto,
        grid_row_start: :auto,
        grid_row_end: :auto,
        list_marker_type: :disc,
        list_style_position: :outside,
        list_style_image: nil,
        text_decoration_line: [],
        text_decoration_color: :current_color,
        text_decoration_style: :solid,
        text_decoration_thickness: :auto,
        css_mask: NativeElixirPdfUtilities.Validators.CssMaskValidator.defaults().css_mask,
        css_clip_path: :none,
        css_filter: [],
        css_backdrop_filter: [],
        css_perspective: :none,
        css_perspective_origin: {{:percent, 0.5}, {:percent, 0.5}},
        css_transform_style: :flat,
        css_backface_visibility: :visible,
        css_translate: :none,
        css_rotate: :none,
        css_scale: :none,
        css_transform_box: :view_box,
        css_transform: :none,
        transform_origin: {{:percent, 0.5}, {:percent, 0.5}},
        math_style: :normal,
        math_shift: :normal,
        math_depth: 0
      }
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssMotionValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssHyphenationValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssWhiteSpaceValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssTextSpacingValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssWritingModeValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssInitialLetterValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssBaselineValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssTextDecorationValidator.defaults())
      |> Map.put(:border_radii, %{
        top_left: {0.0, 0.0},
        top_right: {0.0, 0.0},
        bottom_left: {0.0, 0.0},
        bottom_right: {0.0, 0.0}
      })
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssBoxPaintValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssBorderImageValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssTextShadowValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.CssTextEmphasisValidator.defaults())
      |> Map.merge(NativeElixirPdfUtilities.Validators.RubyValidator.defaults())
      |> NativeElixirPdfUtilities.Validators.CssValueValidator.ensure_box_style()

    if Map.get(context, :_svg_element, false),
      do: Map.merge(base, SvgStyleValidator.defaults(%{})),
      else: base
  end

  # Optional computed-value metadata belongs to the inherited value as a whole.
  # Merging initial flags into an absolute inherited line height would make it normal again.
  defp inherited_property_context(initial, inherited, property, context) do
    present =
      case hd(property_fields(property, context)) do
        [key] -> Map.has_key?(inherited, key)
        [key, side] -> Map.has_key?(Map.get(inherited, key, %{}), side)
        path -> get_in(inherited, Enum.map(path, &Access.key(&1, %{}))) != %{}
      end

    if present, do: inherited, else: initial
  end

  @doc "Returns a physical property's canonical computed field for logical inheritance."
  @spec primary_field(String.t(), map()) :: [atom()]
  def primary_field(property, context), do: hd(property_fields(property, context))

  defp property_fields(property, context) do
    property = Map.get(@aliases, property, property)

    svg =
      if Map.get(context, :_svg_element, false),
        do: Map.get(SvgStyleValidator.presentation_properties(), property)

    cond do
      svg == :svg_visibility -> [[:svg_visibility], [:visibility]]
      svg -> [[svg]]
      true -> Map.get(@fields, property)
    end
  end

  defp copy_path(result, source, path) do
    case path do
      [key] ->
        case Map.fetch(source, key) do
          {:ok, value} -> Map.put(result, key, value)
          :error -> Map.delete(result, key)
        end

      [key, side] ->
        value = source |> Map.get(key, %{}) |> Map.get(side)
        Map.update(result, key, %{side => value}, &Map.put(&1, side, value))

      [key, inner, leaf] ->
        path = [Access.key(key, %{}), Access.key(inner, %{})]
        copied = copy_path(get_in(result, path), get_in(source, path), [leaf])
        put_in(result, path, copied)
    end
  end
end
