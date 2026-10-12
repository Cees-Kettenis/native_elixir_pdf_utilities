defmodule NativeElixirPdfUtilities.Validators.CssValueValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.MathLayoutValidator
  alias NativeElixirPdfUtilities.Validators.SvgStyleValidator

  @border_styles [
    :none,
    :hidden,
    :dotted,
    :dashed,
    :solid,
    :double,
    :groove,
    :ridge,
    :inset,
    :outset
  ]
  @medium_border_width 2.25
  @doc false
  @spec custom_property_declaration?(term()) :: boolean()
  def custom_property_declaration?(declaration) do
    case declaration do
      {"--" <> _custom_property, _value} -> true
      {"--" <> _custom_property, _value, :important} -> true
      _ -> false
    end
  end

  @doc """
  Validates a normalized declaration using its validated cascade context and shared budget.

  Custom properties retain their authored raw value. Ordinary declarations use
  HtmlValidator's variable resolver before the shared resolved-value policy.
  Font selection and resource authorization/loading remain downstream operations.
  """
  @spec validate_declaration(map(), term(), :atomics.atomics_ref()) ::
          {:ok, map()} | {:error, atom() | {atom(), Diagnostics.diagnostic()}}
  def validate_declaration(style, declaration, budget) do
    normalized =
      case declaration do
        {property, value} when is_binary(property) and is_binary(value) ->
          {property, value}

        {property, value, :important} when is_binary(property) and is_binary(value) ->
          {property, value}

        _ ->
          nil
      end

    case normalized do
      {"--" <> _ = property, value} ->
        validate_resolved(style, property, value)

      {property, value} ->
        with {:ok, value} <-
               HtmlValidator.resolve_css_variables(
                 value,
                 Map.get(style, :_custom_properties, %{}),
                 %{},
                 budget
               ),
             {:ok, validated} <- validate_resolved(style, property, value) do
          {:ok, validated}
        else
          :error -> {:error, :invalid_document}
          error -> error
        end

      nil ->
        {:error, :invalid_document}
    end
  end

  @doc false
  @spec important_declaration?(CssParser.declaration()) :: boolean()
  def important_declaration?(declaration) do
    case declaration do
      {_property, _value, :important} -> true
      _ -> false
    end
  end

  @doc false
  @spec edges(term()) :: term()
  def edges(value) do
    edges(value, value, value, value)
  end

  @doc false
  @spec edges(term(), term(), term(), term()) :: term()
  def edges(top, right, bottom, left) do
    %{top: top, right: right, bottom: bottom, left: left}
  end

  @doc """
  Validates a self-contained authored declaration block without loading resources.

  This is a standalone block, not a stylesheet or removed-source certificate.
  Use validate_declarations_in_context/3 for an actual inherited cascade context.
  Supply the invocation's shared CSS budget and enclosing render budget across
  blocks so original source and expansion reservations accumulate.
  """
  @spec validate_declarations(term(), :atomics.atomics_ref()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_declarations(css, budget \\ HtmlValidator.new_css_budget()) do
    with {:ok, _context} <- validate_declarations_in_context(css, policy_style(), budget),
         do: :ok
  end

  @doc """
  Validates authored declarations against their actual inherited cascade context.

  The internal context must come from the responsible caller's validated tree
  traversal, including inherited custom properties and root font size. This
  applies important precedence within the block and returns the updated context.
  Cross-rule ordering and selector matching remain the caller's cascade work.
  No fonts, assets or files are read, and no receipt or resource permission is
  issued. The result certifies neither unmatched rules nor a whole stylesheet.
  """
  @spec validate_declarations_in_context(term(), map(), :atomics.atomics_ref()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_declarations_in_context(css, context, budget) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, declarations} <- CssParser.parse_declarations_detailed(css) do
        validate_cascade_steps(
          context,
          Enum.sort_by(declarations, &important_declaration?/1),
          budget,
          fn _declaration, validated -> {:ok, validated} end,
          fn error, declaration ->
            case error do
              {:error, {_, _}} -> error
              _ -> declaration_failure(css, declaration)
            end
          end
        )
      end
    end)
  end

  @doc """
  Validates ordered normalized declarations using an actual cascade context.

  The responsible caller supplies declarations in cascade order and a shared
  invocation CSS budget. Custom properties resolve before color and font size,
  then dependent values use that validated state. Root rem font size is resolved
  at the same point as in full Style execution. The optional internal consumer
  runs only after a declaration succeeds, allowing Style to select fonts at its
  existing execution points. The default consumer performs no resource reads.
  This API does not select resources, certify source removal or accept receipts.
  """
  @spec validate_cascade(map(), [CssParser.declaration()], :atomics.atomics_ref()) ::
          {:ok, map()} | {:error, atom() | {atom(), Diagnostics.diagnostic()}}
  @spec validate_cascade(
          map(),
          [CssParser.declaration()],
          :atomics.atomics_ref(),
          (CssParser.declaration(), map() ->
             {:ok, map()} | {:error, atom() | {atom(), Diagnostics.diagnostic()}})
        ) :: {:ok, map()} | {:error, atom() | {atom(), Diagnostics.diagnostic()}}
  def validate_cascade(
        context,
        declarations,
        budget,
        consumer \\ fn _declaration, validated -> {:ok, validated} end
      ) do
    if is_function(consumer, 2) do
      validate_cascade_steps(context, declarations, budget, consumer, fn error, _declaration ->
        error
      end)
    else
      Diagnostics.error(
        :css,
        :invalid_options,
        "internal cascade consumer must accept a validated declaration and style context",
        module: __MODULE__,
        source: "consumer"
      )
    end
  end

  defp validate_cascade_steps(style, declarations, budget, consumer, failure) do
    {custom_declarations, ordinary_declarations} =
      Enum.split_with(declarations, &custom_property_declaration?/1)

    {foundational_declarations, dependent_declarations} =
      Enum.split_with(ordinary_declarations, fn declaration ->
        case declaration do
          {property, _value} -> property in ["color", "font-size"]
          {property, _value, :important} -> property in ["color", "font-size"]
          _ -> false
        end
      end)

    with {:ok, style} <-
           validate_cascade_list(style, custom_declarations, budget, consumer, failure),
         {:ok, custom_properties} <-
           HtmlValidator.compute_custom_properties(style._custom_properties, budget),
         style = Map.put(style, :_custom_properties, custom_properties),
         {:ok, style} <-
           validate_cascade_list(style, foundational_declarations, budget, consumer, failure) do
      style =
        case Map.get(style, :_root_element, false) do
          true -> Map.put(style, :_root_font_size, Map.fetch!(style, :font_size))
          false -> style
        end

      style =
        if Map.get(style, :_fieldset_default_padding, false) do
          font_size = style.font_size
          padding = edges(font_size * 0.35, font_size * 0.75, font_size * 0.625, font_size * 0.75)

          padding =
            Map.new(padding, fn {side, value} ->
              {side, Float.floor(value / 0.75 * 64) / 64 * 0.75}
            end)

          style |> Map.put(:padding, padding) |> Map.delete(:_fieldset_default_padding)
        else
          style
        end

      validate_cascade_list(style, dependent_declarations, budget, consumer, failure)
    end
  end

  @doc false
  @spec validate_source_cascade(map(), [CssParser.declaration()], :atomics.atomics_ref(), map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_source_cascade(context, declarations, budget, original_node) do
    validate_cascade_steps(
      context,
      declarations,
      budget,
      fn _declaration, validated -> {:ok, validated} end,
      fn error, declaration ->
        case error do
          {:error, {_reason, _diagnostic}} -> error
          _ -> HtmlValidator.source_style_error(original_node, declaration)
        end
      end
    )
  end

  defp validate_cascade_list(style, declarations, budget, consumer, failure) do
    Enum.reduce_while(declarations, {:ok, style}, fn declaration, {:ok, context} ->
      with {:ok, validated} <- validate_declaration(context, declaration, budget),
           {:ok, consumed} <- consumer.(declaration, validated) do
        {:cont, {:ok, consumed}}
      else
        error -> {:halt, failure.(error, declaration)}
      end
    end)
  end

  @doc false
  @spec stylesheet_declaration_sources(String.t()) :: [String.t()]
  def stylesheet_declaration_sources(css) do
    css
    |> then(&Regex.replace(~r/@font-face\s*\{[^{}]*\}/ui, &1, ""))
    |> then(&Regex.scan(~r/[^{}]+\{(?<declarations>[^{}]*)\}/u, &1, capture: ["declarations"]))
    |> List.flatten()
    |> Enum.flat_map(&CssParser.declaration_sources/1)
  end

  @doc false
  @spec declaration_error(String.t(), [String.t()]) :: nil | {atom(), Diagnostics.diagnostic()}
  def declaration_error(css, sources) do
    Enum.find_value(sources, fn source ->
      with {:ok, [declaration]} <- CssParser.parse_declarations(source),
           {:ok, _validated} <-
             validate_declaration(policy_style(), declaration, HtmlValidator.new_css_budget()) do
        nil
      else
        _ ->
          {:error, error} = declaration_failure(css, source)
          {reason, diagnostic} = error
          {reason, Map.delete(diagnostic, :module)}
      end
    end)
  end

  defp policy_style do
    %{color: {0, 0, 0}, font_size: 12.0, _root_font_size: 12.0, _custom_properties: %{}}
    |> ensure_box_style()
    |> flex_container_defaults()
    |> grid_container_defaults()
  end

  defp declaration_failure(css, declaration) do
    source =
      case declaration do
        source when is_binary(source) ->
          HtmlValidator.trim_css_whitespace(source)

        declaration ->
          CssParser.declaration_sources(css)
          |> Enum.find(fn source ->
            {:ok, parsed} = HtmlValidator.validate_css_declaration(source)
            parsed == declaration
          end)
          |> HtmlValidator.trim_css_whitespace()
      end

    {index, _length} = :binary.match(css, source)
    prefix = binary_part(css, 0, index)
    lines = String.split(prefix, "\n", trim: false)
    line = length(lines)
    column = String.length(List.last(lines) || "") + 1

    Diagnostics.error(
      :css,
      :invalid_css,
      ~s(line #{line}: declaration "#{source}" is invalid or unsupported),
      module: __MODULE__,
      source: source,
      line: line,
      column: column
    )
  end

  @doc """
  Validates one resolved declaration against the renderer's supported value rules.

  The internal style context and variable substitution are already validated.
  Returns the validated style result without resolving fonts or reading assets.
  This does not authorize references or certify their resource contents.
  """
  @spec validate_resolved(map(), String.t(), String.t()) ::
          {:ok, map()} | :error | {:error, atom() | {atom(), Diagnostics.diagnostic()}}
  def validate_resolved(style, property, value) do
    svg_property =
      if Map.get(style, :_svg_prepare, false) and
           (Map.get(style, :_svg_element, false) or
              property not in ~w(width height opacity visibility transform)),
         do: SvgStyleValidator.apply(style, property, value),
         else: :unhandled

    case svg_property do
      :unhandled ->
        case property do
          "--" <> _custom_property ->
            properties = style |> Map.get(:_custom_properties, %{}) |> Map.put(property, value)
            {:ok, Map.put(style, :_custom_properties, properties)}

          "color" ->
            with {:ok, color} <-
                   HtmlValidator.parse_css_color_declaration(
                     value,
                     Map.get(style, :_inherited_color, Map.fetch!(style, :color))
                   ) do
              case color do
                :transparent -> {:ok, Map.put(style, :color, {0, 0, 0, 0.0})}
                color -> {:ok, Map.put(style, :color, color)}
              end
            end

          "accent-color" ->
            case HtmlValidator.parse_css_accent_color(value, style.color) do
              {:ok, :auto} -> {:ok, Map.delete(style, :accent_color)}
              {:ok, color} -> {:ok, Map.put(style, :accent_color, color)}
              :error -> {:error, :invalid_document}
            end

          "appearance" ->
            with {:ok, appearance} <- HtmlValidator.parse_gauge_appearance(value),
                 do: {:ok, Map.put(style, :appearance, appearance)}

          "background-color" ->
            with {:ok, color} <- HtmlValidator.parse_css_color(value, Map.fetch!(style, :color)) do
              {:ok, Map.put(style, :background_color, transparent_color_to_nil(color))}
            end

          "background" ->
            put_background(style, value)

          "background-image" ->
            put_background_image(style, value)

          "background-size" ->
            put_background_size(style, value)

          "background-position" ->
            put_two_axis_position(style, :background_position, value)

          "background-repeat" ->
            put_background_repeat(style, value)

          "box-sizing" ->
            put_box_sizing(style, value)

          "margin" ->
            with {:ok, lengths} <- HtmlValidator.parse_css_box_lengths(value, :margin),
                 do: {:ok, put_box_lengths(style, "margin", lengths)}

          "padding" ->
            with {:ok, lengths} <- parse_box_lengths(value),
                 do: {:ok, put_box_lengths(style, "padding", lengths)}

          "border-width" ->
            with {:ok, widths} <- parse_border_widths(value),
                 do: {:ok, put_box_lengths(style, "border-width", widths)}

          property
          when property in ["margin-top", "margin-right", "margin-bottom", "margin-left"] ->
            with {:ok, length} <- HtmlValidator.parse_css_margin(value),
                 do: {:ok, put_edge_length(style, property, length)}

          property
          when property in ["padding-top", "padding-right", "padding-bottom", "padding-left"] ->
            with {:ok, length} <- parse_length(value),
                 do: {:ok, put_edge_length(style, property, length)}

          "border-color" ->
            with {:ok, border_colors} <-
                   parse_border_colors(value, Map.fetch!(style, :color)) do
              border_color = border_colors.top

              {:ok,
               style
               |> Map.put(:border_color, border_color)
               |> Map.put(:border_colors, border_colors)}
            end

          property
          when property in ["border-top", "border-right", "border-bottom", "border-left"] ->
            with {:ok, width, color, border_style} <-
                   parse_border_edge(value, Map.fetch!(style, :color)) do
              {:ok,
               style
               |> put_border_edge_width(property, width)
               |> put_border_edge_color(property, color)
               |> put_border_edge_style(property, border_style)}
            end

          property
          when property in [
                 "border-top-width",
                 "border-right-width",
                 "border-bottom-width",
                 "border-left-width"
               ] ->
            with {:ok, width} <- parse_border_width(value) do
              {:ok, put_border_edge_width(style, border_edge_property(property), width)}
            end

          property
          when property in [
                 "border-top-color",
                 "border-right-color",
                 "border-bottom-color",
                 "border-left-color"
               ] ->
            with {:ok, color} <- HtmlValidator.parse_css_color(value, Map.fetch!(style, :color)) do
              {:ok,
               put_border_edge_color(
                 style,
                 border_edge_property(property),
                 transparent_color_to_nil(color)
               )}
            end

          property
          when property in [
                 "border-style",
                 "border-top-style",
                 "border-right-style",
                 "border-bottom-style",
                 "border-left-style"
               ] ->
            put_border_style(style, property, value)

          "border-radius" ->
            with {:ok, length} <- parse_length(value),
                 do: {:ok, Map.put(style, :border_radius, length)}

          "text-align" ->
            put_text_align(style, value)

          "text-transform" ->
            put_text_transform(style, value)

          "content" ->
            put_generated_content(style, value)

          "counter-reset" ->
            put_counter_values(style, :counter_reset, value, 0)

          "counter-increment" ->
            put_counter_values(style, :counter_increment, value, 1)

          "letter-spacing" ->
            put_letter_spacing(style, value)

          property
          when property in [
                 "white-space",
                 "word-break",
                 "word-wrap",
                 "overflow-wrap",
                 "line-break"
               ] ->
            put_text_wrapping(style, property, value)

          property when property in ["break-before", "page-break-before"] ->
            with {:ok, break_value} <- page_break_value(value),
                 do: {:ok, Map.put(style, :break_before, break_value)}

          property when property in ["break-after", "page-break-after"] ->
            with {:ok, break_value} <- page_break_value(value),
                 do: {:ok, Map.put(style, :break_after, break_value)}

          property when property in ["break-inside", "page-break-inside"] ->
            put_break_inside(style, value)

          "vertical-align" ->
            with {:ok, align} <- HtmlValidator.parse_vertical_align(value, style.font_size),
                 do: {:ok, Map.put(style, :vertical_align, align)}

          "min-height" ->
            put_size(style, :min_height, value)

          property when property in ["min-width", "max-width", "max-height"] ->
            put_size(style, length_property(property), value)

          "border" ->
            with {:ok, border_widths, border_color, border_styles} <-
                   parse_border(value, Map.fetch!(style, :color)) do
              style =
                style
                |> Map.put(:border_widths, border_widths)
                |> Map.put(:border_color, border_color)
                |> Map.put(:border_colors, edges(border_color))
                |> Map.put(:border_styles, border_styles)

              {:ok, style}
            end

          "font-size" ->
            with {:ok, properties} <-
                   MathLayoutValidator.parse_property("font-size", value, style),
                 do: {:ok, Map.merge(style, properties)}

          property when property in ["math-style", "math-shift", "math-depth"] ->
            with {:ok, properties} <- MathLayoutValidator.parse_property(property, value, style),
                 do: {:ok, Map.merge(style, properties)}

          "line-height" ->
            case parse_line_height(value) do
              {:ok, {:multiplier, multiplier}} ->
                {:ok,
                 style
                 |> Map.put(:line_height_multiplier, multiplier)
                 |> Map.delete(:line_height_normal)
                 |> Map.put(:line_height_explicit, true)}

              {:ok, :normal} ->
                {:ok,
                 style
                 |> Map.put(:line_height_normal, true)
                 |> Map.delete(:line_height_multiplier)
                 |> Map.put(:line_height_explicit, true)}

              {:ok, line_height} ->
                {:ok,
                 style
                 |> Map.put(:line_height, line_height)
                 |> Map.delete(:line_height_multiplier)
                 |> Map.delete(:line_height_normal)
                 |> Map.put(:line_height_explicit, true)}

              :error ->
                :error
            end

          "font-weight" ->
            put_font_weight(style, value)

          "font-style" ->
            put_font_style(style, value)

          "font-family" ->
            {:ok, Map.put(style, :font_families, value)}

          property
          when property in ~w(text-decoration text-decoration-line text-decoration-color text-decoration-thickness text-decoration-style) ->
            case HtmlValidator.parse_css_text_decoration(
                   property,
                   value,
                   style.font_size,
                   style.color
                 ) do
              {:ok, properties} -> {:ok, Map.merge(style, properties)}
              :error -> {:error, :invalid_document}
            end

          "display" ->
            put_display(style, value)

          property when property in ~w(list-style list-style-type list-style-position) ->
            case HtmlValidator.parse_css_list_style(property, value) do
              {:ok, properties} -> {:ok, Map.merge(style, properties)}
              :error -> {:error, :invalid_document}
            end

          property when property in ["width", "height"] ->
            put_size(style, length_property(property), value)

          "position" ->
            put_position(style, value)

          property when property in ["top", "right", "bottom", "left"] ->
            put_position_offset(style, property, value)

          "z-index" ->
            put_z_index(style, value)

          "object-fit" ->
            put_object_fit(style, value)

          "object-position" ->
            put_two_axis_position(style, :object_position, value)

          "overflow" ->
            put_overflow(style, value)

          "aspect-ratio" ->
            with {:ok, aspect_ratio} <- HtmlValidator.parse_aspect_ratio(value),
                 do: {:ok, Map.put(style, :aspect_ratio, aspect_ratio)}

          "border-collapse" ->
            put_border_collapse(style, value)

          "border-spacing" ->
            with {:ok, horizontal, vertical} <- parse_border_spacing(value) do
              {:ok, Map.put(style, :border_spacing, {horizontal, vertical})}
            end

          "table-layout" ->
            put_table_layout(style, value)

          "flex-direction" ->
            put_flex_direction(style, value)

          "flex-wrap" ->
            put_flex_wrap(style, value)

          "gap" ->
            with {:ok, row_gap, column_gap} <- parse_gap(value) do
              {:ok, style |> Map.put(:row_gap, row_gap) |> Map.put(:column_gap, column_gap)}
            end

          property when property in ["row-gap", "column-gap"] ->
            with {:ok, length} <- parse_length(value),
                 do: {:ok, Map.put(style, gap_property(property), length)}

          "justify-content" ->
            put_justify_content(style, value)

          "align-items" ->
            put_align_items(style, value)

          "align-self" ->
            put_align_self(style, value)

          "justify-self" ->
            put_grid_alignment(style, :justify_self, value)

          "order" ->
            put_order(style, value)

          "flex-grow" ->
            put_nonnegative_number(style, :flex_grow, value)

          "flex-shrink" ->
            put_nonnegative_number(style, :flex_shrink, value)

          "flex-basis" ->
            put_flex_basis(style, value)

          "flex" ->
            put_flex(style, value)

          property when property in ["grid-template-columns", "grid-template-rows"] ->
            with {:ok, tracks} <- parse_grid_tracks(value) do
              {:ok, Map.put(style, grid_template_property(property), tracks)}
            end

          property when property in ["grid-auto-columns", "grid-auto-rows"] ->
            with {:ok, track} <- parse_grid_track(value) do
              {:ok, Map.put(style, grid_auto_property(property), track)}
            end

          property
          when property in [
                 "grid-column",
                 "grid-row",
                 "grid-column-start",
                 "grid-column-end",
                 "grid-row-start",
                 "grid-row-end",
                 "grid-area"
               ] ->
            put_grid_placement(style, property, value)

          "justify-items" ->
            put_grid_alignment(style, :justify_items, value)

          "align-content" ->
            put_justify_content(style, value, :align_content)

          _ ->
            {:error, :invalid_document}
        end

      result ->
        result
    end
  end

  defp put_display(style, value) do
    case value |> String.trim() |> String.downcase() do
      value when value in ["math", "inline math", "block math"] ->
        with {:ok, properties} <- MathLayoutValidator.parse_property("display", value, style),
             do: {:ok, Map.merge(style, properties)}

      "block" ->
        style =
          if Map.has_key?(style, :_math_context),
            do: Map.merge(style, %{_math_display: :block, _math_formatting: false}),
            else: style

        {:ok, style |> ensure_box_style() |> Map.put(:display, :block)}

      "inline" ->
        style =
          if Map.has_key?(style, :_math_context),
            do: Map.merge(style, %{_math_display: :inline, _math_formatting: false}),
            else: style

        {:ok, Map.put(style, :display, :inline)}

      "inline-block" ->
        style =
          if Map.has_key?(style, :_math_context),
            do: Map.merge(style, %{_math_display: :inline, _math_formatting: false}),
            else: style

        {:ok, style |> ensure_box_style() |> Map.put(:display, :inline_block)}

      "none" ->
        {:ok, Map.put(style, :display, :none)}

      "flex" ->
        {:ok,
         style |> ensure_box_style() |> flex_container_defaults() |> Map.put(:display, :flex)}

      "inline-flex" ->
        {:ok,
         style
         |> ensure_box_style()
         |> flex_container_defaults()
         |> Map.put(:display, :inline_flex)}

      "grid" ->
        {:ok,
         style |> ensure_box_style() |> grid_container_defaults() |> Map.put(:display, :grid)}

      "inline-grid" ->
        {:ok,
         style
         |> ensure_box_style()
         |> grid_container_defaults()
         |> Map.put(:display, :inline_grid)}

      _ ->
        {:error, :invalid_document}
    end
  end

  @doc false
  @spec ensure_box_style(map()) :: map()
  def ensure_box_style(style) do
    style
    |> Map.put_new(:background_color, nil)
    |> Map.put_new(:background_image, nil)
    |> Map.put_new(:background_position, {{:percent, 0.0}, {:percent, 0.0}})
    |> Map.put_new(:background_repeat, :repeat)
    |> Map.put_new(:background_size, {:auto, :auto})
    |> Map.put_new(:border_color, :current_color)
    |> Map.put_new(:border_colors, edges(Map.get(style, :border_color, :current_color)))
    |> Map.put_new(:border_radius, 0.0)
    |> Map.put_new(:border_styles, edges(:none))
    |> Map.put_new(:border_widths, edges(@medium_border_width))
    |> Map.put_new(:box_sizing, :content_box)
    |> Map.put_new(:margin, edges(0.0, 0.0, Map.get(style, :margin_after, 0.0), 0.0))
    |> Map.put_new(:margin_after, 0.0)
    |> Map.put_new(:padding, edges(0.0))
    |> Map.put_new(:position, :static)
    |> Map.put_new(:offsets, edges(:auto))
    |> Map.put_new(:z_index, :auto)
  end

  defp put_size(style, property, value) do
    with {:ok, size} <- HtmlValidator.parse_css_size(value, property),
         do: {:ok, Map.put(style, property, size)}
  end

  defp put_background(style, value) do
    style =
      style
      |> Map.put(:background_color, nil)
      |> Map.put(:background_image, nil)
      |> Map.delete(:background_image_source)
      |> Map.put(:background_position, {{:percent, 0.0}, {:percent, 0.0}})
      |> Map.put(:background_repeat, :repeat)
      |> Map.put(:background_size, {:auto, :auto})

    with {:ok, color} <-
           HtmlValidator.parse_css_background_color(value, Map.fetch!(style, :color)) do
      {:ok, Map.put(style, :background_color, transparent_color_to_nil(color))}
    end
  end

  defp put_background_image(style, value) do
    case HtmlValidator.parse_background_image_reference(value) do
      {:ok, nil} ->
        {:ok, style |> Map.put(:background_image, nil) |> Map.delete(:background_image_source)}

      {:ok, source} ->
        {:ok, Map.put(style, :background_image_source, source)}

      error ->
        error
    end
  end

  defp put_background_size(style, value) do
    case value |> String.trim() |> String.downcase() do
      "contain" ->
        {:ok, Map.put(style, :background_size, :contain)}

      "cover" ->
        {:ok, Map.put(style, :background_size, :cover)}

      value ->
        parts = HtmlValidator.split_css_tokens(value)

        parsed =
          Enum.map(parts, fn part ->
            case part do
              "auto" -> {:ok, :auto}
              part -> parse_size(part)
            end
          end)

        case parsed do
          [{:ok, size}] ->
            {:ok, Map.put(style, :background_size, {size, :auto})}

          [{:ok, width}, {:ok, height}] ->
            {:ok, Map.put(style, :background_size, {width, height})}

          _ ->
            {:error, :invalid_document}
        end
    end
  end

  defp put_background_repeat(style, value) do
    case value |> String.trim() |> String.downcase() do
      "repeat" -> {:ok, Map.put(style, :background_repeat, :repeat)}
      "no-repeat" -> {:ok, Map.put(style, :background_repeat, :no_repeat)}
      "repeat-x" -> {:ok, Map.put(style, :background_repeat, :repeat_x)}
      "repeat-y" -> {:ok, Map.put(style, :background_repeat, :repeat_y)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_position(style, value) do
    case value |> String.trim() |> String.downcase() do
      "static" -> {:ok, Map.put(style, :position, :static)}
      "relative" -> {:ok, Map.put(style, :position, :relative)}
      "absolute" -> {:ok, Map.put(style, :position, :absolute)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_position_offset(style, property, value) do
    parsed =
      case value |> String.trim() |> String.downcase() do
        "auto" -> {:ok, :auto}
        value -> parse_position_component(value)
      end

    with {:ok, offset} <- parsed do
      side = String.to_existing_atom(property)
      offsets = style |> Map.get(:offsets, edges(:auto)) |> Map.put(side, offset)
      {:ok, Map.put(style, :offsets, offsets)}
    end
  end

  defp put_z_index(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" ->
        {:ok, Map.put(style, :z_index, :auto)}

      value ->
        case Integer.parse(value) do
          {z_index, ""} -> {:ok, Map.put(style, :z_index, z_index)}
          _ -> {:error, :invalid_document}
        end
    end
  end

  defp put_object_fit(style, value) do
    case value |> String.trim() |> String.downcase() do
      "fill" -> {:ok, Map.put(style, :object_fit, :fill)}
      "contain" -> {:ok, Map.put(style, :object_fit, :contain)}
      "cover" -> {:ok, Map.put(style, :object_fit, :cover)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_two_axis_position(style, property, value) do
    with {:ok, position} <- parse_two_axis_position(value) do
      {:ok, Map.put(style, property, position)}
    end
  end

  defp put_overflow(style, value) do
    case value |> String.trim() |> String.downcase() do
      "visible" -> {:ok, Map.put(style, :overflow, :visible)}
      "hidden" -> {:ok, Map.put(style, :overflow, :hidden)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_border_style(style, property, value) do
    case property do
      "border-style" ->
        with {:ok, border_styles} <- parse_border_styles(value) do
          {:ok, Map.put(style, :border_styles, border_styles)}
        end

      _ ->
        with {:ok, border_style} <- parse_border_style(value) do
          {:ok, put_border_edge_style(style, border_edge_property(property), border_style)}
        end
    end
  end

  defp put_box_sizing(style, value) do
    case value |> String.trim() |> String.downcase() do
      "border-box" -> {:ok, Map.put(style, :box_sizing, :border_box)}
      "content-box" -> {:ok, Map.put(style, :box_sizing, :content_box)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_text_transform(style, value) do
    case value |> String.trim() |> String.downcase() do
      "math-auto" ->
        with {:ok, properties} <-
               MathLayoutValidator.parse_property("text-transform", value, style),
             do: {:ok, Map.merge(style, properties)}

      "none" ->
        {:ok, Map.put(style, :text_transform, :none)}

      "uppercase" ->
        {:ok, Map.put(style, :text_transform, :uppercase)}

      "lowercase" ->
        {:ok, Map.put(style, :text_transform, :lowercase)}

      "capitalize" ->
        {:ok, Map.put(style, :text_transform, :capitalize)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_generated_content(style, value) do
    normalized = String.trim(value)

    case String.downcase(normalized) do
      keyword when keyword in ["none", "normal"] ->
        {:ok, Map.put(style, :content, :none)}

      _ ->
        token_regex =
          ~r/\s*("(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|attr\(\s*[a-zA-Z_][a-zA-Z0-9_:-]*\s*\)|counter\(\s*[a-zA-Z_][a-zA-Z0-9_-]*\s*\))/u

        tokens = Regex.scan(token_regex, normalized)
        parsed_source = Enum.map_join(tokens, "", &List.first/1) |> String.trim()

        case tokens != [] and parsed_source == normalized do
          true ->
            parts =
              Enum.map(tokens, fn [_, token] ->
                cond do
                  String.starts_with?(token, ["\"", "'"]) ->
                    text = String.slice(token, 1, String.length(token) - 2)
                    {:string, String.replace(text, ~r/\\([\\"'])/u, "\\1")}

                  String.starts_with?(token, "attr(") ->
                    [_, name] = Regex.run(~r/^attr\(\s*([a-zA-Z_][a-zA-Z0-9_:-]*)\s*\)$/u, token)
                    {:attr, String.downcase(name)}

                  String.starts_with?(token, "counter(") ->
                    [_, name] =
                      Regex.run(~r/^counter\(\s*([a-zA-Z_][a-zA-Z0-9_-]*)\s*\)$/u, token)

                    {:counter, name}
                end
              end)

            {:ok, Map.put(style, :content, parts)}

          false ->
            {:error, :invalid_document}
        end
    end
  end

  defp put_counter_values(style, property, value, default_value) do
    tokens = value |> String.trim() |> String.split(~r/\s+/u, trim: true)

    case tokens do
      ["none"] ->
        {:ok, Map.put(style, property, [])}

      tokens ->
        result =
          Enum.reduce_while(tokens, {:ok, [], false}, fn token, {:ok, values, integer_allowed?} ->
            identifier? =
              token != "none" and Regex.match?(~r/^[a-zA-Z_][a-zA-Z0-9_-]*$/u, token)

            cond do
              identifier? ->
                {:cont, {:ok, values ++ [{token, default_value}], true}}

              integer_allowed? ->
                case Integer.parse(token) do
                  {integer, ""} ->
                    {name, _old_value} = List.last(values)
                    {:cont, {:ok, List.replace_at(values, -1, {name, integer}), false}}

                  _ ->
                    {:halt, :error}
                end

              true ->
                {:halt, :error}
            end
          end)

        case result do
          {:ok, values, _integer_allowed?} -> {:ok, Map.put(style, property, values)}
          :error -> {:error, :invalid_document}
        end
    end
  end

  defp put_letter_spacing(style, value) do
    case value |> String.trim() |> String.downcase() do
      "normal" ->
        {:ok, Map.put(style, :letter_spacing, 0.0)}

      value ->
        with {:ok, length} <-
               HtmlValidator.parse_css_length(value, :margin, Map.fetch!(style, :font_size)) do
          {:ok, Map.put(style, :letter_spacing, length)}
        else
          :error -> {:error, :invalid_document}
        end
    end
  end

  defp put_text_wrapping(style, property, value) do
    case HtmlValidator.parse_css_text_wrapping(property, value) do
      {:ok, key, value} -> {:ok, Map.put(style, key, value)}
      :error -> {:error, :invalid_document}
    end
  end

  defp put_break_inside(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" -> {:ok, Map.put(style, :break_inside, :auto)}
      "avoid" -> {:ok, Map.put(style, :break_inside, :avoid)}
      _ -> {:error, :invalid_document}
    end
  end

  defp flex_container_defaults(style) do
    style
    |> Map.put_new(:flex_direction, :row)
    |> Map.put_new(:flex_wrap, :nowrap)
    |> Map.put_new(:row_gap, 0.0)
    |> Map.put_new(:column_gap, 0.0)
    |> Map.put_new(:justify_content, :flex_start)
    |> Map.put_new(:align_items, :stretch)
  end

  defp grid_container_defaults(style) do
    style
    |> Map.put_new(:grid_template_columns, [])
    |> Map.put_new(:grid_template_rows, [])
    |> Map.put_new(:grid_auto_columns, :auto)
    |> Map.put_new(:grid_auto_rows, :auto)
    |> Map.put_new(:row_gap, 0.0)
    |> Map.put_new(:column_gap, 0.0)
    |> Map.put_new(:justify_items, :stretch)
    |> Map.put_new(:align_items, :stretch)
    |> Map.put_new(:justify_content, :flex_start)
    |> Map.put_new(:align_content, :stretch)
  end

  defp put_flex_direction(style, value) do
    case value |> String.trim() |> String.downcase() do
      "row" -> {:ok, Map.put(style, :flex_direction, :row)}
      "row-reverse" -> {:ok, Map.put(style, :flex_direction, :row_reverse)}
      "column" -> {:ok, Map.put(style, :flex_direction, :column)}
      "column-reverse" -> {:ok, Map.put(style, :flex_direction, :column_reverse)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_flex_wrap(style, value) do
    case value |> String.trim() |> String.downcase() do
      "nowrap" -> {:ok, Map.put(style, :flex_wrap, :nowrap)}
      "wrap" -> {:ok, Map.put(style, :flex_wrap, :wrap)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_justify_content(style, value) do
    put_justify_content(style, value, :justify_content)
  end

  defp put_justify_content(style, value, property) do
    case value |> String.trim() |> String.downcase() do
      value when value in ["start", "flex-start"] -> {:ok, Map.put(style, property, :flex_start)}
      value when value in ["end", "flex-end"] -> {:ok, Map.put(style, property, :flex_end)}
      "center" -> {:ok, Map.put(style, property, :center)}
      "stretch" -> {:ok, Map.put(style, property, :stretch)}
      "space-between" -> {:ok, Map.put(style, property, :space_between)}
      "space-around" -> {:ok, Map.put(style, property, :space_around)}
      "space-evenly" -> {:ok, Map.put(style, property, :space_evenly)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_align_items(style, value) do
    case value |> String.trim() |> String.downcase() do
      "stretch" ->
        {:ok, Map.put(style, :align_items, :stretch)}

      value when value in ["start", "flex-start"] ->
        {:ok, Map.put(style, :align_items, :flex_start)}

      value when value in ["end", "flex-end"] ->
        {:ok, Map.put(style, :align_items, :flex_end)}

      "center" ->
        {:ok, Map.put(style, :align_items, :center)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_align_self(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" ->
        {:ok, Map.put(style, :align_self, :auto)}

      "stretch" ->
        {:ok, Map.put(style, :align_self, :stretch)}

      value when value in ["start", "flex-start"] ->
        {:ok, Map.put(style, :align_self, :flex_start)}

      value when value in ["end", "flex-end"] ->
        {:ok, Map.put(style, :align_self, :flex_end)}

      "center" ->
        {:ok, Map.put(style, :align_self, :center)}

      _ ->
        {:error, :invalid_document}
    end
  end

  defp put_grid_alignment(style, property, value) do
    case value |> String.trim() |> String.downcase() do
      "stretch" -> {:ok, Map.put(style, property, :stretch)}
      value when value in ["start", "flex-start"] -> {:ok, Map.put(style, property, :flex_start)}
      value when value in ["end", "flex-end"] -> {:ok, Map.put(style, property, :flex_end)}
      "center" -> {:ok, Map.put(style, property, :center)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_border_collapse(style, value) do
    case value |> String.trim() |> String.downcase() do
      "collapse" -> {:ok, Map.put(style, :border_collapse, :collapse)}
      "separate" -> {:ok, Map.put(style, :border_collapse, :separate)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_table_layout(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" -> {:ok, Map.put(style, :table_layout, :auto)}
      "fixed" -> {:ok, Map.put(style, :table_layout, :fixed)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_order(style, value) do
    case Integer.parse(String.trim(value)) do
      {order, ""} -> {:ok, Map.put(style, :order, order)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_nonnegative_number(style, key, value) do
    case parse_nonnegative_number(value) do
      {:ok, number} -> {:ok, Map.put(style, key, number)}
      :error -> {:error, :invalid_document}
    end
  end

  defp put_flex_basis(style, value) do
    case value |> String.trim() |> String.downcase() do
      "auto" ->
        {:ok, Map.put(style, :flex_basis, :auto)}

      _ ->
        with {:ok, length} <- parse_length(value), do: {:ok, Map.put(style, :flex_basis, length)}
    end
  end

  defp put_flex(style, value) do
    tokens = value |> String.trim() |> String.downcase() |> String.split(~r/\s+/u, trim: true)

    case tokens do
      ["none"] ->
        {:ok,
         style
         |> Map.put(:flex_grow, 0.0)
         |> Map.put(:flex_shrink, 0.0)
         |> Map.put(:flex_basis, :auto)}

      ["auto"] ->
        {:ok,
         style
         |> Map.put(:flex_grow, 1.0)
         |> Map.put(:flex_shrink, 1.0)
         |> Map.put(:flex_basis, :auto)}

      ["initial"] ->
        {:ok,
         style
         |> Map.put(:flex_grow, 0.0)
         |> Map.put(:flex_shrink, 1.0)
         |> Map.put(:flex_basis, :auto)}

      [grow] ->
        with {:ok, grow} <- parse_nonnegative_number(grow) do
          {:ok,
           style
           |> Map.put(:flex_grow, grow)
           |> Map.put_new(:flex_shrink, 1.0)
           |> Map.put_new(:flex_basis, 0.0)}
        end

      [grow, shrink] ->
        with {:ok, grow} <- parse_nonnegative_number(grow),
             {:ok, shrink} <- parse_nonnegative_number(shrink) do
          {:ok,
           style
           |> Map.put(:flex_grow, grow)
           |> Map.put(:flex_shrink, shrink)
           |> Map.put_new(:flex_basis, 0.0)}
        end

      [grow, shrink, basis] ->
        with {:ok, grow} <- parse_nonnegative_number(grow),
             {:ok, shrink} <- parse_nonnegative_number(shrink),
             {:ok, basis} <- flex_basis_value(basis) do
          {:ok,
           style
           |> Map.put(:flex_grow, grow)
           |> Map.put(:flex_shrink, shrink)
           |> Map.put(:flex_basis, basis)}
        end

      _ ->
        {:error, :invalid_document}
    end
  end

  defp length_property(property) do
    case property do
      "width" -> :width
      "height" -> :height
      "min-width" -> :min_width
      "max-width" -> :max_width
      "max-height" -> :max_height
    end
  end

  defp gap_property(property) do
    case property do
      "row-gap" -> :row_gap
      "column-gap" -> :column_gap
    end
  end

  defp flex_basis_value(value) do
    case value do
      "auto" -> {:ok, :auto}
      value -> parse_length(value)
    end
  end

  defp parse_grid_tracks(value) do
    result =
      value
      |> String.trim()
      |> String.downcase()
      |> HtmlValidator.split_css_tokens()
      |> Enum.reduce_while({:ok, [], 0}, fn source, {:ok, chunks, track_count} ->
        case parse_grid_track_source(source) do
          {:ok, tracks} ->
            track_count = track_count + length(tracks)

            case HtmlValidator.validate_layout_cardinality(:grid_tracks, track_count) do
              :ok -> {:cont, {:ok, [tracks | chunks], track_count}}
              {:error, {_reason, _diagnostic}} = error -> {:halt, error}
            end

          :error ->
            {:halt, :error}

          {:error, {_reason, _diagnostic}} = error ->
            {:halt, error}
        end
      end)

    case result do
      {:ok, chunks, _track_count} -> {:ok, chunks |> Enum.reverse() |> List.flatten()}
      result -> result
    end
  end

  defp parse_grid_track_source(value) do
    normalized = value |> String.trim() |> String.downcase()

    case Regex.named_captures(~r/^repeat\((?<count>[1-9]\d*)\s*,\s*(?<track>.+)\)$/u, normalized) do
      %{"count" => count, "track" => track} ->
        with {count, ""} <- Integer.parse(count),
             :ok <- HtmlValidator.validate_layout_cardinality(:grid_tracks, count),
             {:ok, parsed_track} <- parse_grid_track(track) do
          {:ok, List.duplicate(parsed_track, count)}
        else
          {:error, {_reason, _diagnostic}} = error -> error
          _ -> :error
        end

      _ ->
        with {:ok, track} <- parse_grid_track(normalized), do: {:ok, [track]}
    end
  end

  defp parse_grid_track(value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      String.starts_with?(normalized, "minmax(") ->
        parse_minmax_grid_track(normalized)

      normalized == "auto" ->
        {:ok, :auto}

      Regex.match?(~r/^\d+(?:\.\d+)?fr$/u, normalized) ->
        with {number, "fr"} <- HtmlValidator.parse_css_number(normalized),
             do: {:ok, {:fr, number}}

      true ->
        with {:ok, length} <- parse_length(normalized), do: {:ok, {:length, length}}
    end
  end

  defp parse_minmax_grid_track(value) do
    case Regex.named_captures(~r/^minmax\((?<min>.+)\s*,\s*(?<max>.+)\)$/u, value) do
      %{"min" => min_track, "max" => max_track} ->
        with {:ok, minimum} <- parse_grid_track(min_track),
             {:ok, maximum} <- parse_grid_track(max_track) do
          minimum_valid = minimum == :auto or match?({:length, _length}, minimum)

          maximum_valid =
            maximum == :auto or
              match?({:length, _length}, maximum) or
              match?({:fr, _fraction}, maximum)

          case minimum_valid and maximum_valid do
            true -> {:ok, {:minmax, minimum, maximum}}
            false -> :error
          end
        end

      _ ->
        :error
    end
  end

  defp put_grid_placement(style, property, value) do
    case property do
      property when property in ["grid-column-start", "grid-column-end"] ->
        with {:ok, placement} <- parse_grid_line(value) do
          {:ok, Map.put(style, grid_line_property(property), placement)}
        end

      property when property in ["grid-row-start", "grid-row-end"] ->
        with {:ok, placement} <- parse_grid_line(value) do
          {:ok, Map.put(style, grid_line_property(property), placement)}
        end

      "grid-column" ->
        with {:ok, start_line, end_line} <- parse_grid_axis(value) do
          {:ok,
           style
           |> Map.put(:grid_column_start, start_line)
           |> Map.put(:grid_column_end, end_line)}
        end

      "grid-row" ->
        with {:ok, start_line, end_line} <- parse_grid_axis(value) do
          {:ok,
           style
           |> Map.put(:grid_row_start, start_line)
           |> Map.put(:grid_row_end, end_line)}
        end

      "grid-area" ->
        with {:ok, row_start, column_start, row_end, column_end} <- parse_grid_area(value) do
          {:ok,
           style
           |> Map.put(:grid_row_start, row_start)
           |> Map.put(:grid_column_start, column_start)
           |> Map.put(:grid_row_end, row_end)
           |> Map.put(:grid_column_end, column_end)}
        end
    end
  end

  defp parse_grid_axis(value) do
    case value |> String.trim() |> String.downcase() |> String.split("/", trim: true) do
      [start_line] ->
        with {:ok, start_line} <- parse_grid_line(start_line), do: {:ok, start_line, :auto}

      [start_line, end_line] ->
        with {:ok, start_line} <- parse_grid_line(start_line),
             {:ok, end_line} <- parse_grid_line(end_line) do
          {:ok, start_line, end_line}
        end

      _ ->
        :error
    end
  end

  defp parse_grid_area(value) do
    case value |> String.trim() |> String.downcase() |> String.split("/", trim: true) do
      [row_start, column_start, row_end, column_end] ->
        with {:ok, row_start} <- parse_grid_line(row_start),
             {:ok, column_start} <- parse_grid_line(column_start),
             {:ok, row_end} <- parse_grid_line(row_end),
             {:ok, column_end} <- parse_grid_line(column_end) do
          {:ok, row_start, column_start, row_end, column_end}
        end

      _ ->
        :error
    end
  end

  defp parse_grid_line(value) do
    normalized = value |> String.trim() |> String.downcase()

    case normalized do
      "auto" ->
        {:ok, :auto}

      _ ->
        parse_grid_number_or_span(normalized)
    end
  end

  defp parse_grid_number_or_span(value) do
    cond do
      Regex.match?(~r/^span\s+[1-9]\d*$/u, value) ->
        ["span", count] = String.split(value, ~r/\s+/u, trim: true)
        {count, ""} = Integer.parse(count)

        with :ok <- HtmlValidator.validate_layout_cardinality(:grid_placement, count) do
          {:ok, {:span, count}}
        end

      Regex.match?(~r/^[1-9]\d*$/u, value) ->
        {line, ""} = Integer.parse(value)

        with :ok <- HtmlValidator.validate_layout_cardinality(:grid_placement, line) do
          {:ok, line}
        end

      true ->
        :error
    end
  end

  defp grid_template_property(property) do
    case property do
      "grid-template-columns" -> :grid_template_columns
      "grid-template-rows" -> :grid_template_rows
    end
  end

  defp grid_auto_property(property) do
    case property do
      "grid-auto-columns" -> :grid_auto_columns
      "grid-auto-rows" -> :grid_auto_rows
    end
  end

  defp grid_line_property(property) do
    case property do
      "grid-column-start" -> :grid_column_start
      "grid-column-end" -> :grid_column_end
      "grid-row-start" -> :grid_row_start
      "grid-row-end" -> :grid_row_end
    end
  end

  defp put_text_align(style, value) do
    case String.downcase(String.trim(value)) do
      "left" -> {:ok, Map.put(style, :text_align, :left)}
      "center" -> {:ok, Map.put(style, :text_align, :center)}
      "right" -> {:ok, Map.put(style, :text_align, :right)}
      _ -> {:error, :invalid_document}
    end
  end

  defp put_font_weight(style, value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      normalized == "normal" ->
        {:ok, Map.put(style, :font_weight, 400)}

      normalized == "bold" ->
        {:ok, Map.put(style, :font_weight, 700)}

      Regex.match?(~r/^[1-9]00$/u, normalized) ->
        {weight, ""} = Integer.parse(normalized)
        {:ok, Map.put(style, :font_weight, weight)}

      true ->
        {:error, :invalid_document}
    end
  end

  defp put_font_style(style, value) do
    case String.downcase(String.trim(value)) do
      "normal" -> {:ok, Map.put(style, :font_style, :normal)}
      "italic" -> {:ok, Map.put(style, :font_style, :italic)}
      _ -> {:error, :invalid_document}
    end
  end

  defp parse_box_lengths(value) do
    parse_box_lengths(value, :nonnegative)
  end

  defp parse_box_lengths(value, length_context) do
    HtmlValidator.parse_css_box_lengths(value, length_context)
  end

  defp parse_gap(value) do
    gaps =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_length/1)

    case gaps do
      [{:ok, both}] -> {:ok, both, both}
      [{:ok, row_gap}, {:ok, column_gap}] -> {:ok, row_gap, column_gap}
      _ -> :error
    end
  end

  defp parse_border_spacing(value) do
    lengths =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_length/1)

    case lengths do
      [{:ok, both}] -> {:ok, both, both}
      [{:ok, horizontal}, {:ok, vertical}] -> {:ok, horizontal, vertical}
      _ -> :error
    end
  end

  defp parse_length(value) do
    parse_length(value, :nonnegative)
  end

  defp parse_length(value, length_context) do
    HtmlValidator.parse_css_length(value, length_context)
  end

  defp parse_size(value) do
    HtmlValidator.parse_css_size(value)
  end

  defp parse_position_component(value) do
    normalized = String.trim(value)

    case Regex.run(~r/^(-?\d+(?:\.\d+)?)%$/u, normalized) do
      [_, percentage] ->
        with {number, ""} <- HtmlValidator.parse_css_number(percentage),
             do: {:ok, {:percent, number / 100.0}}

      _ ->
        parse_length(normalized, :margin)
    end
  end

  defp parse_two_axis_position(value) do
    tokens = value |> String.trim() |> String.downcase() |> String.split(~r/\s+/u, trim: true)

    case tokens do
      [token] ->
        case position_token(token) do
          {:ok, :horizontal, component} -> {:ok, {component, {:percent, 0.5}}}
          {:ok, :vertical, component} -> {:ok, {{:percent, 0.5}, component}}
          {:ok, :both, component} -> {:ok, {component, component}}
          {:ok, :component, component} -> {:ok, {component, {:percent, 0.5}}}
          _ -> :error
        end

      [first, second] ->
        with {:ok, first_axis, first_component} <- position_token(first),
             {:ok, second_axis, second_component} <- position_token(second),
             {:ok, position} <-
               order_position_components(
                 first_axis,
                 first_component,
                 second_axis,
                 second_component
               ) do
          {:ok, position}
        end

      _ ->
        :error
    end
  end

  defp position_token(token) do
    case token do
      "left" ->
        {:ok, :horizontal, {:percent, 0.0}}

      "right" ->
        {:ok, :horizontal, {:percent, 1.0}}

      "top" ->
        {:ok, :vertical, {:percent, 0.0}}

      "bottom" ->
        {:ok, :vertical, {:percent, 1.0}}

      "center" ->
        {:ok, :both, {:percent, 0.5}}

      token ->
        case parse_position_component(token) do
          {:ok, component} -> {:ok, :component, component}
          _ -> :error
        end
    end
  end

  defp order_position_components(first_axis, first, second_axis, second) do
    cond do
      first_axis == :vertical and second_axis in [:horizontal, :component, :both] ->
        {:ok, {second, first}}

      first_axis in [:horizontal, :component, :both] and
          second_axis in [:vertical, :component, :both] ->
        {:ok, {first, second}}

      first_axis == :both and second_axis == :horizontal ->
        {:ok, {second, first}}

      true ->
        :error
    end
  end

  defp parse_line_height(value) do
    normalized = value |> String.trim() |> String.downcase()

    cond do
      normalized == "normal" ->
        {:ok, :normal}

      match?({:ok, _length}, parse_length(normalized)) ->
        parse_length(normalized)

      true ->
        with {:ok, multiplier} <- parse_nonnegative_number(normalized) do
          {:ok, {:multiplier, multiplier}}
        end
    end
  end

  defp parse_nonnegative_number(value) do
    case HtmlValidator.parse_css_number(String.trim(value)) do
      {number, ""} when number >= 0 -> {:ok, number}
      _ -> :error
    end
  end

  defp put_box_lengths(style, property, lengths) do
    case property do
      "margin" ->
        style
        |> Map.put(:margin, lengths)
        |> Map.put(:margin_after, lengths.bottom)
        |> Map.delete(:_margin_before_em)
        |> Map.delete(:_margin_after_em)

      "padding" ->
        Map.put(style, :padding, lengths)

      "border-width" ->
        Map.put(style, :border_widths, lengths)
    end
  end

  defp put_edge_length(style, property, length) do
    [box_property, edge] = String.split(property, "-", parts: 2)
    key = String.to_existing_atom(edge)
    map_key = String.to_existing_atom(box_property)
    lengths = style |> Map.fetch!(map_key) |> Map.put(key, length)
    style = Map.put(style, map_key, lengths)

    case property do
      "margin-top" ->
        Map.delete(style, :_margin_before_em)

      "margin-bottom" ->
        style
        |> Map.put(:margin_after, length)
        |> Map.delete(:_margin_after_em)

      _ ->
        style
    end
  end

  defp put_border_edge_width(style, property, width) do
    edge =
      property
      |> String.replace_prefix("border-", "")
      |> String.to_existing_atom()

    Map.update!(style, :border_widths, &Map.put(&1, edge, width))
  end

  defp put_border_edge_color(style, property, color) do
    edge =
      property
      |> String.replace_prefix("border-", "")
      |> String.to_existing_atom()

    Map.update(
      style,
      :border_colors,
      edges(Map.get(style, :border_color, :current_color)),
      &Map.put(&1, edge, color)
    )
  end

  defp put_border_edge_style(style, property, border_style) do
    edge =
      property
      |> String.replace_prefix("border-", "")
      |> String.to_existing_atom()

    Map.update(style, :border_styles, edges(:none), &Map.put(&1, edge, border_style))
  end

  defp border_edge_property(property) do
    property
    |> String.replace_suffix("-width", "")
    |> String.replace_suffix("-color", "")
    |> String.replace_suffix("-style", "")
  end

  defp parse_border(value, current_color) do
    tokens = HtmlValidator.split_css_tokens(value)

    parsed =
      Enum.reduce_while(tokens, %{width: nil, style: nil, color: nil}, fn token, acc ->
        cond do
          is_nil(acc.style) and match?({:ok, _}, parse_border_style(token)) ->
            {:ok, border_style} = parse_border_style(token)
            {:cont, %{acc | style: border_style}}

          is_nil(acc.width) and match?({:ok, _}, parse_border_width(token)) ->
            {:ok, width} = parse_border_width(token)
            {:cont, %{acc | width: width}}

          is_nil(acc.color) and
              match?({:ok, _}, HtmlValidator.parse_css_color(token, current_color)) ->
            {:ok, color} = HtmlValidator.parse_css_color(token, current_color)
            {:cont, %{acc | color: transparent_color_to_nil(color)}}

          true ->
            {:halt, :error}
        end
      end)

    case parsed do
      %{width: width, style: border_style, color: color} ->
        width = width || @medium_border_width
        border_style = border_style || :none

        color =
          if is_nil(color) and not Enum.any?(tokens, &transparent_token?/1),
            do: current_color,
            else: color

        {:ok, edges(width), color, edges(border_style)}

      :error ->
        :error
    end
  end

  defp parse_border_edge(value, current_color) do
    with {:ok, border_widths, border_color, border_styles} <-
           parse_border(value, current_color) do
      {:ok, border_widths.top, border_color, border_styles.top}
    end
  end

  defp parse_border_styles(value) do
    styles =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_border_style/1)

    case styles do
      [{:ok, all}] ->
        {:ok, edges(all)}

      [{:ok, vertical}, {:ok, horizontal}] ->
        {:ok, edges(vertical, horizontal, vertical, horizontal)}

      [{:ok, top}, {:ok, horizontal}, {:ok, bottom}] ->
        {:ok, edges(top, horizontal, bottom, horizontal)}

      [{:ok, top}, {:ok, right}, {:ok, bottom}, {:ok, left}] ->
        {:ok, edges(top, right, bottom, left)}

      _ ->
        :error
    end
  end

  defp parse_border_colors(value, current_color) do
    colors =
      value
      |> HtmlValidator.split_css_tokens()
      |> Enum.map(fn color ->
        case HtmlValidator.parse_css_color(color, current_color) do
          {:ok, color} -> {:ok, transparent_color_to_nil(color)}
          :error -> :error
        end
      end)

    case colors do
      [{:ok, all}] ->
        {:ok, edges(all)}

      [{:ok, vertical}, {:ok, horizontal}] ->
        {:ok, edges(vertical, horizontal, vertical, horizontal)}

      [{:ok, top}, {:ok, horizontal}, {:ok, bottom}] ->
        {:ok, edges(top, horizontal, bottom, horizontal)}

      [{:ok, top}, {:ok, right}, {:ok, bottom}, {:ok, left}] ->
        {:ok, edges(top, right, bottom, left)}

      _ ->
        :error
    end
  end

  defp parse_border_style(value) do
    normalized = value |> String.trim() |> String.downcase()

    case Enum.find(@border_styles, &(Atom.to_string(&1) == normalized)) do
      nil -> :error
      border_style -> {:ok, border_style}
    end
  end

  defp parse_border_widths(value) do
    widths =
      value
      |> String.split(~r/\s+/u, trim: true)
      |> Enum.map(&parse_border_width/1)

    case widths do
      [{:ok, all}] ->
        {:ok, edges(all)}

      [{:ok, vertical}, {:ok, horizontal}] ->
        {:ok, edges(vertical, horizontal, vertical, horizontal)}

      [{:ok, top}, {:ok, horizontal}, {:ok, bottom}] ->
        {:ok, edges(top, horizontal, bottom, horizontal)}

      [{:ok, top}, {:ok, right}, {:ok, bottom}, {:ok, left}] ->
        {:ok, edges(top, right, bottom, left)}

      _ ->
        :error
    end
  end

  defp parse_border_width(value) do
    case value |> String.trim() |> String.downcase() do
      "thin" -> {:ok, 0.75}
      "medium" -> {:ok, @medium_border_width}
      "thick" -> {:ok, 3.75}
      value -> parse_length(value)
    end
  end

  defp transparent_color_to_nil(color) do
    case color do
      :transparent -> nil
      color -> color
    end
  end

  defp page_break_value(value) do
    case String.downcase(String.trim(value)) do
      "auto" -> {:ok, :auto}
      "always" -> {:ok, :page}
      "page" -> {:ok, :page}
      _ -> :error
    end
  end

  defp transparent_token?(token) do
    String.downcase(String.trim(token)) == "transparent"
  end
end
