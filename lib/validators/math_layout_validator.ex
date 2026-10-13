defmodule NativeElixirPdfUtilities.Validators.MathLayoutValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.MathOperatorValidator
  alias NativeElixirPdfUtilities.Validators.MathTableValidator

  @grouping_elements ~w(math mrow mphantom)
  @rendered_elements @grouping_elements ++
                       ~w(mi mn mo mtext ms mspace mfrac msqrt mroot mtable mtr mtd)

  @doc false
  @spec validate_flow_geometry([term()], number(), number()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_flow_geometry(nodes, width, height) do
    NativeElixirPdfUtilities.HtmlToPdf.MathLayout.with_context(fn ->
      validate_nodes_geometry(nodes, width, height)
    end)
  end

  defp validate_nodes_geometry(nodes, width, height) do
    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      result =
        case node do
          %{style: %{_math_box: measured}} ->
            with :ok <- validate_measured_geometry(measured) do
              if measured.width <= width and measured.ascent + measured.descent <= height do
                :ok
              else
                Diagnostics.error(
                  :layout,
                  :invalid_layout,
                  "atomic MathML equation is #{measured.width} × #{measured.ascent + measured.descent}pt and exceeds the printable #{width} × #{height}pt area; increase the page size or reduce the equation font size",
                  operation: :layout,
                  module: __MODULE__,
                  source: "math"
                )
              end
            end

          %{children: children} ->
            validate_nodes_geometry(children, width, height)

          _ ->
            :ok
        end

      if result == :ok, do: {:cont, :ok}, else: {:halt, result}
    end)
  end

  @doc false
  @spec validate_measured_geometry(term()) ::
          :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_measured_geometry(measured) do
    NativeElixirPdfUtilities.HtmlToPdf.MathLayout.with_context(fn ->
      with :ok <- validate_row([measured]),
           :ok <- validate_measured_box(measured) do
        Enum.reduce_while(measured.drawing_items, :ok, fn item, :ok ->
          result =
            case item do
              %{kind: :native, box: box} ->
                HtmlValidator.validate_measured_child_paint([box], %{})

              _ ->
                :ok
            end

          if result == :ok, do: {:cont, :ok}, else: {:halt, result}
        end)
      end
    end)
  end

  defp validate_measured_box(measured) do
    valid? =
      case measured do
        %{
          actual_text: text,
          drawing_items: items,
          glyphs: glyphs,
          width: width,
          min_content_width: minimum,
          max_content_width: maximum,
          ascent: ascent,
          descent: descent,
          ink_ascent: ink_ascent,
          ink_descent: ink_descent
        }
        when is_binary(text) and is_list(items) ->
          drawing_glyphs =
            items
            |> Enum.filter(&(is_map(&1) and Map.get(&1, :kind) == :glyph))
            |> Enum.map(&Map.drop(&1, [:kind, :logical_group, :logical_text]))

          String.valid?(text) and is_number(width) and width >= 0 and
            Enum.all?([minimum, maximum, ink_ascent, ink_descent], &is_number/1) and
            ascent + descent >= 0 and minimum >= 0 and maximum >= minimum and
            drawing_glyphs == glyphs and
            Enum.all?(items, fn item ->
              case item do
                %{kind: :glyph} ->
                  true

                %{kind: :logical, actual_text: text, x: x, y: y} ->
                  is_number(x) and is_number(y) and is_binary(text) and String.valid?(text)

                %{kind: :native, box: _, x: x, y: y} ->
                  is_number(x) and is_number(y)

                %{kind: :background, x: x, y: y, width: width, height: height, style: style} ->
                  Enum.all?([x, y, width, height], &is_number/1) and width >= 0 and height >= 0 and
                    is_map(style)

                _ ->
                  false
              end
            end)

        _ ->
          false
      end

    if valid?,
      do: :ok,
      else:
        failure(
          "math boxes require bounded layout/ink extents, ordered intrinsic widths, original text and validated drawing items"
        )
  end

  @doc false
  @spec validate_element_support(map()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_element_support(node) do
    if node.tag in @rendered_elements and
         (node.tag != "mspace" or
            Enum.all?(node.children, fn child ->
              case child do
                %{type: :text, text: text} -> Regex.match?(~r/^[ \t\n\r\f]*$/u, text)
                %{type: :processing_instruction} -> true
                _ -> false
              end
            end)) do
      :ok
    else
      Diagnostics.error(
        :layout,
        :unsupported_html,
        "MathML #{node.tag} or its content model requires a later math layout batch; its visible content cannot be rendered yet",
        operation: :render,
        module: __MODULE__,
        source: node.tag
      )
    end
  end

  @doc false
  @spec defaults(map(), map()) :: map()
  def defaults(node, inherited) do
    root? = node.tag == "math"
    block? = String.downcase(Map.get(node.attributes, "display", "")) == "block"

    %{
      display: if(root? and not block?, do: :inline_block, else: :block),
      _math_display: if(root? and not block?, do: :inline, else: :block),
      _math_text: true,
      _math_formatting: true,
      font_size: if(root?, do: inherited.font_size, else: :math),
      text_transform: if(node.tag == "mi", do: :math_auto, else: :none),
      _math_context: %{
        size: inherited.font_size,
        depth: Map.get(inherited, :math_depth, 0),
        style: Map.get(inherited, :math_style, :normal),
        font: inherited.font_face
      }
    }
    |> then(fn defaults ->
      defaults =
        case node.tag do
          "mtable" ->
            Map.merge(defaults, %{
              display: :inline_table,
              math_style: :compact,
              border_spacing: {0.0, 0.0}
            })

          "mtr" ->
            Map.put(defaults, :display, :table_row)

          "mtd" ->
            Map.merge(defaults, %{
              display: :table_cell,
              text_align: :center,
              vertical_align: :baseline,
              padding: %{
                top: {:math_cell_ex, 0.5},
                bottom: {:math_cell_ex, 0.5},
                left: {:math_cell_em, 0.4},
                right: {:math_cell_em, 0.4}
              }
            })

          _ ->
            defaults
        end

      defaults =
        if node.tag in ["msqrt", "mroot"],
          do: Map.put(defaults, :math_shift, :compact),
          else: defaults

      defaults =
        if node.tag == "mfrac",
          do: Map.put(defaults, :padding, %{top: 0.0, right: 0.75, bottom: 0.0, left: 0.75}),
          else: defaults

      if root? do
        Map.merge(defaults, %{
          font_families: ["math"],
          font_style: :normal,
          font_weight: 400,
          math_depth: 0,
          math_style: if(block?, do: :normal, else: :compact),
          math_shift: :normal,
          white_space: :normal,
          line_height_normal: true,
          letter_spacing: 0.0
        })
      else
        defaults
      end
    end)
  end

  @doc false
  @spec schema_child_defaults(map(), map(), [map()]) :: map()
  def schema_child_defaults(node, inherited, ancestors) do
    case ancestors do
      [%{namespace: :mathml, tag: "mroot", children: children} | _] ->
        first = Enum.find(children, &(Map.get(&1, :type) == :element))

        if first != nil and Map.has_key?(node, :_selector_id) and
             Map.get(first, :_selector_id) != Map.get(node, :_selector_id),
           do: %{math_depth: Map.get(inherited, :math_depth, 0) + 2, math_style: :compact},
           else: %{}

      [%{namespace: :mathml, tag: "mfrac", children: children} | _] ->
        elements = Enum.filter(children, &(Map.get(&1, :type) == :element))
        second = Enum.at(elements, 1)

        defaults = %{
          math_depth:
            Map.get(inherited, :math_depth, 0) +
              if(Map.get(inherited, :math_style, :normal) == :compact, do: 1, else: 0),
          math_style: :compact
        }

        if second != nil and Map.has_key?(node, :_selector_id) and
             Map.get(second, :_selector_id) == Map.get(node, :_selector_id),
           do: Map.put(defaults, :math_shift, :compact),
           else: defaults

      _ ->
        %{}
    end
  end

  @doc false
  @spec style_hints(map()) :: [{String.t(), String.t()}]
  def style_hints(node) do
    if Map.get(node, :namespace) == :mathml do
      Enum.flat_map(node.attributes, fn {name, value} ->
        case name do
          name when name in ["mathcolor", "mathbackground"] ->
            case HtmlValidator.parse_css_color(value, {0, 0, 0}) do
              {:ok, _} ->
                [{if(name == "mathcolor", do: "color", else: "background-color"), value}]

              _ ->
                []
            end

          "mathsize" when value not in ["inherit", "math"] ->
            case HtmlValidator.parse_css_number(value) do
              {number, "ex"} when number >= 0 ->
                [{"font-size", value}]

              _ ->
                case parse_font_size(value, %{font_size: 1, _root_font_size: 1}) do
                  {:ok, _} -> [{"font-size", value}]
                  _ -> []
                end
            end

          "displaystyle" ->
            case String.downcase(value) do
              "true" -> [{"math-style", "normal"}]
              "false" -> [{"math-style", "compact"}]
              _ -> []
            end

          "scriptlevel" ->
            if Regex.match?(~r/^[+-]?[0-9]+$/, value) do
              [
                {"math-depth",
                 if(String.starts_with?(value, ["+", "-"]), do: "add(#{value})", else: value)}
              ]
            else
              []
            end

          "mathvariant" ->
            if node.tag == "mi" and String.downcase(value) == "normal",
              do: [{"text-transform", "none"}],
              else: []

          _ ->
            []
        end
      end)
    else
      []
    end
  end

  @doc false
  @spec parse_property(String.t(), String.t(), map()) :: {:ok, map()} | :error
  def parse_property(property, value, style) do
    value = value |> String.trim() |> String.downcase()

    case {property, value} do
      {"math-style", "normal"} ->
        {:ok, %{math_style: :normal}}

      {"math-style", "compact"} ->
        {:ok, %{math_style: :compact}}

      {"math-shift", "normal"} ->
        {:ok, %{math_shift: :normal}}

      {"math-shift", "compact"} ->
        {:ok, %{math_shift: :compact}}

      {"math-depth", value} ->
        inherited =
          Map.get(style, :_math_context, %{
            depth: Map.get(style, :math_depth, 0),
            style: Map.get(style, :math_style, :normal)
          })

        parsed =
          case value do
            "auto-add" ->
              inherited.depth + if(inherited.style == :compact, do: 1, else: 0)

            _ ->
              case Regex.run(~r/^add\(\s*([+-]?[0-9]+)\s*\)$/, value) do
                [_, number] ->
                  case HtmlValidator.parse_css_number(number) do
                    {number, ""} -> inherited.depth + trunc(number)
                    _ -> nil
                  end

                nil ->
                  if Regex.match?(~r/^[+-]?[0-9]+$/, value) do
                    case HtmlValidator.parse_css_number(value) do
                      {number, ""} -> trunc(number)
                      _ -> nil
                    end
                  end
              end
          end

        if is_integer(parsed) and abs(parsed) <= Limits.get(:max_css_numeric_magnitude),
          do: {:ok, %{math_depth: parsed}},
          else: :error

      {"text-transform", "math-auto"} ->
        {:ok, %{text_transform: :math_auto}}

      {"font-size", _} ->
        with {:ok, size} <- parse_font_size(value, style), do: {:ok, %{font_size: size}}

      {"display", "inline math"} ->
        if Map.has_key?(style, :_math_context),
          do: {:ok, %{display: :inline, _math_display: :inline, _math_formatting: true}},
          else: :error

      {"display", value} when value in ["math", "block math"] ->
        if Map.has_key?(style, :_math_context),
          do: {:ok, %{display: :block, _math_display: :block, _math_formatting: true}},
          else: :error

      _ ->
        :error
    end
  end

  @doc false
  @spec parse_font_size(String.t(), map()) :: {:ok, number() | :math} | :error
  def parse_font_size(value, style) do
    case value do
      "math" ->
        if Map.has_key?(style, :_math_context), do: {:ok, :math}, else: :error

      "inherit" ->
        {:ok, Map.get(style, :_math_context, %{size: style.font_size}).size}

      _ ->
        size = Map.get(style, :_math_context, %{size: style.font_size}).size

        case HtmlValidator.parse_css_number(value) do
          {_number, "ex"} ->
            parent =
              Map.get(style, :_math_context, %{font: Map.get(style, :font_face), size: size})

            with {:ok, value} <- math_length(value, %{font_face: parent.font, font_size: size}),
                 true <- value >= 0 do
              {:ok, value}
            else
              _ -> :error
            end

          {number, "%"} when number >= 0 ->
            case HtmlValidator.parse_css_number(value, size / 100) do
              {scaled, "%"} -> {:ok, scaled}
              _ -> :error
            end

          _ ->
            case HtmlValidator.parse_css_length(value, :nonnegative, size) do
              {:ok, {:rem, ratio}} -> {:ok, ratio * style._root_font_size}
              result -> result
            end
        end
    end
  end

  @doc false
  @spec compute_style(map()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def compute_style(style) do
    computed =
      case Map.pop(style, :_math_context) do
        {nil, style} ->
          {:ok, style}

        {parent, style} ->
          depth = Map.get(style, :math_depth, parent.depth)

          if style.font_size == :math do
            FontValidator.reserve(:max_font_math_work, abs(depth - parent.depth))

            with {:ok, metrics} <- Font.math_metrics(parent.font) do
              if metrics.constants.script_percent_scale_down <= 0 or
                   metrics.constants.script_script_percent_scale_down <= 0 do
                failure("selected font MATH script scaling percentages must be positive")
              else
                a = min(parent.depth, depth)
                b = max(parent.depth, depth)

                {factor, remainder} =
                  cond do
                    a == b ->
                      {1.0, 0}

                    a <= 0 and b >= 2 ->
                      {metrics.constants.script_script_percent_scale_down / 100, b - a - 2}

                    a == 1 ->
                      {metrics.constants.script_script_percent_scale_down /
                         metrics.constants.script_percent_scale_down, b - a - 1}

                    b == 1 ->
                      {metrics.constants.script_percent_scale_down / 100, b - a - 1}

                    true ->
                      {1.0, b - a}
                  end

                logarithm = :math.log(factor) + remainder * :math.log(0.71)
                logarithm = if depth < parent.depth, do: -logarithm, else: logarithm

                # IEEE-754 binary64's normal exponent range is a numeric invariant,
                # not a configurable work limit. Validate before exp can overflow.
                if logarithm < :math.log(2.2250738585072014e-308) or
                     logarithm > :math.log(Limits.get(:max_css_numeric_magnitude)) or
                     (parent.size > 0 and
                        :math.log(parent.size) + logarithm >
                          :math.log(Limits.get(:max_css_numeric_magnitude))) do
                  failure(
                    "math-depth scales the inherited font size outside supported numeric bounds"
                  )
                else
                  {:ok,
                   style
                   |> Map.put(:font_size, parent.size * :math.exp(logarithm))
                   |> Map.put(:math_depth, depth)}
                end
              end
            end
          else
            {:ok, Map.put(style, :math_depth, depth)}
          end
      end

    with {:ok, style} <- computed, do: MathTableValidator.resolve_padding(style)
  end

  @doc false
  @spec prepare_element(term(), map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_element(node, context) do
    NativeElixirPdfUtilities.HtmlToPdf.MathLayout.with_context(fn ->
      with {:ok, prepared} <- prepare_node(node, context) do
        inline = Map.get(context, :inline_stretch_size)
        block = Map.get(context, :block_stretch_size)
        constraint = if inline != nil, do: inline, else: block

        with :ok <- validate_operator_constraint(constraint) do
          cond do
            inline != nil and (block != nil or not is_number(inline)) ->
              failure("math preparation requires one inline or block stretch constraint")

            block != nil and not is_tuple(block) ->
              failure("math preparation requires one inline or block stretch constraint")

            true ->
              {:ok, prepared |> Map.put(:root, true) |> Map.put(:stretch_constraint, constraint)}
          end
        end
      end
    end)
  end

  defp prepare_node(node, context) do
    case {node, context} do
      {%{
         type: :element,
         namespace: :mathml,
         tag: tag,
         attributes: attributes,
         style:
           %{
             display: display,
             font_size: size,
             font_face: font,
             color: color,
             line_height: line_height
           } = style,
         children: children
       }, context}
      when is_map(attributes) and is_list(children) and is_map(context) and
             is_number(size) and size >= 0 and is_map(font) and is_tuple(color) and
             is_number(line_height) and line_height >= 0 ->
        with :ok <- validate_element_support(node),
             :ok <- validate_box_style(tag, style) do
          cond do
            tag == "mtable" and display in [:inline_table, :table] ->
              MathTableValidator.prepare(node, context)

            tag in ["mtr", "mtd"] and display != :none ->
              Diagnostics.error(
                :layout,
                :unsupported_html,
                "MathML table rows and cells require a containing mtable",
                operation: :measure_math,
                module: __MODULE__,
                source: tag
              )

            display not in [:block, :inline, :inline_block, :none] ->
              Diagnostics.error(
                :layout,
                :unsupported_html,
                "MathML #{tag} with #{display} display requires the later CSS composition batch",
                operation: :render,
                module: __MODULE__,
                source: tag
              )

            display == :none ->
              {:ok, %{kind: :space, width: 0.0, ascent: 0.0, descent: 0.0, style: style}}

            Map.get(style, :background_image) != nil ->
              Diagnostics.error(
                :layout,
                :unsupported_html,
                "MathML background images require the later box composition batch",
                operation: :render,
                module: __MODULE__,
                source: tag
              )

            tag == "mspace" ->
              attributes = node.attributes

              values =
                Enum.reduce_while(["width", "height", "depth"], {:ok, []}, fn property,
                                                                              {:ok, values} ->
                  case math_length(
                         Map.get(attributes, property, "0"),
                         style
                       ) do
                    {:ok, number} when is_number(number) -> {:cont, {:ok, values ++ [number]}}
                    {:error, _} = error -> {:halt, error}
                    _ -> {:cont, {:ok, values ++ [0.0]}}
                  end
                end)

              with {:ok, [width, requested_ascent, descent]} <- values do
                ascent = max(requested_ascent, 0.0)
                width = max(width, 0.0)
                width = Map.get(style, :width, width)
                height = Map.get(style, :height, max(requested_ascent + descent, 0.0))

                if is_number(width) and is_number(height) and width >= 0 and height >= 0 do
                  {:ok,
                   %{
                     kind: :space,
                     width: width,
                     ascent: ascent,
                     descent: height - ascent,
                     style: style
                   }}
                else
                  failure("mspace requires resolved nonnegative width and height")
                end
              end

            tag == "mo" ->
              with {:ok, runs} <- token_runs(children, true),
                   {:ok, x_height} <- FontValidator.prepare_x_height(font),
                   {:ok, operator} <-
                     MathOperatorValidator.prepare(
                       runs |> token_characters() |> Enum.map_join(& &1.text),
                       attributes,
                       Map.merge(Map.get(context, :operator_context, %{}), %{
                         font_size: size,
                         root_font_size: Map.get(style, :_root_font_size, size),
                         x_height: x_height * size,
                         text_only: Enum.all?(children, &(Map.get(&1, :type) == :text))
                       })
                     ),
                   {:ok, content} <-
                     if(operator.invisible,
                       do:
                         {:ok,
                          %{
                            kind: :space,
                            width: 0.0,
                            ascent: 0.0,
                            descent: 0.0,
                            style: style,
                            actual_text: operator.text
                          }},
                       else: prepare_token_content(children, style, runs, context)
                     ) do
                source =
                  case content do
                    %{kind: :token, children: [%{glyph: glyph}]} when operator.single_character ->
                      glyph

                    _ ->
                      nil
                  end

                {:ok,
                 %{
                   kind: :operator,
                   content: content,
                   style: style,
                   actual_text: content.actual_text,
                   core_operator: operator,
                   source_glyph: source,
                   math_formatting: Map.get(style, :_math_formatting, true)
                 }}
              end

            tag in ~w(mi mn mtext ms) ->
              with {:ok, runs} <- token_runs(children, true),
                   do: prepare_token_content(children, style, runs, context)

            true ->
              with {:ok, row} <- prepare_row_children(node, context) do
                cond do
                  tag == "mfrac" and length(row.children) == 2 ->
                    prepare_fraction(row, attributes)

                  tag == "msqrt" ->
                    prepare_radical(
                      %{
                        row
                        | style:
                            Map.drop(style, [:width, :height, :padding, :border_widths, :margin])
                      },
                      nil,
                      style
                    )

                  tag == "mroot" and length(row.children) == 2 ->
                    [base, index] = row.children
                    prepare_radical(base, index, style)

                  tag == "mphantom" ->
                    {:ok, Map.put(row, :hide_paint, true)}

                  true ->
                    {:ok, row}
                end
              end
          end
        end

      _ ->
        failure(
          "math preparation requires styled MathML nodes; non-token HTML children need a later composition batch"
        )
    end
  end

  # Core 3.3.3 resolves the selected face's values before geometry consumes them.
  defp prepare_radical(base, index, style) do
    mode = Map.get(style, :math_style, :normal)

    if mode in [:normal, :compact] and style.font_size <= Limits.get(:max_css_numeric_magnitude) do
      with {:ok, metrics} <- Font.math_metrics(style.font_face) do
        gap =
          if mode == :compact,
            do: :radical_vertical_gap,
            else: :radical_display_style_vertical_gap

        names = [
          gap,
          :radical_rule_thickness,
          :radical_extra_ascender,
          :radical_kern_before_degree,
          :radical_kern_after_degree
        ]

        FontValidator.reserve(:max_font_math_work, length(names))

        values =
          Enum.reduce_while(names, {:ok, []}, fn name, {:ok, values} ->
            case prepare_value(Map.fetch!(metrics.constants, name), style.font_size) do
              {:ok, {value, pixels}} ->
                {:cont,
                 {:ok, values ++ [value * style.font_size / metrics.units_per_em + pixels]}}

              error ->
                {:halt, error}
            end
          end)

        with {:ok, [gap, thickness, extra, before_degree, after_degree]} <- values do
          if thickness > 0,
            do: HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :layout)

          {:ok,
           %{
             kind: :radical,
             base: base,
             index: index,
             style: style,
             gap: gap,
             rule_thickness: max(thickness, 0.0),
             extra_ascender: extra,
             kern_before: max(before_degree, 0.0),
             kern_after: after_degree,
             degree_raise: metrics.constants.radical_degree_bottom_raise_percent / 100
           }}
        end
      end
    else
      failure("radicals require a bounded font size and normal or compact math-style")
    end
  end

  @doc false
  @spec prepare_radical_stretch(term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_radical_stretch(prepared, measured) do
    case {prepared, measured} do
      {%{
         kind: :radical,
         style: %{font_face: font, font_size: size},
         gap: gap,
         rule_thickness: thickness
       }, %{ink_ascent: ascent, ink_descent: descent}}
      when is_number(ascent) and is_number(descent) and is_number(gap) and is_number(thickness) ->
        target = max(thickness + gap + ascent + descent, 0.0)

        with {:ok, base} <- prepare_glyph("√", font, size, []),
             {:ok, shape} <-
               prepare_stretch("√", font, size, :vertical, target, []) do
          construction = Map.fetch!(base.source.metrics.variants.vertical, base.glyph_id)
          {:ok, %{shape: shape, intrinsic_width: preferred_vertical_width(base, construction)}}
        end

      _ ->
        failure(
          "radical stretching requires validator-prepared radical state and measured base ink"
        )
    end
  end

  defp prepare_row_children(node, context) do
    %{tag: tag, children: children, style: style} = node

    in_flow =
      Enum.reject(children, fn child ->
        Map.get(child, :style, %{}) |> Map.get(:display) == :none or
          (Map.get(child, :type) == :text and
             Regex.match?(~r/^[ \t\n\r\f]*$/u, child.text))
      end)

    count = Enum.count(in_flow, &(not space_like?(&1)))
    embellished? = embellished?(node)

    in_flow
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, [], 0}, fn {child, child_index}, {:ok, prepared, index} ->
      operator_context =
        if embellished? and tag != "math" do
          Map.get(context, :operator_context, %{})
        else
          %{parent: tag, index: index, count: count, child_index: child_index}
        end

      child_context = Map.put(context, :operator_context, operator_context)

      result =
        case child do
          %{type: :text} ->
            prepare_node(
              %{
                type: :element,
                namespace: :mathml,
                tag: "mtext",
                attributes: %{},
                style: style,
                children: [child]
              },
              child_context
            )

          _ ->
            prepare_node(child, child_context)
        end

      case result do
        {:ok, result} ->
          {:cont, {:ok, [result | prepared], index + if(space_like?(child), do: 0, else: 1)}}

        error ->
          {:halt, error}
      end
    end)
    |> case do
      {:ok, prepared, _index} ->
        children = Enum.reverse(prepared)

        core =
          if embellished?, do: Enum.find_value(children, &Map.get(&1, :core_operator))

        {:ok,
         %{
           kind: :row,
           children: children,
           style: style,
           core_operator: core,
           add_operator_space: tag == "math" or not embellished?
         }}

      error ->
        error
    end
  end

  # Core 3.3.2 resolves font values and authored thickness before measurement.
  defp prepare_fraction(row, attributes) do
    style = row.style
    math_style = Map.get(style, :math_style, :normal)

    if math_style in [:normal, :compact] and is_number(style.font_size) and
         style.font_size <= Limits.get(:max_css_numeric_magnitude) do
      with {:ok, metrics} <- Font.math_metrics(style.font_face) do
        names =
          if math_style == :compact do
            [
              :axis_height,
              :fraction_rule_thickness,
              :fraction_numerator_shift_up,
              :fraction_numerator_gap_min,
              :fraction_denominator_shift_down,
              :fraction_denominator_gap_min,
              :stack_top_shift_up,
              :stack_bottom_shift_down,
              :stack_gap_min
            ]
          else
            [
              :axis_height,
              :fraction_rule_thickness,
              :fraction_numerator_display_style_shift_up,
              :fraction_num_display_style_gap_min,
              :fraction_denominator_display_style_shift_down,
              :fraction_denom_display_style_gap_min,
              :stack_top_display_style_shift_up,
              :stack_bottom_display_style_shift_down,
              :stack_display_style_gap_min
            ]
          end

        FontValidator.reserve(:max_font_math_work, length(names))

        values =
          Enum.reduce_while(names, {:ok, []}, fn name, {:ok, values} ->
            case prepare_value(Map.fetch!(metrics.constants, name), style.font_size) do
              {:ok, {value, pixels}} ->
                {:cont,
                 {:ok, values ++ [value * style.font_size / metrics.units_per_em + pixels]}}

              error ->
                {:halt, error}
            end
          end)

        with {:ok,
              [
                axis,
                default,
                numerator_shift,
                numerator_gap,
                denominator_shift,
                denominator_gap,
                stack_top,
                stack_bottom,
                stack_gap
              ]} <- values do
          default = max(default, 0.0)

          thickness =
            case Map.get(attributes, "linethickness") do
              nil ->
                {:ok, default}

              value ->
                case HtmlValidator.parse_css_number(value) do
                  {number, "%"} ->
                    {:ok, number * default / 100}

                  _ ->
                    case math_length(value, style) do
                      {:ok, {:rem, ratio}} ->
                        {:ok, ratio * Map.get(style, :_root_font_size, style.font_size)}

                      {:ok, number} when is_number(number) ->
                        {:ok, number}

                      {:error, {_reason, _diagnostic}} = error ->
                        error

                      _ ->
                        {:ok, default}
                    end
                end
            end

          with {:ok, thickness} <- thickness do
            if thickness > 0,
              do: HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :layout)

            {:ok,
             row
             |> Map.put(:kind, :fraction)
             |> Map.put(:axis, axis)
             |> Map.put(:rule_thickness, max(thickness, 0.0))
             |> Map.put(:numerator_shift, numerator_shift)
             |> Map.put(:numerator_gap, numerator_gap)
             |> Map.put(:denominator_shift, denominator_shift)
             |> Map.put(:denominator_gap, denominator_gap)
             |> Map.put(:stack_top, stack_top)
             |> Map.put(:stack_bottom, stack_bottom)
             |> Map.put(:stack_gap, stack_gap)}
          end
        end
      end
    else
      failure("fractions require a bounded font size and normal or compact math-style")
    end
  end

  # Core's multi-character operator fallback is the same text/HTML flow as
  # the other tokens, including blockification of original direct child boxes.
  defp prepare_token_content(children, style, runs, context) do
    native? = Enum.any?(children, &native_token_child?(&1, style.font_face))

    with :ok <- validate_token_text(runs, native?) do
      math_formatting? =
        Map.get(style, :_math_formatting, style.display != :inline_block)

      if native? do
        prepare_native_token(children, style, runs, context, math_formatting?)
      else
        if not math_formatting? or Enum.any?(children, &(Map.get(&1, :type) == :element)) do
          # Math formatting blockifies direct child boxes. An authored
          # flow display instead retains ordinary HTML inline flow.
          lines =
            if not math_formatting?,
              do: [children],
              else: Enum.map(children, &[&1])

          Enum.reduce_while(lines, {:ok, []}, fn line, {:ok, prepared} ->
            with {:ok, runs} <- token_runs(line, true),
                 {:ok, glyphs, text} <- prepare_token_glyphs(runs) do
              if glyphs == [] do
                {:cont, {:ok, prepared}}
              else
                {:cont,
                 {:ok,
                  [
                    %{kind: :token, children: glyphs, style: style, actual_text: text}
                    | prepared
                  ]}}
              end
            else
              error -> {:halt, error}
            end
          end)
          |> case do
            {:ok, prepared} ->
              {:ok,
               %{
                 kind: :flow,
                 children: Enum.reverse(prepared),
                 style: style,
                 actual_text: runs |> token_characters() |> Enum.map_join(& &1.text),
                 baseline_depth:
                   if(prepared == [],
                     do: 0.0,
                     else: HtmlValidator.text_baseline_depth(style)
                   ),
                 line_height: style.line_height
               }}

            error ->
              error
          end
        else
          with {:ok, glyphs, actual_text} <- prepare_token_glyphs(runs),
               do:
                 {:ok,
                  %{
                    kind: :token,
                    children: glyphs,
                    style: style,
                    actual_text: actual_text
                  }}
        end
      end
    end
  end

  # Transparent phrasing retains the existing selected-math-glyph path. Native
  # layout owns boxes with artwork, links, decorations, breaks or atomic sizing.
  defp native_token_child?(node, parent_font) do
    case node do
      %{type: :text} ->
        false

      %{style: %{display: :none}} ->
        false

      %{type: :element, style: style, children: children} ->
        (is_map(Map.get(style, :font_face)) and style.font_face != parent_font) or
          Map.get(style, :display) != :inline or
          Map.get(style, :position, :static) != :static or
          Map.get(style, :background_color) not in [nil, :transparent] or
          Enum.any?(
            [
              :background_image,
              :link_url,
              :image,
              :svg_image,
              :form_kind,
              :width,
              :height,
              :min_width,
              :max_width,
              :min_height,
              :max_height
            ],
            &(Map.get(style, &1) != nil)
          ) or
          Map.get(style, :_text_decorations, []) != [] or
          Enum.any?([:margin, :padding, :border_widths], fn property ->
            case Map.get(style, property, %{}) do
              edges when is_map(edges) -> Enum.any?(Map.values(edges), &(&1 != 0))
              _ -> true
            end
          end) or Enum.any?(children, &native_token_child?(&1, parent_font))
    end
  end

  defp prepare_native_token(children, style, runs, context, math_formatting?) do
    children = Enum.map(children, &prepare_native_text(&1, true))

    boxes =
      if math_formatting? do
        Enum.reject(children, &(Map.get(&1.style, :display) == :none))
      else
        [
          %{
            type: :element,
            tag: "span",
            style: Map.put(style, :display, :inline),
            children: children
          }
        ]
      end

    constraints = %{unsupported_glyphs: Map.get(context, :unsupported_glyphs, :replace)}

    Enum.reduce_while(boxes, {:ok, []}, fn node, {:ok, measured} ->
      case NativeElixirPdfUtilities.HtmlToPdf.Layout.measure_child(
             node,
             Map.get(context, :resource_context),
             constraints
           ) do
        {:ok, child} ->
          child =
            if math_formatting? do
              first = child.first_alphabetic_baseline

              child
              |> Map.put(:ascent, child.ascent - first)
              |> Map.put(:descent, child.descent + first)
              |> Map.put(:ink_ascent, child.ink_ascent - first)
              |> Map.put(:ink_descent, child.ink_descent + first)
              |> Map.put(:baseline_offset, -first)
            else
              Map.put(child, :baseline_offset, 0.0)
            end

          {:cont, {:ok, [child | measured]}}

        error ->
          {:halt, error}
      end
    end)
    |> case do
      {:ok, measured} ->
        {:ok,
         %{
           kind: :native_flow,
           children: Enum.reverse(measured),
           style: style,
           actual_text: runs |> token_characters() |> Enum.map_join(& &1.text)
         }}

      error ->
        error
    end
  end

  # Apply math-auto to each original node before ordinary shaping. A fragment
  # map preserves original Unicode when the native line helper splits its text.
  defp prepare_native_text(node, math_text?) do
    case node do
      %{style: %{display: :none}} ->
        node

      %{type: :text, text: text, style: style} ->
        FontValidator.check(:max_rendered_text_bytes, byte_size(text))
        FontValidator.reserve(:max_font_math_work, byte_size(text))

        displayed =
          if Map.get(style, :text_transform) == :math_auto, do: math_auto_text(text), else: text

        style = if math_text?, do: Map.put(style, :white_space, :nowrap), else: style

        style =
          style
          |> Map.put(:_math_native_group, make_ref())
          |> Map.put(
            :_math_original_characters,
            Map.new(Enum.zip(String.codepoints(displayed), String.codepoints(text)))
          )

        %{node | text: displayed, style: style}

      %{children: children} ->
        %{node | children: Enum.map(children, &prepare_native_text(&1, false))}
    end
  end

  # Core 3.2.4.1 and 3.2.5.1 classify original in-flow structure, before
  # preparation adds anonymous token boxes. This determines the outer form.
  defp space_like?(node) do
    case node do
      %{namespace: :mathml, tag: tag} when tag in ["mtext", "mspace"] ->
        true

      %{namespace: :mathml, tag: tag, children: children} when tag in @grouping_elements ->
        Enum.all?(children, fn child ->
          Map.get(child, :style, %{}) |> Map.get(:display) == :none or space_like?(child)
        end)

      %{type: :text} ->
        # Straight row text is prepared as anonymous mtext below; classify that
        # prescribed result before computing the outer operator's form.
        true

      _ ->
        false
    end
  end

  defp embellished?(node) do
    case node do
      %{namespace: :mathml, tag: "mo"} ->
        true

      %{namespace: :mathml, tag: "mfrac", children: children} ->
        case Enum.reject(children, fn child ->
               Map.get(child, :style, %{}) |> Map.get(:display) == :none or
                 (Map.get(child, :type) == :text and Regex.match?(~r/^[ \t\n\r\f]*$/u, child.text))
             end) do
          [first | _] -> embellished?(first)
          [] -> false
        end

      %{namespace: :mathml, tag: tag, children: children} when tag in @grouping_elements ->
        case Enum.reject(children, fn child ->
               Map.get(child, :style, %{}) |> Map.get(:display) == :none or space_like?(child)
             end) do
          [child] -> embellished?(child)
          _ -> false
        end

      _ ->
        false
    end
  end

  @doc false
  @spec prepare_operator_stretch(term(), term()) ::
          {:ok, map() | nil} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_operator_stretch(prepared, constraint) do
    NativeElixirPdfUtilities.HtmlToPdf.MathLayout.with_context(fn ->
      case {prepared, constraint} do
        {%{
           kind: :operator,
           source_glyph: base,
           core_operator: operator,
           math_formatting: formatting?,
           style: style
         }, constraint}
        when is_map(operator) and is_map(style) and
               (is_nil(constraint) or is_tuple(constraint) or is_number(constraint)) ->
          with :ok <- validate_operator_constraint(constraint) do
            if is_nil(base) or not formatting? do
              {:ok, nil}
            else
              prepare_operator_shape(base, operator, style, constraint)
            end
          end

        _ ->
          failure("operator stretching requires validator-prepared operator state")
      end
    end)
  end

  # Shared transport validation for the public styled-tree measurement entry
  # and the operator request used when a row supplies its measured ink target.
  defp validate_operator_constraint(constraint) do
    valid? =
      case constraint do
        nil ->
          true

        {ascent, descent} ->
          Enum.all?(
            [ascent, descent],
            &(is_number(&1) and abs(&1) <= Limits.get(:max_css_numeric_magnitude))
          )

        width when is_number(width) ->
          width >= 0 and width <= Limits.get(:max_css_numeric_magnitude)

        _ ->
          false
      end

    if valid?,
      do: :ok,
      else: failure("operator stretch constraints require bounded point extents")
  end

  # Core 3.2.4.3 resolves glyph-relative constraints here, so geometry consumers
  # never parse attributes or reinterpret min/max sizing.
  defp prepare_operator_shape(base, operator, style, constraint) do
    scale = base.font_size / base.units_per_em
    construction = Map.get(base.source.metrics.variants[operator.stretch_axis], base.glyph_id)

    stretch? =
      operator.stretchy and construction != nil and
        ((operator.stretch_axis == :vertical and is_tuple(constraint)) or
           (operator.stretch_axis == :horizontal and is_number(constraint)))

    cond do
      stretch? ->
        with {:ok, {axis, pixels}} <-
               prepare_value(base.source.metrics.constants.axis_height, base.font_size) do
          axis = axis * scale + pixels

          {ascent, descent, target} =
            if operator.stretch_axis == :vertical do
              {ascent, descent} = constraint

              {ascent, descent} =
                if operator.symmetric do
                  half = max(ascent - axis, descent + axis)
                  {half + axis, half - axis}
                else
                  {ascent, descent}
                end

              natural = (elem(base.bounds, 3) - elem(base.bounds, 1)) * scale
              minimum = max(resolve_operator_size(operator.minsize, natural), 0)

              maximum =
                case operator.maxsize do
                  :infinity -> :infinity
                  value -> max(resolve_operator_size(value, natural), minimum)
                end

              total = ascent + descent

              target =
                cond do
                  total <= 0 -> minimum
                  total < minimum -> minimum
                  maximum != :infinity and total > maximum -> maximum
                  true -> total
                end

              ascent =
                cond do
                  total <= 0 -> target / 2 + axis
                  target == total -> ascent
                  true -> max(0, (ascent - axis) * (target / total) + axis)
                end

              {ascent, target - ascent, target}
            else
              {nil, nil, constraint}
            end

          with {:ok, shape} <-
                 prepare_stretch(
                   base.text,
                   base.font_face,
                   base.font_size,
                   operator.stretch_axis,
                   target,
                   script_feature_level: min(max(Map.get(style, :math_depth, 0), 0), 2)
                 ) do
            preferred =
              if operator.stretch_axis == :vertical do
                preferred_vertical_width(base, construction)
              else
                base.advance * scale
              end

            {:ok,
             %{
               shape: shape,
               target_ascent: ascent,
               target_descent: descent,
               intrinsic_width: preferred
             }}
          end
        end

      not operator.stretchy and operator.largeop and
          Map.get(style, :math_style, :normal) == :normal ->
        construction = Map.get(base.source.metrics.variants.vertical, base.glyph_id)
        variants = if construction, do: construction.variants, else: []
        minimum = base.source.metrics.constants.display_operator_min_height * scale
        selected = Enum.find(variants, &(&1.advance * scale >= minimum)) || List.last(variants)

        if selected do
          with {:ok, glyph} <- selected_glyph(selected.glyph, base.source),
               do:
                 {:ok,
                  %{
                    shape: %{kind: :glyph, glyph: glyph},
                    target_ascent: nil,
                    target_descent: nil,
                    intrinsic_width: glyph.advance * scale
                  }}
        else
          {:ok, nil}
        end

      true ->
        {:ok, nil}
    end
  end

  defp preferred_vertical_width(base, construction) do
    scale = base.font_size / base.units_per_em

    ids =
      [base.glyph_id] ++
        Enum.map(construction.variants, & &1.glyph) ++
        case construction.assembly do
          %{parts: parts} -> Enum.map(parts, & &1.glyph)
          _ -> []
        end

    Enum.max(Enum.map(ids, &(elem(base.source.advances, &1) * scale)))
  end

  defp resolve_operator_size(value, natural) do
    case value do
      {:percentage, number} -> natural * (number / 100)
      number -> number
    end
  end

  defp validate_box_style(tag, style) do
    unsupported? =
      ((tag in @grouping_elements or tag in ~w(mspace mfrac msqrt mroot)) and
         not Map.get(style, :_math_formatting, true)) or
        Map.get(style, :link_url) != nil or Map.get(style, :link_destination) != nil or
        (tag != "math" and Map.get(style, :html_target) != nil) or
        Map.get(style, :position, :static) != :static or
        Map.get(style, :overflow, :visible) != :visible or
        Map.get(style, :box_sizing, :content_box) != :content_box or
        Enum.any?([:width, :height], fn property ->
          value = Map.get(style, property)

          value != nil and
            (not is_number(value) or
               (tag in ["mfrac", "msqrt", "mroot"] and
                  (value < 0 or value > Limits.get(:max_css_numeric_magnitude))))
        end) or
        Enum.any?(
          [:min_width, :max_width, :min_height, :max_height],
          &(Map.get(style, &1) != nil)
        ) or
        (tag == "math" and
           (Map.get(style, :width) != nil or Map.get(style, :height) != nil or
              Enum.any?([:margin, :padding, :border_widths], fn property ->
                Enum.any?(Map.values(Map.get(style, property, %{})), &(&1 != 0))
              end)))

    if unsupported? do
      Diagnostics.error(
        :layout,
        :unsupported_html,
        "MathML ordinary flow schemata, linked token content or targets, positioned, clipped, constrained or root box sizing requires the later CSS composition batch",
        operation: :render,
        module: __MODULE__,
        source: tag
      )
    else
      :ok
    end
  end

  defp validate_token_text(runs, native?) do
    unsupported =
      Enum.find(runs, fn run ->
        (not native? and Map.get(run.style, :white_space) in [:pre, :pre_wrap, :break_spaces] and
           Regex.match?(~r/[\t\n\r\f]/u, run.text)) or
          Regex.match?(~r/\p{M}/u, run.text) or
          (String.length(run.text) > 1 and
             Regex.match?(~r/[\x{0590}-\x{08FF}\x{FB1D}-\x{FDFF}\x{FE70}-\x{FEFF}]/u, run.text)) or
          (not native? and Map.get(run.style, :_text_decorations, []) != []) or
          (not native? and Map.get(run.style, :letter_spacing, 0.0) != 0) or
          Map.get(run.style, :direction, :ltr) != :ltr
      end)

    if unsupported do
      Diagnostics.error(
        :layout,
        :unsupported_html,
        "MathML token text with preserved breaks/tabs, combining shaping, bidi, letter spacing or decorations requires the later text/composition batch",
        operation: :render,
        module: __MODULE__,
        source: unsupported.text
      )
    else
      :ok
    end
  end

  defp math_length(value, style) do
    case HtmlValidator.parse_css_number(value) do
      {_number, "ex"} ->
        with {:ok, x_height} <- FontValidator.prepare_x_height(style.font_face) do
          scale = x_height * style.font_size

          case HtmlValidator.parse_css_number(value, scale) do
            {number, "ex"} -> {:ok, number}
            _ -> :error
          end
        end

      _ ->
        HtmlValidator.parse_css_length(value, :margin, style.font_size)
    end
  end

  defp token_runs(children, math_text? \\ false) do
    Enum.reduce_while(children, {:ok, []}, fn child, {:ok, runs} ->
      result =
        case child do
          %{type: :text, text: text, style: style} when is_binary(text) and is_map(style) ->
            # Core 2.2.2 uses nowrap for MathML text, while HTML phrasing
            # descendants retain their own CSS whitespace semantics. Core 4.2
            # applies math-auto to original single-character text nodes.
            style = if math_text?, do: Map.put(style, :white_space, :nowrap), else: style
            style = Map.put(style, :_math_auto_scalar, length(String.codepoints(text)) == 1)
            {:ok, [%{text: text, style: style}]}

          %{type: :element, style: %{display: :none}} ->
            {:ok, []}

          %{type: :element, namespace: :mathml} ->
            failure("nested MathML in a token needs the later HTML-child composition batch")

          %{type: :element, children: nested} when is_list(nested) ->
            token_runs(nested)

          _ ->
            failure("math token children require validated styled text or element nodes")
        end

      case result do
        {:ok, children} -> {:cont, {:ok, runs ++ children}}
        error -> {:halt, error}
      end
    end)
  end

  defp token_characters(runs) do
    # Collapse ASCII whitespace across phrasing boundaries and trim token edges.
    # NBSP and other authored Unicode spacing remain part of the token.
    {characters, _space} =
      Enum.reduce(runs, {[], true}, fn run, {characters, previous_space} ->
        Enum.reduce(String.codepoints(run.text), {characters, previous_space}, fn character,
                                                                                  {characters,
                                                                                   previous_space} ->
          space? = character in [" ", "\t", "\n", "\r", "\f"]
          preserve? = Map.get(run.style, :white_space) in [:pre, :pre_wrap, :break_spaces]

          if space? and previous_space and not preserve? do
            {characters, true}
          else
            {[%{text: if(space?, do: " ", else: character), style: run.style} | characters],
             space?}
          end
        end)
      end)

    characters
    |> Enum.drop_while(
      &(&1.text == " " and Map.get(&1.style, :white_space) not in [:pre, :pre_wrap, :break_spaces])
    )
    |> Enum.reverse()
  end

  defp prepare_token_glyphs(runs) do
    characters = token_characters(runs)

    Enum.reduce_while(characters, {:ok, []}, fn character, {:ok, glyphs} ->
      style = character.style

      transform =
        if Map.get(style, :_math_auto_scalar, false) and
             Map.get(style, :text_transform) == :math_auto, do: :math_auto, else: :none

      case prepare_glyph(character.text, style.font_face, style.font_size,
             text_transform: transform,
             script_feature_level: min(max(Map.get(style, :math_depth, 0), 0), 2)
           ) do
        {:ok, glyph} -> {:cont, {:ok, [%{kind: :glyph, glyph: glyph, style: style} | glyphs]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, glyphs} ->
        glyphs = Enum.reverse(glyphs)

        with {:ok, prepared} <- prepare_token_kerning(glyphs),
             do: {:ok, prepared, Enum.map_join(glyphs, & &1.glyph.text)}

      error ->
        error
    end
  end

  defp prepare_token_kerning(glyphs) do
    glyphs
    |> Enum.chunk_by(& &1.style)
    |> Enum.reduce_while({:ok, []}, fn run, {:ok, prepared} ->
      [first | _] = run

      # Shape displayed Unicode after math-auto, preserving original Unicode
      # separately in the token's ActualText. Original f/i nodes must not turn
      # selected mathematical italic glyphs back into a regular Latin ligature.
      text = Enum.map_join(run, fn item -> <<item.glyph.source.displayed::utf8>> end)

      shaped =
        Font.shape_ligatures(
          text,
          first.glyph.font_face,
          Map.get(first.style, :letter_spacing, 0.0)
        )

      shaped_run =
        if shaped == text do
          {:ok, run}
        else
          Enum.reduce_while(String.codepoints(shaped), {:ok, []}, fn text, {:ok, glyphs} ->
            case prepare_glyph(text, first.glyph.font_face, first.glyph.font_size,
                   script_feature_level: min(max(Map.get(first.style, :math_depth, 0), 0), 2)
                 ) do
              {:ok, glyph} ->
                {:cont, {:ok, [%{kind: :glyph, glyph: glyph, style: first.style} | glyphs]}}

              error ->
                {:halt, error}
            end
          end)
          |> case do
            {:ok, glyphs} -> {:ok, Enum.reverse(glyphs)}
            error -> error
          end
        end

      with {:ok, run} <- shaped_run do
        pairs =
          run
          |> Enum.map(& &1.glyph.glyph_id)
          |> Enum.chunk_every(2, 1, :discard)
          |> Enum.map(&List.to_tuple/1)
          |> Enum.uniq()

        script =
          if Enum.any?(run, &(&1.glyph.source.displayed in 0x370..0x3FF)),
            do: "grek",
            else: "latn"

        result =
          if pairs == [],
            do: {:ok, %{}},
            else: FontValidator.prepare_math_kerning(first.glyph.font_face, pairs, script)

        case result do
          {:ok, adjustments} ->
            {run, _previous} =
              Enum.map_reduce(run, nil, fn glyph, previous ->
                adjustment = Map.get(adjustments, {previous, glyph.glyph.glyph_id}, 0)

                {Map.put(
                   glyph,
                   :kerning_before,
                   adjustment * glyph.glyph.font_size / glyph.glyph.units_per_em
                 ), glyph.glyph.glyph_id}
              end)

            {:cont, {:ok, prepared ++ run}}

          error ->
            {:halt, error}
        end
      else
        error -> {:halt, error}
      end
    end)
  end

  # MathML Core 2025-06-24, section C.1. Keep the exceptional Unicode mappings.
  @italic_mappings %{
    0x0041 => 0x1D434,
    0x0042 => 0x1D435,
    0x0043 => 0x1D436,
    0x0044 => 0x1D437,
    0x0045 => 0x1D438,
    0x0046 => 0x1D439,
    0x0047 => 0x1D43A,
    0x0048 => 0x1D43B,
    0x0049 => 0x1D43C,
    0x004A => 0x1D43D,
    0x004B => 0x1D43E,
    0x004C => 0x1D43F,
    0x004D => 0x1D440,
    0x004E => 0x1D441,
    0x004F => 0x1D442,
    0x0050 => 0x1D443,
    0x0051 => 0x1D444,
    0x0052 => 0x1D445,
    0x0053 => 0x1D446,
    0x0054 => 0x1D447,
    0x0055 => 0x1D448,
    0x0056 => 0x1D449,
    0x0057 => 0x1D44A,
    0x0058 => 0x1D44B,
    0x0059 => 0x1D44C,
    0x005A => 0x1D44D,
    0x0061 => 0x1D44E,
    0x0062 => 0x1D44F,
    0x0063 => 0x1D450,
    0x0064 => 0x1D451,
    0x0065 => 0x1D452,
    0x0066 => 0x1D453,
    0x0067 => 0x1D454,
    0x0068 => 0x0210E,
    0x0069 => 0x1D456,
    0x006A => 0x1D457,
    0x006B => 0x1D458,
    0x006C => 0x1D459,
    0x006D => 0x1D45A,
    0x006E => 0x1D45B,
    0x006F => 0x1D45C,
    0x0070 => 0x1D45D,
    0x0071 => 0x1D45E,
    0x0072 => 0x1D45F,
    0x0073 => 0x1D460,
    0x0074 => 0x1D461,
    0x0075 => 0x1D462,
    0x0076 => 0x1D463,
    0x0077 => 0x1D464,
    0x0078 => 0x1D465,
    0x0079 => 0x1D466,
    0x007A => 0x1D467,
    0x0131 => 0x1D6A4,
    0x0237 => 0x1D6A5,
    0x0391 => 0x1D6E2,
    0x0392 => 0x1D6E3,
    0x0393 => 0x1D6E4,
    0x0394 => 0x1D6E5,
    0x0395 => 0x1D6E6,
    0x0396 => 0x1D6E7,
    0x0397 => 0x1D6E8,
    0x0398 => 0x1D6E9,
    0x0399 => 0x1D6EA,
    0x039A => 0x1D6EB,
    0x039B => 0x1D6EC,
    0x039C => 0x1D6ED,
    0x039D => 0x1D6EE,
    0x039E => 0x1D6EF,
    0x039F => 0x1D6F0,
    0x03A0 => 0x1D6F1,
    0x03A1 => 0x1D6F2,
    0x03F4 => 0x1D6F3,
    0x03A3 => 0x1D6F4,
    0x03A4 => 0x1D6F5,
    0x03A5 => 0x1D6F6,
    0x03A6 => 0x1D6F7,
    0x03A7 => 0x1D6F8,
    0x03A8 => 0x1D6F9,
    0x03A9 => 0x1D6FA,
    0x2207 => 0x1D6FB,
    0x03B1 => 0x1D6FC,
    0x03B2 => 0x1D6FD,
    0x03B3 => 0x1D6FE,
    0x03B4 => 0x1D6FF,
    0x03B5 => 0x1D700,
    0x03B6 => 0x1D701,
    0x03B7 => 0x1D702,
    0x03B8 => 0x1D703,
    0x03B9 => 0x1D704,
    0x03BA => 0x1D705,
    0x03BB => 0x1D706,
    0x03BC => 0x1D707,
    0x03BD => 0x1D708,
    0x03BE => 0x1D709,
    0x03BF => 0x1D70A,
    0x03C0 => 0x1D70B,
    0x03C1 => 0x1D70C,
    0x03C2 => 0x1D70D,
    0x03C3 => 0x1D70E,
    0x03C4 => 0x1D70F,
    0x03C5 => 0x1D710,
    0x03C6 => 0x1D711,
    0x03C7 => 0x1D712,
    0x03C8 => 0x1D713,
    0x03C9 => 0x1D714,
    0x2202 => 0x1D715,
    0x03F5 => 0x1D716,
    0x03D1 => 0x1D717,
    0x03F0 => 0x1D718,
    0x03D5 => 0x1D719,
    0x03F1 => 0x1D71A,
    0x03D6 => 0x1D71B
  }

  @doc false
  @spec prepare_glyph(term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_glyph(text, font, size, opts) do
    codepoint =
      case text do
        <<codepoint::utf8>> -> codepoint
        _ -> nil
      end

    valid? =
      is_integer(codepoint) and
        is_number(size) and size >= 0 and size <= Limits.get(:max_css_numeric_magnitude) and
        Keyword.keyword?(opts) and
        Enum.all?(Keyword.keys(opts), &(&1 in [:text_transform, :script_feature_level])) and
        Keyword.get(opts, :text_transform, :none) in [:none, :math_auto] and
        Keyword.get(opts, :script_feature_level, 0) in [0, 1, 2]

    case {valid?, font} do
      {true, %{type: :embedded, units_per_em: units, cmap: cmap, widths: widths}}
      when is_integer(units) and units > 0 and is_map(cmap) and is_list(widths) ->
        FontValidator.reserve(:max_font_math_work, 1)
        HtmlValidator.reserve_render_resource(:max_layout_boxes, 1, :layout)

        displayed =
          if Keyword.get(opts, :text_transform, :none) == :math_auto,
            do: Map.get(@italic_mappings, codepoint, codepoint),
            else: codepoint

        with {:ok, metrics} <- Font.math_metrics(font),
             {:ok, bounds} <- Font.glyph_bounds(font) do
          glyph = Map.get(cmap, displayed, 0)
          level = Keyword.get(opts, :script_feature_level, 0)

          glyph =
            if level == 0 do
              glyph
            else
              metrics.substitutions
              |> Map.get("ssty", [])
              |> Enum.zip(Map.get(metrics.substitution_kinds, "ssty", []))
              |> Enum.reduce(glyph, fn {lookup, kind}, id ->
                case Map.get(lookup, id) do
                  nil ->
                    id

                  alternatives ->
                    Enum.at(alternatives, if(kind == :single, do: 0, else: level - 1), id)
                end
              end)
            end

          if metrics.units_per_em != units do
            failure("selected font units disagree with its MATH data")
          else
            # Shared source metrics are decoded once for repeated math glyphs.
            source = %{
              text: text,
              displayed: displayed,
              font: font,
              size: size,
              metrics: metrics,
              bounds: bounds,
              advances: Font.glyph_widths(font, tuple_size(bounds) - 1)
            }

            selected_glyph(glyph, source)
          end
        end

      _ ->
        failure(
          "math glyph measurement requires one Unicode scalar, an embedded font, a nonnegative size, and supported transform/script options"
        )
    end
  end

  @doc false
  @spec math_auto_text(String.t()) :: String.t()
  def math_auto_text(text) do
    case text do
      <<codepoint::utf8>> -> <<Map.get(@italic_mappings, codepoint, codepoint)::utf8>>
      _ -> text
    end
  end

  @doc false
  @spec prepare_stretch(term(), term(), term(), term(), term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_stretch(text, font, size, axis, target, opts) do
    if axis in [:horizontal, :vertical] and is_number(target) and target >= 0 and
         target <= Limits.get(:max_css_numeric_magnitude) do
      with {:ok, base} <- prepare_glyph(text, font, size, opts) do
        source = base.source
        construction = Map.get(source.metrics.variants[axis], base.glyph_id)

        case construction do
          nil ->
            Diagnostics.error(
              :layout,
              :unsupported_glyph,
              "selected math font has no #{axis} stretch construction for #{inspect(text)}",
              operation: :measure_math,
              module: __MODULE__,
              source: text
            )

          construction ->
            FontValidator.reserve(:max_font_math_work, length(construction.variants))
            scale = size / base.units_per_em
            {_xmin, ymin, _xmax, ymax} = base.bounds

            natural_size =
              if axis == :horizontal, do: base.advance * scale, else: (ymax - ymin) * scale

            if natural_size >= target do
              {:ok, %{kind: :glyph, glyph: base}}
            else
              selected = Enum.find(construction.variants, &(&1.advance * scale >= target))

              case selected do
                %{glyph: id} ->
                  with {:ok, glyph} <- selected_glyph(id, source),
                       do: {:ok, %{kind: :glyph, glyph: glyph}}

                nil ->
                  case prepare_assembly(
                         construction.assembly,
                         source.metrics.variants.min_connector_overlap,
                         scale,
                         target
                       ) do
                    {:ok, assembly} ->
                      with {:ok, metric_parts} <- prepare_parts(assembly.metric_parts, source),
                           {:ok, italic} <- prepare_value(assembly.italic, size) do
                        metrics = Map.new(metric_parts, &{&1.glyph, &1.metric})

                        parts =
                          Enum.map(
                            assembly.parts,
                            &Map.put(&1, :metric, Map.fetch!(metrics, &1.glyph))
                          )

                        {:ok,
                         Map.merge(assembly, %{
                           kind: :assembly,
                           parts: parts,
                           metric_parts: metric_parts,
                           axis: axis,
                           source: source,
                           italic: italic
                         })}
                      end

                    :unusable ->
                      id =
                        case List.last(construction.variants) do
                          nil -> base.glyph_id
                          variant -> variant.glyph
                        end

                      with {:ok, glyph} <- selected_glyph(id, source),
                           do: {:ok, %{kind: :glyph, glyph: glyph}}
                  end
              end
            end
        end
      end
    else
      failure(
        "math stretching requires a horizontal or vertical axis and a bounded nonnegative target"
      )
    end
  end

  defp selected_glyph(id, source) do
    advance =
      if is_integer(id) and id > 0 and id < tuple_size(source.bounds) and
           id < tuple_size(source.advances),
         do: elem(source.advances, id)

    if is_number(advance) and advance >= 0 do
      with {:ok, italic} <-
             prepare_value(
               Map.get(source.metrics.glyph_info.italics_corrections, id),
               source.size
             ),
           {:ok, accent} <-
             prepare_value(
               Map.get(source.metrics.glyph_info.top_accent_attachments, id),
               source.size
             ) do
        {:ok,
         %{
           text: source.text,
           glyph_id: id,
           font_face: source.font,
           font_size: source.size,
           units_per_em: source.metrics.units_per_em,
           advance: advance,
           bounds: elem(source.bounds, id),
           italic: italic,
           accent: accent,
           source: source
         }}
      end
    else
      Diagnostics.error(
        :layout,
        :unsupported_glyph,
        "selected math font has no usable glyph for U+#{Integer.to_string(source.displayed, 16)}",
        operation: :measure_math,
        module: __MODULE__,
        source: source.text
      )
    end
  end

  defp prepare_parts(parts, source) do
    Enum.reduce_while(parts, {:ok, []}, fn part, {:ok, prepared} ->
      case selected_glyph(part.glyph, source) do
        {:ok, glyph} -> {:cont, {:ok, [Map.put(part, :metric, glyph) | prepared]}}
        {:error, _} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, prepared} -> {:ok, Enum.reverse(prepared)}
      error -> error
    end
  end

  defp prepare_assembly(assembly, minimum, scale, target) do
    case assembly do
      %{parts: parts, italics_correction: italic} when scale > 0 ->
        FontValidator.reserve(:max_font_math_work, length(parts))
        {extenders, fixed} = Enum.split_with(parts, & &1.extender?)
        extender_sum = Enum.sum(Enum.map(extenders, & &1.full_advance)) * scale
        fixed_sum = Enum.sum(Enum.map(fixed, & &1.full_advance)) * scale
        minimum = minimum * scale
        growth = extender_sum - minimum * length(extenders)
        last_index = length(parts) - 1

        # OpenType permits zero connectors at the two unused outer ends. The
        # pinned MathML Core text reverses those ends; Chromium 152 uses actual joins.
        joins =
          parts
          |> Enum.with_index()
          |> Enum.flat_map(fn {part, index} ->
            start = if part.extender? or index != 0, do: [part.start_connector * scale], else: []

            finish =
              if part.extender? or index != last_index, do: [part.end_connector * scale], else: []

            start ++ finish
          end)

        maximum = Enum.min(joins, fn -> 0.0 end)

        if extenders != [] and growth > 0 and maximum >= minimum do
          needed = max(target - fixed_sum + minimum * (length(fixed) - 1), 0)
          work_limit = Limits.get(:max_font_math_work)

          # Bound the ratio before dividing; tiny positive sizes can overflow a
          # float even though their target is bounded. Each repeat costs work.
          if needed > growth * work_limit do
            FontValidator.reserve(:max_font_math_work, work_limit + 1)
          end

          repetitions =
            max(
              ceil(needed / growth),
              if(fixed == [], do: 1, else: 0)
            )

          count = length(fixed) + repetitions * length(extenders)
          FontValidator.reserve(:max_font_math_work, count)
          HtmlValidator.reserve_render_resource(:max_layout_boxes, count, :layout)
          total = fixed_sum + repetitions * extender_sum

          overlap =
            if count <= 1,
              do: 0.0,
              else: max(minimum, min(maximum, (total - target) / (count - 1)))

          expanded =
            Enum.flat_map(parts, fn part ->
              List.duplicate(part, if(part.extender?, do: repetitions, else: 1))
            end)

          {:ok,
           %{
             parts: expanded,
             metric_parts: parts,
             stretch_size: total - overlap * max(count - 1, 0),
             overlap: overlap,
             italic: italic
           }}
        else
          :unusable
        end

      _ ->
        :unusable
    end
  end

  @doc false
  @spec prepare_value(term(), number()) ::
          {:ok, {number(), number()} | nil} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_value(record, size) do
    case record do
      nil ->
        {:ok, nil}

      %{value: value, device: device} when is_integer(value) ->
        case device do
          nil ->
            {:ok, {value, 0.0}}

          %{start_size: first, end_size: last, deltas: deltas}
          when is_integer(first) and is_integer(last) and is_list(deltas) ->
            # Device sizes and deltas are CSS pixels, independent of design units.
            pixels = round(size / 0.75)

            delta =
              if pixels >= first and pixels <= last, do: Enum.at(deltas, pixels - first), else: 0

            if first <= last and length(deltas) == last - first + 1 and
                 Enum.all?(deltas, &is_integer/1) do
              {:ok, {value, delta * 0.75}}
            else
              failure("invalid MATH device adjustment")
            end

          _ ->
            failure("MATH variation adjustments need supported variation coordinates")
        end

      _ ->
        failure("invalid MATH metric record")
    end
  end

  @doc false
  @spec validate_row(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_row(children) do
    valid? =
      is_list(children) and
        Enum.all?(children, fn child ->
          FontValidator.reserve(:max_font_math_work, 1)

          case child do
            %{
              width: width,
              ascent: ascent,
              descent: descent,
              italic_correction: italic,
              glyphs: glyphs
            } ->
              Enum.all?([width, ascent, descent, italic], &is_number/1) and
                is_list(glyphs) and
                Enum.all?(glyphs, fn glyph ->
                  FontValidator.reserve(:max_font_math_work, 1)

                  case glyph do
                    %{
                      x: x,
                      y: y,
                      text: text,
                      glyph_id: id,
                      font_face: %{type: :embedded},
                      font_size: size,
                      width: width,
                      height: height
                    } ->
                      is_number(x) and is_number(y) and is_binary(text) and String.valid?(text) and
                        length(String.to_charlist(text)) == 1 and is_integer(id) and
                        id in 1..65_535 and
                        is_number(size) and size >= 0 and is_number(width) and width >= 0 and
                        is_number(height) and height >= 0

                    _ ->
                      false
                  end
                end)

            _ ->
              false
          end
        end)

    if valid?,
      do: :ok,
      else: failure("math rows require measured child geometry and selected glyphs")
  end

  defp failure(message) do
    Diagnostics.error(:layout, :invalid_document, message,
      operation: :measure_math,
      module: __MODULE__
    )
  end
end
