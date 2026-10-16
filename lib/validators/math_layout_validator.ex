defmodule NativeElixirPdfUtilities.Validators.MathLayoutValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, Layout}
  alias NativeElixirPdfUtilities.Validators.CssValueValidator
  alias NativeElixirPdfUtilities.Validators.WriterValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.HtmlLinkValidator
  alias NativeElixirPdfUtilities.Validators.PrintMediaValidator
  alias NativeElixirPdfUtilities.Validators.MathOperatorValidator
  alias NativeElixirPdfUtilities.Validators.MathTableValidator
  alias NativeElixirPdfUtilities.Validators.MathFractionValidator
  alias NativeElixirPdfUtilities.Validators.MathRadicalValidator

  @token_elements ~w(mi mn mtext ms)
  @script_elements ~w(msub msup msubsup munder mover munderover mmultiscripts)
  @grouping_elements ~w(math mrow mphantom semantics maction mprescripts mstyle merror annotation annotation-xml)
  @core_elements ~w(annotation annotation-xml maction math merror mfrac mi mn mmultiscripts mo mover mpadded mphantom mprescripts mroot mrow ms mspace msqrt mstyle msub msubsup msup mtable mtd mtext mtr munder munderover semantics)
  @rendered_elements @grouping_elements ++
                       ~w(mi mn mo mtext ms annotation annotation-xml mspace mfrac msqrt mroot mpadded mtable mtr mtd) ++
                       @script_elements

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
            with {:ok, paint} <-
                   NativeElixirPdfUtilities.Validators.MathPaintValidator.prepare_geometry(
                     measured
                   ) do
              {paint_width, paint_height} =
                case paint do
                  nil ->
                    {measured.width, measured.ascent + measured.descent}

                  {left, bottom, right, top} ->
                    {max(measured.width, right) - min(0.0, left),
                     max(measured.ascent, top) - min(-measured.descent, bottom)}
                end

              if paint_width <= width and paint_height <= height do
                :ok
              else
                Diagnostics.error(
                  :layout,
                  :invalid_layout,
                  "atomic MathML equation occupies #{paint_width} × #{paint_height}pt including visible selected-font paint and exceeds the printable #{width} × #{height}pt area; increase the page size or reduce the equation font size",
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

          is_boolean(Map.get(measured, :css_math_owner, false)) and
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
    cond do
      not (is_binary(node.tag) and String.valid?(node.tag) and
               Regex.match?(~r/\A[a-zA-Z][^ \t\n\f\r\x00\/>]*\z/u, node.tag)) ->
        failure(
          "MathML tag names require the nonempty ASCII-letter-led names produced by HTML tokenization"
        )

      (node.tag in @rendered_elements or grouping_element?(node.tag)) and
          (node.tag != "mspace" or
             Enum.all?(node.children, fn child ->
               case child do
                 %{type: :text, text: text} -> Regex.match?(~r/^[ \t\n\r\f]*$/u, text)
                 %{type: :processing_instruction} -> true
                 _ -> false
               end
             end)) ->
        :ok

      true ->
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
        if Map.get(node, :namespace) == :mathml,
          do: Map.put(defaults, :_math_element, {:mathml, node.tag}),
          else: defaults

      defaults =
        if node.tag in ["msqrt", "mroot"],
          do: Map.put(defaults, :math_shift, :compact),
          else: defaults

      defaults =
        if node.tag == "mfrac",
          do: Map.put(defaults, :padding, %{top: 0.0, right: 0.75, bottom: 0.0, left: 0.75}),
          else: defaults

      # Core error highlighting is a UA default, below authored CSS.
      defaults =
        if node.tag == "merror" do
          Map.merge(defaults, %{
            background_color: {1.0, 1.0, 224 / 255},
            border_widths: CssValueValidator.edges(0.75),
            border_styles: CssValueValidator.edges(:solid),
            border_colors: CssValueValidator.edges({1.0, 0.0, 0.0})
          })
        else
          defaults
        end

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

  # Core 3.6 and 3.7 select the first element, even if that element is hidden.
  # This is a UA default, so author declarations can show alternate children.
  # Canonical selector identities also cover HTML and SVG integration children.
  @doc false
  @spec child_selection_defaults(map(), [map()]) :: map()
  def child_selection_defaults(node, ancestors) do
    case ancestors do
      [%{namespace: :mathml, tag: tag, children: children} | _]
      when tag in ~w(semantics maction) ->
        first = Enum.find(children, &(Map.get(&1, :type) == :element))

        if first != nil and Map.fetch!(first, :_selector_id) != Map.fetch!(node, :_selector_id),
          do: %{display: :none},
          else: %{}

      _ ->
        %{}
    end
  end

  @doc "Identifies a MathML entry that owns a math context inside HTML or a token."
  @spec independent_math_context?([map()]) :: boolean()
  def independent_math_context?(ancestors) do
    case Enum.find(ancestors, &(Map.get(&1, :namespace) == :mathml)) do
      nil ->
        true

      %{tag: tag} when tag in ~w(mi mn mo ms mtext annotation annotation-xml) ->
        true

      _ ->
        case ancestors do
          [%{namespace: namespace} | _] -> namespace != :mathml
          _ -> false
        end
    end
  end

  @doc false
  @spec schema_child_defaults(map(), map(), [map()]) :: map()
  def schema_child_defaults(node, inherited, ancestors) do
    case ancestors do
      [%{namespace: :mathml, tag: tag, children: children, attributes: attributes} | _]
      when tag in @script_elements ->
        elements = Enum.filter(children, &(Map.get(&1, :type) == :element))
        position = Enum.find_index(elements, &(Map.get(&1, :_selector_id) == node._selector_id))
        marker = Enum.find_index(elements, &(Map.get(&1, :tag) == "mprescripts"))

        multi_lower =
          tag == "mmultiscripts" and position != nil and position > 0 and
            if(marker != nil and position > marker,
              do: rem(position, 2) == 0,
              else: rem(position, 2) == 1 and position != marker
            )

        over = if tag == "munderover", do: 2, else: 1
        accent = tag in ["mover", "munderover"] and accent?(attributes, "accent")
        under = tag in ["munder", "munderover"] and accent?(attributes, "accentunder")

        defaults =
          if position != nil and position > 0 do
            %{
              math_depth: Map.get(inherited, :math_depth, 0) + 1,
              math_style: :compact
            }
          else
            %{}
          end

        defaults =
          if (accent and position == over) or (under and position == 1),
            do: Map.put(defaults, :font_size, inherited.font_size),
            else: defaults

        if multi_lower or (tag in ["msub", "msubsup"] and position == 1) or
             (accent and position == 0),
           do: Map.put(defaults, :math_shift, :compact),
           else: defaults

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

  # Anonymous Math boxes do not receive their owner's CSS box geometry.
  # Final inherited font/text values remain available to the ordinary measurer.
  @doc false
  @spec anonymous_box_style(map()) :: map()
  def anonymous_box_style(style) do
    style
    |> Map.drop([
      :width,
      :height,
      :min_width,
      :max_width,
      :min_height,
      :max_height,
      :margin,
      :padding,
      :border_widths,
      :background_color,
      :background_image,
      :position,
      :offsets,
      :overflow,
      :box_sizing
    ])
    |> Map.put(:display, :block)
  end

  @doc "Returns the atomic adapter style after the validated source owner has measured its complete box."
  @spec owner_atom_style(map()) :: map()
  def owner_atom_style(%{tag: tag, style: style}) do
    if grouping_element?(tag) or tag in ~w(msqrt mroot) or
         (tag in ["mo" | @token_elements] and Map.get(style, :_math_formatting, true)) or
         css_schema?(tag, style),
       do: anonymous_box_style(style),
       else: style
  end

  @doc "Prepares the real Math owner around its measured content for shared CSS paint and clipping."
  @spec prepare_owner_paint(map(), map()) :: {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare_owner_paint(prepared, measured) do
    case prepared do
      %{source_node: %{style: style} = source, source_context: context}
      when is_map(style) and is_map(context) ->
        with :ok <- validate_measured_geometry(measured) do
          style = Map.put(style, :_math_flow, true)

          atom = %{
            type: :element,
            tag: "span",
            children: [],
            style:
              style
              |> anonymous_box_style()
              |> Map.delete(:html_target)
              |> Map.put(:display, :inline_block)
              |> Map.put(:_math_box, measured)
          }

          context = Map.put(context, :native_formatting_context, :math)

          with {:ok, native} <-
                 prepare_native_token([atom], style, [], context, false, :inline, %{
                   source
                   | style: style
                 }) do
            {:ok, Map.put(native, :actual_text, measured.actual_text)}
          end
        end

      _ ->
        failure("Math owner paint requires a prepared source node and its measurement context")
    end
  end

  @doc false
  @spec style_hints(map()) :: [{String.t(), String.t()}]
  def style_hints(node) do
    if Map.get(node, :namespace) == :mathml do
      Enum.flat_map(node.attributes, fn {name, value} ->
        case name do
          "width" when node.tag == "mpadded" ->
            case parse_property("width", value, %{_math_element: {:mathml, "mpadded"}}) do
              {:ok, %{width: {:math_padded_length, _}}} -> [{"width", value}]
              _ -> []
            end

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
      {"width", value} ->
        if Map.get(style, :_math_element) == {:mathml, "mpadded"} do
          case HtmlValidator.parse_css_size(value, :width) do
            {:ok, :auto} ->
              {:ok, %{width: nil, _math_width_set: true}}

            {:ok, size} when is_number(size) or is_tuple(size) ->
              if is_number(size) or match?({:rem, _}, size),
                do: {:ok, %{width: {:math_padded_length, value}, _math_width_set: true}},
                else: {:ok, %{width: size, _math_width_set: true}}

            _ ->
              valid? =
                match?({:ok, _}, HtmlValidator.parse_css_length(value, :nonnegative, 1)) or
                  case HtmlValidator.parse_css_number(value) do
                    {number, "ex"} -> number >= 0
                    _ -> false
                  end

              if valid?,
                do: {:ok, %{width: {:math_padded_length, value}, _math_width_set: true}},
                else: :error
          end
        else
          :error
        end

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

      {"display", value} ->
        with {:ok, display} <- parse_display(value),
             true <- Map.has_key?(style, :_math_context) do
          {:ok, %{display: display, _math_display: display, _math_formatting: true}}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  @doc "Parses the supported Math display keywords without creating an element or font context."
  @spec parse_display(term()) :: {:ok, :inline | :block} | :error
  def parse_display(value) do
    case is_binary(value) and String.valid?(value) do
      true ->
        case value |> String.trim() |> String.downcase() do
          "inline math" -> {:ok, :inline}
          keyword when keyword in ["math", "block math"] -> {:ok, :block}
          _ -> :error
        end

      false ->
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

  @doc "Resolves the inherited selected-font MATH size while retaining its cascade context."
  @spec compute_font_size(map()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def compute_font_size(style) do
    case {Map.get(style, :_math_context), style} do
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
  end

  @doc false
  @spec compute_style(map()) :: {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def compute_style(style) do
    computed =
      with {:ok, style} <- compute_font_size(style), do: {:ok, Map.delete(style, :_math_context)}

    # CSS Display3's MathML appendix computes contents to none for every
    # Math namespace element. HTML integration children have their own defaults.
    with {:ok, style} <- computed,
         style =
           if(
             match?({:mathml, _}, Map.get(style, :_math_element)) and
               Map.get(style, :display) == :contents,
             do: Map.put(style, :display, :none),
             else: style
           ),
         {:ok, style} <-
           (case Map.get(style, :width) do
              {:math_padded_length, value} ->
                with {:ok, width} <- resolve_padded_length(value, style), true <- width >= 0 do
                  {:ok, Map.put(style, :width, width)}
                else
                  {:error, _} = error ->
                    error

                  _ ->
                    failure(
                      "mpadded width requires an available selected-font context and bounded nonnegative length",
                      "mpadded"
                    )
                end

              _ ->
                {:ok, style}
            end) do
      MathTableValidator.resolve_padding(style)
    end
  end

  # The inner row has the inherited font/text state, not its owner's CSS box.
  # Core's no-attribute mrow equivalence and box diagram use inner descent;
  # the dated requested-depth paragraph instead repeats "line-ascent".
  defp prepare_padded(row, attributes) do
    style = row.style

    parameters =
      Enum.reduce_while(~w(height depth lspace voffset), {:ok, %{}}, fn name, {:ok, values} ->
        result =
          case Map.get(attributes, name) do
            nil -> {:ok, nil}
            value when is_binary(value) -> resolve_padded_length(value, style)
            _ -> failure("mpadded #{name} requires an authored string length", "mpadded")
          end

        case result do
          {:ok, value} ->
            value = if is_number(value) and name != "voffset", do: max(value, 0), else: value
            {:cont, {:ok, Map.put(values, String.to_existing_atom(name), value)}}

          :error ->
            {:cont, {:ok, Map.put(values, String.to_existing_atom(name), nil)}}

          {:error, _} = error ->
            {:halt, error}
        end
      end)

    width =
      if Map.get(style, :_math_width_set, false) or Map.get(style, :width) != nil do
        {:ok, Map.get(style, :width)}
      else
        case Map.get(attributes, "width") do
          nil ->
            {:ok, nil}

          value when is_binary(value) ->
            case resolve_padded_length(value, style) do
              {:ok, number} when number >= 0 -> {:ok, number}
              {:error, _} = error -> error
              _ -> {:ok, nil}
            end

          _ ->
            failure("mpadded width requires an authored string length", "mpadded")
        end
      end

    with {:ok, parameters} <- parameters,
         {:ok, width} <- width do
      {:ok,
       %{
         kind: :padded,
         inner: %{
           row
           | style: anonymous_box_style(style)
         },
         style: style,
         core_operator: row.core_operator,
         parameters: Map.put(parameters, :width, width)
       }}
    end
  end

  defp resolve_padded_length(value, style) do
    value = value |> HtmlValidator.trim_css_whitespace() |> String.downcase(:ascii)
    size = Map.get(style, :font_size)
    root_size = Map.get(style, :_root_font_size, size)
    limit = Limits.get(:max_css_numeric_magnitude)

    case HtmlValidator.parse_css_number(value) do
      :error ->
        :error

      {_, "%"} ->
        :error

      {_, unit} ->
        if is_number(size) and size >= 0 and size <= limit and
             is_number(root_size) and root_size >= 0 and root_size <= limit and
             (unit != "ex" or is_map(Map.get(style, :font_face))) do
          with {:ok, length} <- math_length(value, style) do
            length =
              case length do
                {:rem, ratio} -> ratio * root_size
                number -> number
              end

            if abs(length) <= limit,
              do: {:ok, length},
              else: failure("mpadded length exceeds supported point bounds", "mpadded")
          end
        else
          failure(
            "mpadded length requires an available selected font size, root size and ex metric context",
            "mpadded"
          )
        end
    end
  end

  @doc false
  @spec prepare_padded_placement(term(), term()) ::
          {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare_padded_placement(prepared, inner) do
    result =
      NativeElixirPdfUtilities.HtmlToPdf.MathLayout.with_context(fn ->
        case prepared do
          %{
            kind: :padded,
            parameters: %{
              width: width,
              height: height,
              depth: depth,
              lspace: left,
              voffset: shift
            }
          } ->
            limit = Limits.get(:max_css_numeric_magnitude)

            valid? =
              Enum.all?(
                [width, height, depth, left],
                &(is_nil(&1) or (is_number(&1) and &1 >= 0 and &1 <= limit))
              ) and
                (is_nil(shift) or (is_number(shift) and abs(shift) <= limit))

            with true <- valid?,
                 :ok <- validate_measured_geometry(inner) do
              values = %{
                width: width || inner.width,
                min_content_width: width || inner.min_content_width,
                max_content_width: width || inner.max_content_width,
                ascent: height || inner.ascent,
                descent: depth || inner.descent,
                x: left || 0,
                y: shift || 0
              }

              if Enum.all?(Map.values(values), &(is_number(&1) and abs(&1) <= limit)) and
                   abs(values.ascent + values.descent) <= limit and
                   abs(inner.ink_ascent + values.y) <= limit and
                   abs(inner.ink_descent - values.y) <= limit and
                   Enum.all?(
                     inner.drawing_items,
                     &(abs(&1.x + values.x) <= limit and abs(&1.y + values.y) <= limit)
                   ) do
                {:ok, values}
              else
                failure("mpadded placement exceeds supported point geometry bounds", "mpadded")
              end
            else
              {:error, _} = error -> error
              _ -> failure("mpadded placement requires bounded prepared lengths", "mpadded")
            end

          _ ->
            failure(
              "mpadded placement requires validator-prepared parameters and measured inner geometry",
              "mpadded"
            )
        end
      end)

    case result do
      {:error, {reason, diagnostic}} ->
        {:error, {reason, Diagnostics.with_context(diagnostic, source: "mpadded")}}

      result ->
        result
    end
  end

  @doc "Identifies source Math owner dimensions that require the actual enclosing block width."
  @spec prepare_block_flow_dependency(term()) ::
          {:ok, boolean()} | {:error, Diagnostics.detailed_error()}
  def prepare_block_flow_dependency(node) do
    NativeElixirPdfUtilities.HtmlToPdf.MathLayout.with_context(fn ->
      inline_size_dependency(node, 0)
    end)
  end

  defp inline_size_dependency(node, depth) do
    HtmlValidator.reserve_render_resource(:max_css_work, 1, :layout)

    if depth > Limits.get(:max_html_depth) do
      Diagnostics.error(
        :layout,
        :resource_limit_exceeded,
        "Math block width dependency exceeds max_html_depth",
        operation: :measure_math,
        module: __MODULE__,
        source: "math"
      )
    else
      case node do
        %{type: :element, namespace: :mathml, tag: "mtable", style: style} when is_map(style) ->
          with :ok <- MathTableValidator.validate_style(style, "mtable") do
            # Cell contents have their own allocated containing block. A nested
            # table percentage must not use the outer Math block's width.
            {:ok,
             Enum.any?(
               [:width, :min_width, :max_width],
               &percentage_size?(Map.get(style, &1))
             )}
          end

        %{type: :element, namespace: :mathml, tag: tag, style: style, children: children}
        when is_map(style) ->
          with :ok <-
                 Enum.reduce_while([:width, :min_width, :max_width], :ok, fn property, :ok ->
                   # mpadded's admitted semantic width uses the Math content's
                   # natural size, rather than a CSS containing-block basis.
                   result =
                     case Map.get(style, property) do
                       {:math_padded_length, _} when property == :width and tag == "mpadded" ->
                         :ok

                       value ->
                         HtmlValidator.validate_prepared_css_size(value, property)
                     end

                   case result do
                     :ok -> {:cont, :ok}
                     error -> {:halt, error}
                   end
                 end) do
            dependent =
              Enum.any?([:width, :min_width, :max_width], &percentage_size?(Map.get(style, &1)))

            inline_children_dependency(children, depth + 1, dependent)
          end

        %{type: :element, children: children} ->
          inline_children_dependency(children, depth + 1, false)

        %{type: :text} ->
          {:ok, false}

        _ ->
          failure("Math block width dependency requires actual styled source nodes", "math")
      end
    end
  end

  defp inline_children_dependency(children, depth, dependent) do
    case children do
      [] ->
        {:ok, dependent}

      [node | rest] ->
        with {:ok, child_dependent} <- inline_size_dependency(node, depth) do
          inline_children_dependency(rest, depth, dependent or child_dependent)
        end

      _ ->
        failure("Math block width dependency requires proper source child lists", "math")
    end
  end

  # This consumes the size grammar already admitted by the shared validator.
  defp percentage_size?(value) do
    case value do
      {:percent, _} -> true
      {:min, values} -> Enum.any?(values, &percentage_size?/1)
      {:css_size_math, {_source, percentage?}} -> percentage?
      _ -> false
    end
  end

  @doc "Validates the definite inline-size basis supplied by actual Math block placement."
  @spec validate_available_inline_size(term()) :: :ok | {:error, Diagnostics.detailed_error()}
  def validate_available_inline_size(size) do
    if size == nil or is_number(size) do
      HtmlValidator.validate_prepared_css_size(size, :width)
    else
      failure("Math containing inline size requires a bounded nonnegative number or nil")
    end
  end

  @doc "Prepares the retained actual styled Math source for allocated block-flow remeasurement."
  @spec prepare_block_flow(term(), term()) ::
          {:ok, map()} | {:error, Diagnostics.detailed_error()}
  def prepare_block_flow(children, width) do
    case children do
      [
        %{
          type: :element,
          namespace: :mathml,
          style: %{_math_box: measured},
          _math_layout_source: %{style: %{_math_display: :block}} = source,
          _math_layout_context: context
        } = atom
      ]
      when is_map(context) ->
        with :ok <- validate_available_inline_size(width),
             :ok <- validate_measured_geometry(measured) do
          if is_number(width) do
            {:ok,
             %{
               atom: atom,
               source: source,
               context: Map.put(context, :available_inline_size, width)
             }}
          else
            failure("Math block placement requires its definite allocated content width")
          end
        end

      _ ->
        failure(
          "Math block flow requires one retained authored Math source and its preparation context"
        )
    end
  end

  @doc false
  @spec prepare_element(term(), map()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_element(node, context) do
    NativeElixirPdfUtilities.HtmlToPdf.MathLayout.with_context(fn ->
      with {:ok, prepared} <- prepare_node(node, context),
           :ok <- validate_available_inline_size(Map.get(context, :available_inline_size)),
           :ok <- validate_available_inline_size(Map.get(context, :available_content_width)) do
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
        annotation_css? =
          tag in ~w(annotation annotation-xml) and
            (display in [:flex, :inline_flex, :grid, :inline_grid] or
               not Map.get(style, :_math_formatting, true))

        with :ok <- validate_element_support(node),
             :ok <- if(annotation_css?, do: :ok, else: validate_box_style(tag, style)) do
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

            annotation_css? and display != :none ->
              prepare_native_element(node, context)

            display not in [:block, :inline, :inline_block, :none] and
                not css_schema?(tag, style) ->
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

            Map.get(style, :background_image) != nil and not css_schema?(tag, style) and
              not grouping_element?(tag) and tag not in ["mo", "msqrt", "mroot" | @token_elements] ->
              Diagnostics.error(
                :layout,
                :unsupported_html,
                "MathML background images require the later box composition batch",
                operation: :render,
                module: __MODULE__,
                source: tag
              )

            css_schema?(tag, style) ->
              prepare_css_schema(node, context)

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
                   {:ok, length_context} <-
                     prepare_operator_length_context(attributes, style, context),
                   {:ok, operator} <-
                     MathOperatorValidator.prepare(
                       runs |> token_characters() |> Enum.map_join(& &1.text),
                       attributes,
                       Map.merge(
                         Map.merge(Map.get(context, :operator_context, %{}), length_context),
                         %{
                           font_size: size,
                           root_font_size: Map.get(style, :_root_font_size, size),
                           x_height: x_height * size,
                           text_only: Enum.all?(children, &(Map.get(&1, :type) == :text))
                         }
                       )
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
                       else: prepare_token_content(node, children, style, runs, context)
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
                   css_math_owner: Map.get(style, :_math_formatting, true),
                   available_inline_size:
                     Map.get(
                       context,
                       :available_content_width,
                       Map.get(context, :available_inline_size)
                     ),
                   source_node: node,
                   source_context: context,
                   style: style,
                   actual_text: content.actual_text,
                   core_operator: operator,
                   source_glyph: source,
                   math_formatting: Map.get(style, :_math_formatting, true)
                 }}
              end

            tag in @token_elements ->
              with {:ok, runs} <- token_runs(children, true),
                   {:ok, token} <- prepare_token_content(node, children, style, runs, context) do
                if Map.get(style, :_math_formatting, true) do
                  {:ok,
                   Map.merge(token, %{
                     css_math_owner: true,
                     available_inline_size:
                       Map.get(
                         context,
                         :available_content_width,
                         Map.get(context, :available_inline_size)
                       ),
                     source_node: node,
                     source_context: context
                   })}
                else
                  {:ok, token}
                end
              end

            true ->
              with {:ok, row} <- prepare_row_children(node, context) do
                cond do
                  tag == "mfrac" and length(row.children) == 2 ->
                    MathFractionValidator.prepare(row, attributes)

                  tag == "msqrt" or (tag == "mroot" and length(row.children) == 2) ->
                    {base, index} =
                      if tag == "msqrt",
                        do:
                          {%{row | style: anonymous_box_style(style), css_math_owner: false}, nil},
                        else: {Enum.at(row.children, 0), Enum.at(row.children, 1)}

                    with {:ok, radical} <- MathRadicalValidator.prepare(base, index, style) do
                      {:ok,
                       Map.merge(
                         radical,
                         Map.take(
                           row,
                           [
                             :css_math_owner,
                             :available_inline_size,
                             :source_node,
                             :source_context
                           ]
                         )
                       )}
                    end

                  tag == "mphantom" ->
                    {:ok, Map.put(row, :hide_paint, true)}

                  tag == "mpadded" ->
                    prepare_padded(row, attributes)

                  tag == "mprescripts" ->
                    {:ok, Map.put(row, :prescript_marker, true)}

                  tag == "mmultiscripts" ->
                    case multiscript_roles(row.children) do
                      {:ok, roles} ->
                        with {:ok, prepared} <- prepare_script_schema(row, "msubsup", attributes),
                             do: {:ok, Map.merge(prepared, %{kind: :multiscripts, roles: roles})}

                      :row ->
                        {:ok, row}
                    end

                  tag in @script_elements and
                      length(row.children) == if(tag in ["msubsup", "munderover"], do: 3, else: 2) ->
                    prepare_script_schema(row, tag, attributes)

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

  defp accent?(attributes, name) do
    String.downcase(Map.get(attributes, name, ""), :ascii) == "true"
  end

  # Child counts and movablelimits choose the schema before any measurement.
  defp prepare_script_schema(row, tag, attributes) do
    style = row.style
    mode = Map.get(style, :math_style, :normal)
    shift = Map.get(style, :math_shift, :normal)
    [base | _] = row.children
    operator = Map.get(base, :core_operator)
    movable? = operator != nil and operator.movablelimits and mode == :compact
    side? = tag in ["msub", "msup", "msubsup"] or movable?
    lower = if tag in ["msub", "msubsup", "munder", "munderover"], do: 1
    upper = if tag in ["msup", "mover"], do: 1, else: if(tag in ["msubsup", "munderover"], do: 2)

    names = script_constant_names(side?)

    if mode in [:normal, :compact] and shift in [:normal, :compact] and
         style.font_size <= Limits.get(:max_css_numeric_magnitude) do
      with {:ok, metrics} <- Font.math_metrics(style.font_face),
           {:ok, values} <- resolve_constants(metrics, names, style.font_size) do
        stretching =
          row.children
          |> Enum.with_index()
          |> Enum.flat_map(fn {child, index} ->
            case Map.get(child, :core_operator) do
              %{stretchy: true, stretch_axis: :horizontal} -> [index]
              _ -> []
            end
          end)

        {:ok,
         %{
           kind: if(side?, do: :scripts, else: :limits),
           children: row.children,
           lower: lower,
           upper: upper,
           style: style,
           core_operator: row.core_operator,
           constants: Map.new(Enum.zip(names, values)),
           largeop: operator != nil and operator.largeop,
           stretchy_base:
             operator != nil and operator.stretchy and operator.stretch_axis == :horizontal,
           accent: accent?(attributes, "accent"),
           accentunder: accent?(attributes, "accentunder"),
           inline_stretch_children: stretching
         }}
      end
    else
      failure("scripts require bounded font size and normal or compact math-style/math-shift")
    end
  end

  defp script_constant_names(side?) do
    if side? do
      [
        :subscript_shift_down,
        :subscript_top_max,
        :subscript_baseline_drop_min,
        :superscript_shift_up,
        :superscript_shift_up_cramped,
        :superscript_bottom_min,
        :superscript_baseline_drop_max,
        :sub_superscript_gap_min,
        :superscript_bottom_max_with_subscript,
        :space_after_script
      ]
    else
      [
        :upper_limit_gap_min,
        :upper_limit_baseline_rise_min,
        :lower_limit_gap_min,
        :lower_limit_baseline_drop_min,
        :stretch_stack_top_shift_up,
        :stretch_stack_bottom_shift_down,
        :stretch_stack_gap_above_min,
        :stretch_stack_gap_below_min,
        :overbar_vertical_gap,
        :overbar_extra_ascender,
        :underbar_vertical_gap,
        :underbar_extra_descender,
        :accent_base_height
      ]
    end
  end

  @doc "Resolves selected MATH value records to points, including device adjustments."
  @spec resolve_constants(map(), [atom()], number()) ::
          {:ok, [number()]} | {:error, Diagnostics.detailed_error()}
  def resolve_constants(metrics, names, size) do
    FontValidator.reserve(:max_font_math_work, length(names))

    Enum.reduce_while(names, {:ok, []}, fn name, {:ok, values} ->
      case prepare_value(Map.fetch!(metrics.constants, name), size) do
        {:ok, {value, pixels}} ->
          {:cont, {:ok, values ++ [value * size / metrics.units_per_em + pixels]}}

        {:error, {reason, diagnostic}} ->
          {:halt,
           {:error,
            {reason, Diagnostics.with_context(diagnostic, source: "selected font MATH #{name}")}}}
      end
    end)
  end

  @doc false
  @spec prepare_limit_stretch(term(), term()) ::
          {:ok, number()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_limit_stretch(prepared, measured) do
    NativeElixirPdfUtilities.HtmlToPdf.MathLayout.with_context(fn ->
      case {prepared, measured} do
        {%{kind: :limits, children: children}, measured} when is_map(measured) ->
          if Enum.all?(
               Map.keys(measured),
               &(is_integer(&1) and &1 >= 0 and &1 < length(children))
             ) do
            with :ok <- validate_script_children(Map.values(measured)) do
              target = Enum.max(Enum.map(Map.values(measured), & &1.width), fn -> 0.0 end)
              with :ok <- validate_operator_constraint(target), do: {:ok, target}
            end
          else
            failure("limit stretching requires measured children keyed by their prepared indices")
          end

        _ ->
          failure(
            "limit stretching requires validator-prepared limit state and measured children"
          )
      end
    end)
  end

  @doc false
  @spec prepare_script_placement(term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_script_placement(prepared, measured) do
    NativeElixirPdfUtilities.HtmlToPdf.MathLayout.with_context(fn ->
      case {prepared, measured} do
        {%{
           kind: kind,
           children: children,
           lower: lower,
           upper: upper,
           constants: constants,
           style: style,
           largeop: largeop,
           stretchy_base: stretchy_base,
           accent: accent,
           accentunder: accentunder
         }, measured}
        when kind in [:scripts, :limits] and is_list(children) and is_list(measured) and
               length(measured) == length(children) and is_map(constants) and is_map(style) and
               is_boolean(largeop) and is_boolean(stretchy_base) and is_boolean(accent) and
               is_boolean(accentunder) ->
          valid_state? =
            {length(children), lower, upper} in [{2, 1, nil}, {2, nil, 1}, {3, 1, 2}] and
              valid_script_constants?(constants, style, kind == :scripts)

          with :ok <-
                 if(valid_state?,
                   do: :ok,
                   else:
                     failure(
                       "script placement requires bounded prepared constants and valid child roles"
                     )
                 ),
               :ok <- validate_script_children(measured) do
            [base | _] = measured
            sub = if lower, do: Enum.at(measured, lower)
            sup = if upper, do: Enum.at(measured, upper)
            constants = prepared.constants
            css_limits? = kind == :limits and Map.has_key?(style, :_math_element)

            large_italic =
              if prepared.largeop and not css_limits?, do: base.italic_correction, else: 0.0

            italic = if prepared.largeop, do: 0.0, else: base.italic_correction

            {lower_shift, upper_shift, extra_under, extra_over} =
              if kind == :scripts do
                {down, up} = side_script_shifts(constants, prepared.style, base, sub, sup)
                {down, up, 0.0, 0.0}
              else
                {down, under_extra} =
                  cond do
                    sub == nil ->
                      {0.0, 0.0}

                    prepared.largeop ->
                      {max(
                         constants.lower_limit_baseline_drop_min,
                         constants.lower_limit_gap_min + sub.ascent
                       ), 0.0}

                    prepared.stretchy_base ->
                      {max(
                         constants.stretch_stack_bottom_shift_down,
                         constants.stretch_stack_gap_above_min + sub.ascent
                       ), 0.0}

                    true ->
                      {if(prepared.accentunder, do: 0.0, else: constants.underbar_vertical_gap),
                       constants.underbar_extra_descender}
                  end

                {up, over_extra} =
                  cond do
                    sup == nil ->
                      {0.0, 0.0}

                    prepared.largeop ->
                      {max(
                         constants.upper_limit_baseline_rise_min,
                         constants.upper_limit_gap_min + sup.descent
                       ), 0.0}

                    prepared.stretchy_base ->
                      {max(
                         constants.stretch_stack_top_shift_up,
                         constants.stretch_stack_gap_below_min + sup.descent
                       ), 0.0}

                    true ->
                      {if(prepared.accent,
                         do: max(constants.accent_base_height - base.ascent, 0.0),
                         else: constants.overbar_vertical_gap
                       ), constants.overbar_extra_ascender}
                  end

                down =
                  if css_limits? and sub != nil and not prepared.largeop and
                       not prepared.stretchy_base, do: down + sub.ascent, else: down

                up =
                  if css_limits? and sup != nil and not prepared.largeop and
                       not prepared.stretchy_base, do: up + sup.descent, else: up

                {down, up, under_extra, over_extra}
              end

            {sub_kern, sup_kern} =
              if kind == :scripts and not Map.has_key?(style, :_math_element) do
                with {:ok, base_glyph} <- prepare_script_kern_glyph(base),
                     {:ok, sub_glyph} <- prepare_script_kern_glyph(sub),
                     {:ok, sup_glyph} <- prepare_script_kern_glyph(sup),
                     {:ok, sub_kern} <-
                       prepare_script_pair_kern(
                         base_glyph,
                         sub_glyph,
                         base,
                         sub,
                         -lower_shift,
                         :sub
                       ),
                     {:ok, sup_kern} <-
                       prepare_script_pair_kern(
                         base_glyph,
                         sup_glyph,
                         base,
                         sup,
                         upper_shift,
                         :sup
                       ) do
                  {sub_kern, sup_kern}
                else
                  {:error, _} = error -> throw({:math_layout_error, error})
                end
              else
                {0.0, 0.0}
              end

            widths =
              Map.new([:width, :min_content_width, :max_content_width], fn field ->
                value = Map.fetch!(base, field)

                if css_limits? do
                  width = Enum.max(Enum.map(measured, &Map.fetch!(&1, field)))
                  {field, {width, -width / 2}}
                else
                  if kind == :scripts do
                    candidates =
                      if(sub,
                        do: [
                          value - large_italic + sub_kern + Map.fetch!(sub, field) +
                            constants.space_after_script
                        ],
                        else: []
                      ) ++
                        if sup,
                          do: [
                            value + italic + sup_kern + Map.fetch!(sup, field) +
                              constants.space_after_script
                          ],
                          else: []

                    {field, {Enum.max(candidates), 0.0}}
                  else
                    accent =
                      if sup,
                        do: Map.get(sup, :top_accent_attachment) || Map.fetch!(sup, field) / 2,
                        else: 0.0

                    left =
                      [-value / 2] ++
                        if(sub, do: [-Map.fetch!(sub, field) / 2 - large_italic / 2], else: []) ++
                        if(sup, do: [-accent + large_italic / 2], else: [])

                    right =
                      [value / 2] ++
                        if(sub, do: [Map.fetch!(sub, field) / 2 - large_italic / 2], else: []) ++
                        if(sup,
                          do: [Map.fetch!(sup, field) - accent + large_italic / 2],
                          else: []
                        )

                    minimum = Enum.min(left)
                    {field, {abs(Enum.max(right) - minimum), minimum}}
                  end
                end
              end)

            {width, minimum} = widths.width

            offsets =
              Enum.with_index(measured)
              |> Enum.map(fn {child, index} ->
                cond do
                  kind == :scripts and index == 0 ->
                    {0.0, 0.0}

                  kind == :scripts and index == lower ->
                    {base.width - large_italic + sub_kern, -lower_shift}

                  kind == :scripts ->
                    {base.width + italic + sup_kern, upper_shift}

                  index == 0 ->
                    {-minimum - base.width / 2, 0.0}

                  index == lower ->
                    {-minimum - child.width / 2 - large_italic / 2,
                     -if(css_limits?, do: base.descent, else: base.ink_descent) - lower_shift}

                  true ->
                    {-minimum - child.width / 2 + large_italic / 2,
                     if(css_limits?, do: base.ascent, else: base.ink_ascent) + upper_shift}
                end
              end)

            # Chromium allocates the limits from the actual translated child
            # fragments. Its baseline includes the base ascent/descent too.
            ascent =
              Enum.max(
                [base.ascent] ++
                  if(sub, do: [sub.ascent - lower_shift], else: []) ++
                  if(sup, do: [sup.ascent + upper_shift + extra_over], else: [])
              )

            descent =
              Enum.max(
                [base.descent] ++
                  if(sub, do: [sub.descent + lower_shift + extra_under], else: []) ++
                  if(sup, do: [sup.descent - upper_shift], else: [])
              )

            ascent =
              if css_limits?,
                do:
                  Enum.max(
                    Enum.zip(measured, offsets)
                    |> Enum.map(fn {child, {_, y}} -> child.ascent + y end)
                  ) + extra_over,
                else: ascent

            descent =
              if css_limits?,
                do:
                  Enum.max(
                    Enum.zip(measured, offsets)
                    |> Enum.map(fn {child, {_, y}} -> child.descent - y end)
                  ) + extra_under,
                else: descent

            placed = Enum.zip(measured, offsets)

            result = %{
              width: width,
              min_content_width: elem(widths.min_content_width, 0),
              max_content_width: elem(widths.max_content_width, 0),
              ascent: ascent,
              descent: descent,
              ink_ascent:
                Enum.max(Enum.map(placed, fn {child, {_, y}} -> child.ink_ascent + y end)),
              ink_descent:
                Enum.max(Enum.map(placed, fn {child, {_, y}} -> child.ink_descent - y end)),
              offsets: offsets
            }

            validate_script_placement(result)
          end

        _ ->
          failure("script placement requires validator-prepared state and every measured child")
      end
    end)
  end

  defp valid_script_constants?(constants, style, side?) do
    Map.get(style, :math_shift, :normal) in [:normal, :compact] and
      Enum.all?(script_constant_names(side?), fn name ->
        value = Map.get(constants, name)
        is_number(value) and abs(value) <= Limits.get(:max_css_numeric_magnitude)
      end)
  end

  # Pairing is a Core structural rule. Invalid pairing is an ordinary mrow.
  defp multiscript_roles(children) do
    case children do
      [base | scripts] ->
        markers =
          children
          |> Enum.with_index()
          |> Enum.filter(fn {child, _} -> Map.get(child, :prescript_marker, false) end)

        if not Map.get(base, :prescript_marker, false) and length(markers) <= 1 do
          marker =
            case markers do
              [{_, index}] -> index
              [] -> nil
            end

          post_count = if marker, do: marker - 1, else: length(scripts)
          pre_count = if marker, do: length(children) - marker - 1, else: 0

          if rem(post_count, 2) == 0 and rem(pre_count, 2) == 0 do
            pairs = fn first, count ->
              if count == 0,
                do: [],
                else: Enum.map(0..(div(count, 2) - 1), &{first + &1 * 2, first + &1 * 2 + 1})
            end

            {:ok,
             %{
               post: pairs.(1, post_count),
               pre: pairs.((marker || 0) + 1, pre_count),
               marker: marker
             }}
          else
            :row
          end
        else
          :row
        end

      [] ->
        :row
    end
  end

  # Public prepared geometry accepts terms, so reserve proper links before any arrays.
  defp validate_script_collection(children) do
    case children do
      [] ->
        :ok

      [child | rest] when is_map(child) ->
        FontValidator.reserve(:max_font_math_work, 1)
        validate_script_collection(rest)

      _ ->
        failure("script placement requires proper lists of prepared and measured child maps")
    end
  end

  @doc false
  @spec prepare_multiscript_placement(term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_multiscript_placement(prepared, measured) do
    NativeElixirPdfUtilities.HtmlToPdf.MathLayout.with_context(fn ->
      case prepared do
        %{
          kind: :multiscripts,
          children: children,
          roles: roles,
          constants: constants,
          style: style,
          largeop: largeop
        }
        when is_map(constants) and is_map(style) and is_boolean(largeop) ->
          with :ok <- validate_script_collection(children),
               :ok <- validate_script_collection(measured),
               :ok <-
                 if(Enum.all?(children, &is_boolean(Map.get(&1, :prescript_marker, false))),
                   do: :ok,
                   else: failure("multiscript markers require validator-produced boolean roles")
                 ),
               {:ok, actual_roles} <-
                 (case multiscript_roles(children) do
                    {:ok, actual} ->
                      {:ok, actual}

                    :row ->
                      failure(
                        "multiscript placement requires a base and complete script pairs with at most one prescript marker"
                      )
                  end),
               :ok <-
                 if(
                   actual_roles == roles and length(children) == length(measured) and
                     valid_script_constants?(constants, style, true),
                   do: :ok,
                   else:
                     failure(
                       "multiscript placement requires matching child roles and bounded selected-font constants"
                     )
                 ),
               :ok <-
                 Enum.reduce_while(measured, :ok, fn child, :ok ->
                   case validate_script_collection(Map.get(child, :glyphs)) do
                     :ok -> {:cont, :ok}
                     {:error, _} = error -> {:halt, error}
                   end
                 end),
               :ok <- validate_script_children(measured) do
            [base | _] = measured
            measured_tuple = List.to_tuple(measured)
            pairs = Enum.map(roles.pre, &{&1, :pre}) ++ Enum.map(roles.post, &{&1, :post})

            shifts =
              Enum.map(pairs, fn {{lower, upper}, _} ->
                side_script_shifts(
                  constants,
                  style,
                  base,
                  elem(measured_tuple, lower),
                  elem(measured_tuple, upper)
                )
              end)

            down = Enum.reduce(shifts, 0.0, fn {value, _}, acc -> max(value, acc) end)
            up = Enum.reduce(shifts, 0.0, fn {_, value}, acc -> max(value, acc) end)

            with {:ok, base_glyph} <- prepare_script_kern_glyph(base),
                 {:ok, kerns} <-
                   Enum.reduce_while(pairs, {:ok, %{}}, fn {{lower, upper}, side}, {:ok, kerns} ->
                     sub = elem(measured_tuple, lower)
                     sup = elem(measured_tuple, upper)

                     if Map.has_key?(style, :_math_element) do
                       {:cont, {:ok, kerns |> Map.put(lower, 0.0) |> Map.put(upper, 0.0)}}
                     else
                       with {:ok, sub_glyph} <- prepare_script_kern_glyph(sub),
                            {:ok, sup_glyph} <- prepare_script_kern_glyph(sup),
                            {:ok, sub_kern} <-
                              prepare_script_pair_kern(
                                base_glyph,
                                sub_glyph,
                                base,
                                sub,
                                -down,
                                if(side == :pre, do: :presub, else: :sub)
                              ),
                            {:ok, sup_kern} <-
                              prepare_script_pair_kern(
                                base_glyph,
                                sup_glyph,
                                base,
                                sup,
                                up,
                                if(side == :pre, do: :presup, else: :sup)
                              ) do
                         {:cont,
                          {:ok, kerns |> Map.put(lower, sub_kern) |> Map.put(upper, sup_kern)}}
                       else
                         {:error, _} = error -> {:halt, error}
                       end
                     end
                   end) do
              large_italic = if largeop, do: base.italic_correction, else: 0.0
              italic = if largeop, do: 0.0, else: base.italic_correction

              pair_width = fn {lower, upper}, side, field ->
                sub = Map.fetch!(elem(measured_tuple, lower), field) + Map.fetch!(kerns, lower)
                sup = Map.fetch!(elem(measured_tuple, upper), field) + Map.fetch!(kerns, upper)
                # Advance beyond the base, preserving the single-pair msubsup contract.
                max(
                  0.0,
                  max(
                    sub - if(side == :post, do: large_italic, else: 0.0),
                    sup + if(side == :post, do: italic, else: 0.0)
                  )
                ) + constants.space_after_script
              end

              widths =
                Map.new([:width, :min_content_width, :max_content_width], fn field ->
                  width =
                    Enum.reduce(pairs, Map.fetch!(base, field), fn {pair, side}, width ->
                      width + pair_width.(pair, side, field)
                    end)

                  {field, width}
                end)

              pre_width =
                Enum.reduce(roles.pre, 0.0, fn pair, x -> x + pair_width.(pair, :pre, :width) end)

              offsets = %{0 => {pre_width, 0.0}}

              offsets =
                if roles.marker,
                  do: Map.put(offsets, roles.marker, {pre_width, 0.0}),
                  else: offsets

              {offsets, _} =
                Enum.reduce(roles.pre, {offsets, 0.0}, fn {lower, upper} = pair, {offsets, x} ->
                  right = x + pair_width.(pair, :pre, :width)

                  {offsets
                   |> Map.put(
                     lower,
                     {right - elem(measured_tuple, lower).width - kerns[lower], -down}
                   )
                   |> Map.put(
                     upper,
                     {right - elem(measured_tuple, upper).width - kerns[upper], up}
                   ), right}
                end)

              {offsets, _} =
                Enum.reduce(roles.post, {offsets, pre_width + base.width}, fn {lower, upper} =
                                                                                pair,
                                                                              {offsets, x} ->
                  {offsets
                   |> Map.put(lower, {x - large_italic + kerns[lower], -down})
                   |> Map.put(upper, {x + italic + kerns[upper], up}),
                   x + pair_width.(pair, :post, :width)}
                end)

              offsets = Enum.map(0..(length(measured) - 1), &Map.fetch!(offsets, &1))

              script_extents =
                Enum.flat_map(pairs, fn {{lower, upper}, _} ->
                  [{elem(measured_tuple, lower), -down}, {elem(measured_tuple, upper), up}]
                end)

              placed = Enum.zip(measured, offsets)

              result =
                Map.merge(widths, %{
                  ascent:
                    Enum.reduce(script_extents, base.ascent, fn {child, y}, a ->
                      max(a, child.ascent + y)
                    end),
                  descent:
                    Enum.reduce(script_extents, base.descent, fn {child, y}, d ->
                      max(d, child.descent - y)
                    end),
                  ink_ascent:
                    Enum.max(Enum.map(placed, fn {child, {_, y}} -> child.ink_ascent + y end)),
                  ink_descent:
                    Enum.max(Enum.map(placed, fn {child, {_, y}} -> child.ink_descent - y end)),
                  offsets: offsets
                })

              validate_script_placement(result)
            end
          end

        _ ->
          failure(
            "multiscript placement requires validator-prepared state and every measured child"
          )
      end
    end)
  end

  defp validate_script_placement(result) do
    if Enum.all?(
         Map.values(Map.delete(result, :offsets)),
         &(is_number(&1) and abs(&1) <= Limits.get(:max_css_numeric_magnitude))
       ) and
         result.width >= 0 and result.min_content_width >= 0 and
         result.max_content_width >= result.min_content_width and
         result.ascent + result.descent >= 0 and
         Enum.all?(result.offsets, fn {x, y} ->
           abs(x) <= Limits.get(:max_css_numeric_magnitude) and
             abs(y) <= Limits.get(:max_css_numeric_magnitude)
         end),
       do: {:ok, result},
       else: failure("script placement exceeds supported point geometry bounds")
  end

  # The six schemas and every tensor pair share the same selected-font clearance rule.
  defp side_script_shifts(constants, style, base, sub, sup) do
    ascent = if Map.has_key?(style, :_math_element), do: :ascent, else: :ink_ascent
    descent = if Map.has_key?(style, :_math_element), do: :descent, else: :ink_descent

    down =
      if sub do
        Enum.max([
          constants.subscript_shift_down,
          Map.fetch!(sub, ascent) - constants.subscript_top_max,
          constants.subscript_baseline_drop_min + Map.fetch!(base, descent)
        ])
      else
        0.0
      end

    up =
      if sup do
        Enum.max([
          constants[
            if(Map.get(style, :math_shift, :normal) == :compact,
              do: :superscript_shift_up_cramped,
              else: :superscript_shift_up
            )
          ],
          constants.superscript_bottom_min + Map.fetch!(sup, descent),
          Map.fetch!(base, ascent) - constants.superscript_baseline_drop_max
        ])
      else
        0.0
      end

    {down, up} =
      if sub != nil and sup != nil do
        gap = down - Map.fetch!(sub, ascent) + up - Map.fetch!(sup, descent)
        deficit = max(constants.sub_superscript_gap_min - gap, 0.0)

        raise =
          min(
            max(
              constants.superscript_bottom_max_with_subscript - (up - Map.fetch!(sup, descent)),
              0.0
            ),
            deficit
          )

        {down + deficit - raise, up + raise}
      else
        {down, up}
      end

    {down, up}
  end

  # OpenType MATH MathKernInfo uses two bounding-box junction heights.
  # This adds its corner spacing to the dated Core side-script equations;
  # Core 2025 section 3.4 itself does not specify a MathKern step.
  defp prepare_script_kern_glyph(child) do
    case child do
      %{width: width, glyphs: [glyph], drawing_items: [%{kind: :glyph}]}
      when width == glyph.width ->
        with {:ok, metrics} <- Font.math_metrics(glyph.font_face) do
          {:ok,
           Map.merge(glyph, %{
             corner_kerns: Map.get(metrics.glyph_info.kerning, glyph.glyph_id, %{}),
             kern_scale: glyph.font_size / metrics.units_per_em
           })}
        end

      _ ->
        {:ok, nil}
    end
  end

  defp prepare_script_pair_kern(base_glyph, script_glyph, base, script, shift, side) do
    if script == nil or (base_glyph == nil and script_glyph == nil) do
      {:ok, 0.0}
    else
      {base_corner, script_corner, junctions} =
        case side do
          :sup ->
            {:top_right, :bottom_left, [shift - script.ink_descent, base.ink_ascent]}

          :sub ->
            {:bottom_right, :top_left, [shift + script.ink_ascent, -base.ink_descent]}

          :presup ->
            {:top_left, :bottom_right, [shift - script.ink_descent, base.ink_ascent]}

          :presub ->
            {:bottom_left, :top_right, [shift + script.ink_ascent, -base.ink_descent]}
        end

      Enum.reduce_while(junctions, {:ok, []}, fn height, {:ok, values} ->
        with {:ok, base_kern} <- prepare_corner_kern(base_glyph, base_corner, height),
             {:ok, script_kern} <-
               prepare_corner_kern(script_glyph, script_corner, height - shift) do
          {:cont, {:ok, [base_kern + script_kern | values]}}
        else
          {:error, _} = error -> {:halt, error}
        end
      end)
      |> case do
        {:ok, values} -> {:ok, Enum.min(values)}
        error -> error
      end
    end
  end

  defp prepare_corner_kern(glyph, corner, height) do
    table = if glyph, do: Map.get(glyph.corner_kerns, corner)

    if is_number(height) and abs(height) <= Limits.get(:max_css_numeric_magnitude) do
      if table do
        FontValidator.reserve(:max_font_math_work, length(table.heights) + 1)

        heights =
          Enum.reduce_while(table.heights, {:ok, []}, fn record, {:ok, values} ->
            case prepare_value(record, glyph.font_size) do
              {:ok, {value, pixels}} ->
                {:cont, {:ok, [value * glyph.kern_scale + pixels | values]}}

              error ->
                {:halt, error}
            end
          end)

        result =
          with {:ok, heights} <- heights,
               heights = Enum.reverse(heights),
               :ok <-
                 if(heights == Enum.sort(heights),
                   do: :ok,
                   else:
                     failure("selected MATH kern device adjustments reorder its height ranges")
                 ),
               height = height - glyph.y,
               index = Enum.find_index(heights, &(height < &1)) || length(heights),
               {:ok, {value, pixels}} <-
                 prepare_value(Enum.at(table.values, index), glyph.font_size) do
            {:ok, value * glyph.kern_scale + pixels}
          end

        case result do
          {:error, {reason, diagnostic}} ->
            {:error,
             {reason,
              Diagnostics.with_context(diagnostic,
                source: "selected font MATH glyph #{glyph.glyph_id} #{corner} kern"
              )}}

          result ->
            result
        end
      else
        {:ok, 0.0}
      end
    else
      failure("script corner kerning requires bounded point junction heights")
    end
  end

  defp validate_script_children(children) do
    with :ok <- validate_row(children) do
      if Enum.all?(children, fn child ->
           Enum.all?(
             [
               :width,
               :ascent,
               :descent,
               :italic_correction,
               :min_content_width,
               :max_content_width,
               :ink_ascent,
               :ink_descent
             ],
             fn field ->
               value = Map.get(child, field)
               is_number(value) and abs(value) <= Limits.get(:max_css_numeric_magnitude)
             end
           ) and
             Enum.all?(child.glyphs, &(&1.font_size <= Limits.get(:max_css_numeric_magnitude))) and
             child.min_content_width >= 0 and child.max_content_width >= child.min_content_width and
             (is_nil(Map.get(child, :top_accent_attachment)) or
                is_number(child.top_accent_attachment))
         end),
         do: :ok,
         else:
           failure(
             "script children require bounded point extents, intrinsic widths, ink and accent attachment"
           )
    end
  end

  @doc false
  @spec prepare_radical_stretch(term(), term()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_radical_stretch(prepared, measured) do
    MathRadicalValidator.prepare_stretch(prepared, measured)
  end

  defp prepare_row_children(node, context) do
    %{tag: tag, children: children, style: style} = node

    available =
      Map.get(context, :available_content_width, Map.get(context, :available_inline_size))

    owner? = grouping_element?(tag) or tag in ~w(msqrt mroot)
    used_width = if owner?, do: Layout.resolve_content_size(style, :width, available, nil)

    child_basis = if used_width == nil, do: available, else: used_width

    in_flow =
      math_block_children(children)

    count = Enum.count(in_flow, &(not space_like?(&1)))
    embellished? = embellished?(node)

    in_flow
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, [], 0}, fn {child, child_index}, {:ok, prepared, index} ->
      operator_context =
        if embellished? and tag != "math" and
             (tag not in ["mfrac" | @script_elements] or child_index == 0) do
          Map.get(context, :operator_context, %{})
        else
          %{
            parent: if(tag in @core_elements, do: tag, else: :grouping),
            index: index,
            count: count,
            child_index: child_index
          }
        end

      child_context =
        context
        |> Map.put(:operator_context, operator_context)
        |> Map.put(:containing_formatting_context, :math)
        |> then(fn child_context ->
          if used_width == nil,
            do: child_context,
            else: Map.put(child_context, :available_content_width, child_basis)
        end)

      result = prepare_node(child, child_context)

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
           add_operator_space: tag == "math" or not embellished?,
           css_math_owner:
             owner? and not (tag == "math" and Map.get(style, :_math_display) == :block),
           available_inline_size: available,
           source_node: node,
           source_context: context
         }}

      error ->
        error
    end
  end

  # Core's multi-character operator fallback is the same text/HTML flow as
  # the other tokens, including blockification of original direct child boxes.
  defp prepare_token_content(node, children, style, runs, context) do
    math_formatting? = Map.get(style, :_math_formatting, style.display != :inline_block)

    metrics = if Map.has_key?(style, :_math_element), do: :ink, else: :advance

    available =
      Map.get(context, :available_content_width, Map.get(context, :available_inline_size))

    used_width =
      if math_formatting?, do: Layout.resolve_content_size(style, :width, available, nil)

    # Native descendants use their real token owner's content width. The source
    # owner itself retains its original enclosing context for CSS paint placement.
    context =
      if used_width == nil,
        do: context,
        else: Map.put(context, :available_content_width, used_width)

    context =
      if math_formatting?, do: Map.put(context, :native_formatting_context, :math), else: context

    native? =
      Enum.any?(children, &native_token_child?(&1, style.font_face)) or
        (not math_formatting? and
           (Map.get(context, :resource_context) != nil or
              Map.get(context, :available_content_width) != nil or
              Map.get(context, :available_inline_size) != nil or
              Enum.any?(
                [:width, :height, :min_width, :max_width, :min_height, :max_height],
                &(Map.get(style, &1) != nil)
              )))

    with :ok <- validate_token_text(runs, native?) do
      if native? do
        if node.tag == "mtext" and not math_formatting? do
          prepare_native_token(
            children,
            style,
            runs,
            Map.put(
              context,
              :native_formatting_context,
              Map.get(context, :containing_formatting_context, :math)
            ),
            math_formatting?,
            :inline,
            node
          )
        else
          prepare_native_token(children, style, runs, context, math_formatting?)
        end
      else
        if not math_formatting? or Enum.any?(children, &(Map.get(&1, :type) == :element)) do
          # Standalone unconstrained glyph measurement retains its intrinsic
          # flow. Actual HTML surfaces use the native branch above. Math
          # formatting blockifies each original direct child box.
          lines = if math_formatting?, do: Enum.map(children, &[&1]), else: [children]

          Enum.reduce_while(lines, {:ok, []}, fn line, {:ok, prepared} ->
            with {:ok, runs} <- token_runs(line, true),
                 {:ok, glyphs, text} <-
                   prepare_token_glyphs(runs, metrics) do
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
          with {:ok, glyphs, actual_text} <-
                 prepare_token_glyphs(runs, metrics),
               {:ok, ink} <- prepare_token_layout_ink(runs, glyphs, style) do
            {:ok,
             %{
               kind: :token,
               children: glyphs,
               style: style,
               actual_text: actual_text,
               layout_ink: ink
             }}
          end
        end
      end
    end
  end

  # Flex and grid are unambiguously nonmath displays, including when the
  # CSS parser retains inherited Math context for their descendants.
  defp css_schema?(tag, style) do
    (tag in ~w(mfrac msqrt mroot mrow mstyle merror) or tag not in @core_elements) and
      (not Map.get(style, :_math_formatting, true) or
         Map.get(style, :display) in [:flex, :inline_flex, :grid, :inline_grid])
  end

  # A schema with ordinary CSS display uses HTML flow, while each MathML
  # child keeps its own math formatting, selected glyphs and original text.
  defp prepare_css_schema(%{children: children, style: style} = source_node, context) do
    containing_context = Map.get(context, :containing_formatting_context, :math)

    # This source owner lays its children out in its own CSS formatting
    # context, even when the owner itself participates in a Math container.
    child_context =
      Map.put(
        context,
        :containing_formatting_context,
        case style.display do
          display when display in [:flex, :inline_flex] -> :flex
          display when display in [:grid, :inline_grid] -> :grid
          _ -> :flow
        end
      )

    children
    |> Enum.reduce_while({:ok, [], ""}, fn child, {:ok, projected, text} ->
      result =
        case child do
          %{type: :element, namespace: :mathml, style: %{display: :none}} ->
            {:ok, child, ""}

          %{type: :element, namespace: :mathml, style: child_style} ->
            with {:ok, measured} <-
                   NativeElixirPdfUtilities.HtmlToPdf.MathLayout.measure(child, child_context) do
              atom_style =
                child_style
                |> anonymous_box_style()
                |> Map.put(:display, :inline_block)
                |> Map.put(:_math_box, measured)

              atom = %{type: :element, tag: "span", style: atom_style, children: []}
              block? = child_style.display in [:block, :flex, :grid]
              tag = if block?, do: "div", else: "span"

              # CSS schemas already include their complete native owner box.
              # Other Math boxes leave owner decoration to this CSS wrapper.
              child_style =
                if css_schema?(child.tag, child_style) or
                     Map.get(measured, :css_math_owner, false) do
                  child_style
                  |> anonymous_box_style()
                  |> Map.put(:display, if(block?, do: :block, else: :inline))
                else
                  child_style
                end

              # Reuse the ordinary block-Math adapter's strut-free container.
              # The source child is already measured, including its own lines.
              child_style =
                if block?, do: Map.put(child_style, :_math_flow, true), else: child_style

              {:ok, %{type: :element, tag: tag, style: child_style, children: [atom]},
               measured.actual_text}
            end

          _ ->
            with {:ok, runs} <- token_runs([child], true),
                 do: {:ok, child, runs |> token_characters() |> Enum.map_join(& &1.text)}
        end

      case result do
        {:ok, child, logical} -> {:cont, {:ok, [child | projected], text <> logical}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, projected, text} ->
        with {:ok, prepared} <-
               prepare_native_token(
                 Enum.reverse(projected),
                 style,
                 [],
                 Map.put(context, :native_formatting_context, containing_context),
                 false,
                 case style.display do
                   :flex -> :inline_flex
                   :grid -> :inline_grid
                   :block -> :inline_block
                   display -> display
                 end,
                 source_node
               ),
             do: {:ok, %{prepared | style: anonymous_box_style(style), actual_text: text}}

      error ->
        error
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

  defp prepare_native_token(
         children,
         style,
         runs,
         context,
         math_formatting?,
         flow_display \\ :inline,
         source_node \\ nil
       ) do
    children = Enum.map(children, &prepare_native_text(&1, math_formatting?, style.display))

    boxes =
      if math_formatting? do
        Enum.reject(children, &(Map.get(&1.style, :display) == :none))
      else
        if source_node != nil do
          # The shared adapter derives used display from the actual containing
          # context. Preserve the source owner and its computed display.
          [%{source_node | children: children}]
        else
          [
            %{
              type: :element,
              tag: "span",
              style: Map.put(style, :display, flow_display),
              children: children
            }
          ]
        end
      end

    constraints = %{
      unsupported_glyphs: Map.get(context, :unsupported_glyphs, :replace),
      available_width:
        Map.get(context, :available_content_width, Map.get(context, :available_inline_size))
    }

    constraints =
      case Map.fetch(context, :native_formatting_context) do
        {:ok, parent} -> Map.put(constraints, :formatting_context, parent)
        :error -> constraints
      end

    first_baseline? =
      math_formatting? or Map.get(context, :native_formatting_context, :flow) != :flow

    Enum.reduce_while(boxes, {:ok, []}, fn node, {:ok, measured} ->
      case NativeElixirPdfUtilities.HtmlToPdf.Layout.measure_child(
             node,
             Map.get(context, :resource_context),
             constraints
           ) do
        {:ok, child} ->
          child =
            if first_baseline? or source_node != nil do
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
           outer_box_measured: source_node != nil,
           actual_text: runs |> token_characters() |> Enum.map_join(& &1.text)
         }}

      error ->
        error
    end
  end

  # Ordinary annotation owners and foreign row children use their actual CSS
  # box. Its paint, size, baseline and resource checks belong to the shared
  # child adapter; the MathML measurer must not add the owner's edges twice.
  defp prepare_native_element(node, context) do
    context =
      Map.put_new(
        context,
        :native_formatting_context,
        Map.get(context, :containing_formatting_context, :math)
      )

    with {:ok, runs} <- token_runs(node.children),
         :ok <- validate_token_text(runs, true),
         {:ok, prepared} <- prepare_native_token([node], node.style, runs, context, true) do
      {:ok, Map.put(prepared, :native_owner, true)}
    end
  end

  # Apply math-auto to each original node before ordinary shaping. A fragment
  # map preserves original Unicode when the native line helper splits its text.
  defp prepare_native_text(node, math_text?, parent_display) do
    case node do
      %{style: %{display: :none}} ->
        node

      %{type: :element, style: style} = atom
      when parent_display in [:flex, :inline_flex, :grid, :inline_grid] and
             (is_map_key(style, :_math_box) or is_map_key(style, :svg_scene)) ->
        # Existing item builders consume contents. An anonymous CSS item owner
        # carries the real source atom, including its prepared scene/font contract.
        inner = %{
          atom
          | style: style |> anonymous_box_style() |> Map.put(:display, :inline_block)
        }

        owner_style = Map.drop(style, [:_math_box, :svg_scene])

        # Flex/grid blockify an inline math root. Reuse its ordinary block-Math
        # contract: no text strut and centered equation content within the item.
        owner_style =
          if atom.tag == "math" and Map.has_key?(style, :_math_box),
            do: owner_style |> Map.put(:_math_flow, true) |> Map.put(:text_align, :center),
            else: owner_style

        %{type: :element, tag: "span", style: owner_style, children: [inner]}

      %{type: :text, text: text, style: style} ->
        FontValidator.check(:max_rendered_text_bytes, byte_size(text))
        FontValidator.reserve(:max_font_math_work, byte_size(text))

        displayed =
          if Map.get(style, :text_transform) == :math_auto, do: math_auto_text(text), else: text

        style = if math_text?, do: Map.put(style, :white_space, :nowrap), else: style

        style =
          style
          |> Map.put(:_math_native_group, make_ref())
          |> Map.put(:_math_native_actual_source, text)
          |> Map.put(
            :_source_glyph_characters,
            NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator.source_characters(
              node,
              displayed
            )
          )
          |> Map.put(
            :_math_original_characters,
            Map.new(Enum.zip(String.codepoints(displayed), String.codepoints(text)))
          )

        %{node | text: displayed, style: style}

      %{children: children, style: style} ->
        %{node | children: Enum.map(children, &prepare_native_text(&1, false, style.display))}
    end
  end

  # Core 3.2.4.1 and 3.2.5.1 classify original in-flow structure, before
  # preparation adds anonymous token boxes. This determines the outer form.
  # Core 2.1 and 4.1 give unknown MathML elements the mrow algorithm.
  # A known Core schema must still pass its own implemented content model.
  defp grouping_element?(tag),
    do: is_binary(tag) and (tag in @grouping_elements or tag not in @core_elements)

  # Blink's LayoutMathMLBlock admits MathMLElement children only. Character
  # data and HTML integration children belong to token or ordinary CSS flow,
  # not anonymous token boxes in a default Math row.
  defp math_block_children(children) do
    Enum.filter(children, fn child ->
      case child do
        %{namespace: :mathml, type: :element} ->
          Map.get(Map.get(child, :style, %{}), :display) != :none

        %{type: type} when type in [:element, :text, :comment, :processing_instruction] ->
          false

        _ ->
          throw({:math_layout_error, failure("Math rows require actual typed source children")})
      end
    end)
  end

  defp space_like?(node) do
    case node do
      %{namespace: :mathml, tag: tag} when tag in ~w(mtext mspace annotation annotation-xml) ->
        true

      %{namespace: :mathml, tag: tag, children: children} ->
        (tag == "mpadded" or grouping_element?(tag)) and
          Enum.all?(math_block_children(children), &space_like?/1)
    end
  end

  defp embellished?(node) do
    case node do
      %{namespace: :mathml, tag: "mo"} ->
        true

      %{namespace: :mathml, tag: tag, children: children}
      when tag in ["mfrac" | @script_elements] ->
        case math_block_children(children) do
          [first | _] -> embellished?(first)
          [] -> false
        end

      %{namespace: :mathml, tag: tag, children: children} ->
        if tag == "mpadded" or grouping_element?(tag) do
          case Enum.reject(math_block_children(children), &space_like?/1) do
            [child] -> embellished?(child)
            _ -> false
          end
        else
          false
        end
    end
  end

  @doc false
  @spec prepare_css_context(keyword()) ::
          {:ok, map()} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare_css_context(opts) do
    with {:ok, viewport} <-
           (case Keyword.get(opts, :__client_css_viewport__) do
              %{width: width, height: height} ->
                PrintMediaValidator.client_context(opts, {width * 0.75, height * 0.75})

              _ ->
                PrintMediaValidator.renderer_context(opts)
            end) do
      {:ok, %{viewport_width: viewport.width * 0.75, viewport_height: viewport.height * 0.75}}
    end
  end

  # CSS metric units use the ordinary selected face, without math-auto or ssty.
  # Calculated Math attributes are ignored by Blink before font metrics resolve.
  defp prepare_operator_length_context(attributes, style, context) do
    units =
      Enum.reduce_while(~w(lspace rspace minsize maxsize), {:ok, []}, fn property, {:ok, units} ->
        case Map.get(attributes, property) do
          nil ->
            {:cont, {:ok, units}}

          value ->
            case HtmlValidator.parse_css_number(
                   if(is_binary(value),
                     do: HtmlValidator.trim_ascii_whitespace(value) |> String.downcase(:ascii),
                     else: value
                   )
                 ) do
              {_number, unit} -> {:cont, {:ok, Enum.uniq(units ++ [unit])}}
              :error -> {:cont, {:ok, units}}
            end
        end
      end)

    with {:ok, units} <- units do
      units = Enum.filter(units, &(&1 in ~w(ch cap lh ic rex rch rcap ric rlh)))

      Enum.reduce_while(
        units,
        {:ok, Map.take(context, [:viewport_width, :viewport_height])},
        fn unit, {:ok, context} ->
          root? = String.starts_with?(unit, "r")
          metric_unit = if root?, do: String.trim_leading(unit, "r"), else: unit

          font =
            if root?, do: Map.get(style, :_root_font_face, style.font_face), else: style.font_face

          size =
            if root?, do: Map.get(style, :_root_font_size, style.font_size), else: style.font_size

          key =
            Map.fetch!(
              %{
                "ex" => :x_height,
                "ch" => :zero_advance,
                "cap" => :cap_height,
                "ic" => :ideographic_advance,
                "lh" => :line_height
              },
              metric_unit
            )

          key =
            if root?,
              do:
                Map.fetch!(
                  %{
                    x_height: :root_x_height,
                    cap_height: :root_cap_height,
                    zero_advance: :root_zero_advance,
                    ideographic_advance: :root_ideographic_advance,
                    line_height: :root_line_height
                  },
                  key
                ),
              else: key

          result =
            case metric_unit do
              "lh" ->
                {:ok,
                 if(root?,
                   do: Map.get(style, :_root_line_height, style.line_height),
                   else: style.line_height
                 )}

              "ex" ->
                with {:ok, ratio} <- FontValidator.prepare_x_height(font), do: {:ok, ratio * size}

              _ ->
                character = Map.fetch!(%{"ch" => "0", "cap" => "H", "ic" => "水"}, metric_unit)

                case prepare_glyph(character, font, size, []) do
                  {:ok, glyph} ->
                    metric =
                      if metric_unit == "cap", do: elem(glyph.bounds, 3), else: glyph.advance

                    {:ok, metric * glyph.font_size / glyph.units_per_em}

                  {:error, {:unsupported_glyph, _}} when metric_unit == "ic" ->
                    {:ok, size}

                  error ->
                    error
                end
            end

          case result do
            {:ok, metric} -> {:cont, {:ok, Map.put(context, key, metric)}}
            error -> {:halt, error}
          end
        end
      )
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

              {_left, bottom, _right, top} = base.ink_bounds
              natural = top - bottom
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
                 prepare_selected_stretch(base, operator.stretch_axis, target) do
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

      operator.stretchy and construction != nil and operator.stretch_axis == :vertical ->
        # Core's preferred inline size is independent of whether a containing
        # row has supplied a block stretch target. Keep natural paint unchanged.
        {:ok,
         %{
           shape: %{kind: :glyph, glyph: base},
           target_ascent: nil,
           target_descent: nil,
           intrinsic_width: preferred_vertical_width(base, construction)
         }}

      not operator.stretchy and operator.largeop and
          (Map.get(style, :math_style, :normal) == :normal or
             Map.get(base.source, :stretch_metrics) == :ink) ->
        construction = Map.get(base.source.metrics.variants.vertical, base.glyph_id)
        variants = if construction, do: construction.variants, else: []

        minimum =
          if Map.get(style, :math_style, :normal) == :normal,
            do: base.source.metrics.constants.display_operator_min_height * scale,
            else: 0.0

        FontValidator.reserve(:max_font_math_work, length(variants))

        selected =
          Enum.find(variants, fn variant ->
            {_left, bottom, _right, top} =
              glyph_ink_bounds(
                elem(base.source.bounds, variant.glyph),
                base.source,
                variant.glyph
              )

            top - bottom >= minimum
          end) || List.last(variants)

        id = if selected, do: selected.glyph, else: base.glyph_id

        with {:ok, glyph} <- selected_glyph(id, base.source),
             {:ok, [axis]} <-
               resolve_constants(base.source.metrics, [:axis_height], base.font_size) do
          {_left, bottom, _right, top} = glyph.ink_bounds
          centered? = operator.symmetric and Map.get(base.source, :stretch_metrics) == :ink

          {:ok,
           %{
             shape: %{kind: :glyph, glyph: glyph},
             target_ascent: if(centered?, do: (top - bottom) / 2 + axis),
             target_descent: if(centered?, do: (top - bottom) / 2 - axis),
             intrinsic_width: glyph.advance * scale
           }}
        end

      true ->
        {:ok, nil}
    end
  end

  @doc false
  @spec preferred_vertical_width(map(), map()) :: number()
  def preferred_vertical_width(base, construction) do
    scale = base.font_size / base.units_per_em

    ids =
      [base.glyph_id] ++
        Enum.map(construction.variants, & &1.glyph) ++
        case construction.assembly do
          %{parts: parts} -> Enum.map(parts, & &1.glyph)
          _ -> []
        end

    FontValidator.reserve(:max_font_math_work, length(ids))
    Enum.max(Enum.map(ids, &(elem(base.source.advances, &1) * scale)))
  end

  defp resolve_operator_size(value, natural) do
    case value do
      {:percentage, number} ->
        natural * (number / 100)

      number ->
        number
    end
  end

  @doc "Validates URI or initial scoped fragment link metadata on real Math paint styles."
  @spec validate_link_style(term(), String.t()) :: :ok | {:error, Diagnostics.detailed_error()}
  def validate_link_style(style, tag) do
    valid? =
      case style do
        style when is_map(style) ->
          link = Map.get(style, :link_url)
          destination = Map.get(style, :link_destination)

          (link == nil or HtmlValidator.valid_link_url?(link)) and
            HtmlLinkValidator.valid_prepared_destination?(destination) and
            (link == nil or destination == nil)

        _ ->
          false
      end

    if valid?,
      do: :ok,
      else:
        Diagnostics.error(
          :layout,
          :invalid_layout,
          "MathML links require a validated URI or scoped initial HTML fragment destination",
          operation: :measure_math,
          module: __MODULE__,
          source: tag
        )
  end

  defp validate_box_style(tag, style) do
    cond do
      tag in ["mtable", "mtr", "mtd"] ->
        with :ok <- validate_link_style(style, tag) do
          MathTableValidator.validate_style(style, tag)
        end

      css_schema?(tag, style) or grouping_element?(tag) or tag in ~w(msqrt mroot) or
        (tag in ["mo" | @token_elements] and Map.get(style, :_math_formatting, true)) or
        tag not in @core_elements or
          (tag == "mtext" and not Map.get(style, :_math_formatting, true)) ->
        with :ok <- validate_link_style(style, tag),
             :ok <- validate_background_style(style, tag),
             :ok <- validate_math_owner_style(tag, style),
             :ok <-
               Enum.reduce_while(
                 [:width, :height, :min_width, :max_width, :min_height, :max_height],
                 :ok,
                 fn property, :ok ->
                   case HtmlValidator.validate_prepared_css_size(
                          Map.get(style, property),
                          property
                        ) do
                     :ok -> {:cont, :ok}
                     error -> {:halt, error}
                   end
                 end
               ) do
          :ok
        else
          {:error, {reason, diagnostic}} ->
            {:error, {reason, Diagnostics.with_context(diagnostic, source: tag)}}
        end

      true ->
        css_schema? = css_schema?(tag, style)

        unsupported? =
          ((grouping_element?(tag) or tag in ~w(mspace mpadded) or
              tag in @script_elements) and
             not Map.get(style, :_math_formatting, true)) or
            (tag != "math" and Map.get(style, :html_target) != nil) or
            (Map.get(style, :position, :static) != :static and
               (not css_schema? or Map.get(style, :position) != :relative)) or
            (not css_schema? and Map.get(style, :overflow, :visible) != :visible) or
            (not css_schema? and Map.get(style, :box_sizing, :content_box) != :content_box) or
            (not css_schema? and
               Enum.any?([:width, :height], fn property ->
                 value = Map.get(style, property)

                 value != nil and
                   (not is_number(value) or
                      (tag in ["mfrac", "msqrt", "mroot" | @script_elements] and
                         (value < 0 or value > Limits.get(:max_css_numeric_magnitude))))
               end)) or
            (not css_schema? and
               Enum.any?(
                 [:min_width, :max_width, :min_height, :max_height],
                 &(Map.get(style, &1) != nil)
               ))

        link_validation = validate_link_style(style, tag)

        cond do
          link_validation != :ok ->
            link_validation

          unsupported? ->
            Diagnostics.error(
              :layout,
              :unsupported_html,
              "MathML ordinary flow schemata, non-root fragment targets, positioned, clipped, constrained or root box sizing requires the later CSS composition batch",
              operation: :render,
              module: __MODULE__,
              source: tag
            )

          true ->
            :ok
        end
    end
  end

  @doc "Validates Math source backgrounds through the canonical decoded writer-image authority."
  @spec validate_background_style(term(), String.t()) ::
          :ok | {:error, Diagnostics.detailed_error()}
  def validate_background_style(style, tag) do
    case style do
      style when is_map(style) ->
        images =
          case Map.get(style, :background_image) do
            nil -> []
            image -> [%{type: :image, image: image, x: 0.0, y: 0.0, width: 1.0, height: 1.0}]
          end

        case WriterValidator.validate_native_boxes(images) do
          :ok ->
            :ok

          {:error, {:invalid_pdf_input, _}} ->
            Diagnostics.error(
              :layout,
              :unsupported_html,
              "MathML background images require validated decoded image data",
              operation: :measure_math,
              module: __MODULE__,
              source: tag
            )

          {:error, {reason, diagnostic}} ->
            {:error, {reason, Diagnostics.with_context(diagnostic, source: tag)}}
        end

      _ ->
        failure("Math backgrounds require a prepared style map", tag)
    end
  end

  # Native CSS schemata use their canonical child adapter. Math owners retain
  # atomic source boxes, so clipping and positioning need a separate paint path.
  defp validate_math_owner_style(tag, style) do
    ordinary? =
      css_schema?(tag, style) or
        (tag == "mtext" and not Map.get(style, :_math_formatting, true))

    edges? =
      Enum.all?([:margin, :padding, :border_widths], fn property ->
        case Map.get(style, property, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}) do
          edges when is_map(edges) ->
            Enum.all?([:top, :right, :bottom, :left], fn edge ->
              value = Map.get(edges, edge)

              is_number(value) and abs(value) <= Limits.get(:max_css_numeric_magnitude) and
                (property == :margin or value >= 0)
            end)

          _ ->
            false
        end
      end)

    if ordinary? or
         (edges? and Map.get(style, :_math_formatting, true) and
            Map.get(style, :position, :static) in [:static, :relative] and
            Map.get(style, :overflow, :visible) in [:visible, :hidden] and
            Map.get(style, :box_sizing, :content_box) in [:content_box, :border_box]) do
      :ok
    else
      Diagnostics.error(
        :layout,
        :unsupported_html,
        "Math formatting owner boxes require bounded box edges and CSS box sizing; Math owner placement requires static or relative positioned boxes and visible or hidden overflow",
        operation: :measure_math,
        module: __MODULE__,
        source: tag
      )
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

  @doc false
  @spec math_length(term(), map()) ::
          {:ok, term()} | :error | {:error, Diagnostics.detailed_error()}
  def math_length(value, style) do
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
            {:ok, [%{text: text, style: style, source_node: child}]}

          %{type: :element, style: %{display: :none}} ->
            {:ok, []}

          %{type: :element, style: %{_math_box: %{actual_text: text}} = style} ->
            {:ok, [%{text: text, style: style}]}

          %{type: :element, style: %{svg_scene: _}} ->
            # Converted SVG is a single image. Its internal text and links do
            # not become ordinary Math token text or PDF annotations.
            {:ok, []}

          %{type: :element, style: %{_math_flow: true}, children: children} ->
            token_runs(children)

          %{type: :element, namespace: :mathml} ->
            failure("nested MathML token content requires its prepared atomic math box")

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
        Enum.reduce(
          Enum.with_index(String.codepoints(run.text)),
          {characters, previous_space},
          fn {character, offset}, {characters, previous_space} ->
            space? = character in [" ", "\t", "\n", "\r", "\f"]
            preserve? = Map.get(run.style, :white_space) in [:pre, :pre_wrap, :break_spaces]

            if space? and previous_space and not preserve? do
              {characters, true}
            else
              {[
                 %{
                   text: if(space?, do: " ", else: character),
                   style: run.style,
                   source_fragments:
                     if(
                       Map.has_key?(run, :source_node) and Map.has_key?(run.source_node, :node_id),
                       do: [
                         %{
                           owner: Map.drop(run.source_node, [:style, :children]),
                           text: character,
                           offset: offset
                         }
                       ],
                       else: []
                     )
                 }
                 | characters
               ], space?}
            end
          end
        )
      end)

    characters
    |> Enum.drop_while(
      &(&1.text == " " and Map.get(&1.style, :white_space) not in [:pre, :pre_wrap, :break_spaces])
    )
    |> Enum.reverse()
  end

  defp prepare_token_glyphs(runs, metrics) do
    characters = token_characters(runs)

    Enum.reduce_while(characters, {:ok, []}, fn character, {:ok, glyphs} ->
      style = character.style

      transform =
        if Map.get(style, :_math_auto_scalar, false) and
             Map.get(style, :text_transform) == :math_auto, do: :math_auto, else: :none

      case prepare_glyph(character.text, style.font_face, style.font_size,
             text_transform: transform,
             stretch_metrics: metrics,
             script_feature_level: min(max(Map.get(style, :math_depth, 0), 0), 2)
           ) do
        {:ok, glyph} ->
          glyph = %{
            glyph
            | source:
                glyph.source
                |> Map.put(:source_fragments, character.source_fragments)
                |> Map.put(:stretch_metrics, Map.get(glyph.source, :stretch_metrics, :advance))
          }

          {:cont, {:ok, [%{kind: :glyph, glyph: glyph, style: style} | glyphs]}}

        error ->
          {:halt, error}
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

  # Token layout measures the original characters before math-auto paint
  # transforms, but retains their selected OpenType script-style substitutions.
  # Measuring an unsubstituted digit would give its fraction the wrong bounds.
  defp prepare_token_layout_ink(runs, glyphs, style) do
    if Map.has_key?(style, :_math_element) do
      # Glyph preparation already validated and retained these exact source
      # outlines. Reuse them for the original-character metric request.
      sources = Map.new(glyphs, fn %{glyph: glyph} -> {glyph.font_face.id, glyph.source} end)

      selected =
        Map.new(glyphs, fn %{glyph: glyph} ->
          {{glyph.font_face.id, glyph.font_size, glyph.source.displayed}, glyph.glyph_id}
        end)

      ink =
        Enum.reduce(token_characters(runs), nil, fn character, bounds ->
          font = character.style.font_face
          <<codepoint::utf8>> = character.text
          source = Map.fetch!(sources, font.id)
          source = %{source | size: character.style.font_size, stretch_metrics: :advance}

          glyph =
            Map.get(
              selected,
              {font.id, character.style.font_size, codepoint},
              Map.get(font.cmap, codepoint, 0)
            )

          {_left, bottom, _right, top} =
            glyph_ink_bounds(elem(source.bounds, glyph), source, glyph)

          if bounds == nil,
            do: {top, -bottom},
            else: {max(elem(bounds, 0), top), max(elem(bounds, 1), -bottom)}
        end)

      {:ok, ink}
    else
      {:ok, nil}
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
          characters =
            Enum.map(run, fn glyph ->
              %{
                displayed: <<glyph.glyph.source.displayed::utf8>>,
                fragments: Map.get(glyph.glyph.source, :source_fragments, [])
              }
            end)

          plan =
            NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator.shape_characters(
              characters,
              first.glyph.font_face,
              Map.get(first.style, :letter_spacing, 0.0)
            )

          Enum.reduce_while(plan, {:ok, [], run}, fn character, {:ok, glyphs, remaining} ->
            text = character.displayed
            {_consumed, remaining} = Enum.split(remaining, character.consumed)
            fragments = character.fragments

            case prepare_glyph(text, first.glyph.font_face, first.glyph.font_size,
                   stretch_metrics: first.glyph.source.stretch_metrics,
                   script_feature_level: min(max(Map.get(first.style, :math_depth, 0), 0), 2)
                 ) do
              {:ok, glyph} ->
                glyph = %{glyph | source: Map.put(glyph.source, :source_fragments, fragments)}

                {:cont,
                 {:ok, [%{kind: :glyph, glyph: glyph, style: first.style} | glyphs], remaining}}

              error ->
                {:halt, error}
            end
          end)
          |> case do
            {:ok, glyphs, []} -> {:ok, Enum.reverse(glyphs)}
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
        Enum.all?(
          Keyword.keys(opts),
          &(&1 in [:text_transform, :script_feature_level, :stretch_metrics])
        ) and
        Keyword.get(opts, :text_transform, :none) in [:none, :math_auto] and
        Keyword.get(opts, :script_feature_level, 0) in [0, 1, 2] and
        Keyword.get(opts, :stretch_metrics, :advance) in [:advance, :ink]

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
              stretch_metrics: Keyword.get(opts, :stretch_metrics, :advance),
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
    with {:ok, base} <- prepare_glyph(text, font, size, opts),
         do: prepare_selected_stretch(base, axis, target)
  end

  # Both authored operators and public scalar measurements stretch their actual
  # selected glyph. Authored provenance survives variants and assembly parts.
  defp prepare_selected_stretch(base, axis, target) do
    text = base.text
    size = base.font_size

    if axis in [:horizontal, :vertical] and is_number(target) and target >= 0 and
         target <= Limits.get(:max_css_numeric_magnitude) do
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
            if Map.get(source, :stretch_metrics) == :ink do
              {left, bottom, right, top} = base.ink_bounds
              if axis == :horizontal, do: right - left, else: top - bottom
            else
              if axis == :horizontal, do: base.advance * scale, else: (ymax - ymin) * scale
            end

          if natural_size >= target do
            {:ok, %{kind: :glyph, glyph: base}}
          else
            selected =
              Enum.find(construction.variants, fn variant ->
                extent =
                  if Map.get(source, :stretch_metrics) == :ink do
                    {left, bottom, right, top} =
                      glyph_ink_bounds(elem(source.bounds, variant.glyph), source, variant.glyph)

                    if axis == :horizontal, do: right - left, else: top - bottom
                  else
                    variant.advance * scale
                  end

                extent >= target
              end)

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
    else
      failure(
        "math stretching requires a horizontal or vertical axis and a bounded nonnegative target"
      )
    end
  end

  # CSS operator stretching uses outward-rounded CSS-pixel ink bounds. The standalone
  # design-metric API retains exact outlines. This rounding affects variant
  # selection as well as the allocated ascent and descent, not PDF glyph paint.
  defp glyph_ink_bounds({left, bottom, right, top}, source, _glyph_id) do
    scale = source.size / source.metrics.units_per_em

    if source.stretch_metrics == :ink do
      {Float.floor(left * scale / 0.75) * 0.75, Float.floor(bottom * scale / 0.75) * 0.75,
       Float.ceil(right * scale / 0.75) * 0.75, Float.ceil(top * scale / 0.75) * 0.75}
    else
      {left * scale, bottom * scale, right * scale, top * scale}
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
           ink_bounds: glyph_ink_bounds(elem(source.bounds, id), source, id),
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

  defp failure(message, source \\ nil) do
    Diagnostics.error(:layout, :invalid_document, message,
      operation: :measure_math,
      module: __MODULE__,
      source: source
    )
  end
end
