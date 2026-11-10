defmodule NativeElixirPdfUtilities.Validators.CssDisplayValidator do
  @moduledoc "Static box-generation, outer/inner display and inherited visibility grammar."
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @legacy %{
    "run-in" => {:inline, false},
    "run-in-flow-root" => {:inline_block, true},
    "run-in-flex" => {:inline_flex, true},
    "run-in-grid" => {:inline_grid, true},
    "run-in-table" => {:inline_table, true},
    "run-in-ruby" => {:inline, false},
    "run-in-list-item" => {:list_item, false},
    "run-in-flow-root-list-item" => {:list_item, true},
    "flow" => {:block, false},
    "block" => {:block, false},
    "inline" => {:inline, false},
    "inline-block" => {:inline_block, true},
    "flow-root" => {:block, true},
    "flex" => {:flex, true},
    "inline-flex" => {:inline_flex, true},
    "grid" => {:grid, true},
    "inline-grid" => {:inline_grid, true},
    "table" => {:table, true},
    "inline-table" => {:inline_table, true},
    "list-item" => {:list_item, false},
    "none" => {:none, false},
    "contents" => {:contents, false},
    "ruby" => {:inline, false},
    "table-row" => {:table_row, false},
    "table-cell" => {:table_cell, true},
    "table-row-group" => {:table_row_group, false},
    "table-header-group" => {:table_row_group, false},
    "table-footer-group" => {:table_row_group, false},
    "table-column" => {:table_column, false},
    "table-column-group" => {:table_column_group, false},
    "table-caption" => {:table_caption, true},
    "ruby-base" => {:inline, false},
    "ruby-text" => {:inline, false},
    "ruby-base-container" => {:inline, false},
    "ruby-text-container" => {:inline, false}
  }

  @doc "Parses equivalent legacy and outer/inner display forms without losing formatting-context boundaries."
  @spec prepare(String.t()) :: {:ok, map()} | :error
  def prepare(value) do
    tokens =
      value
      |> HtmlValidator.split_css_tokens()
      |> Enum.map(fn token ->
        case HtmlValidator.parse_css_identifier(token) do
          {:ok, name, ""} -> String.downcase(name, :ascii)
          _ -> nil
        end
      end)

    canonical =
      case tokens do
        [one] ->
          one

        _ ->
          outer = Enum.filter(tokens, &(&1 in ["block", "inline", "run-in"]))

          inner =
            Enum.filter(tokens, &(&1 in ["flow", "flow-root", "flex", "grid", "table", "ruby"]))

          list? = "list-item" in tokens

          if tokens != [] and length(outer) <= 1 and length(inner) <= 1 and
               length(tokens) == length(outer) + length(inner) + if(list?, do: 1, else: 0) and
               length(tokens) == length(Enum.uniq(tokens)) do
            outside = List.first(outer) || "block"
            inside = List.first(inner) || "flow"

            case {outside, inside, list?} do
              {"run-in", "flow", false} -> "run-in"
              {"run-in", kind, false} -> "run-in-" <> kind
              {"run-in", "flow", true} -> "run-in-list-item"
              {"run-in", "flow-root", true} -> "run-in-flow-root-list-item"
              {"block", "flow", false} -> "block"
              {"inline", "flow", false} -> "inline"
              {"block", "flow-root", false} -> "flow-root"
              {"inline", "flow-root", false} -> "inline-block"
              {"block", kind, false} when kind in ["flex", "grid", "table"] -> kind
              {"inline", kind, false} when kind in ["flex", "grid", "table"] -> "inline-" <> kind
              {"inline", "ruby", false} -> "ruby"
              {"block", "ruby", false} -> "block-ruby"
              {"block", "flow", true} -> "list-item"
              {"block", "flow-root", true} -> "flow-root-list-item"
              {"inline", "flow", true} -> "inline-list-item"
              {"inline", "flow-root", true} -> "inline-flow-root-list-item"
              _ -> nil
            end
          end
      end

    result =
      case canonical do
        "block-ruby" ->
          {:ok, %{display: :block, _display_role: :ruby}}

        "flow-root-list-item" ->
          {:ok, %{display: :list_item, _flow_root: true}}

        "inline-list-item" ->
          {:ok, %{display: :list_item, _display_outer: :inline}}

        "inline-flow-root-list-item" ->
          {:ok, %{display: :list_item, _display_outer: :inline, _flow_root: true}}

        canonical ->
          case Map.fetch(@legacy, canonical) do
            {:ok, {display, root?}} ->
              properties = %{display: display}

              properties =
                if String.starts_with?(canonical, "run-in"),
                  do: Map.put(properties, :_display_outer, :run_in),
                  else: properties

              properties = if root?, do: Map.put(properties, :_flow_root, true), else: properties

              properties =
                case canonical do
                  "table-header-group" ->
                    Map.put(properties, :table_section, :head)

                  "table-row-group" ->
                    Map.put(properties, :table_section, :body)

                  "table-footer-group" ->
                    Map.put(properties, :table_section, :foot)

                  kind when kind in ["ruby", "run-in-ruby"] ->
                    Map.put(properties, :_display_role, :ruby)

                  "ruby-base" ->
                    Map.put(properties, :_display_role, :ruby_base)

                  "ruby-text" ->
                    Map.merge(properties, %{_display_role: :ruby_text, _ruby_annotation: true})

                  "ruby-base-container" ->
                    Map.put(properties, :_display_role, :ruby_base_container)

                  "ruby-text-container" ->
                    Map.put(properties, :_display_role, :ruby_text_container)

                  _ ->
                    properties
                end

              {:ok, properties}

            :error ->
              :error
          end
      end

    result
  end

  @doc "Parses inherited visibility without removing the element's formatting box."
  @spec visibility(String.t()) :: {:ok, :visible | :hidden | :force_hidden | :collapse} | :error
  def visibility(value) do
    case HtmlValidator.parse_css_identifier(value) do
      {:ok, name, ""} ->
        case String.downcase(name, :ascii) do
          "visible" -> {:ok, :visible}
          "hidden" -> {:ok, :hidden}
          "force-hidden" -> {:ok, :force_hidden}
          "collapse" -> {:ok, :collapse}
          _ -> :error
        end

      _ ->
        :error
    end
  end

  @doc "Preserves forced invisibility independently of a descendant's computed visibility override."
  @spec compute_style(map()) :: map()
  def compute_style(style) do
    style =
      case style do
        %{_cascade_inherited: %{_force_hidden: true}} ->
          Map.put(style, :_force_hidden, true)

        %{_cascade_inherited: %{visibility: :force_hidden}} ->
          Map.put(style, :_force_hidden, true)

        _ ->
          style
      end

    if Map.has_key?(style, :svg_visibility) and
         (Map.get(style, :_force_hidden, false) or Map.get(style, :visibility) == :force_hidden),
       do: Map.put(style, :svg_visibility, :hidden),
       else: style
  end

  @doc "Forms run-in sequences after inheritance and selectors, before block-in-inline box formation."
  @spec prepare_children([map()], map()) :: [map()]
  def prepare_children(children, parent_style) do
    relevant? =
      Enum.any?(children, fn
        %{style: %{_display_outer: :run_in}} ->
          true

        %{style: %{_display_role: role}}
        when role in [:ruby_base, :ruby_text, :ruby_base_container, :ruby_text_container] ->
          true

        _ ->
          false
      end)

    if relevant? do
      children =
        if Map.get(parent_style, :display) in [:flex, :inline_flex, :grid, :inline_grid] or
             not Enum.any?(children, &match?(%{style: %{_display_outer: :run_in}}, &1)),
           do: children,
           else: form_run_ins(children, parent_style)

      if Map.get(parent_style, :_display_role) in [
           :ruby,
           :ruby_base_container,
           :ruby_text_container
         ],
         do: children,
         else: form_anonymous_ruby(children, parent_style)
    else
      children
    end
  end

  defp ruby_internal?(node) do
    get_in(node, [:style, :_display_role]) in [
      :ruby_base,
      :ruby_text,
      :ruby_base_container,
      :ruby_text_container
    ]
  end

  defp form_anonymous_ruby(children, parent_style) do
    case children do
      [] ->
        []

      [first | rest] ->
        if ruby_internal?(first) do
          {sequence, following} =
            Enum.split_while(rest, fn child ->
              ruby_internal?(child) or
                (match?(%{type: :text}, child) and
                   HtmlValidator.trim_ascii_whitespace(child.text) == "")
            end)

          owner = %{
            type: :element,
            tag: "ruby",
            style:
              NativeElixirPdfUtilities.HtmlToPdf.Style.anonymous_box_style(parent_style, :inline)
              |> Map.put(:_display_role, :ruby),
            children: [first | sequence]
          }

          [owner | form_anonymous_ruby(following, parent_style)]
        else
          [first | form_anonymous_ruby(rest, parent_style)]
        end
    end
  end

  defp form_run_ins(children, parent_style) do
    case children do
      [] ->
        []

      [%{style: %{_display_outer: :run_in}} | _] ->
        {sequence, rest} =
          Enum.split_while(children, fn node ->
            case node do
              %{style: %{_display_outer: :run_in}} -> true
              %{style: %{position: position}} when position in [:absolute, :fixed] -> true
              %{type: :text, text: text} -> HtmlValidator.trim_ascii_whitespace(text) == ""
              _ -> false
            end
          end)

        last =
          sequence
          |> Enum.with_index()
          |> Enum.filter(fn {node, _} -> match?(%{style: %{_display_outer: :run_in}}, node) end)
          |> List.last()
          |> elem(1)

        {sequence, trailing} = Enum.split(sequence, last + 1)
        rest = trailing ++ rest

        sequence =
          Enum.map(sequence, fn node ->
            case node do
              %{style: %{_display_outer: :run_in} = style} ->
                if style.display == :inline, do: inlinify(node), else: node

              _ ->
                node
            end
          end)

        case rest do
          [%{style: %{display: display} = style, children: nested} = block | following]
          when display in [:block, :list_item] ->
            if independent?(style) do
              [anonymous_run_in(sequence, parent_style) | form_run_ins(rest, parent_style)]
            else
              # Recurse into the newly adjacent first paragraph with the same
              # sequence, retaining the original inherited styles.
              leading = sequence

              [
                %{block | children: form_run_ins(leading ++ nested, style)}
                | form_run_ins(following, parent_style)
              ]
            end

          _ ->
            {inline, following} =
              Enum.split_while(rest, fn node ->
                case node do
                  %{type: :text} ->
                    true

                  %{style: %{display: display}} ->
                    display in [
                      :inline,
                      :inline_block,
                      :inline_flex,
                      :inline_grid,
                      :inline_table,
                      :image,
                      :none
                    ]
                end
              end)

            [
              anonymous_run_in(sequence ++ inline, parent_style)
              | form_run_ins(following, parent_style)
            ]
        end

      [node | rest] ->
        [node | form_run_ins(rest, parent_style)]
    end
  end

  defp anonymous_run_in(children, parent_style) do
    %{
      type: :element,
      tag: "::run-in",
      style: NativeElixirPdfUtilities.HtmlToPdf.Style.anonymous_box_style(parent_style, :block),
      children:
        Enum.map(children, fn
          %{style: %{_display_outer: :run_in} = style} = node ->
            %{node | style: Map.delete(style, :_display_outer)}

          node ->
            node
        end)
    }
  end

  defp inlinify(node) do
    case node do
      %{type: :element, style: style, children: children} ->
        display =
          case style.display do
            :block -> if Map.get(style, :_flow_root, false), do: :inline_block, else: :inline
            :flex -> :inline_flex
            :grid -> :inline_grid
            :table -> :inline_table
            other -> other
          end

        %{
          node
          | style: Map.put(style, :display, display),
            children: Enum.map(children, &inlinify/1)
        }

      _ ->
        node
    end
  end

  @doc "Identifies a separate static formatting context for margins and descendant line counting."
  @spec independent?(map()) :: boolean()
  def independent?(style) do
    Map.get(style, :display) in [
      :inline_block,
      :flex,
      :inline_flex,
      :grid,
      :inline_grid,
      :table,
      :inline_table,
      :table_cell
    ] or Map.get(style, :_flow_root, false) or
      Map.get(style, :position, :static) in [:absolute, :fixed] or
      Map.get(style, :contain, []) != [] or Map.get(style, :_fieldset, false) or
      Map.has_key?(style, :embedded_document) or
      NativeElixirPdfUtilities.Validators.CssOverflowValidator.independent?(style)
  end

  @doc "Returns actionable diagnostics for malformed computed box metadata."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    if valid_style?(style) do
      :ok
    else
      NativeElixirPdfUtilities.Diagnostics.error(
        :layout,
        :invalid_layout,
        "computed display, visibility, logical axes or stacking geometry is malformed; recompute styles before layout",
        module: __MODULE__,
        operation: :layout,
        source: source
      )
    end
  end

  @doc "Checks computed box-generation and axis metadata before layout consumes it."
  @spec valid_style?(map()) :: boolean()
  def valid_style?(style) do
    case style do
      style
      when not is_map_key(style, :writing_mode) and not is_map_key(style, :direction) and
             not is_map_key(style, :visibility) and not is_map_key(style, :_flow_root) and
             not is_map_key(style, :_display_outer) and not is_map_key(style, :_display_role) and
             not is_map_key(style, :_force_hidden) and
             not is_map_key(style, :_containing_direction) and
             not is_map_key(style, :_containing_writing_mode) ->
        true

      _ ->
        Map.get(style, :writing_mode, :horizontal_tb) in [
          :horizontal_tb,
          :vertical_rl,
          :vertical_lr,
          :sideways_rl,
          :sideways_lr
        ] and
          Map.get(style, :direction, :ltr) in [:ltr, :rtl] and
          Map.get(style, :_containing_direction, :ltr) in [:ltr, :rtl] and
          Map.get(style, :_containing_writing_mode, :horizontal_tb) in [
            :horizontal_tb,
            :vertical_rl,
            :vertical_lr,
            :sideways_rl,
            :sideways_lr
          ] and
          Map.get(style, :visibility, :visible) in [:visible, :hidden, :force_hidden, :collapse] and
          is_boolean(Map.get(style, :_force_hidden, false)) and
          is_boolean(Map.get(style, :_flow_root, false)) and
          Map.get(style, :_display_outer, :block) in [:block, :inline, :run_in] and
          Map.get(style, :_display_role) in [
            nil,
            :ruby,
            :ruby_base,
            :ruby_text,
            :ruby_base_container,
            :ruby_text_container
          ]
    end
  end
end
