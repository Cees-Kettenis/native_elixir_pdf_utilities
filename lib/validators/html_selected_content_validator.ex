defmodule NativeElixirPdfUtilities.Validators.HtmlSelectedContentValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec prepare_select_defaults(map(), [tuple()]) :: map()
  def prepare_select_defaults(style, declarations) do
    case Map.get(style, :_select_inherited_style) do
      inherited when is_map(inherited) ->
        appearance =
          Enum.reduce(declarations, :auto, fn declaration, appearance ->
            declaration =
              case declaration do
                %{declaration: declaration} -> declaration
                declaration -> declaration
              end

            case declaration do
              {"appearance", value} ->
                case HtmlValidator.parse_control_appearance(value) do
                  {:ok, value} -> value
                  _ -> appearance
                end

              {"appearance", value, :important} ->
                case HtmlValidator.parse_control_appearance(value) do
                  {:ok, value} -> value
                  _ -> appearance
                end

              _ ->
                appearance
            end
          end)

        if appearance == :base_select do
          size = inherited.font_size
          listbox? = style._select_state.listbox

          defaults = %{
            display: if(listbox?, do: :inline_block, else: :inline_flex),
            form_kind: if(listbox?, do: :listbox, else: :base_select),
            _base_select: true,
            background_color: nil,
            border_widths: NativeElixirPdfUtilities.Validators.CssValueValidator.edges(0.75),
            border_styles: NativeElixirPdfUtilities.Validators.CssValueValidator.edges(:solid),
            border_color: :current_color,
            border_colors:
              NativeElixirPdfUtilities.Validators.CssValueValidator.edges(:current_color),
            border_radius: if(listbox?, do: 0, else: size * 0.5),
            padding:
              NativeElixirPdfUtilities.Validators.CssValueValidator.edges(
                size * 0.25,
                size * 0.5,
                size * 0.25,
                size * 0.5
              ),
            min_width: 18.0,
            min_height: max(18.0, inherited.line_height),
            width: :auto,
            height:
              if(listbox?,
                do: max(18.0, inherited.line_height) * style._select_state.rows,
                else: :auto
              ),
            row_gap: size * 0.5,
            column_gap: size * 0.5,
            flex_direction: :row,
            flex_wrap: :nowrap,
            justify_content: :flex_start,
            align_items: :stretch,
            align_content: :stretch,
            overflow: if(listbox?, do: :auto, else: :visible)
          }

          style
          |> Map.merge(defaults)
          |> Map.merge(
            Map.take(inherited, [
              :font_families,
              :font_family,
              :font_size,
              :font_weight,
              :font_style,
              :font_face,
              :line_height,
              :line_height_normal,
              :line_height_multiplier,
              :_font_family_scope
            ])
          )
          |> Map.delete(:_select_inherited_style)
        else
          Map.delete(style, :_select_inherited_style)
        end

      _ ->
        style
    end
  end

  @doc false
  @spec required?([map()]) :: boolean()
  def required?(nodes) do
    Enum.any?(nodes, fn node ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
      Map.get(node, :tag) == "selectedcontent" or required?(Map.get(node, :children, []))
    end)
  end

  @doc false
  @spec reserve_authored_nodes(map()) :: :ok
  def reserve_authored_nodes(index) do
    # Parsed sources have already charged these nodes. Direct tree composition
    # must account for them before adding clones under the same global limit.
    budget = Process.get({HtmlValidator, :render_budget}, %{})
    used = Map.get(budget, :max_html_nodes, 0)

    HtmlValidator.reserve_render_resource(
      :max_html_nodes,
      max(map_size(index) - 1 - used, 0),
      :html
    )
  end

  @doc false
  @spec logical_index(map()) :: map()
  def logical_index(dom), do: index_nodes([dom], %{}, true)

  @doc false
  @spec prepared_index([map()]) :: map()
  def prepared_index(nodes), do: index_nodes(nodes, %{}, false)

  defp index_nodes(nodes, index, content?) do
    Enum.reduce(nodes, index, fn node, index ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
      key = Map.get_lazy(node, :node_id, fn -> Map.get_lazy(node, :_selector_id, &make_ref/0) end)
      index = Map.put(index, key, node)
      index = index_nodes(Map.get(node, :children, []), index, content?)

      case {content?, Map.get(node, :content)} do
        {true, content} when is_map(content) -> index_nodes([content], index, true)
        _ -> index
      end
    end)
  end

  @doc false
  @spec selected_content_plan(map(), [map()], map(), map()) :: map()
  def selected_content_plan(node, projected_ancestors, logical, prepared) do
    ancestors =
      if Map.has_key?(node, :parent_id),
        do: logical_ancestors(node.parent_id, logical, []),
        else: projected_ancestors

    selects = Enum.filter(ancestors, &(Map.get(&1, :tag) == "select"))

    disabled? =
      length(selects) > 1 or
        Enum.any?(ancestors, &(Map.get(&1, :tag) in ["option", "selectedcontent"]))

    select = List.first(selects)

    select =
      if select, do: Map.get(prepared, Map.get(select, :node_id, Map.get(select, :_selector_id)))

    selection = if select, do: get_in(select, [:_initial_form_state, :selection])

    option =
      if selection do
        HtmlValidator.select_options(select.children)
        |> Enum.find(&(Map.get(&1, :_selector_id) == selection.id))
      end

    %{
      disabled: disabled?,
      update: not disabled? and not is_nil(selection) and not selection.multiple,
      option: option
    }
  end

  defp logical_ancestors(parent, index, ancestors) do
    case Map.get(index, parent) do
      nil ->
        Enum.reverse(ancestors)

      node ->
        HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :html)
        logical_ancestors(Map.get(node, :parent_id), index, [node | ancestors])
    end
  end
end
