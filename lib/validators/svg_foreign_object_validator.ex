defmodule NativeElixirPdfUtilities.Validators.SvgForeignObjectValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.{Layout, RenderCache, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    EmbeddedViewportValidator,
    HtmlValidator,
    ResourceValidator,
    SvgSceneValidator
  }

  @doc false
  @spec prepare_tree(map(), keyword()) :: {:ok, map(), map()} | {:error, {atom(), map()}}
  def prepare_tree(root, options) do
    nodes = SvgSceneValidator.tree_nodes(root)
    foreign = Enum.filter(nodes, fn {node, _} -> node.tag == "foreignObject" end)
    context = ResourceContext.current()

    with :ok <- if(foreign == [], do: :ok, else: ResourceValidator.validate_context(context)) do
      authored_ids = MapSet.new(nodes, fn {node, _} -> Map.get(node.attributes, "id") end)

      slots =
        foreign
        |> Enum.with_index()
        |> Map.new(fn {{node, _}, index} ->
          id =
            Enum.find_value(0..MapSet.size(authored_ids), fn suffix ->
              scope = :crypto.hash(:sha256, :erlang.term_to_binary(context.document_id))
              candidate = "NativeHtmlSlot#{Base.encode16(scope, case: :lower)}:#{index}:#{suffix}"
              if MapSet.member?(authored_ids, candidate), do: nil, else: candidate
            end)

          permission = make_ref()

          slot = %{
            permission: permission,
            node: node,
            context: context,
            document: %{type: :document, children: node.children},
            forms: Keyword.get(options, :forms, :interactive)
          }

          RenderCache.fetch(
            context.scope,
            {:svg_foreign_slot, context.document_id, permission},
            fn -> {id, slot} end
          )

          {id, slot}
        end)

      by_node =
        Map.new(slots, fn {id, slot} -> {Map.get(slot.node, :node_id) || slot.node, id} end)

      {:ok, project(root, by_node), slots}
    end
  end

  defp project(node, slots) do
    if node.tag == "foreignObject" do
      id = Map.fetch!(slots, Map.get(node, :node_id) || node)

      node
      |> Map.put(:tag, "rect")
      |> Map.put(:children, [])
      |> Map.update!(:attributes, &Map.put(&1, "id", id))
      |> Map.update!(:style, &Map.merge(&1, %{svg_fill: {:color, {0, 0, 0}}, svg_stroke: :none}))
    else
      Map.update!(node, :children, fn children ->
        Enum.map(children, fn child ->
          if Map.get(child, :type) == :element, do: project(child, slots), else: child
        end)
      end)
    end
  end

  @doc false
  @spec project_surfaces(map(), map()) :: map()
  def project_surfaces(root, slots) do
    by_node = Map.new(slots, fn {id, slot} -> {Map.get(slot.node, :node_id) || slot.node, id} end)
    project_images(root, by_node)
  end

  defp project_images(node, slots) do
    if node.tag == "foreignObject" do
      id = Map.fetch!(slots, Map.get(node, :node_id) || node)

      node
      |> Map.put(:tag, "image")
      |> Map.put(:children, [])
      |> Map.update!(
        :attributes,
        &(Map.put(&1, "id", id)
          |> Map.put("href", "NativeHtmlSurface:" <> id)
          |> Map.put("preserveAspectRatio", "none"))
      )
    else
      Map.update!(node, :children, fn children ->
        Enum.map(children, fn child ->
          if Map.get(child, :type) == :element, do: project_images(child, slots), else: child
        end)
      end)
    end
  end

  @doc false
  @spec validate_slots(map()) :: {:ok, [binary()]} | {:error, {atom(), map()}}
  def validate_slots(slots) do
    case slots do
      slots when is_map(slots) ->
        Enum.reduce_while(slots, {:ok, []}, fn {id, slot}, {:ok, ids} ->
          case slot do
            %{
              context: context,
              node: %{tag: "foreignObject", namespace: :svg},
              document: %{type: :document, children: children},
              forms: forms,
              permission: permission
            }
            when is_binary(id) and is_list(children) and forms in [:static, :interactive] ->
              with :ok <- ResourceValidator.validate_context(context),
                   true <- is_reference(permission),
                   {^id, ^slot} <-
                     RenderCache.fetch(
                       context.scope,
                       {:svg_foreign_slot, context.document_id, permission},
                       fn -> nil end
                     ) do
                {:cont, {:ok, [id | ids]}}
              else
                {:error, _} = error ->
                  {:halt, error}

                _ ->
                  {:halt,
                   Diagnostics.error(
                     :svg,
                     :invalid_document,
                     "SVG HTML slot requires its unchanged issued subtree, fonts, bytes and context",
                     source: id,
                     module: __MODULE__,
                     operation: :prepare
                   )}
              end

            _ ->
              {:halt,
               Diagnostics.error(
                 :svg,
                 :invalid_document,
                 "HTML form slots require their original SVG subtree and issued document context",
                 source: "foreignObject",
                 module: __MODULE__,
                 operation: :prepare
               )}
          end
        end)

      _ ->
        Diagnostics.error(:svg, :invalid_document, "SVG HTML slots require an issued slot map",
          source: "foreignObject",
          module: __MODULE__,
          operation: :prepare
        )
    end
  end

  @doc false
  @spec layout_slot(map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def layout_slot(slot) do
    original = slot.original

    ResourceContext.with_document(original.context, fn ->
      with {:ok, layout} <- Layout.layout_viewport(original.document, slot.width, slot.height),
           {:ok, boxes} <-
             EmbeddedViewportValidator.prepare_paint(
               layout,
               %{forms: original.forms, host: :surface, context: original.context},
               slot.width,
               slot.height
             ) do
        {:ok,
         %{
           type: HtmlValidator.reserve_layout_box(:drawing_group),
           width: slot.width,
           height: slot.height,
           transform: {1, 0, 0, 1, 0, 0},
           clip: %{x: 0, y: 0, width: slot.width, height: slot.height},
           boxes: boxes
         }}
      end
    end)
  end
end
