defmodule NativeElixirPdfUtilities.Validators.SvgReferenceValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    SvgSceneValidator,
    SvgViewportValidator,
    SvgTransformValidator
  }

  @targets ~w(symbol svg use g path rect circle ellipse line polyline polygon)

  @doc false
  @spec referenced_ids(map()) :: MapSet.t()
  def referenced_ids(root) do
    root
    |> SvgSceneValidator.tree_nodes()
    |> Enum.reduce(MapSet.new(), fn {node, _}, ids ->
      if node.tag == "use" do
        case Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", "")) do
          "#" <> id -> MapSet.put(ids, id)
          _ -> ids
        end
      else
        ids
      end
    end)
  end

  @doc false
  @spec validate_summary(map()) :: :ok | {:error, {atom(), map()}}
  def validate_summary(summary) do
    if summary.context.mode == :inline,
      do: validate_tree(SvgSceneValidator.summary_tree(summary)),
      else: :ok
  end

  @doc false
  @spec validate_tree(map()) :: :ok | {:error, {atom(), map()}}
  def validate_tree(root) do
    nodes = SvgSceneValidator.tree_nodes(root)
    ids = Map.new(nodes, fn {node, _} -> {Map.get(node.attributes, "id"), node.tag} end)

    Enum.reduce_while(nodes, :ok, fn {node, ancestors}, :ok ->
      href = Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", ""))

      artwork =
        Enum.filter(
          node.children,
          &(Map.get(&1, :type) == :element and &1.tag not in ~w(title desc))
        )

      result =
        cond do
          node.tag in ~w(use symbol) and not Map.has_key?(node, :style) and
              Map.get(node.attributes, "style", "") != "" ->
            invalid("SVG instance CSS requires host cascade preparation", node.tag)

          node.tag in ~w(use symbol) and
              SvgTransformValidator.parse(Map.get(node.attributes, "transform", "none")) == :error ->
            invalid("SVG instances require a valid bounded affine transform", node.tag)

          node.tag == "symbol" and List.first(ancestors) != "defs" ->
            invalid("SVG symbols currently require a direct defs parent", "symbol")

          node.tag == "symbol" ->
            with {:ok, _} <- SvgViewportValidator.view_box(Map.get(node.attributes, "viewBox")),
                 {:ok, _} <-
                   SvgViewportValidator.aspect_ratio(
                     Map.get(node.attributes, "preserveAspectRatio")
                   ) do
              :ok
            else
              _ ->
                invalid(
                  "SVG symbol requires valid viewBox and preserveAspectRatio geometry",
                  "symbol"
                )
            end

          node.tag == "use" and artwork != [] ->
            invalid("SVG use accepts descriptive metadata, not authored instance artwork", "use")

          node.tag == "use" ->
            case href do
              "#" <> id ->
                if Map.get(ids, id) in @targets,
                  do: :ok,
                  else:
                    invalid(
                      "SVG use target ##{id} must identify same-scene static geometry or a symbol",
                      "use"
                    )

              _ ->
                invalid("SVG use requires an explicit same-scene #id reference", "use")
            end

          true ->
            :ok
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp invalid(message, source) do
    Diagnostics.error(:layout, :invalid_document, message,
      source: source,
      module: __MODULE__,
      operation: :prepare
    )
  end
end
