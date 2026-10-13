defmodule NativeElixirPdfUtilities.Validators.SvgTextPathValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  @geometry ~w(path rect circle ellipse line polyline polygon)

  @doc false
  @spec validate_tree(map()) :: :ok | {:error, {atom(), map()}}
  def validate_tree(root) do
    nodes = source_nodes(root, [])

    ids =
      Map.new(nodes, fn {node, parents} -> {Map.get(node.attributes, "id"), {node, parents}} end)

    Enum.reduce_while(nodes, :ok, fn {node, parents}, :ok ->
      result =
        cond do
          Map.has_key?(node.attributes, "startOffset") and node.tag != "textPath" ->
            failure(
              :invalid_document,
              "SVG startOffset belongs to textPath",
              node.tag <> " startOffset"
            )

          node.tag != "textPath" ->
            :ok

          Map.get(List.first(parents) || %{}, :tag) != "text" ->
            failure(:invalid_document, "SVG textPath requires a direct text parent", "textPath")

          Enum.any?(node.children, fn child ->
            Map.get(child, :type) == :element and child.tag not in ~w(tspan title desc)
          end) ->
            failure(
              :invalid_document,
              "SVG textPath children require tspan or descriptive metadata",
              "textPath"
            )

          Map.has_key?(node.attributes, "path") or
              Enum.any?([{"method", "align"}, {"spacing", "exact"}, {"side", "left"}], fn {key,
                                                                                           default} ->
                Map.get(node.attributes, key, default) != default
              end) ->
            failure(
              :unsupported_html,
              "SVG textPath path/method/spacing/side overrides require complete path-layout preparation",
              "textPath"
            )

          Enum.any?(~w(textLength lengthAdjust), &Map.has_key?(node.attributes, &1)) ->
            failure(
              :unsupported_html,
              "SVG textPath length adjustment requires complete path length distribution",
              "textPath"
            )

          true ->
            href = Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", ""))

            case href do
              "#" <> id when id != "" ->
                case Map.get(ids, id) do
                  {%{tag: tag} = target, ancestors} when tag in @geometry ->
                    # SVG2 resolves referenced path coordinates in the current
                    # text coordinate system. Target ancestor transforms do not
                    # participate; only the target's own transform applies.
                    if (tag != "path" and Enum.any?(ancestors, &(&1 != root and &1.tag == "svg"))) or
                         Map.has_key?(target.attributes, "pathLength") do
                      failure(
                        :unsupported_html,
                        "SVG textPath basic-shape nested viewports and pathLength calibration require reference-coordinate preparation",
                        "textPath href"
                      )
                    else
                      :ok
                    end

                  _ ->
                    failure(
                      :invalid_document,
                      "SVG textPath target must identify same-scene static path geometry",
                      "textPath href"
                    )
                end

              _ ->
                failure(
                  :invalid_document,
                  "SVG textPath requires an explicit same-scene #id reference",
                  "textPath href"
                )
            end
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  # Keep actual ancestor nodes for reference-coordinate checks; tag-only ancestry
  # cannot establish whether the authored reference transform is preserved.
  defp source_nodes(node, parents) do
    [
      {node, parents}
      | Enum.flat_map(node.children, fn child ->
          if Map.get(child, :type) == :element,
            do: source_nodes(child, [node | parents]),
            else: []
        end)
    ]
  end

  defp failure(reason, message, source) do
    Diagnostics.error(:layout, reason, message,
      source: source,
      module: __MODULE__,
      operation: :prepare
    )
  end
end
