defmodule NativeElixirPdfUtilities.Validators.SvgClipValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    SvgSceneValidator,
    SvgStyleValidator,
    SvgTransformValidator
  }

  @shapes ~w(path rect circle ellipse polyline polygon)

  @doc false
  @spec validate_summary(map()) :: :ok | {:error, {atom(), map()}}
  def validate_summary(summary) do
    if summary.context.mode == :inline do
      validate_tree(SvgSceneValidator.summary_tree(summary))
    else
      :ok
    end
  end

  # Both the styled host tree and the byte backend use this same clip profile.
  @doc false
  @spec validate_tree(map()) :: :ok | {:error, {atom(), map()}}
  def validate_tree(root) do
    nodes = SvgSceneValidator.tree_nodes(root)
    ids = Map.new(nodes, fn {node, _} -> {Map.get(node.attributes, "id"), node.tag} end)

    Enum.reduce_while(nodes, :ok, fn {node, ancestors}, :ok ->
      clip =
        case Map.get(node, :style) do
          nil ->
            SvgStyleValidator.parse("clip-path", Map.get(node.attributes, "clip-path", "none"))

          style ->
            {:ok, Map.get(style, :svg_clip_path, :none)}
        end

      children = Enum.filter(node.children, &(Map.get(&1, :type) == :element))
      geometry = Enum.reject(children, &(&1.tag in ~w(title desc)))

      structure = SvgSceneValidator.validate_structure(node)

      result =
        cond do
          structure != :ok ->
            structure

          not Map.has_key?(node, :style) and
              Regex.match?(~r/(?:^|;)\s*clip-path\s*:/i, Map.get(node.attributes, "style", "")) ->
            unsupported("inline SVG clip CSS requires host cascade preparation", node.tag)

          (node.tag == "clipPath" or "clipPath" in ancestors) and
              SvgTransformValidator.parse(Map.get(node.attributes, "transform", "none")) == :error ->
            invalid("SVG clip geometry requires a valid bounded affine transform", node.tag)

          clip == :error ->
            invalid("clip-path requires none or a same-document url(#id)", node.tag)

          node.tag == "defs" and
              Enum.any?(
                geometry,
                &(&1.tag not in ~w(clipPath pattern marker mask filter symbol use svg g path rect circle ellipse line polyline polygon text linearGradient radialGradient))
              ) ->
            unsupported(
              "SVG defs requires supported static geometry, symbols or bounded paint/clip definitions",
              "defs"
            )

          node.tag == "clipPath" and
              (List.first(ancestors) != "defs" or Map.get(node.attributes, "id", "") == "") ->
            invalid("SVG clipPath requires an id and a direct defs parent", "clipPath")

          node.tag == "clipPath" and
              Map.get(node.attributes, "clipPathUnits", "userSpaceOnUse") != "userSpaceOnUse" ->
            unsupported("SVG clipPath currently requires userSpaceOnUse coordinates", "clipPath")

          node.tag == "clipPath" and
              (length(geometry) != 1 or hd(geometry).tag not in @shapes) ->
            unsupported("SVG clipPath requires exactly one static geometry child", "clipPath")

          clip != {:ok, :none} and
              (node.tag in ~w(defs clipPath) or "clipPath" in ancestors) ->
            unsupported(
              "linked or nested SVG clip definitions require complete clip composition",
              node.tag
            )

          clip != {:ok, :none} and node.tag == "svg" and ancestors == [] ->
            unsupported(
              "outer SVG clipping requires unified host box and annotation composition",
              "svg"
            )

          true ->
            case clip do
              {:ok, {:fragment, id}} ->
                if Map.get(ids, id) == "clipPath",
                  do: :ok,
                  else:
                    invalid(
                      "SVG clip-path target ##{id} must identify a clipPath in this scene",
                      node.tag
                    )

              {:ok, :none} ->
                :ok
            end
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

  defp unsupported(message, source) do
    Diagnostics.error(:layout, :unsupported_html, message,
      source: source,
      module: __MODULE__,
      operation: :prepare
    )
  end
end
