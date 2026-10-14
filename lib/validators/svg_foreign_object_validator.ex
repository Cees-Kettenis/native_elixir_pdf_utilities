defmodule NativeElixirPdfUtilities.Validators.SvgForeignObjectValidator do
  @moduledoc false

  @doc "Determines HTML layout applicability from the authored SVG ancestor context."
  @spec layout_applicable?(map(), [String.t()]) :: boolean()
  def layout_applicable?(node, ancestors) do
    node.tag != "foreignObject" or
      not Enum.any?(
        ancestors,
        &(&1 in ~w(defs symbol pattern marker mask clipPath filter linearGradient radialGradient))
      )
  end
end
