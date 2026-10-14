defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgSourceAdmissionTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Validators.{
    SvgConverterValidator,
    SvgSceneValidator,
    SvgFilterValidator,
    SvgForeignObjectValidator
  }

  test "actual filter source tags retain format admission without a filter execution bridge" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='4'><defs><filter id='color'><feFlood flood-color='red'/><feMerge><feMergeNode in='SourceGraphic'/></feMerge></filter></defs><rect width='8' height='4' filter='url(#color)'/></svg>"

    assert {:ok, document} = SvgConverterValidator.prepare_extraction(source)
    assert {:ok, root} = SvgSceneValidator.extraction_root(document)

    for {node, _} <- SvgSceneValidator.tree_nodes(root) do
      assert :ok = SvgSceneValidator.validate_node(node)
    end

    assert Enum.all?(~w(filter feFlood feMerge feMergeNode), &(&1 in SvgFilterValidator.tags()))
  end

  test "foreignObject layout applicability uses the actual parsed ancestor context only" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg'><foreignObject width='8' height='4'><div xmlns='http://www.w3.org/1999/xhtml'>Visible source</div></foreignObject><defs><foreignObject width='8' height='4'><div xmlns='http://www.w3.org/1999/xhtml'>Definition source</div></foreignObject></defs></svg>"

    assert {:ok, document} = SvgConverterValidator.prepare_extraction(source)
    assert {:ok, root} = SvgSceneValidator.extraction_root(document)
    nodes = SvgSceneValidator.tree_nodes(root)

    [{active, active_ancestors}, {inert, inert_ancestors}] =
      Enum.filter(nodes, fn {node, _} -> node.tag == "foreignObject" end)

    assert SvgForeignObjectValidator.layout_applicable?(active, active_ancestors)
    refute SvgForeignObjectValidator.layout_applicable?(inert, inert_ancestors)
    assert SvgForeignObjectValidator.layout_applicable?(root, [])
  end
end
