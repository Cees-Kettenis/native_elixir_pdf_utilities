defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgSourceAdmissionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser

  alias NativeElixirPdfUtilities.Validators.{
    SvgConverterValidator,
    SvgSceneValidator,
    SvgFilterValidator,
    SvgForeignObjectValidator,
    SvgValidator
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

    assert {:ok, document} = HtmlParser.parse_detailed(source)
    assert {:ok, root} = SvgSceneValidator.extraction_root(document)
    nodes = SvgSceneValidator.tree_nodes(root)

    [{active, active_ancestors}, {inert, inert_ancestors}] =
      Enum.filter(nodes, fn {node, _} -> node.tag == "foreignObject" end)

    assert SvgForeignObjectValidator.layout_applicable?(active, active_ancestors)
    refute SvgForeignObjectValidator.layout_applicable?(inert, inert_ancestors)
    assert SvgForeignObjectValidator.layout_applicable?(root, [])
  end

  test "extraction requires an actual parsed document tree" do
    assert {:error, {:invalid_document, diagnostic}} = SvgSceneValidator.extraction_root("junk")

    assert diagnostic.module == SvgSceneValidator
    assert diagnostic.source == "SVG"
    assert diagnostic.message =~ "SVG extraction requires a parsed document"
  end

  test "alignment requires the XML summary and parsed tree to share one structure" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='4' height='4'><rect width='4' height='4'/></svg>"

    assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :image})
    assert {:ok, document} = HtmlParser.parse_detailed(source)
    [svg] = document.children
    [rect] = svg.children

    # Fewer parsed elements than admitted XML nodes leave unaligned sources.
    trimmed = %{document | children: [%{svg | children: []}]}

    assert {:error, {:invalid_document, missing}} =
             SvgSceneValidator.align_xml_source(trimmed, summary)

    assert missing.source == "SVG"

    assert missing.message =~
             "SVG XML source and parsed extraction tree have different structures"

    # Extra parsed elements have no admitted XML node to align against.
    padded = %{document | children: [%{svg | children: [rect, rect]}]}

    assert {:error, {:invalid_document, extra}} =
             SvgSceneValidator.align_xml_source(padded, summary)

    assert extra.message == missing.message
  end
end
