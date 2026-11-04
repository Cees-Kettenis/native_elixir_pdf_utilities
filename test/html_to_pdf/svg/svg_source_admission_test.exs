defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgSourceAdmissionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser

  alias NativeElixirPdfUtilities.Validators.{
    SvgConverterValidator,
    SvgSceneValidator,
    SvgFilterValidator,
    SvgForeignObjectValidator
  }

  test "filter source tags retain format admission without a filter execution bridge" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='8' height='4'><defs><filter id='color'><feFlood flood-color='red'/><feMerge><feMergeNode in='SourceGraphic'/></feMerge></filter></defs><rect width='8' height='4' filter='url(#color)'/></svg>"

    assert {:ok, document} = SvgConverterValidator.prepare_extraction(source)
    assert {:ok, root} = SvgSceneValidator.extraction_root(document)

    for {node, _} <- SvgSceneValidator.tree_nodes(root) do
      assert :ok = SvgSceneValidator.validate_node(node)
    end

    assert Enum.all?(~w(filter feFlood feMerge feMergeNode), &(&1 in SvgFilterValidator.tags()))
  end

  test "foreignObject layout applicability uses the parsed ancestor context only" do
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

  test "extraction requires an parsed document tree" do
    assert {:error, {:invalid_document, diagnostic}} = SvgSceneValidator.extraction_root("junk")

    assert diagnostic.module == SvgSceneValidator
    assert diagnostic.source == "SVG"
    assert diagnostic.message =~ "SVG extraction requires a parsed document"

    for children <- [[], [%{type: :element, tag: "g", namespace: :svg}]] do
      assert {:error, {:invalid_document, root}} =
               SvgSceneValidator.extraction_root(%{children: children})

      assert root.source == "SVG" and root.message =~ "exactly one complete SVG document"
    end
  end

  test "canonical XML retains case-distinct attributes, prefixed names and original document identity" do
    source = """
    <s:svg xmlns:s="http://www.w3.org/2000/svg" xmlns:q="http://www.w3.org/1999/xlink"
      width="8" WIDTH="99" height="4" viewBox="0 0 8 4" viewbox="invalid">
      <s:defs><s:rect id="tile" width="4" height="4" fill="red" FILL="blue"/></s:defs>
      <s:use q:href="#tile"/>
    </s:svg>
    """

    assert {:ok, document, summary} = SvgConverterValidator.prepare_extraction_details(source)
    assert document.source.text == source
    assert {:ok, root} = SvgSceneValidator.extraction_root(document)
    assert root.attributes["width"] == "8" and root.attributes["WIDTH"] == "99"
    assert root.xml_name == "s:svg"
    assert summary.root["width"] == "8" and summary.root["viewbox"] == "0 0 8 4"
    nodes = SvgSceneValidator.tree_nodes(root)
    assert length(Enum.uniq_by(nodes, fn {node, _} -> node.node_id end)) == 4

    assert Enum.all?(nodes, fn {node, _} ->
             node.document_id == document.document_id and
               node.tree_scope_id == document.tree_scope_id
           end)

    {use, _} = Enum.find(nodes, fn {node, _} -> node.tag == "use" end)
    assert use.attribute_namespaces["q:href"].namespace == "http://www.w3.org/1999/xlink"
    assert use.attributes["q:href"] == "#tile"

    assert {:ok, request} = SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
    assert request.options == [width: 8, height: 4]
    assert request.source =~ "<s:svg " and request.source =~ "<s:use "
    assert request.source =~ ~s(WIDTH="99") and request.source =~ ~s(FILL="blue")
    assert request.source =~ ~s(q:href="#tile")

    assert {:ok, png} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(source, [], nil)

    assert {:ok, expected} =
             Resvg.svg_string_to_png_binary(source,
               skip_system_fonts: true,
               resources_dir: System.tmp_dir!()
             )

    assert png == expected
  end

  test "XML case rules remain distinct from inline HTML duplicate attribute admission" do
    source = "<svg width='8' WIDTH='99' height='4'><rect width='8' height='4'/></svg>"
    assert {:ok, _} = SvgConverterValidator.prepare_extraction(source)
    assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(source)
    assert diagnostic.message =~ "unsupported"

    assert {:error, {:invalid_document, diagnostic}} =
             SvgConverterValidator.prepare_extraction("<svg width='8' width='9'/>")

    assert diagnostic.stage == :style and diagnostic.source == "SVG"
    assert diagnostic.line == 1
  end

  test "XML text, CDATA and character references retain order without HTML entity decoding" do
    source = "<svg width='8' height='4'><title>one<![CDATA[<two>]]>&amp;&#x3a9;</title></svg>"
    assert {:ok, document} = SvgConverterValidator.prepare_extraction(source)
    assert {:ok, root} = SvgSceneValidator.extraction_root(document)
    [title] = root.children
    assert Enum.map_join(title.children, & &1.text) == "one<two>&Ω"

    assert NativeElixirPdfUtilities.HtmlToPdf.SvgScene.authored_source(root) =~
             "one&lt;two&gt;&amp;Ω"
  end

  test "attributes in unrelated XML namespaces stay inert even when their local names match SVG" do
    source = """
    <svg xmlns:q="urn:metadata" xmlns:xlink="urn:metadata" width="8" height="4" q:href="https://unused.invalid">
      <rect q:href="#missing" q:fill="url(https://unused.invalid)" width="8" height="4" fill="red"/>
    </svg>
    """

    assert {:ok, document} = SvgConverterValidator.prepare_extraction(source)
    assert {:ok, root} = SvgSceneValidator.extraction_root(document)
    assert root.attributes["q:href"] == "https://unused.invalid"
    assert {:ok, request} = SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
    assert request.source =~ ~s|q:fill="url(https://unused.invalid)"|

    assert NativeElixirPdfUtilities.Validators.SvgReferenceValidator.href_attribute(%{
             attributes: %{"xlink:href" => "ignored"},
             attribute_namespaces: %{
               "xlink:href" => %{namespace: "urn:metadata", local_name: "href"}
             }
           }) == "href"
  end

  test "style projection preserves authored namespace bindings instead of activating metadata hrefs" do
    source = """
    <s:svg xmlns:s="http://www.w3.org/2000/svg" xmlns="urn:metadata" xmlns:xlink="urn:metadata" width="8" height="4">
      <s:rect width="8" height="4" fill="red"/><s:use xlink:href="#missing"/>
    </s:svg>
    """

    assert {:ok, scene} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgScene.extract(source, nil,
               system_font_discovery: false
             )

    assert scene.source =~ ~s(xmlns="urn:metadata")
    assert scene.source =~ ~s(xmlns:xlink="urn:metadata")
    assert {:ok, %{image: image}} = NativeElixirPdfUtilities.HtmlToPdf.SvgScene.paint(scene, 6)
    assert image.data == :binary.copy(<<255, 0, 0>>, image.width_px * image.height_px)
  end

  test "XML admission reports original malformed source before converter viewport rewriting" do
    source = "<svg width='8' height='4'>\n<rect></svg>"

    assert {:error, {:invalid_document, diagnostic}} =
             SvgConverterValidator.with_document_viewport(source, {8, 4}, {20, 20}, %{
               mode: :image
             })

    assert diagnostic.stage == :style and diagnostic.source == "SVG" and diagnostic.line == 2
    assert diagnostic.message =~ "SVG XML is malformed"
  end
end
