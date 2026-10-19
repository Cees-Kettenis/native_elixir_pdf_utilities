defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlAttributeOrderTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, HtmlTree, Style}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlCssSourceValidator,
    HtmlTreeValidator,
    HtmlValidator
  }

  test "validated original attribute order survives namespace normalization and original source" do
    source =
      "<body CLASS='original' marginwidth=16 marginheight=12 leftmargin=1 topmargin=2 data-note='x > y'><p>Text</p></body>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
    body = dom.children |> hd() |> Map.fetch!(:children) |> Enum.find(&(&1.tag == "body"))

    assert body.attribute_order ==
             ~w(class marginwidth marginheight leftmargin topmargin data-note)

    assert body.attributes["class"] == "original" and body.attributes["data-note"] == "x > y"
    assert body.source.text == hd(String.split(source, "<p>"))
    assert :ok = HtmlTreeValidator.validate_transport(dom)
    assert {:ok, prepared} = HtmlCssSourceValidator.prepare(dom)
    assert prepared.token_bindings[body.node_id].raw == body.source.text
    tree = HtmlTree.from_document(dom)
    assert HtmlTree.fetch(tree, body.node_id).attribute_order == body.attribute_order

    assert {:ok, svg} =
             HtmlParser.parse_fragment_tree_detailed(
               "<svg viewBox='0 0 1 1' preserveAspectRatio='none'><rect width='1' height='1'/></svg>",
               "div"
             )

    [root] = svg.children
    assert root.attribute_order == ["viewBox", "preserveAspectRatio"]
    assert :ok = HtmlTreeValidator.validate_transport(svg)
  end

  test "reversed body source order remains a factual difference without changing attribute values" do
    sources = [
      "<body marginwidth=16 leftmargin=1><p>A</p></body>",
      "<body leftmargin=1 marginwidth=16><p>A</p></body>"
    ]

    [first, second] =
      Enum.map(sources, fn source ->
        {:ok, dom} = HtmlParser.parse_document_detailed(source)
        dom.children |> hd() |> Map.fetch!(:children) |> Enum.find(&(&1.tag == "body"))
      end)

    assert first.attributes == second.attributes
    assert first.attribute_order == ["marginwidth", "leftmargin"]
    assert second.attribute_order == ["leftmargin", "marginwidth"]
  end

  test "caller attribute order must describe the complete unique validated key set" do
    for order <- [
          ["leftmargin"],
          ["leftmargin", "leftmargin"],
          ["marginwidth", "bogus"],
          nil,
          [123, "marginwidth"]
        ] do
      body = %{
        type: :element,
        tag: "body",
        attributes: %{"marginwidth" => "16", "leftmargin" => "1"},
        attribute_order: order,
        children: []
      }

      assert {:error, {:invalid_document, diagnostic}} =
               Style.compute_detailed(%{type: :document, children: [body]},
                 system_font_discovery: false
               )

      assert diagnostic.stage == :style and diagnostic.operation == :compute
      assert diagnostic.source == "attribute_order" and diagnostic.message =~ "exactly once"
    end

    assert :ok =
             HtmlValidator.validate_attribute_order(%{
               type: :element,
               attributes: %{"a" => "x"},
               attribute_order: ["a"]
             })

    assert :ok =
             HtmlValidator.validate_attribute_order(%{type: :element, attributes: %{"a" => "x"}})

    assert {:error, {:invalid_document, _}} =
             HtmlValidator.validate_attribute_order(%{
               type: :element,
               attributes: %{"a" => "x"},
               attribute_order: nil,
               source: :malformed
             })
  end

  test "canonical source order cannot contradict the original opening token" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<body marginwidth=16 leftmargin=1><p>A</p></body>"
             )

    [html] = dom.children

    children =
      Enum.map(html.children, fn node ->
        if node.tag == "body",
          do: %{node | attribute_order: Enum.reverse(node.attribute_order)},
          else: node
      end)

    changed = %{dom | children: [%{html | children: children}]}
    assert {:error, {:invalid_document, diagnostic}} = HtmlCssSourceValidator.prepare(changed)
    assert diagnostic.stage == :html and diagnostic.operation == :source_style_contexts
    assert diagnostic.source == "<body marginwidth=16 leftmargin=1>"
  end
end
