defmodule NativeElixirPdfUtilities.HtmlToPdf.CompositionClosureTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlComposition, HtmlParser}
  alias NativeElixirPdfUtilities.Validators.{HtmlCompositionValidator, TemplateExecutionValidator}

  test "composition rejects replacement attributes with an actionable shared diagnostic" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlCompositionValidator.validate_attributes(%{
               "for" => "A",
               "shadowrootmode" => "open"
             })

    assert diagnostic.stage == :html
    assert diagnostic.module == HtmlCompositionValidator
    assert diagnostic.operation == :compose
    assert diagnostic.source == "template"
    assert diagnostic.message =~ "cannot be combined"
  end

  test "processing instructions stay logical and cannot be assigned to an initial slot" do
    source =
      "<div><template shadowrootmode=open><slot>Fallback</slot></template><?marker name='A'></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    [host] = dom.children
    assert [%{tag: "template"}, %{type: :processing_instruction}] = host.children
    assert {:ok, projection} = HtmlComposition.compose(dom)
    assert projection.logical == dom
    assert [%{children: [%{children: [%{text: "Fallback"}]}]}] = projection.composed.children
    assert Map.values(projection.assignments) == [[]]
  end

  test "root collection skips ordinary inert templates before a real shadow host" do
    source =
      "<template><div><template shadowrootmode=open>Inert nested</template></div></template><div><template shadowrootmode=open>Actual shadow</template></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, projection} = HtmlComposition.compose(dom)
    assert map_size(projection.roots) == 1
    [root] = Map.values(projection.roots)
    assert [%{text: "Actual shadow"}] = root.children
    assert projection.logical == dom
  end

  test "original runtime observations require an issued preparation invocation" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p>Original</p>")

    assert {:error, {:invalid_document, diagnostic}} =
             TemplateExecutionValidator.validate_original_runtime_scope(dom, %{}, make_ref())

    assert diagnostic.stage == :html
    assert diagnostic.module == TemplateExecutionValidator
    assert diagnostic.operation == :original_runtime_source
    assert diagnostic.source == "original runtime scope"
    assert diagnostic.message =~ "active original preparation invocation"
  end

  test "unsupported SVG geometry in active shadow fallback propagates through nested composition" do
    source =
      "<div><section><template shadowrootmode=open><slot><svg><rect width='bananas'></rect></svg></slot></template></section></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:error, {:invalid_document, diagnostic}} = HtmlComposition.compose(dom)
    assert diagnostic.stage == :layout
    assert diagnostic.reason == :invalid_document
    assert diagnostic.source =~ "rect"
    assert diagnostic.message != ""
  end
end
