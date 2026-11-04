defmodule NativeElixirPdfUtilities.HtmlToPdf.LegacyApplicabilityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlLegacyMarkupValidator, HtmlMetadataValidator}

  test "body aliases follow authored order and skip malformed hints" do
    for {attributes, horizontal, vertical} <- [
          {"marginwidth=16 marginheight=12 leftmargin=1 topmargin=2 rightmargin=99 bottommargin=99",
           0.75, 1.5},
          {"leftmargin=12 topmargin=14 rightmargin=40 bottommargin=88", 9.0, 10.5},
          {"marginwidth=no marginheight=no leftmargin=0 topmargin=0 rightmargin=0 bottommargin=0",
           0.0, 0.0},
          {"marginwidth=' +4tail' topmargin=0", 3.0, 0.0},
          {"leftmargin=1 topmargin=2 marginwidth=16 marginheight=12", 12.0, 9.0},
          {"leftmargin=1 topmargin=2 marginwidth=no marginheight=no", 0.75, 1.5},
          {"marginwidth=-2 topmargin=-3", 0.0, 0.0}
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed("<body #{attributes}>Visible</body>")
      assert {:ok, %{children: [body]}} = Style.compute_detailed(dom)

      assert body.style.margin == %{
               left: horizontal,
               right: horizontal,
               top: vertical,
               bottom: vertical
             }

      assert body.attributes == hd(dom.children).attributes
    end
  end

  test "inert right and bottom hints preserve complete PDF while author CSS wins" do
    assert {:ok, pdf} = HtmlToPdf.render("<body rightmargin=99 bottommargin=99>Visible</body>")
    assert {:ok, reference} = HtmlToPdf.render("<body>Visible</body>")
    assert pdf == reference

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<body marginwidth=99 marginheight=99 style='margin:3pt'>Visible</body>"
             )

    assert {:ok, %{children: [body]}} = Style.compute_detailed(dom)
    assert body.style.margin == %{left: 3.0, right: 3.0, top: 3.0, bottom: 3.0}
  end

  test "qualified trailing aliases cannot overwrite authored unqualified margins" do
    node = %{
      type: :element,
      tag: "body",
      attributes: %{"marginwidth" => "16", "leftmargin" => "1", "topmargin" => "2"},
      attribute_order: ["marginwidth", "leftmargin", "topmargin"],
      attribute_namespaces: %{"leftmargin" => "urn:other"},
      children: []
    }

    assert {:ok, %{children: [body]}} =
             Style.compute_detailed(%{type: :document, children: [node]})

    assert body.attributes == node.attributes
    assert body.attribute_order == node.attribute_order
    assert body.style.margin == %{left: 12.0, right: 12.0, top: 1.5, bottom: 1.5}
  end

  test "body language and translation inherit through qualified metadata without changing source" do
    body = %{
      type: :element,
      tag: "body",
      attributes: %{"lang" => "fr", "translate" => "no"},
      attribute_namespaces: %{"lang" => "urn:other", "translate" => "urn:other"},
      children: [%{type: :element, tag: "p", attributes: %{}, children: []}]
    }

    html = %{
      type: :element,
      tag: "html",
      attributes: %{"lang" => "en"},
      children: [body]
    }

    assert {:ok, %{children: [styled]}} =
             Style.compute_detailed(%{type: :document, children: [html]},
               stylesheets: [{:css, "p:lang(en){color:blue}p:lang(fr){color:red}"}]
             )

    assert styled.attributes == body.attributes
    assert HtmlMetadataValidator.translation_mode(styled, true)
    assert HtmlMetadataValidator.translation_mode(hd(styled.children), true)
    assert hd(styled.children).style.color == {0.0, 0.0, 1.0}
  end

  test "authored body axes retain separately validated frame defaults on the absent axis" do
    base = %{left: 0.0, right: 0.0, top: 2.0, bottom: 2.0}
    node = %{tag: "body", attributes: %{"marginheight" => "12"}}

    assert HtmlLegacyMarkupValidator.presentation_hints(node, base).margin == %{
             left: 0.0,
             right: 0.0,
             top: 9.0,
             bottom: 9.0
           }

    node =
      Map.merge(node, %{
        attributes: %{"marginwidth" => "invalid", "leftmargin" => "1"},
        attribute_order: ["leftmargin", "marginwidth"]
      })

    assert HtmlLegacyMarkupValidator.presentation_hints(node, base).margin == %{
             left: 0.75,
             right: 0.75,
             top: 2.0,
             bottom: 2.0
           }

    assert HtmlLegacyMarkupValidator.presentation_hints(%{node | attributes: %{}}, base) == %{}
  end

  test "qualified font body and dialog metadata remain source-visible but do not select hints or resources" do
    nodes = [
      %{
        type: :element,
        tag: "font",
        attributes: %{"face" => "Unavailable family", "color" => "red", "size" => "7"},
        attribute_namespaces: %{
          "face" => "urn:other",
          "color" => "urn:other",
          "size" => "urn:other"
        },
        children: [%{type: :text, text: "Font source"}]
      },
      %{
        type: :element,
        tag: "body",
        attributes: %{
          "text" => "red",
          "bgcolor" => "red",
          "link" => "red",
          "background" => "javascript:ignored",
          "marginwidth" => "99"
        },
        attribute_namespaces: %{
          "text" => "urn:other",
          "bgcolor" => "urn:other",
          "link" => "urn:other",
          "background" => "urn:other",
          "marginwidth" => "urn:other"
        },
        children: [%{type: :text, text: "Body source"}]
      },
      %{
        type: :element,
        tag: "dialog",
        attributes: %{"open" => "false"},
        attribute_namespaces: %{"open" => "urn:other"},
        children: [%{type: :text, text: "Dialog source"}]
      }
    ]

    for node <- nodes do
      assert HtmlMetadataValidator.unqualified_attributes(node) == %{}
      assert HtmlLegacyMarkupValidator.presentation_hints(node) == %{}

      assert {:ok, %{children: [styled]}} =
               Style.compute_detailed(%{type: :document, children: [node]},
                 asset_resolver: fn request ->
                   flunk("qualified source fetched #{inspect(request)}")
                 end
               )

      assert styled.attributes == node.attributes

      if node.tag == "dialog" do
        assert styled.style.display == :none
        assert styled.children == []
      else
        assert styled.style.color == {0, 0, 0}
        assert styled.style.font_size == 12.0
        refute Map.has_key?(styled.style, :background_image_source)
        assert hd(styled.children).text in ["Font source", "Body source"]
      end
    end
  end

  test "active body URLs and malformed namespace maps retain actionable diagnostics" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<body background='javascript:active'>Visible</body>")

    assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
    assert diagnostic.source == "background" and diagnostic.message =~ "Active URL"

    node = %{
      type: :element,
      tag: "font",
      attributes: %{"color" => "red"},
      attribute_namespaces: nil,
      children: []
    }

    assert {:error, {:invalid_document, diagnostic}} =
             Style.compute_detailed(%{type: :document, children: [node]})

    assert diagnostic.stage == :style and diagnostic.source == "font"
    assert diagnostic.message =~ "attribute_namespaces"
  end

  test "duplicate body hints retain the existing attribute diagnostic instead of inventing order" do
    source = "<body leftmargin=16 leftmargin=1>Visible</body>"
    assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(source)
    assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
    assert diagnostic.source == "<body leftmargin=16 leftmargin=1>"
    assert diagnostic.line == 1 and diagnostic.column == 1
    assert diagnostic.message =~ "duplicate"
  end
end
