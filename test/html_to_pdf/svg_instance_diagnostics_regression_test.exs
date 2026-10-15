Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgInstanceDiagnosticsRegressionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{SvgReferenceValidator}

  test "reference discovery ignores nonfragments while instance validation diagnoses them" do
    root = %{
      type: :element,
      tag: "svg",
      attributes: %{},
      children: [
        %{type: :element, tag: "use", attributes: %{"href" => "asset.svg#tile"}, children: []},
        %{type: :element, tag: "use", attributes: %{"href" => "#tile"}, children: []}
      ]
    }

    assert SvgReferenceValidator.referenced_ids(root) == MapSet.new(["tile"])
  end

  test "defs admit image structure while missing original resource references retain diagnostics" do
    node = %{
      type: :element,
      tag: "defs",
      attributes: %{},
      children: [%{type: :element, tag: "image", attributes: %{}, children: []}]
    }

    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render("<svg><defs><image /></defs></svg>")

    assert diagnostic.stage == :style
    assert diagnostic.source == "image href"
    assert diagnostic.message =~ "explicit raster or SVG resource reference"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgImageValidator
    _unsupported = put_in(node.children, [%{hd(node.children) | tag: "script"}])
  end

  test "explicit inherited reference color and font size survive canonical serialization" do
    html =
      "<svg width=80 height=30 color=red font-size=12>\n<defs><g id=tile style='color:inherit;font-size:inherit'><rect width=1em height=1em fill=currentColor /></g></defs>\n<use href='#tile' color=blue font-size=20 />\n</svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    source = svg.style.svg_scene.source
    assert source =~ "color=\"inherit\""
    assert source =~ "font-size=\"inherit\""
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  test "locally specified relative em font size remains relative within reused geometry" do
    html =
      "<svg width=80 height=30 font-size=10>\n<defs><g id=tile style='font-size:2em'><rect width=1em height=1em /></g></defs>\n<use href='#tile' font-size=10 /><use href='#tile' x=40 font-size=15 />\n</svg>\n"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    source = svg.style.svg_scene.source
    assert source =~ "font-size=\"2em\""
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end
end
