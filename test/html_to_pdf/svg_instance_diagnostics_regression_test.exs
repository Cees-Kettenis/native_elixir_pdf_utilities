defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgInstanceDiagnosticsRegressionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgBackend}

  alias NativeElixirPdfUtilities.Validators.{
    SvgClipValidator,
    SvgPaintValidator,
    SvgReferenceValidator
  }

  test "invalid raw paint values reach the shared paint diagnostic rather than disappear" do
    for property <- ["fill", "stroke"] do
      source =
        "<svg width='20' height='20'><rect width='10' height='10' #{property}='unrecognized-paint' /></svg>"

      assert {:error, {:invalid_document, diagnostic}} =
               SvgBackend.vectorize(source, [width: 20, height: 20], %{mode: :inline})

      assert diagnostic.stage == :layout
      assert diagnostic.message =~ "supported color or local gradient reference"
      assert diagnostic.module == SvgPaintValidator
      assert diagnostic.operation == :prepare
    end

    node = %{type: :element, tag: "rect", attributes: %{"fill" => "bogus"}, children: []}

    assert {:error, {:invalid_document, %{module: SvgPaintValidator}}} =
             SvgPaintValidator.validate_tree(node)
  end

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
    assert {:error, {:invalid_document, diagnostic}} = SvgReferenceValidator.validate_tree(root)
    assert diagnostic.source == "use"
    assert diagnostic.message =~ "same-scene #id"
  end

  test "defs reject artwork outside the complete supported static definition profile" do
    node = %{
      type: :element,
      tag: "defs",
      attributes: %{},
      children: [
        %{type: :element, tag: "image", attributes: %{}, children: []}
      ]
    }

    assert {:error, {:unsupported_html, diagnostic}} = SvgClipValidator.validate_tree(node)
    assert diagnostic.stage == :layout
    assert diagnostic.source == "defs"
    assert diagnostic.message =~ "supported static geometry"
    assert diagnostic.module == SvgClipValidator
  end

  test "explicit inherited reference color and font size survive canonical serialization" do
    html = """
    <svg width=80 height=30 color=red font-size=12>
    <defs><g id=tile style='color:inherit;font-size:inherit'><rect width=1em height=1em fill=currentColor /></g></defs>
    <use href='#tile' color=blue font-size=20 />
    </svg>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    source = svg.style.svg_scene.source
    assert source =~ "color=\"inherit\""
    assert source =~ "font-size=\"inherit\""
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    content = document.objects |> Map.values() |> Enum.map_join(&(Map.get(&1, :stream) || ""))
    assert content =~ "0 0 1 rg"
    assert content =~ "20 20 l"
  end

  test "locally specified relative em font size remains relative within reused geometry" do
    html = """
    <svg width=80 height=30 font-size=10>
    <defs><g id=tile style='font-size:2em'><rect width=1em height=1em /></g></defs>
    <use href='#tile' font-size=10 /><use href='#tile' x=40 font-size=15 />
    </svg>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.compute_detailed(dom)
    source = svg.style.svg_scene.source
    assert source =~ "font-size=\"2em\""
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    content = document.objects |> Map.values() |> Enum.map_join(&(Map.get(&1, :stream) || ""))
    assert content =~ "20 20 l"
    assert content =~ "30 30 l"
  end
end
