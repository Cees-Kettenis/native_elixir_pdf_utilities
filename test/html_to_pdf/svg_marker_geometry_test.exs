defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgMarkerGeometryTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{SvgStyleValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "marker shorthand keeps canonical longhand precedence and real context paint" do
    assert {:ok, {:fragment, "m"}} = SvgStyleValidator.parse("marker", "url(#m)")
    assert {:ok, "context-stroke"} = SvgStyleValidator.parse("fill", "CONTEXT-STROKE")
    assert :error = SvgStyleValidator.parse("stop-color", "context-stroke")

    source =
      "<style>.marked{marker:url(#m);marker-end:none}</style><svg width='100' height='60'><defs><marker id='m' markerUnits='userSpaceOnUse' markerWidth='8' markerHeight='8' refX='4' refY='4'><circle cx='4' cy='4' r='3' fill='context-stroke'/></marker></defs><path class='marked' d='M10 30H40V45' fill='none' stroke='#174a86'/></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, tree} = Style.compute_detailed(dom)
    svg = Enum.find(tree.children, &(&1[:tag] == "svg"))
    assert svg.style.svg_scene.source =~ "marker-start=\"url(#m)\""
    assert svg.style.svg_scene.source =~ "marker-mid=\"url(#m)\""
    assert svg.style.svg_scene.source =~ "marker-end=\"none\""
    assert svg.style.svg_scene.source =~ "fill=\"context-stroke\""
    assert {:ok, pdf} = HtmlToPdf.render(source)
    assert pdf =~ "/Subtype /Image"
  end
end
