defmodule NativeElixirPdfUtilities.HtmlToPdf.PositionMathTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.{CssPositionValidator, CssValueValidator}

  @image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

  test "position math retains real percentage space and typed functions in both axes" do
    assert {:ok, style} =
             CssValueValidator.validate_resolved(
               %{font_size: 12.0},
               "object-position",
               "calc(50% - 6pt) min(100%, 3pt)"
             )

    assert style.object_position ==
             {{:css_position_math, "calc(50% - 6pt)"}, {:css_position_math, "min(100%, 3pt)"}}

    assert {:ok, _} =
             CssValueValidator.validate_resolved(
               %{font_size: 12.0},
               "object-position",
               ~S|r\69ght calc(1em + 3pt) b\6fttom max(2pt, 10%)|
             )

    for invalid <- ["calc(1px / 1s)", "sqrt(4px)", "calc(1px + 1)", "unknown(2px)"] do
      assert :error = CssPositionValidator.prepare_math(invalid)
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<img src='#{@image}' style='display:block;width:60pt;height:40pt;object-fit:none;object-position:calc(50% - 6pt) min(100%, 3pt)'>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {100, 100}, margin: 0)
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert_in_delta image.x, 18, 0.000001
    assert_in_delta image.y, 91, 0.000001
    assert image.width == 12 and image.height == 6
  end

  test "root and element metrics come from the selected font and authored used sizes" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<html style='font-size:20pt;line-height:30pt'><body><img src='#{@image}' style='display:block;font-size:10pt;line-height:15pt;width:80pt;height:50pt;object-fit:none;object-position:calc(1rem + 1em) calc(1rlh - 1lh)'></body></html>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {100, 100}, margin: 0)
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert_in_delta image.x, 30, 0.000001
    assert_in_delta image.y, 79, 0.000001
  end

  test "plain extended units and escaped units use the same selected metric authority" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<html style='font-family:DejaVu Serif;font-size:20pt;line-height:30pt'><body><img src='#{@image}' style='display:block;font-family:DejaVu Sans;font-size:10pt;line-height:15pt;width:80pt;height:50pt;object-fit:none;object-position:1rch 1ch'></body></html>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {100, 100}, margin: 0)
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert {:ok, registry} = NativeElixirPdfUtilities.HtmlToPdf.Font.load_registry([])

    assert {:ok, _, root} =
             NativeElixirPdfUtilities.HtmlToPdf.Font.resolve(
               "DejaVu Serif",
               400,
               :normal,
               registry
             )

    assert {:ok, _, element} =
             NativeElixirPdfUtilities.HtmlToPdf.Font.resolve(
               "DejaVu Sans",
               400,
               :normal,
               registry
             )

    assert_in_delta image.x,
                    NativeElixirPdfUtilities.HtmlToPdf.Font.text_width("0", root, 20.0),
                    0.000001

    assert_in_delta image.y,
                    94 - NativeElixirPdfUtilities.HtmlToPdf.Font.text_width("0", element, 10.0),
                    0.000001

    assert {:ok, _} = CssPositionValidator.prepare_math(~S|1\70 c|)
  end

  test "cover positions evaluate against negative remaining space and zero font size remains valid" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<img src='#{@image}' style='display:block;font-size:0;line-height:0;width:40pt;height:40pt;object-fit:cover;object-position:min(10pt, 50%) calc(1em + 50%)'>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {100, 100}, margin: 0)
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert image.width == 80 and image.height == 40
    assert_in_delta image.x, -20, 0.000001
    assert_in_delta image.y, 60, 0.000001
    assert image.clip == %{x: 0.0, y: 60.0, width: 40.0, height: 40.0}
  end

  test "invalid selected contexts and malformed calculations use shared diagnostics" do
    for component <- [
          {:css_position_math, "calc(1ch)"},
          {:css_position_math, :forged},
          {:percent, :forged}
        ] do
      assert {:error, {reason, diagnostic}} = CssPositionValidator.resolve(component, 10, %{})
      assert reason in [:invalid_layout, :invalid_font]
      assert diagnostic.stage in [:layout, :font]
      assert diagnostic.reason == reason
      assert diagnostic.message != ""
    end

    assert :error = CssPositionValidator.prepare_math("calc(1px / 1e-320)")
    assert :error = CssPositionValidator.prepare_math(nil)

    assert {:error, {:invalid_layout, _}} =
             CssPositionValidator.resolve({:percent, 0.5}, :invalid, %{})

    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.Validators.SourceSizeValidator.prepare_font_metrics(
               %{},
               -1,
               12
             )

    assert diagnostic.stage == :style
    assert diagnostic.message != ""
  end

  test "actual unbounded position math reports a shared resource failure from rendering" do
    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<img src='#{@image}' style='width:40pt;height:40pt;object-fit:none;object-position:calc(infinity*1px) 0'>"
             )

    assert diagnostic.stage == :layout
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message != ""
    assert diagnostic.source == "calc(infinity*1px)"
  end

  @tag :browser_parity
  @tag :position_math_original
  test "calculated object and background positions match original Chromium" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/browser_parity/html_position_math.html", __DIR__)

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_position_math",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
