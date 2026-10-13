defmodule NativeElixirPdfUtilities.HtmlToPdf.ObjectPositionFollowupTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Validators.CssValueValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

  @image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

  test "four-value edge offsets preserve axes, signs and current font sizing" do
    for value <- ["right 6pt bottom 3pt", "bottom 3pt right 6pt"] do
      assert {:ok, style} =
               CssValueValidator.validate_resolved(%{font_size: 20.0}, "object-position", value)

      assert style.object_position == {{:end, 6.0}, {:end, 3.0}}
    end

    assert {:ok, style} =
             CssValueValidator.validate_resolved(
               %{font_size: 20.0},
               "object-position",
               "right 1em top -2em"
             )

    assert style.object_position == {{:end, 20.0}, -40.0}

    assert {:ok, style} =
             CssValueValidator.validate_resolved(
               %{font_size: 12.0},
               "object-position",
               "left +1e1% bottom -20%"
             )

    assert style.object_position == {{:percent, 0.1}, {:end, {:percent, -0.2}}}

    for value <- [
          "left 2pt right 3pt",
          "right 6pt top",
          "left top center",
          "center 2pt bottom 3pt"
        ] do
      assert :error =
               CssValueValidator.validate_resolved(%{font_size: 12.0}, "object-position", value)
    end

    assert {:ok, style} =
             CssValueValidator.validate_resolved(
               %{font_size: 12.0},
               "background-position",
               "right 6pt top"
             )

    assert style.background_position == {{:end, 6.0}, {:percent, 0.0}}
  end

  test "edge offsets place the real fitted image and retain its actual viewport clip" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<img src='#{@image}' style='display:block;width:60pt;height:40pt;object-fit:none;object-position:right 6pt bottom 3pt'>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {100, 100}, margin: 0)
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert image.width == 12 and image.height == 6
    assert image.x == 42 and image.y == 63
    assert image.clip == %{x: 0.0, y: 60.0, width: 60.0, height: 40.0}
  end

  @tag :browser_parity
  @tag :object_position_followup
  test "four-value object positioning matches the original Chromium fixture" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_object_position_edges.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_object_position_edges",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
