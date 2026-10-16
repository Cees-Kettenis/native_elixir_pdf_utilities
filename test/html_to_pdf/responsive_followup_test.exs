Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.ResponsiveFollowupTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.Font

  alias NativeElixirPdfUtilities.Validators.{
    PrintMediaValidator,
    ResponsiveImageValidator,
    SourceSizeValidator
  }

  test "source-size units use actual initial-font metrics independent of authored declarations" do
    assert {:ok, registry} = Font.load_registry(system_font_discovery: false)
    assert {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    context = %{width: 800.0, height: 600.0, density: 1.0, type: :print}

    assert {:ok, measured} =
             SourceSizeValidator.prepare_context(context, ["2ch", "1ex", "1cap", "1ic", "1lh"],
               __source_font__: font
             )

    assert {:ok, ch} = PrintMediaValidator.source_length("2ch", measured)
    assert_in_delta ch, Font.text_width("00", font, 16.0), 0.000001
    assert {:ok, ex} = PrintMediaValidator.source_length("1ex", measured)
    assert ex > 0 and ex < 16
    assert {:ok, cap} = PrintMediaValidator.source_length("1cap", measured)
    assert cap > ex
    assert {:ok, 16.0} = PrintMediaValidator.source_length("1ic", measured)
    assert {:ok, lh} = PrintMediaValidator.source_length("1lh", measured)

    assert_in_delta lh,
                    NativeElixirPdfUtilities.Validators.HtmlValidator.normal_line_height(%{
                      font_face: font,
                      font_size: 12.0
                    }) / 0.75,
                    0.000001

    assert ResponsiveImageValidator.source_size("calc(10ch + 1cap)", measured) > ch
  end

  test "malformed size components fall through while resource failures retain diagnostics" do
    context = %{width: 800.0, height: 600.0, density: 1.0, type: :print}
    assert ResponsiveImageValidator.source_size("'invalid', 40px", context) == 40.0
    assert ResponsiveImageValidator.source_size("calc(", context) == 800.0

    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)
    NativeElixirPdfUtilities.Limits.install(%{limits | max_css_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             ResponsiveImageValidator.source_size("calc(20px + 10px)", context)

    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.stage == :style
    assert diagnostic.message =~ "max_css_work"
  end

  test "styling an automatic lazy image without its resource scope returns the preparation diagnostic" do
    assert {:ok, document} =
             NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_detailed(
               "<img loading='lazy' sizes='auto' src='image.png'>"
             )

    assert {:error, {:invalid_document, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.Style.compute_detailed(document)

    assert diagnostic.stage == :style
    assert diagnostic.source == "img"
    assert diagnostic.message =~ "active rendering resource scope"
    assert diagnostic.message =~ "HtmlToPdf.render"
  end

  test "media condition grammar supports nested boolean expressions and rejects mixed unparenthesized operators" do
    context = %{width: 800.0, height: 600.0, density: 1.0, type: :print}

    for query <- [
          "(width:800px) and (height:600px)",
          "((width < 700px) or (width > 750px)) and (height >= 600px)",
          "not ((width < 700px) or (height > 650px))",
          "only print and ((width:800px) or (width:900px))",
          "(width: calc(700px + 100px))",
          "(width > -1px)"
        ] do
      assert PrintMediaValidator.matches?(query, context), query
    end

    for query <- [
          "(width:800px) or (height:600px) and (width:900px)",
          "(width:800px) and",
          "and (width:800px)",
          "not (width:700px) and (height:600px)",
          "print or (width:800px)",
          "(width:800px))",
          "((width:800px)",
          "only (width:800px)",
          "print and (width:700px) or (height:600px)"
        ] do
      refute PrintMediaValidator.matches?(query, context), query
    end
  end

  test "font-relative selection reads only the selected authorized image" do
    image =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(128, 64, 2, 8, 0, fn _x, _y ->
        [0, 128, 64]
      end)

    assert {:ok, _pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<img src='unused-fallback' srcset='unused-small 20w, selected 128w' sizes='10ch' style='width:48pt;height:24pt;font-size:40pt'>",
               asset_resolver: fn request ->
                 assert request.reference == "selected"
                 send(self(), {:selected_image, request.reference})
                 {:ok, image}
               end
             )

    assert_received {:selected_image, "selected"}
    refute_received {:selected_image, _}
  end

  @tag :browser_parity
  @tag :responsive_followup
  test "source metrics and nested media selection match untouched Chromium" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_responsive_source_metrics.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_responsive_source_metrics",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
