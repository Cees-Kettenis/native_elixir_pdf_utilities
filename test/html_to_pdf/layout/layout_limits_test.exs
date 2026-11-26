defmodule NativeElixirPdfUtilities.HtmlToPdf.LayoutLimitsTest do
  use ExUnit.Case, async: false
  import NativeElixirPdfUtilities.TestSupport.LayoutFixture
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{Layout, HtmlParser, Style}

  test "standalone size measurements return the same calculation diagnostic as a full render" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_numeric_magnitude: 1_000})

    html = "<div style='width:calc(200% + 1px)'>Measure</div>"
    opts = [margin: 0, system_font_discovery: false]
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [%{style: style}]}} = Style.compute_detailed(dom, opts)

    assert Layout.resolve_size_value(12, nil) == 12
    assert Layout.resolve_content_size(style, :width, nil, nil) == nil
    assert Layout.resolve_size_value(style.width, nil, style) == nil
    assert Layout.resolve_content_size(style, :width, 375, nil) == 750.75
    assert Layout.resolve_size_value(style.width, 375, style) == 750.75
    assert {:ok, _pdf} = HtmlToPdf.render(html, [page_size: {375, 100}] ++ opts)

    assert {:error, {:invalid_document, diagnostic}} =
             error = HtmlToPdf.render(html, [page_size: {400, 100}] ++ opts)

    assert %{stage: :layout, source: "calc(200% + 1px)", operation: :prepare_css_calculation} =
             diagnostic

    assert Layout.resolve_content_size(style, :width, 400, nil) == error
    assert Layout.resolve_size_value(style.width, 400, style) == error
    assert Layout.resolve_size_value({:min, [10, style.width]}, 400, style) == error
  end

  test "popover scrollbar limits return the actionable layout diagnostic" do
    html =
      "<div popover style='display:block;width:1pt;height:1pt;padding:0;border:0;font-size:1pt;line-height:1pt'>" <>
        "<div style='height:2pt'></div></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    Limits.install(%{original_limits | max_numeric_magnitude: 10})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Layout.layout(styled, page_size: "9pt 9pt", margin: 0)

    assert diagnostic.stage == :layout and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.operation == :prepare_scrollbar
    assert diagnostic.source == "scrollbar geometry"
    assert diagnostic.message =~ "max_numeric_magnitude"
    assert diagnostic.message =~ "reduce the viewport dimensions"
  end

  test "layout returns an error when background repetition exceeds its resource limit" do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    Limits.install(Map.put(original_limits, :max_entries, 1))

    style =
      block_style()
      |> Map.merge(%{
        position: :relative,
        offsets: %{top: :invalid, right: :auto, bottom: :auto, left: :auto},
        width: 30.0,
        height: 30.0,
        background_image: image_fixture(5.0, 5.0),
        background_size: {0.000_001, 0.000_001},
        background_position: {{:percent, 0.0}, {:percent, 0.0}},
        background_repeat: :repeat
      })

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Layout.layout(
               document([%{type: :element, style: style, children: []}]),
               page_size: {100, 100},
               margin: 0
             )

    assert diagnostic.stage == :layout
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_entries"

    Limits.install(original_limits)

    assert {:error, :invalid_layout} =
             Layout.layout(
               document([
                 %{type: :element, style: %{style | background_repeat: :invalid}, children: []}
               ]),
               page_size: {100, 100},
               margin: 0
             )
  end
end
