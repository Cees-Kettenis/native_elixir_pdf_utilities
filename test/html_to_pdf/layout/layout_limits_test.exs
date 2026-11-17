defmodule NativeElixirPdfUtilities.HtmlToPdf.LayoutLimitsTest do
  use ExUnit.Case, async: false
  import NativeElixirPdfUtilities.TestSupport.LayoutFixture
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{Layout, HtmlParser, Style}

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
