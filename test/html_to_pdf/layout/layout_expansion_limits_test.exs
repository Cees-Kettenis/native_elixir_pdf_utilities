defmodule NativeElixirPdfUtilities.HtmlToPdf.LayoutExpansionLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Limits

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "grid cell admission counts both axes and repeated overlapping placements" do
    item = "<div style='grid-row:1 / span 2;grid-column:1 / span 2'></div>"
    html = "<div style='display:grid'>#{item}#{item}</div>"
    original = Limits.effective()
    Limits.install(%{original | max_grid_occupied_cells: 8})
    assert {:ok, _} = HtmlToPdf.render(html, system_font_discovery: false)

    Limits.install(%{original | max_grid_occupied_cells: 7})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :layout and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.operation == :render and diagnostic.message =~ "max_grid_occupied_cells"
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlValidator

    # A failed grid must release its cumulative budget for the next render.
    assert {:ok, _} = HtmlToPdf.render("<div style='display:grid'>#{item}</div>")
  end

  test "grid track admission includes sparse implicit axes and multiple containers" do
    original = Limits.effective()

    for {html, tracks} <- [
          {"<div style='display:grid'><div style='grid-row:10 / span 2;grid-column:10 / span 2'></div></div>",
           22},
          {"<div style='display:grid'></div><div style='display:grid'></div>", 4}
        ] do
      Limits.install(%{original | max_grid_tracks: tracks})
      assert {:ok, _} = HtmlToPdf.render(html, system_font_discovery: false)
      Limits.install(%{original | max_grid_tracks: tracks - 1})
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :layout and diagnostic.message =~ "max_grid_tracks"
    end
  end

  test "automatic grid searches stop under their shared placement work allowance" do
    Limits.install(%{Limits.effective() | max_grid_placement_work: 1})

    for placement <- ["", "grid-column:1", "grid-row:1"] do
      html =
        "<div style='display:grid;grid-template-columns:10pt 10pt'>" <>
          "<div style='grid-column:1;grid-row:1'></div><div style='#{placement}'></div></div>"

      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :layout and diagnostic.message =~ "max_grid_placement_work"
    end
  end

  test "preserved line work admits its exact boundary and accumulates across elements" do
    original = Limits.effective()
    html = "<pre>A\nB</pre>"
    Limits.install(%{original | max_layout_line_work: 2})
    assert {:ok, _} = HtmlToPdf.render(html, system_font_discovery: false)
    Limits.install(%{original | max_layout_line_work: 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :layout and diagnostic.message =~ "max_layout_line_work"

    Limits.install(%{original | max_layout_line_work: 3})
    assert {:error, {:resource_limit_exceeded, _}} = HtmlToPdf.render(html <> html)
  end

  test "empty preserved lines consume work before the remaining text is expanded" do
    Limits.install(%{Limits.effective() | max_layout_line_work: 2})

    for mode <- ["pre", "pre-wrap", "pre-line", "break-spaces"] do
      html = "<div style='white-space:#{mode}'>\n\n\nTail</div>"
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :layout and diagnostic.message =~ "max_layout_line_work"
      assert diagnostic.operation == :render and diagnostic.reason == :resource_limit_exceeded
    end

    assert {:ok, _} = HtmlToPdf.render("<p>Unaffected</p>")
  end

  test "preserved empty lines grow linearly and keep following text geometry" do
    for count <- [256, 1_024] do
      html =
        "<div style='font-size:1pt;line-height:1pt;white-space:pre'>" <>
          "A" <> String.duplicate("\n", count) <> "<span>B</span></div>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, page_size: {200, 2_000}, margin: 0)
      [a, b] = Enum.filter(layout.boxes, &(&1.type == :text))
      assert a.text == "A" and b.text == "B"
      assert_in_delta a.y - b.y, count, 0.0001
    end
  end

  test "quadrupling a preserved newline run stays below quadratic construction work" do
    reductions =
      for count <- [1_024, 4_096] do
        assert {:ok, dom} = HtmlParser.parse("<pre>" <> String.duplicate("\n", count) <> "</pre>")
        assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
        {:reductions, before} = Process.info(self(), :reductions)
        assert {:ok, _} = Layout.layout(styled, page_size: {200, 100_000}, margin: 0)
        {:reductions, after_count} = Process.info(self(), :reductions)
        after_count - before
      end

    [small, large] = reductions
    assert large < small * 6, inspect(reductions)
  end
end
