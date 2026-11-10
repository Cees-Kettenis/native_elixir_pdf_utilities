defmodule NativeElixirPdfUtilities.HtmlToPdf.UsedDimensionsTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, CssSizingValidator}

  # CSS Sizing 3 CR 22 May 2019 and Sizing 4 ED 2da1cb17a433,
  # definite percentages and the preferred aspect-ratio sizing rules.
  test "the common content-size result distinguishes indefinite percentages and applies min after max" do
    style = %{width: {:percent, 0.5}, min_width: 20, max_width: 60}
    assert Layout.resolve_content_size(style, :width, 200, 40) == 60
    assert Layout.resolve_content_size(style, :width, nil, 40) == 40
    assert Layout.resolve_content_size(%{style | min_width: 80}, :width, 200, 40) == 80
    edges = %{left: 5, right: 5, top: 2, bottom: 2}
    border = %{left: 1, right: 1, top: 1, bottom: 1}
    style = Map.merge(style, %{box_sizing: :border_box, padding: edges, border_widths: border})
    assert Layout.resolve_content_size(style, :width, 100, 40) == 38
    assert Layout.resolve_content_size(%{style | width: :auto}, :width, nil, 40) == 40
  end

  test "auto ratios prefer a replaced intrinsic ratio and retain the authored nonreplaced fallback" do
    assert {:ok, :auto} = HtmlValidator.parse_aspect_ratio("auto")
    assert {:ok, {:auto, 2.0}} = HtmlValidator.parse_aspect_ratio("auto 2 / 1")
    assert {:ok, {:auto, 2.0}} = HtmlValidator.parse_aspect_ratio("2/1 auto")
    assert CssSizingValidator.preferred_ratio(%{aspect_ratio: {:auto, 2}}, 3) == 3
    assert CssSizingValidator.preferred_ratio(%{aspect_ratio: {:auto, 2}}) == 2
    assert CssSizingValidator.preferred_ratio(%{aspect_ratio: 2}, 3) == 2
    assert CssSizingValidator.preferred_ratio(%{aspect_ratio: :auto}, 3) == 3

    for value <- ["auto auto", "/2", "2/", "1//2", "1 auto /2", "-1/1", "1/-1"] do
      assert :error = HtmlValidator.parse_aspect_ratio(value), value
    end
  end

  test "a ratio-dependent automatic minimum preserves content unless min-height or a scroll container removes it" do
    for {extra, expected} <- [{"", 60}, {"min-height:0", 20}, {"overflow:hidden", 20}] do
      html =
        "<div style='width:40pt;aspect-ratio:auto 2/1;line-height:20pt;background:red;#{extra}'>One<br>Two<br>Three</div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
      red = Enum.find(result.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert_in_delta red.height, expected, 0.75
    end
  end

  test "malformed computed display and overflow geometry returns actionable shared diagnostics" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<div>Content</div>")
    assert {:ok, %{children: [child]} = tree} = Style.compute_detailed(dom)

    for {property, value} <- [
          {:_margin_struts, %{top: {1, :bad}}},
          {:_margin_struts, %{left: {1, -1}}},
          {:_margin_struts, :bad},
          {:_collapses_through, :yes},
          {:aspect_ratio, -1},
          {:aspect_ratio, {:auto, :broken}},
          {:writing_mode, :diagonal},
          {:_containing_writing_mode, :diagonal},
          {:_containing_direction, :backward},
          {:direction, :reverse},
          {:visibility, :gone},
          {:_flow_root, :yes},
          {:isolation, :foreign},
          {:contain, [:unknown]},
          {:overflow_x, :broken},
          {:overflow_clip_margin, {:padding_box, -1}},
          {:text_overflow, <<255>>},
          {:max_lines, 0},
          {:continue, :scroll}
        ] do
      bad = %{tree | children: [%{child | style: Map.put(child.style, property, value)}]}
      assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(bad)
      assert diagnostic.stage == :layout and diagnostic.operation == :layout
      assert diagnostic.source == "div" and diagnostic.message =~ "recompute styles"
    end
  end

  test "degenerate ratios behave as auto and auto plus a ratio always uses the content box" do
    for value <- ["0", "0/1", "1/0", "0/0", "auto 0/1"] do
      assert {:ok, :auto} = HtmlValidator.parse_aspect_ratio(value)
    end

    assert {:ok, 2.0} = HtmlValidator.parse_aspect_ratio("calc(2 + 2) / 2")

    for {ratio, expected} <- [{"2/1", 40}, {"auto 2/1", 45}] do
      html =
        "<div style='width:80pt;padding:5pt;box-sizing:border-box;aspect-ratio:#{ratio};background:red'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(tree, page_size: {200, 200}, margin: 0)
      red = Enum.find(result.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert_in_delta red.height, expected, 0.75
    end
  end

  test "ratio-derived definite heights resolve descendant percentages in content and border boxes" do
    for {extra, expected} <- [
          {"", 20},
          {"padding:5pt;box-sizing:border-box", 15},
          {"max-height:30pt", 15}
        ] do
      html =
        "<div style='width:80pt;aspect-ratio:2;#{extra}'><div style='height:50%;background:red'></div></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(tree, page_size: {200, 200}, margin: 0)
      red = Enum.find(result.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert_in_delta red.height, expected, 0.75
    end
  end

  test "nested positioned ratio backgrounds retain all fragment owners during pagination" do
    html =
      "<div style='height:50pt;background:gray;position:relative'><div style='position:absolute;left:4pt;top:0;width:20pt;padding:2pt;box-sizing:border-box;aspect-ratio:2;background:blue'><div style='height:50%;background:red'></div></div></div>"

    assert {:ok, _pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)
  end

  test "root ratio and axes use the same semantic validation as descendant boxes" do
    for root <- [
          %{aspect_ratio: 2},
          %{aspect_ratio: {:auto, 2}},
          %{writing_mode: :vertical_rl, direction: :rtl}
        ] do
      assert :ok = CssSizingValidator.validate_root(root)
    end

    for root <- [%{aspect_ratio: -1}, %{direction: :backward}, %{writing_mode: :diagonal}] do
      assert {:error, {:invalid_layout, diagnostic}} = CssSizingValidator.validate_root(root)
      assert diagnostic.source == "document_root_sizing"
    end
  end

  test "canonical collapse extrema retain their bounded positive and negative components" do
    assert :ok =
             CssSizingValidator.validate_auto_margins(
               %{_margin_struts: %{top: {10, -5}, bottom: 0}, _collapses_through: true},
               "div"
             )
  end
end
