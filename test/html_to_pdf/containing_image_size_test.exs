defmodule NativeElixirPdfUtilities.HtmlToPdf.ContainingImageSizeTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  @image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

  test "block images resolve percentage height only against a definite containing content height" do
    for {container, expected} <- [
          {"height:80pt", 40.0},
          {"min-height:80pt", 6.0},
          {"height:80pt;padding:5pt;border:1pt solid;box-sizing:border-box", 34.25}
        ] do
      html =
        "<div style='width:80pt;#{container}'><img src='#{@image}' style='display:block;width:20pt;height:50%'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {150, 200}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))

      assert_in_delta image.height,
                      if(container == "min-height:80pt", do: 10.0, else: expected),
                      0.000001
    end
  end

  test "positioned replaced elements retain intrinsic auto sizing and resolve percentages in the padding box" do
    cases = [
      {"left:10pt;right:10pt;top:10pt;bottom:10pt", {12.0, 6.0}},
      {"left:10pt;top:10pt;width:50%;height:50%", {55.0, 45.0}},
      {"left:10pt;top:10pt;height:50%", {90.0, 45.0}}
    ]

    for {declaration, expected} <- cases do
      html =
        "<div style='position:relative;width:100pt;height:80pt;padding:5pt;border:1pt solid'><img src='#{@image}' style='position:absolute;#{declaration}'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {150, 200}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert {image.width, image.height} == expected
    end
  end

  test "flex and grid percentage images use definite content height and allocated area height" do
    for wrapper <- [
          "<div style='display:flex;width:100pt;height:80pt;align-items:flex-start'>IMAGE</div>",
          "<div style='display:flex;flex-direction:column;width:100pt;height:80pt;align-items:flex-start'>IMAGE</div>",
          "<div style='height:160pt'><div style='display:flex;width:100pt;height:50%;align-items:flex-start'>IMAGE</div></div>",
          "<div style='display:grid;width:100pt;height:80pt;grid-template-rows:80pt;grid-template-columns:100pt'>IMAGE</div>"
        ] do
      html =
        String.replace(wrapper, "IMAGE", "<img src='#{@image}' style='width:20pt;height:50%'>")

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {150, 200}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert {image.width, image.height} == {20.0, 40.0}, wrapper
    end
  end

  test "percentage descendants are laid out again after flex stretch supplies their real height" do
    for {child_style, expected} <- [{"", 40.0}, {"height:50%", 20.0}] do
      html =
        "<div style='display:flex;width:100pt;height:80pt'><div style='width:40pt;#{child_style}'><span><img src='#{@image}' style='width:20pt;height:50%'></span></div></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {150, 200}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert {image.width, image.height} == {20.0, expected}
    end
  end

  test "embedded HTML surfaces use their actual definite percentage viewport" do
    html =
      "<div style='height:80pt'><iframe srcdoc='<p>Child</p>' style='display:block;border:0;width:40pt;height:50%'></iframe></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.run(
      [],
      NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget(),
      fn _context ->
        assert {:ok, styled} = Style.compute_detailed(dom)
        assert {:ok, layout} = Layout.layout(styled, page_size: {150, 200}, margin: 0)

        flatten = fn recur, boxes ->
          Enum.flat_map(boxes, fn box -> [box | recur.(recur, Map.get(box, :boxes, []))] end)
        end

        group =
          layout.boxes
          |> then(&flatten.(flatten, &1))
          |> Enum.find(&(Map.get(&1, :role) == :embedded_viewport_host))

        assert group.width == 40 and group.height == 40
      end
    )
  end

  test "authored root percentage height supplies the initial page height through the body" do
    for {root_height, expected} <- [{"height:100%", 50.0}, {"", 10.0}] do
      html =
        "<html style='#{root_height}'><body style='height:50%'><img src='#{@image}' style='display:block;width:20pt;height:50%'></body></html>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {150, 200}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert_in_delta image.height, expected, 0.000001
    end
  end

  test "malformed root size metadata reports a shared layout diagnostic" do
    for root <- [
          %{height: -1},
          %{height: {:percent, nil}},
          %{padding: nil},
          %{box_sizing: :unknown},
          %{unexpected: 1}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{type: :document, children: [], document_root_sizing: root})

      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
      assert diagnostic.source == "document_root_sizing" and diagnostic.message != ""
    end

    assert {:error, {:resource_limit_exceeded, _}} =
             NativeElixirPdfUtilities.Validators.CssSizingValidator.validate_root(%{
               height: 1.0e100
             })
  end

  test "one definite replaced dimension keeps its size when constraints alter the auto dimension" do
    for {size, expected} <- [
          {"height:40pt;max-width:50pt", {50.0, 40.0}},
          {"width:40pt;min-height:80pt", {40.0, 80.0}},
          {"max-width:10pt;min-height:10pt", {10.0, 10.0}},
          {"width:20pt;aspect-ratio:4;min-height:10pt", {20.0, 10.0}}
        ] do
      html = "<img src='#{@image}' style='display:block;#{size}'>"
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {150, 200}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert {image.width, image.height} == expected
    end
  end

  test "nonempty constrained flex containers use actual allocated cross height for their descendants" do
    html =
      "<div style='display:flex;width:100pt;min-height:80pt'><div style='width:40pt'><img src='#{@image}' style='width:20pt;height:50%'></div></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {150, 200}, margin: 0)
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert {image.width, image.height} == {20.0, 40.0}
  end

  @tag :browser_parity
  @tag :replaced_constraints_original
  test "real constrained replaced dimensions match original Chromium" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_replaced_constraints.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_replaced_constraints",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  @tag :browser_parity
  @tag :containing_flex_grid_original
  test "flex, grid and embedded percentages match original Chromium with real root sizing" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_containing_flex_grid.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_containing_flex_grid",
        render_opts: [base_url: Path.expand("../..", __DIR__)],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  @tag :browser_parity
  @tag :containing_image_size_single_page
  test "same original sizing cases fit on one page independently of fragmentation" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_containing_image_size_single_page.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_containing_image_size_single_page",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  @tag :browser_parity
  @tag :containing_image_size_original
  test "definite, indefinite and positioned replaced sizing matches original Chromium" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_containing_image_size.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_containing_image_size",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
  end
end
