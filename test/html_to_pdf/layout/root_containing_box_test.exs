alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.RootContainingBoxTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  @image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

  test "root authored and constrained widths define body percentage references" do
    for {root, expected} <- [
          {"width:200pt", 50.0},
          {"width:50%;max-width:120pt", 30.0},
          {"width:20pt;min-width:100pt", 25.0}
        ] do
      html =
        "<html style='#{root}'><body style='margin:0;width:50%'><img src='#{@image}' style='display:block;width:50%'></body></html>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {300, 200}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert image.width == expected
    end
  end

  test "root edges, automatic margins and border-box sizing allocate and paint one border box" do
    html =
      "<html style='width:75%;max-width:180pt;min-width:100pt;height:120pt;box-sizing:border-box;padding:10pt;border:2pt solid #345;margin:10pt auto'><body style='margin:0;width:50%;height:50%'><img src='#{@image}' style='display:block;width:100%;height:50%'></body></html>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 200}, margin: 0)
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert {image.x, image.y, image.width, image.height} == {71.5, 154.25, 78.5, 24.25}

    [border] =
      Enum.filter(
        layout.boxes,
        &(Map.get(&1, :border_widths) == %{top: 1.5, right: 1.5, bottom: 1.5, left: 1.5})
      )

    assert {border.x, border.y, border.width, border.height} == {60.0, 70.0, 180.0, 120.0}
  end

  test "auto root width deducts its fixed outer and inner edges once" do
    html =
      "<html style='padding:10pt;border:2pt solid #345;margin:10pt 15pt'><body style='margin:0;height:80pt'><img src='#{@image}' style='display:block;width:100%;height:50%'></body></html>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 200}, margin: 0)
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert {image.x, image.y, image.width, image.height} == {26.5, 138.5, 247.0, 40.0}
  end

  test "root forged horizontal geometry is rejected before paint with shared diagnostics" do
    for root <- [
          %{width: -1},
          %{margin: %{top: 0, left: 0}},
          %{margin_inline_auto: %{left: true}},
          %{border_styles: :solid},
          %{border_styles: nil},
          %{
            border_styles: %{
              top: :solid,
              left: :solid,
              bottom: :solid,
              right: :solid,
              extra: :none
            }
          },
          %{border_colors: %{top: :invalid}},
          %{border_radius: -1}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{type: :document, children: [], document_root_sizing: root})

      assert diagnostic.stage == :layout and diagnostic.source == "document_root_sizing"
    end
  end

  test "a root fragment target retains the root border origin" do
    html =
      "<html id='root' style='width:100pt;margin:10pt auto'><body style='margin:0'><a href='#root'>Jump</a></body></html>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               page_size: {300, 200},
               margin: 0,
               system_font_discovery: false
             )

    assert pdf =~ "/XYZ 100 190 null]"
  end

  @tag :browser_parity
  @tag :root_containing_box_original
  test "root box dimensions, padding, borders and margins match original Chromium" do
    for name <- [
          "html_root_containing_box.html",
          "html_root_auto_box.html",
          "html_root_child_box.html"
        ] do
      fixture = Paths.fixture("html_to_pdf/browser_parity/#{name}")

      stats =
        NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
          artifact_dir: "tmp/browser_parity/#{name}",
          render_opts: [base_url: Paths.project(".")],
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
    end
  end
end
