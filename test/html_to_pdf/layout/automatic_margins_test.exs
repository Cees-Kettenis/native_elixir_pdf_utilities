alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.AutomaticMarginsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  @image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

  test "positioned replaced auto margins distribute free space on both axes" do
    for {extra, expected} <- [
          {"margin:auto", {40.0, 55.0}},
          {"margin-top:auto", {10.0, 30.0}},
          {"width:120pt;margin-left:auto;margin-right:auto", {10.0, 80.0}}
        ] do
      html =
        "<div style='position:relative;width:100pt;height:80pt'><img src='#{@image}' style='position:absolute;left:10pt;right:10pt;top:10pt;bottom:10pt;width:20pt;height:10pt;#{extra}'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {150, 100}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert {image.x, image.y} == expected
    end
  end

  test "flex and grid image margins consume main and cross space before alignment" do
    for display <- [
          "display:flex",
          "display:flex;flex-direction:column",
          "display:grid;grid-template-columns:100pt;grid-template-rows:80pt"
        ] do
      html =
        "<div style='#{display};width:100pt;height:80pt;justify-content:center;align-items:center'><img src='#{@image}' style='flex:0 0 auto;width:20pt;height:10pt;margin:auto'></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled, page_size: {150, 100}, margin: 0)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      assert {image.x, image.y, image.width, image.height} == {40.0, 55.0, 20.0, 10.0}, display
    end
  end

  test "main margins divide across sides and cross auto margins suppress stretching" do
    html =
      "<div style='display:flex;width:100pt;height:80pt;gap:10pt;justify-content:center'><img src='#{@image}' style='width:20pt;height:10pt;margin-left:auto'><img src='#{@image}' style='width:20pt;margin-top:auto'></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {150, 100}, margin: 0)
    [first, second] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert first.x == 50 and second.x == 80
    assert second.y == 20 and second.height == 10
  end

  test "selected-content cloning paints the default auto-margin picker" do
    html =
      "<a href='#copied'>Clone jump</a><select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option><div><template shadowrootmode=open shadowrootclonable><slot></slot><p>Shadow copy</p></template><span id=copied>Copied target</span></div></option></select>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert String.starts_with?(pdf, "%PDF-")
    budget = NativeElixirPdfUtilities.Validators.HtmlValidator.new_image_budget()

    NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.run([], budget, fn _ ->
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

      assert {:ok, prepared} =
               NativeElixirPdfUtilities.HtmlToPdf.prepare_document(
                 dom,
                 [system_font_discovery: false],
                 budget
               )

      paint = Enum.map_join(hd(prepared.pages).boxes, &Map.get(&1, :text, ""))
      assert paint =~ "Copied target" and paint =~ "Shadow copy" and paint =~ "▼"
    end)
  end

  test "nonmodal dialog defaults center vertically only when both authored vertical insets are definite" do
    html =
      "<div style='position:relative;width:100pt;height:80pt'><dialog open style='top:10pt;bottom:10pt;width:20pt;height:10pt;padding:0;border:0'>D</dialog></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [host] = styled.children
    [dialog] = host.children
    assert dialog.style.margin_block_auto == %{top: true, bottom: true}
    assert {:ok, layout} = Layout.layout(styled, page_size: {150, 100}, margin: 0)
    background = Enum.find(layout.boxes, &(Map.get(&1, :fill_color) == {1.0, 1.0, 1.0}))

    assert {background.x, background.y, background.width, background.height} ==
             {40.0, 55.0, 20.0, 10.0}
  end

  test "malformed vertical automatic-margin intent returns shared layout diagnostics" do
    for flags <- [
          nil,
          false,
          %{top: true},
          %{top: true, bottom: 1},
          %{top: true, bottom: false, left: true}
        ] do
      tree = %{
        type: :document,
        children: [
          %{
            type: :element,
            tag: "div",
            children: [],
            style: %{display: :block, margin_block_auto: flags}
          }
        ]
      }

      assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(tree)
      assert diagnostic.stage == :layout and diagnostic.operation == :layout
      assert diagnostic.source == "div" and diagnostic.message =~ "boolean top and bottom"
    end
  end

  @tag :browser_parity
  @tag :automatic_margins_original
  test "all-axis automatic margins match original Chromium" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/html_automatic_margins.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_automatic_margins",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  @tag :browser_parity
  @tag :default_picker_original
  test "the default picker paints copied contents and its icon like Chromium" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/html_default_picker_auto.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_default_picker_auto",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    {text, 0} = System.cmd("pdftotext", [Path.join(stats.artifact_dir, "native.pdf"), "-"])
    assert text =~ "Copied target" and text =~ "Shadow copy" and text =~ "▼"
  end
end
