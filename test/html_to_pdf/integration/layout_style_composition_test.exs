alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.LayoutStyleCompositionTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Layout,
    Pagination,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{
    CssSizingValidator,
    CssTransformValidator,
    HtmlValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  defp source_layout(html, options) do
    with {:ok, dom} <- HtmlParser.parse_document_detailed(html),
         {:ok, tree} <- Style.compute_detailed(dom, system_font_discovery: false),
         {:ok, layout} <- Layout.layout(tree, options) do
      {:ok, layout}
    end
  end

  test "avoided empty and nonempty source blocks consume their reserved height" do
    for contents <- ["", "<p>Reserved</p>"], decoration <- ["", "background:#eee"] do
      html = """
      <style>html,body,p{margin:0;font-size:10pt;line-height:10pt}section{height:40pt;break-inside:avoid;#{decoration}}</style>
      <p>Before</p><section>#{contents}</section><p>After</p>
      """

      assert {:ok, layout} = source_layout(html, page_size: {100, 60}, margin: 10)
      assert Enum.any?(layout.boxes, &Map.has_key?(&1, :block_placement_height))
      assert {:ok, pages} = Pagination.paginate(layout)
      assert length(pages) == 3
      assert Enum.any?(hd(pages).boxes, &(Map.get(&1, :text) == "Before"))
      assert Enum.any?(List.last(pages).boxes, &(Map.get(&1, :text) == "After"))

      spaces = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :explicit_block_space))

      expected = if contents == "", do: 40, else: 30
      assert_in_delta Enum.sum(Enum.map(spaces, & &1.height)), expected, 1.0e-9
    end
  end

  test "list items retain markers and ordinary content in row, column and grid tracks" do
    for display <- ["flex", "grid"], direction <- ["row", "column"] do
      html = """
      <style>html,body{margin:0}ul{display:#{display};flex-direction:#{direction};grid-template-columns:70pt 70pt;gap:4pt;width:150pt;margin:0;padding:0}li{list-style-position:inside;padding:2pt;background:#eee}</style>
      <ul><li><span>First</span></li><li><span>Second</span></li></ul>
      """

      assert {:ok, layout} = source_layout(html, page_size: {200, 180}, margin: 10)
      texts = for box <- layout.boxes, box.type == :text, do: box
      assert Enum.count(texts, &(&1.text == "First")) == 1
      assert Enum.count(texts, &(&1.text == "Second")) == 1
      assert Enum.count(texts, &String.contains?(&1.text, "•")) == 2
      first = Enum.find(texts, &(&1.text == "First"))
      second = Enum.find(texts, &(&1.text == "Second"))
      assert first.x >= 10 and second.x >= 10
      if display == "flex" and direction == "column", do: assert(first.y > second.y)
    end
  end

  test "block Ruby and transformed inline atoms retain selection and links" do
    html = """
    <style>html,body{margin:0}ruby{display:block}math,ruby,img,span.atom{transform:rotate(2deg);transform-origin:0 0}p{margin:0}</style>
    <ruby>Base<rt>Reading</rt></ruby><p><a href='https://example.test'><math><mi>x</mi></math><span class=atom style='display:inline-block'>Atomic</span></a></p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 140}, margin: 10)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    for expected <- ["Base", "Reading", "x", "Atomic"], do: assert(text =~ expected)
    assert {:ok, parsed} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    assert Enum.any?(parsed.document.objects, fn {_id, object} ->
             is_map(object.value) and Map.get(object.value, "Subtype") == {:name, "Link"}
           end)
  end

  test "authored patterned decorations keep edge paint rather than filled bars" do
    html = """
    <style>html,body{margin:0}span{font-size:12pt;text-decoration-line:underline;text-decoration-thickness:1px} .dash{text-decoration-style:dashed}.dot{text-decoration-style:dotted}</style>
    <p><span class=dash>Dashed</span> <span class=dot>Dotted</span></p>
    """

    assert {:ok, layout} = source_layout(html, page_size: {200, 120}, margin: 10)
    strokes = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_decoration))
    assert length(strokes) == 2
    assert Enum.sort(Enum.map(strokes, & &1.border_styles.bottom)) == [:dashed, :dotted]
    assert Enum.all?(strokes, &(&1.fill_color == nil and &1.height == 0.75 and &1.width > 0))
  end

  test "inert temporal controls and unscoped shadow selectors retain passive source policy" do
    html = """
    <style>:host{color:red}::slotted(span){color:blue}span::before{content:'Visible'}</style>
    <div inert><input type=date value=2026-10-14><input type=time value=12:30></div><span>Source</span>
    """

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(tree, page_size: {300, 180}, margin: 10)
    texts = Enum.filter(layout.boxes, &(&1.type == :text))
    assert Enum.any?(texts, &(&1.text == "Source" and &1.color == {0, 0, 0}))
    assert Enum.any?(texts, &(&1.text == "Visible"))
    assert Enum.all?(layout.boxes, &(Map.get(&1, :form_control) == nil))
  end

  test "disabled base selects retain selected option content and native black text" do
    html = """
    <style>select{appearance:base-select}div::before{content:'Hidden'}div{content-visibility:hidden}</style>
    <select disabled><option>Other</option><option selected>Selected</option></select><div>Concealed</div>
    """

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
    [%{children: [select, skipped]}] = tree.children
    assert select.style.form_kind == :base_select and select.style.color == {0, 0, 0}
    assert skipped.style._html_content_skipped
    assert {:ok, layout} = Layout.layout(tree, page_size: {220, 140}, margin: 10)
    texts = for box <- layout.boxes, box.type == :text, do: box.text
    assert Enum.count(texts, &(&1 == "Selected")) == 1
    refute "Other" in texts or "Hidden" in texts or "Concealed" in texts
  end

  test "quote nesting crosses element boundaries independently of counters" do
    for {content, expected} <- [
          {"<q>A<span><q>B</q></span>C</q>", "«A‹B›C»"},
          {"<div><q>First</q></div><p><q>Next</q></p>", "«First»«Next»"}
        ] do
      html = "<style>q{quotes:'«' '»' '‹' '›'}span{counter-reset:item}</style>" <> content
      assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 140}, margin: 10)
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
      assert String.replace(text, ~r/\s+/, "") == expected
    end
  end

  test "empty positioned targets retain source line ownership and root transforms" do
    html = """
    <html id=root style='transform:translateX(2pt)'><style>html,body,p{margin:0}span{position:relative;line-height:0}</style><body><p><span id=empty></span></p><p><a href='#root'>Root</a><a href='#empty'>Empty</a></p></body></html>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 140}, margin: 10)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert String.replace(text, ~r/\s+/, "") == "RootEmpty"
    assert {:ok, parsed} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    destinations =
      for {_id, object} <- parsed.document.objects,
          is_map(object.value),
          Map.has_key?(object.value, "Dest"),
          do: object.value["Dest"]

    assert length(destinations) == 2
    assert hd(destinations) == List.last(destinations)

    assert Enum.all?(destinations, fn [_, {:name, "XYZ"}, x, y, _] ->
             x >= 0 and x <= 200 and y > 100 and y <= 140
           end)
  end

  test "converter SVG artwork carries only its outer HTML link through ordinary image paint" do
    source = """
    <a href='https://example.test/outer'><svg width='40' height='20'><a href='https://example.test/internal'><rect width='40' height='20' fill='red'/></a></svg></a>
    """

    assert {:ok, pdf} = HtmlToPdf.render(source, page_size: {160, 100}, margin: 10)
    assert {:ok, parsed} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    links =
      for {_id, object} <- parsed.document.objects,
          is_map(object.value),
          object.value["Subtype"] == {:name, "Link"},
          do: object.value

    assert length(links) == 1
    assert hd(links)["A"]["URI"] == {:string, "https://example.test/outer"}
  end

  test "root skipped content suppresses its generated content and descendants" do
    source =
      "<style>body{content-visibility:hidden}body::before{content:'Generated'}</style><p>Descendant</p>"

    assert {:ok, pdf} = HtmlToPdf.render(source, page_size: {160, 100}, margin: 10)

    assert {:error, {:no_extractable_text, diagnostic}} =
             NativeElixirPdfUtilities.Text.extract(pdf)

    assert diagnostic.stage == :text_extraction
    assert {:ok, layout} = source_layout(source, page_size: {160, 100}, margin: 10)
    refute Enum.any?(layout.boxes, &(&1.type in [:text, :glyph_group, :image]))
  end

  test "zero-height transformed image retains its inline allocation without visible ink" do
    bytes =
      File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/iframe-bars.png"))

    source =
      "<p style='margin:0'><img src='data:image/png;base64,#{Base.encode64(bytes)}' style='width:20pt;height:0;transform:rotate(2deg)'>After</p>"

    assert {:ok, layout} = source_layout(source, page_size: {160, 100}, margin: 10)

    assert Enum.any?(layout.boxes, fn box ->
             box.type == :flow_marker and Map.get(box, :css_transform_scopes, []) != []
           end)

    [text] = Enum.filter(layout.boxes, &(&1.type == :text))
    assert text.text == "After" and text.x == 30
    refute Enum.any?(layout.boxes, &(&1.type == :image))
    assert {:ok, pdf} = HtmlToPdf.render(source, page_size: {160, 100}, margin: 10)
    assert {:ok, extracted} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert String.trim(extracted) == "After"
  end

  test "root border metadata and oversized calculations return shared actionable diagnostics" do
    edges = %{top: :solid, right: :solid, bottom: :solid, left: :solid}
    colors = Map.new(Map.keys(edges), &{&1, {0, 0, 0}})
    assert :ok = CssSizingValidator.validate_root(%{border_styles: edges, border_colors: colors})

    for root <- [
          %{border_styles: %{edges | top: :invalid}},
          %{border_styles: :invalid},
          %{border_colors: %{colors | left: :invalid}},
          %{border_colors: :invalid},
          %{margin: :invalid}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} = CssSizingValidator.validate_root(root)
      assert diagnostic.stage == :layout and diagnostic.module == CssSizingValidator
      assert diagnostic.source == "document_root_sizing"
    end

    Limits.install(%{Limits.effective() | max_html_numeric_token_bytes: 4})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssSizingValidator.prepare_math("calc(1pt)")

    assert diagnostic.message =~ "max_html_numeric_token_bytes"
  end

  test "lazy image resolution retains its surviving source viewport" do
    bytes =
      File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/iframe-bars.png"))

    image = "data:image/png;base64,#{Base.encode64(bytes)}"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               HtmlParser.parse_document_detailed(
                 "<img loading=lazy sizes=auto srcset='#{image} 64w' style='width:36pt;height:12pt'>"
               )

      assert {:ok, tree} = Style.compute_detailed(dom, [], context.image_budget)
      assert {:ok, layout} = Layout.layout(tree, page_size: {120, 90}, margin: 10)
      source = Enum.find(layout.boxes, &(Map.get(&1, :role) == :responsive_image))
      assert source != nil
      clip = %{x: source.x, y: source.y, width: source.width / 2, height: source.height / 2}
      clipped = %{layout | boxes: DrawingGeometry.clip_boxes(layout.boxes, clip)}
      assert {:ok, resolved} = Layout.resolve_images(clipped)
      [paint] = Enum.filter(resolved.boxes, &(&1.type == :image))
      assert paint.width == source.width and paint.height == source.height
      assert paint.clip == clip
      refute Enum.any?(resolved.boxes, &(Map.get(&1, :role) == :responsive_image))
    end)
  end

  test "cross-page transform rejects invalid carried source paint canonically" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               HtmlParser.parse_document_detailed(
                 "<style>html,body{margin:0}section{width:30pt;height:20pt;background:red;transform:translateY(70pt)}</style><section></section>"
               )

      assert {:ok, tree} = Style.compute_detailed(dom, [], context.image_budget)
      assert {:ok, layout} = Layout.layout(tree, page_size: {100, 60}, margin: 0)
      pages = [%{size: layout.page_size, boxes: layout.boxes}]

      assert {:ok, projected} =
               CssTransformValidator.materialize(
                 pages,
                 [css_fragmentation_margins: layout.margins],
                 context
               )

      assert length(projected) >= 2

      pages =
        Enum.map(pages, fn page ->
          %{
            page
            | boxes:
                Enum.map(page.boxes, fn box ->
                  if box.type == :rect, do: %{box | fill_color: :invalid}, else: box
                end)
          }
        end)

      assert {:error, {:invalid_pdf_input, diagnostic}} =
               CssTransformValidator.materialize(
                 pages,
                 [css_fragmentation_margins: layout.margins],
                 context
               )

      assert is_binary(diagnostic.message) and diagnostic.message =~ "color"
    end)
  end
end
