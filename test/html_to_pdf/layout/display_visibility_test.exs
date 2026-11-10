defmodule NativeElixirPdfUtilities.HtmlToPdf.DisplayVisibilityTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Forms}
  alias NativeElixirPdfUtilities.TestSupport.CssStyles
  alias NativeElixirPdfUtilities.Validators.CssDisplayValidator

  # CSS Display 4, 6 November 2025, sections 2 and 4.
  test "display outer and inner grammar preserves independent formatting contexts" do
    for {source, display, root?} <- [
          {"block flow", :block, false},
          {"inline flow", :inline, false},
          {"block flow-root", :block, true},
          {"inline flow-root", :inline_block, true},
          {"flow-root", :block, true},
          {"inline flex", :inline_flex, true},
          {"grid block", :grid, true},
          {"table inline", :inline_table, true},
          {"flow-root list-item", :list_item, true},
          {"flow", :block, false}
        ] do
      assert {:ok, properties} = CssDisplayValidator.prepare(source)
      assert properties.display == display
      assert Map.get(properties, :_flow_root, false) == root?
    end

    for source <- [
          "",
          "block inline",
          "inline grid flex",
          "grid grid",
          "none block",
          "flow list-item flex"
        ] do
      assert :error = CssDisplayValidator.prepare(source), source
    end

    styles =
      CssStyles.by_id(
        "<div id='root' style='display:block flow-root'></div><div id='inline' style='display:inline flow-root'></div>"
      )

    assert styles["root"].display == :block and styles["root"]._flow_root
    assert styles["inline"].display == :inline_block
  end

  test "flow roots contain child margins while ordinary blocks collapse" do
    render = fn display ->
      html =
        "<body style='margin:0;font-size:10pt;line-height:12pt'><div style='display:#{display};background:red'><div style='margin-top:20pt'>First</div></div><p style='margin:0'>After</p></body>"

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 page_size: {200, 200},
                 margin: 0,
                 system_font_discovery: false
               )

      assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
      Enum.find(page.spans, &String.starts_with?(&1.text, "A")).y
    end

    # Both place the text at the same physical y, but the independent container
    # retains its own zero top margin and paints the child's 20pt inset.
    assert_in_delta render.("flow-root"), render.("block"), 0.01
  end

  test "orphan CSS table roles gain anonymous parents without losing source content" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

    html =
      "<div><span style='display:table-cell;width:40pt'>Left</span><span style='display:table-cell;width:40pt'>Right</span></div><p>After</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, result} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
    left = Enum.find(result.boxes, &(Map.get(&1, :text) == "Left"))
    right = Enum.find(result.boxes, &(Map.get(&1, :text) == "Right"))
    assert left.y == right.y and right.x > left.x
    assert Enum.any?(result.boxes, &(Map.get(&1, :text) == "After"))
    assert {:ok, _pdf} = HtmlToPdf.render(html, system_font_discovery: false)
  end

  test "authored CSS table header and footer roles retain their section owner" do
    for {role, section} <- [
          {"table-header-group", :head},
          {"table-row-group", :body},
          {"table-footer-group", :foot}
        ] do
      style =
        CssStyles.by_id(
          "<div id=a style='display:#{role}'><div style='display:table-row'><span style='display:table-cell'>Content</span></div></div>"
        )["a"]

      assert style.display == :table_row_group and style.table_section == section

      assert {:ok, _pdf} =
               HtmlToPdf.render(
                 "<div style='display:#{role}'><div style='display:table-row'><span style='display:table-cell'>Content</span></div></div>",
                 system_font_discovery: false
               )
    end
  end

  test "flow roots paint child margin allocation inside their independent border box" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

    for {display, top, height} <- [{"flow-root", 200, 30}, {"block", 182, 12}] do
      html =
        "<div style='display:#{display};font-size:10pt;line-height:12pt;background:red'><div style='margin-top:18pt'>First</div></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
      red = Enum.find(result.boxes, &(Map.get(&1, :fill_color) == {1, 0, 0}))
      assert_in_delta red.y + red.height, top, 0.01
      assert_in_delta red.height, height, 0.01
    end
  end

  test "hidden artwork keeps allocation and visible descendants can override inherited visibility" do
    html =
      "<body style='margin:0;font-size:10pt;line-height:12pt'><div style='visibility:hidden;height:30pt;background:red'><span>Hidden</span><span style='visibility:visible'>Shown</span></div><div>After</div></body>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               page_size: {200, 200},
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
    text = Enum.map_join(page.spans, & &1.text)
    refute text =~ "Hidden"
    assert text =~ "Shown" and text =~ "After"
    after_span = Enum.find(page.spans, &String.starts_with?(&1.text, "A"))
    assert after_span.y > 30
  end

  test "hidden controls and links create no PDF fields or clickable regions" do
    html =
      "<body><div style='visibility:hidden'><a href='https://example.com'>Hidden link</a><input name='secret' value='Hidden field'></div><input name='shown' value='Visible'><p>After</p></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, fields} = Forms.fields(pdf)
    assert Enum.map(fields, & &1.name) == ["shown"]
    assert :binary.matches(pdf, "/Subtype /Link") == []
    assert {:ok, text} = Text.extract(pdf)
    refute text =~ "Hidden"
  end

  test "hidden Math glyphs and rules preserve sizing and permit visible child overrides" do
    for source <- [
          "<math><mn style='visibility:hidden'>1</mn><mn>2</mn></math>",
          "<math style='visibility:hidden'><mfrac><mn>1</mn><mn style='visibility:visible'>2</mn></mfrac></math>",
          "<math><mi><span style='visibility:hidden'>Hidden</span><span>Shown</span></mi></math>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(source <> "<p>After</p>", system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      refute text =~ "1"
      refute text =~ "Hidden"
      assert text =~ "2" or text =~ "Shown"
      assert text =~ "After"
    end
  end

  test "HTML visibility enters an SVG scene while visible SVG descendants retain their own ink" do
    image =
      NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(
        "<div style='visibility:hidden'><svg width=20 height=10><rect width=20 height=10 fill=red /><rect x=10 width=10 height=10 fill=blue visibility=visible /></svg></div>"
      )

    for y <- 0..(image.height - 1), x <- 0..(image.width - 1) do
      alpha = :binary.at(image.alpha, y * image.width + x)
      if x < div(image.width, 2), do: assert(alpha == 0), else: assert(alpha == 255)
    end
  end

  test "forced invisibility keeps descendant overrides and controls invisible while retaining flow" do
    html =
      "<div style='visibility:force-hidden;height:30pt'><span style='visibility:visible'>Hidden</span><math><mn style='visibility:visible'>1</mn></math><input name=secret style='visibility:visible'></div><p>After</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    refute text =~ "Hidden"
    refute text =~ "1"
    assert text =~ "After"
    assert {:ok, []} = Forms.fields(pdf)
  end

  test "hidden embedded viewports suppress their complete independent document surface" do
    html =
      "<body><iframe style='visibility:hidden;width:100pt;height:40pt' srcdoc=\"<body>Child hidden<input name='secret'></body>\"></iframe><div>After</div></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    refute text =~ "Child"
    assert text =~ "After"
    assert {:ok, []} = Forms.fields(pdf)
  end

  test "equivalent ruby and list display grammars retain their authored roles" do
    for source <- [
          "ruby",
          "inline ruby",
          "block ruby",
          "ruby-base",
          "ruby-text",
          "ruby-base-container",
          "ruby-text-container",
          "block list-item",
          "inline list-item",
          "inline flow-root list-item"
        ] do
      assert {:ok, _} = CssDisplayValidator.prepare(source)

      assert {:ok, _} =
               HtmlToPdf.render("<div style='display:#{source}'>Content</div>",
                 system_font_discovery: false
               )
    end

    for source <- ["42", "run-in grid list-item", "run-in unknown", "list-item list-item"] do
      assert :error = CssDisplayValidator.prepare(source)
    end

    assert {:ok, :collapse} = CssDisplayValidator.visibility("collapse")
    assert :error = CssDisplayValidator.visibility("12")
  end

  test "anonymous table formation retains ordinary and hidden siblings around CSS cells" do
    html =
      "<div>Before<span style='display:table-cell'>Left</span> <span style='display:none'>Hidden</span><span style='display:table-cell'>Right</span>After</div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before" and text =~ "Left" and text =~ "Right" and text =~ "After"
    refute text =~ "Hidden"
  end

  test "inline list items share their surrounding line and preserve the prepared marker" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

    html =
      "<div>Before <span style='display:inline list-item;list-style-type:decimal'>Item</span> After</div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, result} = Layout.layout(tree, page_size: {400, 200}, margin: 0)
    text = Enum.filter(result.boxes, &(&1.type == :text))
    assert Enum.map(text, & &1.y) |> Enum.uniq() |> length() == 1
    assert Enum.map_join(text, & &1.text) =~ "1."
    assert Enum.map_join(text, & &1.text) =~ "Item"
  end

  test "run-ins enter the following paragraph without changing authored inheritance" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

    for content <- [
          "<p style='margin:0'>Paragraph</p>",
          "<div><p style='margin:0'>Paragraph</p></div>"
        ] do
      html =
        "<div style='font:10pt sans-serif;line-height:12pt'><span style='display:run-in;color:red'>Label </span>#{content}</div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(tree, page_size: {300, 200}, margin: 0)
      texts = Enum.filter(result.boxes, &(&1.type == :text))
      label = Enum.find(texts, &(&1.text =~ "Label"))
      paragraph = Enum.find(texts, &(&1.text =~ "Paragraph"))
      assert label.y == paragraph.y and paragraph.x > label.x
      assert label.color == {1, 0, 0} and paragraph.color == {0, 0, 0}
    end
  end

  test "inline tables share their first row baseline and retain their native table measurements" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

    for width <- ["", "width:60pt;"] do
      html =
        "<div style='font:10pt sans-serif;line-height:12pt'>Before<table style='display:inline table;#{width}border-spacing:0'><tr><td style='padding:0;vertical-align:baseline'>Cell</td><td style='padding:0;vertical-align:baseline'>Peer</td></tr><tr><td style='padding:0;vertical-align:baseline'>Second</td><td style='padding:0;vertical-align:baseline'>Row</td></tr></table>After</div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(tree, page_size: {300, 200}, margin: 0)
      text = Enum.filter(result.boxes, &(&1.type == :text))
      before_box = Enum.find(text, &(&1.text == "Before"))
      cell = Enum.find(text, &(&1.text == "Cell"))
      peer = Enum.find(text, &(&1.text == "Peer"))
      after_box = Enum.find(text, &(&1.text == "After"))
      assert_in_delta before_box.y, cell.y, 0.75
      assert_in_delta peer.y, cell.y, 0.75
      assert_in_delta after_box.y, cell.y, 0.75
      assert peer.x > cell.x and after_box.x > peer.x
    end
  end

  test "empty inline tables retain surrounding inline content without phantom tracks" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<p>Before<span style='display:inline-table'></span>After</p>",
               system_font_discovery: false
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before" and text =~ "After"
  end

  test "orphan ruby roles form an owner and explicit containers pair base and annotation columns" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

    for body <- [
          "<span style='display:ruby-base'>Base</span><span style='display:ruby-text;font-size:6pt'>Read</span>",
          "<span style='display:ruby-base-container'><span style='display:ruby-base'>Base</span></span><span style='display:ruby-text-container'><span style='display:ruby-text;font-size:6pt'>Read</span></span>",
          "<span style='display:ruby'><span style='display:ruby-base-container'><span style='display:ruby-base'>Base</span></span><span style='display:ruby-text-container'><span style='display:ruby-text;font-size:6pt'>Read</span></span></span>"
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<p style='font:12pt sans-serif'>Before #{body} After</p>"
               )

      assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(tree, page_size: {200, 200}, margin: 0)
      base = Enum.find(result.boxes, &(Map.get(&1, :text) == "Base"))
      reading = Enum.find(result.boxes, &(Map.get(&1, :text) == "Read"))
      assert reading.y > base.y
      assert_in_delta base.x + base.width / 2, reading.x + reading.width / 2, 0.1
    end
  end

  test "run-in grammar and reparenting retain content across formatting-context boundaries" do
    for display <- [
          "run-in flow",
          "run-in flex",
          "run-in grid",
          "run-in table",
          "run-in flow list-item",
          "run-in flow-root list-item"
        ] do
      assert {:ok, _} = CssDisplayValidator.prepare(display)
    end

    for inner <- [
          "<div>Block</div>",
          "<div style='display:flow-root'>Root</div>",
          "<div style='display:flex'>Flex</div>",
          "<div style='display:grid'>Grid</div>",
          "<table><tr><td>Table</td></tr></table>"
        ] do
      for following <- [
            "<div>Following</div>",
            "<div style='display:flow-root'>Following</div>",
            "<span>Following</span>"
          ] do
        html =
          "<p>Before</p><div style='display:run-in'>#{inner}</div> <span style='position:absolute;left:130pt;top:0'>Positioned</span>#{following}"

        assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
        assert {:ok, text} = Text.extract(pdf)
        assert text =~ "Before" and text =~ "Following" and text =~ "Positioned"
      end
    end
  end

  test "force-hidden prevents visible SVG descendants from restoring paint" do
    computed =
      CssDisplayValidator.compute_style(%{
        visibility: :visible,
        svg_visibility: :visible,
        _cascade_inherited: %{_force_hidden: true}
      })

    assert computed.svg_visibility == :hidden

    assert NativeElixirPdfUtilities.Validators.CssCascadeValidator.primary_field("visibility", %{
             _svg_element: true
           }) == [:svg_visibility]

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<div style='visibility:force-hidden'><svg width=10 height=10><rect width=10 height=10 visibility=visible fill=red /></svg></div><p>After</p>",
               system_font_discovery: false
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "After"
  end

  test "run-in sequences retain intervening out-of-flow content and explicit ruby containers keep unmatched columns" do
    for html <- [
          "<div style='display:run-in'>First</div> <span style='position:absolute;left:130pt'>Positioned</span><div style='display:run-in'>Second</div><p>Following</p>",
          "<span style='display:ruby'><span style='display:ruby-base-container'></span></span>",
          "<span style='display:ruby'><span style='display:ruby-base-container'><span style='display:ruby-base'>A</span><span style='display:ruby-base'>B</span></span><span style='display:ruby-text-container'><span style='display:ruby-text'>a</span></span><span style='display:ruby-text-container'><span style='display:ruby-text'>alpha</span><span style='display:ruby-text'>beta</span></span></span>"
        ] do
      assert {:ok, _pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    end
  end

  test "inline table intrinsic sizing ignores cyclic percentage tracks before allocating used widths" do
    for extra <- ["", "border-collapse:collapse"] do
      html =
        "<div>Before<table style='display:inline-table;#{extra}'><colgroup><col style='width:50%'><col></colgroup><tr><td style='width:50%'>First</td><td>Last</td></tr></table>After</div>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "First" and text =~ "Last" and text =~ "After"
    end
  end
end
