defmodule NativeElixirPdfUtilities.HtmlToPdf.PagedPublishingTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Validators.CssPageValidator

  test "all margin boxes are selectable and obey page-rule specificity" do
    rules = """
    @page{size:200pt 200pt;margin:30pt;@top-center{content:'General'}}
    @page:first{margin-top:40pt;@top-center{content:'First'}}
    @page report:left{margin-left:50pt}
    """

    assert {:ok, prepared} = CssPageValidator.rules(rules)

    assert {"margin-top", "40pt"} in CssPageValidator.select(prepared, "report", 1, false).declarations

    assert {"margin-left", "50pt"} in CssPageValidator.select(prepared, "report", 2, false).declarations

    assert length(CssPageValidator.margin_boxes()) == 16

    for name <- CssPageValidator.margin_boxes() do
      assert {:ok, [_]} = CssPageValidator.rules("@page{@#{name}{content:'X'}}")
    end
  end

  test "named page sizes and margin page counters survive public writing" do
    html = """
    <style>
    @page {size:200pt 180pt;margin:24pt;@bottom-center{content:counter(page) '/' counter(pages);font-size:8pt}}
    @page wide{size:300pt 180pt}
    body{margin:0;font-size:10pt;line-height:12pt}
    section{page:wide}
    </style><p>Portrait</p><section>Landscape</section><p>Portrait again</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert pdf =~ "/MediaBox [0 0 200 180]"
    assert pdf =~ "/MediaBox [0 0 300 180]"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "1/3"
    assert text =~ "2/3"
    assert text =~ "3/3"
  end

  test "named strings and captured running headers select page-local values" do
    html = """
    <style>
    @page {size:200pt 160pt;margin:24pt;@top-center{content:string(chapter)}@bottom-center{content:element(footer)}}
    body{margin:0;font-size:10pt;line-height:12pt}
    h2{string-set:chapter content(text);font-size:12pt;margin:0}
    .footer{position:running(footer)}
    .next{break-before:page}
    </style><div class=footer>Running footer</div><h2>Chapter one</h2><p>First body</p><h2 class=next>Chapter two</h2><p>Second body</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert length(Regex.scan(~r/Running footer/, text)) == 2
    assert length(Regex.scan(~r/Chapter one/, text)) == 2
    assert length(Regex.scan(~r/Chapter two/, text)) == 2
  end

  test "target references settle and missing targets return actionable diagnostics" do
    html = """
    <style>@page{size:200pt 180pt;margin:24pt}body{margin:0}.ref::after{content:' on page ' target-counter(attr(href),page)}section{break-before:page}</style>
    <a class=ref href='#chapter'>Read</a><section id=chapter>Chapter</section>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "on page 2"

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlToPdf.render(String.replace(html, "id=chapter", "id=other"))

    assert diagnostic.stage == :pagination
    assert diagnostic.message =~ "target"
  end

  test "recto breaks create intentional blank pages and blank furniture" do
    html =
      "<style>@page{size:200pt 160pt;margin:20pt}@page:blank{@top-center{content:'Blank'}}body{margin:0}section{break-before:recto}</style><p>First</p><section>Third</section>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, extracted} = Text.extract_spans(pdf)
    assert length(extracted.pages) == 3
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Blank"
  end

  test "physical and relative page breaks use the same spread as the printed side labels" do
    for {dir, expected} <- [
          {"ltr", [left: "L", right: "R", recto: "R", verso: "L"]},
          {"rtl", [left: "L", right: "R", recto: "L", verso: "R"]}
        ],
        {value, side} <- expected,
        {property, selector} <- [{"before", ".next"}, {"after", ".start"}] do
      html = """
      <html dir="#{dir}"><head><style>
      @page{size:160pt 140pt;margin:20pt}
      @page:left{@top-center{content:'L'}}@page:right{@top-center{content:'R'}}
      @page:blank{@bottom-center{content:'Blank'}}
      p{direction:ltr;margin:0}#{selector}{break-#{property}:#{value}}
      </style></head><body><p class=start>Start</p><p class=next>Next</p></body></html>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, %{pages: pages}} = Text.extract_spans(pdf)
      labels = Enum.map(pages, fn page -> Enum.map_join(page.spans, & &1.text) end)
      first = if dir == "ltr", do: "R", else: "L"
      assert length(labels) == if(first == side, do: 3, else: 2)
      assert hd(labels) =~ first
      assert hd(labels) =~ "Start"
      assert List.last(labels) =~ side
      assert List.last(labels) =~ "Next"
      if length(labels) == 3, do: assert(Enum.at(labels, 1) =~ "Blank")
    end
  end

  test "a leading forced break chooses the first printed side across named page geometry" do
    for selector <- ["html", "body", "section p"] do
      html = """
      <html><head><style>
      @page{size:160pt 140pt;margin:20pt;@bottom-center{content:counter(page) '/' counter(pages)}}
      @page:left{@top-left{content:'Left'}}@page:right{@top-left{content:'Right'}}
      @page:first{@top-right{content:'First'}}@page:blank{@top-right{content:'Blank'}}
      @page wide:right{size:220pt 140pt}#{selector}{break-before:left}
      .next{page:wide;break-before:recto}
      </style></head><body><section><p>Start</p></section><p class=next>Next</p></body></html>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert pdf =~ "/MediaBox [0 0 220 140]"
      assert {:ok, %{pages: [first, second]}} = Text.extract_spans(pdf)

      for marker <- ~w(Start Left First 1/2),
          do: assert(Enum.map_join(first.spans, & &1.text) =~ marker)

      for marker <- ~w(Next Right 2/2),
          do: assert(Enum.map_join(second.spans, & &1.text) =~ marker)
    end
  end

  test "vertical block flow defines recto independently of inline direction" do
    alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry

    for {mode, recto, verso} <- [
          {:vertical_rl, :left, :right},
          {:vertical_lr, :right, :left},
          {:sideways_rl, :left, :right},
          {:sideways_lr, :right, :left}
        ],
        direction <- [:ltr, :rtl],
        {initial, first} <- [left: :left, right: :right, recto: recto, verso: verso, page: verso] do
      assert %{recto: ^recto, first: ^first} =
               PageGeometry.prepare_spread(%{
                 children: [],
                 document_recto: PageGeometry.recto(%{writing_mode: mode, direction: direction}),
                 document_break_before: initial
               }).page_spread
    end

    for {css, label} <- [{"vertical-rl", "Left"}, {"vertical-lr", "Right"}] do
      html = """
      <style>@page{size:160pt 140pt;margin:20pt}
      @page:left{@top-center{content:'Left'}}@page:right{@top-center{content:'Right'}}
      body{writing-mode:#{css}}</style><body><p>A</p></body>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
      assert Enum.map_join(page.spans, & &1.text) =~ label
    end
  end

  test "leading in-flow content determines the spread without page furniture" do
    html = """
    <html dir="rtl"><body>
    <div style="position:absolute">Positioned</div><div style="float:left">Float</div>
    <section><p style="break-before:right">Start</p><p style="break-before:recto">Next</p></section>
    </body></html>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, %{pages: [first, second]}} = Text.extract_spans(pdf)
    assert Enum.map_join(first.spans, & &1.text) =~ "Start"
    assert Enum.map_join(second.spans, & &1.text) =~ "Next"
  end

  test "pagination diagnoses malformed page-side metadata on supplied layout trees" do
    layout = %{type: :layout, boxes: [], page_size: {100, 100}, page_spread: %{recto: :left}}

    assert {:error, {:invalid_layout, %{source: "page_spread", operation: :paginate}}} =
             NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(layout)
  end

  test "a forced blank sheet uses its own geometry during mixed-width pagination" do
    html = """
    <style>@page{size:220pt 160pt;margin:20pt}
    @page:left{size:250pt 160pt}
    @page:blank{size:300pt 180pt;@top-center{content:'Blank sheet'}}
    body{margin:0}section{break-before:recto}</style>
    <p>First sheet</p><section>Third sheet</section>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/MediaBox [0 0 300 180]"
    assert {:ok, %{pages: [first, blank, third]}} = Text.extract_spans(pdf)
    assert Enum.map_join(first.spans, & &1.text) =~ "First sheet"
    assert Enum.map_join(blank.spans, & &1.text) == "Blank sheet"
    assert Enum.map_join(third.spans, & &1.text) =~ "Third sheet"
  end

  test "page margin boxes paint on an empty document" do
    css = "<style>@page{size:200pt 160pt;margin:25pt;@top-center{content:'Empty report'}}</style>"

    for html <- [css, "<html style='display:none'><head>#{css}</head><body>Hidden</body></html>"] do
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
      assert Enum.map_join(page.spans, & &1.text) == "Empty report"
    end
  end

  test "incompatible float offsets and footnote areas fail with pagination diagnostics" do
    html = """
    <style>@page{size:200pt 160pt;margin:20pt}
    .figure{float:bottom;float-reference:page;float-offset:100%}</style>
    <p>Body</p><div class=figure>Figure</div>
    """

    assert {:error, {:invalid_layout, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :pagination
    assert diagnostic.operation == :paginate
    assert diagnostic.message =~ "leave no room"

    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render("""
             <style>@page{@footnote{max-height:broken}}.note{float:footnote}</style>
             <p>Body<span class=note>Note</span></p>
             """)

    assert diagnostic.source =~ "max-height"
  end

  test "footnotes remain with their calls and retain all text across continuation" do
    notes = Enum.map_join(1..24, "<br>", &"Note line #{&1}")

    html =
      "<style>@page{size:200pt 160pt;margin:20pt}body{margin:0;font-size:10pt;line-height:12pt}.note{float:footnote;font-size:8pt;line-height:10pt}</style><p>Body call<span class=note>#{notes}</span> after call.</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    for n <- 1..24, do: assert(text =~ "Note line #{n}")
    assert text =~ "Body call"
    assert text =~ "after call."
  end

  test "continuing a note onto different page geometry returns a pagination diagnostic" do
    notes = Enum.map_join(1..12, "<br>", &"Note line #{&1}")

    html = """
    <style>@page{size:200pt 160pt;margin:20pt;@footnote{max-height:30pt}}
    @page:left{size:300pt 180pt;margin-left:40pt}body{margin:0}
    .note{float:footnote;font:8pt/10pt 'DejaVu Sans'}</style>
    <p>Body<span class=note>#{notes}</span></p>
    """

    assert {:error, {:invalid_layout, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :pagination
    assert diagnostic.operation == :paginate
    assert diagnostic.message =~ "different page geometry"
  end

  test "top and bottom page floats reserve body space and keep text" do
    html =
      "<style>@page{size:200pt 180pt;margin:20pt}body{margin:0;font-size:10pt;line-height:12pt}.top{float:top;float-reference:page}.bottom{float:bottom;float-reference:page}</style><div class=top>Top float</div><p>Body text</p><div class=bottom>Bottom float</div>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Top float"
    assert text =~ "Bottom float"
    assert text =~ "Body text"
  end

  test "print bleed and marks emit sheet, trim and bleed boxes and CSS bookmarks" do
    html =
      "<style>@page{size:200pt 180pt;margin:20pt;bleed:3pt;marks:crop cross}h2{bookmark-level:1;bookmark-label:content(text);bookmark-state:closed}</style><h2>Bookmarked chapter</h2>"

    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert pdf =~ "/MediaBox [0 0 236 216]"
    assert pdf =~ "/TrimBox [18 18 218 198]"
    assert pdf =~ "/BleedBox [15 15 221 201]"
    assert pdf =~ "/Outlines"
  end

  test "page counter overrides and margin counter scopes stay independent" do
    html =
      "<style>@page{size:200pt 160pt;margin:20pt;counter-increment:page 2;@top-left{content:counter(page)}@top-right{counter-reset:page 90;content:counter(page)}}body{margin:0}section{break-before:page}</style><p>First body</p><section>Second body</section>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "90"
    assert text =~ "2"
    assert text =~ "4"
  end

  test "footnote call and note marker pseudo styles retain custom numbering" do
    html =
      "<style>@page{size:200pt 160pt;margin:20pt}.note{float:footnote}.note::footnote-call{content:'[' counter(footnote) ']';color:red}.note::footnote-marker{content:counter(footnote) ') '}</style><p>Body<span class=note>Custom note</span></p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "[1]"
    assert text =~ "1) Custom note"
  end

  test "margin box sizing follows independent specification vectors" do
    alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry

    assert PageGeometry.margin_axis(
             [%{min: 10.0, max: 20.0}, %{generated: false, fixed: 0.0}, %{min: 20.0, max: 40.0}],
             180.0
           ) == [60.0, 0.0, 120.0]

    assert PageGeometry.margin_axis(
             [%{min: 10.0, max: 20.0}, %{min: 10.0, max: 20.0}, %{min: 10.0, max: 20.0}],
             180.0
           ) == [60.0, 60.0, 60.0]

    assert PageGeometry.margin_axis([%{fixed: 30.0}, %{fixed: 80.0}, %{fixed: 20.0}], 180.0) == [
             30.0,
             80.0,
             20.0
           ]
  end

  test "long paragraphs reflow when page selectors change the printable width" do
    words = Enum.map_join(1..100, " ", &"word#{&1}")

    html =
      "<style>@page{size:200pt 100pt;margin:20pt}@page:left{size:300pt 120pt}body{margin:0;font-size:10pt;line-height:12pt}p{margin:0;box-decoration-break:clone;padding:1pt;border:1pt solid black}</style><p>#{words}</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert pdf =~ "/MediaBox [0 0 200 100]"
    assert pdf =~ "/MediaBox [0 0 300 120]"
    assert {:ok, text} = Text.extract(pdf)
    found = Regex.scan(~r/word[0-9]+/, text) |> List.flatten()
    assert found == Enum.map(1..100, &"word#{&1}")
  end

  test "mixed page widths diagnose an oversized empty allocation that cannot advance" do
    html = """
    <style>@page{size:200pt 100pt;margin:20pt}@page:left{size:300pt 120pt}
    body{margin:0}.empty{height:320pt}</style><div class=empty></div><p>After</p>
    """

    assert {:error, {:invalid_layout, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :pagination
    assert diagnostic.message =~ "could not advance"
  end

  test "leaders occupy the remaining table-of-contents line" do
    html =
      "<style>@page{size:240pt 180pt;margin:20pt}body{margin:0;font-size:10pt}.toc::after{content:leader(dotted) target-counter(attr(href),page)}section{break-before:page}</style><p><a class=toc href='#chapter'>Chapter</a></p><section id=chapter>Target</section>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "........"
    assert text =~ "2"
  end

  test "publishing tab stops retain source text and one accessible generated alternative" do
    html = """
    <style>@page{size:240pt 180pt;margin:20pt;@top-center{content:counter(page)}}
    pre::before{content:'\\9 Label' / 'Accessible'}</style>
    <pre>Tab\tEnd</pre>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Tab"
    assert text =~ "End"
    assert Regex.scan(~r/Accessible/, text) == [["Accessible"]]
  end

  test "multiple footnotes retain document order at the bottom of their call page" do
    html =
      "<style>@page{size:220pt 180pt;margin:20pt}body{margin:0;font-size:10pt;line-height:12pt}.note{float:footnote}</style><p>One<span class=note>Note A</span> and two<span class=note>Note B</span>.</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert Regex.scan(~r/Note [AB]/, text) |> List.flatten() == ["Note A", "Note B"]
  end

  test "column-reference deferred floats use their destination column" do
    html =
      "<style>@page{size:300pt 200pt;margin:20pt}body{margin:0;font-size:10pt;line-height:12pt}.columns{columns:2;column-gap:10pt;height:120pt;column-fill:auto}.float{float:top;float-reference:column;float-defer:1;float-offset:4pt}</style><div class=columns><div class=float>DeferredFloat</div><p>Body text in the first column.</p></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, extracted} = Text.extract_spans(pdf)
    span = hd(extracted.pages).spans |> Enum.find(&(&1.text == "D" and &1.x >= 150))
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "DeferredFloat"
    assert span != nil
    assert span.x >= 150
  end

  test "authored footnote areas cap continuation and preserve every note line" do
    lines = Enum.map_join(1..12, "<br>", &"AreaNote#{&1}")

    html =
      "<style>@page{size:220pt 180pt;margin:20pt;@footnote{max-height:30pt;border-top:0.5pt solid black}}body{margin:0;font-size:10pt;line-height:12pt}.note{float:footnote;font-size:8pt;line-height:10pt}</style><p>Call<span class=note>#{lines}</span></p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)

    assert Regex.scan(~r/AreaNote[0-9]+/, text) |> List.flatten() ==
             Enum.map(1..12, &"AreaNote#{&1}")

    assert {:ok, extracted} = Text.extract_spans(pdf)
    assert length(extracted.pages) >= 4
  end

  test "inline notes share a row and symbolic footnote counters are supported" do
    html =
      "<style>@page{size:220pt 180pt;margin:20pt}body{margin:0;font-size:10pt;line-height:12pt}.note{float:footnote;footnote-display:inline}.note::footnote-call{content:counter(footnote,symbols('*','†'))}.note::footnote-marker{content:counter(footnote,symbols('*','†')) ' '}</style><p>First<span class=note>AlphaNote</span> Second<span class=note>BetaNote</span></p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "AlphaNote"
    assert text =~ "BetaNote"
    assert text =~ "*"
    assert text =~ "†"
    assert {:ok, extracted} = Text.extract_spans(pdf)
    alpha = Enum.find(hd(extracted.pages).spans, &(&1.text == "A"))
    beta = Enum.find(hd(extracted.pages).spans, &(&1.text == "B"))
    assert_in_delta alpha.y, beta.y, 0.01
  end

  test "page dimensions resolve font units, math and side-specific percentages without overriding API geometry" do
    html = """
    <style>@page{font-size:10pt;size:calc(20 * 1em) 160pt;margin:10% calc(5% + 2pt);
      @top-center{content:'Header'}}body{margin:0;font-size:10pt}p{margin:0}</style><p>Body</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert pdf =~ "/MediaBox [0 0 200 160]"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, page_size: {240, 180}, margin: 20, compress_streams: false)

    assert pdf =~ "/MediaBox [0 0 240 180]"
  end

  test "start strings retain incoming values until an assignment at the page start" do
    html = """
    <style>@page{size:240pt 180pt;margin:24pt;@top-left{content:'START=' string(chapter,start)}
      @top-right{content:'LAST=' string(chapter,last)}}body{margin:0;font-size:10pt;line-height:12pt}
      h2{string-set:chapter content(text);font-size:10pt;line-height:12pt;margin:0}
      section{break-before:page}p{margin:0}</style>
    <h2>Alpha</h2><section><p>Earlier body</p><h2>Beta</h2></section><section><h2>Gamma</h2></section>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "START=Alpha"
    assert text =~ "LAST=Beta"
    assert text =~ "START=Gamma"
    refute text =~ "START=Beta"
  end

  test "page border and padding inset body content independently of margin boxes" do
    html = """
    <style>@page{size:240pt 180pt;margin:20pt;border:2pt solid black;padding:calc(10% / 3);
      @top-center{content:'Header'}}body{margin:0;font-size:10pt;line-height:12pt}p{margin:0}</style><p>Body</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, extracted} = Text.extract_spans(pdf)
    body = Enum.find(hd(extracted.pages).spans, &(&1.text == "B"))
    assert_in_delta body.x, 30.0, 0.01
  end

  test "published transforms retain the visible word and link on its destination page" do
    for extra <- [
          "",
          "@page :first{margin:10pt}",
          "@page report{size:200pt 230pt;margin:15pt}.second{page:report}"
        ],
        {distance, destination} <- [{90, 1}, {115, 2}] do
      html = """
      <style>@page{size:200pt 130pt;margin:10pt}body{margin:0}#{extra}</style>
      <div style="transform:translateY(#{distance}pt)"><a href="https://example.com/moved">MOVED</a></div>
      <section class="second" style="break-before:page">SECOND</section>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, %{pages: pages, page_count: 2}} = Text.extract_spans(pdf)

      for %{number: number, media_box: [0, 0, _, height], spans: spans} <- pages do
        visible_text =
          spans
          |> Enum.filter(&(&1.y >= 0 and &1.y <= height))
          |> Enum.map_join(& &1.text)

        assert String.contains?(visible_text, "MOVED") == (number == destination)
        assert String.contains?(visible_text, "SECOND") == (number == 2)
      end

      assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

      for {page, number} <- Enum.with_index(context.pages, 1) do
        assert length(Map.get(page.dictionary, "Annots", [])) ==
                 if(number == destination, do: 1, else: 0)
      end
    end
  end

  test "page orientation rotates editable widget bounds and preserves static artwork" do
    render = fn orientation, mode ->
      html = """
      <style>@page{size:200pt 150pt;margin:18pt;page-orientation:#{orientation}}
      body{margin:0}input{width:70pt;height:20pt}</style>
      <input name="field" value="Initial"><a href="https://example.com">Link</a>
      """

      HtmlToPdf.render(html, forms: mode)
    end

    assert {:ok, upright} = render.("upright", :interactive)
    assert {:ok, [%{widgets: [%{rect: [left, bottom, right, top]}]}]} = Forms.fields(upright)

    for {orientation, expected} <- [
          {"rotate-left", [150 - top, left, 150 - bottom, right]},
          {"rotate-right", [bottom, 200 - right, top, 200 - left]}
        ] do
      assert {:ok, pdf} = render.(orientation, :interactive)

      assert {:ok, [%{name: "field", value: "Initial", widgets: [%{page: 1, rect: actual}]}]} =
               Forms.fields(pdf)

      for {a, b} <- Enum.zip(actual, expected), do: assert_in_delta(a, b, 0.001)
      assert pdf =~ "/Subtype /Link"
      assert {:ok, filled} = Forms.fill(pdf, %{"field" => "Updated"})
      assert {:ok, flat} = Forms.flatten(filled)
      assert {:ok, text} = Text.extract(flat, layout: false)
      assert text =~ "Updated"

      assert {:ok, static} = render.(orientation, :static)
      assert {:ok, []} = Forms.fields(static)
      assert {:ok, text} = Text.extract(static, layout: false)
      assert text =~ "Initial"
      assert static =~ "/Subtype /Link"
    end
  end

  test "rotated marked pages retain valid physical trim and bleed boxes" do
    for orientation <- ["rotate-left", "rotate-right"] do
      html =
        "<style>@page{size:200pt 160pt;margin:20pt;bleed:4pt;marks:crop;page-orientation:#{orientation}}</style><p>Rotated</p>"

      assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
      assert pdf =~ "/MediaBox [0 0 196 236]"
      assert pdf =~ "/TrimBox [18 18 178 218]"
      assert pdf =~ "/BleedBox [14 14 182 222]"
    end
  end

  test "invalid footnote area and leader targets return diagnostics" do
    assert {:error, {:invalid_layout, %{message: message}}} =
             HtmlToPdf.render(
               "<style>@page{size:200pt 180pt;margin:20pt;@footnote{max-height:1pt}}.n{float:footnote}</style><p>Call<span class=n>Note</span></p>"
             )

    assert message =~ "one line"

    assert {:error, {:invalid_layout, _}} =
             HtmlToPdf.render(
               "<style>a::after{content:leader(dotted) target-counter(attr(href),page)}</style><a href='#missing'>Missing</a>"
             )
  end

  test "uppercase orientation rotates without marks and computed bleed rejects negative lengths" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<style>@page{size:200pt 160pt;marks:initial;page-orientation:ROTATE-RIGHT}</style><p>Rotated</p>"
             )

    assert pdf =~ "/MediaBox [0 0 160 200]"
    refute pdf =~ "/TrimBox"

    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render("<style>@page{bleed:calc(1pt - 2pt)}</style><p>Body</p>")

    assert diagnostic.message =~ "nonnegative length"
  end
end
