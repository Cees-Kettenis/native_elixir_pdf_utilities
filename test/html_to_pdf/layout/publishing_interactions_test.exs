defmodule NativeElixirPdfUtilities.HtmlToPdf.PublishingInteractionsTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{CounterState, CounterStyle, PageGeometry}
  alias NativeElixirPdfUtilities.Validators.{CssCounterStyleValidator, CssPageValidator}
  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "page counters use the same authored values in body references and margin boxes" do
    html = """
    <style>@counter-style binary{system:numeric;symbols:'0' '1'}
    @page{size:240pt 180pt;margin:30pt;counter-increment:page 2;@top-center{content:counter(page,upper-roman)}}
    body{margin:0;counter-reset:section 5}.page::after{content:counter(page,upper-roman) '/' counter(page)}
    .ref::after{content:target-counter(attr(href),page) ' ' target-counter(attr(href),section,binary) ' ' target-text(attr(href),before) ' ' target-text(attr(href),first-letter)}
    h2{counter-increment:section;break-before:page}h2::before{content:'Prefix '}</style>
    <p class=page>Page:</p><a class=ref href='#chapter'>Reference:</a><h2 id=chapter>Chapter</h2>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Page:II/2"
    assert text =~ "Reference:4 110 Prefix C"
    assert text =~ "IV"
  end

  test "a standalone target resolves before fallback and keeps one accessible alternative" do
    html = """
    <style>body{font-family:Helvetica}.ref::before{content:target-text(attr(href));text-transform:uppercase}
    .alt::before{content:target-text(attr(href)) / 'Accessible label'}</style>
    <p><a class=ref href='#heading'></a></p><p><a class=alt href='#heading'></a></p>
    <h2 id=heading>Chapter Ж</h2>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "CHAPTER Ж"
    assert Regex.scan(~r/Accessible label/, text) == [["Accessible label"]]
    assert Regex.scan(~r/Chapter Ж/, text) == [["Chapter Ж"]]
  end

  test "a report converges with a deferred float, running caption, notes and cross-references" do
    html = """
    <style>@page{size:240pt 180pt;margin:25pt;
      @top-center{content:string(report)}
      @bottom-center{content:counter(page) '/' counter(pages)}}
    body{margin:0;font:10pt/12pt 'DejaVu Sans'}p,h1,h2{margin:0}
    h1{font:inherit;string-set:report 'Report header'}
    h2{font:inherit;break-before:page;bookmark-level:1}
    a::after{content:leader(dotted) target-counter(attr(href),page)}
    .float{float:block-end;float-reference:page;float-defer:1}
    .note{float:footnote}</style>
    <h1>Report title</h1><p><a href='#appendix'>Appendix entry</a></p>
    <div class=float>Deferred figure</div><p>Body<span class=note>Source note</span></p>
    <h2 id=appendix>Appendix heading</h2><p>Appendix body</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, %{pages: [first, second]}} = Text.extract_spans(pdf)
    first = Enum.map_join(first.spans, & &1.text)
    second = Enum.map_join(second.spans, & &1.text)
    assert first =~ ~r/Appendix entry[ .]+2/
    assert first =~ "Source note"
    assert first =~ "1/2"
    assert second =~ "Appendix heading"
    assert second =~ "Deferred figure"
    assert second =~ "2/2"
    assert first =~ "Report header"
    assert second =~ "Report header"
    assert pdf =~ "/Outlines"
  end

  test "all sixteen margin boxes paint independent content and constrained dimensions" do
    boxes =
      CssPageValidator.margin_boxes()
      |> Enum.with_index(1)
      |> Enum.map_join(fn {name, n} ->
        "@#{name}{content:'M#{n}';font:6pt/8pt 'DejaVu Sans';width:20pt;height:12pt;min-width:10pt;max-width:50pt;min-height:8pt;max-height:30pt}"
      end)

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<style>@page{size:300pt 240pt;margin:40pt;#{boxes}}body{margin:0}</style><p>Body</p>"
             )

    assert {:ok, text} = Text.extract(pdf)
    for n <- 1..16, do: assert(text =~ "M#{n}")

    assert PageGeometry.margin_axis(
             [%{fixed: 30.0}, %{generated: false, fixed: 0.0}, %{min: 10.0, max: 20.0}],
             100.0
           ) == [30.0, 0.0, 70.0]

    assert PageGeometry.margin_axis(
             [%{min: 10.0, max: 50.0}, %{generated: false, fixed: 0.0}, %{min: 20.0, max: 60.0}],
             60.0
           ) == [25.0, 0.0, 35.0]

    assert PageGeometry.margin_axis(
             [%{min: 40.0, max: 50.0}, %{generated: false, fixed: 0.0}, %{min: 40.0, max: 60.0}],
             60.0
           ) == [30.0, 0.0, 30.0]
  end

  test "running copy keeps source content and first-except suppresses assignment pages" do
    html = """
    <style>@page{size:240pt 180pt;margin:30pt;@top-center{content:element(chapter,start)}@bottom-center{content:string(label,first-except)}}
    body{margin:0;font:10pt/12pt 'DejaVu Sans'}h2{running:chapter;position:static;string-set:label 'Assigned';font:10pt/12pt 'DejaVu Sans';margin:0}section{break-before:page}</style>
    <h2>Heading</h2><section>Later page</section>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert length(Regex.scan(~r/Heading/, text)) == 3
    assert length(Regex.scan(~r/Assigned/, text)) == 1
  end

  test "generated margin images are loaded only for selected page rules" do
    png = File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/initial-small.png"))
    owner = self()

    resolver = fn %{reference: reference} ->
      send(owner, {:image, reference})
      {:ok, png}
    end

    html = """
    <style>@page{size:240pt 180pt;margin:30pt;@top-center{content:url('active.png')}}
    @page{@top-right{content:none;background-image:url('inhibited.png')}}
    @page unused{@bottom-center{content:url('dormant.png')}}body{margin:0}</style><p>Body</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, asset_resolver: resolver)
    assert pdf =~ "/Subtype /Image"
    assert_receive {:image, "active.png"}
    refute_receive {:image, "dormant.png"}
    refute_receive {:image, "inhibited.png"}
    assert {:error, {:invalid_document, %{stage: :asset}}} = HtmlToPdf.render(html)
  end

  test "a remote stylesheet base resolves a selected header image through the caller's resolver" do
    png = File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/initial-small.png"))
    owner = self()

    resolver = fn %{reference: reference} ->
      send(owner, {:image, reference})
      {:ok, png}
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<style>@page{margin:30pt;@top-center{content:url(logo.png)}}</style><p>Body</p>",
               base_url: "https://example.test/reports/index.html",
               asset_resolver: resolver
             )

    assert pdf =~ "/Subtype /Image"
    assert_receive {:image, "https://example.test/reports/logo.png"}
  end

  test "page backgrounds, margin images and footnote markers use their own generated content" do
    png = File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/initial-small.png"))
    owner = self()

    resolver = fn %{reference: reference} ->
      send(owner, {:image, reference})
      {:ok, png}
    end

    html = """
    <style>@page{size:240pt 180pt;margin:30pt;background-image:url(page.png);
      @top-center{content:url(header.png) / 'Header description';background-image:url(margin.png)}}
    .note{float:footnote}
    .note::footnote-call{content:url(call.png) / 'Note call'}
    .note::footnote-marker{content:url(marker.png) / 'Note marker'}</style>
    <p>Body<span class=note>Footnote text</span></p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, asset_resolver: resolver)
    assert pdf =~ "/Subtype /Image"
    assert {:ok, text} = Text.extract(pdf)

    for label <- ["Header description", "Note call", "Note marker", "Footnote text"],
        do: assert(text =~ label)

    for image <- ~w(page.png header.png margin.png call.png marker.png),
        do: assert_receive({:image, ^image})
  end

  test "named strings retain case-insensitive attribute names and nested counter text" do
    html = """
    <style>@page{size:240pt 180pt;margin:30pt;@top-center{content:open-quote string(chapter) close-quote}}
    main{counter-reset:section 2}.echo::after{content:string(chapter,last)} h2{counter-reset:section 4;
      string-set:chapter attr(DATA-NAME raw-string,'Default') ' ' counters(section,'.'), title content()}</style>
    <main><h2 data-name='Volume'>Heading</h2><p class=echo>Body: </p></main>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "“Volume 2.4”"
    assert text =~ "Body: Volume 2.4"
  end

  @tag :tmp_dir
  test "an authored header image resolves beside its stylesheet within the document root", %{
    tmp_dir: dir
  } do
    styles = Path.join(dir, "styles")
    File.mkdir!(styles)

    File.cp!(
      Paths.fixture("html_to_pdf/browser_parity/assets/initial-small.png"),
      Path.join(styles, "logo.png")
    )

    css = Path.join(styles, "report.css")
    File.write!(css, "@page report{@top-center{content:url(logo.png) / 'Report logo'}}")
    opts = [stylesheets: [{:file, css}]]

    assert {:ok, _} = HtmlToPdf.render("<p>Ordinary sheet</p>", opts)

    html = "<style>@page{margin:30pt}section{page:report}</style><section>Report</section>"
    assert {:error, {:resource_denied, _}} = HtmlToPdf.render(html, opts)
    assert {:ok, pdf} = HtmlToPdf.render(html, Keyword.put(opts, :base_url, dir))
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Report logo"
  end

  test "footnote line and block policies advance kept calls without dropping text" do
    for policy <- ["line", "block"] do
      html = """
      <style>@page{size:240pt 140pt;margin:20pt}body{margin:0;font:10pt/12pt 'DejaVu Sans'}p{margin:0}
      .space{height:60pt}.note{float:footnote;footnote-policy:#{policy}}</style>
      <div class=space>Earlier</div><p>Call<em><span class=note>LineA<br>LineB<br>LineC</span></em>End</p>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, document} = Text.extract_spans(pdf)
      assert length(document.pages) == 2
      assert {:ok, text} = Text.extract(pdf)
      assert length(Regex.scan(~r/Line[ABC]/, text)) == 3
      assert text =~ "Call"
    end
  end

  test "footnote call and marker content can be suppressed without dropping the note" do
    html = """
    <style>@page{size:240pt 180pt;margin:25pt}.note{float:footnote}
    .note::footnote-call,.note::footnote-marker{content:NONE}</style>
    <p>Call<span class=note>Note text</span>End</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "CallEnd"
    assert text =~ "Note text"
    refute text =~ "1"
  end

  test "block-policy footnotes at the root retain the call when they move" do
    html = """
    <style>@page{size:240pt 140pt;margin:20pt}body{margin:0;font:10pt/12pt 'DejaVu Sans'}
    .space{height:60pt}.note{float:footnote;footnote-policy:block}</style>
    <div class=space>Earlier</div>Call<span class=note>LineA<br>LineB<br>LineC</span>End
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, %{pages: [_, page]}} = Text.extract_spans(pdf)
    text = Enum.map_join(page.spans, & &1.text)
    assert text =~ "LineALineBLineC"
    assert text =~ "End"
  end

  test "fragment decoration policies, kept margins and avoided breaks preserve all text" do
    for decoration <- ["slice", "clone"], margin <- ["keep", "discard"] do
      html = """
      <style>@page{size:240pt 100pt;margin:20pt}body{margin:0;font:10pt/12pt 'DejaVu Sans'}
      .box{border:1pt solid red;padding:2pt;box-decoration-break:#{decoration};margin-break:#{margin};margin-top:6pt}
      p{margin:0;break-after:avoid}h2{margin:0;font:10pt/12pt 'DejaVu Sans';break-before:avoid}</style>
      <div class=box><p>One<br>Two<br>Three<br>Four<br>Five<br>Six</p><h2>Seven</h2></div>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)

      for word <- ~w(One Two Three Four Five Six Seven),
          do: assert(length(Regex.scan(Regex.compile!(word), text)) == 1)
    end
  end

  test "CSS bookmarks retain nested transform destinations and invalid references are recoverable" do
    html =
      "<style>h2{bookmark-level:1;bookmark-state:closed;transform:translate(5pt,4pt)}h2::before{content:'Before '}</style><h2>Chapter</h2>"

    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert pdf =~ "/Outlines"
    assert pdf =~ "Chapter"

    assert {:error, {:invalid_layout, _}} =
             HtmlToPdf.render(
               "<style>h2{bookmark-level:1;bookmark-label:target-text('#missing')}</style><h2>Chapter</h2>"
             )
  end

  test "custom style extension cycles, anonymous styles and zero representations terminate" do
    assert {:ok, styles} =
             CssCounterStyleValidator.prepare("""
             @counter-style roman{system:extends upper-roman;prefix:'[';suffix:']'}
             @counter-style first{system:extends second;prefix:'<' }@counter-style second{system:extends first;suffix:'>'}
             @counter-style numeric{system:numeric;symbols:'0' '1';negative:'~';range:auto;speak-as:numbers}
             """)

    assert CounterStyle.marker("roman", 4, styles) == "[IV]"
    assert CounterStyle.format("numeric", 0, styles) == "0"
    assert CounterStyle.format("numeric", -1, styles) == "~1"
    assert is_binary(CounterStyle.marker("first", 2, styles))
    assert {:ok, definition} = CssCounterStyleValidator.symbols("symbols(alphabetic 'a' 'b')")
    assert CounterStyle.marker({:custom_counter_style, definition}, 3, %{}) == "aa "
    assert CounterState.values(CounterState.new(), "missing") == [0]
  end

  test "footnote counter resets are shared by calls and markers in each page context" do
    for {reset, first, second} <- [
          {"body{counter-reset:footnote 4}", "5", "6"},
          {"@page{counter-reset:footnote 4}", "5", "5"},
          {"@page{counter-reset:footnote 4}.note{counter-increment:footnote 2}", "6", "6"}
        ] do
      html =
        "<style>@page{size:240pt 180pt;margin:30pt}#{reset}body{margin:0}.note{float:footnote}.note::footnote-call{content:'[' counter(footnote) ']'}section{break-before:page}</style><p>Call<span class=note>FirstNote</span></p><section>Call<span class=note>SecondNote</span></section>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "[#{first}]"
      assert text =~ "#{first}. FirstNote"
      assert text =~ "#{second}. SecondNote"
    end
  end

  test "deferred float queues continue in source order and last placement settles" do
    for defer <- ["1", "last", "-1"] do
      html =
        "<style>@page{size:240pt 120pt;margin:20pt}body{margin:0;font:10pt/12pt 'DejaVu Sans'}p{margin:0}.f{float:block-start;float-reference:page;float-defer:#{defer}}.g{float:block-start;float-reference:page}</style><div class=f>FloatA<br>A2<br>A3<br>A4</div><div class=g>FloatB<br>B2<br>B3<br>B4</div><p>Body<br>Body2<br>Body3<br>Body4<br>Body5</p>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)
      assert Regex.scan(~r/Float[AB]/, text) |> List.flatten() == ["FloatA", "FloatB"]
      for line <- ~w(A2 A3 A4 B2 B3 B4 Body2 Body3 Body4 Body5), do: assert(text =~ line)
    end
  end

  test "later declarations cancel publishing float and running position state" do
    html =
      "<style>@page{size:240pt 180pt;margin:30pt}.f{float:footnote;float:none}.r{position:running(header);position:static}</style><p class=f>Normal float</p><p class=r>Normal position</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Normal float"
    assert text =~ "Normal position"
    refute text =~ "1."
  end

  test "adjacent avoided boundaries move both blocks to the next page" do
    html =
      "<style>@page{size:240pt 100pt;margin:20pt}body{margin:0;font:10pt/12pt 'DejaVu Sans'}p{margin:0}.space{height:45pt}.first{break-after:avoid}</style><div class=space>Earlier</div><p class=first>KeptFirst</p><p>KeptSecond</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, document} = Text.extract_spans(pdf)
    assert length(document.pages) == 2
    [first, second] = document.pages
    refute Enum.map_join(first.spans, & &1.text) =~ "KeptFirst"
    assert Enum.map_join(second.spans, & &1.text) =~ "KeptFirst"
    assert Enum.map_join(second.spans, & &1.text) =~ "KeptSecond"
  end

  test "an oversized avoided boundary relaxes the keep and honors the following verso break" do
    html = """
    <style>@page{size:240pt 100pt;margin:20pt}body{margin:0;font:10pt/12pt 'DejaVu Sans'}
    p{margin:0}.first{break-after:avoid}.second{break-after:verso}</style>
    <p class=first>One<br>Two<br>Three<br>Four</p><p class=second>Five<br>Six<br>Seven</p>
    <p>Eight</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, %{pages: [first, second, blank, fourth]}} = Text.extract_spans(pdf)
    assert Enum.map_join(first.spans, & &1.text) =~ "Four"
    assert Enum.map_join(second.spans, & &1.text) =~ "Seven"
    assert blank.spans == []
    assert Enum.map_join(fourth.spans, & &1.text) == "Eight"
  end
end
