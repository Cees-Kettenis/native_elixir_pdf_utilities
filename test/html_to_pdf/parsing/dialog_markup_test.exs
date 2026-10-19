alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.DialogMarkupTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Text
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  test "dialog open is boolean and closed descendants do not request assets" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<dialog><img src=closed.png>Closed</dialog><dialog open=false>Open</dialog>",
               asset_resolver: fn request ->
                 flunk("closed dialog requested #{inspect(request)}")
               end
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Open"
    refute text =~ "Closed"

    assert {:ok, dom} =
             HtmlParser.parse_detailed("<dialog open=false>Open</dialog><dialog>Closed</dialog>")

    assert {:ok, %{children: [open, closed]}} = Style.compute_detailed(dom)
    assert open.style.position == :absolute
    assert open.style.width == :fit_content
    assert closed.style.display == :none
  end

  test "author display and position override the dialog defaults without modal state" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<dialog style='display:block;position:static;width:100pt'>Authored visible</dialog><dialog open hidden>Hidden</dialog>"
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Authored visible"
    refute text =~ "Hidden"
  end

  test "supplied dialogs enforce applicable attributes through the HTML validator" do
    dom = %{
      type: :document,
      children: [
        %{type: :element, tag: "dialog", attributes: %{"onclick" => "showModal()"}, children: []}
      ]
    }

    assert {:error, {:unsupported_html, diagnostic}} = Style.compute_detailed(dom)
    assert diagnostic.stage == :html
    assert diagnostic.source == "dialog"
    assert diagnostic.message =~ "attributes"
  end

  test "q generates nested automatic quotes and authored pseudo overrides" do
    html = "<p><q>Outer <q>Inner</q> tail</q> <q>Sibling</q></p>"
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "“Outer‘Inner’tail”“Sibling”"

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<style>q::before{content:'['}q::after{content:']'}</style><q>Override</q>"
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "[Override]"
  end

  test "quotes inheritance locale fallback suppression and depth words share document state" do
    html =
      "<style>p{quotes:'[' ']' '{' '}'}i::before{content:no-open-quote}i::after{content:no-close-quote}q.hide::before{display:none}</style><p><q>A<q>B<q>C</q></q></q><i><q>D</q></i><q class=hide>E</q><q>F</q></p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "[A{B{C}}]{D}E[F]"

    for {lang, expected} <- [
          {"fr-CA", "«A»"},
          {"de", "„A“"},
          {"not-a-locale", "“A”"}
        ] do
      assert {:ok, pdf} = HtmlToPdf.render("<q lang='#{lang}'>A</q>")
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert String.replace(text, ~r/\s/u, "") == expected
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<style>p::before{content:no-open-quote}q{quotes:'[' ']' '{' '}'}</style><p>A</p><section><q>B</q></section>"
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "A{B}"

    assert {:ok, dom} = HtmlParser.parse_detailed("<q lang='zh-Hant-TW'>A</q>")

    assert {:ok, %{children: [%{children: [opening, _text, closing]}]}} =
             Style.compute_detailed(dom)

    assert hd(opening.children).text == "「"
    assert hd(closing.children).text == "」"

    assert {:ok, pdf} = HtmlToPdf.render("<q style='quotes:none'>No<q>quotes</q></q>")
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "Noquotes"
  end

  test "malformed quotes declarations return CSS source diagnostics" do
    for value <- ["'unpaired'", "'left' right", "auto 'right'"] do
      assert {:error, {:invalid_css, diagnostic}} =
               HtmlToPdf.render("<q style=\"quotes:#{value}\">A</q>")

      assert diagnostic.stage == :css
      assert diagnostic.reason == :invalid_css
      assert diagnostic.source =~ "quotes:"
      assert diagnostic.message =~ "invalid"
    end
  end

  test "legacy font hints use fixed HTML sizes colors and font families before author CSS" do
    html =
      "<font size='+1' color=red face='DejaVu Sans Mono'>Hint</font><font size=-99>Small</font><font size=99>Large</font><font size=5 color=red style='font-size:11pt;color:blue'>Author</font>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [hint, small, large, author]}} = Style.compute_detailed(dom)
    assert hint.style.font_size == 13.5
    assert hint.style.color == {1.0, 0.0, 0.0}
    assert hint.style.font_family == "DejaVu Sans Mono"
    assert small.style.font_size == 7.5
    assert large.style.font_size == 36.0
    assert author.style.font_size == 11.0
    assert author.style.color == {0.0, 0.0, 1.0}
  end

  test "body legacy colors margins and unvisited link hints remain overridable" do
    html =
      "<body text=red bgcolor=yellow link=green marginwidth=4 marginheight=6><a href='https://example.test/'>Link</a><p>Text</p></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [body]}} = Style.compute_detailed(dom)
    assert body.style.color == {1.0, 0.0, 0.0}
    assert body.style.background_color == {1.0, 1.0, 0.0}
    assert body.style.margin == %{top: 4.5, bottom: 4.5, left: 3.0, right: 3.0}
    {red, green, blue} = hd(body.children).style.color
    assert red == 0 and blue == 0
    assert_in_delta green, 128 / 255, 0.000001

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<body background='javascript:run()'>Text</body>")

    assert diagnostic.stage == :html
    assert diagnostic.source == "background"
    assert diagnostic.message =~ "Active URL"
  end

  @tag :browser_parity
  @tag :dialog_markup
  test "authored dialog visibility positioning and sizing match untouched Chromium" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/html_dialog_initial.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_dialog_initial",
        render_opts: [forms: :static],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  @tag :browser_parity
  @tag :quote_markup
  test "q and authored quote depth match untouched Chromium" do
    fixture = Paths.fixture("html_to_pdf/browser_parity/html_q_quotes.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_q_quotes",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  @tag :browser_parity
  @tag :legacy_markup
  test "font and body presentation hints match untouched Chromium" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/html_font_body_hints.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_font_body_hints",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
