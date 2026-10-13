defmodule NativeElixirPdfUtilities.HtmlToPdf.LegacyMarkupFollowupTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  test "abbreviation titles add dotted artwork with author override and no tooltip text" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<abbr title='Expanded'>Short</abbr><acronym title='' style='text-decoration-style:solid'>Solid</acronym><abbr>Plain</abbr>"
             )

    assert {:ok, %{children: [abbr, acronym, plain]}} = Style.compute_detailed(dom)
    assert hd(abbr.style._text_decorations).style == :dotted
    assert hd(acronym.style._text_decorations).style == :solid
    refute Map.has_key?(plain.style, :_text_decorations)

    assert {:ok, shorthand_dom} =
             HtmlParser.parse_detailed(
               "<abbr title='Expanded' style='text-decoration:underline'>Short</abbr>"
             )

    assert {:ok, %{children: [shorthand]}} = Style.compute_detailed(shorthand_dom)
    assert hd(shorthand.style._text_decorations).style == :solid
    assert hd(abbr.children).text == "Short"
  end

  test "removed basefont and background sound remain inert with no inherited font or resource request" do
    html =
      "<html><head><basefont size=7 face=Missing><bgsound src=unused.wav></head><body><p><blink>Static blink</blink><basefont size=7>Unchanged size<keygen name=unused>After keygen</p></body></html>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html,
               asset_resolver: fn request ->
                 flunk("removed metadata fetched #{inspect(request)}")
               end
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Static blink"
    assert text =~ "Unchanged size"
    assert text =~ "After keygen"
    assert {:ok, dom} = HtmlParser.parse_detailed("<basefont size=7><p>Ordinary size</p>")
    assert {:ok, %{children: [_metadata, paragraph]}} = Style.compute_detailed(dom)
    assert paragraph.style.font_size == 12.0
  end

  @tag :browser_parity
  @tag :legacy_markup_followup
  test "titled abbreviations and dotted overrides match untouched Chromium" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/browser_parity/html_abbreviation_titles.html", __DIR__)

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_abbreviation_titles",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  @tag :browser_parity
  @tag :legacy_removed_markup
  test "removed-element initial print behavior matches untouched Chromium" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_removed_static_markup.html",
        __DIR__
      )

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_removed_static_markup",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
