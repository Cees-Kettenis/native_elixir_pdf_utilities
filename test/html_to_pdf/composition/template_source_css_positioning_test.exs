alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateSourceCssPositioningTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @opts [system_font_discovery: false, compress_streams: false]
  @positions "object-position:right 1em bottom calc(25% + 1ch);background-position:right 2rem top"

  test "unmatched and inactive original positions allow replacement without invented image or font owners" do
    for css <- [".absent{#{@positions}}", "@media screen{p{#{@positions}}}"] do
      html =
        "<style>#{css}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

      assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
      assert {:ok, expected} = HtmlToPdf.render("<p>Good</p>", @opts)
      assert pdf == expected
      assert {:ok, "Good"} = Text.extract(pdf)
    end
  end

  test "symbolic positions retain their source, axes and uncomputed font and percentage expressions" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<style>.absent{#{@positions}}</style><p>Active</p>"
             )

    assert {:ok, result} =
             Style.with_source_style_contexts(
               dom,
               [],
               HtmlValidator.new_css_budget(),
               fn _, _, _ -> :ok end,
               @opts
             )

    [object, background] = result.source_obligations.declaration_contexts

    for record <- [object, background] do
      assert record.status == :checked_symbolically
      assert record.contexts == []
      assert record.source_value.kind == :image_position
      assert record.selection.kind == :unapplied_stylesheet
      assert is_reference(record.source.owner.node_id)
    end

    assert object.source_value.axes ==
             {{:end, {:css_position_math, "1em"}},
              {:end, {:css_position_math, "calc(25% + 1ch)"}}}

    assert background.source_value.axes == {{:end, {:rem, 2.0}}, {:percent, 0.0}}
  end

  test "inactive malformed position syntax and dimensional types report the exact authored source" do
    for declaration <- [
          "object-position:left right",
          "object-position:right 1pt top",
          "background-position:left 2pt right 3pt",
          "object-position:calc(1px + 1deg) top",
          "background-position:unknown(2px)"
        ] do
      html =
        "<style>@media screen{.absent{#{declaration}}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
      assert diagnostic.stage == :css
      assert diagnostic.source == declaration
      assert diagnostic.line == 1 and is_integer(diagnostic.column)
    end
  end

  test "unapplied background shorthand reuses its supported color grammar without inherited color" do
    for value <- ["#ddd", "none", "currentColor", "var(--missing)"] do
      html =
        "<style>.absent{background:#{value}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

      assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
      assert {:ok, expected} = HtmlToPdf.render("<p>Good</p>", @opts)
      assert pdf == expected
    end

    declaration = "background:invalid-color"

    html =
      "<style>@media screen{p{#{declaration}}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.source == declaration
  end

  test "unapplied edge longhands keep canonical length and keyword policy" do
    for declaration <- [
          "margin-bottom:6pt",
          "margin-left:-2em",
          "margin-right:auto",
          "padding-top:1rem",
          "border-left-width:thin",
          "border-bottom-width:2em"
        ] do
      html =
        "<style>.absent{#{declaration}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

      assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
      assert {:ok, expected} = HtmlToPdf.render("<p>Good</p>", @opts)
      assert pdf == expected
    end

    for declaration <- ["padding-top:-1em", "border-left-width:25%", "margin-right:2bananas"] do
      html =
        "<style>@media screen{p{#{declaration}}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
      assert diagnostic.source == declaration
    end
  end

  test "prospective selected position cannot reuse the unapplied symbolic state" do
    declaration = "object-position:var(--missing)"

    html =
      "<style>.selected{#{declaration}}</style><body><?marker name='A'><template for='A'><p class='selected'>Good</p></template></body>"

    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.source == declaration
  end

  @tag :browser_parity
  @tag :unapplied_positions_original
  test "original dormant positions and fitted images match Chromium print selection" do
    fixture =
      Paths.fixture("html_to_pdf/template_replacement/source_css_positioning.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/template_source_css_positioning/unapplied-positions-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
