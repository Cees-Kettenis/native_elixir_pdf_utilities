alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.FormAppearanceConverterBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :form_appearance_converter_fidelity

  test "default filling fits the full value and uses bundled glyphs against an independent browser reference" do
    fixture = Paths.fixture("html_to_pdf/form_appearance_converter/fitted_editor.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/form_appearance_converter_browser_parity/fitted-browser",
        native_renderer: fn _html, opts ->
          source = """
          <style>
          html,body{margin:0;padding:0}
          input{position:absolute;left:0;width:66pt;height:19.5pt;box-sizing:border-box;
          margin:0;padding:0;border:0;font-family:'STIX Two Math';font-size:12pt;color:red}
          </style>
          <input name='long' value='ABC' style='top:0'>
          <input name='fallback' value='ABC' style='top:30pt'>
          """

          with {:ok, pdf} <- HtmlToPdf.render(source, opts),
               {:ok, flat} <-
                 Forms.fill(
                   pdf,
                   %{"long" => "This longer text was fitted before", "fallback" => "Ա"},
                   flatten: true
                 ) do
            {:ok, flat}
          end
        end,
        regions: [fitted_value: {0, 0, 66, 10}, bundled_glyph: {0, 30, 20, 20}],
        max_changed_ratio: 0.12,
        max_average_delta: 0.03
      )

    assert stats.page_count == 1
    native = Path.join(stats.artifact_dir, "native.pdf")
    assert {:ok, text} = Text.extract_file(native, layout: false)
    assert String.normalize(text, :nfkc) =~ "This longer text was fitted before"
    assert text =~ "Ա"
    {visible, 0} = System.cmd("pdftotext", ["-layout", native, "-"])
    assert String.normalize(visible, :nfkc) =~ "This longer text was fitted before"
    assert visible =~ "Ա"
  end

  test "a filled ordinary editor retains its original approved font, color and clip" do
    fixture =
      Paths.fixture("html_to_pdf/form_appearance_converter/ordinary_editor.html")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/form_appearance_converter_browser_parity/current-browser",
        native_renderer: fn html, opts ->
          with {:ok, pdf} <-
                 HtmlToPdf.render(
                   String.replace(html, "value='ffiBeta'", "value='Initial'"),
                   opts
                 ),
               {:ok, filled} <- Forms.fill(pdf, %{"editor" => "ffiBeta"}, appearance: :source),
               {:ok, [%{value: "ffiBeta"}]} <- Forms.fields(filled),
               {:ok, flat} <- Forms.flatten(filled) do
            {:ok, flat}
          end
        end,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01,
        channel_tolerance: 12
      )

    assert stats.page_count == 1
  end
end
