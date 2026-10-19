alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.FormAppearanceConverterBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf}
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :form_appearance_converter_fidelity

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
               {:ok, filled} <- Forms.fill(pdf, %{"editor" => "ffiBeta"}),
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
