defmodule NativeElixirPdfUtilities.HtmlToPdf.SelectedFontLigatureBrowserTest do
  use ExUnit.Case, async: false
  @moduletag :browser_parity

  test "original selected Mono and Sans Latin text match Chromium" do
    fixture = Path.expand("../fixtures/html_to_pdf/font_ligatures/selected_latin.html", __DIR__)

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/selected-latin-browser",
        render_opts: [
          fonts: [
            %{
              family: "ProofMono",
              path:
                Path.join(
                  :code.priv_dir(:native_elixir_pdf_utilities),
                  "fonts/dejavu/DejaVuSansMono.ttf"
                )
            },
            %{
              family: "ProofSans",
              path:
                Path.join(
                  :code.priv_dir(:native_elixir_pdf_utilities),
                  "fonts/dejavu/DejaVuSans.ttf"
                )
            }
          ],
          assets: %{
            "../../../../priv/fonts/dejavu/DejaVuSansMono.ttf" =>
              {:file,
               Path.join(
                 :code.priv_dir(:native_elixir_pdf_utilities),
                 "fonts/dejavu/DejaVuSansMono.ttf"
               )},
            "../../../../priv/fonts/dejavu/DejaVuSans.ttf" =>
              {:file,
               Path.join(
                 :code.priv_dir(:native_elixir_pdf_utilities),
                 "fonts/dejavu/DejaVuSans.ttf"
               )}
          }
        ],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    assert stats.scripting == :disabled
  end
end
