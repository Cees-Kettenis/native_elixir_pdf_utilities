alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.SelectedFontLigatureBrowserParityTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity

  test "original selected Mono and Sans Latin text match Chromium" do
    fixture = Paths.fixture("html_to_pdf/font_ligatures/selected_latin.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/selected_font_ligature_browser_parity/selected-latin-browser",
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
            "../../fonts/cff-static-sans.otf" =>
              {:file, Paths.fixture("fonts/cff-static-sans.otf")},
            "../../fonts/selected-container.woff" =>
              {:file, Paths.fixture("fonts/selected-container.woff")},
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
        regions: [
          {"weight_search", {12, 75, 30, 16}},
          {"width_search", {12, 91, 30, 16}},
          {"unicode_composite", {12, 107, 30, 16}},
          {"woff_reconstruction", {12, 123, 30, 16}},
          {"cff_embedding", {168, 75, 108, 64}}
        ],
        # Skia emits CFF as unhinted Type 3 paths, so Poppler's font and vector
        # rasterizers differ at glyph edges. Keep every original region at 1%.
        # This new large-outline sample also has exact independent fontTools
        # bounds/program checks in CffEmbeddingTest and CffCharstringTest.
        # https://github.com/google/skia/blob/main/src/pdf/SkPDFFont.cpp
        region_thresholds: %{
          "cff_embedding" => %{max_changed_ratio: 0.05}
        },
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    assert stats.scripting == :disabled
    assert [%{regions: regions}] = stats.pages
    assert length(regions) == 5
    for region <- regions, do: assert(region.native_ink_pixels >= region.minimum_ink_pixels)
    native_pdf = File.read!(Path.join(stats.artifact_dir, "native.pdf"))

    assert {:ok, %{pages: [%{spans: spans}]}} =
             NativeElixirPdfUtilities.Text.extract_spans(native_pdf)

    cff_row = Enum.filter(spans, &(&1.y >= 75 and &1.y < 139 and &1.x >= 167.999))
    assert Enum.map_join(cff_row, & &1.text) == "Hx0"
    assert Enum.all?(cff_row, &(&1.end_x <= 276.001))

    for top <- [75, 91, 107, 123] do
      row = Enum.filter(spans, &(&1.y >= top and &1.y < top + 16 and &1.x < 42.001))
      assert Enum.map_join(row, & &1.text) == "Hx0"
      assert Enum.all?(row, &(&1.x >= 11.999 and &1.end_x <= 42.001))
    end
  end
end
