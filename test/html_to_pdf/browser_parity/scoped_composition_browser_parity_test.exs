alias NativeElixirPdfUtilities.TestSupport.Paths
Code.require_file(Paths.support("pdf_visual_compare.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.ScopedCompositionBrowserParityTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity

  test "same-named shadow fonts retain declaration scope and inherited metrics" do
    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/shadow_scoped_fonts.html"),
        artifact_dir: "tmp/test_artifacts/scoped_composition/shadow_scoped_fonts",
        # Chromium issue 41085401 ignores @font-face inside shadow roots.
        # Only the reference hoists those two faces under unique names.
        chromium_transform: fn html ->
          html
          |> String.replace(".local {font-family:Shared}", ".local {font-family:ScopedMono}")
          |> String.replace("span {font-family:Shared}", "span {font-family:ScopedBold}")
          |> String.replace(
            "</head>",
            "<style>@font-face{font-family:ScopedMono;src:url(../../../../priv/fonts/dejavu/DejaVuSansMono.ttf)}@font-face{font-family:ScopedBold;src:url(../../../../priv/fonts/dejavu/DejaVuSans-Bold.ttf)}</style></head>"
          )
        end,
        render_opts: [
          assets:
            Map.new(~w(DejaVuSans.ttf DejaVuSansMono.ttf DejaVuSans-Bold.ttf), fn name ->
              {"../../../../priv/fonts/dejavu/#{name}",
               {:file, Paths.project("priv/fonts/dejavu/#{name}")}}
            end)
        ],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end

  test "dormant original resources and controls validate before the range is removed" do
    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/template_replacement/dormant_original_sources.html"),
        artifact_dir: "tmp/test_artifacts/scoped_composition/dormant_original_sources",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1 and stats.scripting == :disabled
  end
end
