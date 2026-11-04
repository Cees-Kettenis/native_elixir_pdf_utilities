defmodule NativeElixirPdfUtilities.HtmlToPdf.MathCompositionBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity
  @moduletag :math_composition

  for fixture <-
        ~w(mathml_script_padding_css.html mathml_positioned_equations.html mathml_positioned_matrix_cells.html mathml_anonymous_matrix.html mathml_fixed_collapsed_matrix.html mathml_child_fragment_targets.html) do
    @fixture fixture
    test "matches Chromium for #{@fixture}" do
      stats =
        PdfVisualCompare.assert_browser_match!(
          Paths.fixture("html_to_pdf/browser_parity/" <> @fixture),
          artifact_dir: Path.join("tmp/browser_parity/math_composition", Path.rootname(@fixture)),
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
    end
  end
end
