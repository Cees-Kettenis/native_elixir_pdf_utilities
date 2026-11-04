alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPatternsMarkersTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "pattern units, inheritance and transformed stroke paints produce native PDF tiling resources" do
    html =
      File.read!(Paths.fixture("html_to_pdf/browser_parity/inline_svg_patterns_markers.html"))

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    (
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
      assert text =~ "Native patterns and marker artwork Ω café"
      assert text =~ "Pattern units, transformed origins and arrows survived."
    )
  end
end
