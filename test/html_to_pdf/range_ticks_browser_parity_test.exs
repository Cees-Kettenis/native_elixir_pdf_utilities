defmodule NativeElixirPdfUtilities.HtmlToPdf.RangeTicksBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  test "static authored range datalist ticks match Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/html_range_datalist_ticks.html",
          __DIR__
        ),
        artifact_dir: "tmp/browser_parity/html_range_datalist_ticks",
        render_opts: [forms: :static],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 2
  end

  test "disabled suggested ticks survive readonly scalar transport and flattening" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_disabled_range_datalist_ticks.html",
        __DIR__
      )

    for {mode, suffix, renderer} <- [
          {:interactive, "interactive", &NativeElixirPdfUtilities.HtmlToPdf.render/2},
          {:interactive, "flattened",
           fn html, opts ->
             with {:ok, pdf} <- NativeElixirPdfUtilities.HtmlToPdf.render(html, opts),
                  do: NativeElixirPdfUtilities.Forms.flatten(pdf)
           end}
        ] do
      stats =
        PdfVisualCompare.assert_browser_match!(fixture,
          artifact_dir: "tmp/browser_parity/html_disabled_range_datalist_ticks-" <> suffix,
          render_opts: [forms: mode],
          native_renderer: renderer,
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 2 and stats.scripting == :disabled

      assert {:ok, fields} =
               NativeElixirPdfUtilities.Forms.fields(
                 File.read!(Path.join(stats.artifact_dir, "native.pdf"))
               )

      if suffix == "interactive" do
        assert Enum.map(fields, &{&1.name, &1.value}) == [
                 {"level", "60"},
                 {"custom", "25"},
                 {"none", "80"}
               ]

        assert Enum.all?(fields, & &1.read_only)
      else
        assert fields == []
      end
    end
  end
end
