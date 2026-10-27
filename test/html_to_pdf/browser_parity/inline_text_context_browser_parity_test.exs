defmodule NativeElixirPdfUtilities.HtmlToPdf.InlineTextContextBrowserParityTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.TestSupport.{Paths, PdfVisualCompare}
  @moduletag :browser_parity

  for mode <- ~w(normal pre pre_line) do
    @mode mode
    test "direct body #{@mode} spacing preserves browser text and word positions" do
      stats =
        PdfVisualCompare.assert_browser_match!(
          Paths.fixture("html_to_pdf/browser_parity/body_inline_spacing_#{@mode}.html"),
          render_opts: [system_font_discovery: false],
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      [reference, native] =
        for name <- ["chromium", "native"] do
          path = Path.join(stats.artifact_dir, name <> ".pdf")
          {bbox, 0} = System.cmd("pdftotext", ["-bbox", path, "-"])
          {word_position(bbox, "Hello"), word_position(bbox, "world")}
        end

      {{reference_x, reference_y}, {reference_world_x, reference_world_y}} = reference
      {{native_x, native_y}, {native_world_x, native_world_y}} = native
      assert_in_delta native_world_x - native_x, reference_world_x - reference_x, 0.1
      assert_in_delta native_world_y - native_y, reference_world_y - reference_y, 0.1
      assert stats.page_count == 1
    end
  end

  test "inline casing and pseudo-content retain the browser's actual letters" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/inline_text_case_context.html"),
        render_opts: [system_font_discovery: false],
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    for name <- ["chromium", "native"] do
      {text, 0} = System.cmd("pdftotext", [Path.join(stats.artifact_dir, name <> ".pdf"), "-"])
      normalized = text |> String.normalize(:nfc) |> String.replace(~r/\s/u, "")

      assert normalized ==
               "AbcdefGhijİSTANBULİZMİRiıiοςοσαAbcdefghÉcoleDon'tHello-World123abcwwwwnext"
    end
  end

  defp word_position(bbox, word) do
    [_, x, y] = Regex.run(~r/<word xMin="([\d.]+)" yMin="([\d.]+)"[^>]*>#{word}<\/word>/u, bbox)
    {String.to_float(x), String.to_float(y)}
  end
end
