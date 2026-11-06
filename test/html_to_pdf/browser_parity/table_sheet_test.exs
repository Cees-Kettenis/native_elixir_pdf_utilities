alias NativeElixirPdfUtilities.TestSupport.Paths
Code.require_file(Paths.support("browser_fixture_sheets.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.BrowserTableSheetTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheetValidator, as: Validator

  setup do
    directory = Paths.fixture("html_to_pdf/browser_parity")
    sheets = :json.decode(File.read!(Path.join(directory, "sheets.json")))
    sheet = Enum.find(sheets, &(&1["name"] == "html_tables_links_transforms_sheet.html"))
    %{directory: directory, sheet: sheet}
  end

  test "an extended sheet accounts for all old and new source owners", %{
    directory: directory,
    sheet: sheet
  } do
    assert map_size(Validator.prepare_sources!(directory, sheet)) == 2

    assert NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheets.build!(directory, sheet) ==
             File.read!(Path.join(directory, sheet["name"]))

    missing = update_in(sheet, ["sources"], &Map.delete(&1, "html_table_legacy_center.html"))

    assert_raise ExUnit.AssertionError, ~r/exactly one source owner/, fn ->
      Validator.prepare_sources!(directory, missing)
    end

    changed = put_in(sheet, ["preserved_sources", "html_table_zero_edge_grid.html"], "changed")

    assert_raise ExUnit.AssertionError, ~r/Preserved sheet source changed/, fn ->
      Validator.prepare_sources!(directory, changed)
    end

    preserved =
      sheet
      |> Map.delete("sources")
      |> Map.update!("regions", fn regions ->
        Enum.filter(regions, fn [name, _] -> Map.has_key?(sheet["preserved_sources"], name) end)
      end)

    html = NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheets.build!(directory, preserved)
    assert :ok = Validator.assert_preserved_sheet!(sheet, html)

    assert_raise ExUnit.AssertionError, ~r/Preserved sheet composition changed/, fn ->
      Validator.assert_preserved_sheet!(sheet, html <> "<!-- changed -->")
    end
  end

  test "percentage columns require their reviewed fixed table basis", %{
    directory: directory,
    sheet: sheet
  } do
    name = "html_table_border_conflicts.html"

    for changed <- [
          update_in(sheet, ["sources", name], &Map.delete(&1, "percentage_columns")),
          put_in(sheet, ["sources", name, "percentage_columns", "table_width"], 200),
          put_in(sheet, ["sources", name, "percentage_columns", "column_width"], 40)
        ] do
      assert_raise ExUnit.AssertionError, fn -> Validator.prepare_sources!(directory, changed) end
    end
  end

  test "sheet extensions preserve existing field values and widget geometry" do
    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render("""
             <style>@page{size:180pt 120pt;margin:0}html,body{margin:0}</style>
             <input name="kept" value="Before" style="width:100pt;height:20pt">
             """)

    assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(pdf)

    sheet = %{
      "regions" => [],
      "sources" => %{},
      "preserved_fields" => %{
        "native" => fields |> :json.encode() |> IO.iodata_to_binary() |> :json.decode()
      }
    }

    assert :ok = Validator.assert_contents!(sheet, pdf)

    assert {:ok, changed} = NativeElixirPdfUtilities.Forms.fill(pdf, %{"kept" => "After"})

    assert_raise ExUnit.AssertionError, ~r/changed preserved native form fields/, fn ->
      Validator.assert_contents!(sheet, changed)
    end
  end

  test "the shared sheet retains the registry's density, font and tool metadata assertions", %{
    directory: directory
  } do
    stats = %{
      page_count: 1,
      pages: [%{}],
      dpi: 72,
      scripting: :disabled,
      pixel_channel_tolerance: 12,
      chromium_version: "Chromium example",
      rasterizer_version: "pdftoppm version example",
      font_sources: List.duplicate(%{sha256: String.duplicate("a", 64)}, 4),
      artifact_dir: directory
    }

    assert :ok = Validator.assert_comparison!(stats)

    for changed <- [
          Map.put(stats, :dpi, 96),
          Map.put(stats, :font_sources, []),
          Map.put(stats, :rasterizer_version, "unrecorded")
        ] do
      assert_raise ExUnit.AssertionError, fn -> Validator.assert_comparison!(changed) end
    end
  end

  test "extension proofs reject covering old artwork or changing existing output" do
    alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

    root =
      Path.expand(
        "tmp/browser_parity/table-extension-proof-#{System.unique_integer([:positive])}"
      )

    on_exit(fn -> File.rm_rf!(root) end)

    original = """
    <style>@page{size:180pt 120pt;margin:0}html,body{margin:0}
    p{position:absolute;left:10pt;top:5pt;margin:0;font-size:12pt}</style>
    <p>Retained text</p>
    """

    addition =
      "<div style='position:absolute;left:100pt;top:40pt;width:40pt;height:40pt;background:red'></div>"

    capture = fn name, html ->
      assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      directory = Path.join(root, name)
      [_] = PdfVisualCompare.rasterize_pdf!(pdf, artifact_dir: directory)

      for engine <- ["native", "chromium"] do
        File.cp!(Path.join(directory, "input.pdf"), Path.join(directory, engine <> ".pdf"))
        File.cp!(Path.join(directory, "page-1.ppm"), Path.join(directory, engine <> "-1.ppm"))
      end

      directory
    end

    before = capture.("before", original)
    after_directory = capture.("after", original <> addition)

    assert :ok =
             PdfVisualCompare.assert_sheet_extension_equivalence!(before, after_directory, [
               {100, 40, 40, 40}
             ])

    assert_raise ExUnit.AssertionError, ~r/covers existing artwork/, fn ->
      PdfVisualCompare.assert_sheet_extension_equivalence!(before, before, [{0, 0, 100, 35}])
    end

    changed = capture.("changed", String.replace(original, "Retained", "Changed") <> addition)

    assert_raise ExUnit.AssertionError, ~r/existing sheet pixels changed/, fn ->
      PdfVisualCompare.assert_sheet_extension_equivalence!(before, changed, [{100, 40, 40, 40}])
    end

    white_text = original <> "<p style='left:100pt;top:40pt;color:white'>Existing</p>"
    invisible = capture.("white-text", white_text)

    assert_raise ExUnit.AssertionError, ~r/covers existing text/, fn ->
      PdfVisualCompare.assert_sheet_extension_equivalence!(invisible, invisible, [
        {100, 40, 80, 40}
      ])
    end
  end

  test "region proofs accept only white padding from physical media rounding" do
    alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

    root = Path.expand("tmp/browser_parity/rounded-media-#{System.unique_integer([:positive])}")
    on_exit(fn -> File.rm_rf!(root) end)

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render("""
             <style>@page{size:180pt 120pt;margin:0}html,body{margin:0}</style><p>Kept</p>
             """)

    [%{width: 180, height: 120, pixels: pixels}] =
      PdfVisualCompare.rasterize_pdf!(pdf, artifact_dir: root)

    for engine <- ["native", "chromium"] do
      File.write!(Path.join(root, engine <> ".pdf"), pdf)
      File.cp!(Path.join(root, "page-1.ppm"), Path.join(root, engine <> "-1.ppm"))
    end

    padded =
      for row <- 0..119 do
        [binary_part(pixels, row * 180 * 3, 180 * 3), <<255, 255, 255>>]
      end

    path = Path.join(root, "chromium-1.ppm")
    File.write!(path, ["P6\n181 121\n255\n", padded, :binary.copy(<<255>>, 181 * 3)])
    assert :ok = PdfVisualCompare.assert_sheet_region_equivalence!(root, root, {0, 0, 180, 120})

    File.write!(path, ["P6\n181 121\n255\n", padded, :binary.copy(<<0>>, 181 * 3)])

    assert_raise ExUnit.AssertionError, ~r/artwork outside its authored canvas/, fn ->
      PdfVisualCompare.assert_sheet_region_equivalence!(root, root, {0, 0, 180, 120})
    end
  end
end
