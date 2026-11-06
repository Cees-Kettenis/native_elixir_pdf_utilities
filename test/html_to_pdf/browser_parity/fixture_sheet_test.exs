alias NativeElixirPdfUtilities.TestSupport.Paths
Code.require_file(Paths.support("browser_fixture_sheets.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.BrowserFixtureSheetTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheetValidator, as: Validator

  setup do
    directory = Paths.fixture("html_to_pdf/browser_parity")
    sheets = :json.decode(File.read!(Path.join(directory, "sheets.json")))
    sheet = Enum.find(sheets, &(&1["name"] == "text_wrapping_geometry_sheet.html"))
    %{directory: directory, sheet: sheet}
  end

  test "scenario metadata rejects duplicate owners, overlap and weakened region limits", %{
    directory: directory,
    sheet: sheet
  } do
    assert map_size(Validator.prepare_sources!(directory, sheet)) == 4

    assert NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheets.build!(directory, sheet) ==
             File.read!(Path.join(directory, sheet["name"]))

    [first, second | rest] = sheet["regions"]

    assert_raise ExUnit.AssertionError, ~r/Duplicate sheet source/, fn ->
      Validator.prepare_sources!(directory, Map.put(sheet, "regions", [first, first]))
    end

    [second_name, [_x, _y, width, height]] = second

    assert_raise ExUnit.AssertionError, ~r/Overlapping sheet regions/, fn ->
      Validator.prepare_sources!(
        directory,
        Map.put(sheet, "regions", [first, [second_name, [0, 0, width, height]] | rest])
      )
    end

    weakened = put_in(sheet, ["sources", hd(first), "max_changed_ratio"], 0.02)
    assert_raise ExUnit.AssertionError, fn -> Validator.prepare_sources!(directory, weakened) end
  end

  test "the limited composer rejects selector functions, escapes, namespaces and changed source bytes",
       %{directory: directory, sheet: sheet} do
    copy =
      Path.expand(
        "tmp/browser_parity/literal-sheet-contract-#{System.unique_integer([:positive])}"
      )

    File.mkdir_p!(copy)
    on_exit(fn -> File.rm_rf!(copy) end)

    for [name, _bounds] <- sheet["regions"],
        do: File.cp!(Path.join(directory, name), Path.join(copy, name))

    name = "css_text_atomic_intrinsics.html"
    path = Path.join(copy, name)
    original = File.read!(path)

    for selector <- [":is(table, div)", "x|table", ".escaped\\,identifier"] do
      source = String.replace(original, "table {", selector <> " {")
      File.write!(path, source)
      digest = Base.encode16(:crypto.hash(:sha256, source), case: :lower)
      changed = put_in(sheet, ["sources", name, "sha256"], digest)

      assert_raise ExUnit.AssertionError, ~r/Unsupported literal sheet selector/, fn ->
        Validator.prepare_sources!(copy, changed)
      end
    end

    File.write!(path, original <> "<!-- changed -->")

    assert_raise ExUnit.AssertionError, ~r/recheck independent equivalence/, fn ->
      Validator.prepare_sources!(copy, sheet)
    end
  end

  test "a neighboring canvas cannot satisfy a missing text assertion", %{
    directory: directory,
    sheet: sheet
  } do
    html = File.read!(Path.join(directory, sheet["name"]))

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert :ok = Validator.assert_contents!(sheet, pdf)
    [section] = Regex.run(~r/<section id="sample_0".*?<\/section>/s, html)
    changed = String.replace(html, section, String.replace(section, "Box", "Lost"))

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(changed, system_font_discovery: false)

    assert_raise ExUnit.AssertionError,
                 ~r/Missing text in sheet region css_text_atomic_intrinsics.html/,
                 fn ->
                   Validator.assert_contents!(sheet, pdf)
                 end
  end

  test "fractional print margins cannot silently change a source canvas", %{directory: directory} do
    sheets = :json.decode(File.read!(Path.join(directory, "sheets.json")))
    sheet = Enum.find(sheets, &(&1["name"] == "text_discretionary_geometry_sheet.html"))
    prepared = Validator.prepare_sources!(directory, sheet)
    assert map_size(prepared) == 3
    assert Enum.all?(prepared, fn {_name, source} -> source.margin == 9.75 end)

    changed =
      put_in(sheet, ["sources", "css_discretionary_shaping.html", "effective_margin"], 10)

    assert_raise ExUnit.AssertionError, fn -> Validator.prepare_sources!(directory, changed) end
  end
end
