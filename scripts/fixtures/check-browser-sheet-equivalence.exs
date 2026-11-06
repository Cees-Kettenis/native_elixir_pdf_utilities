Code.require_file("../../test/support/paths.ex", __DIR__)
import ExUnit.Assertions

alias NativeElixirPdfUtilities.TestSupport.{
  Paths,
  PdfVisualCompare,
  BrowserFixtureSheetValidator,
  BrowserFixtureSheets
}

Code.require_file(Paths.support("pdf_visual_compare.ex"))
Code.require_file(Paths.support("browser_fixture_sheet_validator.ex"))
Code.require_file(Paths.support("browser_fixture_sheets.ex"))
ExUnit.start(autorun: false, max_cases: 1)

directory = Paths.fixture("html_to_pdf/browser_parity")
sheets = :json.decode(File.read!(Path.join(directory, "sheets.json")))
selected = Enum.filter(sheets, &Map.has_key?(&1, "sources"))

output =
  Path.expand(System.get_env("BROWSER_SHEET_EQUIVALENCE", ".quality/browser-sheet-equivalence"))

for sheet <- selected do
  BrowserFixtureSheetValidator.prepare_sources!(directory, sheet)

  options = [
    render_opts:
      if(sheet["render_opts"],
        do: [system_font_discovery: sheet["render_opts"]["system_font_discovery"]],
        else: []
      ),
    max_changed_ratio: 0.01,
    max_average_delta: 0.01
  ]

  combined =
    PdfVisualCompare.assert_browser_match!(
      Path.join(directory, sheet["name"]),
      options ++ [artifact_dir: Path.join(output, sheet["name"])]
    )

  BrowserFixtureSheetValidator.assert_comparison!(combined)

  for engine <- sheet["assertion_engines"] || ["native"] do
    BrowserFixtureSheetValidator.assert_contents!(
      sheet,
      File.read!(Path.join(combined.artifact_dir, engine <> ".pdf")),
      engine
    )
  end

  if sheet["preserved_sources"] do
    baseline_directory =
      case System.get_env("BROWSER_SHEET_BASELINE") do
        nil ->
          preserved =
            sheet
            |> Map.delete("sources")
            |> Map.update!("regions", fn regions ->
              Enum.filter(regions, fn [name, _bounds] ->
                Map.has_key?(sheet["preserved_sources"], name)
              end)
            end)

          html = BrowserFixtureSheets.build!(directory, preserved)
          BrowserFixtureSheetValidator.assert_preserved_sheet!(sheet, html)

          input =
            Path.join(directory, ".chromium-input-#{System.unique_integer([:positive])}.html")

          File.write!(input, html)

          try do
            stats =
              PdfVisualCompare.assert_browser_match!(
                input,
                options ++ [artifact_dir: Path.join(output, "preserved-" <> sheet["name"])]
              )

            stats.artifact_dir
          after
            File.rm!(input)
          end

        baseline ->
          Path.join(baseline, sheet["name"])
      end

    additions =
      for [name, bounds] <- sheet["regions"],
          Map.has_key?(sheet["sources"], name),
          do: List.to_tuple(bounds)

    PdfVisualCompare.assert_sheet_extension_equivalence!(
      baseline_directory,
      combined.artifact_dir,
      additions
    )

    File.write!(
      Path.join(combined.artifact_dir, "extension-equivalence.exs"),
      inspect(
        %{
          status: :pass,
          preserved_sources: sheet["preserved_sources"],
          additions: additions,
          engines: [:native, :chromium],
          old_pixels_unchanged: true,
          old_text_unchanged: true,
          old_fields_unchanged: true,
          before: baseline_directory,
          elixir: System.version()
        },
        pretty: true
      )
    )

    IO.puts("PASS #{sheet["name"]}: existing sheet pixels, text and fields unchanged")
  end

  for [name, bounds] <- sheet["regions"], Map.has_key?(sheet["sources"], name) do
    original =
      PdfVisualCompare.assert_browser_match!(
        Path.join(directory, name),
        options ++ [artifact_dir: Path.join(output, name)]
      )

    assert original.page_count == sheet["sources"][name]["assertions"]["page_count"]

    PdfVisualCompare.assert_sheet_region_equivalence!(
      original.artifact_dir,
      combined.artifact_dir,
      List.to_tuple(bounds)
    )

    File.write!(
      Path.join(original.artifact_dir, "equivalence.exs"),
      inspect(
        %{
          status: :pass,
          source: sheet["sources"][name],
          bounds: bounds,
          engines: [:native, :chromium],
          dpi: 72,
          changed_pixels: 0,
          chromium: original.chromium_version,
          poppler: original.rasterizer_version,
          elixir: System.version(),
          otp: to_string(:erlang.system_info(:otp_release))
        },
        pretty: true
      )
    )

    IO.puts("PASS #{name}: unchanged native and Chromium pixels, text and baselines")
  end
end
