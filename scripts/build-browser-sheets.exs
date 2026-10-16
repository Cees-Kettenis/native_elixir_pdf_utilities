Code.require_file("../test/support/browser_fixture_sheets.ex", __DIR__)

alias NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheets

directory = Path.expand("../test/fixtures/html_to_pdf/browser_parity", __DIR__)
sheets = :json.decode(File.read!(Path.join(directory, "sheets.json")))

for sheet <- sheets do
  path = Path.join(directory, sheet["name"])
  File.write!(path, BrowserFixtureSheets.build!(directory, sheet))
  IO.puts(path)
end
