defmodule NativeElixirPdfUtilities.HtmlToPdf.FixturePagesBrowserParityTest do
  use ExUnit.Case, async: true
  @moduletag :browser_parity
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.TestSupport.{PdfVisualCompare, BrowserFixturePages}

  test "one capture retains independent page checks, original native variants and immutable artifacts" do
    directory = Path.expand("tmp/browser_parity/page_reference_contract")
    File.mkdir_p!(directory)
    samples = [{"first.html", "First", "#174a86"}, {"second.html", "Second", "#4f6b2a"}]

    sources =
      for {name, text, color} <- samples do
        html = """
        <!doctype html><html><head><style>
        @page{size:180pt 120pt;margin:0}
        html,body{margin:0;padding:0;font-family:"DejaVu Sans";font-size:12pt;line-height:18pt}
        p{margin:0}div{width:40pt;height:40pt;background:#{color}}
        </style></head><body><p>#{text}</p><div></div></body></html>
        """

        File.write!(Path.join(directory, name), html)

        %{
          "name" => name,
          "sha256" => Base.encode16(:crypto.hash(:sha256, html), case: :lower),
          "page_count" => 1
        }
      end

    group = %{
      "name" => "combined.html",
      "page_css" => "size:180pt 120pt;margin:0",
      "native" => "original",
      "sources" => sources
    }

    fixture = Path.join(directory, group["name"])
    File.write!(fixture, BrowserFixturePages.build!(directory, group))
    options = [max_changed_ratio: 0.01, max_average_delta: 0.01]
    entries = Enum.map(sources, &{&1["name"], options})

    references =
      PdfVisualCompare.prepare_fixture_page_references!(fixture, group, entries, options)

    for {name, text, _} <- samples do
      stats =
        PdfVisualCompare.assert_browser_match!(
          Path.join(directory, name),
          options ++
            [
              reference: references[name],
              artifact_dir: Path.join(directory, name <> "-comparison")
            ]
        )

      assert stats.page_count == 1
      assert stats.capture.pages in [[1], [2]]
      assert {:ok, actual} = Text.extract(File.read!(Path.join(stats.artifact_dir, "native.pdf")))
      assert actual =~ text
    end

    assert_raise ExUnit.AssertionError, ~r/changed-pixel threshold exceeded/, fn ->
      PdfVisualCompare.assert_browser_match!(
        Path.join(directory, "second.html"),
        options ++
          [
            reference: references["second.html"],
            artifact_dir: Path.join(directory, "changed-second"),
            native_renderer: fn original, opts ->
              HtmlToPdf.render(String.replace(original, "#4f6b2a", "#ff0000"), opts)
            end
          ]
      )
    end

    reference = references["first.html"]
    File.write!(reference.chromium_pdf, "changed immutable browser output")

    assert_raise ExUnit.AssertionError, ~r/reference artifact changed/, fn ->
      PdfVisualCompare.assert_browser_match!(
        Path.join(directory, "first.html"),
        options ++ [reference: reference, artifact_dir: Path.join(directory, "tampered")]
      )
    end
  end
end
