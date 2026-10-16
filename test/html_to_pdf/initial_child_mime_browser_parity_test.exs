Code.require_file("../support/browser_fixture_sheets.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialChildMimeBrowserParityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :initial_child_mime
  @fixtures Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)

  test "initial child MIME transport and legacy iframe geometry match Chromium together" do
    sheet = %{
      "size" => [320, 440],
      "font_size" => "12pt",
      "regions" => [
        ["html_iframe_initialization.html", [0, 0, 140, 420]],
        ["html_initial_child_documents.html", [160, 0, 160, 390]]
      ]
    }

    source = NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheets.build!(@fixtures, sheet)
    source = String.replace(source, "<head>", "<head><base href=\"file://#{@fixtures}/\">")
    File.mkdir_p!("tmp/browser-fixture-sheets")

    fixture =
      Path.expand("tmp/browser-fixture-sheets/initial-child-documents-#{System.pid()}.html")

    File.write!(fixture, source)
    artifacts = "tmp/browser_parity/html_initial_child_documents"

    on_exit(fn ->
      File.mkdir_p!(artifacts)
      File.cp!(fixture, Path.join(artifacts, "source.html"))
      File.rm!(fixture)
    end)

    {output, 0} = System.cmd("fc-match", ["--format=%{file}\n%{family}\n", "monospace"])
    [path, family | _] = String.split(output, "\n")
    bytes = File.read!(path)
    font = %{family: "monospace", data: bytes, weight: 400}

    proof = %{
      path: path,
      family: family,
      sha256: Base.encode16(:crypto.hash(:sha256, bytes), case: :lower),
      elixir: System.version(),
      otp: to_string(:erlang.system_info(:otp_release))
    }

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: artifacts,
        regions:
          Enum.map(sheet["regions"], fn [name, bounds] -> {name, List.to_tuple(bounds)} end),
        render_opts: [fonts: [], base_url: @fixtures],
        native_renderer: fn html, opts ->
          File.write!(
            Path.join(artifacts, "native-monospace-source.exs"),
            inspect(proof, pretty: true)
          )

          HtmlToPdf.render(html, Keyword.put(opts, :fonts, [font]))
        end,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    File.write!(
      Path.join(stats.artifact_dir, "native-monospace-source.exs"),
      inspect(proof, pretty: true)
    )

    assert stats.page_count == 1
    assert length(hd(stats.pages).regions) == 2
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end
end
