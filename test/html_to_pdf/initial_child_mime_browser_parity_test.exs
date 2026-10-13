defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialChildMimeBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :initial_child_mime
  @fixture Path.expand(
             "../fixtures/html_to_pdf/browser_parity/html_initial_child_mime.html",
             __DIR__
           )

  test "original plaintext and natural image documents match Chromium without source transforms" do
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
      PdfVisualCompare.assert_browser_match!(@fixture,
        render_opts: [fonts: []],
        native_renderer: fn html, opts ->
          File.write!(
            "tmp/browser_parity/html_initial_child_mime/native-monospace-source.exs",
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
    refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
  end
end
