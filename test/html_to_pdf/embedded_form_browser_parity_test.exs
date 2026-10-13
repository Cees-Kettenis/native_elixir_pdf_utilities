defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedFormBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf}
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :embedded_interactive_fields
  @fixtures Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)

  for {fixture, count} <- [
        {"html_embedded_interactive_fields.html", 9},
        {"html_frameset_interactive_fields.html", 6}
      ] do
    test "original #{fixture} paints real clipped fields and isolated document values" do
      {output, 0} = System.cmd("fc-match", ["--format=%{file}\n%{family}\n", "Arial"])
      [path, family | _] = String.split(output, "\n")
      bytes = File.read!(path)
      font = %{family: "Arial", data: bytes, weight: 400}
      artifact = Path.join("tmp/browser_parity", Path.rootname(unquote(fixture)))

      proof = %{
        path: path,
        family: family,
        sha256: Base.encode16(:crypto.hash(:sha256, bytes), case: :lower),
        elixir: System.version(),
        otp: to_string(:erlang.system_info(:otp_release))
      }

      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures, unquote(fixture)),
          render_opts: [fonts: []],
          native_renderer: fn html, opts ->
            File.write!(
              Path.join(artifact, "native-control-font-source.exs"),
              inspect(proof, pretty: true)
            )

            HtmlToPdf.render(html, Keyword.put(opts, :fonts, [font]))
          end,
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.page_count == 1
      assert {:ok, fields} = Forms.fields(File.read!(Path.join(stats.artifact_dir, "native.pdf")))
      assert length(fields) == unquote(count)
      refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
    end
  end
end
