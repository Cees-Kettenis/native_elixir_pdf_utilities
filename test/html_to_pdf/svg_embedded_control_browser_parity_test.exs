defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgEmbeddedControlBrowserParityTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :svg_embedded_controls
  @fixtures Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)

  for fixture <- [
        "html_svg_embedded_controls.html",
        "html_svg_effect_widgets.html"
      ] do
    test "original #{fixture} reports unsupported foreignObject with its original browser reference" do
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
        PdfVisualCompare.assert_browser_unsupported!(Path.join(@fixtures, unquote(fixture)),
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

      assert stats.status == :unsupported_feature
      assert stats.visual_comparison == :not_performed
      assert stats.chromium_page_count == 1
      refute File.exists?(Path.join(stats.artifact_dir, "chromium-input.html"))
    end
  end
end
