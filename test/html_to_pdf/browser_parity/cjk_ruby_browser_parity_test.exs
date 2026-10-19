alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.CjkRubyBrowserParityTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
  @moduletag :browser_parity
  @moduletag :cjk_ruby

  test "default CJK ruby and styled ordinary line breaks match Chromium with glyphs" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/ruby_cjk_default_wrap.html")

    font =
      Paths.fixture("html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        render_opts: [
          fonts: [%{family: "CJK Ruby Fixture", path: font}],
          system_font_discovery: false
        ],
        max_changed_ratio: 0.01,
        max_average_delta: 0.045
      )

    assert stats.scripting == :disabled
    assert stats.page_count == 2

    for name <- ["native.pdf", "chromium.pdf"] do
      assert {:ok, text} =
               NativeElixirPdfUtilities.Text.extract(
                 File.read!(Path.join(stats.artifact_dir, name))
               )

      assert text =~ "東京"
      assert text =~ "大阪"
      assert text =~ "漢字"
    end
  end

  test "long default CJK ruby fragments and paginates with exact configured outlines" do
    fixture =
      Paths.fixture("html_to_pdf/browser_parity/ruby_cjk_long_group.html")

    font =
      Paths.fixture("html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf")

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        render_opts: [
          fonts: [%{family: "CJK Ruby Fixture", path: font}],
          system_font_discovery: false
        ],
        max_changed_ratio: 0.01,
        max_average_delta: 0.045
      )

    assert stats.scripting == :disabled
    assert stats.page_count == 2

    for name <- ["native.pdf", "chromium.pdf"] do
      assert {:ok, text} =
               NativeElixirPdfUtilities.Text.extract(
                 File.read!(Path.join(stats.artifact_dir, name))
               )

      assert text =~ "東京京都"
      assert text =~ "大阪漢字"
    end
  end
end
