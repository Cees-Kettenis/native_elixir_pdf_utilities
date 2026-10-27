defmodule NativeElixirPdfUtilities.HtmlToPdf.CssUnicodeSeparatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator

  test "Unicode separators paint as spaces while preserving their original text" do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, face} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    for {source, visible} <- [
          {"A\u2028B", "A B"},
          {"A\u2029B", "A B"},
          {"A\u2028\u2029B", "A  B"},
          {"A \u2028 B", "A   B"}
        ] do
      assert {:ok, ^visible} = HtmlLineBreakValidator.display_text(source, face)

      assert {:ok, %{text: ^visible, actual_text: ^source, discretionary?: true}} =
               HtmlLineBreakValidator.display_plan(source, face)
    end
  end

  test "all whitespace modes keep separator spacing without forcing a newline" do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, face} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    space_width = Font.css_text_width(" ", face, 12)

    for mode <- ~w(normal nowrap pre pre-line pre-wrap break-spaces),
        separator <- ["\u2028", "\u2029", "\u2028\u2029"] do
      html =
        "<p style='margin:0;font-family:DejaVu Sans;font-size:12pt;white-space:#{mode}'>" <>
          "<span>A</span>#{separator}<span>B</span></p>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, %{pages: [page]}} = Text.extract_spans(pdf)
      first = Enum.find(page.spans, &(&1.text == "A"))
      last = Enum.find(page.spans, &(&1.text == "B"))
      assert_in_delta last.y, first.y, 1.0e-6
      assert_in_delta last.x - first.end_x, space_width * String.length(separator), 0.02
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text == "A" <> separator <> "B"
    end
  end
end
