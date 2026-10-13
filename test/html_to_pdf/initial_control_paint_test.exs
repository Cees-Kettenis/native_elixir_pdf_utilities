defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialControlPaintTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, PdfWriter}
  alias NativeElixirPdfUtilities.Validators.HtmlFormValidator

  test "initial single-line editors center within the real allocated height and textarea stays top aligned" do
    for tag <- ["input", "select", "textarea"] do
      children = if tag == "select", do: "<option>Value</option>", else: ""
      suffix = if tag == "input", do: "", else: "#{children}</#{tag}>"
      html = "<#{tag} style='font-size:12px;line-height:16px'>#{suffix}"
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom)
      [node] = tree.children
      style = node.style
      assert style.line_height == 12.0

      assert HtmlFormValidator.initial_control_content_offset(style, 13.5) ==
               if(tag == "textarea", do: 0.0, else: 0.75)

      assert HtmlFormValidator.initial_control_content_offset(style, 3.01) ==
               if(tag == "textarea", do: 0.0, else: -4.48828125)
    end
  end

  test "real textarea resize paint uses the print hairline and canonical resize:none suppresses it" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<textarea>Original notes</textarea>")

    assert {:ok, tree} = Style.compute_detailed(dom)
    [node] = tree.children
    assert length(HtmlFormValidator.initial_resizer_strokes(node.style, 100, 30)) == 4

    assert HtmlFormValidator.initial_resizer_strokes(Map.put(node.style, :resize, :none), 100, 30) ==
             []

    assert HtmlFormValidator.initial_resizer_strokes(node.style, 0, 30) == []
    assert {:ok, layout} = Layout.layout(tree, page_size: {200, 100}, margin: 0)
    strokes = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :textarea_resizer))
    assert length(strokes) == 4

    assert Enum.all?(
             strokes,
             &(&1.shape == {:diagonal, :up} and elem(&1.stroke_color, 3) == 0.45)
           )

    assert {:ok, pdf} =
             PdfWriter.render([%{size: {200, 100}, boxes: strokes}],
               compress_streams: false,
               forms: :static
             )

    assert pdf =~ "0 w"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert Enum.any?(document.objects, fn {_id, object} ->
             is_map(object.value) and Map.get(object.value, "CA") == 0.45
           end)
  end
end
