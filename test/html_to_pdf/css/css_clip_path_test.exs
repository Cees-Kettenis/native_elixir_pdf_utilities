defmodule NativeElixirPdfUtilities.HtmlToPdf.CssClipPathTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Validators.CssClipValidator

  test "basic clips and SVG path clips resolve against measured border geometry" do
    for source <- [
          "circle(50%)",
          "ellipse(40% 30% at 25% 50%)",
          "inset(4px round 3px)",
          "polygon(0 0,100% 0,0 100%)",
          "path('M0 0H40V40Z')",
          "PATH('M0 0H40V40Z') content-box",
          ~S|p\61th('M0 0H40V40Z')|,
          "content-box"
        ] do
      assert {:ok, style} = CssClipValidator.prepare(%{}, source)
      assert CssClipValidator.valid_style?(style)

      assert {:ok, %{path_commands: commands}} =
               CssClipValidator.resolve(style, %{x: 10.0, top: 90.0, width: 80.0, height: 60.0})

      assert commands != []
    end

    assert :error = CssClipValidator.prepare(%{}, "path('L0 0')")
    assert :error = CssClipValidator.prepare(%{}, "circle(-1px)")

    refute CssClipValidator.valid_style?(%{
             css_clip_path: %{
               type: :path,
               operations: [{?L, [1, 2]}],
               rule: :nonzero,
               reference: :border_box
             }
           })
  end

  test "empty and move-only source paths clip all paint and links without raising" do
    for source <- ["path('')", "path('M0 0')"] do
      assert {:ok, style} = CssClipValidator.prepare(%{}, source)

      assert {:ok, %{width: width, height: height}} =
               CssClipValidator.resolve(style, %{x: 10, top: 30, width: 20, height: 20})

      assert width == 0 and height == 0

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<div style=\"clip-path:#{source};width:40pt;height:20pt;background:red\"><a href='https://example.com/empty'>Clipped</a></div>",
                 system_font_discovery: false
               )

      refute pdf =~ "/Subtype /Link"
    end
  end

  test "native path clips restrict artwork and annotation quadrilaterals together" do
    html =
      "<style>html,body{margin:0;font:12pt/18pt DejaVu Sans}div{width:100pt;height:45pt;background:red;clip-path:inset(0 50% 0 0)}</style><div><a href='https://example.com/clip'>Clipped native link</a></div>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, page_size: {180, 120}, margin: 0, compress_streams: false)

    assert pdf =~ " W n "
    assert pdf =~ "/QuadPoints ["
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Clipped"
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    links =
      context.document.objects
      |> Map.values()
      |> Enum.map(& &1.value)
      |> Enum.filter(&(is_map(&1) and Map.get(&1, "Subtype") == {:name, "Link"}))

    assert links != []

    assert Enum.all?(links, fn %{"Rect" => [left, _bottom, right, _top]} ->
             left >= 0 and right <= 50.001
           end)
  end
end
