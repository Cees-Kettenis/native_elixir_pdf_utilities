defmodule NativeElixirPdfUtilities.HtmlToPdf.StaticPaintingTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Info, Text}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "paint images preserve alpha, selected density and searchable HTML captions" do
    html = File.read!(Paths.fixture("html_to_pdf/browser_parity/static_painting_values.html"))

    for density <- [1, 2] do
      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 system_font_discovery: false,
                 image_selection_density: density,
                 compress_streams: false
               )

      assert {:ok, 1} = Info.page_count(pdf)
      assert {:ok, text} = Text.extract(pdf)

      for caption <- [
            "Relative RGB",
            "Missing channel",
            "Oklab gradient",
            "Border image",
            "Border round",
            "Outer shadow",
            "Inset shadow",
            "Text stroke",
            "Stroke",
            "SVG geometry",
            "Dark field",
            "Dark button",
            "Oriented image"
          ],
          do: assert(text =~ caption)

      assert pdf =~ "2 Tr"
      assert pdf =~ "/SMask"
      assert {:ok, document} = Reader.read_validated(pdf)

      widths =
        for {_ref, %{value: %{"Subtype" => {:name, "Image"}, "Width" => width}}} <-
              document.document.objects,
            do: width

      assert 8 in widths == (density == 2)
    end
  end

  test "root and fixed layers retain three page fragments and each logical caption once" do
    html = File.read!(Paths.fixture("html_to_pdf/browser_parity/painting_canvas_fragments.html"))
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, 3} = Info.page_count(pdf)
    assert {:ok, text} = Text.extract(pdf)

    for caption <- [
          "Canvas first",
          "Canvas second",
          "Canvas third",
          "Canvas fourth",
          "Canvas fifth"
        ] do
      assert length(:binary.matches(text, caption)) == 1
    end

    assert {:ok, document} = Reader.read_validated(pdf)
    assert length(document.pages) == 3
    assert Enum.all?(document.pages, &is_map(&1.resources["XObject"]))
  end
end
