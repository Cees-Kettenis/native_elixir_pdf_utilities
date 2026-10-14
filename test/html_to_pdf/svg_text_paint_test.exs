Code.require_file("../support/svg_converted_image.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPaintTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    %{limits: limits, data: data}
  end

  test "HTML enclosing link retains the same text plan when fill is a server" do
    html =
      "<a href='https://example.com/paint' style='text-decoration:none'><svg width=230 height=60 font-family='DejaVu Sans'><defs><linearGradient id=g><stop offset=0 stop-color=red /><stop offset=1 stop-color=blue /></linearGradient></defs><text x=5 y=35 font-size=24 fill='url(#g)'>Linked Ω</text></svg></a>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, solid} = HtmlToPdf.render(String.replace(html, "fill='url(#g)'", "fill='red'"))
    assert {:ok, painted_document} = Reader.read(pdf)
    assert {:ok, solid_document} = Reader.read(solid)

    rectangles = fn document ->
      document.objects
      |> Map.values()
      |> Enum.filter(&(get_in(&1, [:value, "Subtype"]) == {:name, "Link"}))
      |> Enum.map(& &1.value["Rect"])
    end

    assert rectangles.(painted_document) != []
    assert rectangles.(painted_document) == rectangles.(solid_document)
    image = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)
    assert NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image) > 100
  end
end
