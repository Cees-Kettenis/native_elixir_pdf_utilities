Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextPaintTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.Reader

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    %{limits: limits, data: data}
  end

  alias NativeElixirPdfUtilities.{Limits}

  test "HTML enclosing link retains the same text plan when fill is a server" do
    html =
      "<a href='https://example.com/paint' style='text-decoration:none'><svg width=230 height=60 font-family='DejaVu Sans'><defs><linearGradient id=g><stop offset=0 stop-color=red /><stop offset=1 stop-color=blue /></linearGradient></defs><text x=5 y=35 font-size=24 fill='url(#g)'>Linked Ω</text></svg></a>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    assert {:ok, solid} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               String.replace(html, "fill='url(#g)'", "fill='red'")
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(solid, false)
    assert {:ok, painted_document} = Reader.read(pdf)
    assert {:ok, solid_document} = Reader.read(solid)

    rectangles = fn document ->
      for {_id, %{value: value}} <- document.objects,
          is_map(value) and value["Subtype"] == {:name, "Link"} do
        value["Rect"]
      end
    end

    assert rectangles.(painted_document) != []
    assert rectangles.(painted_document) == rectangles.(solid_document)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test("overlapping opaque bands keep source painter order and charge their emitted paths", %{
    limits: _limits
  }) do
    source =
      "<svg width='120' height='70'><defs><pattern id='p' patternUnits='userSpaceOnUse' width='12' height='10'><rect width='12' height='10' fill='blue'/><rect x='3' width='6' height='10' fill='red'/></pattern></defs><rect x='5' y='5' width='100' height='50' fill='url(#p)'/></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 120, height: 70],
               %{mode: :inline}
             )

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _document} = Reader.read(pdf)
    Limits.install(Map.put(Limits.effective(), :max_svg_raster_pixels, 0))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(
               source,
               [width: 120, height: 70],
               %{mode: :inline}
             )

    assert diagnostic.stage == :limits
    assert is_binary(Map.get(diagnostic, :source)) and Map.get(diagnostic, :source) != ""
  end
end
