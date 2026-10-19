defmodule NativeElixirPdfUtilities.HtmlToPdf.OverflowClipTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, PdfWriter}

  test "zero ordinary overflow validates paint before clipping it away" do
    assert {:ok, pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<div style='overflow:hidden;height:0'><p>Clipped source</p></div><p>After</p>"
             )

    assert pdf =~ "0 0 0 0 re W n"
    assert {:ok, _} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  test "overflow wrappers preserve the original paint plane through relative movement" do
    host = %{
      type: :drawing_group,
      role: :embedded_viewport_host,
      width: 50,
      height: 10,
      transform: {1, 0, 0, 1, 0, 90},
      clip: %{x: 0, y: 0, width: 50, height: 10},
      boxes: []
    }

    [wrapper] = DrawingGeometry.clip_boxes([host], %{x: 0, y: 90, width: 50, height: 10})
    wrapper = DrawingGeometry.translate_box(wrapper, 7, -3)
    assert wrapper.transform == {1, 0, 0, 1, 0, 0}
    assert hd(wrapper.boxes).transform == {1, 0, 0, 1, 7, 87}
    composition = DrawingGeometry.compose_group(wrapper, {1, 1, 0, 0, nil, 100})
    assert composition == {1, 1, 0, 0, {6.75, 87.0, 56.75, 97.0}, 100}

    assert DrawingGeometry.compose_group(hd(wrapper.boxes), composition) ==
             {1, 1, 6.75, 87.0, {6.75, 87.0, 56.75, 97.0}, 10}
  end

  test "overflow coordinate carriers reject missing clips and nonidentity transforms through the public writer" do
    box = %{
      type: :drawing_group,
      role: :overflow_clip,
      width: 10,
      height: 10,
      transform: {1, 0, 0, 1, 0, 0},
      clip: %{x: 0, y: 0, width: 10, height: 10},
      boxes: []
    }

    for malformed <- [Map.delete(box, :clip), %{box | transform: {1, 0, 0, 1, 1, 0}}] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [malformed]}])

      assert diagnostic.stage == :pdf
      assert diagnostic.message =~ "containing coordinate plane"
    end
  end
end
