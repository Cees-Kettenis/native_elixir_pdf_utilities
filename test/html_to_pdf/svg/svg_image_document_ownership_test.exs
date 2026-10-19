alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgImageDocumentOwnershipTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "default image and document adapters retain resource and scope ownership" do
    source = "<svg width='4' height='2'><rect width='4' height='2' fill='#112233'/></svg>"
    assert {:ok, png} = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(source)
    assert {:ok, image} = NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(png, nil, false)
    assert {image.width_px, image.height_px} == {4, 2}
    assert image.data == :binary.copy(<<17, 34, 51>>, 8)
  end
end
