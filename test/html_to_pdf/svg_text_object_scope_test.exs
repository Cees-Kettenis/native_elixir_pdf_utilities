Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextObjectScopeTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Pdf.Reader

  test("real selected text initializes ActualText geometry for external readers", %{test: name}) do
    directory = Path.join(System.tmp_dir!(), "svg-text-object-#{:erlang.phash2(name)}")
    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    data = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")

    source =
      "<svg width='200' height='50'><text x='5' y='25' font-size='20' font-family='NativeSvgFont0'>ffi á̈ Ω</text></svg>"

    assert {:ok, pdf} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
               mode: :inline,
               fonts: [%{data: data}]
             })

    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _document} = Reader.read(pdf)
    path = Path.join(directory, "real-selected-glyphs.pdf")
    File.write!(path, pdf)
    tool = System.find_executable("pdftotext")

    assert is_binary(tool),
           "Poppler pdftotext is required for the selected-text bounding-box contract"

    assert {bbox, 0} = System.cmd(tool, ["-bbox", path, "-"], stderr_to_stdout: true)
    refute bbox =~ "<word"
  end

  test "a multi-glyph replacement uses one genuine text object and keeps its complete source selection" do
    html = "<svg width=200 height=50><text x=5 y=25 font-size=20>á̈ á̈</text></svg>"
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert {:ok, _document} = Reader.read(pdf)
  end
end
