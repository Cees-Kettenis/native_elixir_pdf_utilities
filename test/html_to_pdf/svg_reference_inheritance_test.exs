Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgReferenceInheritanceTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "qualified passive attributes do not become inherited SVG presentation declarations" do
    html =
      "<svg width=100 height=80 xmlns:q='urn:example:passive'><defs><g id=target q:opacity='inherit'><rect width=40 height=30 fill=red /></g></defs><use href='#target' opacity='.5' /></svg>"

    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end

  alias NativeElixirPdfUtilities.{Limits}

  test "the original Chromium opacity discrepancy is a precise unsupported reference diagnostic" do
    source = File.read!("test/fixtures/html_to_pdf/svg_reference_inherit_original.html")

    assert Base.encode16(:crypto.hash(:sha256, source), case: :lower) ==
             "62416a4ca70e8a78a828b33ec6c52163a81ddaec76e1f319d78ddb98fe94ebbc"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(source)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    refute pdf =~ "/Subtype /Link"
  end

  test "authored effects in reused target descendants retain their unfinished instance scope" do
    for {property, instance} <- [
          {"opacity", "opacity='.5'"},
          {"clip-path", "clip-path='url(#c)'"},
          {"transform", "transform='translate(20 0)'"}
        ] do
      html =
        "<svg width=100 height=80><defs><clipPath id=c><rect width=70 height=60 /></clipPath><g id=target><g #{property}='inherit'><rect width=40 height=30 /></g></g></defs><use href='#target' #{instance} /></svg>"

      assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    end
  end

  test "winning host CSS inheritance of non-inherited instance properties is diagnosed" do
    for {property, instance} <- [
          {"opacity", "opacity='.5'"},
          {"clip-path", "clip-path='url(#c)'"},
          {"transform", "transform='translate(20 0)'"},
          {"width", "width='70'"}
        ] do
      html =
        "<style>#child {#{property}:inherit}</style><svg width=100 height=80><defs><clipPath id=c><rect width=70 height=60 /></clipPath><g id=target><rect id=child width=40 height=30 /></g></defs><use href='#target' #{instance} /></svg>"

      assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    end
  end

  test "naturally inherited live instance paints and prepared static opacity remain supported" do
    html =
      "<svg width=100 height=80><defs><g id=target><rect width=10 height=10 fill=inherit stroke=currentColor /></g></defs><use href='#target' fill=blue color=red /><use href='#target' x=20 fill=green color=blue /><g opacity='.5'><g opacity='inherit'><rect x=40 width=10 height=10 fill=red /></g></g></svg>"

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert {:ok, _doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
  end

  test "prepared host CSS and the existing image-mode CSS contract remain supported" do
    html =
      "<svg width=100 height=80><g style='opacity:.5'><g opacity='inherit'><rect width=40 height=30 fill=red /></g></g></svg>"

    assert {:ok, png_pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute png_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               png_pdf,
               false
             )

    source =
      "<svg width='100' height='80'><g opacity='.5'><g style='opacity:inherit'><rect width='40' height='30' fill='red'/></g></g></svg>"

    assert {:ok, png} =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.png(
               source,
               [width: 100, height: 80],
               %{mode: :image}
             )

    assert binary_part(png, 0, 8) == <<137, 80, 78, 71, 13, 10, 26, 10>>
  end
end
