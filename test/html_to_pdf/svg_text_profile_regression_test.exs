Code.require_file("../support/svg_converted_image.ex", __DIR__)
Code.require_file("../support/svg_public_workflow.ex", __DIR__)
Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextProfileRegressionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, SvgScene}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test("prepared scene font count stays bounded and independent preparation recovers", %{
    limits: limits
  }) do
    html =
      "<svg width=200 height=50><text y=25>Normal<tspan font-weight=700>Bold</tspan></text></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
    assert {:ok, original_scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert length(original_scene.selected_fonts) == 2
    Limits.install(Map.put(limits, :max_font_count, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html)

    assert diagnostic.stage == :font and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.FontValidator
    assert diagnostic.operation == :load_registry
    assert diagnostic.message =~ "max_font_count"
    Limits.install(limits)
    assert {:ok, scene} = SvgScene.prepare(svg, HtmlValidator.new_image_budget())
    assert length(scene.selected_fonts) == 2
  end

  test "descriptive metadata within visible text never changes converter pixels" do
    html =
      "<svg width=200 height=50><text y=25><title>Title only</title><desc>Description only</desc>Visible Ω<tspan>!</tspan></text></svg>"

    image = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(html)

    without_metadata =
      html
      |> String.replace("<title>Title only</title>", "")
      |> String.replace("<desc>Description only</desc>", "")

    reference = NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.render(without_metadata)
    assert image == reference
    assert NativeElixirPdfUtilities.TestSupport.SvgConvertedImage.coverage(image) > 20
  end

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  test "original text whitespace and tspan transforms cannot silently change logical geometry" do
    for content <- [
          "<text xml:space='preserve'>  Original  </text>",
          "<text>A<tspan transform='translate(5 0)'>B</tspan></text>"
        ] do
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=200 height=50>#{content}</svg>"
               )

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    end
  end

  test "raw text CSS requires preparation while prepared source paints and extracts" do
    html = "<svg width=200 height=50><text y=25 style='font-size:20px'>Prepared Ω</text></svg>"
    assert {:ok, %{children: [_svg]}} = HtmlParser.parse_detailed(html)
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  test "prepared stroke gradient and transformed text require coherent visible logical painting" do
    for html <- ["<svg width=200 height=50><text y=25 stroke=red>Stroke</text></svg>"] do
      assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
      refute pdf =~ "/Subtype /Link"
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [svg]}} = Style.prepare_svg_detailed(dom)
      [text] = svg.children
      _face = text.style.font_face
      assert {:ok, _document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    end

    assert {:ok, gradient_pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<svg width=200 height=50><defs><linearGradient id=g><stop offset=0 /><stop offset=1 /></linearGradient></defs><text y=25 fill='url(#g)'>Gradient</text></svg>"
             )

    refute gradient_pdf =~ "/Subtype /Link"

    assert :ok =
             NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
               gradient_pdf,
               false
             )

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(gradient_pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(gradient_pdf, layout: false)

    for style <- ["text-transform:uppercase", "white-space:pre", "text-decoration:underline"] do
      assert {:ok, pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<svg width=200 height=50><text y=25 style='#{style}'>Original</text></svg>"
               )

      assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    end
  end

  test "descriptive metadata within visible text never enters logical extraction" do
    html =
      "<svg width=200 height=50><text y=25><title>Title only</title><desc>Description only</desc>Visible Ω<tspan>!</tspan></text></svg>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [_svg]}} = Style.compute_detailed(dom)
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Link"
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf, false)
    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

    assert {:error, {:no_extractable_text, _}} =
             NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end
end
