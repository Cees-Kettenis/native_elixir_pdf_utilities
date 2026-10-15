Code.require_file("../support/svg_public_workflow.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFilterInkTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

  test "generated and constant effect outputs do not claim unrelated original glyphs" do
    for primitive <- [
          "<feFlood flood-color='blue'/>",
          "<feTurbulence baseFrequency='.05'/>",
          "<feGaussianBlur result='unused'/><feFlood flood-color='blue'/>",
          "<feFlood result='flood' flood-color='blue'/><feOffset in='flood' dx='2'/>",
          "<feComposite operator='arithmetic' k4='1'/>",
          "<feColorMatrix type='matrix' values='0 0 0 0 .2 0 0 0 0 .3 0 0 0 0 .5 0 0 0 0 1'/>",
          "<feComponentTransfer><feFuncR type='linear' slope='0' intercept='.2'/><feFuncG type='linear' slope='0' intercept='.3'/><feFuncB type='linear' slope='0' intercept='.5'/><feFuncA type='linear' slope='0' intercept='1'/></feComponentTransfer>"
        ],
        original <- [
          "<text x='5' y='25' font-family='DejaVu Sans'>Original glyphs</text>",
          "<foreignObject width='150' height='35'><div style='font-family:DejaVu Sans'>Original glyphs</div></foreignObject>"
        ] do
      html =
        "<svg width='200' height='60'><defs><filter id='f'>#{primitive}</filter></defs><g filter='url(#f)'>#{original}</g></svg>"

      if String.contains?(original, "<foreignObject") do
        assert {:error, {:unsupported_html, diagnostic}} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(html)

        assert diagnostic.reason == :unsupported_html
        assert diagnostic.stage == :layout
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
        assert diagnostic.operation == :convert
        assert diagnostic.source == "foreignObject"
        assert diagnostic.message =~ "foreignObject"
      else
        assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
        assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

        assert {:error, {:no_extractable_text, _}} =
                 NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

        refute pdf =~ "/Subtype /Link"
      end
    end
  end

  test "dead SourceGraphic paint cannot enlarge or shift a constant result canvas" do
    images =
      for x <- ["20", "-700.25"] do
        source =
          "<svg width='180' height='70'><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='100' height='50'><feFlood flood-color='#407080'/></filter></defs><g filter='url(#f)'><rect x='#{x}' y='15' width='20' height='20'/></g></svg>"

        assert {:ok, pdf} = HtmlToPdf.render(source)
        assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

        for {ref, object} <- document.objects,
            is_map(object.value) and object.value["Subtype"] == {:name, "Image"},
            {:ok, bytes} =
              NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(document, {:ref, ref}) do
          {object.value["Width"], object.value["Height"], bytes}
        end
      end

    assert [first, second] = images
    assert first != []
    assert first == second
  end

  test "nonlinear chains cannot recover SourceGraphic ink clipped outside the filter region" do
    # Chromium 152 does not pre-clip SourceGraphic to the filter region and moves
    # this "Y" into view (feOffset ink lands at CSS x[10, 22]); resvg clips the
    # filter input to the userSpaceOnUse region x=0..100 first, so the glyph at
    # x=-40 is erased before feOffset runs and the result canvas stays empty.
    # Enlarging the region during conversion would change constant-primitive
    # result canvases, so this resvg input clipping is a documented engine gap.
    source =
      "<svg width='180' height='70'><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='100' height='50'><feOffset dx='50'/><feColorMatrix type='saturate' values='.5'/></filter></defs><g filter='url(#f)'><text x='-40' y='25' font-family='DejaVu Sans' font-size='18'>Y</text></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(source)
    assert pdf =~ "/Subtype /Image"

    (
      assert :ok =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                 pdf,
                 false
               )

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    )

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    images =
      for {ref, object} <- document.objects,
          is_map(object.value) and object.value["Subtype"] == {:name, "Image"} and
            object.value["ColorSpace"] == {:name, "DeviceGray"},
          {:ok, bytes} = NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(document, {:ref, ref}) do
        bytes
      end

    assert images != []

    assert Enum.all?(images, fn alpha ->
             not Enum.any?(:binary.bin_to_list(alpha), &(&1 > 0))
           end)
  end

  test "nonlinear ownership excludes glyphs whose real output ink is erased or covered" do
    cases = [
      {"<feMorphology operator='erode' radius='6' result='ink'/><feFlood flood-color='blue' result='background'/><feMerge><feMergeNode in='background'/><feMergeNode in='ink'/></feMerge>",
       "<text x='10' y='25' font-family='DejaVu Sans' font-size='9'>I</text>", nil},
      {"<feComponentTransfer result='ink'><feFuncA type='linear' slope='0' intercept='0'/></feComponentTransfer><feFlood flood-color='blue' result='background'/><feMerge><feMergeNode in='background'/><feMergeNode in='ink'/></feMerge>",
       "<text x='10' y='25' font-family='DejaVu Sans' font-size='18'>Erase</text>", nil},
      {"<feColorMatrix type='saturate' values='.5'/>",
       "<text x='20' y='35' font-family='DejaVu Sans' font-size='24'>AB</text><rect x='19' y='5' width='18' height='40' fill='blue'/>",
       "B"}
    ]

    for {operations, input, expected} <- cases do
      source =
        "<svg width='180' height='70'><defs><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='150' height='50'>#{operations}</filter></defs><g filter='url(#f)'>#{input}</g></svg>"

      assert {:ok, pdf} = HtmlToPdf.render(source)
      assert pdf =~ "/Subtype /Image"

      if expected do
        assert :ok =
                 NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                   pdf,
                   true
                 )

        assert {:error, {:no_extractable_text, _}} =
                 NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

        refute pdf =~ "/Subtype /Link"
      else
        assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
      end
    end
  end

  test "displaced nonlinear ink outside the filter region is clipped away by resvg" do
    # Chromium 152 samples SourceGraphic without clipping it to the filter
    # region, so the feDisplacementMap (scale 80, xChannelSelector R over the
    # #ff8000 flood) pulls the glyph at x=105 into view at CSS x[65, 77].
    # resvg clips the filter input to the userSpaceOnUse region x=0..100 first,
    # the displaced source is empty, and the result canvas stays fully
    # transparent. Documented resvg divergence from Chromium.
    source =
      "<svg width='180' height='70'><defs><filter id='f' filterUnits='userSpaceOnUse' color-interpolation-filters='sRGB' x='0' y='0' width='100' height='50'><feFlood flood-color='#ff8000' result='map'/><feDisplacementMap in='SourceGraphic' in2='map' scale='80' xChannelSelector='R' yChannelSelector='G'/></filter></defs><g filter='url(#f)'><text x='105' y='25' font-family='DejaVu Sans' font-size='18'>Y</text></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(source)
    assert pdf =~ "/Subtype /Image"

    (
      assert :ok =
               NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(
                 pdf,
                 false
               )

      assert {:error, {:no_extractable_text, _}} =
               NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

      refute pdf =~ "/Subtype /Link"
    )
  end

  test "a live graph dependency retains its genuinely effected selected Unicode" do
    for primitive <- [
          "<feFlood result='flood' flood-color='blue'/><feBlend in='SourceGraphic' in2='flood'/>",
          "<feGaussianBlur result='blur' stdDeviation='1'/><feFlood result='flood' flood-color='blue'/><feMerge><feMergeNode in='flood'/><feMergeNode in='blur'/></feMerge>",
          "<feComposite in='SourceGraphic' operator='arithmetic' k2='1'/>",
          "<feColorMatrix type='saturate' values='.5'/>"
        ] do
      html =
        "<svg width='200' height='60'><defs><filter id='f'>#{primitive}</filter></defs><g filter='url(#f)'><foreignObject width='150' height='35'><div style='font-family:DejaVu Sans'>Original café Ω</div></foreignObject></g></svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html)

      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :layout
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
      assert diagnostic.operation == :convert
      assert diagnostic.source == "foreignObject"
      assert diagnostic.message =~ "foreignObject"
    end
  end
end
