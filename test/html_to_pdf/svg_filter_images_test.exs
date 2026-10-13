defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFilterImagesTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "local feImage input paints genuine independently transformed vector content" do
    html =
      "<svg width='200' height='100'><defs><linearGradient id='gradient'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient><g id='input' transform='translate(4 6)'><rect x='5' y='5' width='50' height='30' fill='url(#gradient)'/><circle cx='70' cy='20' r='12' fill='green'/></g><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='140' height='80'><feImage href='#input'/></filter></defs><g filter='url(#f)'><text x='5' y='25' font-family='DejaVu Sans'>Unpainted original</text></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"
    refute pdf =~ "/ActualText"
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
  end

  test "shared filter definitions retain independently normalized gradient inputs on every instance" do
    html =
      "<svg width='360' height='90'><defs><linearGradient id='gradient'><stop stop-color='red'/><stop offset='1' stop-color='blue'/></linearGradient><rect id='input' x='5' y='5' width='90' height='40' fill='url(#gradient)'/><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='130' height='70'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/><g transform='translate(180 0)'><rect width='100' height='50' filter='url(#f)'/></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    images =
      Enum.filter(document.objects, fn {_ref, object} ->
        is_map(object.value) and object.value["Subtype"] == {:name, "Image"} and
          object.value["ColorSpace"] == {:name, "DeviceRGB"}
      end)

    assert length(images) == 2

    for {ref, _object} <- images do
      assert {:ok, rgb} =
               NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(document, {:ref, ref})

      pixels = for <<r, g, b <- rgb>>, do: {r, g, b}
      assert Enum.any?(pixels, fn {r, g, b} -> r > b and r > g end)
      assert Enum.any?(pixels, fn {r, g, b} -> b > r and b > g end)
    end
  end

  test "missing targets and independently unprepared selected inputs diagnose through validator authority" do
    for {target, expected} <- [
          {"", :invalid_document},
          {"<foreignObject id='input' width='50' height='30'><p>Real HTML</p></foreignObject>",
           :unsupported_html}
        ] do
      source =
        "<svg width='200' height='100'><defs>#{target}<filter id='f'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

      assert {:error, {^expected, %{message: message, stage: stage}}} = HtmlToPdf.render(source)
      assert message != ""
      assert stage in [:svg, :layout, :style]
    end
  end

  test "independent selected feImage text retains its actual Unicode without borrowing SourceGraphic text" do
    defs =
      "<defs><text id='input' x='5' y='25' font-family='DejaVu Sans'>Independent café Ω</text><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='50'><feImage href='#input' result='image'/></filter></defs>"

    html =
      "<svg width='220' height='80'>#{defs}<g filter='url(#f)'><text x='5' y='25' font-family='DejaVu Sans'>Unpainted source</text></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "/ActualText"
    assert {:ok, "Independent café Ω"} = Text.extract(pdf, layout: false)
    refute pdf =~ "Unpainted source"

    # Keep the original formerly unprepared target source as a no-ink case.
    original =
      "<svg width='200' height='100'><defs><text id='input'>Selected source</text><filter id='f'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(original)
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
  end

  test "only live independent image results contribute their original selected text" do
    for {operations, expected} <- [
          {"<feImage href='#input' result='image'/><feGaussianBlur in='image' stdDeviation='1'/>",
           "Independent café Ω"},
          {"<feImage href='#input' result='image'/><feFlood flood-color='blue'/>", nil},
          {"<feImage href='#input' result='image'/><feColorMatrix in='image' values='0 0 0 0 .2 0 0 0 0 .3 0 0 0 0 .5 0 0 0 0 1'/>",
           nil}
        ] do
      html =
        "<svg width='220' height='80'><defs><text id='input' x='5' y='25' font-family='DejaVu Sans'>Independent café Ω</text><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='50'>#{operations}</filter></defs><rect width='100' height='30' filter='url(#f)'/></svg>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert pdf =~ "/Subtype /Image"

      if expected do
        assert {:ok, ^expected} = Text.extract(pdf, layout: false)
      else
        refute pdf =~ "/ActualText"
        assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
      end
    end
  end

  test "hidden and clipped independent image glyphs cannot leak logical text" do
    for {text_attributes, group_attributes} <- [
          {"fill-opacity='0'", ""},
          {"", "opacity='0'"},
          {"", "clip-path='url(#outside)'"}
        ] do
      html =
        "<svg width='220' height='80'><defs><text id='input' x='5' y='25' font-family='DejaVu Sans' #{text_attributes}>Independent café Ω</text><clipPath id='outside'><rect x='200' y='70' width='10' height='5'/></clipPath><filter id='f' filterUnits='userSpaceOnUse' x='0' y='0' width='180' height='50'><feImage href='#input'/></filter></defs><rect width='100' height='30' filter='url(#f)' #{group_attributes}/></svg>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      refute pdf =~ "/ActualText"
      assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
    end
  end

  test "feImage input pattern and stroke work is bounded before effect allocation", %{
    limits: limits
  } do
    source =
      "<svg width='200' height='100'><defs><pattern id='p' width='1000' height='1000' patternUnits='userSpaceOnUse'><rect width='1000' height='1000' fill='blue'/></pattern><rect id='input' width='100' height='50' fill='url(#p)'/><filter id='f'><feImage href='#input'/></filter></defs><rect width='100' height='50' filter='url(#f)'/></svg>"

    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 100_000))

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             SvgBackend.vectorize(source, [width: 400, height: 200], %{mode: :inline})

    assert message =~ "max_aggregate_svg_surface_pixels"
  end
end
