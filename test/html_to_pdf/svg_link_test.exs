defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgLinkTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend
  alias NativeElixirPdfUtilities.Validators.SvgFormValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test "shape links are actual PDF annotations at transformed painted bounds" do
    source =
      "<svg width='200' height='100'><a href='https://example.test/shape'><rect x='20' y='20' width='40' height='20' transform='translate(10 5)' fill='red'/></a></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline})
    assert {:ok, form} = SvgFormValidator.prepare_source(pdf)
    assert [%{source: "https://example.test/shape", rect: [l, b, r, t]}] = form.links
    assert_in_delta l, 22.5, 0.001
    assert_in_delta b, 41.25, 0.001
    assert_in_delta r, 52.5, 0.001
    assert_in_delta t, 56.25, 0.001
    assert pdf =~ "/Subtype /Link"
    assert pdf =~ "/URI (https://example.test/shape)"
  end

  test "selected glyph links preserve Unicode real fonts and clipped ink bounds", %{data: data} do
    source =
      "<svg width='300' height='100'><defs><clipPath id='c'><rect x='10' y='0' width='50' height='100'/></clipPath></defs><g clip-path='url(#c)' opacity='.7'><a href='https://example.test/text'><text x='20' y='45' font-family='NativeSvgFont0' font-size='22'>ffi Ωé ABC</text></a></g></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline, fonts: [%{data: data}]})
    assert {:ok, "ffi Ωé ABC"} = Text.extract(pdf, layout: false)
    assert {:ok, form} = SvgFormValidator.prepare_source(pdf)
    assert form.links != []
    assert Enum.all?(form.links, fn %{rect: [l, b, r, t]} -> l >= 7.5 and r <= 45 and t > b end)
    refute pdf =~ "3 Tr"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    assert Enum.any?(Map.values(document.objects), &(Map.get(&1, :stream) == data))
  end

  test "zero opacity visibility and hidden clips do not emit detached links" do
    for inner <- [
          "<g opacity='0'><rect width='30' height='30'/></g>",
          "<rect width='30' height='30' visibility='hidden'/>",
          "<g clip-path='url(#c)'><rect width='30' height='30'/></g>"
        ] do
      source =
        "<svg width='100' height='80'><defs><clipPath id='c'><rect width='40' height='40' visibility='hidden'/></clipPath></defs><a href='https://example.test/hidden'>#{inner}</a><rect x='70' width='20' height='20'/></svg>"

      assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline})
      assert {:ok, form} = SvgFormValidator.prepare_source(pdf)
      assert form.links == []
    end
  end

  test "inline scene carries real links and fragment targets through final pagination" do
    html =
      "<svg width='240' height='100'><a href='https://example.test/host'><text x='10' y='30'>Host Ωé</text></a><a href='#destination'><rect x='10' y='45' width='40' height='20'/></a><text id='destination' x='100' y='80'>Target</text></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "Host ΩéTarget"} = Text.extract(pdf, layout: false)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      for %{value: %{"Subtype" => {:name, "Link"}} = item} <- Map.values(document.objects),
          do: item

    assert Enum.any?(links, &(get_in(&1, ["A", "URI"]) == {:string, "https://example.test/host"}))
    assert Enum.any?(links, &Map.has_key?(&1, "Dest"))
  end

  test "original URI failures and unfinished inline ranges return validator diagnostics" do
    assert {:error, {_reason, diagnostic}} =
             HtmlToPdf.render(
               "<svg><a href='javascript:alert(1)'><rect width='20' height='20'/></a></svg>"
             )

    assert diagnostic.message != ""

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<svg><text y='30'>A<a href='https://example.test'>B</a></text></svg>"
             )

    assert diagnostic.stage == :layout
  end

  test "anonymous non-link anchors and reused source anchors retain real instance bounds" do
    source =
      "<svg width='240' height='100'><defs><g id='art'><rect width='20' height='15'/></g></defs><a><rect x='180' width='20' height='20'/></a><a href='https://example.test/reuse'><use href='#art' x='10' y='20'/><use href='#art' x='80' y='35'/></a></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline})
    assert {:ok, form} = SvgFormValidator.prepare_source(pdf)
    assert length(form.links) == 2
    assert Enum.map(form.links, &hd(&1.rect)) == [7.5, 60]
    assert Enum.all?(form.links, &(&1.index == 0))
  end

  test "host base and external HTML fragment destinations share original link authority" do
    html =
      "<base href='https://example.test/base/'><svg width='180' height='70'><a href='next?q=1&amp;x=2'><rect width='20' height='20'/></a><a href='#html-target'><rect x='30' width='20' height='20'/></a></svg><p id='html-target'>Destination</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      for %{value: %{"Subtype" => {:name, "Link"}} = item} <- Map.values(document.objects),
          do: item

    assert Enum.any?(
             links,
             &(get_in(&1, ["A", "URI"]) == {:string, "https://example.test/base/next?q=1&x=2"})
           )

    # An authored base makes the fragment an external URI under shared HTML policy.
    assert Enum.any?(
             links,
             &(get_in(&1, ["A", "URI"]) == {:string, "https://example.test/base/#html-target"})
           )
  end

  test "authorized image links and actual HTML slot siblings preserve their own regions" do
    png = File.read!("test/fixtures/html_to_pdf/browser_parity/assets/svg-resource-colors.png")

    html =
      "<svg width='120' height='80'><a href='https://example.test/image'><image href='paint.png' x='20' y='10' width='60' height='30' preserveAspectRatio='none'/></a><foreignObject x='10' y='50' width='100' height='20'><div xmlns='http://www.w3.org/1999/xhtml'>Actual HTML</div></foreignObject></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html, assets: %{"paint.png" => {:bytes, png}})
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Actual HTML"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    assert Enum.any?(Map.values(document.objects), fn item ->
             get_in(item, [:value, "A", "URI"]) == {:string, "https://example.test/image"}
           end)

    assert pdf =~ "/Subtype /Image"
  end

  test "referenced original anchor ranges diagnose before painting instead of silently disappearing" do
    source =
      "<svg width='100' height='80'><defs><g id='a'><a href='https://example.test'><rect width='20' height='20'/></a></g></defs><use href='#a'/></svg>"

    for fun <- [&SvgBackend.vectorize/3, &SvgBackend.rasterize/3] do
      assert {:error, {:unsupported_html, diagnostic}} = fun.(source, [], %{mode: :inline})
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgLinkValidator
      assert diagnostic.source == "use"
      assert diagnostic.message =~ "per-instance"
    end
  end

  test "invisible stroke does not widen real fill links and affine clips retain visible geometry" do
    source =
      "<svg width='120' height='80'><defs><clipPath id='c' transform='translate(10 5)'><rect width='20' height='20'/></clipPath></defs><a href='https://example.test'><rect x='5' y='5' width='40' height='30' stroke='black' stroke-width='30' stroke-opacity='0' clip-path='url(#c)'/></a></svg>"

    assert {:ok, pdf} = SvgBackend.vectorize(source, [], %{mode: :inline})
    assert {:ok, form} = SvgFormValidator.prepare_source(pdf)
    assert [%{rect: [l, b, r, t]}] = form.links
    assert_in_delta l, 7.5, 0.001
    assert_in_delta b, 41.25, 0.001
    assert_in_delta r, 22.5, 0.001
    assert_in_delta t, 56.25, 0.001
  end

  test "nested image anchors diagnose their missing child annotation scope before visible painting" do
    child =
      "<svg xmlns='http://www.w3.org/2000/svg' width='30' height='20'><a href='https://example.test/child'><rect width='30' height='20'/></a></svg>"

    source =
      "<svg width='100' height='60'><image href='svg-resource:child' width='60' height='40'/></svg>"

    context = %{
      mode: :inline,
      resources: %{"svg-resource:child" => %{data: child, media_type: "image/svg+xml"}}
    }

    assert {:error, {:unsupported_html, diagnostic}} = SvgBackend.vectorize(source, [], context)
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgLinkValidator
    assert diagnostic.source == "svg-resource:child"
    assert diagnostic.message =~ "child-document"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<svg width='100' height='60'><image href='child.svg' width='60' height='40'/></svg>",
               assets: %{"child.svg" => {:bytes, child}}
             )

    assert diagnostic.message =~ "child-document"
  end

  test "annotation plans retain independent work box object and raster ceilings", %{
    limits: limits
  } do
    source =
      "<svg width='100' height='80'><a href='https://example.test'><rect width='20' height='20'/></a></svg>"

    Limits.install(Map.put(limits, :max_pdf_objects, 4))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgBackend.vectorize(source, [], %{mode: :inline})

    assert diagnostic.message =~ "max_pdf_objects"
    Limits.install(Map.put(limits, :max_layout_boxes, 0))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgBackend.vectorize(source, [], %{mode: :inline})

    assert diagnostic.message =~ "max_layout_boxes"
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))
    assert {:ok, _pdf} = SvgBackend.vectorize(source, [], %{mode: :inline})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgBackend.rasterize(source, [], %{mode: :inline})

    assert diagnostic.message =~ "max_aggregate_svg_surface_pixels"
  end
end
