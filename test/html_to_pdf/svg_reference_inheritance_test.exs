defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgReferenceInheritanceTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend

  alias NativeElixirPdfUtilities.Validators.{
    SvgReferenceValidator,
    SvgStyleValidator,
    SvgSceneValidator,
    SvgValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "the original Chromium opacity discrepancy is a precise unsupported reference diagnostic" do
    source = File.read!("test/fixtures/html_to_pdf/svg_reference_inherit_original.html")

    assert Base.encode16(:crypto.hash(:sha256, source), case: :lower) ==
             "62416a4ca70e8a78a828b33ec6c52163a81ddaec76e1f319d78ddb98fe94ebbc"

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(source)
    assert diagnostic.stage == :layout
    assert diagnostic.reason == :unsupported_html
    assert diagnostic.source == "g opacity"
    assert diagnostic.module == SvgReferenceValidator
    assert diagnostic.operation == :prepare
    assert diagnostic.message =~ "prepared instance effects"
    assert diagnostic.message =~ "explicit supported value"
  end

  test "authored effects in reused target descendants retain their unfinished instance scope" do
    for {property, instance} <- [
          {"opacity", "opacity='.5'"},
          {"clip-path", "clip-path='url(#c)'"},
          {"transform", "transform='translate(20 0)'"}
        ] do
      html =
        "<svg width=100 height=80><defs><clipPath id=c><rect width=70 height=60 /></clipPath><g id=target><g #{property}='inherit'><rect width=40 height=30 /></g></g></defs><use href='#target' #{instance} /></svg>"

      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.source == "g " <> property
      assert diagnostic.message =~ property <> ":inherit"
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

      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.source == "rect " <> property
      assert diagnostic.message =~ "allocation accounting"
    end
  end

  test "raw inline inheritance receives a diagnostic before raster allocations", %{limits: limits} do
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))

    for operation <- [&SvgBackend.vectorize/3, &SvgBackend.rasterize/3],
        property <- ["opacity", "transform"],
        contents <- [
          "<defs><g id='target' #{property}='inherit'><rect width='40' height='30' /></g></defs><use href='#target' opacity='.5' />",
          "<g opacity='.5'><g #{property}='inherit'><rect width='40' height='30' /></g></g>"
        ] do
      source = "<svg width='100' height='80'>#{contents}</svg>"

      assert {:error, {:unsupported_html, diagnostic}} =
               operation.(source, [width: 100, height: 80], %{mode: :inline})

      assert diagnostic.module == SvgReferenceValidator
      assert diagnostic.source == "g " <> property
      assert diagnostic.stage == :layout
    end
  end

  test "raw inline CSS and stylesheet effects require host preparation before allocations", %{
    limits: limits
  } do
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))

    for operation <- [&SvgBackend.vectorize/3, &SvgBackend.rasterize/3],
        {contents, expected_source} <- [
          {"<g style='opacity:inherit'><rect width='40' height='30'/></g>", "g style"},
          {"<g style='opacity:INHERIT'><rect width='40' height='30'/></g>", "g style"},
          {"<g style='opacity:.5'><rect width='40' height='30'/></g>", "g style"}
        ] do
      source = "<svg width='100' height='80'>#{contents}</svg>"

      assert {:error, {:invalid_document, diagnostic}} =
               operation.(source, [width: 100, height: 80], %{mode: :inline})

      assert diagnostic.stage == :layout
      assert diagnostic.source == expected_source
      assert diagnostic.module == SvgReferenceValidator
      assert diagnostic.message =~ "host cascade preparation"
      assert diagnostic.message =~ "allocation accounting"
    end
  end

  test "raw stylesheet effects retain the existing capability diagnostic before allocations", %{
    limits: limits
  } do
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))

    source =
      "<svg width='100' height='80'><style>g{opacity:.5}</style><g><rect width='40' height='30'/></g></svg>"

    for operation <- [&SvgBackend.vectorize/3, &SvgBackend.rasterize/3] do
      assert {:error, {:unsupported_html, diagnostic}} =
               operation.(source, [width: 100, height: 80], %{mode: :inline})

      assert diagnostic.stage == :layout
      assert diagnostic.source == "SVG"
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgBackendValidator
      assert diagnostic.message =~ "complete scenes remain pending"
    end
  end

  test "mixed-case original attributes use the shared SVG value authority" do
    for value <- ["INHERIT", "InHeRiT"] do
      assert {:error, {expected_reason, expected}} =
               SvgStyleValidator.apply(SvgStyleValidator.defaults(%{}), "opacity", value)

      source =
        "<svg width='100' height='80'><g opacity='#{value}'><rect width='40' height='30'/></g></svg>"

      for operation <- [&SvgBackend.vectorize/3, &SvgBackend.rasterize/3] do
        assert {:error, {reason, diagnostic}} =
                 operation.(source, [width: 100, height: 80], %{mode: :inline})

        assert reason == expected_reason
        assert diagnostic.stage == expected.stage
        assert diagnostic.message == expected.message
      end
    end
  end

  test "prepared metadata cannot hide original inline CSS inheritance or malformed declarations" do
    for {css, reason} <- [
          {"opacity:inherit", :unsupported_html},
          {"opacity:INHERIT", :invalid_css},
          {"opacity:", :invalid_css}
        ] do
      source =
        "<svg width='100' height='80'><defs><g id='target' style='#{css}'><rect width='40' height='30'/></g></defs><use href='#target' opacity='.5'/></svg>"

      assert {:ok, summary} = SvgValidator.validate_processing(source, %{mode: :inline})
      root = SvgSceneValidator.summary_tree(summary)
      [defs, use] = root.children
      [target] = defs.children
      [rect] = target.children
      target = %{target | children: [Map.put(rect, :style, %{})]}
      target = Map.put(target, :style, %{_svg_specified: %{"opacity" => ".5"}})
      defs = %{defs | children: [target]} |> Map.put(:style, %{})
      root = %{root | children: [defs, Map.put(use, :style, %{})]} |> Map.put(:style, %{})
      assert {:error, {actual_reason, diagnostic}} = SvgReferenceValidator.validate_tree(root)
      assert actual_reason == reason
      assert diagnostic.message != ""
    end
  end

  test "naturally inherited live instance paints and prepared static opacity remain supported" do
    html =
      "<svg width=100 height=80><defs><g id=target><rect width=10 height=10 fill=inherit stroke=currentColor /></g></defs><use href='#target' fill=blue color=red /><use href='#target' x=20 fill=green color=blue /><g opacity='.5'><g opacity='inherit'><rect x=40 width=10 height=10 fill=red /></g></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, doc} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
    content = doc.objects |> Map.values() |> Enum.map_join(&(Map.get(&1, :stream) || ""))
    assert content =~ "0 0 1 rg"
    assert content =~ "0 0.5019608 0 rg"
    assert length(Regex.scan(~r/\/ca 0\.5/, pdf)) == 2
  end

  test "qualified passive attributes do not become inherited SVG presentation declarations" do
    html =
      "<svg width=100 height=80 xmlns:q='urn:example:passive'><defs><g id=target q:opacity='inherit'><rect width=40 height=30 fill=red /></g></defs><use href='#target' opacity='.5' /></svg>"

    assert {:ok, _pdf} = HtmlToPdf.render(html)
  end

  test "prepared host CSS and the existing image-mode CSS contract remain supported" do
    html =
      "<svg width=100 height=80><g style='opacity:.5'><g opacity='inherit'><rect width=40 height=30 fill=red /></g></g></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert length(Regex.scan(~r/\/ca 0\.5/, pdf)) == 2

    source =
      "<svg width='100' height='80'><g opacity='.5'><g style='opacity:inherit'><rect width='40' height='30' fill='red'/></g></g></svg>"

    assert {:ok, png} = SvgBackend.rasterize(source, [width: 100, height: 80], %{mode: :image})
    assert binary_part(png, 0, 8) == <<137, 80, 78, 71, 13, 10, 26, 10>>
  end
end
