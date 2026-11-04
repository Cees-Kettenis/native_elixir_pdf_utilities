defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgFilterWorkLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgConverterValidator,
    SvgFilterWorkValidator
  }

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "the original small-source, large-radius trigger is rejected without native execution" do
    for radius <- ["512", "1e10", "1e300", "512+512"] do
      source = svg("<feMorphology radius='#{radius}'/>", 512)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})

      assert diagnostic.stage == :limits
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.module == HtmlValidator
      assert diagnostic.operation == :prepare
      assert diagnostic.source == "SVG"
      assert diagnostic.message =~ "max_svg_filter_work"
    end
  end

  test "inline, mapped and standalone SVGs share actionable work diagnostics" do
    Limits.install(Map.put(Limits.effective(), :max_svg_filter_work, 1))
    source = svg("<feMorphology radius='2'/>")

    for {html, options} <- [
          {source, []},
          {"<img src='filter.svg'>", [assets: %{"filter.svg" => {:bytes, source}}]}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(html, [system_font_discovery: false] ++ options)

      assert diagnostic.stage == :limits
      assert diagnostic.module == HtmlValidator
      assert diagnostic.message =~ "max_svg_filter_work (configured limit 1)"
    end

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgRasterizer.rasterize(source, [], nil)

    assert diagnostic.message =~ "reduce SVG size"
  end

  test "a configurable limit rejects a kernel while ordinary filters still render" do
    source = svg("<feConvolveMatrix order='3' kernelMatrix='0 0 0 0 1 0 0 0 0'/>")
    Limits.install(Map.put(Limits.effective(), :max_svg_filter_work, 1))

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})

    Limits.install(Limits.defaults())
    assert {:ok, <<137, 80, 78, 71, _::binary>>} = SvgRasterizer.rasterize(source, [], nil)

    assert {:ok, <<137, 80, 78, 71, _::binary>>} =
             SvgRasterizer.rasterize(svg("<feMorphology radius='1'/>"), [], nil)
  end

  test "filter work accumulates across conversions in one document and resets afterward" do
    source = svg("<feMorphology radius='1'/>")
    Limits.install(Map.put(Limits.effective(), :max_svg_filter_work, 200_000))

    assert :ok =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, _} =
                        SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})

               assert {:error, {:resource_limit_exceeded, _}} =
                        SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})

               :ok
             end)

    assert {:ok, _} = SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
  end

  test "final raster dimensions and a magnified viewBox are accounted for" do
    source = svg("<feMorphology radius='1'/>")
    Limits.install(Map.put(Limits.effective(), :max_svg_filter_work, 10_000_000))
    assert {:ok, _} = SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgConverterValidator.prepare(source, [width: 512, height: 512], nil, [], %{
               mode: :image
             })

    magnified =
      String.replace(source, "width='8' height='8'", "width='128' height='128' viewBox='0 0 1 1'")

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgConverterValidator.prepare(magnified, [], nil, [], %{mode: :image})
  end

  test "transforms and object bounding box units cannot conceal an expensive radius" do
    Limits.install(Map.put(Limits.effective(), :max_svg_filter_work, 500_000))

    for attributes <- [
          "transform='scale(1000)'",
          "transform='scale(1000, 2)'",
          "transform='matrix(1000 0 0 1000 0 0)'",
          "transform='skewX(89)'",
          "transform='skewY(89)'",
          "transform='scale(1e300)'",
          "transform='nonsense'",
          "transform='matrix(1)'"
        ] do
      source = svg("<feMorphology radius='1'/>", 8, attributes)

      assert {:error, {:resource_limit_exceeded, _}} =
               SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
    end

    source = svg("<feMorphology radius='.1'/>", 8, "", "primitiveUnits='objectBoundingBox'")

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
  end

  test "ordinary SVG transform syntax remains admitted" do
    for attributes <- [
          "",
          "transform='none'",
          "transform='translate(1)'",
          "transform='translate(1 2) rotate(10) rotate(10 2 2)'",
          "transform='matrix(1 0 0 1 0 0)'",
          "transform='skewX(1),skewY(1)'"
        ] do
      assert {:ok, _} =
               SvgConverterValidator.prepare(
                 svg("<feMorphology/>", 8, attributes),
                 [],
                 nil,
                 [],
                 %{mode: :image}
               )
    end
  end

  test "invalid, missing, fractional and extreme numeric parameters fail safely or use backend defaults" do
    for radius <- ["", "0", "0 2", "1 2", "-1", "1 2 3", "oops", ".5", "1e300"] do
      assert {:ok, _} =
               SvgConverterValidator.prepare(
                 svg("<feMorphology radius='#{radius}'/>"),
                 [],
                 nil,
                 [],
                 %{mode: :image}
               )
    end

    for order <- [nil, "1", "2 3", "0", "-1 3", "3.5", "1e300"] do
      attribute = if order == nil, do: "", else: "order='#{order}'"

      result =
        SvgConverterValidator.prepare(svg("<feConvolveMatrix #{attribute}/>"), [], nil, [], %{
          mode: :image
        })

      if order == "1e300",
        do: assert(match?({:error, {:resource_limit_exceeded, _}}, result)),
        else: assert(match?({:ok, _}, result))
    end

    for order <- ["oops", "", "1 2 3"] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgConverterValidator.prepare(
                 svg("<feConvolveMatrix order='#{order}'/>"),
                 [],
                 nil,
                 [],
                 %{mode: :image}
               )

      assert diagnostic.message =~ "convolution order"
    end
  end

  test "wrap convolution includes repeated edge adjustment work" do
    Limits.install(Map.put(Limits.effective(), :max_svg_filter_work, 50_000))

    assert {:ok, _} =
             SvgConverterValidator.prepare(svg("<feConvolveMatrix order='3'/>"), [], nil, [], %{
               mode: :image
             })

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgConverterValidator.prepare(
               svg("<feConvolveMatrix order='3' edgeMode='wrap'/>"),
               [],
               nil,
               [],
               %{mode: :image}
             )
  end

  test "unknown producer regions are rejected, including missing input-name fallbacks" do
    for input <- ["", "in='missing'", "in='paint'"] do
      source = svg("<feFlood result='paint'/><feMorphology #{input} radius='2'/>")

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})

      assert diagnostic.message =~ "intermediate filter region"
      assert diagnostic.module == SvgFilterWorkValidator
    end

    for input <-
          ~w(SourceGraphic SourceAlpha BackgroundImage BackgroundAlpha FillPaint StrokePaint) do
      source = svg("<feFlood/><feMorphology in='#{input}' radius='2'/>")
      assert {:ok, _} = SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
    end
  end

  test "explicit transformed filter regions and named preserving results remain bounded" do
    primitives =
      "<feFlood result='paint'/><feOffset in='paint' result='moved'/><feMorphology in='moved' radius='2'/>"

    attributes = "filterUnits='userSpaceOnUse' width='150' height='1'"

    assert {:ok, _} =
             SvgConverterValidator.prepare(
               svg(primitives, 8, "transform='rotate(45)'", attributes),
               [],
               nil,
               [],
               %{mode: :image}
             )

    # A rotation must not leave a long, thin filter region estimated as a one-pixel strip.
    Limits.install(Map.put(Limits.effective(), :max_svg_filter_work, 1_000_000))

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgConverterValidator.prepare(
               svg(primitives, 128, "transform='rotate(45)'", attributes),
               [],
               nil,
               [],
               %{mode: :image}
             )
  end

  test "native-generated result names cannot hide a producer's larger plane" do
    for producer <- ["<feFlood/>", "<feConvolveMatrix kernelMatrix='bad'/>"] do
      primitives =
        producer <> "<feOffset in='SourceGraphic'/><feMorphology in='result1' radius='512'/>"

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               SvgConverterValidator.prepare(svg(primitives), [], nil, [], %{mode: :image})

      assert diagnostic.message =~ "intermediate filter region"
    end

    primitives =
      "<feGaussianBlur stdDeviation='1'/><feOffset in='SourceGraphic'/><feMorphology in='result1'/>"

    assert {:ok, _} = SvgConverterValidator.prepare(svg(primitives), [], nil, [], %{mode: :image})
  end

  test "work estimation obeys existing CSS complexity limits" do
    Limits.install(Map.put(Limits.effective(), :max_css_transform_operations, 2))
    source = svg("<feMorphology/>", 8, "transform='translate(1) translate(2) translate(3)'")

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})

    assert diagnostic.message =~ "max_css_transform_operations"

    Limits.install(Map.put(Limits.defaults(), :max_aggregate_css_source_bytes, 1))
    source = svg("<feMorphology/>", 8, "style='filter:url(#f)'")

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
  end

  test "native work remains bounded when applications raise numeric magnitude limits" do
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 1.0e307))

    for attributes <- ["transform='scale(1e200)'", "transform='scale(1e200) scale(1e200)'"] do
      assert {:ok, _} =
               SvgConverterValidator.prepare(
                 svg("<feMorphology/>", 8, attributes),
                 [],
                 nil,
                 [],
                 %{mode: :image}
               )
    end

    assert {:ok, _} =
             SvgConverterValidator.prepare(
               svg(
                 "<feMorphology/>",
                 8,
                 "transform='scale(1000)'",
                 "filterUnits='userSpaceOnUse' width='1e307' height='1e307'"
               ),
               [],
               nil,
               [],
               %{mode: :image}
             )
  end

  test "CSS parse errors fail closed and stylesheet resource errors retain their reason" do
    source =
      String.replace(svg("<feMorphology/>"), "</svg>", "<style>rect {broken}</style></svg>")

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})

    for content <- [
          "<style><g>rect {filter:url(#f)}</g></style>",
          "<style><!-- comment -->rect {filter:url(#f)}</style>"
        ] do
      source = String.replace(svg("<feMorphology/>"), "</svg>", content <> "</svg>")

      assert {:ok, _} =
               SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
    end

    Limits.install(Map.put(Limits.effective(), :max_css_work, 1))

    source =
      String.replace(
        svg("<feMorphology/>"),
        "</svg>",
        "<style>rect {filter:url(#f)}</style></svg>"
      )

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
  end

  test "reused and CSS-attached filters are charged for every possible application" do
    Limits.install(Map.put(Limits.effective(), :max_svg_filter_work, 150_000))
    base = svg("<feMorphology radius='1'/>")
    assert {:ok, _} = SvgConverterValidator.prepare(base, [], nil, [], %{mode: :image})

    sources = [
      String.replace(base, "</svg>", "<rect width='8' height='8' filter='url(#f)'/></svg>"),
      String.replace(base, "filter='url(#f)'", "style='filter:url(#f)'"),
      String.replace(base, "filter='url(#f)'", "class='effect'")
      |> String.replace("</svg>", "<style>.effect {filter:url(#f)}</style></svg>"),
      String.replace(base, "filter='url(#f)'", "filter='url(#f) url(#f)'"),
      String.replace(base, "filter='url(#f)'", "style='filter:url(#f) url(#f)'"),
      String.replace(base, "filter='url(#f)'", "style='transform:scale(1000);filter:url(#f)'")
    ]

    for source <- sources do
      assert {:error, {:resource_limit_exceeded, _}} =
               SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
    end
  end

  test "presentation inheritance and native-only fragment spellings cannot bypass reuse accounting" do
    Limits.install(Map.put(Limits.effective(), :max_svg_filter_work, 150_000))

    inherited =
      "<svg width='8' height='8'><filter id='f'><feMorphology radius='1'/></filter><g filter='url(#f)'><rect filter='inherit' width='8' height='8'/><rect filter='inherit' width='8' height='8'/></g></svg>"

    unicode =
      svg("<feMorphology radius='1'/>")
      |> String.replace("id='f'", "id='é'")
      |> String.replace("url(#f)", "url(#é)")

    for source <- [inherited, unicode] do
      assert {:error, {:resource_limit_exceeded, _}} =
               SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
    end
  end

  test "uninterpretable CSS and unbounded paint-server contexts fail closed" do
    for content <- [
          "<style>rect {--size:2;filter:url(#f)}</style>",
          "<style>@media all {rect {filter:url(#f)}}</style>",
          "<pattern id='p'/>",
          "<marker id='m'/>",
          "<mask id='m'/>",
          "<filter id='other'><feImage href='#shape'/></filter><rect id='shape' width='1' height='1'/>"
        ] do
      source = String.replace(svg("<feMorphology radius='1'/>"), "</svg>", content <> "</svg>")

      assert {:error, {:resource_limit_exceeded, _}} =
               SvgConverterValidator.prepare(source, [], nil, [], %{mode: :inline, resources: %{}})
    end
  end

  test "filter inheritance and nested viewports use a full-plane radius bound" do
    Limits.install(Map.put(Limits.effective(), :max_svg_filter_work, 500_000))
    base = svg("<feMorphology radius='1'/>")

    for source <- [
          String.replace(
            base,
            "<filter id='f'",
            "<filter id='base'/><filter href='#base' id='f'"
          ),
          String.replace(base, "</svg>", "<svg width='1' height='1'/></svg>"),
          String.replace(base, "</svg>", "<symbol id='s'/></svg>")
        ] do
      assert {:error, {:resource_limit_exceeded, _}} =
               SvgConverterValidator.prepare(source, [], nil, [], %{mode: :image})
    end
  end

  test "embedded SVG work follows the parent canvas rather than its tiny intrinsic size" do
    child = svg("<feMorphology radius='2'/>", 1)

    outer =
      "<svg width='128' height='128'><image href='svg-resource:child' width='128' height='128'/></svg>"

    context = %{
      mode: :inline,
      resources: %{"svg-resource:child" => %{data: child, media_type: "image/svg+xml"}}
    }

    assert {:ok, _} = SvgConverterValidator.prepare(child, [], nil, [], %{mode: :image})

    assert {:error, {:resource_limit_exceeded, _}} =
             SvgConverterValidator.prepare(outer, [], nil, [], context)

    masked = String.replace(child, "</svg>", "<mask id='m'/></svg>")
    masked_context = put_in(context, [:resources, "svg-resource:child", :data], masked)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             SvgConverterValidator.prepare(outer, [], nil, [], masked_context)

    assert diagnostic.message =~ "rendering contexts"

    ordinary = "<svg width='1' height='1'><rect width='1' height='1'/></svg>"
    context = put_in(context, [:resources, "svg-resource:child", :data], ordinary)
    assert {:ok, _} = SvgConverterValidator.prepare(outer, [], nil, [], context)
  end

  defp svg(primitives, size \\ 8, shape_attributes \\ "", filter_attributes \\ "") do
    "<svg xmlns='http://www.w3.org/2000/svg' width='#{size}' height='#{size}'><defs><filter id='f' #{filter_attributes}>#{primitives}</filter></defs><rect width='#{size}' height='#{size}' filter='url(#f)' #{shape_attributes}/></svg>"
  end
end
