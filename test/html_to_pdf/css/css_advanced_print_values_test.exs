defmodule NativeElixirPdfUtilities.HtmlToPdf.CssAdvancedPrintValuesTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    CssColor,
    CssImageValue,
    Font,
    HtmlParser,
    Layout,
    Style
  }

  alias NativeElixirPdfUtilities.TestSupport.{FontCorpus, PngFixture}

  alias NativeElixirPdfUtilities.Validators.{
    CssColorProfileValidator,
    CssColorValidator,
    CssImageValueValidator,
    CssLinkParameterValidator,
    CssObjectViewBoxValidator,
    CssPageValidator,
    CssTextBoxValidator,
    FontMetricValidator,
    FontRenderingValidator
  }

  test "relative colorimetric RGB profiles retain the XYZ columns and TRC" do
    profile = profile_bytes()

    assert {:ok, parsed} = CssColorProfileValidator.prepare(profile)
    assert [_, {:gamma, 2.0}, {:table, <<0, 0, 255, 255>>, 2}] = parsed.curves

    assert {:ok, styled} =
             styles(
               """
               <style>@color-profile --Print{src:url(print.icc);rendering-intent:relative-colorimetric}
               p{color:color(--Print 1 0 0)}</style><p id=out>Print primary</p>
               """,
               assets: %{"print.icc" => {:bytes, profile}}
             )

    {r, g, b} = find(styled, "out").style.color
    assert_in_delta r, 1.0, 0.002
    assert_in_delta g, 0.0, 0.002
    assert_in_delta b, 0.0, 0.002

    assert {_, green, _} =
             CssColor.with_profiles(%{"--Print" => parsed}, fn ->
               assert {:ok, %{space: "xyz-d50"}} =
                        CssColorValidator.prepare("color(--Print 0 .5 0)", %{})

               CssColor.to_srgb("xyz-d50", [0.0962638855, 0.1792182922, 0.0242767334])
               |> List.to_tuple()
             end)

    assert_in_delta green, 0.5371, 0.002
    assert :error = CssColorProfileValidator.prepare(<<0, 0, 0, 0>>)

    assert :error =
             CssColorProfileValidator.prepare(
               profile_bytes(<<"curv", 0::32, 1::32, 0::16, 0::16>>)
             )

    assert {:ok, _} =
             CssColorProfileValidator.prepare(
               profile_bytes(<<"para", 0::32, 0::16, 0::16, 65536::signed-32>>)
             )

    assert %{} ==
             CssColorProfileValidator.rules(
               "@color-profile --a{src:url(a.icc)}@color-profile --a{src:none}" <>
                 "@color-profile --b{src:url(b.icc);rendering-intent:perceptual}@color-profile --c{}"
             )

    assert :error = CssColorValidator.prepare("color(--missing 1 0 0)", %{})
  end

  test "CMYK maps to a portable print color and HDR uses the zero-headroom endpoint" do
    assert {:ok, %{space: "srgb", channels: cmyk, alpha: 0.25}} =
             CssColorValidator.prepare("device-cmyk(.5 .75 1 .1 / 25%)", %{})

    assert cmyk == [0.45, 0.225, 0.0]

    assert {:ok, %{channels: cyan}} =
             CssColorValidator.prepare("device-cmyk(1, 0, 0, 0)", %{})

    assert cyan == [0.0, 1.0, 1.0]

    for source <- ["hdr-color(blue 2, red)", "color-hdr(red 0, blue 4)"] do
      assert {:ok, %{channels: red}} = CssColorValidator.prepare(source, %{})
      assert red == [1.0, 0.0, 0.0]
    end

    for invalid <- ["device-cmyk(0 1 2)", "hdr-color(red, blue)", "color-hdr(red -1, blue 2)"] do
      assert :error = CssColorValidator.prepare(invalid, %{})
    end

    for {space, white} <- [
          {"rec2100-linear", 1.0},
          {"rec2100-pq", 0.5806889},
          {"rec2100-hlg", 0.75}
        ] do
      assert {:ok, %{space: ^space}} =
               CssColorValidator.prepare("color(#{space} #{white} #{white} #{white})", %{})

      assert Enum.all?(
               CssColor.to_srgb(space, List.duplicate(white, 3)),
               &(abs(1.0 - &1) < 0.015)
             )

      assert Enum.all?(
               CssColor.convert("xyz-d65", [0.95047, 1, 1.08883], space),
               &(abs(white - &1) < 0.015)
             )
    end
  end

  test "perceptual HDR uses the pinned diffuse-white and sRGB lime vectors" do
    for {source, expected} <- [
          {"ictcp(58.069% 0 0)", [1, 1, 1]},
          {"jzazbz(.22207 -.00016 -.00012)", [1, 1, 1]},
          {"ictcp(.5393 -.2643 -.0625)", [0, 1, 0.188]},
          {"jzazbz(.17542 -.1179 .1092)", [0, 1, 0.188]},
          {"jzczhz(.17542 .16071 137.19)", [0, 1, 0.188]}
        ] do
      assert {:ok, color} = CssColorValidator.prepare(source, %{})
      assert CssColorValidator.valid_prepared?(color)

      for {value, expected} <- Enum.zip(CssColor.to_srgb(color.space, color.channels), expected) do
        assert_in_delta value,
                        expected,
                        0.008,
                        source <>
                          " produced " <> inspect(CssColor.to_srgb(color.space, color.channels))
      end
    end

    for source <- [
          "ictcp(from white i ct cp)",
          "jzazbz(from white j a b)",
          "jzczhz(from white j c h)",
          "color(from white rec2100-linear r g b)",
          "color-mix(in jzczhz shorter hue, white, black)"
        ] do
      prepared = CssColorValidator.prepare(source, %{})
      assert match?({:ok, _}, prepared), source
      {:ok, color} = prepared
      assert CssColorValidator.valid_prepared?(color)
    end
  end

  test "cross-fade uses premultiplied color and keeps unallocated alpha transparent" do
    assert {:ok, value} = CssImageValueValidator.prepare("cross-fade(red 25%, blue 25%)", %{})

    assert {:ok, loaded} =
             CssImageValue.load(value, [], fn _ -> flunk("solid fade loaded an asset") end)

    assert {:ok, image} = CssImageValue.rasterize(loaded, 0.75, 0.75, %{})
    assert image.data == :binary.copy(<<128, 0, 128>>, image.width_px * image.height_px)
    assert image.alpha_data == :binary.copy(<<128>>, image.width_px * image.height_px)

    for source <- ["cross-fade(red, blue)", "cross-fade(75% red, 75% blue)"] do
      assert {:ok, value} = CssImageValueValidator.prepare(source, %{})
      assert {:ok, image} = CssImageValue.rasterize(value, 0.75, 0.75, %{})
      assert image.data == :binary.copy(<<128, 0, 128>>, image.width_px * image.height_px)
      refute Map.has_key?(image, :alpha_data)
    end

    assert {:ok, zero} = CssImageValueValidator.prepare("cross-fade(red 0%, blue 0%)", %{})

    assert {:ok, transparent} = CssImageValue.rasterize(zero, 0.75, 0.75, %{})

    assert transparent.data ==
             :binary.copy(<<0>>, transparent.width_px * transparent.height_px * 3)

    assert transparent.alpha_data ==
             :binary.copy(<<0>>, transparent.width_px * transparent.height_px)

    for source <- [
          "cross-fade(red -1%, blue)",
          "cross-fade(none, blue)",
          "cross-fade(red 30% 30%, blue)"
        ] do
      assert :error = CssImageValueValidator.prepare(source, %{})
    end
  end

  test "image crops implicit density and element resolves only a same-document static image" do
    png =
      PngFixture.build(2, 1, 6, 8, 0, fn
        0, 0 -> [255, 0, 0, 32]
        1, 0 -> [0, 0, 255, 192]
      end)

    assert {:ok, styled} =
             styles(
               """
               <style>
               i{display:block;width:10px;height:10px}
               #crop{background-image:image('strip.png#xywh=percent:50,0,50,100', red)}
               #match{background-image:element(#source)}
               #source{display:none}
               </style><img id=source src=strip.png><i id=crop></i><i id=match></i>
               """,
               assets: %{"strip.png" => {:bytes, png}}
             )

    assert [%{image: cropped}] = find(styled, "crop").style.background_images
    assert cropped.data == <<0, 0, 255>> and cropped.alpha_data == <<192>>
    assert cropped.width_px == 1 and cropped.width == 0.75
    assert [%{image: %{width_px: 2}}] = find(styled, "match").style.background_images

    assert {:error, {:invalid_document, _}} =
             styles("<div style='background-image:element(#source)'></div><div id=source></div>")

    assert :error = CssImageValueValidator.prepare("element(div)", %{})
    assert :error = CssImageValueValidator.prepare("element(#x #y)", %{})
  end

  test "object-view-box changes natural allocation and retains the original image on paint" do
    assert {:ok, box} = CssObjectViewBoxValidator.prepare("inset(25% 20% 0 10%)")
    assert {:ok, {left, 20.0, 70.0, 60.0}} = CssObjectViewBoxValidator.resolve(box, 100, 80, %{})
    assert_in_delta left, 10, 0.0001
    assert {:ok, box} = CssObjectViewBoxValidator.prepare("rect(auto auto 75% 10%)")
    assert {:ok, {left, top, 90.0, 60.0}} = CssObjectViewBoxValidator.resolve(box, 100, 80, %{})
    assert_in_delta left, 10, 0.0001
    assert top == 0
    assert {:ok, box} = CssObjectViewBoxValidator.prepare("xywh(10% 5% 25% 60%)")
    assert {:ok, {left, top, 25.0, 48.0}} = CssObjectViewBoxValidator.resolve(box, 100, 80, %{})
    assert_in_delta left, 10, 0.0001
    assert_in_delta top, 4, 0.0001
    assert {:ok, rem_box} = CssObjectViewBoxValidator.prepare("xywh(1rem 1pt 4pt 5pt)")
    assert CssObjectViewBoxValidator.valid_style?(%{object_view_box: rem_box})

    assert {:ok, bounds} =
             CssObjectViewBoxValidator.resolve(rem_box, 100, 80, %{_root_font_size: 15})

    assert bounds == {15, 1.0, 4.0, 5.0}
    refute CssObjectViewBoxValidator.valid_style?(%{object_view_box: {:xywh, [:auto, 0, 1, 1]}})
    refute CssObjectViewBoxValidator.valid_style?(%{object_view_box: :invalid})

    assert {:error, {:invalid_layout, _}} =
             CssObjectViewBoxValidator.resolve({:xywh, [:bad, 0, 1, 1]}, 100, 80, %{})

    assert {:ok, :none} = CssObjectViewBoxValidator.prepare("none")

    for invalid <- [
          "auto",
          "inset(auto)",
          "xywh(0 0 -1px 1px)",
          "none(",
          "circle(50%)",
          "xywh(auto 0 1px 1px)"
        ],
        do: assert(:error = CssObjectViewBoxValidator.prepare(invalid))

    assert {:ok, styled} =
             styles("<img src=tile.png style='display:block;object-view-box:inset(0 50% 0 0)'>",
               assets: %{"tile.png" => {:bytes, PngFixture.solid_rgb(12, 8)}}
             )

    assert {:ok, %{boxes: boxes}} = Layout.layout(styled, margin: 0)

    assert %{width: 9.0, height: 6.0, clip: %{width: 4.5}} =
             Enum.find(boxes, &(&1.type == :image))
  end

  test "SVG paint variables use property, fragment and URL-modifier precedence" do
    svg = """
    <svg xmlns="http://www.w3.org/2000/svg" width="4" height="3">
      <rect width="4" height="3" fill="env(--ink, red)" />
    </svg>
    """

    assert {:ok, styled} =
             styles(
               """
               <style>
               b{display:block;width:10px;height:10px;link-parameters:param(--ink, blue)}
               #property{background-image:url('motif.svg')}
               #fragment{background-image:url('motif.svg#param(--ink,lime)')}
               #modifier{background-image:url('motif.svg#param(--ink,lime)' param(--ink,black))}
               </style><b id=property></b><b id=fragment></b><b id=modifier></b>
               """,
               assets: %{"motif.svg" => {:bytes, svg}}
             )

    for {id, pixel} <- [
          {"property", <<0, 0, 255>>},
          {"fragment", <<0, 255, 0>>},
          {"modifier", <<0, 0, 0>>}
        ] do
      style = find(styled, id).style

      assert %{data: <<^pixel::binary-size(3), _::binary>>} =
               Map.get(style, :background_image) ||
                 style
                 |> Map.get(:background_images, [])
                 |> List.first()
                 |> then(&(&1 && &1.image))
    end

    assert {:ok, %{"--a" => "", "--b" => "3px"}} =
             CssLinkParameterValidator.prepare("param(--a,),param(--b,3px)")

    assert {:ok, %{}} = CssLinkParameterValidator.prepare("none")
    assert :error = CssLinkParameterValidator.prepare("param(--image,url(other.svg))")

    assert {"motif.svg#details", %{"--a" => "lime"}} =
             CssLinkParameterValidator.reference("motif.svg#details&param%28--a%2Clime%29")
  end

  test "a named group records its own page number across a different nested page name" do
    html = """
    <style>
    @page {size:200pt 140pt;margin:20pt}
    @page :nth(2) { @top-left{content:'Physical two'} }
    @page :nth(2 of chapter) { @top-right{content:'Chapter two'} }
    body{margin:0;line-height:14pt}
    article{page:chapter;break-before:page}.middle{page:insert}.next{break-before:page}
    </style>
    <article><p>Opening</p><aside class=middle>Insert</aside><p class=next>Continued</p></article>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Physical two"
    assert text =~ "Chapter two"
    assert Enum.any?(String.split(text, "\f"), &(&1 =~ "Chapter two" and &1 =~ "Insert"))
    assert {:error, _} = CssPageValidator.rules("@page:nth(nope){margin:10pt}")
  end

  test "text-box uses the first and last line's edges and text-fit scales a plain text line" do
    font = File.read!(FontCorpus.path("selected-variable.ttf"))
    opts = [fonts: [%{family: "Metrics", data: font}], default_font: "Metrics"]

    assert {:ok, style} =
             styles(
               """
               <style>
                 html,body,p{margin:0}
                 p{font-size:20pt;line-height:30pt}
                 #trim{text-box:cap alphabetic}
                 #fit{width:60pt;text-fit:grow per-line-all 200%}
               </style>
               <p id=trim>H</p><p id=fit>H</p>
               """,
               opts
             )

    assert {:ok, %{boxes: boxes}} = Layout.layout(style, margin: 0)
    [trimmed, fitted] = Enum.filter(boxes, &(&1.type == :text))
    assert_in_delta trimmed.line_box_height, 14.0, 0.0001
    assert_in_delta fitted.font_size, 40.0, 0.0001

    assert {:ok, %{line_fit_edge: {:ex, :text}}} =
             CssTextBoxValidator.prepare("line-fit-edge", "ex")

    assert {:ok, %{line_fit_edge: :leading}} =
             CssTextBoxValidator.prepare("line-fit-edge", "leading")

    assert {:ok, %{text_box_edge: :auto}} = CssTextBoxValidator.prepare("text-box-edge", "auto")

    assert {:ok, %{text_box_trim: :start}} =
             CssTextBoxValidator.prepare("text-box-trim", "trim-start")

    assert {:ok, %{text_box_trim: :none, text_box_edge: :auto}} =
             CssTextBoxValidator.prepare("text-box", "normal")

    assert {:ok, %{text_fit: {:shrink, :per_line, 0.5}}} =
             CssTextBoxValidator.prepare("text-fit", "shrink per-line 50%")

    assert {:ok, %{text_fit: {:none, :consistent, nil}}} =
             CssTextBoxValidator.prepare("text-fit", "none")

    assert :error = CssTextBoxValidator.prepare("line-fit-edge", "auto")
    assert :error = CssTextBoxValidator.prepare("text-box", "trim-start trim-end cap")
    assert :error = CssTextBoxValidator.prepare("text-fit", "grow 42px")
  end

  test "script face descriptors override size and baseline only for synthesized glyphs" do
    bytes = File.read!(FontCorpus.path("selected-variable.ttf"))

    assert {:ok, [font]} =
             Font.load_configured_faces([
               %{
                 family: "Metrics",
                 data: bytes,
                 superscript_size_override: "40% from-font",
                 superscript_position_override: "50%",
                 subscript_size_override: :normal,
                 subscript_position_override: "-30%"
               }
             ])

    assert {:ok, selected} =
             FontRenderingValidator.prepare(font, %{
               font_controls: %{:"variant-position" => ["super"]}
             })

    assert {:ok, synthetic} = FontRenderingValidator.prepare_grapheme(selected, "H")
    assert Font.used_size(synthetic, 20) == 8.0
    assert FontMetricValidator.baseline_offset(synthetic, 20) == 10.0
    assert {0.65, -0.3} = FontMetricValidator.script_metrics(font, :subscript, nil)
    assert :error = FontMetricValidator.prepare_descriptor("subscript-size-override", "-1%")
    assert :error = FontMetricValidator.prepare_descriptor("subscript-size-override", "20px")
  end

  test "font edge metrics participate in flex measurement and preserve forced lines" do
    assert {:ok, styled} =
             styles("""
             <style>
               div, p {margin:0}
               .root{display:flex;align-items:start}
               .trim{line-height:32pt;text-box:trim-both text}
               .fit{width:40pt;text-fit:shrink per-line 50%;white-space:pre}
               .limit{width:30pt;text-fit:shrink consistent;white-space:nowrap}
               .grow{width:60pt;text-fit:grow;line-fit-edge:ex alphabetic}
             </style><div class=root><p class=trim>Tall</p></div>
             <p class=fit>Wide wide wide<br>Last</p><p class=limit>Another long line</p><p class=grow>X</p>
             """)

    assert {:ok, %{boxes: boxes}} = Layout.layout(styled, margin: 0)
    runs = Enum.filter(boxes, &(&1.type == :text))
    assert Enum.find(runs, &(&1.text == "Last")).font_size == 12
    assert Enum.find(runs, &(&1.text == "Another long line")).font_size < 12
    assert Enum.find(runs, &(&1.text == "X")).font_size > 12
    assert {:ok, %{text_box_edge: :auto}} = CssTextBoxValidator.prepare("text-box", "trim-both")
    assert :error = CssTextBoxValidator.prepare("text-box-edge", "text text text")
    assert :error = CssTextBoxValidator.prepare("text-fit", "grow per-line-all 200% 300%")
    assert :error = CssTextBoxValidator.prepare("unknown", "none")
  end

  test "positional geometry uses the physical page number and the named group reset" do
    html = """
    <style>
      @page {size:160pt 120pt;margin:16pt}
      @page :nth(2 of part) {size:180pt 120pt}
      @page :nth(3) { @top-center{content:'Third'} }
      body{margin:0;line-height:14pt}
      section{page:part;break-after:page}
      p{margin:0} p + p{break-before:page}
    </style>
    <section><p>First</p><p>Second</p></section><section><p>Third body</p></section>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert pdf =~ "/MediaBox [0 0 180 120]"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Third"
    assert {:ok, rules} = CssPageValidator.rules("@page:nth(even){margin-top:10pt}")
    assert CssPageValidator.positional_geometry?(rules)
  end

  test "image crop accepts pixel units and retains the fallback for unsupported storage or bounds" do
    pixels = %{
      format: :png,
      width_px: 2,
      height_px: 1,
      width: 1.5,
      height: 0.75,
      data: <<255, 0, 0, 0, 0, 255>>,
      color_space: :device_rgb,
      bits_per_component: 8,
      decoded_stride: 8
    }

    for token <- ["pixel:1,0,1,1", "1,0,1,1"] do
      assert {:ok, value} = CssImageValueValidator.prepare("image('tile.png#xywh=#{token}')", %{})

      assert {:ok, %{image: %{data: <<0, 0, 255>>}}} =
               CssImageValue.load(value, [], fn "tile.png" -> {:ok, pixels} end)
    end

    for token <- ["", "em:1,0,1,1", "0,0,-1,1"] do
      assert :error = CssImageValueValidator.prepare("image('tile.png#xywh=#{token}')", %{})
    end

    assert {:ok, value} =
             CssImageValueValidator.prepare("image('tile.png#xywh=4,0,1,1', red)", %{})

    assert {:ok, %{kind: :solid}} = CssImageValue.load(value, [], fn _ -> {:ok, pixels} end)

    assert {:ok, %{kind: :solid}} =
             CssImageValue.load(value, [], fn _ -> {:ok, %{format: :jpeg}} end)
  end

  test "cross-fade samples oriented RGB and gray images at both supported channel depths" do
    assert {:ok, value} =
             CssImageValueValidator.prepare("cross-fade(url(tile.png) 100%, blue 0%)", %{})

    for {space, bits, data, alpha} <- [
          {:device_gray, 8, <<127>>, <<192>>},
          {:device_gray, 16, <<127, 128>>, <<192, 0>>},
          {:device_rgb, 16, <<127, 128, 0, 0, 0, 0>>, <<192, 0>>},
          {:device_rgb, 8, <<127, 0, 0>>, <<192>>}
        ],
        orientation <- 2..8 do
      image = %{
        format: :png,
        width_px: 1,
        height_px: 1,
        width: 0.75,
        height: 0.75,
        color_space: space,
        bits_per_component: bits,
        data: data,
        alpha_data: alpha,
        image_orientation: orientation
      }

      assert {:ok, loaded} = CssImageValue.load(value, [], fn _ -> {:ok, image} end)
      assert {:ok, raster} = CssImageValue.rasterize(loaded, 0.75, 0.75, %{})
      assert <<127, _::binary>> = raster.data
      assert <<192, _::binary>> = raster.alpha_data
      assert CssImageValueValidator.valid_loaded?(loaded)
    end

    assert {:ok, jpeg} = CssImageValue.load(value, [], fn _ -> {:ok, %{format: :jpeg}} end)
    assert {:error, {:not_implemented, _}} = CssImageValue.rasterize(jpeg, 0.75, 0.75, %{})
  end

  test "extended CSS image declarations load and authorize nested fade operands" do
    for {color, match} <- [
          {"device-cmyk(0 1 1 0)", [1, 0, 0]},
          {"hdr-color(red 0, blue 2)", [1, 0, 0]}
        ] do
      assert {:ok, %{channels: channels}} = CssColorValidator.prepare(color, %{})
      assert channels == match
    end

    assert :error = CssColorValidator.prepare("hdr-color(red 0 surplus, blue 2)", %{})

    assert {:ok, gradient} =
             CssColorValidator.prepare("color-mix(in ictcp, ictcp(none 0 0), white)", %{})

    assert CssColorValidator.valid_prepared?(gradient)
    assert [_, _, _] = CssColor.convert("rec2100-hlg", [0.25, 0.25, 0.25], "xyz-d65")
    assert [_, _, _] = CssColor.convert("srgb", [0.1, 0.1, 0.1], "rec2100-hlg")

    assert {:ok, pdf} =
             HtmlToPdf.render(
               """
               <p style="width:20pt;height:9pt;background-image:cross-fade(url(image.png) 60%, blue)">
               <img src="image.png" style="border-radius:5px;object-view-box:rect(0 0 0 0);width:8pt;height:8pt">
               <img src="image.png" style="border-radius:2px;object-view-box:rect(auto auto auto 2px);width:8pt;height:8pt">
               </p>
               """,
               assets: %{"image.png" => {:bytes, PngFixture.solid_rgb(10, 10)}}
             )

    assert pdf =~ "/Subtype /Image"

    assert {:error, {_, _}} =
             CssImageValueValidator.prepare("cross-fade(url(javascript:bad), blue)", %{})

    assert :error = CssObjectViewBoxValidator.prepare("inset()")
    assert :error = CssObjectViewBoxValidator.prepare(")")
  end

  @tag :tmp_dir
  test "an authorized stylesheet profile may be loaded from a configured style or a child document",
       %{tmp_dir: root} do
    profile = profile_bytes()
    rule = "@color-profile --Print{src:url(print.icc)} p{color:color(--Print 1 0 0)}"
    assets = %{"print.icc" => {:bytes, profile}}

    assert {:ok, tree} =
             styles("<p id=ink>Ink</p>", assets: assets, stylesheets: [{:css, rule}])

    assert elem(find(tree, "ink").style.color, 0) >= 0.99

    assert {:error, {:invalid_css, _}} =
             styles("<style>#{rule}</style><p id=ink>Missing profile</p>")

    assert {:error, {:invalid_css, _}} =
             styles("<style>#{rule}</style><p id=ink>Malformed profile</p>",
               assets: %{"print.icc" => {:bytes, "invalid"}}
             )

    File.write!(Path.join(root, "print.icc"), profile)
    sheet = Path.join(root, "profile.css")
    File.write!(sheet, "@color-profile --Trusted{src:url(print.icc)}")
    File.write!(Path.join(root, "child.html"), "<style>#{rule}</style><p>Child ink</p>")

    assert {:ok, pdf} =
             HtmlToPdf.render("<iframe src='child.html'></iframe>",
               base_url: root,
               stylesheets: [{:file, sheet}],
               system_font_discovery: false
             )

    assert {:error, {:resource_denied, _}} =
             HtmlToPdf.render(
               ~s|<iframe srcdoc="<style>#{rule}</style><p>Child ink</p>"></iframe>|,
               assets: assets,
               system_font_discovery: false
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Child ink"
    assert :error = CssColorProfileValidator.prepare(profile_bytes(<<"malformed!!!">>))
    assert :error = CssColorProfileValidator.prepare(:binary.replace(profile, "rXYZ", "junk"))
  end

  test "SVG environment variables preserve child styles and invalid parameter handling" do
    source = """
    <svg xmlns="http://www.w3.org/2000/svg" width="9" height="9">
      <style>rect {fill: env(--ink, red)}</style>
      <desc>Author caption</desc>
      <rect width="9" height="9" fill="blue" stroke="env(--unknown)" style="opacity:1" />
    </svg>
    """

    assert {:ok, prepared} = CssLinkParameterValidator.svg(source, %{"--ink" => "lime"})
    assert prepared =~ "fill: lime"
    assert prepared =~ "env(--unknown)"
    assert prepared =~ "Author caption"
    assert {"ordinary.svg", %{}} = CssLinkParameterValidator.reference("ordinary.svg")

    assert {:error, {:unsupported_html, _}} =
             CssLinkParameterValidator.modified_url(~s|url("javascript:bad" param(--ink, red))|)

    assert {:error, :invalid_document} =
             NativeElixirPdfUtilities.Validators.HtmlValidator.parse_background_image_reference(
               ~s|url("tile.svg" param(--x, url(elsewhere.svg)))|
             )
  end

  test "font edges and a capped nonterminal shrink line preserve layout" do
    assert {:ok, tree} =
             styles(
               """
               <style>p{margin:0;line-height:24pt}.limit{width:20pt;text-fit:shrink per-line 65%;white-space:pre-wrap}</style>
               <p style="text-box:cap alphabetic">Ink</p><p class=limit>Humongous word</p>
               """,
               default_font: "Helvetica"
             )

    assert {:ok, %{boxes: boxes}} = Layout.layout(tree, margin: 0)
    [trimmed | other] = Enum.filter(boxes, &(&1.type == :text))
    assert_in_delta trimmed.line_box_height, 1493 / 2048 * 12, 0.0001
    assert Enum.any?(other, &(abs(&1.font_size - 7.8) < 0.0001))
  end

  test "prepared built-in fonts use a deterministic cap edge" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p style='text-box:cap alphabetic;line-height:22pt'>Ink</p>"
             )

    assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
    builtin = %{type: :built_in, family: "Helvetica", pdf_name: "Helvetica"}

    tree =
      update_in(tree, [:children, Access.at(0), :style], &Map.put(&1, :font_face, builtin))

    assert {:ok, %{boxes: boxes}} = Layout.layout(tree)
    text = Enum.find(boxes, &(&1.type == :text))
    assert_in_delta text.line_box_height, 8.4, 0.0001
  end

  test "object view-box rejects nonfinite authored coordinates during layout" do
    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlToPdf.render(
               "<img src='image.png' style='object-view-box:xywh(calc(infinity * 1pt) 0 3pt 4pt)'>",
               assets: %{"image.png" => {:bytes, PngFixture.solid_rgb(3, 4)}}
             )
  end

  test "substitution prefilter recognizes non-ASCII custom function names" do
    alias NativeElixirPdfUtilities.Validators.CssSubstitutionValidator
    assert CssSubstitutionValidator.contains?("--🦄()")
    refute CssSubstitutionValidator.contains?("var(--ink)")
  end

  defp styles(source, options \\ []) do
    assert {:ok, dom} = HtmlParser.parse_document_detailed(source)
    Style.compute_detailed(dom, Keyword.put_new(options, :system_font_discovery, false))
  end

  defp find(node, id) do
    if get_in(node, [:attributes, "id"]) == id,
      do: node,
      else: Enum.find_value(Map.get(node, :children, []), &find(&1, id))
  end

  defp profile_bytes(red_curve \\ <<"curv", 0::32, 0::32>>) do
    tags = [
      {"rXYZ", xyz(0.43607, 0.22250, 0.01393)},
      {"gXYZ", xyz(0.38506, 0.71688, 0.09710)},
      {"bXYZ", xyz(0.14308, 0.06062, 0.71417)},
      {"rTRC", red_curve},
      {"gTRC", <<"curv", 0::32, 1::32, 512::16>>},
      {"bTRC", <<"curv", 0::32, 2::32, 0::16, 65535::16>>}
    ]

    start = 132 + length(tags) * 12

    {table, {payload, total}} =
      Enum.map_reduce(tags, {<<>>, start}, fn {name, value}, {payload, offset} ->
        padding = rem(4 - rem(byte_size(value), 4), 4)
        padded = value <> :binary.copy(<<0>>, padding)

        {<<name::binary, offset::32, byte_size(value)::32>>,
         {payload <> padded, offset + byte_size(padded)}}
      end)

    <<total::32, 0::32, 4, 0::24, "mntr", "RGB ", "XYZ ", 0::96, "acsp", 0::704,
      length(tags)::32>> <>
      IO.iodata_to_binary(table) <> payload
  end

  defp xyz(x, y, z),
    do:
      <<"XYZ ", 0::32, round(x * 65536)::signed-32, round(y * 65536)::signed-32,
        round(z * 65536)::signed-32>>
end

defmodule NativeElixirPdfUtilities.HtmlToPdf.CssLinkParameterBudgetTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{CssLinkParameterValidator, HtmlValidator}

  test "a linked SVG fallback honors the CSS decoded-byte limit" do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(Map.put(previous, :max_decoded_bytes, 1024))
    fallback = String.duplicate("red ", 512)

    svg = """
    <svg xmlns="http://www.w3.org/2000/svg" width="9" height="9">
      <rect width="9" height="9" style="fill:env(--ink, attr(missing, #{fallback}))" />
    </svg>
    """

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlValidator.with_render_budget(fn ->
               CssLinkParameterValidator.svg(svg, %{})
             end)
  end
end
