defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgImageFontPolicyTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{FontValidator, SvgValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "shape-only parents carry independently permitted child font bytes to the converter" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{SvgRasterizer, PngDecoder}

    source =
      "<svg width='60' height='20'><image href='svg-resource:child' width='60' height='20'/></svg>"

    child = "<svg width='60' height='20'><text y='15' font-size='12'>Child Ω</text></svg>"

    context = %{
      mode: :inline,
      resources: %{"svg-resource:child" => %{data: child, media_type: "image/svg+xml"}}
    }

    assert {:ok, png} =
             SvgRasterizer.rasterize(source, [], nil, [], context, system_font_discovery: true)

    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    assert Enum.any?(:binary.bin_to_list(image.alpha_data), &(&1 > 0))

    assert {:error, {:unsupported_html, diagnostic}} =
             SvgRasterizer.rasterize(source, [], nil, [], context, system_font_discovery: false)

    assert diagnostic.message =~ "font"
    assert diagnostic.source == "SVG text"

    fonts = [%{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), family: "Child font"}]

    assert {:ok, png} =
             SvgRasterizer.rasterize(source, [], nil, fonts, context,
               system_font_discovery: false
             )

    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    assert Enum.any?(:binary.bin_to_list(image.alpha_data), &(&1 > 0))
  end

  test "the genuine shape-only summary suppresses default discovery under zero font budgets" do
    assert {:ok, summary} =
             SvgValidator.validate_processing(
               "<svg xmlns='http://www.w3.org/2000/svg' width='10' height='10'><rect width='10' height='10'/></svg>",
               %{mode: :image}
             )

    assert summary.text == []
    Limits.install(%{Limits.effective() | max_font_source_bytes: 0, max_font_count: 0})
    assert {:ok, []} = FontValidator.prepare_svg_image_fonts([], [], summary)

    assert {:ok, []} =
             FontValidator.prepare_svg_image_fonts([], [system_font_discovery: true], summary)

    assert {:ok, []} =
             FontValidator.prepare_svg_image_fonts([], [system_font_discovery: false], summary)

    assert {:ok, []} = FontValidator.prepare_svg_image_fonts([])

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FontValidator.prepare_svg_image_fonts([], [])

    assert diagnostic.stage == :font
  end

  test "shape-only summaries preserve malformed option diagnostics rather than replacing discovery values" do
    assert {:ok, summary} =
             SvgValidator.validate_processing("<svg xmlns='http://www.w3.org/2000/svg'/>", %{
               mode: :image
             })

    for opts <- [nil, [:invalid], [system_font_discovery: :enabled], [system_font_discovery: nil]] do
      assert {:error, {:invalid_options, diagnostic}} =
               FontValidator.prepare_svg_image_fonts([], opts, summary)

      assert diagnostic.stage == :options
      assert diagnostic.reason == :invalid_options
      assert diagnostic.message != ""

      assert FontValidator.prepare_svg_image_fonts([], opts) ==
               {:error, {:invalid_options, diagnostic}}
    end
  end

  test "actual configured bytes remain admitted and malformed faces still fail for shape-only SVG" do
    assert {:ok, summary} =
             SvgValidator.validate_processing("<svg xmlns='http://www.w3.org/2000/svg'/>", %{
               mode: :image
             })

    bytes = File.read!("priv/fonts/dejavu/DejaVuSansMono.ttf")
    configs = [%{family: "Caller Alias", data: bytes}]
    assert {:ok, [face]} = FontValidator.prepare_svg_image_fonts(configs, [], summary)
    assert face.data == bytes and face.family == "Caller Alias"
    assert {:ok, [^face]} = FontValidator.prepare_svg_image_fonts(configs, [])

    for configs <- [nil, [%{}], [%{family: "Invalid", data: <<0>>}]] do
      assert {:error, {reason, diagnostic}} =
               FontValidator.prepare_svg_image_fonts(configs, [], summary)

      assert diagnostic.reason == reason
      assert diagnostic.stage in [:font, :options]
      assert diagnostic.message != ""
    end

    Limits.install(%{Limits.effective() | max_font_source_bytes: 0})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FontValidator.prepare_svg_image_fonts(configs, [], summary)

    assert diagnostic.stage == :font
  end

  test "real text chunks including whitespace keep the existing discovery policy" do
    for text <- ["SG", "<![CDATA[ ]]>"] do
      assert {:ok, summary} =
               SvgValidator.validate_processing(
                 "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='20'><text x='1' y='12'>#{text}</text></svg>",
                 %{mode: :image}
               )

      assert summary.text != [] and Enum.all?(summary.text, &is_binary/1)
      assert {:ok, faces} = FontValidator.prepare_svg_image_fonts([], [], summary)
      assert {:ok, ^faces} = FontValidator.prepare_svg_image_fonts([], [])
      assert [%{data: data}] = faces
      assert {:ok, family} = FontValidator.prepare_font_family_name(data)
      assert family != ""

      assert {:ok, []} =
               FontValidator.prepare_svg_image_fonts([], [system_font_discovery: false], summary)
    end
  end

  test "malformed summary text facts return actionable shared diagnostics" do
    assert {:ok, actual} =
             SvgValidator.validate_processing(
               "<svg xmlns='http://www.w3.org/2000/svg'><text>A</text></svg>",
               %{mode: :image}
             )

    for summary <- [
          nil,
          Map.delete(actual, :text),
          %{actual | text: ""},
          %{actual | text: [1]},
          %{actual | text: ["A" | :invalid]}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               FontValidator.prepare_svg_image_fonts([], [], summary)

      assert diagnostic.stage == :font
      assert diagnostic.operation == :prepare_svg_image_fonts
      assert diagnostic.source == "SVG summary.text"
      assert diagnostic.message =~ "binary text chunks"
    end
  end

  test "the converter caller discovers genuine permitted bytes only for actual SVG text" do
    source = "<svg width='30' height='20'><text y='15' font-size='12'>SG</text></svg>"

    assert {:ok, png} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(
               source,
               [],
               nil,
               [],
               %{mode: :image},
               system_font_discovery: true
             )

    assert {:ok, image} = NativeElixirPdfUtilities.HtmlToPdf.PngDecoder.decode(png, nil, false)
    assert Enum.any?(:binary.bin_to_list(image.alpha_data), &(&1 > 0))

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(
               source,
               [],
               nil,
               [],
               %{mode: :image},
               system_font_discovery: false
             )

    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.reason == :unsupported_html and diagnostic.source == "SVG text"
    assert diagnostic.message =~ "approved SVG font bytes"
  end

  test "actual shape conversion preserves zero font budgets and original malformed option diagnostics" do
    source = "<svg width='1' height='1'><rect width='1' height='1'/></svg>"
    Limits.install(%{Limits.effective() | max_font_source_bytes: 0, max_font_count: 0})

    assert {:ok, _} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(
               source,
               [],
               nil,
               [],
               %{mode: :image},
               system_font_discovery: true
             )

    assert {:error, {:invalid_options, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer.rasterize(
               source,
               [],
               nil,
               [],
               %{mode: :image},
               system_font_discovery: :enabled
             )

    assert diagnostic.reason == :invalid_options and diagnostic.stage == :options
    assert diagnostic.message =~ "must be true or false"
  end

  test "selected document extraction retains an explicit default from two genuine configured faces" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{ResourceContext, SvgScene}
    alias NativeElixirPdfUtilities.Validators.HtmlValidator
    first = File.read!("priv/fonts/dejavu/DejaVuSans.ttf")
    second = File.read!("priv/fonts/dejavu/DejaVuSansMono.ttf")
    fonts = [%{family: "DejaVu Sans", data: first}, %{family: "DejaVu Sans Mono", data: second}]
    options = [fonts: fonts, default_font: "DejaVu Sans Mono", system_font_discovery: false]
    source = "<svg width='40' height='20'><text y='15'>SG</text></svg>"
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run(options, budget, fn _ ->
      assert {:ok, scene, snapshot} =
               SvgScene.extract(source, budget, options, system_font_discovery: false)

      assert Enum.map(snapshot, & &1.data) == [first, second]
      assert [selected] = scene.selected_fonts
      assert selected.data == second and selected.family == "DejaVu Sans Mono"
      assert {:ok, paint} = SvgScene.paint(scene, 30, nil)
      assert Enum.any?(:binary.bin_to_list(paint.image.alpha_data), &(&1 > 0))
    end)
  end
end
