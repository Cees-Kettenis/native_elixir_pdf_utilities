defmodule NativeElixirPdfUtilities.Validators.HtmlValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.PageFurniture
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  test "source, link, furniture-fit, and styled-tree rules are validator owned" do
    assert {:ok, "<p>Hello</p>"} = HtmlValidator.validate_html_source("<p>Hello</p>")
    assert {:error, {:invalid_html, %{stage: :html}}} = HtmlValidator.validate_html_source(nil)

    assert {:ok, "p { color: red }"} =
             HtmlValidator.validate_css_source("p { color: red }")

    assert {:error, {:invalid_css, %{stage: :css, message: "CSS input must be valid UTF-8"}}} =
             HtmlValidator.validate_css_source(<<255>>)

    assert {:error, {:invalid_css, %{stage: :css}}} =
             HtmlValidator.validate_css_source(nil, :declarations)

    assert HtmlValidator.valid_link_url?("https://example.com/report")
    refute HtmlValidator.valid_link_url?("javascript:alert(1)")

    Enum.each(~w(row col rowgroup colgroup COL), fn scope ->
      assert HtmlValidator.valid_table_header_scope?(scope)
    end)

    refute HtmlValidator.valid_table_header_scope?("column")
    refute HtmlValidator.valid_table_header_scope?(nil)

    assert {:ok, 1} = HtmlValidator.validate_table_span_attribute(%{}, "colspan")

    assert {:ok, 1_000} =
             HtmlValidator.validate_table_span_attribute(%{"rowspan" => " 1000 "}, "rowspan")

    assert :error = HtmlValidator.validate_table_span_attribute(%{"span" => "0"}, "span")
    assert :error = HtmlValidator.validate_table_span_attribute(%{"span" => "many"}, "span")
    assert :error = HtmlValidator.validate_table_span_attribute(%{"span" => 2}, "span")
    assert :error = HtmlValidator.validate_table_span_attribute(:bad, "span")
    assert :error = HtmlValidator.validate_table_span_attribute(%{}, "width")

    for kind <- [:grid_placement, :grid_tracks, :table_span] do
      assert :ok = HtmlValidator.validate_layout_cardinality(kind, 1_000)

      assert {:error,
              {:resource_limit_exceeded,
               %{stage: :limits, reason: :resource_limit_exceeded, message: message}}} =
               HtmlValidator.validate_layout_cardinality(kind, 1_001)

      assert message =~ "1000-item limit"
    end

    assert :error = HtmlValidator.validate_layout_cardinality(:grid_tracks, 0)
    assert :error = HtmlValidator.validate_layout_cardinality(:unsupported, 1)

    assert :ok = HtmlValidator.validate_furniture_fit(:header, 12.0, 12.0)

    assert {:error, {:invalid_layout, %{stage: :layout}}} =
             HtmlValidator.validate_furniture_fit(:footer, 12.0, 10.0)

    assert {:error, {:invalid_layout, %{stage: :layout}}} =
             HtmlValidator.validate_furniture_fit(:sidebar, :bad, :bad)

    assert HtmlValidator.validate_layout_input(
             %{type: :document, children: [%{type: :text, text: :invalid, style: %{}}]},
             [],
             {:ok, {100.0, 100.0}},
             {:ok, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}}
           ) == {:error, :invalid_layout}

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_fallback_input(
               %{
                 type: :document,
                 children: [
                   %{type: :element, children: [%{type: :text, text: "Missing", style: %{}}]}
                 ]
               },
               :replace
             )

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_coverage(:not_a_document, :replace, "\uFFFD")

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_coverage(
               %{
                 type: :document,
                 children: [%{type: :element, children: [%{type: :invalid}]}]
               },
               :replace,
               "\uFFFD"
             )

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_coverage(
               %{
                 type: :document,
                 children: [
                   %{type: :text, _font_candidates: [%{}], _font_graphemes: [:invalid]}
                 ]
               },
               :replace,
               "\uFFFD"
             )
  end

  test "local document resources are confined beneath their base directory" do
    fixture_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-resource-validator")
    resource_root = Path.join(fixture_dir, "root")
    inside_path = Path.join(resource_root, "inside.png")
    outside_path = Path.join(fixture_dir, "outside.png")
    outside_directory = Path.join(fixture_dir, "outside-directory")
    symlink_path = Path.join(resource_root, "linked.png")
    directory_symlink_path = Path.join(resource_root, "linked-directory")
    File.mkdir_p!(resource_root)
    File.mkdir_p!(outside_directory)
    File.write!(inside_path, "inside")
    File.write!(outside_path, "outside")
    File.write!(Path.join(outside_directory, "nested.png"), "outside")
    :ok = File.ln_s(outside_path, symlink_path)
    :ok = File.ln_s(outside_directory, directory_symlink_path)

    assert {:ok, ^inside_path} =
             HtmlValidator.validate_local_resource_path("inside.png", resource_root)

    assert {:ok, ^inside_path} =
             HtmlValidator.validate_local_resource_path(inside_path, resource_root)

    assert {:ok, ^inside_path} =
             HtmlValidator.validate_local_resource_path(
               "inside.png",
               "file://localhost#{resource_root}"
             )

    assert {:ok, ^resource_root} =
             HtmlValidator.validate_local_resource_path(".", resource_root)

    for {source, base_url} <- [
          {outside_path, resource_root},
          {"../outside.png", resource_root},
          {"linked.png", resource_root},
          {"linked-directory/nested.png", resource_root},
          {"missing.png", resource_root},
          {"inside.png", nil},
          {"inside.png", "https://example.com"},
          {"inside.png", "file://remote#{resource_root}"},
          {"bad\0name.png", resource_root},
          {"https://example.com/image.png", resource_root},
          {:invalid, resource_root}
        ] do
      assert {:error,
              {:invalid_document,
               %{
                 stage: :style,
                 reason: :invalid_document,
                 message: "local document resource path is not authorized by base_url"
               }}} = HtmlValidator.validate_local_resource_path(source, base_url)
    end
  after
    File.rm_rf(Path.join(System.tmp_dir!(), "native-elixir-pdf-resource-validator"))
  end

  test "image resource budgets bound count, source bytes, and retained decoded bytes" do
    count_budget = HtmlValidator.new_image_budget()

    for _image <- 1..1_000 do
      assert :ok = HtmlValidator.reserve_image_source(count_budget, 0)
    end

    assert {:error, {:resource_limit_exceeded, count_diagnostic}} =
             HtmlValidator.reserve_image_source(count_budget, 0)

    assert count_diagnostic.stage == :limits
    assert count_diagnostic.message == "image count exceeds the limit"

    source_budget = HtmlValidator.new_image_budget()

    for _image <- 1..5 do
      assert :ok = HtmlValidator.reserve_image_source(source_budget, 10_000_000)
    end

    assert {:error, {:resource_limit_exceeded, source_diagnostic}} =
             HtmlValidator.reserve_image_source(source_budget, 1)

    assert source_diagnostic.stage == :limits
    assert source_diagnostic.message == "aggregate image source bytes exceed the limit"

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlValidator.reserve_image_source(HtmlValidator.new_image_budget(), 10_000_001)

    decoded_budget = HtmlValidator.new_image_budget()

    for _image <- 1..20 do
      assert :ok = HtmlValidator.reserve_decoded_image(decoded_budget, 1_000, 1_000, 4)
    end

    assert {:error, {:resource_limit_exceeded, decoded_diagnostic}} =
             HtmlValidator.reserve_decoded_image(decoded_budget, 1, 1, 1)

    assert decoded_diagnostic.stage == :limits
    assert decoded_diagnostic.message == "aggregate decoded image bytes exceed the limit"

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlValidator.reserve_decoded_image(
               HtmlValidator.new_image_budget(),
               10_000,
               2_000,
               3
             )

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.reserve_image_source(HtmlValidator.new_image_budget(), -1)

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.reserve_decoded_image(HtmlValidator.new_image_budget(), 0, 1, 3)

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.reserve_image_source(make_ref(), 1)
  end

  test "render requests and paths are validated at the shared boundary" do
    assert :ok =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [margin: 10],
               Font.normalize_options(margin: 10)
             )

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [margni: 10],
               Font.normalize_options(margni: 10)
             )

    assert {:error, {:invalid_encoding, %{stage: :html}}} =
             HtmlValidator.validate_render_request(<<255>>, [], Font.normalize_options([]))

    assert :ok =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [unsupported_glyphs: :replace],
               Font.normalize_options(unsupported_glyphs: :replace)
             )

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               message: "unsupported_glyphs must be :replace or :error"
             }}} =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [unsupported_glyphs: :ignore],
               Font.normalize_options(unsupported_glyphs: :ignore)
             )

    assert {:ok, %{input_path: "input.html", output_path: "output.pdf"}} =
             HtmlValidator.validate_paths("input.html", "output.pdf")

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [base_url: 123],
               Font.normalize_options(base_url: 123)
             )

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [default_font: 123],
               Font.normalize_options(default_font: 123)
             )

    assert Style.load_stylesheets(%{type: :document, children: []}, stylesheets: [123]) ==
             {:error, :invalid_stylesheet_options}
  end

  test "asset maps, resolvers, and resolver results are validated at the shared boundary" do
    resolver = fn _request -> :not_found end

    assert :ok =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [
                 assets: %{
                   "asset:logo" => {:bytes, <<1, 2, 3>>},
                   "asset:file" => {:file, "/tmp/logo.png"}
                 },
                 asset_resolver: resolver
               ],
               Font.normalize_options(
                 assets: %{
                   "asset:logo" => {:bytes, <<1, 2, 3>>},
                   "asset:file" => {:file, "/tmp/logo.png"}
                 },
                 asset_resolver: resolver
               )
             )

    for opts <- [
          [assets: []],
          [assets: %{1 => {:bytes, <<1>>}}],
          [assets: %{"asset:x" => <<1>>}],
          [assets: %{"asset:x" => {:file, ""}}],
          [asset_resolver: :not_a_function]
        ] do
      assert {:error, {:invalid_options, %{stage: :options}}} =
               HtmlValidator.validate_render_request(
                 "<p>Hello</p>",
                 opts,
                 Font.normalize_options(opts)
               )
    end

    assert {:ok, "bytes"} =
             HtmlValidator.validate_asset_resolver_result({:ok, "bytes"}, "asset:x")

    assert :not_found = HtmlValidator.validate_asset_resolver_result(:not_found, "asset:x")

    assert {:error, {:invalid_document, %{stage: :asset, source: "asset:x"}}} =
             HtmlValidator.validate_asset_resolver_result({:ok, 123}, "asset:x")
  end

  test "SVG raster budgets are validated before native rendering" do
    svg = ~s(<svg xmlns="http://www.w3.org/2000/svg" width="200" height="100"></svg>)

    assert {:ok, [width: 400, height: 200]} =
             HtmlValidator.validate_svg_raster(svg, width: 400)

    assert {:ok, [width: 200, height: 100]} =
             HtmlValidator.validate_svg_raster(svg, [])

    percentage_svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="50%" height="50%" viewBox="0 0 20 10"></svg>)

    assert {:ok, [width: 10, height: 5]} =
             HtmlValidator.validate_svg_raster(percentage_svg, [])

    for {unit, expected_width} <- [
          {"px", 1},
          {"pt", 1},
          {"pc", 16},
          {"mm", 4},
          {"cm", 38},
          {"in", 96},
          {"q", 1}
        ] do
      unit_svg =
        ~s(<svg xmlns="http://www.w3.org/2000/svg" width="1#{unit}" height="1"></svg>)

      assert {:ok, [width: ^expected_width, height: 1]} =
               HtmlValidator.validate_svg_raster(unit_svg, [])
    end

    height_sized_svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="2" height="1"></svg>)

    assert {:ok, [width: 20, height: 10]} =
             HtmlValidator.validate_svg_raster(height_sized_svg, height: 10)

    assert {:error,
            {:resource_limit_exceeded,
             %{
               stage: :limits,
               reason: :resource_limit_exceeded,
               message: dimension_message
             }}} = HtmlValidator.validate_svg_raster(svg, width: 8_193)

    assert dimension_message =~ "8192-pixel per-axis limit"

    extreme_intrinsic_svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="4294967295" height="1"></svg>)

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlValidator.validate_svg_raster(extreme_intrinsic_svg, [])

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :limits, reason: :resource_limit_exceeded, message: pixel_message}}} =
             HtmlValidator.validate_svg_raster(svg, width: 8_192, height: 4_096)

    assert pixel_message =~ "16777216-pixel limit"

    oversized_source =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1">) <>
        :binary.copy(" ", 5_000_000) <> "</svg>"

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :limits, reason: :resource_limit_exceeded, message: source_message}}} =
             HtmlValidator.validate_svg_raster(oversized_source, [])

    assert source_message =~ "5000000-byte limit"

    for invalid_svg <- [
          "not SVG",
          ~s(<svg width="1" height="1" viewBox="0 0 nope 1"></svg>),
          ~s(<svg width="0" height="1"></svg>),
          ~s(<svg width="0px" height="1"></svg>),
          ~s(<svg width="auto" height="1"></svg>),
          ~s(<svg width="1e9999" height="1"></svg>)
        ] do
      assert {:error, {:invalid_document, %{stage: :style}}} =
               HtmlValidator.validate_svg_raster(invalid_svg, [])
    end

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.validate_svg_raster(svg, width: 0)

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.validate_svg_raster(:not_svg, [])
  end

  test "layout pagination and furniture inputs share normalized page geometry" do
    styled_tree = %{type: :document, children: []}

    assert {:ok, {792.0, 612.0} = page_size} =
             PageGeometry.normalize_page_size("letter landscape")

    assert :ok = HtmlValidator.validate_page_size(page_size)
    assert {:error, :invalid_page_size} = HtmlValidator.validate_page_size({0.0, 612.0})

    assert {:ok, canonical_margins} = PageGeometry.normalize_margins("5pt 10pt")
    assert :ok = HtmlValidator.validate_margins(canonical_margins)
    assert :ok = HtmlValidator.validate_printable_area(page_size, canonical_margins)

    assert {:error, :invalid_margin} =
             HtmlValidator.validate_printable_area({10.0, 10.0}, canonical_margins)

    assert {:error, :invalid_margin} =
             HtmlValidator.validate_margins(Map.put(canonical_margins, :extra, 0.0))

    assert {:ok, margins} = PageGeometry.normalize_margins("5pt 10pt")

    assert :ok =
             HtmlValidator.validate_layout_input(
               styled_tree,
               [page_size: {100, 80}, margin: "5pt 10pt"],
               PageGeometry.normalize_page_size({100, 80}),
               PageGeometry.normalize_margins("5pt 10pt")
             )

    assert margins == %{top: 5.0, right: 10.0, bottom: 5.0, left: 10.0}

    assert HtmlValidator.validate_layout_input(
             styled_tree,
             [:not_options],
             {:error, :invalid_page_size},
             {:error, :invalid_margin}
           ) ==
             {:error, :invalid_layout}

    layout_tree = %{
      type: :layout,
      page_size: {100.0, 80.0},
      margins: margins,
      boxes: []
    }

    assert :ok = HtmlValidator.validate_pagination_input(layout_tree, [], {:ok, margins})

    pages = [%{size: {100.0, 80.0}, boxes: []}]

    assert :ok =
             HtmlValidator.validate_furniture_input(
               pages,
               layout_tree,
               [page_furniture: [header: "Page {{page}}"]],
               {:ok, margins},
               PageFurniture.normalize_option(header: "Page {{page}}")
             )

    assert {:ok, canonical_furniture} =
             PageFurniture.normalize_option(header: "Page {{page}}")

    assert :ok = HtmlValidator.validate_furniture_option(canonical_furniture)

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlValidator.validate_furniture_option(%{header: %{}, footer: %{}, extra: %{}})

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlValidator.validate_furniture_input(
               pages,
               layout_tree,
               [],
               {:ok, margins},
               {:ok, %{header: %{}, footer: %{}, extra: %{}}}
             )

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlValidator.validate_furniture_input(
               [],
               %{},
               [:not_options],
               {:error, :invalid_margin},
               {:error, :invalid_furniture_container}
             )

    assert {:error, {:invalid_layout, %{stage: :layout}}} =
             HtmlValidator.validate_furniture_input(
               :not_pages,
               :not_layout,
               :not_options,
               {:error, :invalid_margin},
               {:error, :invalid_furniture_container}
             )
  end

  test "font configuration is normalized before the loader consumes it" do
    assert {:ok,
            [
              %{
                family: "Example Sans",
                path: ["/tmp/example.ttf"],
                weight: 700,
                style: :italic
              }
            ] = normalized} =
             Font.normalize_configs([
               [
                 family: "Example Sans",
                 path: "/tmp/example.ttf",
                 weight: "bold",
                 style: "italic"
               ]
             ])

    assert :ok = HtmlValidator.validate_font_configs(normalized)

    assert :error = Font.normalize_configs([[family: "Missing path"]])
    assert :error = Font.normalize_configs([[family: "Empty", path: ""]])

    assert :error =
             HtmlValidator.validate_font_configs([
               %{family: "Example", path: [], weight: 400, style: :normal}
             ])

    assert :error = HtmlValidator.validate_font_configs(:not_a_list)
    assert :error = HtmlValidator.validate_font_configs([%{unexpected: true}])
  end

  test "validates static form attributes and element semantics" do
    for pair <- [
          {"input", "checked"},
          {"input", "disabled"},
          {"select", "disabled"},
          {"option", "selected"},
          {"option", "disabled"},
          {"textarea", "disabled"},
          {"button", "disabled"}
        ] do
      assert apply(HtmlValidator, :valid_boolean_form_attribute?, Tuple.to_list(pair))
    end

    refute HtmlValidator.valid_boolean_form_attribute?("input", "value")

    for {tag, name, value} <- [
          {"input", "type", "RADIO"},
          {"input", "name", "answer"},
          {"select", "name", "status"},
          {"option", "value", "yes"},
          {"textarea", "value", "notes"},
          {"button", "type", "SUBMIT"},
          {"button", "value", "Save"}
        ] do
      assert HtmlValidator.valid_form_attribute?(tag, name, value)
    end

    refute HtmlValidator.valid_form_attribute?("input", "type", "email")
    refute HtmlValidator.valid_form_attribute?("select", "multiple", "multiple")

    text = %{type: :text, text: "Choice"}
    option = %{type: :element, tag: "option", attributes: %{}, children: [text]}
    selected = put_in(option.attributes, %{"selected" => ""})

    assert HtmlValidator.valid_form_element?("input", %{}, [])
    refute HtmlValidator.valid_form_element?("input", %{}, [text])
    assert HtmlValidator.valid_form_element?("select", %{}, [option, selected])
    assert HtmlValidator.valid_form_element?("option", %{}, [text])
    assert HtmlValidator.valid_form_element?("textarea", %{}, [])
    assert HtmlValidator.valid_form_element?("button", %{}, [text])
    refute HtmlValidator.valid_form_element?("unknown", %{}, [])
  end
end
