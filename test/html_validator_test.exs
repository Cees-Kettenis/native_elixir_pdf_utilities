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
             HtmlValidator.validate_font_fallback_input(%{
               type: :document,
               children: [
                 %{type: :element, children: [%{type: :text, text: "Missing", style: %{}}]}
               ]
             })

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_coverage(:not_a_document)

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_coverage(%{
               type: :document,
               children: [%{type: :element, children: [%{type: :invalid}]}]
             })

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_coverage(%{
               type: :document,
               children: [
                 %{type: :text, _font_candidates: [%{}], _font_graphemes: [:invalid]}
               ]
             })
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
end
