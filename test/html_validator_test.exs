defmodule NativeElixirPdfUtilities.Validators.HtmlValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  test "render requests and paths are validated at the shared boundary" do
    assert {:ok, %{html: "<p>Hello</p>", options: [margin: 10]}} =
             HtmlValidator.validate_render_request("<p>Hello</p>", margin: 10)

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlValidator.validate_render_request("<p>Hello</p>", margni: 10)

    assert {:error, {:invalid_encoding, %{stage: :html}}} =
             HtmlValidator.validate_render_request(<<255>>, [])

    assert {:ok, %{input_path: "input.html", output_path: "output.pdf"}} =
             HtmlValidator.validate_paths("input.html", "output.pdf")

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.validate_render_request("<p>Hello</p>", base_url: 123)

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.validate_render_request("<p>Hello</p>", default_font: 123)

    assert Style.load_stylesheets(%{type: :document, children: []}, stylesheets: [123]) ==
             {:error, :invalid_stylesheet_options}
  end

  test "layout pagination and furniture inputs share normalized page geometry" do
    styled_tree = %{type: :document, children: []}

    assert {:ok, %{page_size: {100.0, 80.0}, margins: margins}} =
             HtmlValidator.prepare_layout(styled_tree, page_size: {100, 80}, margin: "5pt 10pt")

    assert margins == %{top: 5.0, right: 10.0, bottom: 5.0, left: 10.0}

    assert HtmlValidator.prepare_layout(styled_tree, [:not_options]) ==
             {:error, :invalid_layout}

    layout_tree = %{
      type: :layout,
      page_size: {100.0, 80.0},
      margins: margins,
      boxes: []
    }

    assert {:ok, %{margins: ^margins}} = HtmlValidator.prepare_pagination(layout_tree, [])

    pages = [%{size: {100.0, 80.0}, boxes: []}]

    assert {:ok, %{furniture: %{header: %{default: "Page {{page}}"}, footer: %{}}}} =
             HtmlValidator.prepare_furniture(
               pages,
               layout_tree,
               page_furniture: [header: "Page {{page}}"]
             )

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlValidator.prepare_furniture([], %{}, [:not_options])

    assert {:error, {:invalid_layout, %{stage: :layout}}} =
             HtmlValidator.prepare_furniture(:not_pages, :not_layout, :not_options)
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
            ]} =
             HtmlValidator.prepare_font_configs([
               [
                 family: "Example Sans",
                 path: "/tmp/example.ttf",
                 weight: "bold",
                 style: "italic"
               ]
             ])

    assert :error = HtmlValidator.prepare_font_configs([[family: "Missing path"]])
    assert :error = HtmlValidator.prepare_font_configs([[family: "Empty", path: ""]])
  end
end
