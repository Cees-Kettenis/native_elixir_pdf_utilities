defmodule NativeElixirPdfUtilities.HtmlToPdf.TemporalContractTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, PdfWriter, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlTemporalValidator

  test "temporal display diagnoses unsupported kinds and invalid font geometry at its authority" do
    {node, _} = prepared_date()
    control = node.style._form_control
    unsupported = put_in(control.initial_state, [:temporal, :kind], "datetime")

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlTemporalValidator.prepare_display(
               node.style,
               unsupported,
               %{"type" => "datetime"},
               :static
             )

    assert diagnostic.stage == :style and diagnostic.operation == :compute
    assert diagnostic.module == HtmlTemporalValidator and diagnostic.source == "datetime"
    assert diagnostic.message =~ "display:none"

    for {style, mode} <- [
          {%{node.style | font_size: -1}, :static},
          {%{node.style | letter_spacing: :unbounded}, :static},
          {node.style, :unsupported}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               HtmlTemporalValidator.prepare_display(
                 style,
                 control.initial_state,
                 control.attributes,
                 mode
               )

      assert diagnostic.stage == :style and diagnostic.operation == :compute
      assert diagnostic.module == HtmlTemporalValidator and diagnostic.source == "date"
      assert diagnostic.message =~ "bounded font geometry"
    end
  end

  test "layout propagates invalid display geometry with its actionable validator diagnostic" do
    {node, _} = prepared_date()
    invalid = %{node | style: %{node.style | font_size: -1}}

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlTemporalValidator.validate_initial_layout(invalid)

    assert diagnostic.stage == :style and diagnostic.operation == :compute
    assert diagnostic.source == "date" and diagnostic.module == HtmlTemporalValidator
    assert diagnostic.message =~ "bounded font geometry"
  end

  test "built-in font estimates cannot authorize native temporal geometry" do
    {node, _} = prepared_date()
    control = node.style._form_control

    for name <- ["Helvetica", "Courier"] do
      face = %{type: :built_in, family: name, pdf_name: name}
      style = %{node.style | font_face: face}

      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlTemporalValidator.prepare_display(
                 style,
                 control.initial_state,
                 control.attributes,
                 :static
               )

      assert diagnostic.stage == :style and diagnostic.operation == :compute
      assert diagnostic.source == "date" and diagnostic.module == HtmlTemporalValidator

      assert diagnostic.message =~ "physical font metrics" and
               diagnostic.message =~ "embedded font"

      assert {:error, {:unsupported_html, ^diagnostic}} =
               HtmlTemporalValidator.validate_initial_layout(%{node | style: style})

      box = %{
        type: :text,
        text: "Ordinary ASCII",
        x: 10,
        y: 20,
        font: name,
        font_face: face,
        font_size: 10,
        color: {0, 0, 0}
      }

      assert {:ok, pdf} = PdfWriter.render([%{size: {200, 100}, boxes: [box]}])
      assert {:ok, "Ordinary ASCII"} = Text.extract(pdf, layout: false)
    end
  end

  test "rounded temporal paint clips uniform contours and admits an omitted side" do
    base =
      "<input type=date readonly value=2026-10-13 style='display:block;border-radius:3pt;border:1pt inset red'>"

    assert {:ok, dom} = HtmlParser.parse_detailed(base)

    assert {:ok, styled} =
             Style.compute_detailed(dom, forms: :static, system_font_discovery: false)

    assert {:ok, layout} = Layout.layout(styled)
    host = Enum.find(layout.boxes, &(Map.get(&1, :role) == :temporal_host))
    assert host.border_styles.top == :inset and host.border_styles.left == :inset

    assert [%{border_radius: 3.0}, %{border_radius: 2.25}] =
             NativeElixirPdfUtilities.HtmlToPdf.PageGeometry.rounded_border_contours(host, 841.89)

    assert {:ok, pages} = Pagination.paginate(layout)
    assert {:ok, pdf} = PdfWriter.render(pages, forms: :static, compress_streams: false)
    assert pdf =~ "W* n" and pdf =~ "1 0 0 RG"
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "2026-10-13"

    html =
      String.replace(base, "border:1pt inset red", "border:1pt inset red;border-top-style:none")

    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "2026-10-13"
  end

  test "table relief paint retains very-dark contrast, near-white light edges and ordinary colors" do
    for {color, expected} <- [
          {"#010203", ["0.1137 0.2275 0.3412", "0.2235 0.4471 0.6706"]},
          {"#fafafa", ["0.9804 0.9804 0.9804", "0.651 0.651 0.651"]},
          {"#808080", ["0.1725 0.1725 0.1725", "0.8314 0.8314 0.8314"]}
        ] do
      html =
        "<table style='border-collapse:separate;border-spacing:0'><tr><td style='border:3pt inset #{color};padding:3pt'>Relief</td></tr></table>"

      assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static, compress_streams: false)
      for rgb <- expected, do: assert(pdf =~ rgb)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text =~ "Relief"
    end
  end

  defp prepared_date do
    assert {:ok, dom} = HtmlParser.parse_detailed("<input type=date readonly value=2026-10-13>")

    assert {:ok, %{children: [node]} = styled} =
             Style.compute_detailed(dom, forms: :static, system_font_discovery: false)

    {node, styled}
  end
end
