defmodule NativeElixirPdfUtilities.HtmlToPdf.MeasurementBudgetTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Layout,
    Pagination,
    ResourceContext,
    Style
  }

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator}

  test "measurement reports exhausted paint budgets locally and preserves subsequent document work" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert :checked_inside_issued_document =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
               assert {:ok, dom} =
                        HtmlParser.parse_detailed("<span>Measured <b>content</b></span>")

               assert {:ok, %{children: [node]}} =
                        Style.compute_detailed(dom, [], context.image_budget)

               before = ResourceContext.current()
               Limits.install(Map.put(limits, :max_layout_boxes, 1))

               assert {:error, {:resource_limit_exceeded, diagnostic}} =
                        Layout.measure_child(node, context)

               assert diagnostic.stage == :layout
               assert diagnostic.reason == :resource_limit_exceeded
               assert diagnostic.message =~ "max_layout_boxes"
               assert ResourceContext.current() == before

               Limits.install(limits)
               assert {:ok, child} = Layout.measure_child(node, context)
               assert Enum.map_join(child.boxes, &Map.get(&1, :text, "")) == "Measured content"
               assert ResourceContext.current() == before
               :checked_inside_issued_document
             end)

    assert ResourceContext.current() == nil
  end

  test "native inline and table measurements retain exhausted font-work diagnostics" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    for html <- [
          "<math><mi>x</mi></math>",
          "<ruby>Base<rt>Reading</rt></ruby>",
          "<span style='display:inline-block'><ruby>Base<rt>Reading</rt></ruby></span>",
          "<table><tr><td style='vertical-align:baseline'><ruby>Base<rt>Reading</rt></ruby></td><td>Peer</td></tr></table>"
        ] do
      Limits.install(limits)

      ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
        assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

        assert {:ok, styled} =
                 Style.compute_detailed(dom, [system_font_discovery: false], context.image_budget)

        # Retain the work actually spent preparing this source. The short
        # remaining allowance fails while measuring its real native artwork.
        used = Map.get(Process.get({FontValidator, :budget}), :max_font_math_work, 0)
        Limits.install(%{limits | max_font_math_work: used + 7})

        assert {:error, {:resource_limit_exceeded, diagnostic}} = Layout.layout(styled)
        assert diagnostic.stage == :font
        assert diagnostic.module == FontValidator
        assert diagnostic.message =~ "max_font_math_work"
      end)
    end
  end

  test "pagination retains the measured Math font-work diagnostic" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<math><mi>x</mi></math>")
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled)
    Limits.install(%{limits | max_font_math_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} = Pagination.paginate(layout)
    assert diagnostic.stage == :font
    assert diagnostic.module == FontValidator
    assert diagnostic.operation == :measure_math
    assert diagnostic.message =~ "max_font_math_work"
  end

  test "middle-aligned color controls retain the parent font metric diagnostic" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<p style='font-family:DejaVu Sans'><input type=color style='vertical-align:middle'></p>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    Limits.install(%{limits | max_font_math_work: 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Layout.layout(styled)
    assert diagnostic.stage == :font
    assert diagnostic.operation == :glyph_bounds
    assert diagnostic.source == "DejaVu Sans"
    assert diagnostic.message =~ "max_font_math_work"
  end

  test "spanning and positioned controls preserve field-count and appearance-work diagnostics" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    for fixture <- ~w(html_table_spanning_controls html_positioned_widget_fragment_source),
        key <- [:max_pdf_form_fields, :max_appearance_text_bytes] do
      html =
        File.read!("test/fixtures/html_to_pdf/browser_parity/#{fixture}.html")
        |> String.replace("<input ", "<input name='budget-extra' value='Second'><input ",
          global: false
        )

      Limits.install(Map.put(limits, key, 1))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ Atom.to_string(key)
      assert diagnostic.stage in [:forms, :layout]
    end
  end

  test "spanning cell paint retains out-of-range source geometry diagnostics" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_pdf_numeric_magnitude: 1000})

    html =
      "<style>@page{size:100pt 100pt;margin:0}html,body{margin:0}table{margin-top:-1000pt}</style>" <>
        "<table><tr><td rowspan=2>Spanning<br>Text</td><td>One</td></tr><tr><td>Two</td></tr></table>"

    assert {:error, {:invalid_layout, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.stage == :layout
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlTableValidator
    assert diagnostic.operation == :prepare_cell_band_paint
    assert diagnostic.message =~ "bounded"
  end

  test "an oversized cell outside the PDF geometry bound retains its pagination diagnostic" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_pdf_numeric_magnitude: 1000})

    html =
      "<style>@page{size:100pt 100pt;margin:10pt}html,body{margin:0;font-family:DejaVu Sans;font-size:10pt;line-height:15pt}td{height:15pt}</style>" <>
        "<table><tr><td><span style='display:inline-block;width:20pt;height:1050pt;background:blue'></span></td></tr></table>"

    assert {:error, {:invalid_layout, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.stage == :pagination
    assert diagnostic.operation == :prepare_row_page_fragment
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlTableValidator
    assert diagnostic.message =~ "bounded row edges"
  end
end
