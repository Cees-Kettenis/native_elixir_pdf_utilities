defmodule NativeElixirPdfUtilities.HtmlToPdf.IntegrationWave9Test do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    CssParser,
    DrawingGeometry,
    HtmlParser,
    PdfWriter,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, PrintMediaValidator, WriterValidator}

  test "source records use the same nested viewport media selection as ordinary stylesheets" do
    css =
      "@media print and (min-width:100px){p{color:red}@media (max-width:200px){p{color:blue}}}@media screen{p{color:green}}"

    assert {:ok, context} = PrintMediaValidator.client_context([], {225, 150})
    assert {:ok, source} = CssParser.prepare_source_stylesheet(css, nil, context)
    assert {:ok, ordinary} = CssParser.prepare_stylesheet(css, context)

    assert Enum.map(source.rules, &Map.take(&1, [:selectors, :declarations, :order])) ==
             ordinary.rules

    assert Enum.map(source.authored_rules, & &1.active) == [true, false, false]
    assert Enum.map(source.authored_media, & &1.active) == [true, false, false]
    assert source.media_provider == PrintMediaValidator

    for record <- source.authored_media do
      assert binary_part(css, record.source.offset, record.source.length) == record.source.text
    end

    assert {:ok, small} = PrintMediaValidator.client_context([], {75, 150})
    assert {:ok, selected} = CssParser.prepare_source_stylesheet(css, nil, small)
    assert Enum.map(selected.authored_rules, & &1.active) == [true, true, false]

    assert {:ok, dormant} =
             CssParser.prepare_source_stylesheet(
               "@media screen{@media print{p{color:red}}}",
               nil,
               context
             )

    assert dormant.rules == []

    assert {:error, {:invalid_css, _}} =
             CssParser.prepare_source_stylesheet("@media print {p{color:red}", nil, context)
  end

  test "select source contexts keep option identities and deferred font metrics without rendering" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<select multiple size=3><optgroup label='Group'><option selected>A</option><option>B</option></optgroup></select>"
             )

    token = make_ref()

    assert {:ok, result} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn node,
                                                                                          pseudo,
                                                                                          context ->
               send(self(), {token, node, pseudo, context})
               :ok
             end)

    assert :font_metrics in result.runtime_obligations
    assert_received {^token, %{tag: "select"}, nil, %{status: :resolved, style: select}}
    assert select.form_kind == :listbox
    assert select._select_state.multiple
    assert select.font_face == nil

    assert_received {^token, %{tag: "option", _selector_id: id}, nil,
                     %{status: :resolved, style: option}}

    assert option._initial_option_id == id
    assert option.font_face == nil
  end

  test "nested group paint and links share locally snapped outer and inner clips" do
    text = %{
      type: :text,
      text: "A",
      x: 3.1,
      y: 4.2,
      width: 20,
      line_height: 12,
      font: "Helvetica",
      font_size: 10,
      color: {0, 0, 0},
      snap_to_css_pixel_grid: true,
      link_url: "https://example.com/text",
      clip: %{
        x: 1.2,
        y: 3.4,
        width: 12.2,
        height: 11.2,
        border_radius: 3,
        inner_clip: %{x: 2.3, y: 4.2, width: 8.9, height: 6.4, border_radius: 2}
      }
    }

    group = %{
      type: :drawing_group,
      width: 60,
      height: 60,
      transform: {2, 0, 0, 3, 4, 5},
      boxes: [%{type: :glyph_group, actual_text: "A", boxes: [text]}]
    }

    pages = [%{size: {200, 200}, boxes: [group]}]
    assert {:ok, prepared} = WriterValidator.prepare(pages, [])
    painted = Enum.find(hd(prepared.pages).boxes, &(&1.type == :text))

    assert painted.clip.inner_clip == %{
             x: 2.25,
             y: 4.5,
             width: 9.0,
             height: 6.0,
             border_radius: 2
           }

    assert painted.drawing_annotation_rect == {10.2, 18.5003, 26.5, 36.5}
    assert {:ok, pdf} = PdfWriter.render(pages)
    assert length(Regex.scan(~r/W n/, pdf)) == 2

    shifted = DrawingGeometry.translate_box(text, 7, -3)
    assert shifted.clip.x == text.clip.x + 7
    assert shifted.clip.inner_clip.x == text.clip.inner_clip.x + 7
    assert shifted.clip.inner_clip.y == text.clip.inner_clip.y - 3

    zero = %{group | boxes: [put_in(text.clip.inner_clip.width, 0)]}
    assert {:ok, prepared} = WriterValidator.prepare([%{size: {200, 200}, boxes: [zero]}], [])
    painted = Enum.find(hd(prepared.pages).boxes, &(&1.type == :text))
    refute Map.has_key?(painted, :link_url)

    assert {:ok, rounded_pdf} =
             PdfWriter.render([%{size: {200, 200}, boxes: [Map.put(group, :clip, text.clip)]}])

    assert rounded_pdf =~ "/Annots"

    corrupt = put_in(text.clip.inner_clip.x, 1_000_000_000)

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfWriter.render([%{size: {200, 200}, boxes: [%{group | boxes: [corrupt]}]}])

    assert diagnostic.stage == :pdf
  end

  test "image border-box links preserve fit letterboxing but obey separate rounded content clips" do
    image = %{
      format: :png,
      data: <<255, 0, 0>>,
      width_px: 1,
      height_px: 1,
      color_space: :device_rgb,
      bits_per_component: 8
    }

    box = %{
      type: :image,
      x: 5,
      y: 5,
      width: 10,
      height: 10,
      image: image,
      link_url: "https://example.com/image",
      link_rect: %{x: 0, y: 0, width: 30, height: 30},
      clip: %{x: 5, y: 5, width: 10, height: 10}
    }

    assert {:ok, ordinary} = PdfWriter.render([%{size: {40, 40}, boxes: [box]}])
    assert ordinary =~ "/Rect [0 0 30 30]"

    clip = %{
      x: 0,
      y: 0,
      width: 30,
      height: 30,
      border_radius: 2,
      inner_clip: %{x: 2, y: 3, width: 6, height: 7, border_radius: 1}
    }

    clipped = %{box | clip: clip}
    assert {:ok, pdf} = PdfWriter.render([%{size: {40, 40}, boxes: [clipped]}])
    assert pdf =~ "/Rect [2 3 8 10]"

    group = %{
      type: :drawing_group,
      width: 40,
      height: 40,
      transform: {2, 0, 0, 3, 4, 5},
      boxes: [clipped]
    }

    assert {:ok, prepared} = WriterValidator.prepare([%{size: {200, 200}, boxes: [group]}], [])
    painted = Enum.find(hd(prepared.pages).boxes, &(&1.type == :image))
    assert painted.drawing_annotation_rect == {8.0, 14.0, 20.0, 35.0}
    empty = put_in(clipped.clip.inner_clip.height, 0)
    assert {:ok, pdf} = PdfWriter.render([%{size: {40, 40}, boxes: [empty]}])
    refute pdf =~ "/Subtype /Link"
  end

  test "ordinary font descriptor errors use the validator's exact superseded declaration index" do
    css = "@font-face{font-family:X;src:url(x.ttf);font-style:normal;font-style:oblique}"

    for prepare <- [&CssParser.prepare_stylesheet/1, &CssParser.prepare_source_stylesheet/1] do
      assert {:error, {:invalid_css, diagnostic}} = prepare.(css)
      assert diagnostic.source == "font-style:oblique"
      assert diagnostic.stage == :css
    end
  end

  test "iframe scrollbar callers use shared geometry and preserve checked failures" do
    alias NativeElixirPdfUtilities.Validators.EmbeddedViewportValidator
    {cases, _} = Code.eval_file("test/fixtures/html_to_pdf/e7_scrollbar_parts.terms")

    for {geometry, expected} <- cases do
      assert {:ok, ^expected} =
               EmbeddedViewportValidator.prepare_scrollbar_parts(
                 Map.put(geometry, :seen, MapSet.new())
               )
    end

    assert {:error, {:invalid_layout, diagnostic}} =
             EmbeddedViewportValidator.prepare_scrollbar_parts(%{vertical: :invalid})

    assert diagnostic.operation == :prepare_scrollbar
  end

  test "number source context defers font metrics and preserves prepared constraints" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<fieldset disabled><legend>Legend</legend><input type=number value=2.5 min=1 max=5></fieldset>"
             )

    token = make_ref()

    assert {:ok, _} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn node,
                                                                                          pseudo,
                                                                                          context ->
               send(self(), {token, node, pseudo, context})
               :ok
             end)

    assert_received {^token, %{tag: "input", _initial_form_state: state}, nil,
                     %{status: :resolved, style: style}}

    assert state.disabled
    assert state.value == "2.5"
    assert state.number.min == 1.0 and state.number.max == 5.0
    assert style.font_face == nil
    assert style._number_state.value == 2.5
  end

  test "bound CSS records retain inactive sheets without applying them to the original cascade" do
    html =
      "<style media='screen'>p{color:red}</style><style>p{color:blue}</style><p id=x style='font-size:2&#48;pt'>Text</p>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    token = make_ref()

    assert {:ok, result} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn node,
                                                                                          pseudo,
                                                                                          context ->
               send(self(), {token, node, pseudo, context})
               :ok
             end)

    assert_received {^token, %{tag: "p", node_id: node_id}, nil,
                     %{status: :resolved, style: style}}

    assert style.color == {0.0, 0.0, 1.0}
    assert style.font_size == 20.0 and style.font_face == nil
    [inactive, active] = result.source_obligations.authored
    refute inactive.stylesheet_applicable
    assert active.stylesheet_applicable
    [record] = result.source_obligations.inline[node_id]
    assert record.source.provenance == :original_html
    assert record.source.text == "font-size:2&#48;pt"
    assert binary_part(html, record.source.offset, record.source.length) == record.source.text
  end

  test "non-CSS style bodies remain inert in source policy as in document stylesheet selection" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<style type='text/plain'>not CSS { broken text</style><p>Visible</p>"
             )

    assert {:ok, result} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn _,
                                                                                          _,
                                                                                          _ ->
               :ok
             end)

    assert result.source_obligations.stylesheets == []
  end

  test "source stylesheet selection preserves SVG owners with namespaced attributes" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<svg><style xml:lang='en'>p{color:blue}</style></svg><p>Visible</p>"
             )

    token = make_ref()

    assert {:ok, result} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn node,
                                                                                          pseudo,
                                                                                          context ->
               send(self(), {token, node, pseudo, context})
               :ok
             end)

    assert_received {^token, %{tag: "p"}, nil, %{status: :resolved, style: style}}
    assert style.color == {0.0, 0.0, 1.0}
    [sheet] = result.source_obligations.authored
    assert sheet.stylesheet_applicable
  end

  test "valueless inline style keeps an empty original source without a byte-offset exception" do
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p style>Visible</p>")

    assert {:ok, result} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn _,
                                                                                          _,
                                                                                          _ ->
               :ok
             end)

    assert Enum.all?(result.source_obligations.inline, fn {_id, records} -> records == [] end)
  end
end
