defmodule NativeElixirPdfUtilities.HtmlToPdf.BrowserParityTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity

  @fixtures_dir Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)
  @real_fixtures_dir Path.expand("../fixtures/html_to_pdf", __DIR__)

  @furniture_image "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMjAiIGhlaWdodD0iNDAiPjxyZWN0IHdpZHRoPSIxMjAiIGhlaWdodD0iNDAiIGZpbGw9IiMxZDRlZDgiLz48L3N2Zz4="
  @page_furniture_header """
  <div style="height: 24pt; display: flex; align-items: center; gap: 6pt; color: #1f4b7a; font-family: 'DejaVu Sans'; font-size: 8pt; line-height: 12pt; background-image: url('#{@furniture_image}'); background-repeat: no-repeat; background-position: right center; background-size: 12pt 12pt">
    <img src="#{@furniture_image}" style="display: block; width: 18pt; height: 14pt; object-fit: cover">
    <span>Quarterly report</span>
  </div>
  """
  @page_furniture_footer """
  <div style="height: 14pt; display: flex; align-items: center; justify-content: flex-end; gap: 6pt; color: #52606d; font-family: 'DejaVu Sans'; font-size: 8pt; line-height: 10pt">
    <span>Page {{page}} of {{pages}}</span>
    <img src="#{@furniture_image}" style="display: block; width: 18pt; height: 10pt; object-fit: contain">
  </div>
  """

  test "browser comparisons disable JavaScript in each fresh profile" do
    fixture = Path.expand("tmp/browser_parity/script_mode_reference.html")
    File.mkdir_p!(Path.dirname(fixture))

    File.write!(fixture, """
    <!doctype html><html><head><style>
    @page {size:240pt 120pt;margin:0;}
    html,body {margin:0;padding:0;font-family:"DejaVu Sans";font-size:10pt;line-height:13pt;}
    p {margin:0;}
    </style></head><body><p>Before</p><noscript><p>Fallback</p></noscript><p>After</p>
    <script>document.body.innerHTML='<p>Script ran</p>';</script>
    </body></html>
    """)

    stats =
      PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/script_mode_reference",
        native_renderer: fn _html, opts ->
          NativeElixirPdfUtilities.HtmlToPdf.render(
            "<body style='margin:0;padding:0;font-family:DejaVu Sans;font-size:10pt;line-height:13pt'><p style='margin:0'>Before</p><p style='margin:0'>Fallback</p><p style='margin:0'>After</p></body>",
            opts
          )
        end,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
    assert stats.scripting == :disabled

    assert {:ok, text} =
             NativeElixirPdfUtilities.Text.extract(
               File.read!(Path.join(stats.artifact_dir, "chromium.pdf"))
             )

    assert text =~ "Fallback"
    refute text =~ "Script ran"
  end

  @tag math_tokens: true
  test "Core reference spacing follows transitions, explicit sizes and nested row ends" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{Font, MathLayout}
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)
    {:ok, x} = MathLayout.glyph("x", font, 12, text_transform: :math_auto)
    {:ok, y} = MathLayout.glyph("y", font, 20, text_transform: :math_auto)

    html =
      ~s(<math><mi data-core-reference="x">x</mi><mi data-core-reference="y" style="font-size:20pt">y</mi><mn>1</mn><mrow data-core-reference="row"><mi data-core-reference="end">x</mi></mrow><mn>2</mn><mi data-core-reference="space"> x </mi><mn>3</mn></math>)

    adjusted = math_token_italic_reference(html)
    assert adjusted =~ ~s(data-core-reference="x" style='margin-right:0.0pt')
    assert adjusted =~ ~s(style="font-size:20pt;margin-right:#{y.italic_correction}pt")
    assert adjusted =~ ~s(data-core-reference="row" style='margin-right:#{x.italic_correction}pt')
    assert adjusted =~ ~s(data-core-reference="end" style='margin-right:#{x.italic_correction}pt')
    assert adjusted =~ ~s(data-core-reference="space" style='margin-right:0.0pt')
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert {:ok, "xy1x2x3"} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
  end

  @fixture_thresholds [
    {"html_child_fragment_destinations.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_table_caption_blocks.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_headers.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_iframe_math_gauges.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_processing_instructions.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_formation.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_legacy_raw_text.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_metadata_preservation.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_iframe_viewports.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_iframe_local.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_iframe_page_flow.html",
     render_opts: [forms: :static],
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"drawing_group_viewports.html",
     native_renderer: &__MODULE__.render_drawing_group_fixture/2,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_mixed_link_button_content.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_gauges.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_gauge_boxes.html",
     chromium_transform: &PdfVisualCompare.gauge_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_initial_text_controls.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_fieldset_fragmentation.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_fieldsets.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_editability.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_responsive_images.html",
     chromium_transform: &__MODULE__.responsive_print_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_inline_images.html",
     chromium_transform: &__MODULE__.responsive_print_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"mathml_operators.html",
     chromium_transform: &__MODULE__.math_operator_core_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"mathml_operators_ordinary.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_token_defaults.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_tokens_rows.html",
     chromium_transform: &__MODULE__.math_token_italic_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_disclosures.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_horizontal_rules.html",
     render_opts: [forms: :static],
     chromium_transform: &PdfVisualCompare.horizontal_rule_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"css_named_colors.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_popover_initial.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_option_semantics.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_selection_state.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_form_ownership.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_language_selectors.html",
     native_renderer: &__MODULE__.render_language_selector_fixture/2,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"css_inline_block_baselines.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_heading_offsets.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_soft_breaks.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_inline_word_boundaries.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_anchor_defaults.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_anonymous_inline_blocks.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"css_identifier_escapes.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_custom_containers.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_hidden_attribute.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_inert_attribute.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_semantic_inline.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_noscript_initial.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_script_supporting_parents.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_inert_templates.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_positional_selectors.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_script_data_blocks.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_password_inputs.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_image_dimensions.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_control_state_selectors.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_input_buttons.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_readonly_controls.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_contact_inputs.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_multiple_email.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_input_type_defaults.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_attribute_selectors.html",
     native_renderer: &__MODULE__.render_attribute_selector_fixture/2,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_search_inputs.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_input_values.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_hidden_inputs.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"math_stretch_glyphs.html",
     native_renderer: &__MODULE__.render_math_stretch_fixture/2,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"math_glyph_rows.html",
     native_renderer: &__MODULE__.render_math_row_fixture/2,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"font_glyph_variants.html",
     native_renderer: &__MODULE__.render_glyph_fixture/2,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_text_decorations.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"font_supplementary_unicode.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_list_attributes.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_optional_tags.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_static_elements.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_comments_attributes.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"flex_explicit_cross_sizes.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"leading_decimal_lengths.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"whitespace_nowrap.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"body_padding_background.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"body_grid.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"ligature_letter_spacing.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"table_outer_border_pagination.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"png_formats.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"hidden_tables.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"cmyk_jpeg_colors.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"png_transparent_color.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"quoted_at_rules.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"computed_custom_properties.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"quoted_variables.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"grid_sparse_placement.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"grid_span_end.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"grid_explicit_sizes.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"empty_flex_height.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"zero_font_size.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"weight_inheritance.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"distributed_gaps.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"block_box_model.html", max_changed_ratio: 0.01, max_average_delta: 0.02},
    {"border_style_variants.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"box_sizing_and_margins.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"css_cascade_selectors.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"generated_content_counters.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"css_remaining_supported_values.html", max_changed_ratio: 0.01, max_average_delta: 0.04},
    {"display_lists_and_inline_block.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"fonts_and_print_media.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"html_semantics_typography.html", max_changed_ratio: 0.01, max_average_delta: 0.04},
    {"images_data_uris.html", max_changed_ratio: 0.01, max_average_delta: 0.045},
    {"image_object_fitting.html", max_changed_ratio: 0.01, max_average_delta: 0.045},
    {"background_images.html", max_changed_ratio: 0.01, max_average_delta: 0.05},
    {"absolute_positioning.html", max_changed_ratio: 0.01, max_average_delta: 0.007},
    {"inline_positioning.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"inline_text_flow.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"html_fragment_destinations.html", max_changed_ratio: 0.01, max_average_delta: 0.04},
    {"html_base_relative_links.html", max_changed_ratio: 0.01, max_average_delta: 0.04},
    {"links_entities_and_protocols.html", max_changed_ratio: 0.01, max_average_delta: 0.04},
    {"static_form_controls.html", max_changed_ratio: 0.01, max_average_delta: 0.05},
    {"system_font_inheritance.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"text_style_variants.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"unsupported_glyph_replacement.html", max_changed_ratio: 0.01, max_average_delta: 0.02},
    {"whitespace_pre_line.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"units_and_sizing.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"break_variants.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"flex_grid_alignment.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"flex_direction_and_justification.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"grid_tracks_and_placement.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"layout_compositions_remaining.html", max_changed_ratio: 0.01, max_average_delta: 0.045},
    {"table_column_layout.html", max_changed_ratio: 0.01, max_average_delta: 0.04},
    {"table_collapsed_borders.html", max_changed_ratio: 0.01, max_average_delta: 0.025},
    {"table_header_near_page_row.html",
     render_opts: [page_size: {240, 180}, margin: 0],
     chromium_page_size: {240, 180},
     max_changed_ratio: 0.01,
     max_average_delta: 0.04},
    {"table_pagination_headers.html", max_changed_ratio: 0.01, max_average_delta: 0.04},
    {"table_rowspan_tfoot.html", max_changed_ratio: 0.01, max_average_delta: 0.04},
    {"table_separate_borders.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"nested_table_grid_flex.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"nested_table_collapsed_borders.html", max_changed_ratio: 0.01, max_average_delta: 0.035},
    {"page_geometry_asymmetric.html", max_changed_ratio: 0.01, max_average_delta: 0.025},
    {"page_rules_landscape.html",
     render_opts: [margin: 0], max_changed_ratio: 0.01, max_average_delta: 0.02},
    {"root_absolute_pagination.html", max_changed_ratio: 0.01, max_average_delta: 0.03},
    {"page_furniture.html",
     render_opts: [
       page_furniture: [
         header: @page_furniture_header,
         footer: @page_furniture_footer
       ]
     ],
     chromium_page_furniture: [
       header: @page_furniture_header
     ],
     chromium_page_margin: 36,
     chromium_page_margin_css: """
     @page {
       @bottom-right {
         content: "Page " counter(page) " of " counter(pages);
         color: #52606d;
         font-family: "DejaVu Sans";
         font-size: 8pt;
         vertical-align: top;
       }
     }
     """,
     max_changed_ratio: 0.01,
     max_average_delta: 0.03},
    {"paragraph_pagination.html",
     render_opts: [page_size: {240, 180}, margin: 0],
     chromium_page_size: {240, 180},
     max_changed_ratio: 0.01,
     max_average_delta: 0.035},
    {"pagination_breaks.html", max_changed_ratio: 0.01, max_average_delta: 0.03}
  ]

  @real_document_fixture_thresholds [
    {"stock_sticker_compact.html",
     render_opts: [page_size: {5 / 2.54, 3 / 2.54}, margin: 0],
     chromium_page_size: {5 / 2.54, 3 / 2.54},
     max_changed_ratio: 0.01,
     max_average_delta: 0.05},
    {"government_application_form.html",
     render_opts: [page_size: :a4],
     chromium_page_size: :a4,
     max_changed_ratio: 0.01,
     max_average_delta: 0.05},
    {"purchase_order.html",
     render_opts: [page_size: :a4],
     chromium_page_size: :a4,
     max_changed_ratio: 0.01,
     max_average_delta: 0.05},
    {"material_requisition.html",
     render_opts: [page_size: :a4],
     chromium_page_size: :a4,
     max_changed_ratio: 0.01,
     max_average_delta: 0.05},
    {"invoice_012.html",
     render_opts: [page_size: :a4],
     chromium_page_size: :a4,
     max_changed_ratio: 0.01,
     max_average_delta: 0.05},
    {"statement_012.html",
     render_opts: [page_size: :a4],
     chromium_page_size: :a4,
     max_changed_ratio: 0.01,
     max_average_delta: 0.05},
    {"multi_page_report_012.html",
     render_opts: [page_size: :a4],
     chromium_page_size: :a4,
     max_changed_ratio: 0.01,
     max_average_delta: 0.05},
    {"stock_sticker.html",
     render_opts: [page_size: {4.92126, 1.49606}, margin: 0],
     chromium_page_size: {4.92126, 1.49606},
     max_changed_ratio: 0.01,
     max_average_delta: 0.05},
    {"trim_card.html",
     render_opts: [page_size: {11.6929, 8.2677}, margin: 0],
     chromium_page_size: {11.6929, 8.2677},
     max_changed_ratio: 0.01,
     max_average_delta: 0.05}
  ]

  @doc "Prepares responsive sources for an initial print environment in Chromium's screen-first headless pipeline."
  @spec responsive_print_reference(String.t()) :: String.t()
  def responsive_print_reference(html) do
    # HTML 4.8.4.3.13 permits a UA to retain its earlier screen selection when
    # entering print. Native rendering selects directly in print. Freeze only
    # environment inputs and codec support. Chromium also supports WebP, while
    # the native static codec set is PNG/JPEG/SVG. Retain candidate choice and sizing.
    html
    |> String.replace("type=\"image/webp\"", "type=\"image/x-unsupported\"")
    |> String.replace("media=\"screen\"", "media=\"not all\"")
    |> String.replace("media=\"print and (min-width:600px)\"", "media=\"all\"")
    |> String.replace("media=\"print\"", "media=\"all\"")
    |> String.replace("sizes=\"(min-width:600px) calc(50vw / 5), 100px\"", "sizes=\"64px\"")
  end

  @doc "Replaces explicitly marked tall operators with independently generated Core artwork."
  @spec math_operator_core_reference(String.t()) :: String.t()
  def math_operator_core_reference(html) do
    manifest =
      :json.decode(
        File.read!(Path.join(@fixtures_dir, "assets/math-operator-core-reference.json"))
      )

    Regex.replace(~r/<mo data-core-stretch="([^"]+)"([^>]*)>([^<]*)<\/mo>/u, html, fn _,
                                                                                      key,
                                                                                      _attributes,
                                                                                      text ->
      record = Map.fetch!(manifest["records"], key)
      true = record["text"] == text
      record["html"]
    end)
  end

  @doc "Adds the selected font's Core italic spacing omitted by Chromium's math-auto tokens."
  @spec math_token_italic_reference(String.t()) :: String.t()
  def math_token_italic_reference(html) do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

    {:ok, dom} = NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_document_detailed(html)
    {_italic, corrections} = math_reference_corrections(dom, font, 12.0)

    Regex.replace(
      ~r/<(mi|mn|mtext|ms|mrow)([^>]*data-core-reference="([^"]+)"[^>]*)>/u,
      html,
      fn _, tag, attributes, identity ->
        correction = Map.fetch!(corrections, identity)

        attributes =
          if Regex.match?(~r/\bstyle=["']/, attributes) do
            Regex.replace(~r/\bstyle=(["'])(.*?)\1/, attributes, fn _, quote, css ->
              "style=#{quote}#{css};margin-right:#{correction}pt#{quote}"
            end)
          else
            "#{attributes} style='margin-right:#{correction}pt'"
          end

        "<#{tag}#{attributes}>"
      end
    )
  end

  defp math_reference_corrections(node, font, inherited_size) do
    attributes = Map.get(node, :attributes, %{})

    size =
      case Regex.run(~r/font-size:\s*([0-9.]+)pt/, Map.get(attributes, "style", "")) do
        [_, value] ->
          {size, ""} = Float.parse(value)
          size

        nil ->
          inherited_size
      end

    children = Map.get(node, :children, [])
    prepared = Enum.map(children, &math_reference_corrections(&1, font, size))

    corrections =
      Enum.reduce(prepared, %{}, fn {_, corrections}, acc -> Map.merge(acc, corrections) end)

    math_children =
      children
      |> Enum.zip(prepared)
      |> Enum.filter(fn {child, _} -> Map.get(child, :namespace) == :mathml end)

    if Map.get(node, :namespace) == :mathml and node.tag in ["math", "mrow"] do
      corrections =
        math_children
        |> Enum.with_index()
        |> Enum.reduce(corrections, fn {{child, {italic, _}}, index}, acc ->
          next = Enum.at(math_children, index + 1)

          contributes =
            case next do
              nil -> true
              {_, {next_italic, _}} -> next_italic == 0
            end

          case Map.get(child.attributes, "data-core-reference") do
            nil -> acc
            identity -> Map.put(acc, identity, if(contributes, do: italic, else: 0.0))
          end
        end)

      italic =
        case List.last(math_children) do
          nil -> 0.0
          {_, {italic, _}} -> italic
        end

      {italic, corrections}
    else
      original = Enum.map_join(children, &Map.get(&1, :text, ""))
      text = String.trim(original)

      italic =
        if Map.get(node, :namespace) == :mathml and node.tag in ~w(mi mn mtext ms) and
             length(String.to_charlist(text)) == 1 do
          auto? =
            case Regex.run(
                   ~r/text-transform:\s*(math-auto|none)/,
                   Map.get(attributes, "style", "")
                 ) do
              [_, value] -> value == "math-auto"
              nil -> node.tag == "mi" and Map.get(attributes, "mathvariant") != "normal"
            end

          transform =
            if auto? and length(String.codepoints(original)) == 1, do: :math_auto, else: :none

          {:ok, measured} =
            NativeElixirPdfUtilities.HtmlToPdf.MathLayout.glyph(text, font, size,
              text_transform: transform
            )

          measured.italic_correction
        else
          0.0
        end

      {italic, corrections}
    end
  end

  @doc "Runs the public renderer with the project root authorizing the same local font bytes as Chromium."
  @spec render_embedded_fixture(String.t(), keyword()) :: {:ok, binary()} | {:error, term()}
  def render_embedded_fixture(html, opts) do
    NativeElixirPdfUtilities.HtmlToPdf.render(
      local_iframe_reference(html),
      Keyword.put(opts, :base_url, Path.expand("."))
    )
  end

  @doc "Resolves local iframe fixture URLs for Chromium's relocated input file."
  @spec local_iframe_reference(String.t()) :: String.t()
  def local_iframe_reference(html) do
    html =
      html
      |> String.replace("__FONT_ROOT__", Path.expand("priv/fonts/dejavu"))
      |> String.replace("__MATH_FONT__", Path.expand("priv/fonts/stix/STIXTwoMath-Regular.ttf"))
      |> String.replace("__ASSET_ROOT__", Path.join(@fixtures_dir, "assets"))

    Enum.reduce(
      ["iframe_local_child.html", "iframe_local_leaf.html", "iframe_link_child.html"],
      html,
      fn name, html ->
        String.replace(
          html,
          ~s(src="#{name}"),
          ~s(src="#{Path.join([@fixtures_dir, "assets", name])}")
        )
      end
    )
  end

  @doc false
  @spec render_drawing_group_fixture(String.t(), keyword()) :: {:ok, binary()} | {:error, term()}
  def render_drawing_group_fixture(_html, opts) do
    alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter}
    {:ok, registry} = Font.load_registry(opts)
    {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)

    text = %{
      type: :text,
      text: "Ω café",
      x: 8,
      y: 8,
      width: 45,
      line_height: 14,
      font: Font.pdf_name(font),
      font_face: font,
      font_size: 12,
      color: {0, 0, 0},
      link_url: "https://example.com/outer"
    }

    rectangle = %{
      type: :rect,
      x: -10,
      y: -10,
      width: 150,
      height: 100,
      fill_color: {238 / 255, 238 / 255, 238 / 255},
      stroke_color: nil,
      stroke_width: 0,
      border_radius: 0
    }

    image = %{
      type: :image,
      x: 2,
      y: 20,
      width: 80,
      height: 30,
      clip: %{x: 5, y: 25, width: 40, height: 20},
      image: %{
        format: :png,
        data: <<255, 0, 0>>,
        width_px: 1,
        height_px: 1,
        color_space: :device_rgb,
        bits_per_component: 8
      }
    }

    nested_text = %{
      text
      | text: "Nested Ω",
        x: 4,
        width: 60,
        link_url: "https://example.com/inner"
    }

    glyphs = %{type: :glyph_group, actual_text: "Nested Ω", boxes: [nested_text]}

    inner = %{
      type: :drawing_group,
      width: 70,
      height: 30,
      transform: {0.5, 0, 0, 2, 20, 20},
      clip: %{x: 0, y: 0, width: 70, height: 30},
      boxes: [
        %{rectangle | width: 100, height: 50, fill_color: {40 / 255, 120 / 255, 200 / 255}},
        image,
        glyphs,
        Map.delete(%{nested_text | text: "Off viewport", x: 100}, :link_url)
      ]
    }

    outer = %{
      type: :drawing_group,
      width: 120,
      height: 80,
      transform: {1.5, 0, 0, 1.25, 20.25, 40.25},
      clip: %{x: 0, y: 0, width: 120, height: 80},
      boxes: [rectangle, text, inner]
    }

    sibling = Map.delete(%{text | text: "Sibling survives", x: 20.25, y: 20.25}, :link_url)
    PdfWriter.render([%{size: {240, 180}, boxes: [outer, sibling]}], opts)
  end

  @doc "Compares language lists and wildcard subtags against equivalent supported Chromium selectors."
  @spec render_language_selector_fixture(String.t(), keyword()) ::
          {:ok, binary()} | {:error, term()}
  def render_language_selector_fixture(html, opts) do
    NativeElixirPdfUtilities.HtmlToPdf.render(
      html
      |> String.replace(
        "p:lang(fr),p:lang(de) {font-weight:700}",
        "p:lang(fr, de) {font-weight:700}"
      )
      |> String.replace(
        "p:lang(de-DE) {font-style:italic}",
        "p:lang(\"de-*-DE\") {font-style:italic}"
      ),
      opts
    )
  end

  @doc false
  @spec render_attribute_selector_fixture(String.t(), keyword()) ::
          {:ok, binary()} | {:error, term()}
  def render_attribute_selector_fixture(html, opts) do
    # Chromium ignores explicit s flags. Its default data-attribute comparison
    # is exact, so these marked selectors provide equivalent browser artwork.
    html =
      html
      |> String.replace(~s(/* native-sensitive */ [data-exact="ABC"]), ~s([data-exact="ABC" s]))
      |> String.replace(~s(/* native-sensitive */ [data-kind*="bC"]), ~s([data-kind*="bC" s]))
      |> String.replace(
        ~s(/* native-sensitive-uppercase */ [data-kind*="bc"]),
        ~s([data-kind*="bc" S])
      )

    NativeElixirPdfUtilities.HtmlToPdf.render(html, opts)
  end

  @doc false
  @spec render_glyph_fixture(String.t(), keyword()) :: {:ok, binary()} | {:error, term()}
  def render_glyph_fixture(html, opts) do
    alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, PdfWriter}
    {:ok, dom} = HtmlParser.parse_document_detailed(html)
    [%{children: [_, %{tag: "body", children: children}]}] = dom.children
    groups = Enum.filter(children, &(Map.get(&1, :tag) == "span"))
    3 = length(groups)
    {:ok, registry} = Font.load_registry(opts)
    {:ok, _, font} = Font.resolve("STIX Two Math", 400, :normal, registry)
    {:ok, metrics} = Font.math_metrics(font)
    [scripts] = metrics.substitutions["ssty"]
    {:ok, _, paint_font} = Font.resolve("Math Glyph Test Reference", 400, :normal, registry)
    true = paint_font.units_per_em == font.units_per_em
    true = paint_font.ascent == font.ascent

    boxes =
      Enum.map(groups, fn %{attributes: attributes, children: [%{text: reference_text}]} ->
        text = attributes["data-text"]
        true = length(String.to_charlist(reference_text)) == length(String.to_charlist(text))
        level = String.to_integer(attributes["data-level"])

        selected =
          Enum.map(String.to_charlist(text), fn unicode ->
            glyph = Map.fetch!(font.cmap, unicode)
            if level == 0, do: glyph, else: scripts |> Map.fetch!(glyph) |> Enum.at(level - 1)
          end)

        reference_glyphs =
          attributes["data-glyph-ids"] |> String.split() |> Enum.map(&String.to_integer/1)

        ^reference_glyphs = selected
        glyphs = Enum.map(String.to_charlist(reference_text), &Map.fetch!(paint_font.cmap, &1))
        top = 12 + level * 36

        baseline =
          top + Float.round(paint_font.ascent * 30 / paint_font.units_per_em / 0.75) * 0.75

        %{
          type: :text,
          text: text,
          glyph_ids: glyphs,
          x: 12,
          y: 132 - baseline,
          font: Font.pdf_name(paint_font),
          font_face: paint_font,
          font_size: 30,
          snap_to_css_pixel_grid: true,
          color: {0, 0, 0}
        }
      end)

    result = PdfWriter.render([%{size: {240, 132}, boxes: boxes}], opts)
    {:ok, pdf} = result
    {:ok, extracted} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    expected = Enum.map_join(groups, " ", & &1.attributes["data-text"])
    ^expected = extracted
    result
  end

  @doc false
  @spec render_math_row_fixture(String.t(), keyword()) :: {:ok, binary()} | {:error, term()}
  def render_math_row_fixture(html, opts) do
    alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, PdfWriter}
    alias NativeElixirPdfUtilities.Validators.FontValidator
    {:ok, dom} = HtmlParser.parse_document_detailed(html)
    [%{children: [_, %{tag: "body", children: children}]}] = dom.children
    groups = Enum.filter(children, &(Map.get(&1, :tag) == "div"))
    3 = length(groups)
    {:ok, registry} = Font.load_registry(opts)
    {:ok, _, source_font} = Font.resolve("STIX Two Math", 400, :normal, registry)
    {:ok, _, paint_font} = Font.resolve("Math Row Test Reference", 400, :normal, registry)
    true = paint_font.units_per_em == source_font.units_per_em
    true = paint_font.ascent == source_font.ascent

    boxes =
      FontValidator.with_budget(fn ->
        Enum.map(groups, fn %{attributes: attributes, children: tokens} ->
          reference_text = Enum.map_join(tokens, fn %{children: [%{text: text}]} -> text end)
          text = attributes["data-text"]
          true = length(String.to_charlist(reference_text)) == length(String.to_charlist(text))
          level = String.to_integer(attributes["data-level"])
          size = String.to_float(attributes["data-font-size"])

          measured =
            Enum.map(String.codepoints(text), fn character ->
              {:ok, glyph} =
                MathLayout.glyph(character, source_font, size, script_feature_level: level)

              glyph
            end)

          {:ok, row} = MathLayout.row(measured)
          selected = Enum.map(row.glyphs, & &1.glyph_id)

          reference_glyphs =
            attributes["data-glyph-ids"] |> String.split() |> Enum.map(&String.to_integer/1)

          ^reference_glyphs = selected
          top = 12 + level * 36

          baseline =
            top + row.ascent

          pieces =
            row.glyphs
            |> Enum.zip(String.to_charlist(reference_text))
            |> Enum.map(fn {glyph, unicode} ->
              id = Map.fetch!(paint_font.cmap, unicode)

              %{
                type: :text,
                text: glyph.text,
                glyph_ids: [id],
                x: Float.floor((12 + glyph.x) / 0.75 * 64) / 64 * 0.75,
                y: 132 - baseline + glyph.y,
                font: Font.pdf_name(paint_font),
                font_face: paint_font,
                font_size: glyph.font_size,
                width: glyph.width,
                height: glyph.height,
                snap_to_css_pixel_grid: true,
                color: {0, 0, 0}
              }
            end)

          %{type: :glyph_group, actual_text: text, boxes: pieces}
        end)
      end)

    result = PdfWriter.render([%{size: {240, 132}, boxes: boxes}], opts)
    {:ok, pdf} = result
    {:ok, extracted} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    expected = Enum.map_join(groups, & &1.attributes["data-text"])
    ^expected = extracted
    result
  end

  @doc false
  @spec render_math_stretch_fixture(String.t(), keyword()) :: {:ok, binary()} | {:error, term()}
  def render_math_stretch_fixture(html, opts) do
    alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, PdfWriter}
    {:ok, dom} = HtmlParser.parse_document_detailed(html)
    [%{children: [_, %{tag: "body", children: children}]}] = dom.children
    groups = Enum.filter(children, &(Map.get(&1, :tag) == "div"))
    6 = length(groups)
    {:ok, registry} = Font.load_registry(opts)
    {:ok, _, source_font} = Font.resolve("STIX Two Math", 400, :normal, registry)
    {:ok, _, paint_font} = Font.resolve("Math Stretch Test Reference", 400, :normal, registry)
    true = paint_font.units_per_em == source_font.units_per_em
    true = paint_font.ascent == source_font.ascent

    boxes =
      MathLayout.with_context(fn ->
        Enum.map(groups, fn %{attributes: attributes, children: tokens} ->
          text = attributes["data-text"]
          axis = if attributes["data-axis"] == "vertical", do: :vertical, else: :horizontal
          target = String.to_integer(attributes["data-target"])
          left = String.to_integer(attributes["data-left"])
          baseline = String.to_integer(attributes["data-baseline"])
          size = String.to_float(attributes["data-font-size"])
          {:ok, measured} = MathLayout.stretch(text, source_font, size, axis, target)

          reference_ids =
            attributes["data-glyph-ids"] |> String.split() |> Enum.map(&String.to_integer/1)

          ^reference_ids = Enum.map(measured.glyphs, & &1.glyph_id)

          aliases =
            Enum.map(tokens, fn %{children: [%{text: alias_text}]} ->
              hd(String.to_charlist(alias_text))
            end)

          true = length(aliases) == length(measured.glyphs)

          pieces =
            measured.glyphs
            |> Enum.zip(aliases)
            |> Enum.map(fn {glyph, unicode} ->
              %{
                type: :text,
                text: glyph.text,
                glyph_ids: [Map.fetch!(paint_font.cmap, unicode)],
                x: Float.floor((left + glyph.x) / 0.75 * 64) / 64 * 0.75,
                y: 264 - baseline + glyph.y,
                font: Font.pdf_name(paint_font),
                font_face: paint_font,
                font_size: glyph.font_size,
                width: glyph.width,
                height: glyph.height,
                snap_to_css_pixel_grid: true,
                color: {0, 0, 0}
              }
            end)

          %{type: :glyph_group, actual_text: text, boxes: pieces}
        end)
      end)

    result = PdfWriter.render([%{size: {264, 264}, boxes: boxes}], opts)
    {:ok, pdf} = result
    {:ok, extracted} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    expected = Enum.map_join(groups, & &1.attributes["data-text"])
    ^expected = extracted
    result
  end

  test "every fixture requires less than one percent changed pixels" do
    for {fixture, thresholds} <- @fixture_thresholds ++ @real_document_fixture_thresholds do
      assert Keyword.fetch!(thresholds, :max_changed_ratio) == 0.01,
             "#{fixture} must retain the one-percent parity gate"
    end
  end

  test "has thresholds for every browser parity fixture" do
    configured_names =
      Enum.map(@fixture_thresholds, fn {fixture_name, _thresholds} -> fixture_name end)

    fixture_names =
      @fixtures_dir
      |> Path.join("*.html")
      |> Path.wildcard()
      |> Enum.map(&Path.basename/1)
      |> Enum.sort()

    assert Enum.sort(configured_names) == fixture_names
  end

  test "has thresholds for every real document parity fixture" do
    configured_names =
      Enum.map(@real_document_fixture_thresholds, fn {fixture_name, _thresholds} ->
        fixture_name
      end)

    assert Enum.sort(configured_names) == [
             "government_application_form.html",
             "invoice_012.html",
             "material_requisition.html",
             "multi_page_report_012.html",
             "purchase_order.html",
             "statement_012.html",
             "stock_sticker.html",
             "stock_sticker_compact.html",
             "trim_card.html"
           ]
  end

  for {fixture_name, thresholds} <- @fixture_thresholds do
    @fixture_name fixture_name
    @thresholds thresholds
    @tag math_tokens: fixture_name in ["mathml_token_defaults.html", "mathml_tokens_rows.html"]
    @tag math_operators:
           fixture_name in ["mathml_operators.html", "mathml_operators_ordinary.html"]

    if fixture_name in ["html_responsive_images.html", "html_inline_images.html"],
      do: @tag(:responsive_images)

    if fixture_name in [
         "html_child_fragment_destinations.html",
         "html_fragment_destinations.html",
         "html_base_relative_links.html"
       ],
       do: @tag(:child_destinations)

    @tag gauge_parity: @fixture_name in ["html_gauges.html", "html_gauge_boxes.html"]
    if fixture_name in [
         "html_table_formation.html",
         "html_table_headers.html",
         "html_table_caption_blocks.html"
       ],
       do: @tag(:table_formation)

    if fixture_name == "drawing_group_viewports.html", do: @tag(:drawing_group)
    if String.starts_with?(fixture_name, "html_iframe_"), do: @tag(:embedded_viewport)

    if fixture_name in ["html_fragment_destinations.html", "html_base_relative_links.html"],
      do: @tag(:destinations)

    test "matches Chromium for #{@fixture_name}" do
      fixture_path = Path.join(@fixtures_dir, @fixture_name)

      assert %{
               page_count: page_count,
               pages: pages,
               dpi: 72,
               pixel_channel_tolerance: 12,
               chromium_version: chromium_version,
               rasterizer_version: rasterizer_version,
               font_sources: font_sources,
               artifact_dir: artifact_dir
             } =
               PdfVisualCompare.assert_browser_match!(
                 fixture_path,
                 @thresholds
               )

      assert page_count >= 1
      assert length(pages) == page_count
      assert String.starts_with?(chromium_version, "Chromium ")
      assert String.starts_with?(rasterizer_version, "pdftoppm version ")

      assert length(font_sources) in [4, 5, 6, 8]

      assert Enum.all?(font_sources, &String.match?(&1.sha256, ~r/\A[0-9a-f]{64}\z/))
      assert File.dir?(artifact_dir)

      if @fixture_name == "html_child_fragment_destinations.html" do
        assert page_count == 2
        alias NativeElixirPdfUtilities.Pdf.Reader

        for name <- ["native.pdf", "chromium.pdf"] do
          assert {:ok, text} =
                   NativeElixirPdfUtilities.Text.extract(
                     File.read!(Path.join(artifact_dir, name))
                   )

          for caption <- [
                "Parent target",
                "Parent jump",
                "Child jump",
                "Child target",
                "Nested jump",
                "Nested target",
                "Child beginning",
                "Child root",
                "End scoped child destinations"
              ],
              do: assert(text =~ caption)

          assert length(Regex.scan(~r/Child target/, text)) == 2
          assert length(Regex.scan(~r/Nested target/, text)) == 2
        end

        assert {:ok, native} =
                 Reader.read_validated(File.read!(Path.join(artifact_dir, "native.pdf")))

        [first, second] = Enum.map(native.pages, & &1.ref)

        destinations =
          for {_ref, %{value: %{"Subtype" => {:name, "Link"}, "Dest" => destination}}} <-
                native.document.objects,
              do: destination

        assert [{:ref, first}, {:name, "XYZ"}, 12, 168, nil] in destinations

        # Child paint origins round once to CSS pixels. Nested origins round
        # in the containing child viewport before cumulative group transforms.
        for {ref, ys} <- [{first, [141.75, 128.75, 102.5]}, {second, [168, 155, 128.75]}] do
          for y <- ys,
              do: assert([{:ref, ref}, {:name, "XYZ"}, 12, y, nil] in destinations)
        end

        assert length(destinations) == 9
        refute File.read!(Path.join(artifact_dir, "native.pdf")) =~ "/URI (#"
      end

      if @fixture_name == "html_table_caption_blocks.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          text = String.replace(text, ~r/\s+/u, " ")

          for label <- [
                "Block captions before.",
                "Block title",
                "Caption linked",
                "First detail",
                "Second detail",
                "Mixed image",
                "end.",
                "Column",
                "Value",
                "Body one",
                "One",
                "Body two",
                "Two",
                "Fixed caption",
                "Fixed body",
                "Fixed value",
                "Empty caption body",
                "Visible value",
                "Block captions after."
              ],
              do: assert(text =~ label)

          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
          assert length(document.pages) == 1

          links =
            for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects,
                do: link

          assert [
                   %{
                     "A" => %{"URI" => {:string, "https://example.com/caption"}},
                     "Rect" => [left, bottom, right, top]
                   }
                 ] = links

          assert right > left and top > bottom

          images =
            for {_ref, %{value: %{"Subtype" => {:name, "Image"}} = object}} <- document.objects,
                do: object

          assert length(images) == 1
          assert image_draw_count(document) == 1
          assert Enum.all?(document.pages, &Map.has_key?(&1.resources, "Font"))
        end
      end

      if @fixture_name == "html_table_headers.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          text = String.replace(text, ~r/\s+/u, " ")

          for label <- [
                "Header associations before.",
                "Code",
                "Quantity",
                "Description",
                "Part A",
                "12",
                "Explicit linked",
                "Part B",
                "24",
                "Automatic value",
                "Main group",
                "Extra",
                "Row group",
                "First data",
                "Extra first",
                "Last data",
                "Extra last",
                "Positive clipped",
                "Logical implied",
                "Next visible",
                "Next neighbor",
                "Header associations after."
              ],
              do: assert(text =~ label)

          refute text =~ "Hidden blocker"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
          assert length(document.pages) == 1

          links =
            for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects,
                do: link

          assert [
                   %{
                     "A" => %{"URI" => {:string, "https://example.com/headers"}},
                     "Rect" => [left, bottom, right, top]
                   }
                 ] = links

          assert right > left and top > bottom
          assert Enum.all?(document.pages, &Map.has_key?(&1.resources, "Font"))
          assert Enum.all?(document.pages, &(not Map.has_key?(&1.resources, "XObject")))
        end
      end

      if @fixture_name == "html_processing_instructions.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for caption <- [
                "Processing instruction markers",
                "First content",
                "Second content",
                "Table content",
                "List content",
                "Literal <?marker name=Raw?>"
              ],
              do: assert(text =~ caption)

          refute text =~ "Invisible PI data"
          refute text =~ "Inert template content"
          refute text =~ "name=unused.png"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end
      end

      if @fixture_name == "html_table_formation.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for content <- [
                "Table formation before.",
                "Label",
                "Value",
                "Zero group",
                "First linked",
                "Last in group",
                "Next group",
                "Next value",
                "Footer label",
                "Footer value",
                "Implied group",
                "One",
                "Two",
                "Zero column",
                "Neighbor",
                "Bounded columns",
                "Table formation after."
              ],
              do: assert(text =~ content)

          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
          assert length(document.pages) == 1

          links =
            for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects,
                do: link

          assert [
                   %{
                     "A" => %{"URI" => {:string, "https://example.com/table"}},
                     "Rect" => [left, bottom, right, top]
                   }
                 ] = links

          assert right > left and top > bottom

          for page <- document.pages do
            assert {:ok, dictionary} =
                     NativeElixirPdfUtilities.Pdf.Reader.dictionary(document, {:ref, page.ref})

            resources =
              case dictionary["Resources"] do
                {:ref, _} = ref ->
                  assert {:ok, resolved} =
                           NativeElixirPdfUtilities.Pdf.Reader.dictionary(document, ref)

                  resolved

                dictionary ->
                  dictionary
              end

            assert Map.has_key?(resources, "Font")
            refute Map.has_key?(resources, "XObject")
          end
        end
      end

      if @fixture_name == "html_legacy_raw_text.html" do
        source = File.read!(fixture_path)
        assert source =~ "Café &amp; &#13;\r\n"
        assert source =~ "<script>literal()</script>\r"
        assert source =~ "</XmP \t>"
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for caption <- [
                "Before raw text",
                "Café &amp; &#13;",
                "<img src=denied.png>",
                "<script>literal()</script>",
                "</xmp-suffix> boundary",
                "After hidden fallback",
                "Before plaintext",
                "Plaintext é &amp;",
                "<input name=literal value=Literal>",
                "</plaintext>",
                "<div>Still literal</div>"
              ],
              do: assert(text =~ caption)

          refute text =~ "Unused raw fallback"
          refute text =~ "Shown &lt; <b>raw</b>"
          refute text =~ "Inert raw text"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end
      end

      if @fixture_name == "html_initial_text_controls.html" do
        assert page_count == 2

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, 2} = NativeElixirPdfUtilities.Info.page_count(pdf)
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          text = String.replace(text, ~r/\s+/u, " ")

          for caption <- [
                "Initial dimensions and whitespace",
                "Sized",
                "Empty input hint",
                "Password hint",
                "Textarea rows and columns",
                "First spaced line",
                "Indented second",
                "Third line",
                "Textarea hint",
                "Second hint line",
                "After initial values",
                "Wrapping and clipping",
                "After clipping",
                "Inline control",
                "Second row",
                "Following inline caption"
              ],
              do: assert(text =~ caption)

          refute text =~ "Unused fallback" or text =~ "Hidden third row" or
                   text =~ "Hidden inline row"
        end
      end

      if @fixture_name == "html_fieldset_fragmentation.html" do
        assert page_count == 3

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, 3} = NativeElixirPdfUtilities.Info.page_count(pdf)
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          assert text =~ "Fragmented group" and text =~ "After the group"
          for index <- 1..25, do: assert(text =~ "Grouped line #{index}")
        end
      end

      if @fixture_name == "html_fieldsets.html" do
        assert page_count == 2

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, 2} = NativeElixirPdfUtilities.Info.page_count(pdf)
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          text = String.replace(text, ~r/\s+/u, " ")

          for caption <- [
                "Default caption",
                "Default grouped prose",
                "Exempt",
                "First legend exception",
                "Disabled text",
                "Locked",
                "Nested caption",
                "Nested locked",
                "Outer disabling still applies",
                "Earlier radio",
                "Selected disabled radio",
                "Choice",
                "Notes",
                "Centered authored caption",
                "Transparent legend keeps the background",
                "Right caption",
                "Right alignment keeps grouped prose",
                "Flex caption",
                "Flex first",
                "Flex second",
                "Grid caption",
                "Grid first",
                "Grid second",
                "Inline caption",
                "Inline group",
                "Following inline text",
                "A long wrapping legend caption",
                "Narrow grouped prose",
                "Empty caption body",
                "After fieldsets"
              ],
              do: assert(text =~ caption)

          refute text =~ "Unused caption"
        end
      end

      if @fixture_name == "html_metadata_preservation.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for caption <- [
                "Authored record",
                "Inherited translation metadata",
                "Empty enables translation",
                "Padded keyword inherits",
                "Case folded keyword",
                "Initial identifier",
                "Referenced authored prose",
                "Custom authored prose"
              ],
              do: assert(text =~ caption)

          refute text =~ "Unused metadata content" or text =~ "schema.org" or text =~ "people/42"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end

        assert {:ok, dom} =
                 NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_document_detailed(
                   File.read!(fixture_path)
                 )

        [html] =
          NativeElixirPdfUtilities.Validators.HtmlValidator.assign_render_metadata(dom.children)

        [_head, body] = html.children
        refute body._html_translate
        nodes = hd(body.children).children ++ body.children

        for {id, expected} <- [
              {"inherited", false},
              {"enabled", true},
              {"padded", false},
              {"disabled", false},
              {"extra", false},
              {"custom", true}
            ] do
          node = Enum.find(nodes, &(get_in(&1, [:attributes, "id"]) == id))
          assert node._html_translate == expected
        end
      end

      if @fixture_name == "html_inline_images.html" do
        assert page_count == 5

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for content <- [
                "Natural",
                "Authored",
                "Sizes",
                "baseline",
                "top",
                "middle",
                "bottom",
                "Wrap",
                "No wrap",
                "Percent",
                "Auto height",
                "Fitted",
                "Background",
                "Flex",
                "Grid",
                "Table",
                "Zero",
                "Block center end.",
                "Page boundary.",
                "Prior page.",
                "Moved before",
                "End inline images."
              ] do
            assert text =~ content
          end

          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

          image_draws = image_draw_count(document)

          assert image_draws == 20

          links =
            for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = annotation}} <- document.objects,
                do: annotation

          assert length(links) == if(name == "native.pdf", do: 7, else: 8)

          assert links |> Enum.map(& &1["A"]["URI"]) |> Enum.uniq() |> Enum.sort() ==
                   Enum.sort([
                     {:string, "https://example.com/image"},
                     {:string, "https://example.com/crop"},
                     {:string, "https://example.com/page"}
                   ])

          assert Enum.all?(links, fn %{"Rect" => [left, bottom, right, top]} ->
                   right > left and top > bottom
                 end)
        end
      end

      if @fixture_name == "html_responsive_images.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for content <- [
                "Responsive one density",
                "Responsive two density",
                "Responsive width candidates",
                "Picture source order",
                "Picture source dimensions",
                "Picture img fallback",
                "Zero source size",
                "Zero fitted source sizes",
                "End of responsive selection"
              ],
              do: assert(text =~ content)

          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end

        native_page = File.read!(Path.join(artifact_dir, "native-1.ppm"))

        for color <- [<<12, 80, 190>>, <<200, 120, 20>>, <<20, 130, 150>>, <<70, 40, 180>>],
            do: assert(:binary.match(native_page, color) != :nomatch)
      end

      if @fixture_name in ["html_gauges.html", "html_gauge_boxes.html"] do
        assert page_count == if(@fixture_name == "html_gauges.html", do: 1, else: 3)

        captions =
          if @fixture_name == "html_gauges.html" do
            [
              "Progress default",
              "Progress zero",
              "Progress maximum",
              "Progress indeterminate",
              "Meter default",
              "Meter low",
              "Meter suboptimal",
              "Meter even less good",
              "Collapsed range",
              "Progress sized",
              "Progress primitive",
              "Meter primitive",
              "Primitive suboptimal",
              "Primitive even less good",
              "After gauge cases"
            ]
          else
            [
              "Absent progress",
              "Empty progress",
              "Numeric prefix",
              "Invalid maximum",
              "Exponent suffix",
              "Finite extremes",
              "Missing meter",
              "Bad meter points",
              "High optimum",
              "Boundary low",
              "Boundary high",
              "Author accent",
              "Inherited accent",
              "Author box",
              "Author inline",
              "Flex caption",
              "Grid caption",
              "Second page caption",
              "Explicit label",
              "Implicit label",
              "Automatic sizes",
              "After box cases"
            ]
          end

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          for caption <- captions, do: assert(text =~ caption)
          refute text =~ "Hidden fallback"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end

        if @fixture_name == "html_gauges.html" do
          assert File.read!(Path.join(artifact_dir, "native.pdf")) =~ "/Shading << /Gauge"
        end
      end

      if @fixture_name == "html_editability.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(pdf)
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(flattened)

          for caption <- [
                "Outside editing region",
                "Inherited true region",
                "Invalid keyword inherits",
                "Untrimmed false inherits",
                "Nested false region",
                "Untrimmed true stays locked",
                "Empty keyword starts host",
                "Plaintext starts nested host",
                "Escapes and negation",
                "Input uses own mutability",
                "Readonly by boolean presence",
                "Disabled by boolean presence",
                "Textarea stays writable",
                "Textarea stays readonly",
                "Disabled button in editing host",
                "Unknown type uses text default",
                "Search remains writable",
                "mail@example.test",
                "Disabled textarea outside host",
                "Button outside editing host",
                "Dropdown is readonly",
                "Plaintext host outside region",
                "Non-ASCII keyword stays locked",
                "After editability cases"
              ],
              do: assert(text =~ caption)

          refute text =~ "Hidden editability value"
        end

        assert {:ok, fields} =
                 NativeElixirPdfUtilities.Forms.fields(
                   File.read!(Path.join(artifact_dir, "native.pdf"))
                 )

        assert Enum.map(fields, & &1.name) ==
                 ~w(editable readonly disabled notes locked unknown search email disabled_notes choice)
      end

      if @fixture_name == "html_legacy_raw_text.html" do
        source = File.read!(fixture_path)
        assert source =~ "Café &amp; &#13;\r\n"
        assert source =~ "<script>literal()</script>\r"
        assert source =~ "</XmP \t>"
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for caption <- [
                "Before raw text",
                "Café &amp; &#13;",
                "<img src=denied.png>",
                "<script>literal()</script>",
                "</xmp-suffix> boundary",
                "After hidden fallback",
                "Before plaintext",
                "Plaintext é &amp;",
                "<input name=literal value=Literal>",
                "</plaintext>",
                "<div>Still literal</div>"
              ],
              do: assert(text =~ caption)

          refute text =~ "Unused raw fallback"
          refute text =~ "Shown &lt; <b>raw</b>"
          refute text =~ "Inert raw text"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end
      end

      if @fixture_name == "html_mixed_link_button_content.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for caption <- [
                "Anchor block",
                "Nested prose",
                "List first",
                "List second",
                "Code",
                "Marked",
                "Added",
                "Removed",
                "Placeholder",
                "Line first",
                "Line second",
                "Attribute",
                "newline",
                "Initial",
                "After"
              ],
              do: assert(text =~ caption)

          refute text =~ "Inert caption"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end
      end

      if @fixture_name in ["mathml_tokens_rows.html", "mathml_token_defaults.html"] do
        assert page_count == if(@fixture_name == "mathml_token_defaults.html", do: 2, else: 1)

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

          compact = String.replace(text, ~r/\s/u, "")

          for caption <- ["Small", "Medium", "Large", "units", "word", "plain"],
              do: assert(compact =~ caption)

          if @fixture_name == "mathml_token_defaults.html" do
            assert compact =~ "Default" and compact =~ "Flow" and compact =~ "Afterdirecttokens."
          else
            assert compact =~ "Styled" and compact =~ "Afterequations."
          end

          refute compact =~ "[plain]"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end
      end

      if @fixture_name == "html_iframe_math_gauges.html" do
        native = File.read!(Path.join(artifact_dir, "native.pdf"))
        assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(native)

        links =
          for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects,
              do: link

        assert [
                 %{
                   "A" => %{"URI" => {:string, "https://example.test/combined-image"}},
                   "Rect" => [left, bottom, right, top]
                 }
               ] = Enum.filter(links, &Map.has_key?(&1, "A"))

        assert [%{"Dest" => [{:ref, destination_page}, {:name, "XYZ"}, x, y, nil]}] =
                 Enum.filter(links, &Map.has_key?(&1, "Dest"))

        assert destination_page == Enum.at(document.pages, 1).ref
        assert x >= 0 and y >= 0

        assert_in_delta right - left, 60, 0.001
        assert_in_delta top - bottom, 36, 0.001
      end

      if String.starts_with?(@fixture_name, "html_iframe_") do
        assert page_count ==
                 if(
                   @fixture_name in ["html_iframe_page_flow.html", "html_iframe_math_gauges.html"],
                   do: 2,
                   else: 1
                 )

        captions =
          case @fixture_name do
            "html_iframe_math_gauges.html" ->
              [
                "First combined page",
                "Child artwork",
                "Leaf tokens",
                "fi42",
                "After combined child"
              ]

            "html_iframe_viewports.html" ->
              [
                "Parent before",
                "Child scope",
                "Visible link",
                "Nested child",
                "Nested link",
                "Parent after clipping",
                "End parent"
              ]

            "html_iframe_local.html" ->
              [
                "Parent local resource",
                "Local child",
                "Ω",
                "Local nested leaf",
                "Srcdoc takes precedence",
                "End local parent"
              ]

            "html_iframe_page_flow.html" ->
              [
                "First parent page",
                "Child controls",
                "Child value",
                "Child after field",
                "After atomic child",
                "Child long content",
                "Last parent text"
              ]
          end

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          for caption <- captions, do: assert(text =~ caption)
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

          links =
            for {_, %{value: %{"Subtype" => {:name, "Link"}} = annotation}} <- document.objects,
                do: annotation

          refute Enum.any?(links, &(&1["A"]["URI"] == {:string, "https://example.com/offscreen"}))

          assert Enum.all?(links, fn %{"Rect" => [left, bottom, right, top]} ->
                   right > left and top > bottom
                 end)
        end
      end

      if @fixture_name == "drawing_group_viewports.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
          # Chromium emits individual SVG characters under nonuniform scaling.
          # Ignore extraction spacing while retaining every Unicode scalar.
          content = String.replace(text, ~r/\s/u, "")
          assert content =~ "Ωcafé" and content =~ "NestedΩ" and content =~ "Siblingsurvives"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end

        native = File.read!(Path.join(artifact_dir, "native.pdf"))
        assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(native)
        assert length(context.pages) == 1
        assert native =~ "/Rect [32.25 50.25 99.75 67.75]"
        assert native =~ "/Rect [53.25 85.25 98.25 120.25]"
      end

      if @fixture_name == "mathml_operators.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          assert {:ok, text} =
                   NativeElixirPdfUtilities.Text.extract(
                     File.read!(Path.join(artifact_dir, name)),
                     layout: false
                   )

          compact = String.replace(text, ~r/\s/u, "")

          for expected <- [
                "Small",
                "Medium",
                "Large",
                "Fences",
                "Assembly",
                "1+2=3",
                "∑1",
                "→"
              ],
              do: assert(compact =~ expected)
        end

        assert {:ok, native} =
                 NativeElixirPdfUtilities.Text.extract(
                   File.read!(Path.join(artifact_dir, "native.pdf")),
                   layout: false
                 )

        assert native =~ "1\u20622"
      end

      if @fixture_name == "html_disclosures.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          assert {:ok, text} =
                   NativeElixirPdfUtilities.Text.extract(
                     File.read!(Path.join(artifact_dir, name))
                   )

          for caption <- [
                "Closed legend",
                "Open by boolean presence",
                "Visible open prose",
                "Details",
                "Fallback body",
                "Outer legend",
                "Inner closed legend",
                "Inner open legend",
                "Nested visible prose",
                "Grouped closed legend",
                "Grouped open legend",
                "Grouped visible prose",
                "No marker legend",
                "Square marker legend",
                "Square body",
                "Block legend",
                "After disclosures"
              ],
              do: assert(text =~ caption)

          refute text =~ "Hidden"
        end
      end

      if @fixture_name == "html_control_state_selectors.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(pdf)
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(flattened)

          for caption <- [
                "Enabled text",
                "Boolean disabled presence",
                "Readonly remains enabled",
                "Text default stays enabled",
                "Enabled button",
                "Disabled button",
                "ARIA alone stays enabled",
                "Disabled button element",
                "Readonly textarea",
                "Still enabled",
                "Disabled textarea",
                "Ordinary text retains its color",
                "Required by boolean presence",
                "Optional text",
                "Readonly stays required",
                "Disabled stays required",
                "Required textarea",
                "Required dropdown",
                "Optional dropdown",
                "Inapplicable button"
              ],
              do: assert(text =~ caption)

          for caption <- ["Unprinted enabled", "Unprinted disabled", "Hidden does not match"],
              do: refute(text =~ caption)
        end
      end

      if @fixture_name == "html_horizontal_rules.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          assert text =~ "Horizontal rule semantics" and text =~ "End of horizontal rules"
          for index <- 1..22, do: assert(text =~ "Rule case #{index}:")

          for caption <- [
                "Repeated rules begin",
                "Repeated rules end",
                "Nested rule follows",
                "Hidden rules occupy no space",
                "Second"
              ],
              do: assert(text =~ caption)

          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end
      end

      if @fixture_name == "css_named_colors.html" do
        assert page_count == 1
        html = File.read!(fixture_path)

        names =
          Regex.scan(~r/background-color:([a-z]+)/, html, capture: :all_but_first)
          |> List.flatten()

        assert length(names) == 148

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          assert text =~ "Complete named color vocabulary" and
                   text =~ "End of named color vocabulary"

          for color <- names, do: assert(text =~ color)
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end
      end

      if @fixture_name == "html_popover_initial.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for content <- [
                "Initial closed popovers",
                "All initial values remain closed",
                "Actual second sibling",
                "Nested closed content",
                "After popovers"
              ] do
            assert text =~ content
          end

          refute text =~ "Unused"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end
      end

      if @fixture_name == "html_option_semantics.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for content <- [
                "Option labels",
                "Explicit display label",
                "Visible label",
                "Collapsed option text",
                "Alpha Beta",
                "Disabled group fallback",
                "Available label",
                "Explicit disabled selection",
                "Selected label",
                "Empty dropdown height",
                "Blank option height",
                "Disabled options height",
                "End of option semantics"
              ] do
            assert text =~ content
          end

          refute text =~ "Underlying value" or text =~ "Unavailable"
        end

        assert {:ok, []} =
                 artifact_dir
                 |> Path.join("native.pdf")
                 |> File.read!()
                 |> NativeElixirPdfUtilities.Forms.fields()
      end

      if @fixture_name == "html_fragment_destinations.html" do
        assert page_count == 2

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          text = String.replace(text, ~r/\s+/u, " ")

          for caption <- [
                "Internal PDF destinations",
                "Later page link",
                "Decoded link",
                "First duplicate link",
                "Hidden link",
                "Missing link",
                "Beginning link",
                "Top link",
                "Empty target link",
                "Legacy name link",
                "External fragment link",
                "Inert internal content",
                "Second page nested target",
                "Decoded target preserves",
                "every visible word",
                "across wrapped lines",
                "First duplicate target",
                "Second duplicate target",
                "Legacy named target",
                "End of internal destination fixture"
              ],
              do: assert(text =~ caption)

          refute text =~ "Hidden target content" or text =~ "Unused template target"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end

        native = File.read!(Path.join(artifact_dir, "native.pdf"))
        assert native =~ "/Dest [" and native =~ "/XYZ"
        assert native =~ "https://example.test/docs/#remote"
        assert length(Regex.scan(~r{/Subtype /Link}, native)) == 8
        refute native =~ "/URI (#"
        alias NativeElixirPdfUtilities.Pdf.Reader
        assert {:ok, native_context} = Reader.read_validated(native)

        assert {:ok, browser_context} =
                 Reader.read_validated(File.read!(Path.join(artifact_dir, "chromium.pdf")))

        native_second = Enum.at(native_context.pages, 1).ref
        browser_second = Enum.at(browser_context.pages, 1).ref

        assert {:ok, browser_destinations} =
                 Reader.dictionary(browser_context.document, browser_context.catalog["Dests"])

        for target <- ["later", "caf%C3%A9", "duplicate", "empty", "legacy"] do
          assert [{:ref, ^browser_second}, {:name, "XYZ"}, _x, _y, 0] =
                   browser_destinations[target]
        end

        refute Map.has_key?(browser_destinations, "missing") or
                 Map.has_key?(browser_destinations, "hidden")

        assert [{:ref, _first}, {:name, "XYZ"}, 0, 240, 0] = browser_destinations[""]

        native_destinations =
          for {_ref, %{value: %{"Subtype" => {:name, "Link"}, "Dest" => destination}}} <-
                native_context.document.objects,
              do: destination

        for {x, y} <- [{12, 228}, {12, 210}, {12, 156}, {12, 120}] do
          assert [{:ref, native_second}, {:name, "XYZ"}, x, y, nil] in native_destinations
        end
      end

      if @fixture_name == "html_base_relative_links.html" do
        assert page_count == 2

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          text = String.replace(text, ~r/\s+/u, " ")

          for caption <- [
                "Base URL and relative anchors",
                "Chapter link",
                "Parent link",
                "Remote fragment link",
                "Protocol relative link",
                "Wrapped relative anchor",
                "preserves every visible",
                "word across lines",
                "Second page target content",
                "Query link",
                "Inert anchor content",
                "No destination content",
                "End of base URL fixture"
              ],
              do: assert(text =~ caption)

          assert {:ok, 2} = NativeElixirPdfUtilities.Info.page_count(pdf)
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end

        native = File.read!(Path.join(artifact_dir, "native.pdf"))
        assert native =~ "https://example.test/docs/chapter.html"
        assert native =~ "https://example.test/docs/#target"
        refute native =~ "ignored.example"
        refute native =~ "inert.example"
        refute native =~ "held.html"
      end

      if @fixture_name == "page_furniture.html" do
        native_page = File.read!(Path.join(artifact_dir, "native-1.ppm"))
        assert :binary.match(native_page, <<29, 78, 216>>) != :nomatch
      end
    end
  end

  test "signed format-4 deltas retain Chromium parity" do
    fixture_path = Path.expand("../fixtures/fonts/wrapped-glyph.html", __DIR__)
    font_path = Path.expand("../fixtures/fonts/wrapped-glyph.ttf", __DIR__)

    assert %{page_count: 1} =
             PdfVisualCompare.assert_browser_match!(fixture_path,
               render_opts: [
                 fonts: [%{family: "Wrapped Glyph Fixture", path: font_path}],
                 system_font_discovery: false
               ],
               max_changed_ratio: 0.01,
               max_average_delta: 0.035
             )
  end

  test "subset font output retains Chromium parity for print typography" do
    fixture_path = Path.join(@fixtures_dir, "fonts_and_print_media.html")

    assert %{page_count: 1} =
             PdfVisualCompare.assert_browser_match!(fixture_path,
               artifact_dir: "tmp/browser_parity/fonts_and_print_media_subset",
               max_changed_ratio: 0.01,
               max_average_delta: 0.035
             )
  end

  for {fixture_name, thresholds} <- @real_document_fixture_thresholds do
    @fixture_name fixture_name
    @thresholds thresholds

    test "matches Chromium for real document #{@fixture_name}" do
      fixture_path = Path.join(@real_fixtures_dir, @fixture_name)

      assert %{
               page_count: page_count,
               pages: pages,
               dpi: 72,
               pixel_channel_tolerance: 12,
               chromium_version: chromium_version,
               rasterizer_version: rasterizer_version,
               font_sources: font_sources,
               artifact_dir: artifact_dir
             } =
               PdfVisualCompare.assert_browser_match!(
                 fixture_path,
                 @thresholds
               )

      assert page_count >= 1
      assert length(pages) == page_count
      assert String.starts_with?(chromium_version, "Chromium ")
      assert String.starts_with?(rasterizer_version, "pdftoppm version ")
      assert length(font_sources) in [4, 6, 8]
      assert Enum.all?(font_sources, &String.match?(&1.sha256, ~r/\A[0-9a-f]{64}\z/))
      assert File.dir?(artifact_dir)
    end
  end

  defp image_draw_count(document) do
    Enum.reduce(document.pages, 0, fn page, total ->
      assert {:ok, dictionary} =
               NativeElixirPdfUtilities.Pdf.Reader.dictionary(document, {:ref, page.ref})

      streams =
        case dictionary["Contents"] do
          values when is_list(values) -> values
          value -> [value]
        end

      total +
        Enum.reduce(streams, 0, fn stream_ref, count ->
          assert {:ok, content} =
                   NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(
                     document,
                     stream_ref
                   )

          count + length(Regex.scan(~r/\/\S+\s+Do\b/u, content))
        end)
    end)
  end
end
