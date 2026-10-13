defmodule NativeElixirPdfUtilities.HtmlToPdf.BrowserParityTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

  @moduletag :browser_parity

  @fixtures_dir Path.expand("../fixtures/html_to_pdf/browser_parity", __DIR__)
  @real_fixtures_dir Path.expand("../fixtures/html_to_pdf", __DIR__)
  @radical_reference_dir Path.expand("../fixtures/mathml_radical_reference", __DIR__)

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

  @tag :document_fallback
  test "canvas fallback and light-tree styles match the original scripting-disabled source" do
    for fixture <- [
          "html_canvas_fallback.html",
          "html_light_tree_style_selection.html",
          "html_composed_editability.html"
        ] do
      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures_dir, fixture),
          artifact_dir: "tmp/browser_parity/" <> Path.rootname(fixture),
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.scripting == :disabled
      assert stats.page_count == 1
    end
  end

  @tag :document_language_context
  test "composed language uses original logical parents and shadow hosts" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.join(@fixtures_dir, "html_composed_language.html"),
        artifact_dir: "tmp/browser_parity/html_composed_language",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled
    assert stats.page_count == 1
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

  test "fraction reference preserves tree and authored geometry while selecting source-derived script artwork" do
    html = File.read!(Path.join(@fixtures_dir, "mathml_fractions.html"))

    manifest =
      :json.decode(
        File.read!(Path.join(@fixtures_dir, "assets/math-fraction-script-reference.json"))
      )

    adjusted = math_fraction_script_reference(html)
    assert manifest["layout_adjustments"] == []
    assert manifest["tokens"]["nested-n"]["computed_depth"] == 2
    assert manifest["tokens"]["small-n"]["computed_depth"] == 1
    assert manifest["tokens"]["small-d"]["source_gids"] == [4277]
    assert manifest["tokens"]["small-d"]["advances"] == [564]
    assert adjusted =~ ~s(<mn style="font-size:12pt;math-depth:0">89</mn>)
    assert adjusted =~ ~s(<mfrac style="width:54pt;padding:0;color:blue" linethickness="1px">)
    assert adjusted =~ ~s(<a href="https://fraction.test/">Link</a>)
    assert length(Regex.scan(~r/<mfrac\b/, adjusted)) == length(Regex.scan(~r/<mfrac\b/, html))
    assert length(Regex.scan(~r/<mo\b/, adjusted)) == length(Regex.scan(~r/<mo\b/, html))

    for key <- [
          "font-size:",
          "line-height:",
          "width:",
          "height:",
          "margin:",
          "padding:",
          "transform:"
        ] do
      assert length(Regex.scan(Regex.compile!(Regex.escape(key)), adjusted)) ==
               length(Regex.scan(Regex.compile!(Regex.escape(key)), html))
    end

    authored = ~s(<mn data-script-reference="small-n" style="color:red">123</mn>)

    assert math_fraction_script_reference(authored) =~
             ~s(style="color:red;font-family:&quot;Math Fraction Script Reference&quot;")
  end

  @tag :math_fraction_reference
  test "approved fraction CSS reference is reversible and keeps source and outer transport separate" do
    html = File.read!(Path.join(@fixtures_dir, "mathml_fractions.html"))

    manifest =
      :json.decode(
        File.read!(Path.join(@fixtures_dir, "assets/math-fraction-core-reference.json"))
      )

    adjusted = math_fraction_core_reference(html)
    assert manifest["replaced_top_fractions"] == 8
    assert manifest["empirical_offsets"] == []
    assert manifest["native_or_browser_geometry_inputs"] == false

    assert manifest["unchanged_thresholds"] == %{
             "max_changed_ratio" => 0.01,
             "max_average_delta" => 0.01,
             "pixel_channel_tolerance" => 12,
             "dpi" => 72
           }

    assert length(Regex.scan(~r/<math\b/, adjusted)) == 6
    assert adjusted =~ ~s(<mtext><span><a href="https://fraction.test/">Link</a></span></mtext>)

    restored =
      Regex.replace(
        ~r/<style data-core-reference-fonts="source-pinned">.*?<\/style>/s,
        adjusted,
        ""
      )

    package = manifest["browser_packaging"]

    restored =
      String.replace(
        restored,
        package["artwork_only_reference"],
        package["artwork_only_original"],
        global: false
      )

    restored =
      Enum.reduce(manifest["records"], restored, fn {_key, record}, document ->
        assert record["outer_css_transport"]["baseline_witness_height"] ==
                 record["outer_css_transport"]["ascent"]

        assert record["original_sha256"] ==
                 Base.encode16(:crypto.hash(:sha256, record["original_fragment"]), case: :lower)

        String.replace(document, record["reference_fragment"], record["original_fragment"],
          global: false
        )
      end)

    assert restored == html
    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    assert text =~ "1234+5"
    assert text =~ "11Link"
  end

  @tag :math_radicals
  test "approved fixed radical CSS artwork restores the authored tree and preserves native text and links" do
    html = File.read!(Path.join(@fixtures_dir, "mathml_radicals.html"))
    adjusted = math_radical_core_reference(html)

    manifest =
      :json.decode(File.read!(Path.join(@radical_reference_dir, "reference-manifest.json")))

    assert manifest["native_or_browser_geometry_inputs"] == false
    assert manifest["empirical_offsets"] == []
    assert manifest["new_outer_radical_display_overrides"] == 4

    assert manifest["unchanged_thresholds"] == %{
             "max_changed_ratio" => 0.01,
             "max_average_delta" => 0.01,
             "dpi" => 72,
             "pixel_channel_tolerance" => 12
           }

    restored = String.replace(adjusted, manifest["font_stylesheet"], "", global: false)

    restored =
      Enum.reduce(manifest["records"], restored, fn record, document ->
        assert length(:binary.matches(document, record["reference_fragment"])) == 1

        String.replace(document, record["reference_fragment"], record["original_fragment"],
          global: false
        )
      end)

    assert restored == html
    assert adjusted =~ ~s(<msqrt><mtext><a href="https://radical.test/">Link</a></mtext></msqrt>)
    assert length(Regex.scan(~r/<math\b/, adjusted)) == 5
    assert length(Regex.scan(~r/data-m4-text=/, adjusted)) == 16

    assert Enum.map(manifest["records"], &hd(&1["geometry"]["glyphs"])["gid"]) ==
             [1657, 1657, 1657, 1660]

    assert {:ok, pdf} = NativeElixirPdfUtilities.HtmlToPdf.render(html)
    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

    assert String.replace(text, ~r/\s/u, "") ==
             "Small2end.Medium163end.Large7+9end.Nested1235end.LinkedLinkend."

    assert pdf =~ "https://radical.test/"
    refute text =~ "√"
  end

  @fixture_thresholds [
    {"html_math_table_source_clones.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_selected_content_slot_order.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_selected_content_clones.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_selected_content_shadow_clones.html",
     max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_declarative_shadow_slots.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_native_date_time_controls.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_readonly_date_time_controls.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_body_end_padding_boundary.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_paragraph_orphans_widows.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_native_temporal_family.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_initial_constraint_values.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_file_empty_controls.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "Arial",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_file_empty_geometry.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "Arial",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_color_initial_swatches.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_image_initial_buttons.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_special_inline_controls.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_output_initial_state.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_datalist_initial_associations.html",
     render_opts: [
       forms: :static,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400,
           style: :normal
         },
         %{
           family: "DejaVu Sans Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400,
           style: :normal
         }
       ],
       system_font_discovery: false
     ],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_temporal_initial_state.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_initial_range_controls.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_disabled_range_controls.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_range_overflow_controls.html",
     render_opts: [forms: :static, base_url: Path.expand(".")],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_svg_stylesheet_cascade.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_fractions.html",
     chromium_transform: &__MODULE__.math_fraction_core_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"mathml_fractions_ordinary.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_radical_owner_boxes.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_radicals.html",
     chromium_transform: &__MODULE__.math_radical_core_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"mathml_html_children.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_semantics_annotations.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_unknown_rows.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_anonymous_row_boxes.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_matrix_cell_boxes.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_matrix_row_backgrounds.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_matrix_integer_spacing_ordinary.html",
     max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_matrix_owner_boxes.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_basic_matrices.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_phantoms.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_operator_in_range_targets.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_text_selected.html",
     max_changed_ratio: 0.01,
     max_average_delta: 0.01,
     render_opts: [
       system_font_discovery: false,
       fonts: [
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf"),
           weight: 400
         },
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans-Bold.ttf"),
           weight: 700
         },
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans-Oblique.ttf"),
           weight: 400,
           style: :italic
         },
         %{
           family: "DejaVu Sans",
           path: Path.expand("priv/fonts/dejavu/DejaVuSans-BoldOblique.ttf"),
           weight: 700,
           style: :italic
         },
         %{
           family: "SVG Byte Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf"),
           weight: 400
         },
         %{
           family: "SVG Byte Mono",
           path: Path.expand("priv/fonts/dejavu/DejaVuSansMono-Bold.ttf"),
           weight: 700
         }
       ]
     ]},
    {"inline_svg_text_font_attributes.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_text_coordinate_lists.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_text_path_defaults.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_text_paths.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_text_lengths.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_text_rotations_canonical.html",
     max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_reuse.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_gradients.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_patterns_markers.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_masks.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_foreign_object.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_filters.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_path_clips_scoped.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_geometry.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_opacity_groups.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"inline_svg_atomic_pages.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_stylesheet_selection.html",
     native_renderer: &__MODULE__.render_stylesheet_fixture/2,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_iframe_media_selection.html",
     native_renderer: &__MODULE__.render_stylesheet_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_iframe_scrollbars.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_child_fragment_destinations.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_table_atomic_whitespace_baselines.html",
     max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_atomic_nested_baselines.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_atomic_baselines.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_atomic_baseline_spans.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_zero_edge_grid.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_rules_separate_override.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_legacy_borders.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_border_conflicts.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_border_spans_groups.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_baseline_override.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_legacy_center.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_legacy_hints.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_bottom_captions.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_background_layers.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_nonempty_sizing.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_zero_columns_sizing.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_zero_columns.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_table_zero_columns_decorations.html",
     max_changed_ratio: 0.01, max_average_delta: 0.01},
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
    {"html_object_selection.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_embedded_images.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_embedded_text.html",
     native_renderer: &__MODULE__.render_plaintext_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_embed_sources.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_embedded_initialization.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_tracks.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_default_borders.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_nested_borders.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_disabled_borders.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_margin_recovery.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_scrolling.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_border_zero_inheritance.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_initial_tracks.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_whitespace.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_nested.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_oversubscription.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_page_margins.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_frameset_margin_hints.html",
     native_renderer: &__MODULE__.render_embedded_fixture/2,
     chromium_transform: &__MODULE__.local_iframe_reference/1,
     render_opts: [forms: :static],
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_image_scale_down.html",
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
    {"html_number_height_controls.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_initial_number_controls.html",
     render_opts: [forms: :static],
     native_renderer: &PdfVisualCompare.render_initial_numbers/2,
     max_changed_ratio: 0.01,
     max_average_delta: 0.01},
    {"html_authored_number_controls.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_initial_text_controls.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_initial_select_controls.html",
     render_opts: [forms: :static], max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"html_authored_select_controls.html",
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
    {"mathml_scripts.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_scripts_ordinary.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_multiscripts.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_script_corner_kerning.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_all_stretch_rows.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
    {"mathml_operator_font_lengths.html", max_changed_ratio: 0.01, max_average_delta: 0.01},
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
    {"ruby_inline_contexts.html", max_changed_ratio: 0.01, max_average_delta: 0.045},
    {"ruby_optional_tags.html", max_changed_ratio: 0.01, max_average_delta: 0.045},
    {"ruby_wrapping_pagination.html", max_changed_ratio: 0.01, max_average_delta: 0.045},
    {"ruby_canonical_groups.html", max_changed_ratio: 0.01, max_average_delta: 0.045},
    {"ruby_horizontal.html", max_changed_ratio: 0.01, max_average_delta: 0.045},
    {"list_marker_images.html", max_changed_ratio: 0.01, max_average_delta: 0.045},
    {"list_composition.html", max_changed_ratio: 0.01, max_average_delta: 0.045},
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

  @doc "Uses the approved source-derived CSS reference for eight fraction interiors."
  @spec math_fraction_core_reference(String.t()) :: String.t()
  def math_fraction_core_reference(html) do
    manifest =
      :json.decode(
        File.read!(Path.join(@fixtures_dir, "assets/math-fraction-core-reference.json"))
      )

    true =
      Base.encode16(:crypto.hash(:sha256, html), case: :lower) ==
        manifest["inputs"]["test/fixtures/html_to_pdf/browser_parity/mathml_fractions.html"]

    html =
      Enum.reduce(manifest["records"], html, fn {_key, record}, document ->
        String.replace(document, record["original_fragment"], record["reference_fragment"],
          global: false
        )
      end)

    package = manifest["browser_packaging"]

    html =
      String.replace(html, package["artwork_only_original"], package["artwork_only_reference"],
        global: false
      )

    css =
      Enum.reduce(package["faces"], package["font_stylesheet"], fn face, css ->
        path = Path.expand(face["path"], File.cwd!())

        true =
          Base.encode16(:crypto.hash(:sha256, File.read!(path)), case: :lower) == face["sha256"]

        String.replace(css, "../../../" <> face["path"], "file://" <> path)
      end)

    String.replace(html, "</head>", css <> "</head>", global: false)
  end

  @doc "Uses the approved fixed four-root CSS artwork comparison, with the original tree retained for native rendering."
  @spec math_radical_core_reference(String.t()) :: String.t()
  def math_radical_core_reference(html) do
    true =
      Base.encode16(:crypto.hash(:sha256, html), case: :lower) ==
        "cc76de79b7b35335a26c78ffe0efa09a5832b31cc5f6798d72365602ce042666"

    reference = File.read!(Path.join(@radical_reference_dir, "reference.html"))

    true =
      Base.encode16(:crypto.hash(:sha256, reference), case: :lower) ==
        "dee328c2a651e5f0c8770658e71055d1b4606dbbf39c4259dfedd376fd89d070"

    reference
  end

  @doc "Replaces only explicitly marked script glyph artwork with the approved source-font aliases."
  @spec math_fraction_script_reference(String.t()) :: String.t()
  def math_fraction_script_reference(html) do
    manifest =
      :json.decode(
        File.read!(Path.join(@fixtures_dir, "assets/math-fraction-script-reference.json"))
      )

    font_path = Path.join(@fixtures_dir, "assets/math-fraction-script-reference.ttf")

    true =
      Base.encode16(:crypto.hash(:sha256, File.read!(font_path)), case: :lower) ==
        manifest["reference_sha256"]

    html =
      Regex.replace(
        ~r/<(mn|mo)([^>]*data-script-reference="([^"]+)"[^>]*)>([^<]*)<\/\1>/u,
        html,
        fn _, tag, attributes, key, text ->
          record = Map.fetch!(manifest["tokens"], key)
          true = record["text"] == text

          attributes =
            append_reference_style(attributes, "font-family:&quot;#{manifest["family"]}&quot;")

          "<#{tag}#{attributes}>#{record["alias_text"]}</#{tag}>"
        end
      )

    css =
      "<style>@font-face{font-family:'#{manifest["family"]}';src:url('file://#{font_path}') format('truetype');font-weight:400;font-style:normal}</style>"

    String.replace(html, "</head>", css <> "</head>", global: false)
  end

  defp append_reference_style(attributes, declaration) do
    if Regex.match?(~r/\bstyle=["']/, attributes) do
      Regex.replace(~r/\bstyle=(["'])(.*?)\1/, attributes, fn _, quote, css ->
        "style=#{quote}#{css};#{declaration}#{quote}"
      end)
    else
      "#{attributes} style='#{declaration}'"
    end
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

  @doc "Supplies the physical default monospace font used by untouched Chromium plaintext documents."
  @spec render_plaintext_fixture(String.t(), keyword()) :: {:ok, binary()} | {:error, term()}
  def render_plaintext_fixture(html, opts) do
    {output, 0} = System.cmd("fc-match", ["--format=%{file}\n%{family}\n", "monospace"])
    [path, family | _] = String.split(output, "\n")
    bytes = File.read!(path)
    font = %{family: "monospace", data: bytes, weight: 400}

    proof = %{
      physical_source: path,
      physical_family: family,
      generic_family: "monospace",
      source_sha256: Base.encode16(:crypto.hash(:sha256, bytes), case: :lower),
      elixir: System.version(),
      otp: to_string(:erlang.system_info(:otp_release))
    }

    File.write!(
      "tmp/browser_parity/html_embedded_text/native-monospace-source.exs",
      inspect(proof, pretty: true)
    )

    render_embedded_fixture(html, Keyword.update!(opts, :fonts, &(&1 ++ [font])))
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

  @doc "Renders authored page CSS without duplicating it as caller media options."
  @spec render_stylesheet_fixture(String.t(), keyword()) :: {:ok, binary()} | {:error, term()}
  def render_stylesheet_fixture(html, opts) do
    render_embedded_fixture(html, Keyword.drop(opts, [:page_size, :margin]))
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
    if fixture_name == "html_math_table_source_clones.html", do: @tag(:math_clone_sources)
    if fixture_name == "html_selected_content_clones.html", do: @tag(:selected_content)
    if fixture_name == "html_selected_content_shadow_clones.html", do: @tag(:clone_shadows)
    if fixture_name == "html_selected_content_slot_order.html", do: @tag(:clone_slot_order)
    if fixture_name == "html_declarative_shadow_slots.html", do: @tag(:shadow_slots)
    @fixture_name fixture_name
    @thresholds thresholds
    if fixture_name in [
         "html_native_date_time_controls.html",
         "html_readonly_date_time_controls.html",
         "html_body_end_padding_boundary.html",
         "html_paragraph_orphans_widows.html",
         "html_native_temporal_family.html"
       ],
       do: @tag(:native_temporal)

    if fixture_name in [
         "html_color_initial_swatches.html",
         "html_image_initial_buttons.html",
         "html_special_inline_controls.html"
       ],
       do: @tag(:special_inputs)

    if fixture_name in ["html_file_empty_controls.html", "html_file_empty_geometry.html"],
      do: @tag(:file_inputs)

    if fixture_name == "html_initial_constraint_values.html", do: @tag(:constraint_values)

    if fixture_name == "html_native_temporal_family.html", do: @tag(:temporal_family)

    if fixture_name in [
         "html_output_initial_state.html",
         "html_datalist_initial_associations.html"
       ],
       do: @tag(:output_datalist)

    if fixture_name in [
         "html_initial_select_controls.html",
         "html_initial_number_controls.html",
         "html_initial_text_controls.html"
       ],
       do: @tag(:auxiliary_controls_regression)

    if fixture_name == "paragraph_pagination.html", do: @tag(:native_temporal)

    if fixture_name == "html_temporal_initial_state.html", do: @tag(:temporal_initial_state)

    if fixture_name in [
         "html_initial_range_controls.html",
         "html_disabled_range_controls.html",
         "html_range_overflow_controls.html"
       ],
       do: @tag(:range_controls)

    if fixture_name in [
         "html_initial_select_controls.html",
         "html_authored_select_controls.html",
         "html_number_height_controls.html",
         "html_initial_number_controls.html",
         "html_authored_number_controls.html",
         "drawing_group_viewports.html",
         "html_iframe_math_gauges.html",
         "html_iframe_scrollbars.html",
         "html_iframe_media_selection.html",
         "html_child_fragment_destinations.html"
       ],
       do: @tag(:wave9_affected)

    if fixture_name in ["mathml_fractions.html", "mathml_fractions_ordinary.html"],
      do: @tag(:math_fractions)

    if fixture_name == "mathml_fractions_ordinary.html", do: @tag(:math_fraction_ordinary)

    if fixture_name == "mathml_radicals.html", do: @tag(:math_radicals)
    if fixture_name == "mathml_radical_owner_boxes.html", do: @tag(:math_radical_owner_boxes)

    if fixture_name == "mathml_html_children.html", do: @tag(:math_html_children)
    if fixture_name == "mathml_semantics_annotations.html", do: @tag(:math_semantics)
    if fixture_name == "mathml_unknown_rows.html", do: @tag(:math_unknown_rows)
    if fixture_name == "mathml_anonymous_row_boxes.html", do: @tag(:math_anonymous_rows)
    if fixture_name == "mathml_phantoms.html", do: @tag(:math_phantoms)
    if fixture_name == "mathml_basic_matrices.html", do: @tag(:math_matrices)
    if fixture_name == "mathml_matrix_cell_boxes.html", do: @tag(:math_matrix_cells)
    if fixture_name == "mathml_matrix_row_backgrounds.html", do: @tag(:math_matrix_rows)

    if fixture_name == "mathml_matrix_integer_spacing_ordinary.html",
      do: @tag(:math_matrix_integer_spacing)

    if fixture_name == "mathml_matrix_owner_boxes.html", do: @tag(:math_matrix_owner_boxes)
    if fixture_name == "mathml_operator_in_range_targets.html", do: @tag(:math_operator_targets)

    if fixture_name == "mathml_multiscripts.html", do: @tag(:math_multiscripts)

    if fixture_name == "mathml_script_corner_kerning.html", do: @tag(:math_script_corners)

    if fixture_name in ["mathml_scripts.html", "mathml_scripts_ordinary.html"],
      do: @tag(:math_scripts)

    if fixture_name == "mathml_all_stretch_rows.html", do: @tag(:math_all_stretch)
    if fixture_name == "mathml_operator_font_lengths.html", do: @tag(:math_operator_font_lengths)
    @tag math_tokens: fixture_name in ["mathml_token_defaults.html", "mathml_tokens_rows.html"]
    @tag math_operators:
           fixture_name in ["mathml_operators.html", "mathml_operators_ordinary.html"]

    if fixture_name in [
         "html_table_zero_columns.html",
         "html_table_zero_columns_decorations.html",
         "html_table_zero_columns_sizing.html"
       ],
       do: @tag(:table_zero_columns)

    if fixture_name == "html_table_nonempty_sizing.html", do: @tag(:table_sizing)

    if fixture_name in [
         "html_table_rules_separate_override.html",
         "html_table_legacy_borders.html",
         "html_table_border_conflicts.html",
         "html_table_border_spans_groups.html"
       ],
       do: @tag(:table_borders_phase3)

    if fixture_name in [
         "html_table_atomic_whitespace_baselines.html",
         "html_table_atomic_nested_baselines.html",
         "html_table_atomic_baselines.html",
         "html_table_atomic_baseline_spans.html"
       ],
       do: @tag(:table_atomic_baselines_phase5)

    if fixture_name == "html_table_zero_edge_grid.html", do: @tag(:table_zero_edge_grid_phase4)

    if fixture_name == "html_table_baseline_override.html",
      do: @tag(:table_presentation_correction)

    if fixture_name in [
         "html_table_legacy_hints.html",
         "html_table_legacy_center.html",
         "html_table_bottom_captions.html",
         "html_table_background_layers.html"
       ],
       do: @tag(:table_presentation_phase2)

    if String.starts_with?(fixture_name, ["html_table_", "table_", "nested_table_"]) or
         fixture_name == "hidden_tables.html",
       do: @tag(:table_presentation)

    if fixture_name in ["html_responsive_images.html", "html_inline_images.html"],
      do: @tag(:responsive_images)

    if fixture_name in ["html_stylesheet_selection.html", "html_iframe_media_selection.html"],
      do: @tag(:stylesheet_selection)

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

    if fixture_name == "ruby_inline_contexts.html", do: @tag(:ruby_contexts)

    if fixture_name in [
         "ruby_optional_tags.html",
         "ruby_wrapping_pagination.html",
         "ruby_canonical_groups.html",
         "ruby_horizontal.html",
         "list_marker_images.html",
         "list_composition.html",
         "html_list_attributes.html",
         "display_lists_and_inline_block.html"
       ],
       do: @tag(:ruby_lists)

    if fixture_name == "drawing_group_viewports.html", do: @tag(:drawing_group)
    if fixture_name == "html_object_selection.html", do: @tag(:object_selection)
    if fixture_name == "html_embedded_initialization.html", do: @tag(:embedded_initialization)
    if String.starts_with?(fixture_name, "html_frameset_"), do: @tag(:frameset)
    if fixture_name == "html_frameset_whitespace.html", do: @tag(:frameset_whitespace)
    if fixture_name == "html_frameset_initial_tracks.html", do: @tag(:frameset_initial_tracks)

    if fixture_name == "html_frameset_border_zero_inheritance.html",
      do: @tag(:frameset_border_initial)

    if fixture_name in [
         "html_frameset_default_borders.html",
         "html_frameset_nested_borders.html",
         "html_frameset_disabled_borders.html",
         "html_frameset_scrolling.html"
       ],
       do: @tag(:frameset_bevel_scroll)

    if fixture_name in [
         "html_embedded_images.html",
         "html_embedded_text.html",
         "html_embed_sources.html",
         "html_image_scale_down.html"
       ],
       do: @tag(:embedded_dispatch)

    if String.starts_with?(fixture_name, "html_iframe_"), do: @tag(:embedded_viewport)

    if fixture_name in ["html_fragment_destinations.html", "html_base_relative_links.html"],
      do: @tag(:destinations)

    if fixture_name == "inline_svg_text_path_defaults.html",
      do: @tag(:inline_svg_text_path_defaults)

    if fixture_name == "inline_svg_text_paths.html", do: @tag(:inline_svg_text_paths)

    if fixture_name == "inline_svg_text_lengths.html", do: @tag(:inline_svg_text_lengths)

    if fixture_name == "inline_svg_text_rotations_canonical.html",
      do: @tag(:inline_svg_text_rotations)

    if fixture_name == "inline_svg_text_coordinate_lists.html",
      do: @tag(:inline_svg_text_positions)

    if fixture_name in ~w(inline_svg_text_selected.html inline_svg_text_font_attributes.html),
      do: @tag(:inline_svg_text)

    if fixture_name == "inline_svg_patterns_markers.html", do: @tag(:svg_patterns_markers)
    if fixture_name == "inline_svg_masks.html", do: @tag(:svg_masks)
    if fixture_name == "inline_svg_foreign_object.html", do: @tag(:svg_foreign_object)
    if fixture_name == "inline_svg_filters.html", do: @tag(:svg_filters)

    if fixture_name in [
         "inline_svg_text_selected.html",
         "inline_svg_text_font_attributes.html",
         "inline_svg_text_coordinate_lists.html",
         "inline_svg_text_rotations_canonical.html",
         "inline_svg_text_lengths.html",
         "inline_svg_text_paths.html",
         "inline_svg_text_path_defaults.html",
         "inline_svg_reuse.html",
         "inline_svg_gradients.html",
         "inline_svg_path_clips_scoped.html",
         "inline_svg_geometry.html",
         "inline_svg_atomic_pages.html",
         "inline_svg_opacity_groups.html",
         "html_svg_stylesheet_cascade.html"
       ],
       do: @tag(:inline_svg)

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

      if @fixture_name == "mathml_semantics_annotations.html" do
        # A mostly blank page can satisfy the pixel threshold while omitting
        # every CSS-visible annotation. Require its original logical content.
        for renderer <- ["native", "chromium"] do
          assert {:ok, text} =
                   NativeElixirPdfUtilities.Text.extract(
                     File.read!(Path.join(artifact_dir, renderer <> ".pdf")),
                     layout: false
                   )

          text = String.replace(text, ~r/\s+/u, "")

          for original <- ["Textα+β4end.", "Alternativeαβ5end.", "HTML126end.", "Nestedα9end."],
              do:
                assert(text =~ original, "#{renderer} omitted the visible annotation #{original}")
        end
      end

      if @fixture_name == "inline_svg_text_selected.html" do
        assert page_count == 2
        source_faces = Enum.filter(font_sources, &(&1.family == "SVG Byte Mono"))

        assert Enum.map(source_faces, & &1.sha256) == [
                 "b4a6c3e4faab8773f4ff761d56451646409f29abedd68f05d38c2df667d3c582",
                 "bce60f1b4421acd9ea51ba6623d7024ecbe6817a953e3654df62a5e6bdf8f769"
               ]

        native_pdf = File.read!(Path.join(artifact_dir, "native.pdf"))

        assert {:ok, public_text} =
                 NativeElixirPdfUtilities.Text.extract(native_pdf, layout: false)

        for original <- [
              "café Ω",
              "Bold",
              "Centered source text",
              "One",
              "Two",
              "Three",
              "Turn Ω",
              "Visible Ω",
              "Visible SVG text survived"
            ],
            do: assert(public_text =~ original)

        assert length(Regex.scan(~r/Repeat Ω/, public_text)) == 2
        refute public_text =~ "Hidden" or public_text =~ "Removed"
        assert {:ok, spans} = NativeElixirPdfUtilities.Text.extract_spans(native_pdf)
        assert Enum.all?(Enum.flat_map(spans.pages, & &1.spans), & &1.paints_text?)
      end

      if @fixture_name == "inline_svg_text_font_attributes.html" do
        assert {:ok, text} =
                 NativeElixirPdfUtilities.Text.extract(
                   File.read!(Path.join(artifact_dir, "native.pdf")),
                   layout: false
                 )

        for original <- ["Uppercase Ω", "Wide café Ω", "Relative size Ω", "Shared font policy"],
            do: assert(text =~ original)
      end

      if @fixture_name == "inline_svg_text_coordinate_lists.html" do
        native_pdf = File.read!(Path.join(artifact_dir, "native.pdf"))
        assert {:ok, original} = NativeElixirPdfUtilities.Text.extract(native_pdf, layout: false)
        for text <- ["AΩBé", "AΩBC", "List"], do: assert(original =~ text)
        assert length(Regex.scan(~r/Run/, original)) == 2
        assert {:ok, spans} = NativeElixirPdfUtilities.Text.extract_spans(native_pdf)
        assert Enum.all?(Enum.flat_map(spans.pages, & &1.spans), & &1.paints_text?)
      end

      assert page_count >= 1
      assert length(pages) == page_count
      assert String.starts_with?(chromium_version, "Chromium ")
      assert String.starts_with?(rasterizer_version, "pdftoppm version ")

      if @fixture_name == "html_object_selection.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          assert {:ok, text} =
                   NativeElixirPdfUtilities.Text.extract(
                     File.read!(Path.join(artifact_dir, name)),
                     layout: false
                   )

          compact = String.replace(text, ~r/\s/u, "")
          assert compact =~ "inlinefallback"
          assert compact =~ "Unsupportedtypefallback"
          assert length(Regex.scan(~r/Objectchildscope/, compact)) == 2
          refute compact =~ "Unselectedchildfallback"
          # The native group keeps logical text beyond its clip. Chromium omits
          # those instructions. The direct pixel comparison proves visibility.
          if name == "native.pdf",
            do: assert(compact =~ "Offscreenchild"),
            else: refute(compact =~ "Offscreenchild")
        end
      end

      if @fixture_name in [
           "html_embedded_images.html",
           "html_embedded_text.html",
           "html_embed_sources.html"
         ] do
        assert page_count == if(@fixture_name == "html_embed_sources.html", do: 2, else: 1)

        for name <- ["native.pdf", "chromium.pdf"] do
          assert {:ok, text} =
                   NativeElixirPdfUtilities.Text.extract(
                     File.read!(Path.join(artifact_dir, name)),
                     layout: false
                   )

          compact = String.replace(text, ~r/\s/u, "")
          refute compact =~ "Unused"

          case @fixture_name do
            "html_embedded_images.html" ->
              assert compact =~ "SelectedPNGimages"
              assert compact =~ "SelectedJPEGimages"
              assert compact =~ "SelectedstaticSVGimages"
              assert compact =~ "Afterimagereplacements"

            "html_embedded_text.html" ->
              assert length(Regex.scan(~r/<literal>&text/, compact)) == 2
              assert length(Regex.scan(~r/Secondline/, compact)) == 2
              assert compact =~ "Afterliteraldocuments"

            "html_embed_sources.html" ->
              assert compact =~ "Absentsource"
              assert compact =~ "Aboutblankviewport"
              assert length(Regex.scan(~r/Objectchildscope/, compact)) == 2
              assert compact =~ "Afterembeddeddocuments"
          end
        end
      end

      if @fixture_name == "html_embedded_initialization.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          assert {:ok, text} =
                   NativeElixirPdfUtilities.Text.extract(
                     File.read!(Path.join(artifact_dir, name)),
                     layout: false
                   )

          compact = String.replace(text, ~r/\s/u, "")
          assert compact =~ "Selectedinlinesource"
          assert length(Regex.scan(~r/Authorizednestedleaf/, compact)) == 2
          assert length(Regex.scan(~r/Inheritedinlinebase/, compact)) == 2
          refute compact =~ "Unusedobjectfallback"
          refute compact =~ "Objectchildscope"
          assert compact =~ "Parenttargetlinkafterchildren"
        end
      end

      if String.starts_with?(@fixture_name, "html_frameset_") do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          assert {:ok, text} =
                   NativeElixirPdfUtilities.Text.extract(
                     File.read!(Path.join(artifact_dir, name)),
                     layout: false
                   )

          compact = String.replace(text, ~r/\s/u, "")
          assert compact =~ "Redframe"
          refute compact =~ "Inactivenoframes"

          if @fixture_name == "html_frameset_margin_hints.html",
            do: assert(compact =~ "Legacyframemargins")
        end
      end

      if @fixture_name in [
           "html_initial_select_controls.html",
           "html_authored_select_controls.html"
         ] do
        assert_select_contents(artifact_dir, :static, @fixture_name)
      end

      if @fixture_name in [
           "html_output_initial_state.html",
           "html_datalist_initial_associations.html"
         ] do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          assert {:ok, text} =
                   NativeElixirPdfUtilities.Text.extract(
                     File.read!(Path.join(artifact_dir, name)),
                     layout: false
                   )

          compact = String.replace(text, ~r/\s/u, "")

          if @fixture_name == "html_output_initial_state.html" do
            assert compact =~ "Initialcalculation:14."
            assert compact =~ "Firstcoloredvalue."
            assert compact =~ "Nestedresults:Outerinnerresult."
            refute compact =~ "hiddensourcetext" or compact =~ "Hiddenoutput"
          else
            assert compact =~ "authoredvalue" and compact =~ "unchanged"

            refute compact =~ "Hiddenoptiontext" or compact =~ "Selectedsuggestion" or
                     compact =~ "Invisiblelegacyfallback"

            assert compact =~ "Afterhiddensuggestionsources"
          end
        end
      end

      if @fixture_name in [
           "html_initial_number_controls.html",
           "html_authored_number_controls.html"
         ] do
        assert page_count == 2
        assert_number_contents(artifact_dir, :static, @fixture_name)
      end

      if @fixture_name == "html_number_height_controls.html" do
        assert page_count == 1
        assert_number_height_contents(artifact_dir, :static)
      end

      if @fixture_name in ["mathml_fractions.html", "mathml_fractions_ordinary.html"] do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          assert {:ok, text} =
                   NativeElixirPdfUtilities.Text.extract(
                     File.read!(Path.join(artifact_dir, name)),
                     layout: false
                   )

          compact = String.replace(text, ~r/\s/u, "")

          captions =
            if @fixture_name == "mathml_fractions_ordinary.html",
              do: ["Small", "Medium", "Large"],
              else: ["Small", "Medium", "Large", "Authored", "Linked", "Link", "Afterfractions."]

          for caption <- captions, do: assert(compact =~ caption)
        end
      end

      cond do
        @fixture_name in [
          "html_native_date_time_controls.html",
          "html_readonly_date_time_controls.html",
          "html_body_end_padding_boundary.html",
          "html_paragraph_orphans_widows.html",
          "html_native_temporal_family.html",
          "html_initial_constraint_values.html",
          "html_output_initial_state.html",
          "html_datalist_initial_associations.html",
          "html_color_initial_swatches.html",
          "html_image_initial_buttons.html",
          "html_special_inline_controls.html"
        ] ->
          assert length(font_sources) == 2

          assert Enum.sort(Enum.map(font_sources, & &1.family)) == [
                   "DejaVu Sans",
                   "DejaVu Sans Mono"
                 ]

        @fixture_name in ["html_file_empty_controls.html", "html_file_empty_geometry.html"] ->
          assert Enum.sort(Enum.map(font_sources, & &1.family)) == [
                   "Arial",
                   "DejaVu Sans",
                   "DejaVu Sans Mono"
                 ]

        @fixture_name == "mathml_html_children.html" ->
          assert length(font_sources) == 9

        true ->
          assert length(font_sources) in [4, 5, 6, 8]
      end

      if @fixture_name == "html_paragraph_orphans_widows.html" do
        extracted =
          for name <- ["native.pdf", "chromium.pdf"] do
            assert {:ok, %{pages: pages}} =
                     NativeElixirPdfUtilities.Text.extract_spans(
                       File.read!(Path.join(artifact_dir, name))
                     )

            Enum.map(pages, fn page ->
              Enum.map_join(page.spans, & &1.text) |> String.replace(~r/\s/u, "")
            end)
          end

        assert hd(extracted) == List.last(extracted)
      end

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

      if @fixture_name in [
           "html_table_legacy_hints.html",
           "html_table_legacy_center.html",
           "html_table_bottom_captions.html",
           "html_table_background_layers.html"
         ] do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          text = String.replace(text, ~r/\s+/u, " ")

          labels =
            case @fixture_name do
              "html_table_legacy_hints.html" ->
                [
                  "Legacy table presentation",
                  "CSS overrides legacy hints",
                  "Group minimum height",
                  "End legacy hints"
                ]

              "html_table_legacy_center.html" ->
                ["Centered", "CSS left margin", "CSS right margin", "End centered table"]

              "html_table_bottom_captions.html" ->
                [
                  "Inherited bottom link",
                  "Caption block",
                  "Legacy bottom",
                  "CSS top override",
                  "Empty grid bottom",
                  "Bottom captions after"
                ]

              "html_table_background_layers.html" ->
                [
                  "Separated background layers",
                  "Collapsed background layers",
                  "Row color",
                  "Group color",
                  "End background layers"
                ]
            end

          for label <- labels, do: assert(text =~ label)
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)

          if @fixture_name == "html_table_bottom_captions.html" do
            refute text =~ "Hidden caption"
            assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

            links =
              for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects,
                  do: link

            assert [
                     %{
                       "A" => %{"URI" => {:string, "https://example.com/bottom-caption"}},
                       "Rect" => [left, bottom, right, top]
                     }
                   ] = links

            assert right > left and top > bottom
          end
        end
      end

      if @fixture_name == "html_table_zero_columns.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          text = String.replace(text, ~r/\s+/u, " ")

          for label <- [
                "Zero columns before.",
                "Collapsed rows after.",
                "Spaced rows after.",
                "Grouped rows after.",
                "Hidden cells after.",
                "Declared height after.",
                "Zero-height rows after.",
                "Empty table caption",
                "Zero columns after."
              ],
              do: assert(text =~ label)

          refute text =~ ~r/\bHidden cell\b/u
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)
          assert length(document.pages) == 1

          links =
            for {_ref, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects,
                do: link

          assert [
                   %{
                     "A" => %{"URI" => {:string, "https://example.com/empty-table"}},
                     "Rect" => [left, bottom, right, top]
                   }
                 ] = links

          assert right > left and top > bottom
          assert Enum.all?(document.pages, &Map.has_key?(&1.resources, "Font"))
          assert Enum.all?(document.pages, &(not Map.has_key?(&1.resources, "XObject")))
        end
      end

      if @fixture_name in [
           "html_table_zero_columns_decorations.html",
           "html_table_zero_columns_sizing.html",
           "html_table_nonempty_sizing.html"
         ] do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          labels =
            cond do
              @fixture_name == "html_table_zero_columns_decorations.html" ->
                ["collapsed-height", "separate-height", "After"]

              @fixture_name == "html_table_nonempty_sizing.html" ->
                [
                  "Default border box",
                  "Authored content box",
                  "Authored border box cell",
                  "After sizing"
                ]

              true ->
                [
                  "Default border box",
                  "Authored content box",
                  "Authored border box empty",
                  "After sizing"
                ]
            end

          for label <- labels, do: assert(text =~ label)
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
        end
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

      if @fixture_name in ["html_stylesheet_selection.html", "html_iframe_media_selection.html"] do
        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          if @fixture_name == "html_stylesheet_selection.html" do
            for caption <- ["Selected print styles", "Source order", "After selection"],
                do: assert(text =~ caption)

            refute text =~ "Screen only"
          else
            refute text =~ "Wide child"
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

          if @fixture_name == "mathml_html_children.html" do
            assert page_count == 2

            for name <- ["native.pdf", "chromium.pdf"] do
              pdf = File.read!(Path.join(artifact_dir, name))
              assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)

              for caption <- [
                    "Small",
                    "Medium",
                    "Large",
                    "Painted",
                    "Ink",
                    "First",
                    "Last",
                    "Override",
                    "Alpha",
                    "Beta"
                  ],
                  do: assert(String.replace(text, ~r/\s/u, "") =~ caption)
            end
          end
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

            "html_iframe_media_selection.html" ->
              [
                "Parent print context",
                "Narrow child",
                "Child jump",
                "Target",
                "After media client"
              ]

            "html_iframe_scrollbars.html" ->
              ["Scrollbars", "Vertical", "Horizontal", "Coupled", "After clients"]

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

      if @fixture_name == "html_svg_stylesheet_cascade.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)

          for caption <- ["Before SVG stylesheet", "Shared document styles", "After sibling SVG"],
              do: assert(text =~ caption)

          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

          refute Enum.any?(document.objects, fn {_ref, %{value: value}} ->
                   is_map(value) and value["Subtype"] == {:name, "Link"}
                 end)
        end

        assert File.read!(Path.join(artifact_dir, "native.pdf")) =~ "/Subtype /Form"
      end

      if @fixture_name in ["inline_svg_geometry.html", "inline_svg_atomic_pages.html"] do
        expected_pages = if @fixture_name == "inline_svg_geometry.html", do: 1, else: 2
        assert page_count == expected_pages

        captions =
          if @fixture_name == "inline_svg_geometry.html",
            do: [
              "Inline baseline Ω",
              "café after",
              "Shapes and host cascade",
              "Paths, strokes and affine artwork",
              "Nested viewports, slice and percentages",
              "Percentage host survived",
              "Zero area survived",
              "End of geometry"
            ],
            else: [
              "First atomic scene Ω",
              "Second atomic scene café",
              "Both atomic scenes survived"
            ]

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          for caption <- captions, do: assert(text =~ caption)
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
          assert length(context.pages) == expected_pages
          assert pdf =~ "/Font"

          if @fixture_name == "inline_svg_atomic_pages.html" do
            assert pdf =~ "https://example.test/svg-first"
            assert pdf =~ "https://example.test/svg-second"
            assert pdf =~ "/Subtype /Link"
          end
        end

        native = File.read!(Path.join(artifact_dir, "native.pdf"))
        assert native =~ "/Subtype /Form"
      end

      if @fixture_name == "inline_svg_opacity_groups.html" do
        assert page_count == 1

        for name <- ["native.pdf", "chromium.pdf"] do
          pdf = File.read!(Path.join(artifact_dir, name))
          assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
          assert text =~ "Sibling opacity Ω café"
          assert text =~ "Every group survived"
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert pdf =~ "/Font"
        end

        assert File.read!(Path.join(artifact_dir, "native.pdf")) =~ "/Subtype /Form"
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

  @tag range_controls: true
  test "disabled interactive ranges retain initial Chromium artwork and scalar fields" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.join(@fixtures_dir, "html_disabled_range_controls.html"),
        render_opts: [forms: :interactive],
        artifact_dir: "tmp/browser_parity/html_disabled_range_controls-interactive",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 2

    assert {:ok, fields} =
             NativeElixirPdfUtilities.Forms.fields(
               File.read!(Path.join(stats.artifact_dir, "native.pdf"))
             )

    assert Enum.map(fields, &{&1.name, &1.value}) == [
             {"mid", "50"},
             {"min", "0"},
             {"tie", "60"},
             {"tall", "75"},
             {"fieldset", "25"},
             {"none", "10"}
           ]

    assert Enum.all?(fields, & &1.read_only)
  end

  test "interactive initial number heights match Chromium" do
    stats =
      PdfVisualCompare.assert_browser_match!(
        Path.join(@fixtures_dir, "html_number_height_controls.html"),
        render_opts: [forms: :interactive],
        artifact_dir: "tmp/browser_parity/html_number_height_controls-interactive",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
    assert_number_height_contents(stats.artifact_dir, :interactive)
  end

  defp assert_number_height_contents(directory, mode) do
    alias NativeElixirPdfUtilities.{Forms, Text, Info}
    alias NativeElixirPdfUtilities.Pdf.Reader
    native = File.read!(Path.join(directory, "native.pdf"))
    assert {:ok, fields} = Forms.fields(native)

    if mode == :interactive do
      assert Enum.map(fields, &{&1.name, &1.value}) == [{"tall", "42.5"}, {"short", "123"}]
      assert Enum.all?(fields, &(&1.type == :text))

      assert Enum.all?(fields, fn field ->
               Enum.all?(field.widgets, fn widget ->
                 [left, bottom, right, top] = widget.rect

                 left >= 0 and bottom >= 0 and right <= 320 and top <= 240 and right > left and
                   top > bottom
               end)
             end)
    else
      assert fields == []
    end

    for name <- ["native.pdf", "chromium.pdf"] do
      pdf = File.read!(Path.join(directory, name))
      assert {:ok, 1} = Info.page_count(pdf)
      assert {:ok, flat} = Forms.flatten(pdf)
      assert {:ok, []} = Forms.fields(flat)
      assert {:ok, text} = Text.extract(flat)

      for caption <- ["Tall authored number", "After tall number", "After short number", "42.5"],
          do: assert(text =~ caption)

      assert {:ok, document} = Reader.read_validated(flat)
      assert Enum.all?(document.pages, &is_map(&1.resources["Font"]))
      assert Enum.all?(document.pages, &(Map.get(&1.dictionary, "Annots", []) == []))
    end
  end

  for fixture <- ["html_initial_number_controls.html", "html_authored_number_controls.html"] do
    @number_fixture fixture
    @tag wave9_affected: true
    test "interactive initial numbers match Chromium for #{@number_fixture}" do
      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures_dir, @number_fixture),
          render_opts: [forms: :interactive],
          artifact_dir: "tmp/browser_parity/#{Path.rootname(@number_fixture)}-interactive",
          native_renderer:
            if(@number_fixture == "html_initial_number_controls.html",
              do: &PdfVisualCompare.render_initial_numbers/2,
              else: &NativeElixirPdfUtilities.HtmlToPdf.render/2
            ),
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.scripting == :disabled and stats.page_count == 2
      assert_number_contents(stats.artifact_dir, :interactive, @number_fixture)
    end
  end

  defp assert_number_contents(directory, mode, fixture) do
    alias NativeElixirPdfUtilities.{Forms, Info, Text}
    alias NativeElixirPdfUtilities.Pdf.Reader
    alias NativeElixirPdfUtilities.Validators.FormValidator
    native = File.read!(Path.join(directory, "native.pdf"))
    assert {:ok, context} = Reader.read_validated(native)
    assert {:ok, %{fields: fields}} = FormValidator.inspect_document(context)

    if mode == :static do
      assert fields == []
    else
      expected =
        [
          {"empty", ""},
          {"decimal", "01.25"},
          {"scientific", "1e+2"},
          {"invalid", ""},
          {"bounded", "42.5"},
          {"any", "5.5"},
          {"underflow", "8"},
          {"overflow", "12"},
          {"small", "123"},
          {"large", "-25.5"},
          {"readonly", "7.25"},
          {"legend", "3"},
          {"disabled", "9"}
        ] ++
          if(fixture == "html_authored_number_controls.html",
            do: [{"clipped", "12345678901234567890"}],
            else: []
          )

      assert Enum.map(fields, &{&1.name, &1.value}) == expected
      assert Enum.all?(fields, &(&1.type == :text))
      mapped = Map.new(fields, &{&1.name, &1})
      assert mapped["readonly"].read_only and mapped["disabled"].read_only
      refute mapped["legend"].read_only
      assert Bitwise.band(mapped["legend"].flags, 2) == 2
      assert Bitwise.band(mapped["readonly"].flags, 2) == 0

      assert Enum.all?(fields, fn field ->
               Enum.all?(field.widgets, fn widget ->
                 [left, bottom, right, top] = widget.rect

                 right > left and top > bottom and left >= 0 and bottom >= 0 and right <= 340 and
                   top <= 420
               end)
             end)
    end

    for name <- ["native.pdf", "chromium.pdf"] do
      pdf = File.read!(Path.join(directory, name))
      assert {:ok, 2} = Info.page_count(pdf)
      assert {:ok, flat} = Forms.flatten(pdf)
      assert {:ok, []} = Forms.fields(flat)
      assert {:ok, text} = Text.extract(flat)
      text = String.replace(text, ~r/\s+/u, " ")

      for caption <- [
            "Default empty number",
            "Authored decimal value",
            "Scientific lexical value",
            "Invalid value becomes empty",
            "Number hint",
            "Bounded decimal width",
            "Any step keeps default width",
            "Constraint underflow remains visible",
            "Constraint overflow remains visible",
            "After first page numbers",
            "Small chosen font",
            "Large chosen font",
            "Read only required number",
            "First legend number",
            "Disabled fieldset number",
            "After second page numbers",
            "Number evidence link",
            "01.25",
            "1e+2",
            "42.5",
            "5.5",
            "123",
            "-25.5",
            "7.25"
          ],
          do: assert(text =~ caption)

      refute text =~ "invalid-number"

      if fixture == "html_authored_number_controls.html",
        do: assert(text =~ "Clipped full scalar export")

      assert {:ok, document} = Reader.read_validated(flat)

      assert Enum.all?(document.pages, fn page ->
               is_map(page.resources) or match?({:ref, _}, page.resources)
             end)

      links =
        Enum.flat_map(document.pages, fn page ->
          Enum.flat_map(Map.get(page.dictionary, "Annots", []), fn ref ->
            {:ok, annotation} = Reader.resolve(document.document, ref)
            if annotation["Subtype"] == {:name, "Link"}, do: [annotation], else: []
          end)
        end)

      assert [%{"Rect" => [left, bottom, right, top]} = link] = links
      assert link["A"]["URI"] == {:string, "https://example.test/number-evidence"}

      assert right > left and top > bottom and left >= 0 and bottom >= 0 and right <= 340 and
               top <= 420
    end
  end

  for fixture <- ["html_initial_select_controls.html", "html_authored_select_controls.html"] do
    @select_fixture fixture
    @tag wave9_affected: true
    test "interactive initial choices match Chromium for #{@select_fixture}" do
      stats =
        PdfVisualCompare.assert_browser_match!(Path.join(@fixtures_dir, @select_fixture),
          render_opts: [forms: :interactive],
          artifact_dir: "tmp/browser_parity/#{Path.rootname(@select_fixture)}-interactive",
          max_changed_ratio: 0.01,
          max_average_delta: 0.01
        )

      assert stats.scripting == :disabled and stats.page_count == 2
      assert_select_contents(stats.artifact_dir, :interactive, @select_fixture)
    end
  end

  # Both appearances and ordinary page painting retain the same selected
  # content. PDF exports and indices remain independent of clipped captions.
  defp assert_select_contents(directory, mode, fixture) do
    alias NativeElixirPdfUtilities.{Forms, Info, Text}
    alias NativeElixirPdfUtilities.Pdf.Reader
    alias NativeElixirPdfUtilities.Validators.FormValidator
    native = File.read!(Path.join(directory, "native.pdf"))
    assert {:ok, context} = Reader.read_validated(native)
    assert {:ok, %{fields: fields}} = FormValidator.inspect_document(context)

    if mode == :static do
      assert fields == []
    else
      assert length(fields) == 10
      fields = Map.new(fields, &{&1.name, &1})
      assert fields["empty"].value == nil
      assert fields["multi"].value == ["a", "b", "d"]
      assert fields["multi"].dictionary["I"] == [0, 1, 3]
      assert fields["scrolled"].value == "Current row"
      assert fields["scrolled"].dictionary["I"] == [2]

      # /TI addresses the first partially visible option, not the selected one.
      assert fields["scrolled"].dictionary["TI"] ==
               if(fixture == "html_initial_select_controls.html", do: 1, else: 0)

      assert Enum.map(fields["grouped"].choices, & &1.value) == [
               "g",
               "h",
               "Hidden row",
               "CSS hidden row",
               "Tail row"
             ]

      assert fields["grouped"].dictionary["I"] == [0]
      assert fields["count"].value == ["A", "B"]
      assert fields["default"].value == []
      assert fields["zero"].value == "Zero selected"
    end

    for name <- ["native.pdf", "chromium.pdf"] do
      pdf = File.read!(Path.join(directory, name))
      assert {:ok, 2} = Info.page_count(pdf)
      assert {:ok, flat} = Forms.flatten(pdf)
      assert {:ok, []} = Forms.fields(flat)
      assert {:ok, text} = Text.extract(flat)
      text = String.replace(text, ~r/\s+/u, " ")

      for caption <- [
            "Apple",
            "Berry",
            "Cherry",
            "Alpha",
            "Beta",
            "Charlie",
            "Current row",
            "Group heading",
            "Display label",
            "Disabled row",
            "Tail row",
            "After first page controls",
            "First row",
            "Second row",
            "Prefix row",
            "Zero selected",
            "2 selected",
            "Inline first",
            "Inline selected",
            "Following inline caption",
            "After second page controls",
            "Offscreen bounded row"
          ] do
        assert text =~ caption
      end

      omitted =
        ["Hidden row", "CSS hidden row", "Export text"] ++
          if(fixture == "html_initial_select_controls.html",
            do: ["Before scroll", "Inline hidden"],
            else: []
          )

      # Authored padding can expose part of an adjacent row. Keep its clipped
      # artwork and extraction instead of deleting a row to match ink culling.
      for omitted <- omitted do
        refute text =~ omitted
      end
    end
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

      if @fixture_name == "html_paragraph_orphans_widows.html" do
        extracted =
          for name <- ["native.pdf", "chromium.pdf"] do
            assert {:ok, %{pages: pages}} =
                     NativeElixirPdfUtilities.Text.extract_spans(
                       File.read!(Path.join(artifact_dir, name))
                     )

            Enum.map(pages, fn page ->
              Enum.map_join(page.spans, & &1.text) |> String.replace(~r/\s/u, "")
            end)
          end

        assert hd(extracted) == List.last(extracted)
      end

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
