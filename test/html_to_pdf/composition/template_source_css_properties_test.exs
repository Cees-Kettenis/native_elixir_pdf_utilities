alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateSourceCssPropertiesTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  @opts [system_font_discovery: false, compress_streams: false]
  @cases [
    {"accent-color", "currentColor"},
    {"border-color", "red currentColor #abc transparent"},
    {"border-top-color", "currentColor"},
    {"border-right-color", "#abc"},
    {"border-bottom-color", "transparent"},
    {"border-left-color", "rgb(0, 0, 0)"},
    {"text-decoration-color", "currentColor"},
    {"letter-spacing", "-1em"},
    {"line-height", "1.5"},
    {"vertical-align", "1em"},
    {"font-weight", "700"},
    {"font-style", "italic"},
    {"text-decoration", "underline dotted currentColor 0.1em"},
    {"text-decoration-line", "underline overline"},
    {"text-decoration-style", "dashed"},
    {"text-decoration-thickness", "10%"},
    {"background-size", "2em auto"},
    {"background-repeat", "repeat-x"},
    {"border", "1em solid currentColor"},
    {"border-top", "thin dotted #abc"},
    {"border-right", "medium dashed transparent"},
    {"border-bottom", "1rem solid currentColor"},
    {"border-left", "2em double blue"},
    {"top", "calc(25% + 1em)"},
    {"right", "auto"},
    {"bottom", "-1em"},
    {"left", "25%"},
    {"quotes", "'[' ']'"},
    {"content", "'prefix' attr(title) counter(chapter) open-quote"},
    {"counter-reset", "chapter 2"},
    {"counter-increment", "chapter -1"},
    {"list-style", "square inside url(missing.png)"},
    {"list-style-type", "decimal"},
    {"list-style-position", "inside"},
    {"grid-template-columns", "repeat(2, minmax(1em, 1fr))"},
    {"grid-template-rows", "2rem auto"}
  ]
  for {property, value} <- @cases do
    @tag :source_property_batch_original
    test "unmatched original #{property} permits parser-time replacement" do
      declaration = unquote(property) <> ":" <> unquote(value)

      html =
        "<style>.absent{#{declaration}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

      resolver = fn reference ->
        send(self(), {:unexpected_dormant_asset, reference})
        {:error, :missing}
      end

      assert {:ok, pdf} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
      assert {:ok, expected} = HtmlToPdf.render("<p>Good</p>", @opts)
      assert pdf == expected
      assert {:ok, "Good"} = Text.extract(pdf)
      refute_received {:unexpected_dormant_asset, _}
    end
  end

  test "all source operations retain canonical uncomputed source states and no element authority" do
    declarations = Enum.map_join(@cases, ";", fn {p, v} -> p <> ":" <> v end)

    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<style>@media screen{p{#{declarations}}}</style><p>Active</p>"
             )

    assert {:ok, result} =
             Style.with_source_style_contexts(
               dom,
               [],
               HtmlValidator.new_css_budget(),
               fn _, _, _ -> :ok end,
               @opts
             )

    records = result.source_obligations.declaration_contexts
    assert length(records) == length(@cases)
    assert Enum.all?(records, &(&1.status == :checked_symbolically and &1.contexts == []))
    assert Enum.all?(records, &is_reference(&1.source.owner.node_id))
    assert Enum.all?(records, &(&1.selection.basis == :inactive_media))
  end

  @invalid_cases [
    {"accent-color", "invalid-color"},
    {"border-color", "red blue green yellow black"},
    {"border-top-color", "invalid-color"},
    {"border-right-color", "invalid-color"},
    {"border-bottom-color", "invalid-color"},
    {"border-left-color", "invalid-color"},
    {"text-decoration-color", "invalid-color"},
    {"letter-spacing", "25%"},
    {"line-height", "-2"},
    {"vertical-align", "2bananas"},
    {"font-weight", "750"},
    {"font-style", "oblique"},
    {"text-decoration", "underline underline"},
    {"text-decoration-line", "underline none"},
    {"text-decoration-style", "unknown"},
    {"text-decoration-thickness", "-1em"},
    {"background-size", "2px 3px 4px"},
    {"background-repeat", "repeat-unknown"},
    {"border", "thin medium solid"},
    {"border-top", "transparent red"},
    {"border-right", "solid dashed"},
    {"border-bottom", "1% solid"},
    {"border-left", "2bananas solid"},
    {"top", "calc(1px + 1deg)"},
    {"right", "2bananas"},
    {"bottom", "unknown(1px)"},
    {"left", "25deg"},
    {"quotes", "'['"},
    {"content", "attr()"},
    {"counter-reset", "chapter 1 2"},
    {"counter-increment", "chapter 1.5"},
    {"list-style", "square circle"},
    {"list-style-type", "unknown"},
    {"list-style-position", "middle"},
    {"grid-template-columns", "minmax(1fr, 2fr)"},
    {"grid-template-rows", "repeat(0, 1pt)"}
  ]
  for {property, value} <- @invalid_cases do
    test "inactive malformed #{property} has actionable exact source diagnostics" do
      declaration = unquote(property) <> ":" <> unquote(value)

      html =
        "<style>@media screen{p{#{declaration}}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
      assert diagnostic.reason == :invalid_css and diagnostic.stage == :css
      assert diagnostic.source == declaration
      assert diagnostic.line == 1 and is_integer(diagnostic.column)
      assert diagnostic.message != ""
    end
  end

  test "symbolic paint, text and resource values retain dependencies without computed ownership" do
    css =
      ".absent{border:1em solid currentColor;letter-spacing:-1em;text-decoration:underline 0.1em currentColor;grid-template-columns:minmax(2rem, 1fr);content:attr(title) counter(chapter) open-quote;list-style:url(missing.png)}"

    assert {:ok, dom} = HtmlParser.parse_document_detailed("<style>#{css}</style><p>Active</p>")

    assert {:ok, result} =
             Style.with_source_style_contexts(
               dom,
               [],
               HtmlValidator.new_css_budget(),
               fn _, _, _ -> :ok end,
               @opts
             )

    [border, spacing, decoration, grid, content, list] =
      result.source_obligations.declaration_contexts

    assert border.source_value.color == :current_color
    assert border.source_value.widths.top.geometry.units == ["em"]
    assert spacing.source_value.spacing.geometry.units == ["em"]
    assert spacing.source_value.spacing.value == "-1em"
    assert decoration.source_value.properties.text_decoration_color == :current_color
    assert decoration.source_value.properties.text_decoration_thickness == {:em, 0.1}
    assert [{:minmax, {:length, length}, {:fr, 1.0}}] = grid.source_value.tracks
    assert length.geometry.units == ["rem"]

    assert content.source_value.properties.content == [
             {:attr, "title"},
             {:counter, "chapter"},
             {:quote, :open_quote}
           ]

    assert list.source_value.properties.list_style_image_source == "missing.png"

    assert Enum.all?(
             result.source_obligations.declaration_contexts,
             &(&1.contexts == [] and &1.status == :checked_symbolically)
           )
  end

  test "new source token families keep cascade keywords and unavailable substitutions symbolic until application" do
    for {property, _} <- @cases do
      for value <- ["inherit", "var(--missing)"] do
        html =
          "<style>.absent{#{property}:#{value}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

        assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
        assert {:ok, expected} = HtmlToPdf.render("<p>Good</p>", @opts)
        assert pdf == expected
      end
    end
  end

  test "selected original style and resource failures cannot be replaced by symbolic policy" do
    for declaration <- [
          "font-style:oblique",
          "border:var(--missing)",
          "border:var(--missing,)",
          "text-decoration:underline underline",
          "border-top:transparent red"
        ] do
      html =
        "<style>.selected{#{declaration}}</style><body><?start name='A'><p class='selected'>Old</p><?end?><template for='A'><p>Good</p></template></body>"

      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
      assert diagnostic.source == declaration
    end

    html =
      "<style>.selected{list-style:url(required.png)}</style><body><?start name='A'><ul class='selected'><li>Old</li></ul><?end?><template for='A'><p>Good</p></template></body>"

    resolver = fn reference ->
      send(self(), {:required_original_asset, reference})
      {:error, :missing}
    end

    assert {:error, {_reason, diagnostic}} =
             HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))

    assert diagnostic.stage == :asset and diagnostic.source == "required.png"
    assert_received {:required_original_asset, _}
  end

  test "prospective selected fonts and tokens cannot reuse the earlier unapplied source completion" do
    html =
      "<style>.selected{font-weight:var(--missing)}</style><body><?marker name='A'><template for='A'><p class='selected'>Good</p></template></body>"

    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.source == "font-weight:var(--missing)"
  end

  test "canonical signed source lengths retain dimensions and reject percentage and automatic spacing" do
    assert {:ok, length} = HtmlValidator.prepare_css_length_source("-2em", :signed_length)
    assert length.value == "-2em" and length.geometry.units == ["em"]
    assert :error == HtmlValidator.prepare_css_length_source("25%", :signed_length)
    assert :error == HtmlValidator.prepare_css_length_source("auto", :signed_length)
    assert {:ok, :baseline} = HtmlValidator.parse_vertical_align("baseline", :source)
    assert {:ok, align} = HtmlValidator.parse_vertical_align("-1rem", :source)
    assert align.geometry.units == ["rem"]
  end

  test "unapplied grid grammar uses canonical cardinality limits and exact original diagnostic coordinates" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(Map.put(limits, :max_layout_cardinality, 2))
    declaration = "grid-template-columns:repeat(3,1em)"

    html =
      "<style>@media screen{p{#{declaration}}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :limits and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.source == declaration
    assert diagnostic.line == 1 and diagnostic.column == 24
    assert diagnostic.operation == :source_style_contexts
    assert diagnostic.message =~ "grid track"
  end

  test "new source token operations consume the supplied shared CSS budget without resetting it" do
    css =
      ".absent{content:attr(title);font-weight:var(--weight,700);list-style:inside square;quotes:'[' ']'}"

    assert {:ok, dom} = HtmlParser.parse_document_detailed("<style>#{css}</style><p>Active</p>")
    budget = HtmlValidator.new_css_budget()

    assert {:ok, _} =
             Style.with_source_style_contexts(dom, [], budget, fn _, _, _ -> :ok end, @opts)

    first = :atomics.get(budget, 2)
    first_bytes = :atomics.get(budget, 1)
    assert first > 0 and first_bytes > 0

    assert {:ok, _} =
             Style.with_source_style_contexts(dom, [], budget, fn _, _, _ -> :ok end, @opts)

    assert :atomics.get(budget, 2) > first
    assert :atomics.get(budget, 1) > first_bytes
  end

  test "new resource token families retain static executable URL policy even when unapplied" do
    for value <- ["url(javascript:alert(1))", "var(--unused,url(javascript:alert(1)))"] do
      declaration = "list-style:#{value}"

      html =
        "<style>.absent{#{declaration}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

      assert {:error, {_reason, diagnostic}} = HtmlToPdf.render(html, @opts)
      assert diagnostic.stage == :css and diagnostic.source =~ "javascript:"
      assert diagnostic.message != ""
    end
  end

  @tag :browser_parity
  @tag :source_property_batch_original
  test "original full property source batch and inserted styles match Chromium" do
    fixture =
      Paths.fixture("html_to_pdf/template_replacement/source_css_properties.html")

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir:
          "tmp/test_artifacts/template_source_css_properties/property-batch-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_with_chromium_arial/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
