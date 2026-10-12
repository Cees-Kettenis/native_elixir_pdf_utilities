defmodule NativeElixirPdfUtilities.HtmlToPdf.RenderLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, HtmlParser, Layout, PdfWriter, Style}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "HTML destination descriptors charge the existing render work budget" do
    alias NativeElixirPdfUtilities.Validators.HtmlLinkValidator
    limits = Limits.effective()
    Limits.install(%{limits | max_css_work: 2})
    identity = {make_ref(), make_ref(), make_ref()}
    targets = List.duplicate(%{identity: identity, dx: 0, dy: 0}, 3)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlLinkValidator.validate_boxes([%{html_targets: targets}])
             end)

    assert diagnostic.stage == :pagination and diagnostic.message =~ "max_css_work"
  end

  test "logical table expansion and header scans have separate shared budgets" do
    original = Limits.effective()
    html = "<table><tr><th id=h>Header</th><td headers=h>Value</td></tr></table>"

    for key <- [:max_table_model_work, :max_table_header_work] do
      Limits.install(Map.put(original, key, 1))
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :style and diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :render and diagnostic.message =~ Atom.to_string(key)
      Limits.install(original)
      assert {:ok, _pdf} = HtmlToPdf.render(html)
    end

    Limits.install(%{original | max_table_model_work: 80})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<table><tr><td rowspan=100>Implied</td></tr></table>")

    assert diagnostic.message =~ "max_table_model_work"
    assert {:ok, _pdf} = HtmlToPdf.render("<p>No table model work</p>")
  end

  test "logical span budget diagnostics preserve optional original source locations" do
    Limits.install(%{Limits.effective() | max_layout_cardinality: 2})

    node = %{
      type: :element,
      tag: "td",
      attributes: %{"rowspan" => "3"},
      children: [],
      source: %{text: "<td rowspan=3>", line: 8, column: 5}
    }

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Style.compute_detailed(%{type: :document, children: [node]})

    assert diagnostic.line == 8 and diagnostic.column == 5
    assert diagnostic.source == "<td rowspan=3>"
    assert diagnostic.reason == :resource_limit_exceeded and diagnostic.message =~ "2-item limit"
  end

  test "column group formation ignores parent span independently of strict source conformance" do
    Limits.install(%{Limits.effective() | max_layout_cardinality: 2})
    html = "<table><colgroup><col span=1></colgroup><tr><td>Actual column</td></tr></table>"
    assert {:ok, reference} = HtmlToPdf.render(String.replace(html, " span=1", ""))
    assert {:ok, ^reference} = HtmlToPdf.render(html)
    assert {:ok, dom} = HtmlParser.parse(html)

    assert {:ok, styled} =
             Style.compute_detailed(dom, stylesheets: [{:css, "col[span='1']{width:42pt}"}])

    [table] = styled.children
    assert table.style._table_model.width == 1
    assert hd(hd(table.children).children).style.width == 42.0

    # HTML source conformance prohibits span together with authored col children.
    invalid = String.replace(html, "<colgroup>", "<colgroup span=99999>")
    assert {:error, :unsupported_html} = HtmlParser.parse(invalid)

    # The logical formation algorithm still defines ignored-parent recovery.
    [table] = dom.children
    [group, row] = table.children
    group = %{group | attributes: Map.put(group.attributes, "span", "99999")}
    nodes = HtmlValidator.assign_render_metadata([%{table | children: [group, row]}])

    assert {:ok, [prepared]} =
             NativeElixirPdfUtilities.Validators.HtmlTableValidator.prepare_document(nodes)

    assert prepared._table_model.width == 1
  end

  test "editability traversal charges shared CSS work and releases failed caches" do
    alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache
    limits = Limits.effective()
    Limits.install(%{limits | max_css_work: 3})
    host = %{tag: "div", attributes: %{"contenteditable" => ""}, _selector_id: make_ref()}
    child = %{tag: "p", attributes: %{}, _selector_id: make_ref()}

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               RenderCache.run(fn cache ->
                 for _ <- 1..4 do
                   assert HtmlValidator.editing_state_matches?(:read_write, child, [host], cache)
                 end

                 HtmlValidator.reserve_render_resource(:max_css_work, 2, :css)
               end)
             end)

    assert diagnostic.stage == :css and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_css_work"

    assert HtmlValidator.with_render_budget(fn ->
             RenderCache.run(fn cache ->
               HtmlValidator.editing_state_matches?(
                 :read_only,
                 child,
                 [%{host | attributes: %{"contenteditable" => "false"}}],
                 cache
               )
             end)
           end)

    html = "<div contenteditable>" <> String.duplicate("<p>Visible</p>", 10) <> "</div>"
    Limits.install(%{limits | max_css_work: 10})

    assert {:error, {:resource_limit_exceeded, public}} =
             HtmlToPdf.render(html, stylesheets: [{:css, "p:read-write{color:blue}"}])

    assert public.operation == :render and public.stage == :css
    assert public.message =~ "max_css_work"
    Limits.install(limits)
    assert {:ok, pdf} = HtmlToPdf.render(html, stylesheets: [{:css, "p:read-write{color:blue}"}])
    assert {:ok, ^pdf} = HtmlToPdf.render(html, stylesheets: [{:css, "p{color:blue}"}])
  end

  test "impossible positive selector requirements do not spend sibling-position work" do
    html = String.duplicate("<p class='exists' data-state='actual'>Visible</p>", 600)
    Limits.install(%{Limits.effective() | max_css_work: 1_000})

    assert {:ok, reference} = HtmlToPdf.render(html)

    for selector <- [
          ".exists.missing:nth-child(2n)",
          "p[data-state='different']:first-child",
          ".exists.missing:not(:nth-last-of-type(2n))"
        ] do
      assert {:ok, ^reference} =
               HtmlToPdf.render(html, stylesheets: [{:css, "#{selector}{color:red}"}])
    end

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(html, stylesheets: [{:css, ".exists:nth-child(2n){color:red}"}])

    assert diagnostic.stage == :css
    assert diagnostic.operation == :render
    assert diagnostic.message =~ "max_css_work"
  end

  test "a compound selector that cannot match does not resolve unused language state" do
    language = String.duplicate("aa-", 1_000)
    html = "<p class='exists' lang='#{language}'>Visible</p>"
    Limits.install(%{Limits.effective() | max_css_work: 500})

    assert {:ok, pdf} =
             HtmlToPdf.render(html, stylesheets: [{:css, ".exists.missing:lang(en){color:red}"}])

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "Visible"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(html, stylesheets: [{:css, ".exists:lang(en){color:red}"}])

    assert diagnostic.stage == :css
    assert diagnostic.message =~ "max_css_work"
  end

  test "language canonicalization and range matching consume the shared CSS budget" do
    alias NativeElixirPdfUtilities.Validators.LanguageValidator
    Limits.install(%{Limits.effective() | max_css_work: 5})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               LanguageValidator.canonicalize("en-US")
               LanguageValidator.matches?(["en"], ["en", "us"])
             end)

    assert diagnostic.stage == :css
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_css_work"

    Limits.install(%{Limits.effective() | max_css_work: 50})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p lang='en'>Budgeted</p>",
               stylesheets: [{:css, "p:lang(en){color:red}"}]
             )

    assert diagnostic.operation == :render
    assert diagnostic.stage == :css
    assert diagnostic.message =~ "max_css_work"
  end

  test "source byte limits reject oversized UTF-8 before parsing" do
    Limits.install(%{Limits.effective() | max_html_source_bytes: 8, max_css_source_bytes: 4})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render("<p>é</p>")
    assert diagnostic.stage == :html
    assert diagnostic.operation == :render
    assert diagnostic.message =~ "max_html_source_bytes"
    assert {:error, {:invalid_css, css_diagnostic}} = CssParser.parse_detailed("p { }")
    assert css_diagnostic.message =~ "max_css_source_bytes"
    assert {:ok, _} = HtmlParser.parse_detailed("<p>x</p>")
  end

  test "wide and deeply nested HTML stop before tree expansion" do
    Limits.install(%{Limits.effective() | max_html_nodes: 5, max_html_depth: 2})

    assert {:error, {:resource_limit_exceeded, wide}} =
             HtmlParser.parse_detailed(String.duplicate("<p>x</p>", 3))

    assert wide.message =~ "max_html_nodes"
    Limits.install(%{Limits.effective() | max_html_nodes: 100})

    assert {:error, {:resource_limit_exceeded, deep}} =
             HtmlParser.parse_detailed("<div><div><div>x</div></div></div>")

    assert deep.message =~ "max_html_depth"

    assert {:ok, %{children: [%{children: [%{text: "x"}]}]}} =
             HtmlParser.parse_detailed("<p>x</p>")
  end

  test "stylesheet rules and matching work have shared limits" do
    Limits.install(%{Limits.effective() | max_css_rules: 1})

    assert {:error, {:resource_limit_exceeded, rules}} =
             CssParser.parse_detailed("p { color:red } div {color:blue}")

    assert rules.message =~ "max_css_rules"
    Limits.install(%{Limits.effective() | max_css_work: 3})

    assert {:error, {:resource_limit_exceeded, work}} =
             HtmlToPdf.render("<p>x</p>", stylesheets: [{:css, "p {color:red}"}])

    assert work.stage == :css
    assert work.message =~ "max_css_work"
  end

  test "convenience parsers keep reason-only resource failures" do
    Limits.install(%{Limits.effective() | max_css_work: 1})
    assert {:error, :resource_limit_exceeded} = CssParser.parse("p {color:red}")
    Limits.install(%{Limits.effective() | max_aggregate_css_source_bytes: 1})
    assert {:error, :resource_limit_exceeded} = CssParser.parse_declarations("color:red")
  end

  test "aggregate CSS source budgets include separate stylesheet entries" do
    Limits.install(%{Limits.effective() | max_aggregate_css_source_bytes: 26})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p>x</p>",
               stylesheets: [{:css, "p {color:red}"}, {:css, "p {color:red}"}]
             )

    assert diagnostic.message =~ "max_aggregate_css_source_bytes"
  end

  test "page options, document styles and furniture share prepared CSS budgets" do
    Limits.install(%{Limits.effective() | max_css_rules: 1, max_aggregate_css_source_bytes: 22})

    assert {:ok, _pdf} =
             HtmlToPdf.render("<p>Body</p>",
               margin: 36,
               stylesheets: [{:css, "p {color:red}"}],
               page_furniture: [header: "<p>Header</p>", footer: "<p>Footer</p>"]
             )

    assert {:ok, [_, _]} =
             NativeElixirPdfUtilities.HtmlToPdf.PageFurniture.decorate(
               List.duplicate(%{size: {200.0, 200.0}, boxes: []}, 2),
               %{
                 type: :layout,
                 page_size: {200.0, 200.0},
                 margin: 36.0,
                 boxes: [],
                 content_width: 128.0,
                 content_height: 128.0
               },
               stylesheets: [{:css, "p {color:red}"}],
               page_furniture: [header: "<p>Header</p>"]
             )

    Limits.install(%{Limits.effective() | max_aggregate_css_source_bytes: 100})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p>Body</p>",
               stylesheets: [{:css, "p {color:red}"}, {:css, "p {color:red}"}]
             )

    assert diagnostic.message =~ "max_css_rules"
  end

  test "generated attr content is bounded before concatenation and font expansion" do
    Limits.install(%{Limits.effective() | max_rendered_text_bytes: 10})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p data-x='123456'>x</p>",
               stylesheets: [{:css, "p::before {content:attr(data-x) attr(data-x)}"}]
             )

    assert diagnostic.stage == :style
    assert diagnostic.message =~ "max_rendered_text_bytes"
    assert {:ok, _} = HtmlToPdf.render("<p>safe</p>")
  end

  test "table grid expansion shares a bounded budget and reports layout diagnostics" do
    table = "<table style='border-collapse:collapse'><tr><td colspan='30'>x</td></tr></table>"

    for maximum <- [1, 50, 110] do
      Limits.install(%{Limits.effective() | max_table_grid_work: maximum})

      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(table)
      assert diagnostic.stage == :layout
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :render
      assert diagnostic.message =~ "max_table_grid_work"
    end

    Limits.install(%{Limits.effective() | max_table_grid_work: 50})

    for columns <- ["<colgroup span='100'></colgroup>", "<colgroup><col span='100'></colgroup>"] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render("<table>#{columns}<tr><td>x</td></tr></table>")

      assert diagnostic.stage == :layout
      assert diagnostic.message =~ "max_table_grid_work"
    end

    # The prepared grid is reused by outer-border and track geometry. A
    # 20-column table fits this budget; real 30-column expansion exceeds it.
    Limits.install(%{Limits.effective() | max_table_grid_work: 110})
    assert {:ok, _pdf} = HtmlToPdf.render(String.replace(table, "colspan='30'", "colspan='20'"))

    Limits.install(%{Limits.effective() | max_table_grid_work: 1_000})
    assert {:ok, _pdf} = HtmlToPdf.render(table)
  end

  test "generated boxes and repeated text measurements are charged before allocation" do
    Limits.install(%{Limits.effective() | max_layout_boxes: 1})

    assert {:error, {:resource_limit_exceeded, boxes}} =
             HtmlToPdf.render("<p>hello</p><p>world</p>")

    assert boxes.stage == :layout
    assert boxes.message =~ "max_layout_boxes"
    Limits.install(%{Limits.defaults() | max_layout_text_work: 1})
    assert {:error, {:resource_limit_exceeded, text}} = HtmlToPdf.render("<p>hello</p>")
    assert text.message =~ "max_layout_text_work"
  end

  test "rendered pages honor both renderer and reader ceilings" do
    html = "<p>one</p><p style='break-before:page'>two</p><p style='break-before:page'>three</p>"

    for limit <- [:max_rendered_pages, :max_pdf_pages] do
      Limits.install(Map.put(Limits.defaults(), limit, 2))
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :pagination
      assert diagnostic.message =~ Atom.to_string(limit)
    end

    Limits.install(Limits.defaults())
    assert {:ok, _} = HtmlToPdf.render(html)
  end

  test "page furniture shares the body source and box budget" do
    Limits.install(%{Limits.effective() | max_aggregate_html_source_bytes: 10})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p>x</p>", margin: 50, page_furniture: [header: "<p>y</p>"])

    assert diagnostic.message =~ "max_aggregate_html_source_bytes"
    Limits.install(%{Limits.defaults() | max_layout_boxes: 3})

    assert {:error, {:resource_limit_exceeded, boxes}} =
             HtmlToPdf.render("<p>x</p>", margin: 50, page_furniture: [header: "<p>y</p>"])

    assert boxes.message =~ "max_layout_boxes"
  end

  test "PDF output is checked before flattening including trailer bytes" do
    pages = [%{size: {100.0, 100.0}, boxes: []}]
    assert {:ok, pdf} = PdfWriter.render(pages)
    Limits.install(%{Limits.effective() | max_rendered_pdf_bytes: byte_size(pdf) - 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = PdfWriter.render(pages)
    assert diagnostic.stage == :writer
    assert diagnostic.message =~ "max_rendered_pdf_bytes"
    Limits.install(%{Limits.effective() | max_rendered_pdf_bytes: 1})
    assert {:error, {:resource_limit_exceeded, _}} = PdfWriter.render(pages)
  end

  test "budgets reset after failures and share counters inside nested scopes" do
    Limits.install(%{Limits.effective() | max_css_work: 2})

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(:max_css_work, 2, :css)

               HtmlValidator.with_render_budget(fn ->
                 HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
               end)
             end)

    assert :ok =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(:max_css_work, 2, :css)
             end)

    assert :ok = HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
    assert Process.get({HtmlValidator, :render_budget}) == nil
  end

  test "advanced style and layout entry points reject oversized trees" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<div><div><p>x</p></div></div>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    Limits.install(%{Limits.effective() | max_html_depth: 1})
    assert {:error, {:resource_limit_exceeded, _}} = Style.compute_detailed(dom)
    assert {:error, {:resource_limit_exceeded, _}} = Layout.layout(styled)
  end
end
