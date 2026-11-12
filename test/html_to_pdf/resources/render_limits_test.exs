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

  test "reversed list preparation and painting share image admission" do
    alias NativeElixirPdfUtilities.TestSupport.Paths
    base = Paths.fixture("html_to_pdf/browser_parity/assets")

    for source <- [
          "<img src='initial-small.png' width=10 height=10>",
          "<object data='initial-small.png' width=10 height=10></object>"
        ] do
      assert {:ok, _pdf} =
               HtmlToPdf.render(
                 "<ol reversed><li>A#{source}</li><li>B</li></ol>",
                 base_url: base
               )
    end
  end

  test "HTML destination descriptors charge the existing render work budget" do
    alias NativeElixirPdfUtilities.Validators.HtmlLinkValidator
    limits = Limits.effective()
    Limits.install(%{limits | max_work: 2})
    identity = {make_ref(), make_ref(), make_ref()}
    targets = List.duplicate(%{identity: identity, dx: 0, dy: 0}, 3)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlLinkValidator.validate_boxes([%{html_targets: targets}])
             end)

    assert diagnostic.stage == :pagination and diagnostic.message =~ "max_work"
  end

  test "logical table expansion and header scans consume the same shared work allowance" do
    alias NativeElixirPdfUtilities.Validators.HtmlTableValidator
    original = Limits.effective()

    sources = [
      "<table><tr><th id=h>Header</th><td headers=h>Value</td></tr></table>",
      "<table><tr><td rowspan=100>Implied</td></tr></table>"
    ]

    for html <- sources do
      Limits.install(original)
      assert {:ok, dom} = HtmlParser.parse(html)
      nodes = HtmlValidator.assign_render_metadata(dom.children)
      assert {:ok, _} = HtmlTableValidator.prepare_document(nodes)
      Limits.install(%{original | max_work: 1})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlTableValidator.prepare_document(nodes)

      assert diagnostic.stage == :style
      assert diagnostic.message =~ "max_work"
    end
  end

  test "automatic table headers avoid unused document ID work and preserve opaque scan bounds" do
    alias NativeElixirPdfUtilities.Validators.HtmlTableValidator
    limits = Limits.effective()

    assert {:ok, dom} =
             HtmlParser.parse(
               "<table><tr><th>Top</th></tr><tr><td>Gap</td></tr><tr><th>Near</th></tr><tr><td>Value</td></tr></table>"
             )

    nodes = HtmlValidator.assign_render_metadata(dom.children)

    {table, used} =
      HtmlValidator.with_render_budget(fn ->
        assert {:ok, [table]} = HtmlTableValidator.prepare_document(nodes)

        {table,
         limits.max_work -
           NativeElixirPdfUtilities.Validators.ResourceBudgetValidator.remaining(:max_work)}
      end)

    Limits.install(%{limits | max_work: used})
    assert {:ok, [^table]} = HtmlTableValidator.prepare_document(nodes)
    [top, gap, near, value] = table._table_model.cells
    assert gap.headers == [top.source]
    assert near.headers == []
    assert value.headers == [near.source]

    Limits.install(%{limits | max_work: used - 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlTableValidator.prepare_document(nodes)

    assert diagnostic.stage == :style and diagnostic.message =~ "max_work"

    Limits.install(limits)

    assert {:ok, explicit} =
             HtmlParser.parse(
               "<table><tr><th id=h>Header</th><td headers=h>Value</td></tr></table>"
             )

    explicit_nodes = HtmlValidator.assign_render_metadata(explicit.children)
    Limits.install(%{limits | max_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlTableValidator.prepare_document(explicit_nodes)

    assert diagnostic.message =~ "max_work"
  end

  test "repeated validated document preparation shares work while changed limits remain enforced" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font
    limits = Limits.effective()
    assert {:ok, dom} = HtmlParser.parse("<p>Text</p>")
    [element] = dom.children
    [text] = element.children
    work = 3 + Enum.sum(Enum.map([dom, element, text], &byte_size(&1.source.text)))
    Limits.install(%{limits | max_work: work})
    options = Font.normalize_options([])

    assert :ok ==
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, false} = HtmlValidator.prepare_style_input(dom, [], options)
               assert {:ok, false} = HtmlValidator.prepare_style_input(dom, [], options)
               :ok
             end)

    Limits.install(limits)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, false} = HtmlValidator.prepare_style_input(dom, [], options)
               Limits.install(%{limits | max_entries: 1})
               HtmlValidator.prepare_style_input(dom, [], options)
             end)

    assert diagnostic.message =~ "max_entries"
  end

  test "logical span budget diagnostics preserve optional original source locations" do
    Limits.install(%{Limits.effective() | max_entries: 2})

    node = %{
      type: :element,
      tag: "td",
      attributes: %{"rowspan" => "3"},
      children: [],
      source: %{text: "<td rowspan=3>", line: 8, column: 5}
    }

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.Validators.HtmlTableValidator.prepare_document([node])

    assert diagnostic.line == 8 and diagnostic.column == 5
    assert diagnostic.source == "<td rowspan=3>"
    assert diagnostic.reason == :resource_limit_exceeded and diagnostic.message =~ "2-item limit"
  end

  test "column group formation ignores parent span independently of strict source conformance" do
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
    Limits.install(%{limits | max_work: 3})
    host = %{tag: "div", attributes: %{"contenteditable" => ""}, _selector_id: make_ref()}
    child = %{tag: "p", attributes: %{}, _selector_id: make_ref()}

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               RenderCache.run(fn cache ->
                 for _ <- 1..4 do
                   assert HtmlValidator.editing_state_matches?(:read_write, child, [host], cache)
                 end

                 HtmlValidator.reserve_render_resource(:max_work, 2, :css)
               end)
             end)

    assert diagnostic.stage == :css and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_work"

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
    Limits.install(%{limits | max_work: 10})

    assert {:error, {:resource_limit_exceeded, public}} =
             HtmlToPdf.render(html, stylesheets: [{:css, "p:read-write{color:blue}"}])

    assert public.operation == :render and public.stage == :html
    assert public.message =~ "max_work"
    Limits.install(limits)
    assert {:ok, pdf} = HtmlToPdf.render(html, stylesheets: [{:css, "p:read-write{color:blue}"}])
    assert {:ok, ^pdf} = HtmlToPdf.render(html, stylesheets: [{:css, "p{color:blue}"}])
  end

  test "impossible positive selector requirements do not spend sibling-position work" do
    alias NativeElixirPdfUtilities.HtmlToPdf.CssSelector
    alias NativeElixirPdfUtilities.Validators.CssSelectorValidator

    assert {:ok, dom} =
             HtmlParser.parse(
               String.duplicate("<p class='exists' data-state='actual'>Visible</p>", 600)
             )

    nodes = HtmlValidator.assign_render_metadata(dom.children)

    selectors =
      for source <- [
            ".exists.missing:nth-child(2n)",
            "p[data-state='different']:first-child",
            ".exists.missing:not(:nth-last-of-type(2n))",
            ".exists:nth-child(2n)"
          ] do
        assert {:ok, [selector]} = CssSelectorValidator.parse(source)
        Enum.reverse(selector.parts)
      end

    Limits.install(%{Limits.effective() | max_work: 1})
    context = %{root_children: nodes, root_selector_id: make_ref(), cache: make_ref()}

    for parts <- Enum.take(selectors, 3) do
      assert false ==
               HtmlValidator.with_render_budget(fn ->
                 CssSelector.matches?(parts, hd(nodes), [], context)
               end)
    end

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               CssSelector.matches?(List.last(selectors), hd(nodes), [], context)
             end)

    assert diagnostic.stage == :css
    assert diagnostic.message =~ "max_work"
  end

  test "a compound selector that cannot match does not resolve unused language state" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{CssSelector, RenderCache}
    alias NativeElixirPdfUtilities.Validators.{CssSelectorValidator, LanguageValidator}
    language = String.duplicate("aa-", 1_000)
    assert {:ok, dom} = HtmlParser.parse("<p class='exists' lang='#{language}'>Visible</p>")
    [node] = HtmlValidator.assign_render_metadata(dom.children)

    selectors =
      for source <- [".exists.missing:lang(en)", ".exists:lang(en)"] do
        assert {:ok, [selector]} = CssSelectorValidator.parse(source)
        Enum.reverse(selector.parts)
      end

    Limits.install(%{Limits.effective() | max_work: 500})

    RenderCache.run(fn cache ->
      LanguageValidator.with_document([node], cache, fn ->
        assert false ==
                 HtmlValidator.with_render_budget(fn ->
                   CssSelector.matches?(hd(selectors), node, [], %{cache: cache})
                 end)

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 HtmlValidator.with_render_budget(fn ->
                   CssSelector.matches?(List.last(selectors), node, [], %{cache: cache})
                 end)

        assert diagnostic.stage == :css
        assert diagnostic.message =~ "max_work"
      end)
    end)
  end

  test "language canonicalization and range matching consume the shared CSS budget" do
    alias NativeElixirPdfUtilities.Validators.LanguageValidator
    Limits.install(%{Limits.effective() | max_work: 5})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               LanguageValidator.canonicalize("en-US")
               LanguageValidator.matches?(["en"], ["en", "us"])
             end)

    assert diagnostic.stage == :css
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_work"

    Limits.install(%{Limits.effective() | max_work: 50})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p lang='en'>Budgeted</p>",
               stylesheets: [{:css, "p:lang(en){color:red}"}]
             )

    assert diagnostic.operation == :render
    assert diagnostic.stage == :html
    assert diagnostic.message =~ "max_work"
  end

  test "source byte limits reject oversized UTF-8 before parsing" do
    Limits.install(%{Limits.effective() | max_input_bytes: 4})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render("<p>é</p>")
    assert diagnostic.stage == :html
    assert diagnostic.operation == :render
    assert diagnostic.message =~ "max_input_bytes"

    assert {:error, {:resource_limit_exceeded, css_diagnostic}} =
             CssParser.parse_detailed("p { }")

    assert css_diagnostic.message =~ "max_input_bytes"
    assert {:ok, _} = HtmlParser.parse_detailed("<br>")
  end

  test "wide and deeply nested HTML stop before tree expansion" do
    Limits.install(%{Limits.effective() | max_entries: 5, max_depth: 2})

    assert {:error, {:resource_limit_exceeded, wide}} =
             HtmlParser.parse_detailed(String.duplicate("<p>x</p>", 3))

    assert wide.message =~ "max_entries"
    Limits.install(%{Limits.effective() | max_entries: 100})

    assert {:error, {:resource_limit_exceeded, deep}} =
             HtmlParser.parse_detailed("<div><div><div>x</div></div></div>")

    assert deep.message =~ "max_depth"

    assert {:ok, %{children: [%{children: [%{text: "x"}]}]}} =
             HtmlParser.parse_detailed("<p>x</p>")
  end

  test "stylesheet rules and matching work have shared limits" do
    Limits.install(%{Limits.effective() | max_entries: 1})

    assert {:error, {:resource_limit_exceeded, rules}} =
             CssParser.parse_detailed("p { color:red } div {color:blue}")

    assert rules.message =~ "max_entries"
    Limits.install(%{Limits.effective() | max_work: 3})

    assert {:error, {:resource_limit_exceeded, work}} =
             HtmlToPdf.render("<p>x</p>", stylesheets: [{:css, "p {color:red}"}])

    assert work.stage == :html
    assert work.message =~ "max_work"
  end

  test "convenience parsers keep reason-only resource failures" do
    Limits.install(%{Limits.effective() | max_work: 1})
    assert {:error, :resource_limit_exceeded} = CssParser.parse("p {color:red}")
    Limits.install(%{Limits.effective() | max_input_bytes: 1})
    assert {:error, :resource_limit_exceeded} = CssParser.parse_declarations("color:red")
  end

  test "aggregate CSS source budgets include separate stylesheet entries" do
    Limits.install(%{Limits.effective() | max_input_bytes: 26})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p>x</p>",
               stylesheets: [{:css, "p {color:red}"}, {:css, "p {color:red}"}]
             )

    assert diagnostic.message =~ "max_input_bytes"
  end

  test "page options, document styles and furniture share prepared CSS budgets" do
    original = Limits.effective()

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

    Limits.install(%{original | max_entries: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p>Body</p>",
               stylesheets: [{:css, "p {color:red}"}, {:css, "p {color:red}"}]
             )

    assert diagnostic.message =~ "max_entries"
  end

  test "generated attr content is bounded before concatenation and font expansion" do
    Limits.install(%{Limits.effective() | max_output_bytes: 10})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p data-x='123456'>x</p>",
               stylesheets: [{:css, "p::before {content:attr(data-x) attr(data-x)}"}]
             )

    assert diagnostic.stage == :style
    assert diagnostic.message =~ "max_output_bytes"
    Limits.install(Limits.defaults())
    assert {:ok, _} = HtmlToPdf.render("<p>safe</p>")
  end

  test "table grid expansion shares a bounded budget and reports layout diagnostics" do
    table = "<table style='border-collapse:collapse'><tr><td colspan='30'>x</td></tr></table>"
    original = Limits.effective()

    for html <- [
          table,
          "<table><colgroup span='100'></colgroup><tr><td>x</td></tr></table>",
          "<table><colgroup><col span='100'></colgroup><tr><td>x</td></tr></table>"
        ] do
      Limits.install(original)
      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, _} = Layout.layout(styled)

      for maximum <- [1, 32, 37] do
        Limits.install(%{original | max_work: maximum})
        assert {:error, {:resource_limit_exceeded, diagnostic}} = Layout.layout(styled)
        assert diagnostic.stage in [:layout, :font]
        assert diagnostic.message =~ "max_work"
      end
    end
  end

  test "generated boxes and repeated text measurements are charged before allocation" do
    Limits.install(%{Limits.effective() | max_entries: 1})

    assert {:error, {:resource_limit_exceeded, boxes}} =
             HtmlToPdf.render("<p>hello</p><p>world</p>")

    assert boxes.stage == :html
    assert boxes.message =~ "max_entries"
    Limits.install(%{Limits.defaults() | max_work: 1})
    assert {:error, {:resource_limit_exceeded, text}} = HtmlToPdf.render("<p>hello</p>")
    assert text.message =~ "max_work"
  end

  test "rendered pages honor both renderer and reader ceilings" do
    html = "<p>one</p><p style='break-before:page'>two</p><p style='break-before:page'>three</p>"

    for limit <- [:max_pages] do
      Limits.install(Map.put(Limits.defaults(), limit, 2))
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :pagination
      assert diagnostic.message =~ Atom.to_string(limit)
    end

    Limits.install(Limits.defaults())
    assert {:ok, _} = HtmlToPdf.render(html)
  end

  test "page furniture shares the body source and box budget" do
    Limits.install(%{Limits.effective() | max_input_bytes: 10})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p>x</p>", margin: 50, page_furniture: [header: "<p>y</p>"])

    assert diagnostic.message =~ "max_input_bytes"
    Limits.install(%{Limits.defaults() | max_entries: 3})

    assert {:error, {:resource_limit_exceeded, boxes}} =
             HtmlToPdf.render("<p>x</p>", margin: 50, page_furniture: [header: "<p>y</p>"])

    assert boxes.message =~ "max_entries"
  end

  test "PDF output is checked before flattening including trailer bytes" do
    pages = [%{size: {100.0, 100.0}, boxes: []}]
    assert {:ok, pdf} = PdfWriter.render(pages)
    Limits.install(%{Limits.effective() | max_input_bytes: byte_size(pdf) - 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = PdfWriter.render(pages)
    assert diagnostic.stage == :writer
    assert diagnostic.message =~ "max_input_bytes"
    Limits.install(%{Limits.effective() | max_input_bytes: 1})
    assert {:error, {:resource_limit_exceeded, _}} = PdfWriter.render(pages)
  end

  test "budgets reset after failures and share counters inside nested scopes" do
    Limits.install(%{Limits.effective() | max_work: 2})

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(:max_work, 2, :css)

               HtmlValidator.with_render_budget(fn ->
                 HtmlValidator.reserve_render_resource(:max_work, 1, :css)
               end)
             end)

    assert :ok =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(:max_work, 2, :css)
             end)

    assert :ok = HtmlValidator.reserve_render_resource(:max_work, 1, :css)
    assert Process.get({HtmlValidator, :render_budget}) == nil
  end

  test "advanced style and layout entry points reject oversized trees" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<div><div><p>x</p></div></div>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    Limits.install(%{Limits.effective() | max_depth: 1})
    assert {:error, {:resource_limit_exceeded, _}} = Style.compute_detailed(dom)
    assert {:error, {:resource_limit_exceeded, _}} = Layout.layout(styled)
  end
end
