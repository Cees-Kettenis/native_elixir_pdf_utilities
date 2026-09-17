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
    Limits.install(%{Limits.effective() | max_aggregate_css_source_bytes: 5})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p>x</p>", stylesheets: [{:css, "p {}"}, {:css, "p {}"}])

    assert diagnostic.message =~ "max_aggregate_css_source_bytes"
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
