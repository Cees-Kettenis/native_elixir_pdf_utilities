defmodule NativeElixirPdfUtilities.HtmlToPdf.CssDimensionsInlinePaddingTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.{CssMathValidator, CssSizingValidator, HtmlValidator}

  test "calculation dependencies come from shared dimensional grammar" do
    assert {:ok, %{units: ["em", "pt"], percentage?: true}} =
             CssMathValidator.length_dependencies("clamp(1em, calc(50% - 2pt), 100pt)")

    assert {:ok, %{units: ["pt"], percentage?: false}} =
             CssMathValidator.length_dependencies("calc(20pt /* 50% */ + 2pt)")

    for source <- ["calc(2pt + 1deg)", "calc(2pt + 3)", "calc(2pt 3pt)", "max(2pt, 3s)", "-2pt"] do
      assert :error = HtmlValidator.parse_css_size(source, :width)
    end
  end

  test "all calculated dimension properties retain their percentage dependency" do
    for property <- [:width, :height, :min_width, :max_width, :min_height, :max_height] do
      assert {:ok, expression} =
               HtmlValidator.parse_css_size("max(20pt, calc(50% - 2pt))", property)

      assert :ok = HtmlValidator.validate_prepared_css_size(expression, property)
      assert {:css_size_math, {_, true}} = expression
      assert :ok = CssSizingValidator.validate_root(%{property => expression})
    end

    assert {:ok, expression} = HtmlValidator.parse_css_size("min(40pt, calc(80% - 2pt))")
    assert {:ok, dom} = HtmlParser.parse("<p>Font context</p>")
    assert {:ok, %{children: [node]}} = Style.compute(dom)

    assert_in_delta Layout.resolve_content_size(
                      Map.put(node.style, :width, expression),
                      :width,
                      50,
                      nil
                    ),
                    38,
                    0.0001

    assert {:ok, height} = HtmlValidator.parse_css_size("calc(50% + 10pt)", :height)

    assert Layout.resolve_content_size(Map.put(node.style, :height, height), :height, nil, nil) ==
             nil

    assert {:ok, nested_height} =
             HtmlValidator.parse_css_size("min(20pt, calc(50% + 10pt))", :height)

    assert Layout.resolve_content_size(
             Map.put(node.style, :height, nested_height),
             :height,
             nil,
             nil
           ) == nil

    assert_in_delta Layout.resolve_content_size(
                      Map.put(node.style, :height, nested_height),
                      :height,
                      10,
                      nil
                    ),
                    15,
                    0.0001

    assert {:ok, fixed_height} = HtmlValidator.parse_css_size("calc(10pt + 10pt)", :height)

    assert Layout.resolve_content_size(
             Map.put(node.style, :height, fixed_height),
             :height,
             nil,
             nil
           ) == 20
  end

  test "calculated percentage dimensions and constraints share block and inline image bases" do
    image =
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

    html =
      "<body style='margin:0'><div style='width:100pt;height:80pt'><span style='display:inline-block;width:calc(50% + 10pt);height:calc(50% + 10pt)'><img src='#{image}' style='width:calc(50% + 5pt);height:calc(50% - 5pt);min-width:max(10pt,20pt);max-width:clamp(15pt,30pt,40pt)'></span></div></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {180, 180}, margin: 10)
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert_in_delta image.width, 30.0, 0.0001
    assert_in_delta image.height, 20.0, 0.0001
  end

  test "selected font and print viewport govern calculation units" do
    html =
      "<html style='line-height:15pt'><body style='margin:0'><div style='font-size:20pt;width:calc(2em + 1rem + 10vw);height:calc(2lh + 1rlh);line-height:30pt;background:red'></div></body></html>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, page_size: {200, 160}, margin: 0)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 160}, margin: 0)
    [background] = Enum.filter(layout.boxes, &(Map.get(&1, :fill_color) == {1.0, 0.0, 0.0}))
    assert_in_delta background.width, 72.0, 0.0001
    assert_in_delta background.height, 75.0, 0.0001
  end

  test "ordinary nested inline padding advances text and atomic children without changing line height" do
    html =
      "<body style='margin:0;font-size:12pt;line-height:18pt'><p style='margin:0'>A<span style='padding:2pt;background:red'>B<span style='padding:3pt'>C</span><span style='display:inline-block;width:10pt;height:5pt;background:blue'></span>D</span>E</p></body>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 100}, margin: 0)
    [a, b, c, d, e] = Enum.filter(layout.boxes, &(Map.get(&1, :text) in ~w(A B C D E)))

    assert_in_delta b.x - a.x -
                      NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.snap_fragment_width(
                        a.annotation_width
                      ),
                    2,
                    0.0001

    assert_in_delta c.x - b.x -
                      NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.snap_fragment_width(
                        b.annotation_width
                      ),
                    3,
                    0.0001

    [atomic] = Enum.filter(layout.boxes, &(Map.get(&1, :fill_color) == {0.0, 0.0, 1.0}))

    assert_in_delta d.x - c.x -
                      NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.snap_fragment_width(
                        c.annotation_width
                      ),
                    atomic.width + 3,
                    0.0001

    assert_in_delta e.x - d.x -
                      NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator.snap_fragment_width(
                        d.annotation_width
                      ),
                    2,
                    0.0001

    assert length(Enum.uniq(Enum.map([a, b, c, d, e], & &1.y))) == 1
    backgrounds = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :inline_background))
    assert Enum.any?(backgrounds, &(&1.width == 2.0))
    assert Enum.all?(backgrounds, &(&1.height > 12))
  end

  test "root calculations keep selected metrics while defining nested percentage references" do
    html =
      "<html style='font-size:20pt;line-height:25pt;width:calc(50% + 2em);height:calc(50% + 1lh);padding:5pt;box-sizing:border-box'><body style='margin:0;height:100%'><div style='width:50%;height:50%;background:red'></div></body></html>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, page_size: {300, 200}, margin: 0)
    assert styled.document_root_sizing.calculation_context.font_size == 20.0
    assert {:ok, layout} = Layout.layout(styled, page_size: {300, 200}, margin: 0)
    [background] = Enum.filter(layout.boxes, &(Map.get(&1, :fill_color) == {1.0, 0.0, 0.0}))
    assert_in_delta background.width, 90.0, 0.0001
    assert_in_delta background.height, 57.5, 0.0001
  end

  test "length calculation depth is the same during preparation and used-size resolution" do
    source = Enum.reduce(1..40, "1pt", fn _, value -> "calc(" <> value <> ")" end)
    assert {:ok, expression} = HtmlValidator.parse_css_size(source, :width)
    assert {:ok, dom} = HtmlParser.parse("<p>Font context</p>")
    assert {:ok, %{children: [node]}} = Style.compute(dom)

    assert Layout.resolve_content_size(Map.put(node.style, :width, expression), :width, 50, nil) ==
             1.0
  end

  test "original declaration contexts retain calculations without loading runtime metrics" do
    html =
      "<html><body><p style='width:calc(50% + 2em);height:max(20pt,1lh)'>Text</p></body></html>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    observed = make_ref()
    Process.put(observed, nil)

    assert {:ok, result} =
             Style.with_source_style_contexts(dom, [], HtmlValidator.new_css_budget(), fn node,
                                                                                          pseudo,
                                                                                          context ->
               if Map.get(node, :tag) == "p" and is_nil(pseudo),
                 do: Process.put(observed, {node, context})

               :ok
             end)

    {original, %{status: :resolved, style: style}} = Process.delete(observed)
    assert original.source.text == "<p style='width:calc(50% + 2em);height:max(20pt,1lh)'>"
    assert style.width == {:css_size_math, {"calc(50% + 2em)", true}}
    assert style.height == {:css_size_math, {"max(20pt,1lh)", false}}
    assert style.font_face == nil
    assert :font_metrics in result.runtime_obligations
  end

  test "unbounded public size calculations return actionable shared diagnostics" do
    html = "<div style='width:calc(infinity * 1pt)'>Text</div>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "finite"
    assert diagnostic.source == "calc(infinity * 1pt)"
  end

  test "externally supplied calculation metadata fails through shared diagnostics" do
    assert {:ok, dom} = HtmlParser.parse("<p>Text</p>")
    assert {:ok, %{children: [node]} = styled} = Style.compute(dom)

    bad = %{
      node
      | style: Map.put(node.style, :width, {:css_size_math, {"calc(50% + 2pt)", false}})
    }

    assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(%{styled | children: [bad]})
    assert diagnostic.stage == :layout
    assert diagnostic.reason == :invalid_layout
    assert diagnostic.operation == :layout
    assert diagnostic.source == "p"
    assert diagnostic.message =~ "percentage dependency"
  end
end
