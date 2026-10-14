defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutSemanticsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.TestSupport.MathPaintGeometry
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  defp styled_math(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _context ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
        {:ok, node.style._math_box}
      end
    end)
  end

  test "initial selection retains the first element geometry and original Unicode" do
    for tag <- ~w(semantics maction) do
      content = "<mrow><mn>12</mn><mo>+</mo><mn>α</mn></mrow>"
      assert {:ok, expected} = styled_math("<math>#{content}</math>")

      assert {:ok, selected} =
               styled_math(
                 "<math><#{tag} selection='2' actiontype='toggle'> \n#{content}<annotation encoding='application/x-tex'>Hidden</annotation><annotation-xml><apply><cn>Hidden</cn></apply></annotation-xml></#{tag}></math>"
               )

      assert Map.take(selected, [
               :width,
               :ascent,
               :descent,
               :ink_ascent,
               :ink_descent,
               :min_content_width,
               :max_content_width,
               :actual_text
             ]) ==
               Map.take(expected, [
                 :width,
                 :ascent,
                 :descent,
                 :ink_ascent,
                 :ink_descent,
                 :min_content_width,
                 :max_content_width,
                 :actual_text
               ])

      assert MathPaintGeometry.glyphs(selected) == MathPaintGeometry.glyphs(expected)
      assert selected.actual_text == "12+α"
      assert :ok == MathLayoutValidator.validate_measured_geometry(selected)
    end
  end

  test "hidden first element does not select a later fallback and whitespace is not an element" do
    for tag <- ~w(semantics maction) do
      assert {:ok, empty} =
               styled_math(
                 "<math><#{tag}> \n<mn style='display:none'>1</mn><mn>2</mn></#{tag}></math>"
               )

      assert empty.actual_text == "" and empty.glyphs == [] and empty.width == 0
      assert {:ok, pdf} = HtmlToPdf.render("<math><#{tag}/><mn>3</mn></math>")
      assert {:ok, "3"} = Text.extract(pdf, layout: false)
    end
  end

  test "default annotation boxes omit character data like Chromium Math blocks" do
    for tag <- ~w(annotation annotation-xml),
        encoding <- ["text/html", "application/x-tex", ""] do
      assert {:ok, measured} =
               styled_math("<math><#{tag} encoding='#{encoding}'>α + β</#{tag}></math>")

      assert measured.actual_text == ""
      assert measured.glyphs == [] and measured.width == 0
      assert :ok == MathLayoutValidator.validate_measured_geometry(measured)
    end
  end

  test "author CSS exposes text and HTML annotations without changing selected source identity" do
    assert {:ok, pdf} =
             HtmlToPdf.render("""
             <style>semantics > :first-child {display:none}
             semantics > annotation {display:inline}
             semantics > annotation-xml {display:inline-block}</style>
             <math><semantics><mn>Hidden</mn><annotation>α</annotation>
             <annotation-xml encoding='TEXT/HTML'><span>β</span></annotation-xml></semantics></math>
             """)

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text == "αβ"

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<math><semantics><mn>1</mn><annotation-xml encoding='application/xhtml+xml'><span id='original'>β</span></annotation-xml></semantics></math>"
             )

    [%{children: [%{children: [first, annotation]}]}] = dom.children
    [html] = annotation.children
    assert html.namespace == :html and html.attributes["id"] == "original"
    assert first.node_id != annotation.node_id
    [parent] = HtmlValidator.assign_render_metadata(hd(dom.children).children)
    [first, annotation] = parent.children
    assert MathLayoutValidator.child_selection_defaults(first, [parent]) == %{}
    assert MathLayoutValidator.child_selection_defaults(annotation, [parent]) == %{display: :none}
  end

  test "unselected HTML and SVG payloads do not fetch, paint, link or consume glyph work" do
    hidden = """
    <annotation-xml encoding='text/html' style='background-image:url(https://unselected.invalid/background.png)'><img src='https://unselected.invalid/image.png'>
    <iframe src='https://unselected.invalid/frame.html'></iframe>
    <a href='https://unselected.invalid/link'>Hidden</a></annotation-xml>
    <annotation-xml><svg width='18' height='18'><rect width='18' height='18' fill='red'/></svg></annotation-xml>
    <annotation-xml><msup><mn>Hidden</mn><mn>2</mn></msup></annotation-xml>
    """

    assert {:ok, expected} = styled_math("<math><mn>12</mn></math>")

    assert {:ok, selected} =
             styled_math("<math><semantics><mn>12</mn>#{hidden}</semantics></math>")

    assert Map.take(selected, [
             :width,
             :ascent,
             :descent,
             :ink_ascent,
             :ink_descent,
             :min_content_width,
             :max_content_width,
             :actual_text
           ]) ==
             Map.take(expected, [
               :width,
               :ascent,
               :descent,
               :ink_ascent,
               :ink_descent,
               :min_content_width,
               :max_content_width,
               :actual_text
             ])

    assert MathPaintGeometry.glyphs(selected) == MathPaintGeometry.glyphs(expected)

    assert {:ok, pdf} =
             HtmlToPdf.render("<math><semantics><mn>12</mn>#{hidden}</semantics></math>")

    assert {:ok, "12"} = Text.extract(pdf, layout: false)
    refute pdf =~ "unselected.invalid"
  end

  test "author declarations can reveal later maction children and selected SVG annotations" do
    assert {:ok, pdf} =
             HtmlToPdf.render("""
             <style>maction > :first-child {display:none} maction > :last-child {display:block}</style>
             <math><maction selection='1'><mn>Hidden</mn><mn>α</mn></maction></math>
             """)

    assert {:ok, "α"} = Text.extract(pdf, layout: false)

    svg = "<svg width='24' height='18'><rect width='24' height='18' fill='blue'/></svg>"
    assert {:ok, ordinary} = styled_math("<math><mtext>#{svg}</mtext></math>")

    assert {:ok, annotation} =
             styled_math(
               "<math><annotation-xml style='display:inline-block'>#{svg}</annotation-xml></math>"
             )

    assert annotation.width == ordinary.width
    assert annotation.actual_text == ordinary.actual_text

    assert Enum.any?(annotation.drawing_items, &(&1.kind == :native))
    assert :ok == MathLayoutValidator.validate_measured_geometry(annotation)
  end

  test "unselected source still obeys static URL and authored-tree limits" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<math><semantics><mn>1</mn><annotation-xml encoding='text/html'><img src='javascript:run()'></annotation-xml></semantics></math>"
             )

    assert diagnostic.stage == :html and diagnostic.source == "src"
    assert diagnostic.message =~ "Active URL schemes"

    Limits.install(Map.put(Limits.effective(), :max_html_nodes, 4))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<math><semantics><mn>1</mn><annotation>Hidden</annotation></semantics></math>"
             )

    assert diagnostic.message =~ "max_html_nodes"
  end

  test "computed hidden backgrounds skip assets while revealed alternatives use ordinary asset diagnostics" do
    html =
      "<math><semantics><mn>1</mn><annotation style='background-image:url(https://unselected.invalid/background.png)'>Hidden</annotation></semantics></math>"

    assert {:ok, _pdf} = HtmlToPdf.render(html)

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render("<style>semantics > annotation {display:block}</style>" <> html)

    assert diagnostic.stage == :asset
    assert diagnostic.source == "https://unselected.invalid/background.png"
    assert diagnostic.message =~ "provide bytes"

    for css <- ["display:none", "display:block;display:none"] do
      assert {:ok, _pdf} =
               HtmlToPdf.render(
                 "<span style='#{css};background-image:url(https://unselected.invalid/background.png)'>Hidden</span>"
               )
    end
  end

  test "selected content retains shared input and font work diagnostics" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<math><semantics><mtext>אב</mtext><annotation>Fallback</annotation></semantics></math>"
             )

    assert diagnostic.stage == :layout and diagnostic.source == "אב"
    assert diagnostic.module == MathLayoutValidator and diagnostic.operation == :measure_math

    Limits.install(Map.put(Limits.effective(), :max_font_math_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             styled_math(
               "<math><semantics><mn>12</mn><annotation>Hidden</annotation></semantics></math>"
             )

    assert diagnostic.message =~ "max_font_math_work"
    assert diagnostic.stage == :font and diagnostic.reason == :resource_limit_exceeded
  end
end
