defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutIntegrationTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
  end

  defp measured(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
        {:ok, node.style._math_box}
      end
    end)
  end

  test "CSS-visible annotation owners use their actual CSS box exactly once" do
    for tag <- ~w(annotation annotation-xml) do
      assert {:ok, box} =
               measured("""
               <math><#{tag} encoding="text/html" style="display:inline-block;width:40.5pt;height:24pt;padding:1.5pt;border:.75pt solid red;background:#ffff80">AlphaBeta</#{tag}></math>
               """)

      assert_in_delta box.width, 45.0, 1.0e-9
      assert_in_delta box.ascent + box.descent, 28.5, 1.0e-9
      assert box.actual_text == "AlphaBeta"
      assert Enum.any?(box.drawing_items, &(&1.kind == :native))
      refute Enum.any?(box.drawing_items, &(&1.kind == :background))
      assert :ok == MathLayoutValidator.validate_measured_geometry(box)
    end
  end

  test "CSS-visible flex and grid alternatives reuse native HTML composition" do
    for display <- ["flex", "inline-flex", "grid", "inline-grid"] do
      html =
        "<math><annotation-xml encoding='text/html' style='display:#{display};width:60pt;gap:3pt;grid-template-columns:24pt 24pt'><span>Alpha</span><math><mn>2</mn><mo>+</mo><mn>3</mn></math></annotation-xml><mn>1</mn></math>"

      assert {:ok, box} = measured(html)
      assert box.actual_text == "Alpha2+31"
      assert Enum.any?(box.drawing_items, &(&1.kind == :native))
      assert box.width >= 60.0
      assert :ok == MathLayoutValidator.validate_measured_geometry(box)
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert String.replace(text, ~r/\s/u, "") == "Alpha2+31"
    end
  end

  test "CSS alternatives preserve SVG images and surrounding selected text" do
    for display <- ["inline-flex", "inline-grid"] do
      html =
        "<math><annotation-xml encoding='text/html' style='display:#{display};width:90pt;gap:3pt;grid-template-columns:42pt 42pt'><span>α</span><svg width='48' height='24'><rect width='48' height='24' fill='#ffff80'/><text x='2' y='16' font-family='DejaVu Sans' font-size='12'>Scene</text></svg></annotation-xml><mn>1</mn></math>"

      assert {:ok, box} = measured(html)
      assert box.actual_text == "α1"
      assert :ok == MathLayoutValidator.validate_measured_geometry(box)
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert pdf =~ "/Subtype /Image"
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert String.replace(text, ~r/\s/u, "") == "α1"
    end
  end

  test "mixed HTML and nested MathML retain source text, glyphs and real native paint" do
    html = """
    <math><mn>1</mn><mtext><span style="background:#ffff80">Before<math><mfrac><mn>2</mn><mn>3</mn></mfrac></math>After</span></mtext><mn>4</mn></math>
    """

    assert {:ok, box} = measured(html)
    assert box.actual_text == "1Before23After4"
    assert length(box.glyphs) == 2
    assert Enum.any?(box.drawing_items, &(&1.kind == :native))
    assert box.ascent > 0 and box.descent > 0
    assert :ok == MathLayoutValidator.validate_measured_geometry(box)
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "1Before23After4"
  end

  test "direct mathematical schemas inside tokens use their own prepared math context" do
    for content <- [
          "<math><mn>2</mn><mo>+</mo><mn>3</mn></math>",
          "<math display='block'><mfrac><mn>2</mn><mn>3</mn></mfrac></math>"
        ] do
      assert {:ok, box} = measured("<math><mtext>#{content}</mtext><mn>4</mn></math>")
      expected = if String.contains?(content, "<mfrac>"), do: "234", else: "2+34"
      assert box.actual_text == expected
      assert :ok == MathLayoutValidator.validate_measured_geometry(box)
      assert {:ok, pdf} = HtmlToPdf.render("<math><mtext>#{content}</mtext><mn>4</mn></math>")
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert String.replace(text, ~r/\s/u, "") == expected
    end
  end

  test "selected XML annotations compose nested math through HTML integration points" do
    for encoding <- ["text/html", "application/xhtml+xml", "TEXT/HTML"] do
      html = """
      <style>semantics > :first-child {display:none} semantics > annotation-xml {display:inline-block}</style>
      <math><semantics><mn>Hidden</mn><annotation-xml encoding="#{encoding}"><span>α</span><math><mn>2</mn><mo>+</mo><mn>3</mn></math><span>β</span></annotation-xml></semantics><mn>4</mn></math>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert String.replace(text, ~r/\s/u, "") == "α2+3β4"
    end
  end

  test "SVG images keep surrounding HTML and nested math logical content" do
    html = """
    <math><annotation-xml style="display:inline-block" encoding="text/html"><span>α</span><svg width="60" height="24"><rect width="60" height="24" fill="#ffff80"/><text x="2" y="16" font-family="DejaVu Sans" font-size="12">Scene</text></svg><math><mn>2</mn></math><span>β</span></annotation-xml><mn>3</mn></math>
    """

    assert {:ok, box} = measured(html)
    assert box.actual_text == "α2β3"
    assert :ok == MathLayoutValidator.validate_measured_geometry(box)
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, ~r/\s/u, "") == "α2β3"
  end

  test "unselected mixed trees do not measure unavailable math, fonts or remote resources" do
    html = """
    <math><semantics><mn>1</mn><annotation-xml encoding="text/html"><math><mtr><mtd>Hidden</mtd></mtr></math><img src="https://hidden.invalid/image.png"><svg width="60" height="24"><rect width="60" height="24" fill="red"/></svg></annotation-xml></semantics></math>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, "1"} = Text.extract(pdf, layout: false)
    refute pdf =~ "hidden.invalid"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<style>annotation-xml {display:inline-block}</style>" <> html)

    assert diagnostic.stage == :layout and diagnostic.source == "mtr"
    assert diagnostic.message =~ "containing mtable"
  end

  test "CSS-constrained annotations retain shared diagnostics and atomic overflow policy" do
    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlToPdf.render("""
             <math><annotation style="display:inline-block;width:900pt;height:24pt">Too wide</annotation></math>
             """)

    assert diagnostic.stage == :layout
    assert diagnostic.source == "math"
    assert diagnostic.message =~ "atomic MathML equation"

    Limits.install(Map.put(Limits.effective(), :max_font_math_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<math><mtext><span>Before<math><mn>23</mn></math></span></mtext></math>"
             )

    assert diagnostic.message =~ "max_font_math_work"
  end

  test "context boundaries depend on original namespaces and token integration" do
    for tag <- ~w(mi mn mo ms mtext annotation annotation-xml),
        do:
          assert(MathLayoutValidator.independent_math_context?([%{namespace: :mathml, tag: tag}]))

    refute MathLayoutValidator.independent_math_context?([%{namespace: :mathml, tag: "mrow"}])
    assert MathLayoutValidator.independent_math_context?([])

    assert MathLayoutValidator.independent_math_context?([
             %{namespace: :html, tag: "span"},
             %{namespace: :mathml, tag: "mrow"}
           ])
  end
end
