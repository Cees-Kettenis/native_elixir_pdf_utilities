defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutDisplayContentsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "every Core namespace element computes contents to none before schema layout" do
    for tag <-
          ~w(annotation annotation-xml maction math merror mfrac mi mn mmultiscripts mo mover mpadded mphantom mprescripts mroot mrow ms mspace msqrt mstyle msub msubsup msup mtable mtd mtext mtr munder munderover semantics equation-data) do
      math =
        if tag == "math",
          do: "<math style='display:contents'>Forbidden</math>",
          else: "<math><#{tag} style='display:contents'>Forbidden</#{tag}><mn>7</mn></math>"

      assert {:ok, pdf} =
               HtmlToPdf.render("<p>Before#{math}After</p>", system_font_discovery: false)

      assert {:ok, text} = Text.extract(pdf, layout: false)
      refute text =~ "Forbidden"
      assert text =~ "Before" and text =~ "After"
      if tag != "math", do: assert(text =~ "7")
    end
  end

  test "computed-none operand changes the real fraction schema to row fallback" do
    html = "<math><mfrac><mn style='display:contents'>Hidden</mn><mn>2</mn></mfrac></math>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, system_font_discovery: false)
    assert node.style._math_box.actual_text == "2"
    assert length(node.style._math_box.glyphs) == 1
    assert Enum.all?(node.style._math_box.drawing_items, &(Map.get(&1, :kind) == :glyph))
  end

  test "semantic initial selection does not fall back when first child's contents computes none" do
    html =
      "<math><semantics><mrow style='display:contents'><mn>First</mn></mrow><annotation>Alternate</annotation></semantics></math>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:error, {:no_extractable_text, diagnostic}} = Text.extract(pdf, layout: false)
    assert diagnostic.stage == :text_extraction and diagnostic.operation == :extract
  end

  test "HTML integration children retain their namespace and ordinary contents unboxing" do
    html =
      "<math><mtext><span style='display:contents'><b>VisibleHTML</b></span></mtext><mn>7</mn></math>"

    assert {:ok, %{children: [math]}} = HtmlParser.parse_detailed(html)
    [mtext, _] = math.children
    [span] = mtext.children
    assert span.namespace == :html and mtext.namespace == :mathml
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "VisibleHTML7"} = Text.extract(pdf, layout: false)
  end

  test "none normalization follows the actual author cascade and does not suppress math overrides" do
    html =
      "<style>math{display:contents}math#shown{display:inline math!important}</style><math id='shown'><mn>7</mn></math><math><mn>Hidden</mn></math>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "7"} = Text.extract(pdf, layout: false)
  end

  test "contents suppresses hidden background fetch and inactive unsupported glyph layout" do
    html =
      "<math style='display:contents;background-image:url(missing-math-contents.svg)'><mtext>אב</mtext></math><p>Visible</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "Visible"} = Text.extract(pdf, layout: false)
    refute pdf =~ "missing-math-contents"
  end

  test "hidden Math content still consumes the ordinary document structure budget" do
    Limits.install(%{Limits.effective() | max_html_nodes: 2})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<math style='display:contents'><mn>1</mn><mn>2</mn></math>")

    assert is_atom(diagnostic.stage) and is_atom(diagnostic.operation)
    assert diagnostic.message =~ "max_html_nodes"
  end
end
