defmodule NativeElixirPdfUtilities.HtmlToPdf.HtmlRecoveryTest do
  use ExUnit.Case, async: false

  import NativeElixirPdfUtilities.HtmlTreeHelpers

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.HtmlRecoveryValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "default parsing diagnoses omitted paragraph list and container ends" do
    for {html, tag} <- [
          {"<p>abc", "p"},
          {"<div>abc<div>def</div>", "div"},
          {"<ul><li>abc", "li"},
          {"<table><tr><td>abc", "td"},
          {"<html><head></head><body>abc", "body"}
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed(html)
      assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
      assert diagnostic.message =~ "missing closing </#{tag}>"
      assert diagnostic.message =~ "html_recovery: true"
      assert diagnostic.source =~ "<#{tag}>"
      assert diagnostic.line == 1 and diagnostic.column > 0

      assert {:error, {:unsupported_html, ^diagnostic}} =
               HtmlParser.parse_detailed(html, html_recovery: false)

      assert {:error, {:unsupported_html, public}} = HtmlToPdf.render(html)
      assert public.operation == :render and public.module == diagnostic.module
      assert public.source == diagnostic.source and public.message == diagnostic.message
    end

    assert {:error, :unsupported_html} = HtmlParser.parse("<p>abc<p>def")
    assert {:ok, _} = HtmlParser.parse_document_detailed("<p>Explicit</p>")
  end

  test "recovery retains browser nesting for the original unclosed div examples" do
    for {html, explicit} <- [
          {"<div>abc<div>def</div>", "<div>abc<div>def</div></div>"},
          {"<div>abc<div/><div>def</div>", "<div>abc<div><div>def</div></div></div>"},
          {"<p>abc<p>def", "<p>abc</p><p>def</p>"},
          {"<ul><li>A<li>B</ul>", "<ul><li>A</li><li>B</li></ul>"},
          {"<table><tr><td>A<td>B</table>", "<table><tr><td>A</td><td>B</td></tr></table>"},
          {"<div><span>abc", "<div><span>abc</span></div>"}
        ] do
      expected = tree_content(HtmlParser.parse(explicit))
      assert tree_content(HtmlParser.parse(html, html_recovery: true)) == expected

      assert tree_content(HtmlParser.parse_document_detailed(html, html_recovery: true)) ==
               tree_content(HtmlParser.parse_document_detailed(explicit))
    end

    assert {:ok, %{children: [outer]}} =
             HtmlParser.parse_detailed("<div>abc<div/><div>def</div>", html_recovery: true)

    [_, middle] = outer.children
    [inner] = middle.children
    assert middle.source == %{text: "<div/>", line: 1, column: 9}
    assert middle.parent_id == outer.node_id and inner.parent_id == middle.node_id
    assert length(Enum.uniq([outer.node_id, middle.node_id, inner.node_id])) == 3
  end

  test "strict self-closing diagnostics preserve void SVG and MathML syntax" do
    for tag <- ~w(div span p style x-widget) do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_detailed("<#{tag}/>")
      assert diagnostic.stage == :html and diagnostic.source == "<#{tag}/>"
      assert diagnostic.message =~ "cannot self-close in HTML"
      assert diagnostic.message =~ "html_recovery: true"
    end

    for opts <- [[], [html_recovery: true]],
        html <- ["<br/>", "<svg><rect/></svg>", "<math><mi/></math>"] do
      assert {:ok, _} = HtmlParser.parse_detailed(html, opts)
    end

    for html <- ["<svg><g>", "<svg><g><rect/>", "<svg><text>Literal", "<math><mrow>"] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_detailed(html, html_recovery: true)

      assert diagnostic.message =~ "foreign content"
      assert diagnostic.message =~ "close it explicitly"
    end
  end

  test "HTML plaintext inside an inert foreign integration template retains its literal tail" do
    html =
      "<svg><foreignObject><template><div><plaintext>Literal</plaintext></div></template></foreignObject></svg>"

    assert {:error, {:unsupported_html, _}} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [svg]}} = HtmlParser.parse_detailed(html, html_recovery: true)
    [foreign] = svg.children
    [template] = foreign.children
    [div] = template.content.children
    [plaintext] = div.children
    assert hd(plaintext.children).text =~ "Literal</plaintext></div></template>"
    assert template.children == []
  end

  test "recovery preserves style raw text and source binding through later styling" do
    html = "<style/>p{color:red}</style><p>Visible"
    assert {:ok, dom} = HtmlParser.parse_detailed(html, html_recovery: true)
    [style, paragraph] = dom.children
    assert style.tag == "style" and hd(style.children).text == "p{color:red}"
    assert paragraph.source.text == "<p>"
    assert {:ok, styled} = Style.compute_detailed(dom, html_recovery: true)
    assert List.last(styled.children).style.color == {1.0, 0.0, 0.0}
    assert {:ok, pdf} = HtmlToPdf.render(html, html_recovery: true)
    assert {:ok, "Visible"} = Text.extract(pdf)
  end

  test "fragment APIs share the explicit opt-in and keep source coordinates" do
    html = "<div>abc<div/><div>def</div>"

    for parse <- [&HtmlParser.parse_fragment/3, &HtmlParser.parse_fragment_detailed/3] do
      assert {:error, {:unsupported_html, _}} = parse.(html, "section", [])
      assert {:ok, %{children: [outer]}} = parse.(html, "section", html_recovery: true)
      assert outer.source == %{text: "<div>", line: 1, column: 1}
    end

    assert {:ok, %{type: :document_fragment, children: [_]}} =
             HtmlParser.parse_fragment_tree_detailed(html, "section", html_recovery: true)

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlParser.parse_detailed("<div>first\n  <span>Second")

    assert diagnostic.source == "<span>" and diagnostic.line == 2 and diagnostic.column == 3
  end

  test "file and child document rendering retain the chosen recovery option" do
    directory = Paths.project("tmp/html_recovery_#{System.unique_integer([:positive])}")
    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    input = Path.join(directory, "source.html")
    output = Path.join(directory, "output.pdf")
    File.write!(input, "<div>File<div/>Nested")

    assert {:error, {:unsupported_html, _}} = HtmlToPdf.render_file(input, output)
    refute File.exists?(output)
    assert :ok = HtmlToPdf.render_file(input, output, html_recovery: true)
    assert {:ok, text} = Text.extract_file(output)
    assert text =~ "File" and text =~ "Nested"

    html = "<iframe srcdoc='<div>Child'></iframe><p>After</p>"
    assert {:error, {:unsupported_html, _}} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, pdf} = HtmlToPdf.render(html, html_recovery: true, forms: :static)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Child" and text =~ "After"
  end

  test "recovery cannot enable scripts event handlers or linked stylesheets" do
    for html <- [
          "<script>alert(1)</script>",
          "<div onclick='alert(1)'>Text</div>",
          "<svg><script/></svg>",
          "<link rel=stylesheet href='https://invalid.test/style.css'><p>Text</p>"
        ] do
      assert {:error, {_reason, diagnostic}} =
               HtmlToPdf.render(html,
                 html_recovery: true,
                 asset_resolver: fn request -> flunk("recovery fetched #{inspect(request)}") end
               )

      assert diagnostic.operation == :render and is_atom(diagnostic.module)
    end
  end

  test "nested recovery scopes restore defaults after success failure and exceptions" do
    refute HtmlRecoveryValidator.enabled?()

    HtmlRecoveryValidator.with_options([html_recovery: true], fn ->
      assert HtmlRecoveryValidator.enabled?()
      assert {:ok, _} = HtmlParser.parse("<p>Inherited")
      assert {:error, :unsupported_html} = HtmlParser.parse("<p>Strict", html_recovery: false)
      assert HtmlRecoveryValidator.enabled?()
    end)

    refute HtmlRecoveryValidator.enabled?()

    assert_raise ArgumentError, fn ->
      HtmlRecoveryValidator.with_options([html_recovery: true], fn -> raise ArgumentError end)
    end

    refute HtmlRecoveryValidator.enabled?()
    assert {:error, _} = HtmlParser.parse("<div><unsupported>", html_recovery: true)
    refute HtmlRecoveryValidator.enabled?()
    assert {:error, :unsupported_html} = HtmlParser.parse("<p>Strict again")
  end

  test "optional-end-tag queries retain their EOF rules only inside recovery scopes" do
    refute HtmlValidator.omitted_html_end_tag?("p", :eof, ["div"])

    HtmlRecoveryValidator.with_options([html_recovery: true], fn ->
      assert HtmlValidator.omitted_html_end_tag?("html", :eof, [])
      assert HtmlValidator.omitted_html_end_tag?("p", :eof, ["div"])
      assert HtmlValidator.omitted_html_end_tag?("li", :eof, ["ul"])
      refute HtmlValidator.omitted_html_end_tag?("p", :eof, ["a"])
      refute HtmlValidator.omitted_html_end_tag?("dt", :eof, ["dl"])
    end)

    refute HtmlValidator.omitted_html_end_tag?("p", :eof, ["div"])
  end

  test "recovery options return shared diagnostics for invalid values shapes and keys" do
    for opts <- [
          %{},
          [unknown: true],
          [html_recovery: true, html_recovery: false],
          [html_recovery: :yes],
          [html_recovery: nil]
        ] do
      assert {:error, {:invalid_options, diagnostic}} = HtmlParser.parse_detailed("", opts)
      assert diagnostic.stage == :options and diagnostic.reason == :invalid_options
      assert diagnostic.message =~ "HTML parsing" or diagnostic.message =~ "html_recovery"
    end

    assert {:error, {:invalid_options, diagnostic}} = HtmlToPdf.render("", html_recovery: :yes)
    assert diagnostic.stage == :options and diagnostic.operation == :render
    assert diagnostic.message =~ "html_recovery must be a boolean"
  end

  test "recovery still enforces nesting budgets before preparing recovered output" do
    Limits.install(Map.put(Limits.effective(), :max_html_depth, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlParser.parse_detailed("<div><div><div>Deep", html_recovery: true)

    assert diagnostic.stage == :html and diagnostic.message =~ "max_html_depth"
    assert {:ok, _} = HtmlParser.parse("<p>Next</p>")
  end
end
