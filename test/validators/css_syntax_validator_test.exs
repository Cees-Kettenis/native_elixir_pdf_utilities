defmodule NativeElixirPdfUtilities.Validators.CssSyntaxValidatorTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, HtmlParser, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssSyntaxValidator,
    HtmlCssSourceValidator,
    HtmlValidator
  }

  alias NativeElixirPdfUtilities.Limits

  # CSS Syntax 3, 1 October 2026, sections 4.3 and 5.5. These authored
  # expectations do not use the production parser to construct a reference.
  test "component spans preserve UTF-8, quoted delimiters, escaped punctuation and nesting" do
    source = ~S|é(foo('a;)'),[x=","],\)) /* ; */ ;|
    values = CssSyntaxValidator.components(source)

    assert [
             %{kind: :block, opener: "é(", closed: true, children: children},
             %{kind: :token},
             %{kind: :comment},
             %{kind: :token},
             %{text: ";"}
           ] = values

    assert Enum.any?(children, &match?(%{kind: :block, opener: "["}, &1))
    for value <- values, do: assert(binary_part(source, value.offset, value.length) == value.text)

    assert CssSyntaxValidator.split(~S|a,[x=","],:is(.a,.b)|, ",") == [
             "a",
             ~S|[x=","]|,
             ":is(.a,.b)"
           ]

    assert CssSyntaxValidator.function(~S|:is([x=")"],.a):not(.b)|, "is") ==
             {:ok, ~S|[x=")"],.a|, ":not(.b)"}

    assert CssSyntaxValidator.function(":not(.a)", "is") == nil
    assert CssSyntaxValidator.function(":is", "is") == :error
    assert CssSyntaxValidator.function(":is(", "is") == :error
    assert CssSyntaxValidator.function("p", "is") == nil
    assert CssSyntaxValidator.function(":1", "is") == nil
  end

  test "malformed declarations recover without losing following declarations or source ownership" do
    css =
      "/* é */p{color:red;broken; padding:2pt;content:\"bad\n; margin:3pt; background-image:url(bad\"x);width:4pt}"

    assert {:ok, ordinary} = CssParser.prepare_stylesheet(css)

    assert {:ok, source} =
             CssParser.prepare_source_stylesheet(css, HtmlCssSourceValidator.local(css))

    expected = [{"color", "red"}, {"padding", "2pt"}, {"margin", "3pt"}, {"width", "4pt"}]
    assert hd(ordinary.rules).declarations == expected
    assert hd(source.rules).declarations == expected

    for record <- hd(source.rules).source_declarations do
      assert binary_part(css, record.source.offset, record.source.length) == record.source.text
    end

    assert Enum.at(hd(source.rules).source_declarations, 2).source.line == 2
  end

  test "balanced unknown at-rules and excluded imports cannot select CSS or fetch resources" do
    css =
      "@charset 'UTF-8';@import url('https://invalid.example/style.css');@future url('asset:blocked'){p{color:red}}p{color:blue}"

    assert {:ok, [rule]} = CssParser.parse(css)
    assert rule.declarations == [{"color", "blue"}]
    assert {:ok, dom} = HtmlParser.parse_detailed("<p>Visible</p>")

    assert {:ok, tree} =
             Style.compute_detailed(dom,
               stylesheets: [css: css],
               asset_resolver: fn request -> flunk("ignored CSS fetched #{inspect(request)}") end
             )

    assert hd(tree.children).style.color == {0, 0, 1}

    assert {:ok, [first, last]} =
             CssParser.parse("<!-- p{color:red} --> @future x; p{color:blue}")

    assert first.declarations == [{"color", "red"}]
    assert last.declarations == [{"color", "blue"}]
  end

  test "valid unsupported resources retain their original failure and acquire no permission" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<p>Visible</p>")

    assert {:error, {:unsupported_html, diagnostic}} =
             Style.compute_detailed(dom,
               stylesheets: [
                 css: "p{broken;background-image:url('javascript:alert(1)');color:red}"
               ]
             )

    assert diagnostic.stage == :css
    assert diagnostic.source =~ "background-image"
  end

  test "EOF closes component blocks while bad strings stop at a newline" do
    assert [%{kind: :block, closed: false, children: [%{kind: :block, closed: false}]}] =
             CssSyntaxValidator.components("f([")

    assert [%{kind: :bad_string}, %{text: "\n"}, %{text: "x"}] =
             CssSyntaxValidator.components("\"bad\nx")

    assert [%{kind: :string}] = CssSyntaxValidator.components("\"EOF")

    assert CssSyntaxValidator.declarations("color:red; broken:);width:4pt")
           |> Enum.map(& &1.declaration) == [{"color", "red"}, {"width", "4pt"}]

    assert {:ok, [rule]} = CssParser.parse("p{color:red")
    assert rule.declarations == [{"color", "red"}]
  end

  test "component nesting is bounded independently of CSS byte length" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(Map.put(limits, :max_depth, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               CssSyntaxValidator.components("a(b(c()))")
             end)

    assert diagnostic.message =~ "max_depth"
  end

  test "source declaration limits retain validator diagnostics before a source context is built" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, dom} = HtmlParser.parse_document_detailed("<p style='color:red'>Text</p>")
    Limits.install(Map.put(limits, :max_input_bytes, 1))
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlCssSourceValidator.prepare(dom)
    assert diagnostic.message =~ "max_input_bytes"
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Style.compute_detailed(dom)
    assert diagnostic.message =~ "max_input_bytes"
  end
end
