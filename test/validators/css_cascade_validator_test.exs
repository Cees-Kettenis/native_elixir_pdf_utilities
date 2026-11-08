defmodule NativeElixirPdfUtilities.Validators.CssCascadeValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  alias NativeElixirPdfUtilities.Validators.{
    CssCascadeValidator,
    CssValueValidator,
    HtmlValidator,
    CssSyntaxValidator,
    CssPropertyRegistrationValidator,
    HtmlQuoteValidator,
    SvgStyleValidator,
    HtmlSelectedContentValidator,
    PrintMediaValidator
  }

  test "important layer rollback retains only normal declarations before the reverted layer" do
    nodes =
      CssStyles.by_id("<p id='a'>Text</p>", [
        """
        @layer first,middle,last;
        @layer first {#a{color:blue}}
        @layer middle {#a{color:revert-layer!important}}
        @layer last {#a{color:red}}
        #a{color:green}
        """
      ])

    assert nodes["a"].color == {0, 0, 1}
  end

  test "standalone property validators retain their documented default and inheritance contexts" do
    assert :font_size in CssCascadeValidator.inherited_fields()
    assert CssCascadeValidator.keyword(" INHERIT ") == "inherit"
    assert CssCascadeValidator.keyword(~S(i\6e herit)) == "inherit"
    assert CssCascadeValidator.keyword(~S(i\6e herit extra)) == nil
    assert "font-size" in CssCascadeValidator.targets("font")
    assert CssCascadeValidator.initial().font_size == 12.0
    assert {:ok, %{width: :auto}} = CssCascadeValidator.wide(%{}, "width", "inherit")

    assert {:ok, inherited_margin} = CssCascadeValidator.wide(%{}, "margin-left", "inherit")
    assert inherited_margin.margin.left == 0.0

    assert "fill" in CssCascadeValidator.targets("all", %{_svg_element: true})
    assert CssCascadeValidator.project(%{foreign: 1}, %{foreign: 2}, ["foreign"]) == %{foreign: 2}

    assert CssValueValidator.validate_declaration(
             %{_custom_properties: %{}},
             {"width", "red)"},
             HtmlValidator.new_css_budget()
           ) == {:error, :invalid_document}

    assert {:error, :invalid_document} =
             CssValueValidator.validate_cascade(
               %{_custom_properties: %{}},
               [{"--bad", nil}],
               HtmlValidator.new_css_budget()
             )

    assert {:error, :invalid_document} =
             CssValueValidator.validate_declaration(
               %{_custom_properties: %{"--bad" => "bad"}},
               {"border-style", "var(--bad)"},
               HtmlValidator.new_css_budget()
             )

    for keyword <- ["inherit", "unset", "initial"] do
      expected = if keyword == "initial", do: {0, 0, 0}, else: {0, 0, 1}
      assert {:ok, ^expected} = HtmlValidator.parse_css_color_declaration(keyword, {0, 0, 1})
    end

    for keyword <- ["inherit", "unset", "revert", "revert-layer"] do
      assert {:ok, [{"[", "]"}]} =
               HtmlQuoteValidator.parse(keyword, %{_inherited_quotes: [{"[", "]"}]})
    end

    svg = SvgStyleValidator.defaults(%{})
    assert {:ok, reset} = SvgStyleValidator.apply(svg, "fill", "initial")
    assert reset.svg_fill == svg.svg_fill
    assert {:ok, reset} = SvgStyleValidator.apply(svg, "fill", "unset")
    assert reset.svg_fill == svg.svg_fill

    assert HtmlSelectedContentValidator.prepare_select_defaults(
             %{font_size: 12.0, _select_inherited_style: %{font_size: 12.0}},
             [
               {"color", "blue"}
             ]
           ) == %{font_size: 12.0}

    assert [%{block: false, closed: false}] = CssSyntaxValidator.rules("@future")
    assert [%{kind: :declaration}] = CssSyntaxValidator.block_items("color:red")
    assert PrintMediaValidator.cache_context("@!bad{p{color:red}}", :print) == :print
  end

  test "computed font shorthand failures remain recoverable" do
    base = %{font_size: 12.0, color: {0, 0, 0}, _custom_properties: %{}}

    for value <- ["bold", "12pt/", "italic italic 12pt serif", "heavy 12pt serif"] do
      assert {:error, :invalid_document} =
               CssValueValidator.validate_declaration(
                 base,
                 {"font", value},
                 HtmlValidator.new_css_budget()
               )
    end
  end

  test "typed expansion propagates unavailable metrics and callback diagnostics without replacing them" do
    {:ok, registrations} =
      CssPropertyRegistrationValidator.prepare([
        %{name: "--size", declarations: "syntax:'<length>';inherits:false;initial-value:1pt"}
      ])

    context = %{
      font_size: 12.0,
      _root_font_size: 12.0,
      color: {0, 0, 0},
      _root_line_height: 14.4,
      _custom_properties: %{},
      _property_registrations: registrations
    }

    assert {:error, {:not_implemented, diagnostic}} =
             CssValueValidator.validate_cascade(
               context,
               [{"--size", "2ch"}],
               HtmlValidator.new_css_budget()
             )

    assert diagnostic.message =~ "selected font metrics"

    expected = Diagnostics.error(:css, :invalid_css, "typed conversion failed", source: "--value")

    for properties <- [%{"--value" => nil}, %{"--value" => "var(--value)"}] do
      assert ^expected =
               HtmlValidator.compute_custom_properties(
                 properties,
                 HtmlValidator.new_css_budget(),
                 fn _, _ -> expected end
               )
    end
  end

  test "computed source validation and supports preserve shared resource failures" do
    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)
    NativeElixirPdfUtilities.Limits.install(%{limits | max_css_work: 25})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssValueValidator.validate_declaration(
               %{_custom_properties: %{"--x" => "bananas"}},
               {"width", "var(--x)"},
               HtmlValidator.new_css_budget()
             )

    assert diagnostic.message =~ "max_css_work"

    assert {:render_resource_limit, {:error, {:resource_limit_exceeded, diagnostic}}} =
             catch_throw(
               NativeElixirPdfUtilities.Validators.CssSupportsValidator.matches?(
                 "(width:1234567890pt)"
               )
             )

    assert diagnostic.message =~ "max_css_work"
  end
end
