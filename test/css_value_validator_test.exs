defmodule NativeElixirPdfUtilities.Validators.CssValueValidatorTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser
  alias NativeElixirPdfUtilities.HtmlToPdf.AssetLoader
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Style
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.CssValueValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "vertical alignment retains keyword and font-relative length authority" do
    context = %{font_size: 12.0, color: {0, 0, 0}, _custom_properties: %{}}

    for {value, expected} <- [{"baseline", :baseline}, {"1pt", 1.0}, {"-.25em", -3.0}] do
      assert {:ok, style} = CssValueValidator.validate_resolved(context, "vertical-align", value)
      assert style.vertical_align == expected
    end

    assert :error = CssValueValidator.validate_resolved(context, "vertical-align", "bananas")
  end

  test "authored values validate without resolving fonts, assets or files" do
    traced = [
      {Font, :load_registry, 1},
      {Font, :resolve, 4},
      {AssetLoader, :resolve, 4},
      {File, :read, 1}
    ]

    Enum.each(traced, fn {module, _, _} -> Code.ensure_loaded!(module) end)
    Enum.each(traced, fn mfa -> assert :erlang.trace_pattern(mfa, true, [:local]) == 1 end)

    task =
      Task.async(fn ->
        receive do
          :validate ->
            assert :ok = CssValueValidator.validate_declarations("")

            assert :ok =
                     CssValueValidator.validate_declarations(
                       "font-family: Missing; background-image:url('/missing.png'); display:grid; grid-template-columns:repeat(2, 1fr); border:1pt solid currentColor"
                     )

            assert {:ok, _prepared} =
                     CssParser.prepare_stylesheet(
                       "@font-face{font-family: Missing;src:url('/missing.ttf')}p{font-family:Missing;background-image:url('/missing.png')}"
                     )

            assert {:ok, _prepared} = CssParser.prepare_stylesheet("")

            context = %{
              color: {0, 0, 0},
              font_size: 12.0,
              _root_font_size: 12.0,
              _custom_properties: %{"--family" => "Missing"}
            }

            budget = HtmlValidator.new_css_budget()

            assert {:ok, inherited} =
                     CssValueValidator.validate_declarations_in_context(
                       "font-family:var(--family);--image:url('/missing.png')",
                       context,
                       budget
                     )

            assert {:ok, _} =
                     CssValueValidator.validate_cascade(
                       inherited,
                       [{"background-image", "var(--image)"}],
                       budget
                     )

            :ok
        end
      end)

    try do
      :erlang.trace(task.pid, true, [:call])
      send(task.pid, :validate)
      assert :ok = Task.await(task)
      refute_receive {:trace, _, :call, _}
    after
      Enum.each(traced, &:erlang.trace_pattern(&1, false, [:local]))
    end
  end

  test "strict value failures retain authored declarations and locations" do
    for {source, snippet, line, column} <- [
          {"color: red;\n  width : bananas", "width : bananas", 2, 3},
          {"padding: -1pt", "padding: -1pt", 1, 1},
          {"not-supported: x", "not-supported: x", 1, 1}
        ] do
      result = CssValueValidator.validate_declarations(source)

      assert {:error, {:invalid_css, diagnostic}} = result
      assert diagnostic.stage == :css
      assert diagnostic.reason == :invalid_css
      assert diagnostic.module == CssValueValidator
      assert diagnostic.source == snippet
      assert diagnostic.line == line
      assert diagnostic.column == column
      assert diagnostic.message =~ "invalid or unsupported"
    end

    assert {:error, {:invalid_css, _}} = CssValueValidator.validate_declarations(nil)
    assert {:error, {:invalid_css, _}} = CssParser.prepare_stylesheet(<<255>>)

    assert {:error, {:invalid_css, _}} =
             CssValueValidator.validate_declarations("color:red!important!important")

    assert {:error, {:invalid_css, _}} =
             CssParser.prepare_stylesheet("@import 'unused.css';")
  end

  test "CSS variables resolve with the responsible shared rules before value validation" do
    assert :ok =
             CssValueValidator.validate_declarations(
               "--Width:2pt!important;--width:3pt;width:var(--Width);color:blue"
             )

    assert {:ok, %{width: 2.0}} =
             CssValueValidator.validate_resolved(
               %{color: {0, 0, 0}, font_size: 12.0},
               "width",
               "2pt"
             )

    assert :ok =
             CssValueValidator.validate_declarations(
               "--size:2pt!important;--size:bananas;width:var(--size)"
             )

    assert {:error, {:invalid_css, diagnostic}} =
             CssValueValidator.validate_declarations("--size:bananas;width:var(--size)")

    assert diagnostic.source == "width:var(--size)"

    assert {:error, {:invalid_css, _}} =
             CssValueValidator.validate_declarations("width:var(--missing)")

    assert {:error, {:invalid_css, _}} =
             CssValueValidator.validate_declarations("--a:var(--b);--b:var(--a);width:var(--a)")
  end

  test "active URL values diagnose through existing HTML CSS URL policy" do
    for source <- [
          "background-image:url('javascript:run()')",
          "background-image:url('j\\61vascript:run()')"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               CssValueValidator.validate_declarations(source)

      assert diagnostic.stage == :css
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.source == "background-image"
      assert diagnostic.message =~ "Active URL"
    end
  end

  test "value and source complexity use unchanged shared Limits and cumulative reservations" do
    Limits.install(Map.put(Limits.effective(), :max_layout_cardinality, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssValueValidator.validate_declarations("grid-template-columns:repeat(2, 1fr)")

    assert diagnostic.message =~ "grid"

    Limits.install(Map.put(Limits.effective(), :max_css_variable_bytes, 2))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssValueValidator.validate_declarations("--unused:abc")

    assert diagnostic.message =~ "max_css_variable_bytes"

    Limits.install(Map.put(Limits.effective(), :max_css_variable_bytes, 100))
    Limits.install(Map.put(Limits.effective(), :max_css_variable_total_bytes, 5))
    budget = HtmlValidator.new_css_budget()
    assert :ok = CssValueValidator.validate_declarations("--x:abc", budget)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             CssValueValidator.validate_declarations("--x:abc", budget)

    assert diagnostic.message =~ "max_css_variable_total_bytes"

    Limits.install(Map.put(Limits.effective(), :max_css_variable_total_bytes, 1_000))
    Limits.install(Map.put(Limits.effective(), :max_aggregate_css_source_bytes, 20))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               assert :ok = CssValueValidator.validate_declarations("color:red")
               assert :ok = CssValueValidator.validate_declarations("color:red")
               CssValueValidator.validate_declarations("color:red")
             end)

    assert diagnostic.message =~ "max_aggregate_css_source_bytes"
  end

  test "normalized raw declarations share custom, variable and malformed-input policy" do
    context = %{color: {0, 0, 0}, font_size: 12.0, _custom_properties: %{"--size" => "2pt"}}
    budget = HtmlValidator.new_css_budget()

    assert {:ok, %{width: 2.0}} =
             CssValueValidator.validate_declaration(
               context,
               {"width", "var(--size)", :important},
               budget
             )

    assert {:ok, %{_custom_properties: properties}} =
             CssValueValidator.validate_declaration(
               context,
               {"--other", "var(--missing)"},
               budget
             )

    assert properties["--other"] == "var(--missing)"
    assert properties["--size"] == "2pt"

    for declaration <- [
          nil,
          {},
          {"width", 2},
          {1, "2pt"},
          {"width", "2pt", :bogus},
          {"width", "bananas"},
          {"unknown", "value"},
          {"width", "var(--missing)"}
        ] do
      assert {:error, :invalid_document} =
               CssValueValidator.validate_declaration(context, declaration, budget)
    end

    refute CssValueValidator.custom_property_declaration?(nil)
  end

  test "valid raw custom declarations do not conceal the failing public CSS source" do
    for {html, source, line, column} <- [
          {"<p style='--x:2pt;width:bananas'>A</p>", "width:bananas", 1, 9},
          {"<p style='--x:var(--missing);width:bananas'>A</p>", "width:bananas", 1, 20},
          {"<style>:root{--x:2pt}\np{width:bananas}</style><p>A</p>", "width:bananas", 2, 3},
          {"<p style='--x:2pt;broken'>A</p>", "broken", 1, 9}
        ] do
      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :css
      assert diagnostic.reason == :invalid_css
      assert diagnostic.operation == :render
      assert diagnostic.module == HtmlToPdf
      assert diagnostic.source == source
      assert diagnostic.line == line
      assert diagnostic.column == column
    end
  end

  test "contextual blocks retain inherited variables and root state for dependent values" do
    budget = HtmlValidator.new_css_budget()

    base = %{
      color: {0, 0, 0},
      font_size: 12.0,
      _root_font_size: 12.0,
      _root_element: true,
      _custom_properties: %{}
    }

    assert {:ok, root} =
             CssValueValidator.validate_declarations_in_context(
               "--tone:red;--size:2rem;font-size:2rem",
               base,
               budget
             )

    assert root.font_size == 24.0
    assert root._root_font_size == 24.0

    inherited = Map.put(root, :_root_element, false)

    assert {:ok, child} =
             CssValueValidator.validate_declarations_in_context(
               "letter-spacing:0.5em;color:var(--tone);border:1pt solid currentColor;font-size:var(--size)",
               inherited,
               budget
             )

    assert child.font_size == 48.0
    assert child._root_font_size == 24.0
    assert child.letter_spacing == 24.0
    assert child.color == {1.0, 0.0, 0.0}
    assert child.border_color == child.color
    assert child._custom_properties == root._custom_properties

    assert {:ok, overridden} =
             CssValueValidator.validate_declarations_in_context(
               "--tone:blue!important;--tone:red;color:var(--tone)",
               child,
               budget
             )

    assert overridden.color == {0.0, 0.0, 1.0}
    assert overridden._custom_properties["--size"] == "2rem"

    assert :ok =
             CssValueValidator.validate_declarations(
               "letter-spacing:0.5em;border:1pt solid currentColor;color:red;font-size:24pt"
             )
  end

  test "cascade consumer sees validated declarations in existing Style execution order" do
    base = %{color: {0, 0, 0}, font_size: 12.0, _root_font_size: 12.0, _custom_properties: %{}}

    declarations = [
      {"font-family", "Missing"},
      {"letter-spacing", "0.5em"},
      {"font-size", "24pt"},
      {"--tone", "blue"},
      {"color", "var(--tone)"}
    ]

    consumer = fn declaration, validated ->
      send(self(), {:consumed, elem(declaration, 0), validated})
      {:ok, Map.put(validated, :consumer_state, :retained)}
    end

    assert {:ok, result} =
             CssValueValidator.validate_cascade(
               base,
               declarations,
               HtmlValidator.new_css_budget(),
               consumer
             )

    assert result.consumer_state == :retained
    assert result.letter_spacing == 12.0

    for property <- ["--tone", "font-size", "color", "font-family", "letter-spacing"] do
      assert_receive {:consumed, ^property, validated}
      if property == "font-family", do: assert(validated.font_families == "Missing")
    end

    refute_receive {:consumed, _, _}

    for declaration <- [{"font-family", "var(--missing)"}, nil, {"font-size", "bananas"}] do
      assert {:error, :invalid_document} =
               CssValueValidator.validate_cascade(
                 base,
                 [declaration],
                 HtmlValidator.new_css_budget(),
                 consumer
               )

      refute_receive {:consumed, _, _}
    end

    assert {:error, {:invalid_options, diagnostic}} =
             CssValueValidator.validate_cascade(
               base,
               [],
               HtmlValidator.new_css_budget(),
               fn _ -> :ok end
             )

    assert diagnostic.stage == :css
    assert diagnostic.source == "consumer"
    assert diagnostic.message =~ "validated declaration"

    expected = Diagnostics.error(:css, :invalid_css, "consumer failed", source: "font-family")

    assert ^expected =
             CssValueValidator.validate_cascade(
               base,
               [{"font-family", "Missing"}],
               HtmlValidator.new_css_budget(),
               fn _, _ -> expected end
             )
  end

  test "contextual diagnostics blame the actual invalid source after inherited resolution" do
    inherited = %{
      color: {0, 0, 0},
      font_size: 12.0,
      _root_font_size: 12.0,
      _custom_properties: %{"--size" => "bananas"}
    }

    assert {:error, {:invalid_css, diagnostic}} =
             CssValueValidator.validate_declarations_in_context(
               "--valid:2pt;\n  width:var(--size)",
               inherited,
               HtmlValidator.new_css_budget()
             )

    assert diagnostic.source == "width:var(--size)"
    assert diagnostic.line == 2
    assert diagnostic.column == 3
    assert diagnostic.stage == :css
    assert diagnostic.module == CssValueValidator

    assert {:error, {:unsupported_html, diagnostic}} =
             CssValueValidator.validate_declarations_in_context(
               "background-image:url('javascript:run()')",
               inherited,
               HtmlValidator.new_css_budget()
             )

    assert diagnostic.source == "background-image"

    assert {:error, {:invalid_css, _}} =
             CssValueValidator.validate_declarations_in_context(
               nil,
               inherited,
               HtmlValidator.new_css_budget()
             )
  end

  test "full Style uses actual inherited cross-rule and inline cascade context" do
    html =
      "<html><head><style>:root{--size:18pt;--tone:blue;font-size:2rem}body{--size:24pt}p{letter-spacing:0.5em;color:var(--tone);font-size:var(--size)}</style></head><body><p style='--tone:red!important;--tone:blue'>A</p></body></html>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [%{tag: "body", children: [%{tag: "p", style: style}]}] = styled.children
    assert style.font_size == 24.0
    assert style.letter_spacing == 12.0
    assert style.color == {1.0, 0.0, 0.0}
    assert style._root_font_size == 24.0
    assert {:ok, <<"%PDF", _::binary>>} = HtmlToPdf.render(html)
  end

  test "ordinary CSS failure authority remains intact when renderer-owned replacement succeeds" do
    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render("<p style='width:bananas'>A</p>")

    assert diagnostic.module == HtmlToPdf
    assert diagnostic.operation == :render
    assert diagnostic.source == "width:bananas"

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<body><?marker name='a'?><template for='a'><p>B</p></template></body>"
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
    assert text =~ "B"

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_document_detailed(
               "<body><?marker name='a'?><template for='a'><p>B</p></template></body>"
             )

    assert diagnostic.operation == :parse and diagnostic.source == "<template for='a'>"
  end
end
