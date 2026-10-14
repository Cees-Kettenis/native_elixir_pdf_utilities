defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateUnappliedSourceTest do
  use ExUnit.Case, async: false
  Code.require_file("../support/png_fixture.ex", __DIR__)
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, TemplateExecutionValidator}

  @opts [system_font_discovery: false, compress_streams: false]

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "unmatched and inactive source types permit real replacement without fetching dormant assets" do
    css =
      ".absent{width:2em;height:25%;padding:calc(1rem + 2pt);gap:2rem 25%;border-width:medium 2em;border-spacing:1em;flex-basis:2em;--x:var(--outer,2rem);width:var(--x);font-family:Missing;background-image:url(missing.png)}@media screen{p{font-size:125%;background-image:url(screen.png)}}"

    html =
      "<style>#{css}</style><body><?marker name='A'><template for='A'><style>.absent{width:var(--scope,2rem);background-image:url(inert.png)}</style><p>Good</p></template></body>"

    resolver = fn reference ->
      send(self(), {:unexpected_dormant_asset, reference})
      {:error, :missing}
    end

    assert {:ok, pdf} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
    assert {:ok, expected} = HtmlToPdf.render("<p>Good</p>", @opts)
    assert pdf == expected
    assert {:ok, "Good"} = Text.extract(pdf)
    refute_received {:unexpected_dormant_asset, _}
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "a matching original Math rule remains pending until its genuine runtime context" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<style>mspace{width:2em}.absent{width:2em}</style><math><mspace></mspace></math>"
             )

    assert {:ok, result} =
             Style.with_source_style_contexts(
               dom,
               [],
               HtmlValidator.new_css_budget(),
               fn _, _, _ -> :ok end,
               @opts
             )

    [matched, absent] = result.source_obligations.declaration_contexts
    assert matched.status == :unresolved
    refute Map.has_key?(matched, :source_value)
    assert absent.status == :checked_symbolically and absent.contexts == []
    assert absent.source_value.length.geometry.units == ["em"]
    assert is_reference(absent.source.owner.node_id)
  end

  test "actual composed clone cascade replaces provisional unapplied source selection" do
    html =
      "<style>button span{width:2em}</style><body><select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option selected><span></span></option></select><?marker name='A'><template for='A'><img src='proof.png'></template></body>"

    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    resolver = fn _ ->
      frame = Process.get({TemplateExecutionValidator, :execution})

      if frame.mode == :original do
        record = Enum.find(frame.declaration_contexts, &(&1.source.text == "width:2em"))
        assert record.status == :checked_in_original_context

        assert Enum.any?(
                 record.contexts,
                 &(is_map(&1[:clone_origin]) and &1.execution == :original_runtime)
               )

        refute Map.has_key?(record, :selection)
        send(self(), :verified_composed_source_selection)
      end

      {:ok, png}
    end

    assert {:ok, _} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
    assert_received :verified_composed_source_selection
  end

  test "source syntax, dimensional type and executable URL failures survive inactive selection" do
    for declaration <- [
          "width:2bananas",
          "width:calc(2pt + 1deg)",
          "width:var(not-custom,2pt)",
          "--x:var(not-custom)",
          "padding:1em 2em 3em 4em 5em",
          "border-width:25%",
          "border-spacing:25%"
        ] do
      html =
        "<style>@media screen{.absent{#{declaration}}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
      assert diagnostic.stage == :css
      assert diagnostic.source == declaration
      assert diagnostic.line == 1 and is_integer(diagnostic.column)
    end

    html =
      "<style>.absent{background-image:var(--bg,url(javascript:alert(1)))}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

    assert {:error, {_reason, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :css
    assert diagnostic.source =~ "javascript:"
  end

  test "prospective application cannot borrow the symbolic declaration's missing variable or asset authority" do
    for declaration <- ["width:var(--missing)", "background-image:url(missing.png)"] do
      html =
        "<style>.selected{#{declaration}}</style><body><?marker name='A'><template for='A'><p class='selected'>Good</p></template></body>"

      assert {:error, {_reason, diagnostic}} = HtmlToPdf.render(html, @opts)
      assert diagnostic.stage in [:css, :asset]
      assert diagnostic.source in [declaration, "missing.png"]
    end
  end

  test "an applied original resource is still resolved before its content can be removed" do
    html =
      "<style>.selected{background-image:url(original.png)}</style><body><?start name='A'><p class='selected'>Old</p><?end?><template for='A'><p>Good</p></template></body>"

    assert {:error, {_reason, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :asset and diagnostic.source == "original.png"
  end

  test "shared variable grammar computes fallback tokens and preserves quoted values and cycles" do
    budget = HtmlValidator.new_css_budget()

    assert {:ok, "calc(2em + 1pt)"} =
             HtmlValidator.resolve_css_variables(
               "calc(var(--missing,var(--size,3em)) + 1pt)",
               %{"--size" => "2em"},
               %{},
               budget
             )

    assert {:ok, "\"var(--missing)\""} =
             HtmlValidator.resolve_css_variables("\"var(--missing)\"", %{}, %{}, budget)

    assert {:ok, ""} = HtmlValidator.resolve_css_variables("var(--missing,)", %{}, %{}, budget)
    assert :error = HtmlValidator.resolve_css_variables("var(not-custom,2pt)", %{}, %{}, budget)

    assert {:ok, properties} =
             HtmlValidator.compute_custom_properties(
               %{"--a" => "var(--b,var(--a))", "--b" => "2pt", "--c" => "var(--a,3pt)"},
               budget
             )

    assert properties == %{"--a" => nil, "--b" => "2pt", "--c" => "3pt"}

    assert {:ok, properties} =
             HtmlValidator.compute_custom_properties(
               %{"--a" => "var(--b,2pt)", "--b" => "var(--a)", "--c" => "var(--a,3pt)"},
               budget
             )

    assert properties == %{"--a" => nil, "--b" => nil, "--c" => "3pt"}
  end

  test "applied fallback declarations execute using their actual inherited font and percentage basis" do
    html =
      "<body style='font-size:20pt'><?marker name='A'><template for='A'><div style='width:100pt'><p style='width:var(--missing,2em);height:var(--height,25%);background-color:var(--color,blue)'>Good</p></div></template></body>"

    expected =
      "<body style='font-size:20pt'><div style='width:100pt'><p style='width:2em;height:25%;background-color:blue'>Good</p></div></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, reference} = HtmlToPdf.render(expected, @opts)
    assert pdf == reference
  end

  test "shared source parser rejects invalid inputs and limits fallback grammar work" do
    assert :error = HtmlValidator.parse_css_variable_references(nil)
    assert :error = HtmlValidator.parse_css_variable_references(<<255>>)

    assert {:error, {:invalid_options, diagnostic}} =
             HtmlValidator.prepare_css_variable_source("var(--x)", nil)

    assert diagnostic.stage == :css and diagnostic.source == "css_budget"

    limits = Limits.effective()
    Limits.install(Map.put(limits, :max_css_variable_depth, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.prepare_css_variable_source(
               "var(--a,var(--b,2em))",
               HtmlValidator.new_css_budget()
             )

    assert diagnostic.message =~ "max_css_variable_depth"

    Limits.install(Map.put(limits, :max_css_variable_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.prepare_css_variable_source(
               "var(--a,var(--b,2em))",
               HtmlValidator.new_css_budget()
             )

    assert diagnostic.message =~ "max_css_variable_work"
  end

  test "unapplied source parsing consumes the same shared invocation budget without resetting it" do
    assert {:ok, dom} =
             HtmlParser.parse_document_detailed(
               "<style>.absent{width:var(--missing,2em)}</style><p>Active</p>"
             )

    budget = HtmlValidator.new_css_budget()
    consume = fn _, _, _ -> :ok end
    assert {:ok, _} = Style.with_source_style_contexts(dom, [], budget, consume, @opts)
    used = :atomics.get(budget, 1)
    assert used == byte_size("var(--missing,2em)")
    Limits.install(Map.put(Limits.effective(), :max_css_variable_total_bytes, used))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Style.with_source_style_contexts(dom, [], budget, consume, @opts)

    assert diagnostic.message =~ "max_css_variable_total_bytes"
  end
end
