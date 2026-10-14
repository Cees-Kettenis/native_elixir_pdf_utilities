defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateUnappliedResidualTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}
  @opts [system_font_discovery: false, compress_streams: false]

  for declaration <- [
        "flex:1 1 2em",
        "flex:1 2 calc(1rem + 2pt)",
        "display:inline math",
        "display:block math",
        "display:math"
      ] do
    test "unapplied #{declaration} permits actual original replacement" do
      for selector <- [".absent", "@media screen{p"] do
        declaration = unquote(declaration)

        rule =
          if selector == ".absent",
            do: ".absent{#{declaration}}",
            else: "@media screen{p{#{declaration}}}"

        html =
          "<style>#{rule}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

        assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
        assert {:ok, expected} = HtmlToPdf.render("<p>Good</p>", @opts)
        assert pdf == expected
        assert {:ok, "Good"} = Text.extract(pdf)
      end
    end
  end

  test "source grammar retains uncomputed flex basis and Math display without a Math context or metrics" do
    html = "<style>.absent{flex:1 1 2em;display:inline math}</style><p>Active</p>"
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    assert {:ok, result} =
             Style.with_source_style_contexts(
               dom,
               [],
               HtmlValidator.new_css_budget(),
               fn _, _, _ -> :ok end,
               @opts
             )

    [flex, display] = result.source_obligations.declaration_contexts
    assert flex.status == :checked_symbolically and flex.contexts == []
    assert flex.source_value.properties.flex_basis.geometry.units == ["em"]
    assert flex.source_value.properties.flex_grow == 1.0
    assert display.status == :checked_symbolically and display.contexts == []
    assert display.source_value.kind == :math_display and display.source_value.display == :inline
    refute Map.has_key?(display.source_value, :_math_context)
    assert :error == MathLayoutValidator.parse_property("display", "inline math", %{})
  end

  test "malformed original residual values retain exact source diagnostics" do
    for declaration <- [
          "flex:1 -2 3em",
          "flex:1 1 -2em",
          "flex:1 1 calc(1em + 1deg)",
          "flex:1 1 2em extra",
          "display:inline math block"
        ] do
      html =
        "<style>@media screen{p{#{declaration}}}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
      assert diagnostic.stage == :css and diagnostic.source == declaration
      assert diagnostic.line == 1 and is_integer(diagnostic.column)
    end
  end

  test "applied ordinary HTML cannot borrow the symbolic Math display context" do
    html =
      "<style>.selected{display:inline math}</style><body><?start name='A'><p class='selected'>Old</p><?end?><template for='A'><p>Good</p></template></body>"

    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.source == "display:inline math"

    html =
      "<style>.selected{flex:var(--missing)}</style><body><?marker name='A'><template for='A'><p class='selected'>Good</p></template></body>"

    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.source == "flex:var(--missing)"
  end

  test "Math display keyword grammar never issues a context for malformed public input" do
    assert {:ok, :inline} = MathLayoutValidator.parse_display(" INLINE MATH ")
    assert {:ok, :block} = MathLayoutValidator.parse_display("block math")

    for value <- [nil, %{}, <<255>>, "inline math block", "block"] do
      assert :error == MathLayoutValidator.parse_display(value)
    end
  end

  @tag :browser_parity
  @tag :source_residual_original
  test "original unapplied residual grammar and actual inserted flex and Math boxes match Chromium" do
    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/template_replacement/unapplied_residuals.html",
        __DIR__
      )

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: ".quality/orchestration/source-residual-original-browser",
        native_renderer:
          &NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.render_initial_numbers/2,
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.scripting == :disabled and stats.page_count == 1
  end
end
