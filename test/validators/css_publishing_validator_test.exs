defmodule NativeElixirPdfUtilities.Validators.CssPublishingValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.{
    CssPublishingValidator,
    CssGeneratedContentValidator,
    CssPageValidator
  }

  test "publishing declarations admit only meaningful values and computed fields" do
    assert "page" in CssPublishingValidator.properties()

    for {property, value} <- [
          {"page", "auto"},
          {"page", "Report"},
          {"position", "running(header)"},
          {"running", "chapter"},
          {"running", "none"},
          {"float", "footnote"},
          {"float", "top"},
          {"string-set", "none"},
          {"string-set", "chapter content()"},
          {"footnote-display", "compact"},
          {"footnote-policy", "line"},
          {"float-reference", "column"},
          {"float-defer", "none"},
          {"float-defer", "last"},
          {"float-defer", "2"},
          {"float-offset", "2em"},
          {"float-offset", "50%"},
          {"bookmark-level", "none"},
          {"bookmark-level", "2"},
          {"bookmark-label", "content(text)"},
          {"bookmark-label", "normal"},
          {"bookmark-state", "closed"},
          {"box-decoration-break", "clone"},
          {"margin-break", "keep"}
        ] do
      assert {:ok, style} = CssPublishingValidator.prepare(%{font_size: 10.0}, property, value)
      assert CssPublishingValidator.valid_style?(style), "#{property}:#{value}"
    end

    for {property, value} <- [
          {"page", "two names"},
          {"running", "2"},
          {"position", "running(x,copy)"},
          {"string-set", "chapter"},
          {"string-set", "chapter element(heading)"},
          {"string-set", "chapter 'First',"},
          {"float-defer", "1.5"},
          {"float-offset", "bogus"},
          {"bookmark-level", "0"},
          {"bookmark-label", "unknown"},
          {"bookmark-label", "url(chapter.png)"},
          {"margin-break", "bad"}
        ] do
      assert {:error, :invalid_document} = CssPublishingValidator.prepare(%{}, property, value)
    end

    for style <- [
          %{page_name: 1},
          %{running_name: :bad},
          %{running_copy: 1},
          %{bookmark_level: -1},
          %{float_offset: :bad},
          %{float_defer: "last"},
          %{publishing_float: :bad},
          %{string_set: [:bad]},
          %{string_set: [{"bad name", [{:string, "X"}]}]},
          %{string_set: [{"title", [{:content, "text"}]} | :bad]},
          %{bookmark_label: [:bad]},
          %{footnote_policy: "bad"}
        ] do
      refute CssPublishingValidator.valid_style?(style)
    end

    assert CssPublishingValidator.valid_style?(%{})

    assert CssPublishingValidator.valid_style?(%{
             running_name: nil,
             float_offset: {:percent, 0.5}
           })
  end

  test "footnote capacity and repeated convergence states return actionable diagnostics" do
    assert {:ok, 40.0} =
             CssPublishingValidator.footnote_capacity(
               %{max_height: {:percent, 0.5}},
               80.0,
               80.0,
               10.0
             )

    assert {:error, {:invalid_layout, %{message: message}}} =
             CssPublishingValidator.footnote_capacity(%{max_height: 1.0}, 80.0, 80.0, 10.0)

    assert message =~ "one line"
    assert :ok = CssPublishingValidator.convergence(0, :initial, %{})

    assert {:error, {:invalid_layout, %{message: message}}} =
             CssPublishingValidator.convergence(2, :cycle, %{cycle: true})

    assert message =~ "repeats an earlier state"
  end

  test "prepared content and physical PDF boxes reject malformed data" do
    assert {:ok, parts} =
             CssGeneratedContentValidator.prepare(
               "attr(label) ' ' attr(title raw-string,'Fallback') counters(section,'.') target-text('#id',before) target-counter('#id',page) content()"
             )

    assert CssGeneratedContentValidator.valid_parts?(parts)

    for parts <- [
          :none,
          [:bad],
          [{:string, "Visual"} | :bad],
          [{:counter, "x", 2}],
          [{:target_text, "#id", "bad"}],
          [{:alternative, [{:alternative, []}]}],
          [{:alternative, :bad}],
          [{:alternative, [{:string, "Alt"} | :bad]}],
          [{:string, "x", "bad"}],
          [{:quote, :bad}]
        ] do
      refute CssGeneratedContentValidator.valid_parts?(parts)
    end

    assert CssGeneratedContentValidator.valid_parts?([{:alternative, [{:string, "alt"}]}])
    assert CssPageValidator.valid_pdf_boxes?(%{size: {100, 100}, trim_box: {1, 1, 99, 99}})
    refute CssPageValidator.valid_pdf_boxes?(%{size: {100, 100}, bleed_box: {0, 0, 101, 99}})
    refute CssPageValidator.valid_pdf_boxes?(%{size: {100, 100}, trim_box: :bad})
    refute CssPageValidator.valid_pdf_boxes?(%{size: {100, 100}, trim_box: {0, :zero, 100, 100}})
  end

  test "running elements must be the only generated item in a page margin box" do
    alias NativeElixirPdfUtilities.HtmlToPdf

    for html <- [
          "<style>p::before{content:element(heading)}</style><p>Body</p>",
          "<style>@page{@top-center{content:element(heading) 'Suffix'}}</style><p>Body</p>"
        ] do
      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :css
    end
  end

  test "important page descriptors outrank later rules and preserve three- and four-side shorthands" do
    assert {:ok, rules} =
             CssPageValidator.rules("""
             @page {size:12em; margin:1em 2em 3em !important}
             @page:first {margin:0}
             @page wide {size:landscape; margin:1pt 2pt 3pt 4pt}
             """)

    opts = [page_size: {200, 400}]
    style = %{font_size: 10.0}
    first = CssPageValidator.select(rules, "", 1, false)
    assert {:ok, geometry} = CssPageValidator.resolve_geometry(first, opts, [], style)
    assert geometry[:page_size] == "120.0pt 120.0pt"
    assert geometry[:margin] == %{top: 10.0, right: 20.0, bottom: 30.0, left: 20.0}

    assert {:ok, single} =
             CssPageValidator.rules("@page wide{size:landscape;margin:1pt 2pt 3pt 4pt}")

    assert {:ok, geometry} =
             CssPageValidator.resolve_geometry(
               CssPageValidator.select(single, "wide", 1, false),
               opts,
               [],
               style
             )

    assert geometry[:page_size] == "400.0pt 200.0pt"
    assert geometry[:margin] == %{top: 1.0, right: 2.0, bottom: 3.0, left: 4.0}
  end

  test "page geometry rejects arithmetic without a usable point dimension" do
    assert {:ok, [invalid_size]} = CssPageValidator.rules("@page{size:calc(0pt)}")
    assert {:ok, [invalid_margin]} = CssPageValidator.rules("@page{margin:var(--missing)}")
    style = %{font_size: 10.0}

    for rule <- [invalid_size, invalid_margin] do
      selected = CssPageValidator.select([rule], "", 1, false)
      assert {:error, _} = CssPageValidator.resolve_geometry(selected, [], [], style)
    end

    assert {:error, _} =
             CssPageValidator.resolve_geometry(
               %{declarations: [{"size", "10pt 20pt 30pt"}]},
               [],
               [],
               style
             )

    assert :error = CssGeneratedContentValidator.prepare("url()")
  end
end
