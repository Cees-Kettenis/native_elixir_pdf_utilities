Code.require_file("../support/png_fixture.ex", __DIR__)

defmodule NativeElixirPdfUtilities.HtmlToPdf.ResponsiveMathTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Validators.{PrintMediaValidator, ResponsiveImageValidator}
  @context %{width: 800.0, height: 600.0, density: 1.0, type: :print}

  test "Values 4 numeric functions and dimensional cancellations resolve real CSS source lengths" do
    for {value, expected} <- [
          {"abs(-64px)", 64},
          {"hypot(48px,64px)", 80},
          {"round(63px,16px)", 64},
          {"round(up,63px,-16px)", 64},
          {"round(up,65px,16px)", 80},
          {"round(down,65px,16px)", 64},
          {"abs(round(to-zero,-65px,16px))", 64},
          {"mod(66px,64px)", 2},
          {"abs(rem(-66px,64px))", 2},
          {"calc(pow(2,6)*1px)", 64},
          {"calc(sqrt(4096)*1px)", 64},
          {"calc(log(64,2)*16px)", 96},
          {"calc(exp(0)*64px)", 64},
          {"calc(sin(90deg)*64px)", 64},
          {"calc(cos(0rad)*64px)", 64},
          {"calc(tan(45deg)*64px)", 64},
          {"calc(asin(1)/1deg*1px)", 90},
          {"calc(acos(0)/1deg*1px)", 90},
          {"calc(atan(1)/1deg*1px)", 45},
          {"calc(atan2(3px,3px)/1deg*1px)", 45},
          {"calc(sign(-3px)*-64px)", 64},
          {"calc(2px*3px/1px)", 6},
          {"calc(8s/2s*16px)", 64},
          {"calc(2turn/180deg*16px)", 64},
          {"calc(25%/5%*16px)", 80},
          {"clamp(none,64px,none)", 64},
          {"min(calc(infinity*1px),64px)", 64},
          {"max(calc(-infinity*1px),64px)", 64},
          {"calc(NaN*1px)", 0},
          {"calc(sqrt(-1)*1px)", 0},
          {"mod(calc(infinity*1px),2px)", 0},
          {"rem(calc(infinity*1px),2px)", 0},
          {"mod(-2px,calc(infinity*1px))", 0},
          {"mod(2px,calc(infinity*1px))", 2},
          {"abs(rem(-2px,calc(infinity*1px)))", 2},
          {"c\\61lc(64\\70 x)", 64},
          {"calc(\\53qrt(4096)*1px)", 64},
          {"round(\\75p,65px,16px)", 80}
        ] do
      assert {:ok, actual} = PrintMediaValidator.source_length(value, @context), value
      assert_in_delta actual, expected, 0.000001, value
    end

    assert ResponsiveImageValidator.source_size(
             "(width < 1px) 100vw, c\\61lc(64\\70 x)",
             @context
           ) == 64
  end

  test "invalid dimensional results are skipped and actual finite resource limits diagnose" do
    for value <- [
          "pow(2px,2)",
          "sqrt(4px)",
          "atan2(1px,1)",
          "sin(1px)",
          "calc(1px*1s)",
          "calc(1px/1s)",
          "calc(1px + 1)",
          "round(2px)",
          "min()",
          "(64px)"
        ] do
      assert :error = PrintMediaValidator.source_length(value, @context), value
      assert ResponsiveImageValidator.source_size(value <> ", 48px", @context) == 48
    end

    for value <- ["calc(1px/0)", "calc(1px/1e-320)", "calc(1000000000px*1000000000)"] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               PrintMediaValidator.source_length(value, @context)

      assert diagnostic.stage == :style and diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message != ""

      assert {:error, {:resource_limit_exceeded, _}} =
               ResponsiveImageValidator.source_size(value, @context)
    end
  end

  test "escaped identifiers, operators and unknown general-enclosed components obey media grammar" do
    for query <- [
          ~S|\70rint|,
          ~S|(\77 idth: 800\70 x)|,
          ~S|(700px<\77 idth<=900px)|,
          ~S|(\77 idth>=800px)|,
          ~S|(width:800px) \61nd (height:600px)|,
          "not télévision",
          ~S|(\6eot (width: 700px))|,
          "(width: 800px) or unknown()",
          "not ((width: 700px) and unknown())"
        ] do
      assert PrintMediaValidator.matches?(query, @context), query
    end

    for query <- [
          ~S|not unknown("a)")|,
          ~S|not (unknown "a)")|,
          "not (unknown: 1)",
          "not (orientation: diagonal)",
          "not (width: 1foo)",
          "not ((width: 800px) and unknown())",
          "not only screen",
          "only not print",
          "not only unknown()",
          "(width < = 900px)",
          ~S|not unknown("unterminated)|,
          ~S|not (unknown "unterminated)|
        ] do
      refute PrintMediaValidator.matches?(query, @context), query
    end
  end

  test "source selection and print predicates use the same math without reading alternatives" do
    png =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(64, 32, 2, 8, 0, fn _, _ ->
        [0, 128, 64]
      end)

    for value <- [
          "abs(-64px)",
          "calc(sqrt(4096)*1px)",
          "hypot(38.4px,51.2px)",
          "round(down,65px,16px)"
        ] do
      html =
        "<style>@media (min-width:#{value}) {p{color:red}}</style><img srcset='chosen 64w, unused 128w' sizes='#{value}' style='width:48pt;height:24pt'><p>Math predicate</p>"

      assert {:ok, _} =
               HtmlToPdf.render(html,
                 asset_resolver: fn request ->
                   assert request.reference == "chosen"
                   send(self(), {:read, request.reference})
                   {:ok, png}
                 end
               )

      assert_received {:read, "chosen"}
      refute_received {:read, _}
      assert PrintMediaValidator.matches?("(width >= #{value})", @context)
    end
  end

  @tag :browser_parity
  @tag :responsive_math_original
  test "numeric source choices and escaped media match original Chromium" do
    fixture =
      Path.expand("../fixtures/html_to_pdf/browser_parity/html_responsive_math.html", __DIR__)

    stats =
      NativeElixirPdfUtilities.TestSupport.PdfVisualCompare.assert_browser_match!(fixture,
        artifact_dir: "tmp/browser_parity/html_responsive_math",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 1
  end
end
