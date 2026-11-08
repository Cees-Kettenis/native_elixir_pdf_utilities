defmodule NativeElixirPdfUtilities.HtmlToPdf.CascadeLayersTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  # Independent source cases from Cascade 5, 13 January 2022, sections 6.4 and 7.3.
  test "normal and important declarations reverse layer order before specificity" do
    html = "<p id='a' class='case'>Text</p>"

    for {css, color} <- [
          {"@layer first,last;@layer last{p{color:blue}}@layer first{#a{color:red}}", {0, 0, 1}},
          {"@layer first,last;@layer last{p{color:blue!important}}@layer first{#a{color:red!important}}",
           {1, 0, 0}},
          {"@layer first{#a{color:red}}p{color:blue}", {0, 0, 1}},
          {"@layer first{p{color:red!important}}#a{color:blue!important}", {1, 0, 0}},
          {"@layer outer.a,outer.b;@layer outer.b{p{color:blue}}@layer outer.a{p{color:red}}",
           {0, 0, 1}},
          {"@layer outer{p{color:blue}@layer a{p{color:red}}}", {0, 0, 1}},
          {"@layer{p{color:red}}@layer{p{color:blue}}", {0, 0, 1}}
        ] do
      assert CssStyles.by_id(html, [css])["a"].color == color, css
    end
  end

  test "layer declarations establish document order across separate stylesheets" do
    nodes =
      CssStyles.by_id("<p id='a'>Text</p>", [
        "@layer base,theme;",
        "@layer theme{p{color:blue}}",
        "@layer base{p{color:red}}"
      ])

    assert nodes["a"].color == {0, 0, 1}
  end

  test "revert-layer restores the preceding layer and revert restores the user agent" do
    nodes =
      CssStyles.by_id("<div style='color:green'><p id='a'>Text</p><p id='b'>Text</p></div>", [
        """
        @layer base,theme;
        @layer base {p {color:blue; margin-left:3pt}}
        @layer theme {p {color:red; margin-left:7pt} #a {color:revert-layer; margin:revert-layer} #b {color:revert}}
        """
      ])

    assert nodes["a"].color == {0, 0, 1}
    assert nodes["a"].margin.left == 3.0
    assert nodes["b"].color == {0, 0.5019607843, 0}
  end

  test "nesting keeps the maximum parent specificity and interleaved declaration order" do
    nodes =
      CssStyles.by_id("<div class='parent'><p id='a'>Text</p><p id='b'>Text</p></div>", [
        """
        .parent,#missing { color:red; & > p {color:blue} color:green; }
        .parent p {color:red}
        .parent { @media print { > #b {font-weight:bold} } }
        """
      ])

    assert nodes["a"].color == {0, 0, 1}
    assert nodes["b"].color == {0, 0, 1}
    assert nodes["b"].font_weight == 700
  end
end
