defmodule NativeElixirPdfUtilities.HtmlToPdf.CascadeValuesTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  test "CSS-wide values use actual inheritance and initial values for every property family" do
    nodes =
      CssStyles.by_id("""
      <div style='color:blue;width:120pt;margin-left: 9pt;font-size:18pt'>
        <p id='inherit' style='color:inherit;width:inherit;margin-left:inherit'>Text</p>
        <p id='unset' style='color:unset;width:unset;margin-left:unset'>Text</p>
        <p id='initial' style='color:initial;font-size:initial;display:initial'>Text</p>
        <p id='revert' style='display:inline;display:revert'>Text</p>
      </div>
      """)

    assert nodes["inherit"].width == 120.0
    assert nodes["inherit"].margin.left == 9.0
    assert nodes["unset"].color == {0, 0, 1}
    assert nodes["unset"].width == :auto
    assert nodes["unset"].margin.left == 0.0
    assert nodes["initial"].color == {0, 0, 0}
    assert nodes["initial"].font_size == 12.0
    assert nodes["initial"].display == :inline
    assert nodes["revert"].display == :block
  end

  test "all resets the supported properties while retaining custom token definitions" do
    nodes =
      CssStyles.by_id(
        "<div style='color:blue'><p id='a' style='--size:3pt;all:initial;width:var(--size)'>Text</p></div>"
      )

    assert nodes["a"].display == :inline
    assert nodes["a"].color == {0, 0, 0}
    assert nodes["a"].width == 3.0
  end

  test "pending shorthand values project only their winning longhands and reset omitted components" do
    nodes =
      CssStyles.by_id("""
      <p id='a' style='--box:1pt 2pt;margin:var(--box);margin-left: 7pt'>Text</p>
      <p id='b' style='--bad:bananas;margin:8pt;margin:var(--bad);margin-right: 5pt'>Text</p>
      <p id='font' style='font-weight:bold;font:italic 15pt/2 serif;font-size:12pt'>Text</p>
      """)

    assert nodes["a"].margin == %{top: 1.0, right: 2.0, bottom: 1.0, left: 7.0}
    assert nodes["b"].margin == %{top: 0.0, right: 5.0, bottom: 0.0, left: 0.0}
    assert nodes["font"].font_weight == 400
    assert nodes["font"].font_style == :italic
    assert nodes["font"].font_size == 12.0
    assert nodes["font"].line_height == 24.0
  end

  test "important layer rollback skips the intervening normal and important layer levels" do
    nodes =
      CssStyles.by_id(
        "<p id='a'>Text</p><p id='b' style='color:revert-layer!important'>Text</p>",
        [
          """
          @layer first,last;
          @layer first {#a {color:revert-layer!important}}
          @layer last {#a {color:blue!important}}
          #b {color:red!important}
          """
        ]
      )

    assert nodes["a"].color == {0, 0, 0}
    assert nodes["b"].color == {1, 0, 0}
  end

  test "substitution retains token boundaries instead of forming a new dimension" do
    nodes =
      CssStyles.by_id("""
      <p id='invalid' style='--number:2;width:var(--number)pt'>Text</p>
      <p id='valid' style='--length:2pt;width:var(--length)'>Text</p>
      """)

    assert nodes["invalid"].width == :auto
    assert nodes["valid"].width == 2.0
  end

  test "substituted CSS-wide values recascade ordinary and custom properties" do
    nodes =
      CssStyles.by_id(
        """
        <div style='color:green'><p id='ordinary'>Text</p><p id='custom'>Text</p><p id='initial'>Text</p></div>
        """,
        [
          """
          @layer base { #ordinary{color:blue}#custom{--source:blue} }
          #ordinary {color:red;color:var(--missing,revert-layer)}
          #custom {--source:var(--missing,revert-layer);--dependent:var(--source);color:var(--dependent)}
          #initial {--source:var(--missing,initial);color:var(--source,blue)}
          """
        ]
      )

    assert nodes["ordinary"].color == {0, 0, 1}
    assert nodes["custom"].color == {0, 0, 1}
    assert nodes["initial"].color == {0, 0, 1}
  end
end
