defmodule NativeElixirPdfUtilities.HtmlToPdf.CustomPropertyRegistrationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.CssStyles
  alias NativeElixirPdfUtilities.Validators.CssPropertyRegistrationValidator

  # Authored independently from Properties and Values API 1, 26 March 2024, sections 2, 3 and 5.
  test "registered defaults inheritance and invalid values replace the winning value without falling through" do
    nodes =
      CssStyles.by_id(
        """
        <div id='parent' style='--size:8pt;--tone:blue'>
          <p id='child'>Text</p><p id='invalid' style='--size:bad;--tone:bad'>Text</p>
        </div>
        """,
        [
          """
          @property --size {syntax:'<length>';inherits:false;initial-value:3pt}
          @property --tone {syntax:'<color>';inherits:true;initial-value:red}
          p {width:var(--size);color:var(--tone)}
          """
        ]
      )

    assert nodes["parent"]._custom_properties["--size"] == "10.666666666666666px"
    assert nodes["child"].width == 3.0
    assert nodes["invalid"].width == 3.0
    assert nodes["child"].color == {0, 0, 1}
    assert nodes["invalid"].color == {0, 0, 1}
  end

  test "typed relative units compute on the declaring element before inheritance" do
    nodes =
      CssStyles.by_id(
        """
        <div id='parent' style='font-size:10pt;--size:2em'>
          <p id='child' style='font-size:20pt;width:var(--size)'>Text</p>
          <p id='local' style='font-size:20pt;--size:2em;width:var(--size)'>Text</p>
        </div>
        """,
        ["@property --size{syntax:'<length>';inherits:true;initial-value:1pt}"]
      )

    assert nodes["child"].width == 20.0
    assert nodes["local"].width == 40.0
  end

  test "registered font dependencies detect cycles while ordinary token variables remain usable" do
    nodes =
      CssStyles.by_id(
        """
        <p id='cycle' style='--size:2em;font-size:var(--size);width:var(--size)'>Text</p>
        <p id='plain' style='--plain:2em;font-size:var(--plain)'>Text</p>
        <p id='fallback' style='--size:bad;--other:var(--size);width:var(--other)'>Text</p>
        """,
        ["@property --size{syntax:'<length>';inherits:false;initial-value:3pt}"]
      )

    assert nodes["cycle"].font_size == 3.0
    assert nodes["cycle"].width == 3.0
    assert nodes["plain"].font_size == 24.0
    assert nodes["fallback"].width == 3.0
  end

  test "invalid descriptors cannot replace a valid registration and universal syntax can remain invalid" do
    nodes =
      CssStyles.by_id("<p id='a' style='width:var(--size);color:var(--tokens,blue)'>Text</p>", [
        """
        @property --size{syntax:'<length>';inherits:false;initial-value:4pt}
        @property --size{syntax:'<length>';inherits:false;initial-value:2em}
        @property --tokens{syntax:'*';inherits:false}
        """
      ])

    assert nodes["a"].width == 4.0
    assert nodes["a"].color == {0, 0, 1}
  end

  test "syntax alternatives and list multipliers validate their own computed types" do
    for {syntax, value, expected} <- [
          {"<number>", "calc(2 + 3)", "5.0"},
          {"<integer>", "calc(1.5)", "2"},
          {"<percentage>", "25%", "25.0%"},
          {"<time>", "500ms", "0.5s"},
          {"<angle>", "1turn", "6.283185307179586rad"},
          {"<resolution>", "96dpi", "1.0dppx"},
          {"<custom-ident>", "custom", "custom"},
          {"<length> | auto", "auto", "auto"},
          {"<length>+", "1pt 2pt", "1.3333333333333333px 2.6666666666666665px"},
          {"<number>#", "1,2", "1.0, 2.0"}
        ] do
      assert {:ok, registrations} =
               CssPropertyRegistrationValidator.prepare([
                 %{
                   name: "--value",
                   declarations: "syntax:'#{syntax}';inherits:false;initial-value:#{value}"
                 }
               ])

      assert registrations["--value"].initial == expected
    end
  end

  test "registered line-height units use computed font metrics and avoid false cycles with fixed line height" do
    nodes =
      CssStyles.by_id(
        """
        <p id='fixed' style='line-height:30pt;--size:1lh;width:var(--size)'>Text</p>
        <p id='normal' style='font-size:20pt;--size:1lh;width:var(--size)'>Text</p>
        <p id='cycle-free' style='line-height:30pt;--size:1lh;font-size:var(--size);width:var(--size)'>Text</p>
        """,
        ["@property --size{syntax:'<length>';inherits:false;initial-value:3pt}"]
      )

    assert nodes["fixed"].width == 30.0
    assert nodes["normal"].width == nodes["normal"].line_height
    assert nodes["cycle-free"].font_size == 30.0
    assert nodes["cycle-free"].width == 30.0
  end

  test "root-relative font cycles and font shorthands share typed dependency checks" do
    for css <- [
          "html{--size:2rem;font-size:var(--size)}",
          "html{--size:2em;font:var(--size) serif}"
        ] do
      nodes =
        CssStyles.by_id("<html><body id='root'><p>Text</p></body></html>", [
          "@property --size{syntax:'<length>';inherits:false;initial-value:3pt}",
          css
        ])

      assert nodes["root"].font_size == 3.0
    end
  end

  test "typed line-relative values use computed number and root-relative line heights" do
    nodes =
      CssStyles.by_id(
        """
        <p id='number' style='font-size:10pt;line-height:2;--size:2lh;width:var(--size)'>Text</p>
        <p id='root' style='font-size:10pt;line-height:2rem;--size:2lh;width:var(--size)'>Text</p>
        """,
        ["@property --size{syntax:'<length>';inherits:false;initial-value:1pt}"]
      )

    assert nodes["number"].width == 40.0
    assert nodes["root"].width == 48.0
  end

  test "registered currentColor cycles default while variable line heights remain independent" do
    nodes =
      CssStyles.by_id(
        """
        <p id='tone' style='--tone:currentColor;color:var(--tone)'>Text</p>
        <p id='height' style='--height:20pt;line-height:var(--height);--size:2lh;width:var(--size)'>Text</p>
        """,
        [
          """
          @property --tone{syntax:'<color>';inherits:false;initial-value:blue}
          @property --size{syntax:'<length>';inherits:false;initial-value:1pt}
          """
        ]
      )

    assert nodes["tone"].color == {0, 0, 1}
    assert nodes["height"].width == 40.0
  end

  test "an absolute variable line height does not create a font-size cycle" do
    nodes =
      CssStyles.by_id(
        """
        <p id='a' style='--height:20pt;line-height:var(--height);--size:2lh;font-size:var(--size)'>Text</p>
          <p id='font' style='--size:2lh;font:var(--size)/20pt serif'>Text</p>
        """,
        ["@property --size{syntax:'<length>';inherits:false;initial-value:1pt}"]
      )

    assert nodes["a"].font_size == 40.0
    assert nodes["a"].line_height == 20.0
    assert nodes["font"].font_size == 40.0
    assert nodes["font"].line_height == 20.0
  end

  test "defaulted and root-relative line heights retain independent inherited metrics" do
    nodes =
      CssStyles.by_id(
        """
        <div style='line-height:20pt'>
          <p id='missing' style='--size:2lh;line-height:var(--missing);font-size:var(--size)'>Text</p>
        <p id='invalid-height' style='--bad:bananas;--size:2lh;line-height:var(--bad);font-size:var(--size)'>Text</p>
          <p id='root-relative' style='--size:2lh;line-height:2rem;font-size:var(--size)'>Text</p>
          <p id='initial' style='font:initial'>Text</p>
        </div>
        """,
        ["@property --size{syntax:'<length>';inherits:false;initial-value:1pt}"]
      )

    assert nodes["missing"].font_size == 40.0
    assert nodes["invalid-height"].font_size == 40.0
    assert nodes["invalid-height"].line_height == 20.0
    assert nodes["root-relative"].font_size == 48.0
    assert nodes["initial"].font_size == 12.0

    for declaration <- ["line-height:bananas", "font:heavy 12pt serif", "font:bold"] do
      assert {:error, {:invalid_css, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<style>@property --size{syntax:'<length>';inherits:false;initial-value:1pt}</style><p style='#{declaration}'>Text</p>"
               )

      assert diagnostic.source == declaration
    end
  end
end
