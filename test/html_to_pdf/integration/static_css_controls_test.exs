defmodule NativeElixirPdfUtilities.HtmlToPdf.StaticCssControlsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}

  @opts [page_size: {300, 300}, margin: 0, system_font_discovery: false]

  test "placeholder styling and content sizing use the initial text but keep the field value empty" do
    html = """
    <style>input{field-sizing:content} input::placeholder{color:red}</style>
    <input name="initial" placeholder="Hint"><input name="default" value="Very long text">
    """

    assert {:ok, tree} = style(html)
    assert {:ok, layout} = Layout.layout(tree, @opts)
    assert %{color: {1, 0, 0}} = Enum.find(layout.boxes, &match?(%{text: "Hint"}, &1))
    widths = for %{form_control: %{tag: "input"}, width: width} <- layout.boxes, do: width
    assert [short, long] = widths
    assert short < long

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, model} = Forms.fields(pdf)
    assert Enum.any?(model, &(&1.name == "initial" and &1.value == ""))
  end

  test "content-sized textareas keep visible lines and editable values at the final width" do
    value = "Another long line that must wrap to more than two lines"

    for display <- ["inline-block", "block"],
        {attrs, dimension, initial, visible, height} <- [
          {"rows=1", "width:8ch", value, value, 132},
          {"rows=1", "width:24ch", "Another long line", "Another long line", 12},
          {"rows=1", "", "One\nTwo\nThree", "One Two Three", 36},
          {"rows=4", "", "One", "One", 12},
          {"rows=4", "", "", "", 12},
          {"", "", "One\n", "One", 24},
          {"placeholder='One&#10;Two'", "", "", "One Two", 24},
          {"rows=4", "field-sizing:fixed", "One", "One", 48},
          {"", "height:12pt", "One\nTwo", "One", 12},
          {"", "max-height:24pt", "One\nTwo\nThree", "One Two", 24},
          {"", "min-height:36pt", "One", "One", 36}
        ] do
      html = """
      <style>body{margin:0}textarea{display:#{display};field-sizing:content;font:12pt/12pt monospace;
        border:0;padding:0;#{dimension}}</style><textarea name="notes" #{attrs}>#{initial}</textarea>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html, @opts)

      assert {:ok, [%{name: "notes", value: ^initial, widgets: [%{rect: [x, y, r, t]}]}]} =
               Forms.fields(pdf)

      assert_in_delta t - y, height, 0.01
      assert r > x
      assert {:ok, static} = HtmlToPdf.render(html, Keyword.put(@opts, :forms, :static))
      assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(static)

      # Wrapping can split a word into separate positioned spans.
      assert spans |> Enum.map_join(& &1.text) |> String.replace(~r/\s+/u, "") ==
               String.replace(visible, ~r/\s+/u, "")
    end
  end

  test "textarea client width responds to height constraints and scrollbar settings" do
    for display <- ["block", "inline-block"] do
      rect = fn css ->
        html = """
        <style>textarea{display:#{display};font:12pt/12pt monospace;field-sizing:content;
          border:0;padding:0;#{css}}</style><textarea name="notes">One</textarea>
        """

        assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
        assert {:ok, [%{widgets: [%{rect: [l, b, r, t]}]}]} = Forms.fields(pdf)
        {r - l, t - b}
      end

      {plain, 12.0} = rect.("overflow:hidden")
      {natural, 12.0} = rect.("")
      {constrained, 36.0} = rect.("min-height:36pt")
      {thin, 12.0} = rect.("scrollbar-width:thin")
      {forced, 23.25} = rect.("overflow:scroll")
      assert_in_delta natural - plain, 11.25, 0.01
      assert_in_delta thin - plain, 7.5, 0.01
      assert_in_delta forced, natural, 0.01
      assert_in_delta constrained, plain, 0.01
    end
  end

  test "skipping a native editor retains its host height and suppresses its value paint" do
    html = """
    <div><input hidden=until-found value="HiddenEditor" style="display:block;width:90pt;line-height:15pt"></div>
    """

    assert {:ok, tree} = style(html)
    assert {:ok, layout} = Layout.layout(tree, @opts)

    assert Enum.any?(layout.boxes, fn box ->
             Map.get(box, :role) == :native_menu and box.height > 15.0
           end)

    refute Enum.any?(layout.boxes, &match?(%{text: "HiddenEditor"}, &1))
  end

  test "range and gauge parts keep the existing fractional value geometry" do
    html = """
    <style>
    input::slider-thumb{background-color:red}
    input::slider-track{background-color:blue}
    progress::fill{background-color:green}
    </style>
    <div><input type="range" value="30"><progress value="0.25"></progress></div>
    """

    assert {:ok, tree} = style(html)
    assert {:ok, layout} = Layout.layout(tree, @opts)
    assert Enum.any?(layout.boxes, &match?(%{role: :range_thumb, fill_color: {1, 0, 0}}, &1))
    assert Enum.any?(layout.boxes, &match?(%{role: :range_track, fill_color: {0, 0, 1}}, &1))
    assert Enum.any?(layout.boxes, &match?(%{role: :gauge_value, fill_color: {0, _, 0}}, &1))
  end

  test "zoom scales used layout lengths and text through nested controls" do
    html = """
    <div style="zoom:200%">
      <div style="width:30pt;height:5pt;background:red;border-radius:1pt 2pt"></div>
      <div style="zoom:0.5;width:25pt;height:5pt;background:blue"></div>
    </div>
    """

    assert {:ok, tree} = style(html)
    assert {:ok, layout} = Layout.layout(tree, @opts)
    assert Enum.any?(layout.boxes, &match?(%{fill_color: {1, 0, 0}, width: 60.0}, &1))
    assert Enum.any?(layout.boxes, &match?(%{fill_color: {0, 0, 1}, width: 25.0}, &1))
  end

  test "root zoom projects both the root box and the lengths inside it" do
    html = """
    <html style="width:120pt;zoom:150%"><body style="margin:0">
      <div style="width:50%;height:10pt;background:red"></div>
    </body></html>
    """

    assert {:ok, tree} = style(html)
    assert {:ok, layout} = Layout.layout(tree, @opts)

    assert %{width: 90.0, height: 15.0} =
             Enum.find(layout.boxes, &match?(%{fill_color: {1, 0, 0}}, &1))
  end

  test "paint-only pseudo-elements preserve the native part geometry and stroke" do
    alias NativeElixirPdfUtilities.HtmlToPdf.ControlArtwork

    box = %{role: :range_thumb, x: 4.0, width: 6.0, stroke_width: 0.75, fill_color: {0, 0, 1}}
    style = %{_control_parts: %{slider_thumb: %{background_color: {1, 0, 0}}}}
    assert ControlArtwork.paint_parts([box], style) == [%{box | fill_color: {1, 0, 0}}]
  end

  test "logical reading order can differ from the unchanged physical paint order" do
    html = """
    <div style="display:flex;flex-direction:row-reverse;reading-flow:flex-visual">
      <div>First</div><div>Second</div><div style="reading-order:-1">Priority</div>
    </div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, %{pages: [%{spans: logical}]}} = Text.extract_spans(pdf, actual_text: true)
    assert Enum.map_join(logical, "\n", & &1.text) == "Priority\nSecond\nFirst"
    assert {:ok, %{pages: [%{spans: physical}]}} = Text.extract_spans(pdf)

    assert {:ok, original} =
             HtmlToPdf.render(String.replace(html, ";reading-flow:flex-visual", ""), @opts)

    assert {:ok, %{pages: [%{spans: baseline}]}} = Text.extract_spans(original)
    assert physical == baseline
  end

  test "static CSS part suppression leaves the filename and range state intact" do
    html = """
    <style>
      input::file-selector-button{background-color:red;color:blue}
      input.hide::file-selector-button{display:none}
      input::color-swatch{visibility:hidden}
      input::slider-fill{display:none}
      input::slider-thumb{border:1px solid red}
      input::placeholder{display:none}
      progress::track{background-color:purple}
      progress::fill{background-color:green}
      input::checkmark{color:red}
    </style>
    <div>
      <input type=file><input type=file class=hide><input type=color>
      <input type=range><input placeholder=Invisible>
      <input type=checkbox checked>
      <progress></progress><progress value=".5"></progress>
    </div>
    """

    opts = Keyword.put(@opts, :forms, :static)
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom, opts)
    assert {:ok, layout} = Layout.layout(tree, opts)

    assert Enum.any?(
             layout.boxes,
             &match?(%{role: :native_file_button, fill_color: {1, 0, 0}}, &1)
           )

    assert Enum.any?(layout.boxes, &match?(%{role: :file_label, color: {0, 0, 1}}, &1))
    refute Enum.any?(layout.boxes, &(Map.get(&1, :role) in [:color_swatch, :range_value]))
    refute Enum.any?(layout.boxes, &match?(%{text: "Invisible"}, &1))
  end

  test "query units in MathML operators use the declaring element's selected container" do
    alias NativeElixirPdfUtilities.Validators.MathOperatorValidator

    context = %{viewport_width: 300.0, viewport_height: 300.0, container_units: %{"cqw" => 2.0}}

    assert {:ok, %{lspace: 15.0}} =
             MathOperatorValidator.prepare("+", %{"lspace" => "10cqw"}, context)

    assert {:error, {:invalid_document, _}} =
             MathOperatorValidator.prepare("+", %{}, %{container_units: :invalid})

    assert {:error, {:invalid_document, _}} =
             MathOperatorValidator.prepare("+", %{}, %{container_units: %{"bad" => 5}})

    html =
      "<div style='width:120pt;container-type:inline-size'><math><mn>7</mn><mo lspace='2cqw'>+</mo><mn>9</mn></math></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "7" and text =~ "+" and text =~ "9"
  end

  test "initial and compatibility property values retain authored CSS defaults" do
    for style <- [
          "zoom:normal",
          "field-sizing:fixed",
          "appearance:base",
          "appearance:textfield",
          "appearance:button"
        ] do
      assert {:ok, _pdf} = HtmlToPdf.render("<input style='#{style}'>", @opts)
    end

    for style <- ["zoom:invalid", "field-sizing:invalid", "reading-flow:invalid"] do
      assert {:error, {:invalid_css, _}} = HtmlToPdf.render("<div style='#{style}'></div>", @opts)
    end

    assert {:ok, _pdf} =
             HtmlToPdf.render(
               "<input type=range style='field-sizing:content'>",
               Keyword.put(@opts, :forms, :static)
             )

    assert {:ok, _pdf} =
             HtmlToPdf.render("<input style='field-sizing:content;width:30pt'>", @opts)
  end

  test "control-value exposes initial authored values without using placeholder or password content" do
    alias NativeElixirPdfUtilities.Validators.CssSubstitutionValidator

    assert {:ok, "\"ready\""} =
             CssSubstitutionValidator.resolve("control-value(<string>)", %{
               _css_control_value: "ready"
             })

    assert {:ok, "calc(12 * 1px)"} =
             CssSubstitutionValidator.resolve("calc(control-value(<number>) * 1px)", %{
               _css_control_value: "12"
             })

    assert :error = CssSubstitutionValidator.resolve("control-value(<number>)", %{})
    assert :error = CssSubstitutionValidator.resolve("control-value(<length>)", %{})
    assert CssSubstitutionValidator.valid_syntax?("control-value()")
    refute CssSubstitutionValidator.valid_syntax?("control-value(<length>)")

    html = """
    <style>input::after,textarea::after{content:control-value()}</style>
    <div><input name="status" value="ready"><input type=password value="ExampleSecret">
    <textarea>Remember</textarea></div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, Keyword.put(@opts, :forms, :static))
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "ready"
    refute text =~ "ExampleSecret"
  end

  test "grid and block reading scopes keep logical fragments nested" do
    html = """
    <div style="reading-flow:source-order">
      <div style="reading-order:1; display:grid;grid-template-columns:40pt 40pt;reading-flow:grid-columns">
        <div style="grid-column:2">Grid last</div><div style="grid-column:1">Grid first</div>
      </div>
      <div>Block first</div>
    </div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Block first\nGrid first\nGrid last"
  end

  test "inline reading order operates on source spans without changing wrapping" do
    html = """
    <p style="reading-flow:source-order">
      <span style="reading-order:2">Second</span> <span style="reading-order:1">First</span>
    </p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "First\nSecond"
  end

  test "invalid selected control parts retain CSS diagnostics" do
    html = "<style>input::placeholder{color:unknown}</style><input placeholder=Hint>"
    assert {:error, {:invalid_css, _}} = HtmlToPdf.render(html, @opts)
  end

  defp style(html) do
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    Style.compute_detailed(dom, @opts)
  end
end
