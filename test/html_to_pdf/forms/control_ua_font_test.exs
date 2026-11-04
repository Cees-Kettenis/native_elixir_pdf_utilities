alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.ControlUaFontTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @sans Paths.project("priv/fonts/dejavu/DejaVuSans.ttf")
  @mono Paths.project("priv/fonts/dejavu/DejaVuSansMono.ttf")
  @bold_italic Paths.project("priv/fonts/dejavu/DejaVuSans-BoldOblique.ttf")
  @arial "/usr/share/fonts/liberation/LiberationSans-Regular.ttf"

  setup do
    # This is the same physical font Chromium selects for its Arial UA request.
    path =
      if File.regular?(@arial),
        do: @arial,
        else: "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"

    bytes = File.read!(path)

    assert Base.encode16(:crypto.hash(:sha256, bytes), case: :lower) in [
             "bade59d822652f76e6941aa87b40a87c13d1cc70db98ededb5011127efafd1d3",
             "baccc64becc3eb7d104b7c84d99f5314a0a1f896e2b3ea6c2f22fc08d2003bee"
           ]

    opts = [
      system_font_discovery: false,
      fonts: [
        %{family: "Arial", data: bytes},
        %{family: "DejaVu Sans", path: @sans},
        %{family: "DejaVu Sans", path: @bold_italic, weight: 700, style: :italic},
        %{family: "DejaVu Sans Mono", path: @mono}
      ]
    ]

    %{opts: opts, arial: bytes}
  end

  test "ordinary controls select caller UA fonts independently of the body font", %{
    opts: opts,
    arial: bytes
  } do
    html =
      "<body style='font-family:DejaVu Sans;font-size:20pt;font-weight:bold;font-style:italic'><input name=entry value=Original><select name=choice><option value=real>Real</option></select><textarea name=notes>Actual</textarea><button disabled>Button</button></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom, opts)
    [body] = tree.children
    [input, select, textarea, button] = body.children
    assert body.style.font_size == 20 and body.style.font_face.family == "DejaVu Sans"

    for node <- [input, select, button] do
      assert node.style.font_families == ["Arial", "sans-serif"]
      assert node.style.font_face.data == bytes and node.style.font_face.source == :configured

      assert node.style.font_size == 10 and node.style.font_style == :normal and
               node.style.font_weight == 400
    end

    assert textarea.style.font_families == ["monospace"] and textarea.style.font_size == 10
    assert textarea.style.font_face.data == File.read!(@mono)
    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert {:ok, fields} = Forms.fields(pdf)
    assert Enum.map(fields, & &1.name) == ["entry", "choice", "notes"]
    assert {:ok, flattened} = Forms.flatten(pdf)
    assert {:ok, text} = Text.extract(flattened)

    assert text =~ "Original" and text =~ "Real" and text =~ "Actual",
           "actual extracted source #{inspect(text)}"
  end

  test "author inherit and unset use ancestor font facts before UA defaults", %{
    opts: opts
  } do
    for keyword <- ["inherit", "unset"] do
      html =
        "<body style='font-family:DejaVu Sans;font-size:20pt;font-weight:bold;font-style:italic'><input name=actual value=Actual style='font-family:#{keyword};font-size:#{keyword};font-weight:#{keyword};font-style:#{keyword}'></body>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom, opts)
      [body] = tree.children
      [input] = body.children
      assert input.style.font_face.id == body.style.font_face.id

      assert input.style.font_size == 20 and input.style.font_weight == 700 and
               input.style.font_style == :italic

      assert {:ok, pdf} = HtmlToPdf.render(html, opts)
      assert {:ok, [field]} = Forms.fields(pdf)
      assert field.name == "actual" and field.value == "Actual"
    end
  end

  test "font percentages use the stable ancestor rather than prior declarations", %{
    opts: opts
  } do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<body style='font-size:20pt'><input style='font-size:75%;font-size:120%' value=Actual></body>"
             )

    assert {:ok, tree} = Style.compute_detailed(dom, opts)
    [body] = tree.children
    [input] = body.children
    assert input.style.font_size == 24
  end

  test "reverting author font rules restores UA defaults while initial retains CSS initial values",
       %{opts: opts, arial: bytes} do
    for keyword <- ["revert", "revert-layer"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<body style='font-family:DejaVu Sans;font-size:20pt'><input style='font-family:DejaVu Sans;font-family:#{keyword};font-size:#{keyword};font-style:#{keyword};font-weight:#{keyword}'></body>"
               )

      assert {:ok, tree} = Style.compute_detailed(dom, opts)
      [body] = tree.children
      [input] = body.children
      assert input.style.font_face.data == bytes and input.style.font_size == 10
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input style='font-family:initial;font-size:initial;font-style:initial;font-weight:initial'>"
             )

    assert {:ok, tree} = Style.compute_detailed(dom, opts)
    [input] = tree.children
    assert input.style.font_families == ["serif"] and input.style.font_size == 12
  end

  test "disabling discovery uses only authorized registry fallback when Arial was not supplied" do
    opts = [system_font_discovery: false, fonts: [%{family: "DejaVu Sans", path: @sans}]]
    assert {:ok, dom} = HtmlParser.parse_detailed("<input value=Actual>")
    assert {:ok, tree} = Style.compute_detailed(dom, opts)
    [input] = tree.children
    assert input.style.font_families == ["Arial", "sans-serif"]
    assert input.style.font_face.family == "DejaVu Sans"
    assert input.style.font_face.data == File.read!(@sans)
    assert input.style.font_face.source == :configured
  end

  test "invalid authored font values retain the original public CSS failure" do
    for declaration <- ["font-style:sideways", "font-weight:heavy", "font-size:banana"] do
      assert {:error, {:invalid_css, diagnostic}} =
               HtmlToPdf.render("<input style='#{declaration}' value=Actual>")

      assert diagnostic.stage == :css and diagnostic.operation == :render
      assert diagnostic.source == declaration
      assert is_integer(diagnostic.line) and is_integer(diagnostic.column)
    end
  end

  test "malformed caller font bytes are rejected before UA font selection" do
    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render("<input value=Actual>",
               system_font_discovery: false,
               fonts: [%{family: "Arial", data: "not a font"}]
             )

    assert diagnostic.stage == :style and diagnostic.operation == :render and
             diagnostic.message =~ "fonts"
  end

  test "prepared SVG sources use canonical layout validation in ordinary formatting parents" do
    for display <- ["block", "flex", "grid"] do
      html =
        "<div style='display:#{display}'><svg width=40 height=20><rect width=40 height=20 fill=red></rect></svg></div>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom)

      assert :ok =
               HtmlValidator.validate_layout_input(
                 tree,
                 [],
                 {:ok, {100, 100}},
                 {:ok, %{top: 0, right: 0, bottom: 0, left: 0}}
               )

      [parent] = tree.children
      [svg] = parent.children
      invalid = %{svg | style: Map.put(svg.style, :css_transform, :invalid)}

      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{tree | children: [%{parent | children: [invalid]}]})

      assert diagnostic.stage == :layout and is_binary(diagnostic.message)
    end
  end

  test "SVG extraction retains HTML integration namespaces and original source positions" do
    html = "<svg>\n<foreignObject><p>Actual source</p></foreignObject></svg>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [svg] = dom.children
    foreign = Enum.find(svg.children, &(Map.get(&1, :tag) == "foreignObject"))
    [paragraph] = foreign.children
    assert svg.namespace == :svg and foreign.namespace == :svg and paragraph.namespace == :html

    assert foreign.source.text == "<foreignObject>" and foreign.source.line == 2 and
             foreign.source.column == 1

    assert paragraph.parent_id == foreign.node_id and paragraph.document_id == svg.document_id
    assert [%{text: "Actual source"}] = paragraph.children
  end
end
