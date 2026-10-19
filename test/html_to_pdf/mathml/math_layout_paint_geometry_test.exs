defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutPaintGeometryTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{DrawingGeometry, HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator, MathPaintValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "selected outline paint retains a prescribed advance and displaced ink independently" do
    box =
      measured(
        "<math style='font-size:36pt'><mpadded width='10pt' height='5pt' depth='0pt' lspace='140pt'><mn>7</mn></mpadded></math>"
      )

    assert box.width == 10.0 and box.ascent == 5.0 and box.descent == 0.0
    assert {:ok, {left, bottom, right, top}} = MathPaintValidator.prepare_geometry(box)
    assert_in_delta left, 141.512, 1.0e-8
    assert_in_delta right, 156.704, 1.0e-8
    assert_in_delta bottom, -0.288, 1.0e-8
    assert_in_delta top, 22.68, 1.0e-8
    assert box.actual_text == "7"
  end

  test "phantom keeps layout metrics while its hidden content contributes no paint" do
    box = measured("<math><mphantom><merror><mn>7</mn></merror></mphantom></math>")
    assert box.width > 0 and box.ascent > 0
    assert {:ok, nil} = MathPaintValidator.prepare_geometry(box)
    assert box.actual_text == ""
  end

  test "fraction rules and error backgrounds contribute allocated paint beyond token outlines" do
    box =
      measured(
        "<math style='font-size:18pt'><merror><mfrac><mn>1</mn><mn>2</mn></mfrac></merror></math>"
      )

    assert Enum.any?(box.drawing_items, &(&1.kind == :background))
    assert Enum.any?(box.drawing_items, &(&1.kind == :native))
    assert {:ok, {left, bottom, right, top}} = MathPaintValidator.prepare_geometry(box)
    assert left == 0.0 and right == box.width
    assert_in_delta top, box.ascent, 1.0e-8
    assert_in_delta bottom, -box.descent, 1.0e-8
  end

  test "native HTML content uses selected outlines through its Math adapter" do
    box =
      measured(
        "<math><mtext><span style='font-family:DejaVu Sans;font-size:18pt;font-style:italic'>jAV</span></mtext></math>"
      )

    assert Enum.any?(box.drawing_items, &(&1.kind == :native))
    assert {:ok, {left, bottom, right, top}} = MathPaintValidator.prepare_geometry(box)
    assert left < 0 and bottom < 0 and right > 0 and top > 0
    assert box.actual_text == "jAV"
  end

  test "native Math background ink follows all corners through an affine paint group" do
    box =
      measured(
        "<math><mtext><span style='background:red;padding:3pt'>Actual</span></mtext></math>"
      )

    rect =
      Enum.find_value(box.drawing_items, fn
        %{kind: :native, box: %{type: :rect} = rect} -> rect
        _ -> nil
      end)

    assert rect != nil

    group = %{
      type: :drawing_group,
      width: 100,
      height: 100,
      transform: {0, 1, -1, 0, 100, 0},
      boxes: [rect]
    }

    measured = %{box | glyphs: [], drawing_items: [%{kind: :native, box: group, x: 0, y: 0}]}
    assert {:ok, {left, bottom, right, top}} = MathPaintValidator.prepare_geometry(measured)
    assert_in_delta left, 100 - rect.y - rect.height, 0.0001
    assert_in_delta right, 100 - rect.y, 0.0001
    assert_in_delta bottom, rect.x, 0.0001
    assert_in_delta top, rect.x + rect.width, 0.0001
  end

  test "positioned native glyph groups measure their original child ink at the issued offset" do
    box =
      measured(
        "<math><mtable style='width:180pt'><mtr><mtd style='padding:0'><mtext style='display:inline;width:60pt;font-family:DejaVu Sans;font-size:12pt;line-height:18pt'><span>Alpha beta gamma delta epsilon</span></mtext></mtd></mtr></mtable></math>"
      )

    groups =
      Enum.filter(box.drawing_items, &match?(%{kind: :native, box: %{type: :glyph_group}}, &1))

    assert groups != []
    box = %{box | drawing_items: groups}
    assert {:ok, {left, bottom, right, top}} = MathPaintValidator.prepare_geometry(box)
    shifted = %{box | drawing_items: Enum.map(groups, &%{&1 | x: &1.x + 11, y: &1.y - 20})}
    assert {:ok, {sl, sb, sr, st}} = MathPaintValidator.prepare_geometry(shifted)
    assert_in_delta sl, left + 11, 1.0e-9
    assert_in_delta sr, right + 11, 1.0e-9
    assert_in_delta sb, bottom - 20, 1.0e-9
    assert_in_delta st, top - 20, 1.0e-9
  end

  test "malformed prepared glyph IDs delegate to actionable selected-font authority" do
    box = measured("<math><mn>7</mn></math>")
    [glyph] = box.glyphs
    broken = %{glyph | glyph_id: 0}
    box = %{box | glyphs: [broken], drawing_items: [Map.put(broken, :kind, :glyph)]}
    assert {:error, {reason, diagnostic}} = MathPaintValidator.prepare_geometry(box)
    assert is_atom(reason) and diagnostic.stage == :layout
    assert diagnostic.operation == :measure_math and is_binary(diagnostic.message)
  end

  test "paint traversal shares the selected-font work budget" do
    box = measured("<math><mn>7</mn></math>")
    Limits.install(%{Limits.effective() | max_font_math_work: 0})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathPaintValidator.prepare_geometry(box)

    assert diagnostic.message =~ "max_font_math_work"
    assert diagnostic.operation == :measure_math
  end

  @tag :math_paint_correction
  test "the unchanged original out-of-page prescribed box returns an actionable failure instead of invisible Unicode" do
    html =
      "<!doctype html><html><head><style>@page{size:120pt 120pt;margin:0}html,body{margin:0;padding:0;font-family:DejaVu Sans;font-size:12pt}</style></head><body>Before <math style='font-size:36pt'><mpadded width='10pt' height='5pt' depth='0pt' lspace='140pt'><mn>7</mn></mpadded></math> end.</body></html>"

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlToPdf.render(html, system_font_discovery: false)

    assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    assert diagnostic.source == "math"
    assert diagnostic.message =~ "156.704"
    assert diagnostic.message =~ "selected-font paint"
  end

  @tag :math_paint_correction
  test "fitting prescribed paint retains ten-point advance and stays entirely on the first page" do
    html =
      "<style>html,body{margin:0;padding:0;font-family:DejaVu Sans;font-size:12pt}</style>Before <math style='font-size:36pt'><mpadded width='10pt' height='5pt' depth='0pt' lspace='140pt'><mn>7</mn></mpadded></math> end."

    prepared = prepared(html, page_size: "240pt 120pt", margin: 0)
    assert [page] = prepared.pages

    [box] =
      for %{type: :glyph_group, boxes: glyphs} <- page.boxes,
          glyph <- glyphs,
          glyph.text == "7",
          do: glyph

    {x, y} = DrawingGeometry.text_position(box, elem(page.size, 1))

    assert {:ok, {left, bottom, right, top}} =
             FontValidator.prepare_text_ink_rectangle(%{box | x: x, y: y})

    assert left >= 0 and right <= 240 and bottom >= 0 and top <= 120,
           inspect(%{
             ink: {left, bottom, right, top},
             glyph:
               Map.take(box, [
                 :x,
                 :y,
                 :font_size,
                 :baseline_snap_depth,
                 :line_baseline_depth,
                 :line_box_height
               ]),
             markers: Enum.filter(page.boxes, &(Map.get(&1, :role) == :math_paint))
           })

    [marker] = Enum.filter(page.boxes, &(Map.get(&1, :role) == :math_paint))
    assert marker.width == 0.0
    assert_in_delta marker.height, 22.968, 1.0e-8

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               page_size: "240pt 120pt",
               margin: 0,
               system_font_discovery: false
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "7" and text =~ "Before" and text =~ "end."
  end

  test "a prescribed equation moves as one complete line when the previous block fills the page" do
    html =
      "<style>html,body{margin:0;padding:0;font-family:DejaVu Sans;font-size:12pt}p{margin:0}</style><p style='height:113pt'>Before</p><p><math style='font-size:36pt'><mpadded width='10pt' height='5pt' depth='0pt'><mn>7</mn></mpadded></math> end.</p>"

    prepared = prepared(html, page_size: "120pt 120pt", margin: 0)
    assert [first, second] = prepared.pages
    refute Enum.any?(first.boxes, &(Map.get(&1, :role) == :math_paint))
    [marker] = Enum.filter(second.boxes, &(Map.get(&1, :role) == :math_paint))
    assert marker.y >= 0 and marker.y + marker.height <= 120

    [glyph] =
      for %{type: :glyph_group, boxes: glyphs} <- second.boxes,
          box <- glyphs,
          box.text == "7",
          do: box

    {x, y} = DrawingGeometry.text_position(glyph, 120)

    assert {:ok, {_left, bottom, _right, top}} =
             FontValidator.prepare_text_ink_rectangle(%{glyph | x: x, y: y})

    assert bottom >= 0 and top <= 120
    assert Enum.any?(second.boxes, &(String.trim(Map.get(&1, :text, "")) == "end."))
  end

  test "clipped Math markers preserve the visible viewport and annotation boxes" do
    prepared =
      prepared(
        "<div style='width:18pt;height:18pt;overflow:hidden'><a href='https://example.test/equation'><math><mn>123456789</mn></math></a></div>",
        page_size: "180pt 120pt",
        margin: 10
      )

    marker = Enum.find(prepared.layout_tree.boxes, &(Map.get(&1, :role) == :math_paint))
    assert marker != nil and marker.math_paint_width <= 18
    assert marker.clip.width == 18
    assert Enum.any?(hd(prepared.pages).boxes, &(&1.type == :link_region))
  end

  test "emitted Math marker malformed geometry and improper collection tails diagnose" do
    prepared = prepared("<math><mn>7</mn></math>", page_size: "180pt 120pt", margin: 10)
    marker = Enum.find(prepared.layout_tree.boxes, &(Map.get(&1, :role) == :math_paint))

    for input <- [
          [%{marker | math_paint_width: "bad"}],
          [%{marker | height: -1}],
          [marker | :bad]
        ] do
      assert {:error, {reason, diagnostic}} =
               MathPaintValidator.prepare_flow_boxes(input, 180, 120)

      assert is_atom(reason) and is_atom(diagnostic.stage) and is_atom(diagnostic.operation)
      assert is_binary(diagnostic.message)
    end

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             MathPaintValidator.prepare_flow_boxes(
               [Map.put(marker, :clip, %{x: 0, y: "bad", width: 10, height: 10})],
               180,
               120
             )

    assert diagnostic.stage == :pdf and diagnostic.message =~ "clip"

    assert {:error, {:invalid_layout, diagnostic}} =
             MathPaintValidator.prepare_flow_boxes([marker], -1, 120)

    assert diagnostic.message =~ "viewport"
  end

  test "proper emitted-marker work reserves the exact link budget before malformed tails" do
    prepared = prepared("<math><mn>7</mn></math>", page_size: "180pt 120pt", margin: 10)
    marker = Enum.find(prepared.layout_tree.boxes, &(Map.get(&1, :role) == :math_paint))
    Limits.install(%{Limits.effective() | max_layout_boxes: 1})
    assert {:ok, [_]} = MathPaintValidator.prepare_flow_boxes([marker], 180, 120)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathPaintValidator.prepare_flow_boxes([marker, marker], 180, 120)

    assert diagnostic.message =~ "max_layout_boxes"

    assert {:error, {:invalid_layout, diagnostic}} =
             MathPaintValidator.prepare_flow_boxes([marker | :bad], 180, 120)

    assert diagnostic.message =~ "proper list"
  end

  defp prepared(html, opts) do
    budget = HtmlValidator.new_image_budget()

    ResourceContext.run([], budget, fn _ ->
      assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

      assert {:ok, document} =
               HtmlToPdf.prepare_document(
                 dom,
                 Keyword.put(opts, :system_font_discovery, false),
                 budget
               )

      document
    end)
  end

  defp measured(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      assert {:ok, dom} = HtmlParser.parse_detailed(html)

      assert {:ok, %{children: [node]}} =
               Style.compute_detailed(dom, system_font_discovery: false)

      node.style._math_box
    end)
  end
end
