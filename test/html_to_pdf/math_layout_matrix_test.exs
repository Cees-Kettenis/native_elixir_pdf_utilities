defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutMatrixTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, ResourceContext, Style}

  alias NativeElixirPdfUtilities.Validators.{
    CssValueValidator,
    FontValidator,
    HtmlValidator,
    MathLayoutValidator,
    MathTableValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("math", 400, :normal, registry)

    style = %{
      display: :block,
      font_size: 20.0,
      font_face: font,
      color: {0, 0, 0},
      line_height: 24.0,
      math_style: :compact,
      text_transform: :none
    }

    {:ok, style: style, font: font}
  end

  defp element(tag, children, style, attributes \\ %{}) do
    %{
      type: :element,
      namespace: :mathml,
      tag: tag,
      children: children,
      style: style,
      attributes: attributes
    }
  end

  defp token(text, style), do: element("mn", [%{type: :text, text: text, style: style}], style)

  defp cell(text, style, padding, attributes \\ %{}) do
    element(
      "mtd",
      [token(text, style)],
      Map.merge(style, %{display: :table_cell, padding: padding, text_align: :center}),
      attributes
    )
  end

  defp matrix(rows, style) do
    element("mtable", Enum.map(rows, &element("mtr", &1, %{style | display: :table_row})), %{
      style
      | display: :inline_table
    })
  end

  defp styled_math(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false),
           do: {:ok, node.style._math_box}
    end)
  end

  test "natural columns, row baselines and selected MATH axis determine placement independently",
       %{style: style, font: font} do
    padding = %{top: 3.0, right: 8.0, bottom: 3.0, left: 8.0}

    cells = [
      [cell("1", style, padding), cell("234", style, padding)],
      [cell("56", style, padding), cell("7", style, padding)]
    ]

    node = matrix(cells, style)
    assert {:ok, one} = MathLayout.measure(token("1", style))
    assert {:ok, long} = MathLayout.measure(token("234", style))
    assert {:ok, two} = MathLayout.measure(token("56", style))
    assert {:ok, seven} = MathLayout.measure(token("7", style))
    assert {:ok, metrics} = Font.math_metrics(font)
    axis = metrics.constants.axis_height.value * 20 / metrics.units_per_em
    a1 = max(one.ascent, long.ascent) + 3
    d1 = max(one.descent, long.descent) + 3
    a2 = max(two.ascent, seven.ascent) + 3
    d2 = max(two.descent, seven.descent) + 3
    width1 = max(one.max_content_width, two.max_content_width) + 16
    width2 = max(long.max_content_width, seven.max_content_width) + 16
    assert {:ok, measured} = MathLayout.measure(node)
    assert_in_delta measured.width, width1 + width2, 1.0e-9
    assert_in_delta measured.ascent, (a1 + d1 + a2 + d2) / 2 + axis, 1.0e-9
    assert_in_delta measured.descent, (a1 + d1 + a2 + d2) / 2 - axis, 1.0e-9
    assert measured.actual_text == "1234567"
    assert :ok == MathLayoutValidator.validate_measured_geometry(measured)
    first = hd(measured.glyphs)
    assert_in_delta first.x, 8 + (width1 - 16 - one.width) / 2 + hd(one.glyphs).x, 1.0e-9
    assert_in_delta first.y, measured.ascent - a1 + hd(one.glyphs).y, 1.0e-9
    second_row = Enum.at(measured.glyphs, 4)
    assert_in_delta second_row.y, measured.ascent - a1 - d1 - a2 + hd(two.glyphs).y, 1.0e-9
  end

  test "ragged/empty cells preserve order and selected final font default padding", %{font: font} do
    assert {:ok, x_height} = FontValidator.prepare_x_height(font)

    assert {:ok, padded} =
             MathTableValidator.resolve_padding(%{
               font_face: font,
               font_size: 18.0,
               padding: %{
                 top: {:math_cell_ex, 0.5},
                 bottom: {:math_cell_ex, 0.5},
                 left: {:math_cell_em, 0.4},
                 right: {:math_cell_em, 0.4}
               }
             })

    assert_in_delta padded.padding.top, 9 * x_height, 1.0e-9
    assert_in_delta padded.padding.left, 7.2, 1.0e-9

    for html <- [
          "<math style='font-size:18pt'><mtable><mtr><mtd><mn>1</mn></mtd><mtd><mn>23</mn></mtd></mtr><mtr><mtd><mn>4</mn></mtd></mtr></mtable><mn>5</mn></math>",
          "<math style='font-size:18pt'><mtable><mtr><mtd></mtd><mtd><mn>α</mn></mtd></mtr></mtable><mn>5</mn></math>",
          "<math><mtable></mtable><mn>5</mn></math>"
        ] do
      assert {:ok, measured} = styled_math(html)
      assert :ok == MathLayoutValidator.validate_measured_geometry(measured)
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><mtable><mtr><mtd><mn>α</mn></mtd><mtd><mn>12</mn></mtd></mtr><mtr><mtd><mn>β</mn></mtd></mtr></mtable></math>",
               system_font_discovery: false
             )

    assert {:ok, text} = Text.extract(pdf)
    assert String.replace(text, ~r/\s+/u, "") == "α12β"
  end

  test "table structure/span validation and pending composition return shared diagnostics", %{
    style: style
  } do
    padding = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    ordinary = cell("1", style, padding)

    assert {:ok, _} =
             MathLayout.measure(
               matrix(
                 [[%{ordinary | attributes: %{"columnspan" => "bad", "rowspan" => "-2"}}]],
                 style
               )
             )

    for node <- [
          element("mtable", [ordinary], %{style | display: :inline_table}),
          ordinary,
          element(
            "mtable",
            [
              element(
                "mtr",
                [ordinary],
                Map.merge(style, %{display: :table_row, background_image: "pending.png"})
              )
            ],
            %{style | display: :inline_table}
          )
        ] do
      assert {:error, {reason, diagnostic}} = MathLayout.measure(node)
      assert reason == diagnostic.reason
      assert diagnostic.stage == :layout and is_binary(diagnostic.message)
      assert diagnostic.source in ["mtable", "mtr", "mtd"]
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><mtable><mtr><mtd><msup><mn>1</mn><mn>2</mn></msup></mtd></mtr></mtable></math>",
               system_font_discovery: false
             )

    assert {:ok, "12"} = Text.extract(pdf, layout: false)

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<math><mtable><mtr><mtd><mtext>אב</mtext></mtd></mtr></mtable></math>",
               system_font_discovery: false
             )

    assert diagnostic.source == "אב" and diagnostic.message =~ "bidi"
    assert diagnostic.operation == :measure_math and diagnostic.stage == :layout

    Limits.install(Map.put(Limits.effective(), :max_table_grid_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathLayout.measure(matrix([[ordinary]], style))

    assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
  end

  test "public measured placement rejects malformed preferred fields/shapes and numeric overflow",
       %{style: style} do
    padding = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(
               matrix([[cell("1", style, padding)]], style),
               %{}
             )

    assert {:ok, measured} = MathLayout.measure(token("1", style))
    assert {:ok, _} = MathTableValidator.prepare_geometry(prepared, [[measured]])

    assert {:error, {_, diagnostic}} =
             MathTableValidator.prepare_geometry(prepared, [
               [Map.delete(measured, :min_content_width)]
             ])

    assert diagnostic.source == "mtd"

    for {state, values} <- [
          {prepared, []},
          {prepared, [[Map.delete(measured, :min_content_width)]]},
          {prepared, [[%{measured | max_content_width: "bad"}]]},
          {%{prepared | axis: "bad"}, [[measured]]},
          {%{prepared | rows: [[nil]]}, [[measured]]},
          {%{}, []}
        ] do
      assert {:error, {reason, diagnostic}} = MathTableValidator.prepare_geometry(state, values)
      assert diagnostic.reason == reason and diagnostic.stage == :layout
      assert is_binary(diagnostic.message) and diagnostic.operation == :measure_math
    end

    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 30))

    large = %{
      prepared
      | rows: [
          [
            %{
              hd(hd(prepared.rows))
              | style:
                  Map.put(style, :padding, %{top: 20.0, right: 20.0, bottom: 20.0, left: 20.0})
            }
          ]
        ]
    }

    assert {:error, {_, diagnostic}} = MathTableValidator.prepare_geometry(large, [[measured]])
    assert diagnostic.source == "mtable"
  end

  test "cell padding and matrix placement reject unavailable context and malformed state without raising",
       %{style: style, font: font} do
    padding = %{top: {:math_cell_ex, 0.5}, bottom: 0.0, left: 0.0, right: 0.0}

    for invalid <- [
          nil,
          %{padding: padding},
          %{font_face: font, font_size: -1.0, padding: padding},
          %{font_face: font, font_size: 20.0, padding: %{padding | top: {:math_cell_ex, "bad"}}},
          %{font_face: font, font_size: 1.0e100, padding: padding}
        ] do
      assert {:error, {reason, diagnostic}} = MathTableValidator.resolve_padding(invalid)
      assert diagnostic.reason == reason and diagnostic.source == "mtd"
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    end

    assert {:error, {_, diagnostic}} =
             MathTableValidator.prepare(matrix([], %{style | font_size: 1.0e100}), %{})

    assert diagnostic.source == "mtable"

    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(matrix([[cell("1", style, zero)]], style), %{})

    assert {:ok, measured} = MathLayout.measure(token("1", style))
    original = hd(hd(prepared.rows))

    for altered <- [
          %{original | column: -1},
          %{original | style: Map.put(original.style, :padding, nil)},
          %{original | style: Map.put(original.style, :border_widths, "bad")},
          %{original | style: Map.put(original.style, :margin, nil)}
        ] do
      assert {:error, {_, diagnostic}} =
               MathTableValidator.prepare_geometry(%{prepared | rows: [[altered]]}, [[measured]])

      assert diagnostic.source == "mtd"
    end

    assert {:error, {_, diagnostic}} =
             MathTableValidator.prepare_geometry(%{prepared | rows: [[original, original]]}, [
               [measured, measured]
             ])

    assert diagnostic.source == "mtd"

    visible = "<mtable><mtr><mtd><mn>α</mn></mtd><mtd><mn>12</mn></mtd></mtr></mtable>"
    assert {:ok, hidden} = styled_math("<math><mphantom>#{visible}</mphantom><mn>3</mn></math>")
    assert hidden.actual_text == "3"
    assert {:ok, shown} = styled_math("<math>#{visible}<mn>3</mn></math>")
    assert hidden.width == shown.width
    assert hidden.ascent == shown.ascent and hidden.descent == shown.descent
  end

  test "table grammar shares ASCII whitespace trimming without discarding visible Unicode", %{
    style: style
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    plain = matrix([[cell("1", style, zero)]], style)
    assert {:ok, expected} = MathLayout.measure(plain)

    for text <- ["", " ", "\t", "\n", "\r", "\f", " \t\n\r\f "] do
      whitespace = %{type: :text, text: text, style: style}
      [row] = plain.children

      altered = %{
        plain
        | children: [whitespace, %{row | children: [whitespace | row.children]}, whitespace]
      }

      assert {:ok, measured} = MathLayout.measure(altered)
      assert Map.drop(measured, [:drawing_items]) == Map.drop(expected, [:drawing_items])

      assert Enum.map(measured.drawing_items, &Map.drop(&1, [:logical_group])) ==
               Enum.map(expected.drawing_items, &Map.drop(&1, [:logical_group]))
    end

    for text <- ["\u00A0", "x", "\u2003", "\u2028"] do
      visible = %{type: :text, text: text, style: style}
      [row] = plain.children

      for altered <- [
            %{plain | children: [visible | plain.children]},
            %{plain | children: [%{row | children: [visible | row.children]}]}
          ] do
        assert {:error, {reason, diagnostic}} = MathLayout.measure(altered)
        assert diagnostic.reason == reason and diagnostic.stage == :layout
        assert diagnostic.source in ["mtable", "mtr"]
      end
    end
  end

  @tag :matrix_owner_axis
  test "owner padding and borders keep the whole decorated table centered on the selected axis",
       %{style: style, font: font} do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    natural = matrix([[cell("12", style, zero)], [cell("34", style, zero)]], style)
    assert {:ok, base} = MathLayout.measure(natural)

    owner =
      Map.merge(natural.style, %{
        padding: %{top: 7.0, right: 4.0, bottom: 1.0, left: 2.0},
        border_widths: %{top: 1.0, right: 1.0, bottom: 1.0, left: 1.0},
        border_color: {0, 0, 1},
        border_style: :solid,
        background_color: {1, 1, 0}
      })

    assert {:ok, measured} =
             MathLayout.measure(element("math", [%{natural | style: owner}], style))

    assert {:ok, metrics} = Font.math_metrics(font)
    axis = metrics.constants.axis_height.value * style.font_size / metrics.units_per_em
    height = base.ascent + base.descent + 10
    assert_in_delta measured.ascent, height / 2 + axis, 1.0e-9
    assert_in_delta measured.descent, height / 2 - axis, 1.0e-9
    assert_in_delta measured.width, base.width + 8, 1.0e-9
    assert_in_delta hd(measured.glyphs).y, hd(base.glyphs).y - 3, 1.0e-9
    assert_in_delta hd(measured.glyphs).x, hd(base.glyphs).x + 3, 1.0e-9
    assert measured.actual_text == base.actual_text
    assert [background | _] = measured.drawing_items
    assert background.kind == :background
    assert_in_delta background.height, height, 1.0e-9
    assert_in_delta background.y, -measured.descent, 1.0e-9
    assert :ok == MathLayoutValidator.validate_measured_geometry(measured)
  end

  test "owner placement validates final decorated bounds and unsupported wrapper geometry",
       %{style: style} do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    node = matrix([[cell("1", style, zero)]], style)

    for alteration <- [
          %{padding: nil},
          %{padding: %{zero | top: -1.0}},
          %{margin: %{zero | left: 1.0}}
        ] do
      assert {:error, {reason, diagnostic}} =
               MathLayout.measure(%{node | style: Map.merge(node.style, alteration)})

      assert diagnostic.reason == reason and diagnostic.source == "mtable"
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    end

    assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
    assert {:ok, child} = MathLayout.measure(token("1", style))
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 30))

    boxed = %{
      prepared
      | style: Map.put(prepared.style, :padding, %{zero | left: 20.0, right: 20.0})
    }

    assert {:error, {_, diagnostic}} = MathTableValidator.prepare_geometry(boxed, [[child]])
    assert diagnostic.source == "mtable" and diagnostic.message =~ "decorated"
  end

  test "cell borders and padding determine allocated paint, common baselines and original identities",
       %{style: style} do
    padding = %{top: 1.0, right: 2.0, bottom: 3.0, left: 4.0}
    border = %{top: 1.0, right: 1.0, bottom: 1.0, left: 1.0}

    paint =
      Map.merge(style, %{
        border_widths: border,
        border_color: {0, 0, 1},
        border_style: :solid,
        background_color: {1, 1, 0}
      })

    first =
      cell("1", style, padding)
      |> Map.put(:_selector_id, 41)
      |> Map.put(:_document_order, [0, 0, 0])

    first = %{
      first
      | style:
          Map.merge(
            first.style,
            Map.take(paint, [:border_widths, :border_color, :border_style, :background_color])
          )
    }

    last = cell("234", style, padding)

    last = %{
      last
      | style:
          Map.merge(
            last.style,
            Map.take(paint, [:border_widths, :border_color, :border_style, :background_color])
          )
    }

    node = matrix([[first], [last]], style)
    assert {:ok, one} = MathLayout.measure(token("1", style))
    assert {:ok, longer} = MathLayout.measure(token("234", style))
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
    assert hd(hd(prepared.rows)).source._selector_id == 41
    assert hd(hd(prepared.rows)).source._document_order == [0, 0, 0]
    assert hd(hd(prepared.rows)).source.tag == "mtd"
    assert {:ok, measured} = MathLayout.measure(node)
    expected_width = max(one.max_content_width, longer.max_content_width) + 8
    assert_in_delta measured.width, expected_width, 1.0e-9
    backgrounds = Enum.filter(measured.drawing_items, &(&1.kind == :background))
    assert length(backgrounds) == 2
    first_box = hd(backgrounds)
    assert_in_delta first_box.width, expected_width, 1.0e-9
    assert_in_delta first_box.height, one.ascent + one.descent + 6, 1.0e-9
    assert first_box.style._math_source._selector_id == 41
    assert first_box.style._math_source.tag == "mtd"

    assert_in_delta hd(measured.glyphs).x,
                    5 + (expected_width - 8 - one.width) / 2 + hd(one.glyphs).x,
                    1.0e-9

    assert measured.actual_text == "1234"
    assert :ok == MathLayoutValidator.validate_measured_geometry(measured)
  end

  test "allocated empty and mixed-size cells preserve glyphs, Unicode and phantom suppression", %{
    style: style
  } do
    html =
      "<mtable><mtr><mtd style='background-color:lightyellow;border:1px solid blue'><mn>α</mn></mtd><mtd style='font-size:24pt;background-color:lightblue;border:1px solid blue'><mn>12</mn></mtd></mtr><mtr><mtd style='background-color:lightblue;border:1px solid blue'></mtd><mtd style='background-color:lightyellow;border:1px solid blue'><mn>β</mn></mtd></mtr></mtable>"

    assert {:ok, visible} = styled_math("<math style='font-size:18pt'>#{html}</math>")
    assert length(Enum.filter(visible.drawing_items, &(&1.kind == :background))) == 4
    assert visible.actual_text == "α12β"

    assert {:ok, hidden} =
             styled_math("<math style='font-size:18pt'><mphantom>#{html}</mphantom></math>")

    assert hidden.glyphs == [] and hidden.drawing_items == [] and hidden.actual_text == ""

    assert hidden.width == visible.width and hidden.ascent == visible.ascent and
             hidden.descent == visible.descent

    assert {:ok, pdf} = HtmlToPdf.render("<math>#{html}</math>", system_font_discovery: false)
    assert {:ok, "α12β"} = Text.extract(pdf, layout: false)
    assert :ok == MathLayoutValidator.validate_measured_geometry(visible)

    assert {:ok, plain} =
             MathLayout.measure(
               matrix(
                 [[cell("1", style, %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0})]],
                 style
               )
             )

    assert plain.drawing_items |> Enum.all?(&(&1.kind == :glyph))
    assert {:ok, glyph} = MathLayout.measure(token("1", style))

    assert_in_delta plain.ink_ascent,
                    glyph.ink_ascent + hd(plain.glyphs).y - hd(glyph.glyphs).y,
                    1.0e-9
  end

  test "cell decoration retains bounded prepared geometry and pending row/span/spacing policy", %{
    style: style
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    ordinary = cell("1", style, zero)

    for alteration <- [
          %{border_widths: nil},
          %{border_widths: %{zero | left: -1.0}},
          %{margin: %{}},
          %{background_image: "blocked"}
        ] do
      assert {:error, {reason, diagnostic}} =
               MathLayout.measure(
                 matrix([[%{ordinary | style: Map.merge(ordinary.style, alteration)}]], style)
               )

      assert diagnostic.reason == reason and diagnostic.source == "mtd"
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    end

    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 30))
    border = %{top: 20.0, right: 20.0, bottom: 20.0, left: 20.0}

    assert {:error, {_, diagnostic}} =
             MathLayout.measure(
               matrix(
                 [[%{ordinary | style: Map.put(ordinary.style, :border_widths, border)}]],
                 style
               )
             )

    assert diagnostic.source == "mtable" and diagnostic.message =~ "bounds"
  end

  test "row backgrounds precede allocated cell paint and retain original row identity", %{
    style: style
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    first = cell("1", style, zero)

    colored = %{
      cell("23", style, zero)
      | style: Map.merge(first.style, %{background_color: {0, 0, 1}})
    }

    node = matrix([[first, colored], [cell("456", style, zero)]], style)
    [row1, row2] = node.children
    row1 = Map.put(row1, :_selector_id, 73)
    row1 = %{row1 | style: Map.put(row1.style, :background_color, {1, 1, 0})}
    row2 = %{row2 | style: Map.put(row2.style, :background_color, {0, 1, 0})}
    node = %{node | children: [row1, row2]}

    assert {:ok, plain} =
             MathLayout.measure(matrix([[first, colored], [cell("456", style, zero)]], style))

    assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})
    assert hd(prepared.row_boxes).source._selector_id == 73
    assert {:ok, measured} = MathLayout.measure(node)

    assert measured.width == plain.width and measured.ascent == plain.ascent and
             measured.descent == plain.descent

    assert measured.glyphs == plain.glyphs and measured.actual_text == "123456"
    backgrounds = Enum.filter(measured.drawing_items, &(&1.kind == :background))
    assert length(backgrounds) == 4

    assert Enum.map(backgrounds, & &1.style._math_source.tag) == [
             "mtr",
             "mtr",
             "mtr",
             "mtd"
           ]

    assert hd(backgrounds).style._math_source._selector_id == 73
    [first_bg, second_bg, ragged_bg, _] = backgrounds
    assert_in_delta second_bg.x, first_bg.width, 1.0e-9
    assert_in_delta first_bg.width + second_bg.width, measured.width, 1.0e-9
    assert_in_delta ragged_bg.width, first_bg.width, 1.0e-9
    assert ragged_bg.width < measured.width
    assert :ok == MathLayoutValidator.validate_measured_geometry(measured)

    assert {:ok, phantom} =
             styled_math(
               "<math><mphantom><mtable><mtr style='background-color:yellow'><mtd><mn>α</mn></mtd></mtr></mtable></mphantom><mn>β</mn></math>"
             )

    assert phantom.actual_text == "β" and Enum.all?(phantom.drawing_items, &(&1.kind == :glyph))
  end

  test "prepared row paint rejects invalid sources, row edges and mismatched row metadata", %{
    style: style
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(matrix([[cell("1", style, zero)]], style), %{})

    assert {:ok, measured} = MathLayout.measure(token("1", style))
    [row] = prepared.row_boxes

    for rows <- [
          [],
          [nil],
          [%{row | source: %{namespace: :html, tag: "mtr"}}],
          [%{row | style: Map.put(row.style, :border_widths, %{zero | top: 1})}],
          [%{row | style: Map.put(row.style, :padding, %{zero | left: 1})}],
          [%{row | style: Map.put(row.style, :background_image, "pending")}]
        ] do
      assert {:error, {reason, diagnostic}} =
               MathTableValidator.prepare_geometry(%{prepared | row_boxes: rows}, [[measured]])

      assert diagnostic.reason == reason and diagnostic.source in ["mtr", "mtable"]
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    end
  end

  test "exact integer CSS-pixel spacing contributes unchanged point lengths to intrinsic sizes and gaps",
       %{style: style} do
    padding = %{top: 2.0, right: 3.0, bottom: 2.0, left: 3.0}

    natural =
      matrix(
        [
          [cell("1", style, padding), cell("23", style, padding)],
          [cell("45", style, padding), cell("6", style, padding)]
        ],
        style
      )

    assert {:ok, base} = MathLayout.measure(natural)

    assert {:ok, parsed} =
             CssValueValidator.validate_resolved(natural.style, "border-spacing", "4px 2px")

    assert parsed.border_spacing == {3.0, 1.5}
    assert {:ok, measured} = MathLayout.measure(%{natural | style: parsed})

    for field <- [:width, :min_content_width, :max_content_width] do
      assert_in_delta Map.fetch!(measured, field), Map.fetch!(base, field) + 9, 1.0e-9
    end

    assert_in_delta measured.ascent, base.ascent + 2.25, 1.0e-9
    assert_in_delta measured.descent, base.descent + 2.25, 1.0e-9
    assert_in_delta hd(measured.glyphs).x, hd(base.glyphs).x + 3, 1.0e-9
    assert_in_delta hd(measured.glyphs).y, hd(base.glyphs).y + 0.75, 1.0e-9
    assert_in_delta Enum.at(measured.glyphs, 3).y, Enum.at(base.glyphs, 3).y - 0.75, 1.0e-9
    assert measured.actual_text == base.actual_text
    assert :ok == MathLayoutValidator.validate_measured_geometry(measured)
  end

  test "separate gaps expose owner paint and remain outside row and cell background rectangles",
       %{style: style} do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    ordinary = cell("1", style, zero)
    colored = %{ordinary | style: Map.put(ordinary.style, :background_color, {0, 0, 1})}
    natural = matrix([[colored, ordinary]], style)
    [row] = natural.children
    row = %{row | style: Map.put(row.style, :background_color, {1, 1, 0})}

    node = %{
      natural
      | children: [row],
        style:
          Map.merge(natural.style, %{
            border_spacing: {3.0, 1.5},
            background_color: {0, 1, 0}
          })
    }

    assert {:ok, measured} = MathLayout.measure(element("math", [node], style))
    [owner, row1, row2, cell_bg] = Enum.filter(measured.drawing_items, &(&1.kind == :background))
    assert owner.width == measured.width and owner.height == measured.ascent + measured.descent
    assert measured.ink_ascent >= measured.ascent and measured.ink_descent >= measured.descent
    assert row1.x == 3.0 and row2.x == row1.x + row1.width + 3.0
    assert cell_bg.x == row1.x and cell_bg.width == row1.width
    assert_in_delta row2.x + row2.width + 3.0, measured.width, 1.0e-9
    assert_in_delta owner.height, row1.height + 3.0, 1.0e-9
    assert Enum.map([row1, row2, cell_bg], & &1.style._math_source.tag) == ["mtr", "mtr", "mtd"]
    assert measured.actual_text == "11"
  end

  test "continuous spacing and empty grids retain exact lengths and numeric bounds",
       %{style: style} do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    natural = matrix([[cell("1", style, zero)]], style)
    assert :error == CssValueValidator.validate_resolved(natural.style, "border-spacing", "-1px")
    assert :error == CssValueValidator.validate_resolved(natural.style, "border-spacing", "10%")

    assert {:ok, ordinary} = MathLayout.measure(natural)

    for {horizontal, vertical} = spacing <- [{4.0, 2.0}, {0.375, 0.0}] do
      assert {:ok, spaced} =
               MathLayout.measure(%{
                 natural
                 | style: Map.put(natural.style, :border_spacing, spacing)
               })

      assert_in_delta spaced.width, ordinary.width + 2 * horizontal, 1.0e-9

      assert_in_delta spaced.ascent + spaced.descent,
                      ordinary.ascent + ordinary.descent + 2 * vertical,
                      1.0e-9
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             MathLayout.measure(%{
               natural
               | style: Map.put(natural.style, :border_spacing, {-0.75, 0.0})
             })

    assert diagnostic.source == "mtable" and diagnostic.operation == :measure_math

    for rows <- [[], [[]], [[], []]] do
      empty = matrix(rows, style)
      assert {:ok, zero_spacing} = MathLayout.measure(empty)

      assert {:ok, spaced} =
               MathLayout.measure(%{
                 empty
                 | style: Map.put(empty.style, :border_spacing, {3.0, 1.5})
               })

      assert spaced.width == zero_spacing.width
      assert spaced.ascent == zero_spacing.ascent and spaced.descent == zero_spacing.descent
    end

    assert {:ok, prepared} = MathLayoutValidator.prepare_element(natural, %{})
    assert {:ok, child} = MathLayout.measure(token("1", style))
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 30))
    wide = %{prepared | style: Map.put(prepared.style, :border_spacing, {15.0, 0.0})}

    assert {:error, {:unsupported_html, diagnostic}} =
             MathTableValidator.prepare_geometry(wide, [[child]])

    assert diagnostic.source == "mtable" and diagnostic.message =~ "bounds"
  end

  test "nonzero spacing composes fractions through nested rows and phantom", %{
    style: style
  } do
    for content <- [
          "<mfrac><mn>1</mn><mn>2</mn></mfrac>",
          "<mrow><mfrac><mn>1</mn><mn>2</mn></mfrac></mrow>",
          "<mphantom><mfrac><mn>1</mn><mn>2</mn></mfrac></mphantom>"
        ] do
      assert {:ok, natural} =
               styled_math("<math><mtable><mtr><mtd>#{content}</mtd></mtr></mtable></math>")

      assert {:ok, spaced} =
               styled_math(
                 "<math><mtable style='border-spacing:4px 2px'><mtr><mtd>#{content}</mtd></mtr></mtable></math>"
               )

      assert_in_delta spaced.width, natural.width + 6, 1.0e-9
      assert_in_delta spaced.ascent + spaced.descent, natural.ascent + natural.descent + 3, 1.0e-9
      assert spaced.actual_text == natural.actual_text
    end

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(
               matrix(
                 [[cell("1", style, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})]],
                 style
               ),
               %{}
             )

    assert {:ok, child} = MathLayout.measure(token("1", style))
    original = hd(hd(prepared.rows))

    for content <- [
          nil,
          Map.put(original.content, :children, [hd(original.content.children) | :bad_tail])
        ] do
      state = %{
        prepared
        | style: Map.put(prepared.style, :border_spacing, {3.0, 1.5}),
          rows: [[%{original | content: content}]]
      }

      assert {:error, {:unsupported_html, diagnostic}} =
               MathTableValidator.prepare_geometry(state, [[child]])

      assert diagnostic.source == "mtd" and diagnostic.operation == :measure_math
    end
  end

  test "spaced prepared content admission uses existing depth and work limits", %{style: style} do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(matrix([[cell("1", style, zero)]], style), %{})

    assert {:ok, child} = MathLayout.measure(token("1", style))
    spaced = %{prepared | style: Map.put(prepared.style, :border_spacing, {3.0, 1.5})}
    limits = Limits.effective()
    Limits.install(Map.put(limits, :max_html_depth, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathTableValidator.prepare_geometry(spaced, [[child]])

    assert diagnostic.source == "mtd" and diagnostic.message =~ "max_html_depth"
    assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    Limits.install(Map.put(limits, :max_table_grid_work, 3))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathTableValidator.prepare_geometry(spaced, [[child]])

    assert diagnostic.message =~ "max_table_grid_work" and diagnostic.operation == :measure_math
  end
end
