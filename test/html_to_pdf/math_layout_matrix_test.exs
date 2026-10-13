defmodule NativeElixirPdfUtilities.HtmlToPdf.MathLayoutMatrixTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, MathLayout, ResourceContext, Style}

  alias NativeElixirPdfUtilities.Validators.{
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
          matrix([[%{ordinary | attributes: %{"columnspan" => "2"}}]], style),
          element("mtable", [ordinary], %{style | display: :inline_table}),
          ordinary,
          matrix(
            [[%{ordinary | style: Map.put(ordinary.style, :background_color, {1, 0, 0})}]],
            style
          ),
          %{
            matrix([[ordinary]], style)
            | style: Map.put(%{style | display: :inline_table}, :width, 50)
          }
        ] do
      assert {:error, {reason, diagnostic}} = MathLayout.measure(node)
      assert reason == diagnostic.reason
      assert diagnostic.stage == :layout and is_binary(diagnostic.message)
      assert diagnostic.source in ["mtable", "mtr", "mtd"]
    end

    assert {:error, {:unsupported_html, _}} =
             HtmlToPdf.render(
               "<math><mtable><mtr><mtd><msup><mn>1</mn><mn>2</mn></msup></mtd></mtr></mtable></math>",
               system_font_discovery: false
             )

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

  test "owner placement validates final decorated bounds while spacing/wrapper geometry remains pending",
       %{style: style} do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    node = matrix([[cell("1", style, zero)]], style)

    for alteration <- [
          %{border_spacing: {4.0, 2.0}},
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
end
