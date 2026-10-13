defmodule NativeElixirPdfUtilities.HtmlToPdf.MathTableValidatorBoundariesTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, MathLayout}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
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

    {:ok, style: style, registry: registry, limits: limits}
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

  test "mixed authored and font-relative cell padding uses each actual selected final face", %{
    style: style,
    registry: registry
  } do
    for family <- ["math", "DejaVu Sans"] do
      assert {:ok, _, font} = Font.resolve(family, 400, :normal, registry)
      assert {:ok, x_height} = FontValidator.prepare_x_height(font)

      final =
        Map.merge(style, %{
          font_face: font,
          font_size: 24.0,
          padding: %{
            top: {:math_cell_ex, 0.5},
            bottom: 3.0,
            left: {:math_cell_em, 0.4},
            right: 7.0
          }
        })

      assert {:ok, resolved} = MathTableValidator.resolve_padding(final)
      assert resolved.font_face == font
      assert_in_delta resolved.padding.top, x_height * 12, 1.0e-9
      assert_in_delta resolved.padding.left, 9.6, 1.0e-9
      assert resolved.padding.bottom == 3.0 and resolved.padding.right == 7.0
    end
  end

  test "resolved font-relative padding is bounded after multiplication", %{style: style} do
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 100))

    deferred =
      Map.put(style, :padding, %{top: {:math_cell_em, 6}, right: 0.0, bottom: 0.0, left: 0.0})

    assert {:error, {:unsupported_html, diagnostic}} =
             MathTableValidator.resolve_padding(deferred)

    assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    assert diagnostic.source == "mtd" and diagnostic.message =~ "exceeds shared numeric bounds"
  end

  test "ordinary padding remains with the cell validator, including malformed public style", %{
    style: style
  } do
    malformed = Map.put(style, :padding, nil)
    assert {:ok, ^malformed} = MathTableValidator.resolve_padding(malformed)
    node = matrix([[cell("1", style, nil)]], style)
    assert {:error, {:unsupported_html, diagnostic}} = MathTableValidator.prepare(node, %{})
    assert diagnostic.source == "mtd" and diagnostic.stage == :layout
    assert diagnostic.operation == :measure_math and diagnostic.message =~ "padding"

    for {node, context} <- [{%{}, %{}}, {node, nil}, {Map.delete(node, :style), %{}}] do
      assert {:error, {reason, diagnostic}} = MathTableValidator.prepare(node, context)
      assert diagnostic.reason == reason and diagnostic.source == "mtable"

      assert diagnostic.operation == :measure_math and
               diagnostic.message =~ "styled MathML mtable"
    end
  end

  test "raw nonstring spans and unsupported cell children propagate actionable diagnostics", %{
    style: style
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    for attributes <- [%{"columnspan" => 2}, %{"rowspan" => []}] do
      assert {:error, {:unsupported_html, diagnostic}} =
               MathTableValidator.prepare(
                 matrix([[cell("1", style, zero, attributes)]], style),
                 %{}
               )

      assert diagnostic.source == "mtd" and diagnostic.message =~ "authored strings"
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    end

    unsupported = element("msup", [token("1", style), token("2", style)], style)
    outer_cell = cell("1", style, zero)
    outer_cell = %{outer_cell | children: [unsupported]}
    assert {:error, {reason, expected}} = MathLayout.measure(unsupported)

    assert {:error, {^reason, propagated}} =
             MathTableValidator.prepare(matrix([[outer_cell]], style), %{})

    assert propagated.source == expected.source and propagated.message == expected.message
    assert propagated.stage == expected.stage and propagated.operation == :measure_math
  end

  test "malformed prepared cell styles and spacing are rejected before consuming geometry", %{
    style: style
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(matrix([[cell("1", style, zero)]], style), %{})

    assert {:ok, child} = MathLayout.measure(token("1", style))
    [first] = hd(prepared.rows)
    malformed = %{prepared | rows: [[%{first | style: nil}]]}

    assert {:error, {:unsupported_html, diagnostic}} =
             MathTableValidator.prepare_geometry(malformed, [[child]])

    assert diagnostic.source == "mtd" and diagnostic.message =~ "validated geometry"
    assert diagnostic.stage == :layout and diagnostic.operation == :measure_math

    for spacing <- [nil, {0.0}, "4px"] do
      altered = %{prepared | style: Map.put(prepared.style, :border_spacing, spacing)}

      assert {:error, {:unsupported_html, diagnostic}} =
               MathTableValidator.prepare_geometry(altered, [[child]])

      assert diagnostic.source == "mtable" and diagnostic.operation == :measure_math
    end
  end

  test "authored empty rows contribute zero natural height and preserve selected MATH axis", %{
    style: style
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    populated = [cell("12", style, zero)]
    assert {:ok, expected} = MathLayout.measure(matrix([populated], style))
    assert {:ok, measured} = MathLayout.measure(matrix([[], populated, []], style))
    assert measured.width == expected.width
    assert measured.ascent == expected.ascent and measured.descent == expected.descent
    assert measured.glyphs == expected.glyphs and measured.actual_text == "12"
    assert {:ok, metrics} = Font.math_metrics(style.font_face)
    axis = metrics.constants.axis_height.value * style.font_size / metrics.units_per_em
    assert_in_delta measured.ascent - measured.descent, 2 * axis, 1.0e-9
  end

  test "allocated left and right alignment uses preferred column width and final cell padding", %{
    style: style
  } do
    padding = %{top: 2.0, right: 5.0, bottom: 3.0, left: 7.0}
    assert {:ok, long} = MathLayout.measure(token("1234", style))
    assert {:ok, short} = MathLayout.measure(token("5", style))

    for align <- [:left, :right] do
      last = cell("5", style, padding)
      last = %{last | style: Map.put(last.style, :text_align, align)}

      assert {:ok, measured} =
               MathLayout.measure(matrix([[cell("1234", style, padding)], [last]], style))

      assert_in_delta measured.width, long.max_content_width + 12, 1.0e-9
      shift = if align == :left, do: 0.0, else: long.max_content_width - short.width
      assert_in_delta List.last(measured.glyphs).x, 7 + shift + hd(short.glyphs).x, 1.0e-9
      assert measured.actual_text == "12345"
      assert :ok == MathLayoutValidator.validate_measured_geometry(measured)
    end
  end

  test "matrix measurement transports accumulated numeric and geometry resource failures", %{
    style: style,
    limits: limits
  } do
    padding = %{top: 0.0, right: 8.0, bottom: 0.0, left: 8.0}
    node = matrix([[cell("1", style, padding), cell("2", style, padding)]], style)
    assert {:ok, _} = MathLayout.measure(node)
    Limits.install(Map.put(limits, :max_css_numeric_magnitude, 30))
    assert {:error, {:unsupported_html, diagnostic}} = MathLayout.measure(node)
    assert diagnostic.source == "mtable" and diagnostic.message =~ "decorated box extents"
    assert diagnostic.stage == :layout and diagnostic.operation == :measure_math

    Limits.install(Map.put(limits, :max_table_grid_work, 8))
    assert {:error, {:resource_limit_exceeded, diagnostic}} = MathLayout.measure(node)
    assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    assert diagnostic.message =~ "max_table_grid_work"
  end

  test "bounded span-one preparation admits ragged prefixes and diagnoses gapped public state", %{
    style: style
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    node =
      matrix(
        [[], [cell("1", style, zero)], [cell("2", style, zero), cell("3", style, zero)], []],
        style
      )

    assert {:ok, prepared} = MathLayoutValidator.prepare_element(node, %{})

    assert Enum.map(prepared.rows, fn row -> Enum.map(row, & &1.column) end) == [
             [],
             [0],
             [0, 1],
             []
           ]

    assert {:ok, one} = MathLayout.measure(token("1", style))
    assert {:ok, two} = MathLayout.measure(token("2", style))
    assert {:ok, three} = MathLayout.measure(token("3", style))
    measured = [[], [one], [two, three], []]
    assert {:ok, _} = MathTableValidator.prepare_geometry(prepared, measured)
    [empty, short, long, last] = prepared.rows

    for {changed_short, changed_long} <- [
          {[Map.put(hd(short), :column, 1)], long},
          {short, [hd(long), Map.put(List.last(long), :column, 2)]},
          {[Map.put(hd(short), :column, 0.0)], long}
        ] do
      altered = %{prepared | rows: [empty, changed_short, changed_long, last]}

      assert {:error, {:unsupported_html, diagnostic}} =
               MathTableValidator.prepare_geometry(altered, measured)

      assert diagnostic.source == "mtd" and diagnostic.message =~ "contiguous zero-based columns"
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    end
  end

  test "public geometry reserves input work before paired allocations without charging twice", %{
    style: style,
    limits: limits
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(matrix([[cell("1", style, zero)]], style), %{})

    assert {:ok, child} = MathLayout.measure(token("1", style))

    # One row/cell pair costs2; geometry costs10; row paint eligibility costs1.
    Limits.install(Map.put(limits, :max_table_grid_work, 13))
    assert {:ok, expected} = MathTableValidator.prepare_geometry(prepared, [[child]])
    Limits.install(Map.put(limits, :max_table_grid_work, 12))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathTableValidator.prepare_geometry(prepared, [[child]])

    assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    assert diagnostic.message =~ "max_table_grid_work"
    Limits.install(limits)
    assert {:ok, ^expected} = MathTableValidator.prepare_geometry(prepared, [[child]])

    malformed = %{prepared | rows: [[%{hd(hd(prepared.rows)) | style: nil}]]}
    Limits.install(Map.put(limits, :max_table_grid_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathTableValidator.prepare_geometry(malformed, [[child]])

    assert diagnostic.message =~ "max_table_grid_work"
    Limits.install(Map.put(limits, :max_table_grid_work, 2))

    assert {:error, {:unsupported_html, diagnostic}} =
             MathTableValidator.prepare_geometry(malformed, [[child]])

    assert diagnostic.source == "mtd" and diagnostic.message =~ "validated geometry"

    # Two authored empty rows are already over a one-row budget, even when
    # the caller's measured shape is malformed and cannot be paired.
    oversized = %{prepared | rows: [[], []]}
    Limits.install(Map.put(limits, :max_table_grid_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathTableValidator.prepare_geometry(oversized, [])

    assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    Limits.install(Map.put(limits, :max_table_grid_work, 2))

    assert {:error, {:unsupported_html, diagnostic}} =
             MathTableValidator.prepare_geometry(oversized, [])

    assert diagnostic.message =~ "matching proper prepared/measured row lists"
  end

  test "public geometry rejects improper prepared and measured collection tails recoverably", %{
    style: style
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}

    assert {:ok, prepared} =
             MathLayoutValidator.prepare_element(matrix([[cell("1", style, zero)]], style), %{})

    assert {:ok, child} = MathLayout.measure(token("1", style))
    first = hd(hd(prepared.rows))

    for {state, measured} <- [
          {%{prepared | rows: [[first] | :malformed]}, [[child]]},
          {%{prepared | rows: [[first | :malformed]]}, [[child]]},
          {prepared, [[child] | :malformed]},
          {prepared, [[child | :malformed]]}
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               MathTableValidator.prepare_geometry(state, measured)

      assert diagnostic.source in ["mtable", "mtd"] and diagnostic.message =~ "proper"
      assert diagnostic.stage == :layout and diagnostic.operation == :measure_math
    end
  end

  test "painted row geometry reserves each actual background rectangle beyond eligibility", %{
    style: style,
    limits: limits
  } do
    zero = %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    node = matrix([[cell("1", style, zero)]], style)
    [row] = node.children
    row = %{row | style: Map.put(row.style, :background_color, {1, 1, 0})}
    assert {:ok, prepared} = MathLayoutValidator.prepare_element(%{node | children: [row]}, %{})
    assert {:ok, child} = MathLayout.measure(token("1", style))
    Limits.install(Map.put(limits, :max_table_grid_work, 14))
    assert {:ok, geometry} = MathTableValidator.prepare_geometry(prepared, [[child]])
    assert length(geometry.row_backgrounds) == 1
    Limits.install(Map.put(limits, :max_table_grid_work, 13))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathTableValidator.prepare_geometry(prepared, [[child]])

    assert diagnostic.message =~ "max_table_grid_work" and diagnostic.operation == :measure_math
  end
end
