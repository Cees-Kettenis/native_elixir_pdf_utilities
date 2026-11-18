defmodule NativeElixirPdfUtilities.HtmlToPdf.ReadingFlowTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.ReadingFlow

  test "inapplicable flows leave layout items alone" do
    box = %{type: :text, text: "Source"}
    item = %{index: 1, style: %{}}
    assert ReadingFlow.attach([box], %{}, item, :flex) == [box]

    assert ReadingFlow.attach(
             [box],
             %{reading_flow: :grid_rows, _reading_scope: make_ref()},
             item,
             :flex
           ) == [box]
  end

  test "row, order-modified, RTL and sideways flows have deterministic item keys" do
    box = %{type: :text, text: "Source"}

    item = %{
      index: 1,
      style: %{order: -2},
      row_start: 3,
      column_start: 1,
      x: 30.0,
      y: 25.0,
      box_width: 15.0,
      box_height: 10.0
    }

    for {flow, layout, style, key} <- [
          {:grid_rows, :grid, %{}, {3, 1, -2}},
          {:grid_order, :grid, %{}, -2},
          {:flex_flow, :flex, %{}, -2},
          {:flex_visual, :flex, %{direction: :rtl}, {-25.0, -45.0, -2}},
          {:flex_visual, :flex, %{writing_mode: :sideways_lr}, {30.0, 15.0, -2}}
        ] do
      scope = make_ref()
      style = Map.merge(style, %{reading_flow: flow, _reading_scope: scope})

      assert [%{reading_path: [{^scope, {0, ^key, 1}}]}] =
               ReadingFlow.attach([box], style, item, layout)
    end
  end

  test "a nested subdocument keeps its own logical text and ignores non-text paint" do
    root = %{reading_flow: :source_order, _reading_scope: make_ref()}

    tagged = [
      %{type: :glyph_group, actual_text: "Logical"},
      %{type: :image, image: %{logical_text: "Image"}},
      %{type: :drawing_group, boxes: [%{type: :text, text: "Nested"}]},
      %{type: :rect}
    ]

    before = %{type: :text, text: "Before"}
    inside = ReadingFlow.attach(tagged, root, %{index: 0, style: %{}}, :block)
    group = %{type: :drawing_group, boxes: [before | inside]}

    assert [%{boxes: [^before, %{type: :actual_text_start, text: "Logical Image Nested"} | _]}] =
             ReadingFlow.prepare([group])

    ReadingFlow.with_document(fn ->
      ReadingFlow.with_document(fn -> :ok end)
      assert ReadingFlow.prepare_pages([%{boxes: inside}]) == [%{boxes: inside}]
    end)
  end
end
