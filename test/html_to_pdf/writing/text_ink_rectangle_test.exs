defmodule NativeElixirPdfUtilities.HtmlToPdf.TextInkRectangleTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Limits

  setup do
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :italic, registry)
    {:ok, font: font}
  end

  test "selected outline rectangle retains italic overhang and the existing vertical projection",
       %{font: font} do
    box = %{text: "jAV", font_face: font, font_size: 20.0, x: 12.0, y: 30.0}
    assert {:ok, {left, bottom, right, top}} = FontValidator.prepare_text_ink_rectangle(box)
    assert left < box.x
    assert right > left and top > bottom
    assert {:ok, {^top, ^bottom}} = FontValidator.prepare_text_ink_bounds(box)

    assert {:ok, {^left, ^bottom, spaced_right, ^top}} =
             FontValidator.prepare_text_ink_rectangle(Map.put(box, :letter_spacing, 3.0))

    assert_in_delta spaced_right - right, 6.0, 0.000001
    assert {:ok, nil} = FontValidator.prepare_text_ink_rectangle(%{box | text: " "})
  end

  test "explicit selected glyphs avoid automatic kerning and preserve authored positions", %{
    font: font
  } do
    box = %{text: "AV", font_face: font, font_size: 20.0, x: 0.0, y: 0.0}
    ids = Enum.map(~c"AV", &Map.fetch!(font.cmap, &1))

    assert {:ok, {_left, bottom, kerned_right, top}} =
             FontValidator.prepare_text_ink_rectangle(box)

    assert {:ok, {_left, ^bottom, selected_right, ^top}} =
             FontValidator.prepare_text_ink_rectangle(Map.put(box, :glyph_ids, ids))

    assert selected_right > kerned_right
  end

  test "malformed coordinate and spacing requests diagnose before geometry", %{font: font} do
    box = %{text: "A", font_face: font, font_size: 12.0, x: 0.0, y: 0.0}

    for invalid <- [
          Map.delete(box, :x),
          %{box | x: :bad},
          Map.put(box, :letter_spacing, :bad),
          Map.put(box, :letter_spacing, 1.0e20),
          Map.put(box, :bidi_levels, [127]),
          Map.put(box, :glyph_basis, {1, 0, 0, 0}),
          Map.put(box, :glyph_basis, nil),
          Map.put(box, :word_spacing, :bad),
          Map.put(box, :glyph_ids, [Map.fetch!(font.cmap, ?A) | :bad]),
          Map.put(box, :glyph_ids, [])
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               FontValidator.prepare_text_ink_rectangle(invalid)

      assert diagnostic.stage == :font
      assert diagnostic.module == FontValidator
      assert diagnostic.operation == :measure_child
      assert diagnostic.source == "A"
      assert diagnostic.message =~ "bounded point geometry"
    end

    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FontValidator.prepare_text_ink_rectangle(box)

    assert diagnostic.stage == :font and diagnostic.reason == :resource_limit_exceeded
  end

  test "selected glyph bases rotate bounds and advances while synthetic shear follows the same basis",
       %{font: font} do
    glyph = Map.fetch!(font.cmap, ?A)
    assert {:ok, bounds} = Font.glyph_bounds(font)
    {left, bottom, right, top} = elem(bounds, glyph)
    scale = 20 / font.units_per_em
    face = Map.put(font, :synthesis, %{shear: 0.25, weight: false})

    box = %{
      text: "AA",
      glyph_ids: [glyph, glyph],
      font_face: face,
      font_size: 20,
      x: 12,
      y: 30,
      glyph_basis: {0, 1, -1, 0}
    }

    assert {:ok, {actual_left, actual_bottom, actual_right, actual_top}} =
             FontValidator.prepare_text_ink_rectangle(box)

    assert_in_delta actual_left, 12 - top * scale, 0.00001
    assert_in_delta actual_right, 12 - bottom * scale, 0.00001
    assert_in_delta actual_bottom, 30 + (left + 0.25 * bottom) * scale, 0.00001

    assert_in_delta actual_top,
                    30 + (right + 0.25 * top + Enum.at(font.widths, glyph)) * scale,
                    0.00001

    # Without an explicit selection, the complete horizontal run's advance
    # also rotates. This is a raw PDF glyph basis, separate from writing mode.
    assert {:ok, {raw_left, raw_bottom, raw_right, raw_top}} =
             FontValidator.prepare_text_ink_rectangle(Map.delete(box, :glyph_ids))

    assert_in_delta raw_left, actual_left, 0.00001
    assert_in_delta raw_right, actual_right, 0.00001
    assert_in_delta raw_bottom, actual_bottom, 0.00001
    # CSS positions quantize the intervening advance in 1/64 CSS pixels;
    # explicit Math selections retain their design-unit advance.
    assert_in_delta raw_top, actual_top, 0.75 / 64
  end
end
