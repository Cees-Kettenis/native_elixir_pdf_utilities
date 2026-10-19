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
    Limits.install(%{limits | max_font_math_work: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FontValidator.prepare_text_ink_rectangle(box)

    assert diagnostic.stage == :font and diagnostic.reason == :resource_limit_exceeded
  end
end
