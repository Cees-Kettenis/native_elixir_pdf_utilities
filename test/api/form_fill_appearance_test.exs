defmodule NativeElixirPdfUtilities.FormFillAppearanceTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "default and explicit fitted appearances keep long replacement text inside the widget" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<input name='entry' value='ABC' style='width:80px;height:20px;font-size:12pt'>",
               system_font_discovery: false
             )

    value = "This longer text was fitted before"
    assert {:ok, [%{widgets: [%{rect: [left, _bottom, right, _top]}]}]} = Forms.fields(pdf)

    for opts <- [[], [appearance: :fit], [flatten: true]] do
      assert {:ok, filled} = Forms.fill(pdf, %{"entry" => value}, opts)
      assert {:ok, flat} = Forms.flatten(filled)
      assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(flat)
      assert spans != []
      assert Enum.all?(spans, &(&1.font_size > 0 and &1.font_size < 5))
      assert Enum.all?(spans, &(&1.x >= left + 2 and &1.end_x <= right - 2 + 0.001))
      assert {:ok, text} = Text.extract(flat, layout: false)
      assert String.normalize(text, :nfkc) == value
    end

    assert {:ok, source} =
             Forms.fill(pdf, %{"entry" => value}, appearance: :source, flatten: true)

    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(source)
    for span <- spans, do: assert_in_delta(span.font_size * abs(hd(span.ctm)), 12, 0.001)
    assert Enum.any?(spans, &(&1.end_x > right))
  end

  test "default filling uses bundled glyphs while original source fonts remain opt-in" do
    font = File.read!(Paths.project("priv/fonts/stix/STIXTwoMath-Regular.ttf"))

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<input name='entry' value='ABC' style='font-family:Original;font-size:12pt;width:80px;height:30px'>",
               fonts: [%{family: "Original", data: font}],
               system_font_discovery: false
             )

    assert {:ok, filled} = Forms.fill(pdf, %{"entry" => "Ա"})
    assert {:ok, [%{value: "Ա"}]} = Forms.fields(filled)
    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, "Ա"} = Text.extract(flat, layout: false)

    assert {:error, {:unsupported_glyph, diagnostic}} =
             Forms.fill(pdf, %{"entry" => "Ա"}, appearance: :source)

    assert diagnostic.stage == :font
    assert diagnostic.reason == :unsupported_glyph
    assert diagnostic.module == Forms
    assert diagnostic.operation == :fill
    assert diagnostic.source == "entry"
    assert diagnostic.message =~ "glyph"
  end

  test "invalid appearance options return public diagnostics" do
    for appearance <- [:unknown, nil, true, "source"] do
      assert {:error, {:invalid_options, diagnostic}} =
               Forms.fill("", %{}, appearance: appearance)

      assert diagnostic.stage == :forms
      assert diagnostic.reason == :invalid_options
      assert diagnostic.module == Forms
      assert diagnostic.operation == :fill
      assert diagnostic.message == "appearance must be :fit or :source"
    end
  end
end
