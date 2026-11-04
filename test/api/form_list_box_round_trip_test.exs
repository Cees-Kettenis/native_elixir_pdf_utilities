defmodule NativeElixirPdfUtilities.FormListBoxRoundTripTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}
  alias NativeElixirPdfUtilities.Validators.FormValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "filled list boxes retain every visible label through cleared single and multiple selections" do
    for multiple <- [false, true], appearance <- [:source, :fit] do
      html =
        "<select name=choice size=3 #{if multiple, do: "multiple", else: ""} style='font-family:DejaVu Sans;font-size:10pt;width:100pt'><option value=a selected>Alpha</option><option value=b>Beta</option><option value=c>Gamma</option></select>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)

      for value <- if(multiple, do: [["c"], ["a", "c"], [], nil], else: ["c", nil]) do
        assert {:ok, filled} = Forms.fill(pdf, %{"choice" => value}, appearance: appearance)
        assert {:ok, [field]} = Forms.fields(filled)
        assert field.value == if(multiple and is_nil(value), do: [], else: value)
        assert Enum.map(field.choices, & &1.label) == ["Alpha", "Beta", "Gamma"]
        assert {:ok, flat} = Forms.flatten(filled)
        assert {:ok, []} = Forms.fields(flat)
        assert {:ok, text} = Text.extract(flat, layout: false)
        assert text =~ "Alpha" and text =~ "Beta" and text =~ "Gamma"
      end
    end
  end

  test "source list boxes scroll to previously offscreen choices and preserve optgroup headings" do
    html =
      "<select name=choice size=2 style='font-family:DejaVu Sans;font-size:10pt;width:100pt'><option value=a selected>Alpha</option><optgroup label=Group><option value=b>Beta</option><option value=c>Gamma</option><option value=d>Delta</option></optgroup></select>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, filled} = Forms.fill(pdf, %{"choice" => "d"}, appearance: :source)
    assert {:ok, context} = Reader.read_validated(filled)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    assert field.dictionary["TI"] > 0
    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, text} = Text.extract(flat, layout: false)
    assert text =~ "Delta"
    refute text =~ "Alpha"
    assert {:ok, filled} = Forms.fill(filled, %{"choice" => "b"}, appearance: :source)
    assert {:ok, _} = Forms.flatten(filled)
  end

  test "editor font serialization respects the configured decompression ratio during the complete lifecycle",
       %{limits: limits} do
    for ratio <- [1, 2, 100] do
      Limits.install(%{limits | max_pdf_decompression_ratio: ratio})

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<input name=editor value=Initial style='font-family:DejaVu Sans'>",
                 system_font_discovery: false
               )

      assert {:ok, [%{value: "Initial"}]} = Forms.fields(pdf)
      assert {:ok, filled} = Forms.fill(pdf, %{"editor" => "Changed"}, appearance: :source)
      assert {:ok, [%{value: "Changed"}]} = Forms.fields(filled)
      assert {:ok, flat} = Forms.flatten(filled)
      assert {:ok, text} = Text.extract(flat, layout: false)
      assert text =~ "Changed"
    end
  end

  test "older source templates reconstruct complete list rows with their original fonts" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<select name=choice size=3><option value=a selected>Alpha</option><option value=b>Beta</option><option value=c>Gamma</option></select>",
               system_font_discovery: false
             )

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    [widget] = field.widgets
    {:ref, {id, generation}} = widget.ref
    dictionary = update_in(widget.dictionary, ["NEPUSourceEditor"], &Map.delete(&1, "ListBox"))

    assert {:ok, legacy} =
             IncrementalWriter.write(context, [{id, generation, {:value, dictionary}}])

    assert {:ok, filled} = Forms.fill(legacy, %{"choice" => "b"}, appearance: :source)
    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, text} = Text.extract(flat, layout: false)
    assert text =~ "Alpha" and text =~ "Beta" and text =~ "Gamma"
  end

  test "retained rows support empty options italic fonts translucent text and authored backgrounds" do
    html =
      "<select name=choice size=3 style='font-family:DejaVu Sans;width:100pt'><option value=a selected></option><option value=b style='font-style:italic;color:rgba(255,0,0,.5);background:yellow'>Beta</option><option value=c>Gamma</option></select>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, filled} = Forms.fill(pdf, %{"choice" => "c"}, appearance: :source)
    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, text} = Text.extract(flat, layout: false)
    assert text =~ "Beta" and text =~ "Gamma"
  end

  test "empty padded list boxes retain the actual scrolling clip when filled" do
    html =
      "<select name=empty size=3 style='font-family:DejaVu Sans;width:100pt;padding:4pt'><option value=a selected></option><option value=b></option><option value=c></option></select>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: [field]}} = FormValidator.inspect_document(context)
    [widget] = field.widgets
    assert_in_delta widget.source_editor.clip.x, Enum.at(widget.rect, 0) + 0.75, 0.01
    assert_in_delta widget.source_editor.clip.y, Enum.at(widget.rect, 1) + 0.75, 0.01
    assert {:ok, filled} = Forms.fill(pdf, %{"empty" => "c"}, appearance: :source)
    assert {:ok, [%{value: "c"}]} = Forms.fields(filled)
    assert {:ok, _} = Forms.flatten(filled)
  end

  test "retained row metadata is admitted during production before serialization", %{
    limits: limits
  } do
    Limits.install(%{limits | max_pdf_form_preparation_entries: 2})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<select name=choice size=3><option>A</option><option>B</option></select>",
               system_font_discovery: false
             )

    assert diagnostic.message =~ "max_pdf_form_preparation_entries"
    assert diagnostic.operation == :render
  end
end
