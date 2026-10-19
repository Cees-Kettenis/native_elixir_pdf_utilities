defmodule NativeElixirPdfUtilities.HtmlToPdf.LineBreakProtocolTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator

  test "native break output cannot claim partial graphemes, repeated offsets or malformed flags" do
    text = "é漢"
    assert {:ok, breaks} = HtmlLineBreakValidator.opportunities(text)
    assert :ok = HtmlLineBreakValidator.validate_native_opportunities(text, breaks)

    for result <- [
          [{1, false}, {byte_size(text), true}],
          [{3, false}, {3, false}, {byte_size(text), true}],
          [{byte_size(text), false}],
          [{byte_size(text) + 1, true}],
          [{byte_size(text), :mandatory}],
          [:invalid],
          [{byte_size(text), true} | :invalid_tail],
          nil
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlLineBreakValidator.validate_native_opportunities(text, result)

      assert diagnostic.stage == :layout
      assert diagnostic.operation == :line_break
      assert diagnostic.module == HtmlLineBreakValidator
      assert diagnostic.source == "HTML inline text"
      assert diagnostic.message =~ "native Unicode"
    end

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlLineBreakValidator.validate_native_opportunities(<<255>>, [])

    assert diagnostic.message =~ "UTF-8"
  end

  test "malformed preceding and discretionary inline text retain public diagnostics" do
    assert {:error, {:invalid_layout, diagnostic}} = HtmlLineBreakValidator.segments("text", nil)
    assert diagnostic.message =~ "preceding inline context"

    for {before, after_text} <- [{nil, "text"}, {"text", nil}, {<<255>>, "text"}] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlLineBreakValidator.transparent_format_boundary?(before, after_text)

      assert diagnostic.message =~ "UTF-8"
      assert diagnostic.operation == :line_break
    end

    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(Map.put(previous, :max_layout_text_work, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlLineBreakValidator.transparent_format_boundary?("AB", "CD")

    assert diagnostic.message =~ "max_layout_text_work"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlLineBreakValidator.validate_native_opportunities("A B", [{2, false}, {3, true}])

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "max_layout_text_work"
  end

  test "code points beyond the packed line-break tables fall back to the unassigned class" do
    # Plane 15 private use (U+F0000) has no packed index entry, so property
    # lookup falls back and the pair automaton treats it as unassigned text.
    assert {:ok, [{6, true}]} = HtmlLineBreakValidator.opportunities("a\u{F0000}b")
  end
end
