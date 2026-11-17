defmodule NativeElixirPdfUtilities.FormsLimitsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "field and appearance resource limits" do
    {:ok, pdf} = HtmlToPdf.render("<div><input name=\"a\"></div>")
    original = Limits.effective()
    Limits.install(%{original | max_input_bytes: byte_size(pdf) - 1})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits, operation: :fill}}} =
             Forms.fill(pdf, %{"a" => "abc"})

    Limits.install(original)
    assert {:ok, _} = Forms.fill(pdf, %{"a" => "ab"})
    Limits.install(Map.put(Limits.effective(), :max_entries, 1))
    assert {:error, {:resource_limit_exceeded, _}} = HtmlToPdf.render("<div><input><input></div>")
  end

  test "password fill counts rendered mask bytes in aggregate appearance limits" do
    assert {:ok, pdf} = HtmlToPdf.render(~s(<input type="password" name="secret">))
    original = Limits.effective()
    Limits.install(Map.put(original, :max_output_bytes, 6))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Forms.fill(pdf, %{"secret" => "ABC"})

    assert diagnostic.stage == :forms and diagnostic.operation == :fill and
             diagnostic.source == "secret"

    assert diagnostic.message =~ "aggregate"

    assert {:ok, "••"} =
             NativeElixirPdfUtilities.Validators.FormValidator.password_display_text("AB")

    Limits.install(original)
    assert {:ok, _} = Forms.fill(pdf, %{"secret" => "AB"})
  end

  test "password display limits propagate through fill and unsupported text flags stay rejected" do
    assert {:ok, pdf} = HtmlToPdf.render(~s(<input type="password" name="secret">))
    original = Limits.effective()
    Limits.install(Map.put(original, :max_output_bytes, 8))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Forms.fill(pdf, %{"secret" => "ABC"})

    assert diagnostic.stage == :forms and diagnostic.operation == :fill and
             diagnostic.source == "secret"

    assert diagnostic.message =~ "max_output_bytes"

    assert {:ok, "••"} =
             NativeElixirPdfUtilities.Validators.FormValidator.password_display_text("AB")

    Limits.install(original)
    assert {:ok, _} = Forms.fill(pdf, %{"secret" => "AB"})

    assert {:ok, context} = Reader.read_validated(pdf)

    assert {:ok, %{fields: [field]}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    {:ref, {id, generation}} = field.ref

    for flags <- [16_777_216, 33_554_432, 16_785_408, 33_562_624] do
      assert {:ok, unsupported} =
               IncrementalWriter.write(context, [
                 {id, generation, {:value, Map.put(field.dictionary, "Ff", flags)}}
               ])

      assert {:error, {:unsupported_form, diagnostic}} =
               Forms.fill(unsupported, %{"secret" => "AB"})

      assert diagnostic.stage == :forms and diagnostic.operation == :fill and
               diagnostic.source == "secret"

      assert diagnostic.message =~ "comb and rich-text"
    end
  end
end
