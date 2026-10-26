defmodule NativeElixirPdfUtilities.Validators.FormInspectionValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.FormInspectionValidator

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    %{limits: original}
  end

  test "decoded source fonts share bytes through aliases and retain cumulative unique decode accounting",
       %{limits: limits} do
    bytes = String.duplicate("A", 20)
    compressed = :zlib.compress(bytes)

    object = %{
      value: %{"Length" => byte_size(compressed), "Filter" => {:name, "FlateDecode"}},
      stream: compressed
    }

    document = %{
      objects: %{{1, 0} => object, {2, 0} => object, {3, 0} => %{value: {:ref, {1, 0}}}}
    }

    Limits.install(%{limits | max_pdf_reader_decoded_bytes: 20, max_pdf_form_font_uses: 3})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FormInspectionValidator.with_scope(document, fn ->
               assert {:ok, ^bytes} =
                        FormInspectionValidator.decoded_font(document, {:ref, {1, 0}})

               assert {:ok, ^bytes} =
                        FormInspectionValidator.decoded_font(document, {:ref, {3, 0}})

               FormInspectionValidator.decoded_font(document, {:ref, {2, 0}})
             end)

    assert diagnostic.message =~ "decoded stream" and diagnostic.message =~ "object 2 0"
    # Every use is still admitted, even when the decoded bytes are cached.
    Limits.install(%{limits | max_pdf_form_font_uses: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FormInspectionValidator.with_scope(document, fn ->
               assert {:ok, ^bytes} =
                        FormInspectionValidator.decoded_font(document, {:ref, {1, 0}})

               FormInspectionValidator.decoded_font(document, {:ref, {3, 0}})
             end)

    assert diagnostic.stage == :forms and diagnostic.message =~ "max_pdf_form_font_uses"
    Limits.install(%{limits | max_pdf_form_font_work_bytes: 39})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FormInspectionValidator.with_scope(document, fn ->
               assert {:ok, ^bytes} =
                        FormInspectionValidator.decoded_font(document, {:ref, {1, 0}})

               FormInspectionValidator.decoded_font(document, {:ref, {1, 0}})
             end)

    assert diagnostic.message =~ "max_pdf_form_font_work_bytes"

    assert {:ok, ^bytes} =
             FormInspectionValidator.with_scope(document, fn ->
               FormInspectionValidator.decoded_font(document, {:ref, {1, 0}})
             end)
  end

  test "loaded-font reuse accepts only identical descriptors and does not cache a failed load" do
    FormInspectionValidator.with_active_scope(fn ->
      assert {:error, {:unsupported_form, %{}}} =
               FormInspectionValidator.loaded_fonts([:first], fn ->
                 {:error, {:unsupported_form, %{}}}
               end)

      assert {:ok, [:one]} =
               FormInspectionValidator.loaded_fonts([:first], fn -> {:ok, [:one]} end)

      assert {:ok, [:one]} =
               FormInspectionValidator.loaded_fonts([:first], fn ->
                 flunk("identical fonts should reuse the checked faces")
               end)

      assert {:ok, [:two]} =
               FormInspectionValidator.loaded_fonts([:second], fn -> {:ok, [:two]} end)
    end)
  end

  test "nested foreign documents cannot reuse font bytes and scopes restore after exceptions" do
    object = fn bytes -> %{value: %{"Length" => byte_size(bytes)}, stream: bytes} end
    first = %{objects: %{{1, 0} => object.("one")}}
    second = %{objects: %{{1, 0} => object.("two")}}

    FormInspectionValidator.with_scope(first, fn ->
      assert {:ok, "one"} = FormInspectionValidator.decoded_font(first, {:ref, {1, 0}})

      assert_raise RuntimeError, "scope failure", fn ->
        FormInspectionValidator.with_scope(second, fn ->
          assert {:ok, "two"} = FormInspectionValidator.decoded_font(second, {:ref, {1, 0}})
          raise "scope failure"
        end)
      end

      assert {:ok, "one"} = FormInspectionValidator.decoded_font(first, {:ref, {1, 0}})
      assert {:error, _} = FormInspectionValidator.decoded_font(first, {:ref, {999, 0}})
    end)
  end
end
