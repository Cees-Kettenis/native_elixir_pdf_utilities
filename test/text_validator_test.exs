defmodule NativeElixirPdfUtilities.Validators.TextValidatorTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Validators.TextValidator

  test "numeric tokens must fit the shared PDF magnitude limit" do
    assert {:ok, 1_000_000_000} = TextValidator.number({:int, 1_000_000_000})

    for value <- [{:int, Integer.pow(10, 400)}, {:real, 1.0e308}, {:int, :bad}] do
      assert :error = TextValidator.number(value)
      assert :error = TextValidator.numbers([value], 1)
    end
  end

  test "public text requests are normalized by the validator" do
    assert {:ok, %{pdf: "%PDF", options: %{layout: false}}} =
             TextValidator.validate_request("%PDF", [layout: false], :extract)

    assert {:ok, %{options: %{order: :visual}}} =
             TextValidator.validate_request("%PDF", [order: :visual], :extract_spans)

    assert {:ok, "/tmp/input.pdf"} = TextValidator.validate_path("/tmp/input.pdf", :extract_file)

    assert {:error, {:invalid_options, %{stage: :options}}} =
             TextValidator.validate_request("%PDF", [unknown: true], :extract)

    assert {:error, {:invalid_pdf_input, %{stage: :input}}} =
             TextValidator.validate_request("%PDF", [], :unknown)

    assert {:error, {:invalid_path, %{stage: :file}}} =
             TextValidator.validate_path("/tmp/input.pdf", 123)
  end

  test "validates reconstructed layout whitespace projections" do
    assert :ok = TextValidator.validate_layout_whitespace([{1, 0}, {2, 10}])

    for projection <- [:not_a_list, [{0, 1}], [{1, -1}], [{1, :not_a_byte_count}]] do
      assert {:error,
              {:invalid_pdf_input,
               %{
                 stage: :text_validation,
                 reason: :invalid_pdf_input,
                 message: "layout whitespace projection is malformed"
               }}} = TextValidator.validate_layout_whitespace(projection)
    end
  end

  test "validate_scopes rejects text showing outside a text object" do
    instructions = [[%{operator: "Tj", operands: [{:string, "outside"}]}]]

    assert {:error,
            {:invalid_pdf_input,
             %{stage: :content, message: "Tj appears outside a text object; page 1"}}} =
             TextValidator.validate_scopes(instructions, 1)
  end

  test "validate_scopes rejects malformed prepared instruction input" do
    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :content,
               reason: :invalid_pdf_input,
               operation: :extract,
               module: TextValidator,
               message: "content scope input is malformed"
             }}} = TextValidator.validate_scopes(:not_instructions, 1)

    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :content,
               reason: :invalid_pdf_input,
               operation: :extract,
               module: TextValidator,
               message: "content scope instruction is malformed; page 1"
             }}} = TextValidator.validate_scopes([[%{}]], 1)
  end
end
