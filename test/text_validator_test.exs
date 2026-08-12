defmodule NativeElixirPdfUtilities.Validators.TextValidatorTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Validators.TextValidator

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
