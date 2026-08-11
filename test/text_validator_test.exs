defmodule NativeElixirPdfUtilities.Validators.TextValidatorTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Validators.TextValidator

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
