defmodule NativeElixirPdfUtilities.Validators.PdfGeometryValidationTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
  end

  test "derived operands use the shared representability and serialized-token bounds" do
    assert :ok = PdfValidator.validate_geometry_numbers([1.0e-200, 0, 1, -1])

    for value <- [Integer.pow(10, 400), 1.0e100, :invalid] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               PdfValidator.validate_geometry_numbers([value])

      assert diagnostic.stage == :geometry and diagnostic.message =~ "max_pdf_numeric_magnitude"
    end

    Limits.install(Map.put(Limits.effective(), :max_pdf_numeric_token_bytes, 4))
    assert :ok = PdfValidator.validate_geometry_numbers([1.25])

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfValidator.validate_geometry_numbers([0.001])

    assert diagnostic.message =~ "max_pdf_numeric_token_bytes"
  end

  test "affine composition retains exact integer cancellation without early float conversion" do
    value = 9_007_199_254_740_993

    assert [1, 0, 0, 1, 0, 3] ==
             PdfValidator.multiply_matrices([1, 0, 0, 1, value, 1], [1, 0, 0, 1, -value, 2])
  end
end
