defmodule NativeElixirPdfUtilities.HtmlToPdf.AffineGeometry do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec multiply(tuple(), tuple()) :: tuple()
  def multiply({a, b, c, d, e, f}, {g, h, i, j, k, l}) do
    {a * g + c * h, b * g + d * h, a * i + c * j, b * i + d * j, a * k + c * l + e,
     b * k + d * l + f}
  end

  @doc "Returns a bounded inverse of a canonical affine transform, or rejects singular geometry."
  @spec inverse(term()) :: {:ok, tuple()} | :error
  def inverse(matrix) do
    case matrix do
      {a, b, c, d, e, f} ->
        if Enum.all?(
             Tuple.to_list(matrix),
             &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
           ) do
          determinant = a * d - b * c

          if determinant == 0 do
            :error
          else
            inverse =
              {d / determinant, -b / determinant, -c / determinant, a / determinant,
               (c * f - d * e) / determinant, (b * e - a * f) / determinant}

            if Enum.all?(
                 Tuple.to_list(inverse),
                 &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
               ), do: {:ok, inverse}, else: :error
          end
        else
          :error
        end

      _ ->
        :error
    end
  rescue
    ArithmeticError -> :error
  end
end
