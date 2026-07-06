defmodule NativeElixirPdfUtilities.HtmlToPdf.PaginationTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Pagination

  test "paginate exposes the pagination boundary while behavior is pending" do
    assert Pagination.paginate(%{boxes: []}, []) == {:error, :not_implemented}
  end
end
