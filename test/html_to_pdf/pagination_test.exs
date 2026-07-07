defmodule NativeElixirPdfUtilities.HtmlToPdf.PaginationTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Pagination

  test "paginate wraps the milestone layout tree in a single page" do
    layout_tree = %{
      type: :layout,
      page_size: {595.28, 841.89},
      boxes: [%{type: :text, text: "Hello"}]
    }

    assert Pagination.paginate(layout_tree, []) ==
             {:ok, [%{size: {595.28, 841.89}, boxes: [%{type: :text, text: "Hello"}]}]}
  end

  test "paginate rejects invalid layout trees" do
    assert Pagination.paginate(%{boxes: []}, []) == {:error, :invalid_layout}
  end
end
