defmodule NativeElixirPdfUtilities.HtmlToPdf.LayoutTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Layout

  test "layout exposes the layout boundary while behavior is pending" do
    assert Layout.layout(%{tag: "p", style: %{}}, []) == {:error, :not_implemented}
  end
end
