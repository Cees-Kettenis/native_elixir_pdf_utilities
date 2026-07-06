defmodule NativeElixirPdfUtilities.HtmlToPdf.StyleTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  test "compute exposes the style boundary while behavior is pending" do
    assert Style.compute(%{tag: "p"}, []) == {:error, :not_implemented}
  end
end
