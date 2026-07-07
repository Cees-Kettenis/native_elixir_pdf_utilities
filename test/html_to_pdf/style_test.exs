defmodule NativeElixirPdfUtilities.HtmlToPdf.StyleTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  test "compute applies default paragraph styles" do
    dom = %{
      type: :document,
      children: [
        %{type: :element, tag: "p", attributes: %{}, children: [%{type: :text, text: "Hello"}]}
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.display == :block
    assert paragraph.style.font_family == "Helvetica"
    assert paragraph.style.font_size == 12.0
    assert paragraph.style.color == {0, 0, 0}
  end

  test "compute rejects unsupported document trees" do
    assert Style.compute(%{tag: "p"}, []) == {:error, :invalid_document}
  end
end
