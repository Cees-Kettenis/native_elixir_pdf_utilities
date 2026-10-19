defmodule NativeElixirPdfUtilities.HtmlToPdf.MeasuredChildUsedDisplayIntrinsicsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Layout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "blockified source inline boxes retain child min and max content widths" do
    html =
      "<span style='font-family:DejaVu Sans;font-size:12pt;line-height:18pt'>Alpha beta gamma delta epsilon</span>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, system_font_discovery: false)
    assert node.style.display == :inline
    before = node

    longest =
      Enum.map(
        ~w(Alpha beta gamma delta epsilon),
        &Font.text_width(&1, node.style.font_face, 12.0)
      )
      |> Enum.max()

    preferred = Font.text_width("Alpha beta gamma delta epsilon", node.style.font_face, 12.0)

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      for formatting <- [:math, :flex, :grid] do
        assert {:ok, measured} =
                 Layout.measure_child(node, context, %{
                   available_width: 100.0,
                   formatting_context: formatting
                 })

        assert_in_delta measured.min_content_width, longest, 1.0e-9
        assert_in_delta measured.max_content_width, preferred, 1.0e-9
        assert_in_delta measured.width, 100.0, 1.0e-9
        assert measured.ascent + measured.descent > 18.0
      end
    end)

    assert node == before
    assert node.style.display == :inline
  end
end
