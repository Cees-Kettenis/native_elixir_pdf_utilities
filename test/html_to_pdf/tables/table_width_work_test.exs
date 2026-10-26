defmodule NativeElixirPdfUtilities.HtmlToPdf.TableWidthWorkTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Limits

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "spanning fixed table preferences share the existing table work allowance" do
    html =
      "<table style='table-layout:fixed;width:120pt'><tr>" <>
        String.duplicate("<td colspan=8 style='width:40pt'></td>", 3) <> "</tr></table>"

    assert {:ok, _} = HtmlToPdf.render(html, system_font_discovery: false)
    Limits.install(%{Limits.effective() | max_table_grid_work: 75})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :layout and diagnostic.message =~ "max_table_grid_work"
  end

  test "spanning width updates avoid repeated linked-list prefix copies" do
    reductions =
      for columns <- [256, 1_024] do
        html =
          "<table style='table-layout:fixed;width:400pt'><tr>" <>
            String.duplicate("<td colspan=8 style='width:2pt'></td>", div(columns, 8)) <>
            "</tr></table>"

        assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
        assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
        assert {:ok, _} = Layout.layout(styled, page_size: {500, 100_000}, margin: 0)
        {:reductions, before} = Process.info(self(), :reductions)
        assert {:ok, _layout} = Layout.layout(styled, page_size: {500, 100_000}, margin: 0)
        {:reductions, after_count} = Process.info(self(), :reductions)
        after_count - before
      end

    [small, large] = reductions
    assert large < small * 9, inspect(reductions)
  end
end
