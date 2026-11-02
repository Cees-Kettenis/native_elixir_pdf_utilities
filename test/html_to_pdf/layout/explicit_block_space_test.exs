defmodule NativeElixirPdfUtilities.HtmlToPdf.ExplicitBlockSpaceTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Info, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Pagination, PdfWriter}

  test "prepared block-space markers without optional clips preserve following placement" do
    layout = %{
      type: :layout,
      page_size: {150, 75},
      margin: 0,
      boxes: [
        %{
          type: :flow_marker,
          role: :explicit_block_space,
          flow_id: {:explicit_block_space, make_ref()},
          x: 0,
          y: -75,
          width: 0,
          height: 150
        },
        %{
          type: :rect,
          x: 0,
          y: -85,
          width: 10,
          height: 10,
          fill_color: {1, 0, 0},
          stroke_color: nil,
          stroke_width: 0,
          border_radius: 0
        }
      ]
    }

    assert {:ok, pages} = Pagination.paginate(layout)
    assert length(pages) == 3
    assert [paint] = Enum.filter(List.last(pages).boxes, &(&1.type == :rect))
    assert {paint.y, paint.height} == {65, 10}
    assert {:ok, pdf} = PdfWriter.render(pages)
    assert {:ok, 3} = Info.page_count(pdf)
  end

  test "an empty authored block reserves its height before following text" do
    for overflow <- ["visible", "hidden"] do
      html = document("<div style='height:400px;overflow:#{overflow}'></div><p>Following</p>")
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, 5} = Info.page_count(pdf)
      assert {:ok, %{pages: pages}} = Text.extract_spans(pdf)
      assert Enum.all?(Enum.take(pages, 4), &(&1.spans == []))
      spans = List.last(pages).spans
      assert Enum.map_join(spans, & &1.text) == "Following"
      assert_in_delta hd(spans).y, following_baseline(), 0.001
    end
  end

  test "short content retains its parent's authored height before following text" do
    for overflow <- ["visible", "hidden"],
        {height, offset} <- [{400, 0}, {410, 7.5}],
        edges <- ["", "box-sizing:border-box;padding:10px;border:2px solid black;"] do
      html =
        document("""
        <div style="width:80px;height:#{height}px;overflow:#{overflow};#{edges}">
          <div style="height:40px">Inside</div>
        </div><p>Following</p>
        """)

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, 5} = Info.page_count(pdf)
      assert {:ok, %{pages: pages}} = Text.extract_spans(pdf)
      assert Enum.any?(hd(pages).spans, &(&1.text == "Inside"))
      assert Enum.map_join(List.last(pages).spans, & &1.text) == "Following"
      y = hd(List.last(pages).spans).y
      assert_in_delta y, following_baseline() + offset, 0.001
      assert Enum.count(Enum.flat_map(pages, & &1.spans), &(&1.text == "Inside")) == 1
    end
  end

  test "line-break gaps count within the authored block extent for visible and hidden overflow" do
    for overflow <- ["visible", "hidden"], following <- ["", "<p>Following</p>"] do
      html =
        document("""
        <div style="width:80px;height:400px;overflow:#{overflow}">
          #{String.duplicate("line<br>", 20)}
        </div>#{following}
        """)

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, count} = Info.page_count(pdf)
      assert count == if(following == "", do: 4, else: 5)
      assert {:ok, %{pages: pages}} = Text.extract_spans(pdf)
      assert Enum.count(Enum.flat_map(pages, & &1.spans), &(&1.text == "line")) == 20

      if following != "" do
        assert Enum.map_join(List.last(pages).spans, & &1.text) == "Following"
        y = hd(List.last(pages).spans).y
        assert_in_delta y, following_baseline(), 0.001
      end
    end
  end

  defp following_baseline do
    assert {:ok, pdf} =
             HtmlToPdf.render(document("<p>Following</p>"), system_font_discovery: false)

    assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
    assert Enum.map_join(spans, & &1.text) == "Following"
    hd(spans).y
  end

  defp document(content) do
    """
    <style>@page{size:200px 100px;margin:0}html,body,p{margin:0}
    body{font-family:"DejaVu Sans";font-size:12px;line-height:19px}</style>
    #{content}
    """
  end
end
