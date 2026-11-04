defmodule NativeElixirPdfUtilities.HtmlToPdf.OverflowPaginationTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.{HtmlToPdf, Info}
  alias NativeElixirPdfUtilities.HtmlToPdf.Pagination

  test "ordinary clipped paint cannot extend the containing flow allocation" do
    for {parent_height, content, pages} <- [
          {40, "<div style='width:80px;height:400px;background:red'></div>", 1},
          {400, "<div style='width:80px;height:800px;background:red'></div>", 4},
          {40, "<div style='height:400px'>#{String.duplicate("line<br>", 20)}</div>", 1},
          {400, "<div style='height:800px'>#{String.duplicate("line<br>", 20)}</div>", 4}
        ] do
      html = """
      <style>@page{size:200px 100px;margin:0}html,body{margin:0}</style>
      <div style="width:80px;height:#{parent_height}px;overflow:hidden">#{content}</div>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, ^pages} = Info.page_count(pdf)
    end
  end

  test "capped fixed-height text leaves following paint at the container's actual end" do
    for {height, pages} <- [{390, 4}, {410, 5}] do
      html = """
      <style>@page{size:200px 100px;margin:0}html,body{margin:0}
      body{font-family:"DejaVu Sans";font-size:12px;line-height:19px}</style>
      <div style="width:80px;height:#{height}px;overflow:hidden">
        <div style="height:800px">#{String.duplicate("line<br>", 20)}</div>
      </div><div style="width:40px;height:10px;background:lime"></div>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, ^pages} = Info.page_count(pdf)
    end
  end

  test "fully clipped primitive flow does not reserve a negative extent or displace following boxes" do
    hidden = %{
      type: :rect,
      x: 0,
      y: -100,
      width: 10,
      height: 20,
      fill: {1, 0, 0},
      clip: %{x: 0, y: 40, width: 10, height: 20}
    }

    following = %{type: :rect, x: 0, y: 80, width: 10, height: 10, fill: {0, 1, 0}}

    assert {:ok, [page]} =
             Pagination.paginate(%{
               type: :layout,
               page_size: {100, 100},
               margin: 0,
               boxes: [hidden, following]
             })

    assert Enum.find(page.boxes, &(&1.fill == {0, 1, 0})).y == 80
  end

  test "pagination diagnoses malformed primitive and nested glyph clips before measuring flow" do
    rectangle = %{type: :rect, x: 0, y: 0, width: 10, height: 10, fill: {1, 0, 0}}

    for clip <- [
          :bad,
          %{x: 0, y: "bad", width: 10, height: 10},
          %{x: 0, y: 0, width: -1, height: 10},
          %{
            x: 0,
            y: 0,
            width: 10,
            height: 10,
            inner_clip: %{x: 0, y: 0, width: 10, height: "bad"}
          }
        ],
        box <- [
          Map.put(rectangle, :clip, clip),
          %{type: :glyph_group, boxes: [Map.put(rectangle, :clip, clip)]}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               Pagination.paginate(%{
                 type: :layout,
                 page_size: {100, 100},
                 margin: 0,
                 boxes: [box]
               })

      assert diagnostic.stage == :pagination
      assert diagnostic.reason == :invalid_layout
      assert diagnostic.operation == :paginate
      assert diagnostic.source == "clip"
      assert diagnostic.message =~ "bounded rectangle geometry"
    end
  end
end
