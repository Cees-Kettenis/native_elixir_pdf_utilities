defmodule NativeElixirPdfUtilities.HtmlToPdf.LayoutFragmentationTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}

  # CSS Fragmentation 3 (4 December 2018), parallel and nested flows.
  test "flex and grid tall parallel items continue at whole lines without text loss" do
    for display <- ["flex", "grid;grid-template-columns:1fr 1fr"] do
      html =
        "<div style='display:#{display}'>#{paragraph("Left", 10)}#{paragraph("Right", 10)}</div><p>After</p>"

      pages = pages(html, 50)
      assert length(pages) == 3

      for prefix <- ["Left", "Right"] do
        assert Enum.flat_map(pages, &texts/1) |> Enum.filter(&String.starts_with?(&1, prefix)) ==
                 Enum.map(1..10, &"#{prefix}#{&1}")
      end

      assert "After" in texts(List.last(pages))

      assert Enum.map(pages, fn page ->
               Enum.count(texts(page), &String.starts_with?(&1, "Left"))
             end) == [4, 4, 2]
    end
  end

  test "fitting flex and grid rows move together and subsequent content follows their final continuation" do
    for display <- ["flex;flex-wrap:wrap", "grid;grid-template-columns:1fr 1fr"] do
      content = Enum.map_join(1..6, &"<div style='width:100pt;height:30pt'>Item#{&1}</div>")
      result = pages("<div style='display:#{display}'>#{content}</div><p>After</p>", 50)
      assert length(result) == 3

      assert Enum.map(result, &texts/1) == [
               ["Item1", "Item2"],
               ["Item3", "Item4"],
               ["Item5", "Item6", "After"]
             ]
    end
  end

  test "multicolumn sets continue vertically on pages and retain nested column source order" do
    content = Enum.map_join(1..24, "<br>", &"Line#{&1}")

    result =
      pages(
        "<div style='columns:2;column-gap:10pt;orphans:1;widows:1'>#{content}</div><p>After</p>",
        50
      )

    assert length(result) == 4
    assert Enum.flat_map(result, &texts/1) == Enum.map(1..24, &"Line#{&1}") ++ ["After"]

    assert Enum.map(
             result,
             &Enum.count(texts(&1), fn text -> String.starts_with?(text, "Line") end)
           ) == [8, 8, 8, 0]

    nested =
      pages(
        "<div style='columns:2;column-gap:10pt'><div style='columns:2;column-gap:5pt'>#{content}</div></div>",
        50
      )

    assert Enum.flat_map(nested, &texts/1) == Enum.map(1..24, &"Line#{&1}")
  end

  test "page breaks inside columns escape the column context and keep following content on the correct page" do
    result =
      pages(
        "<div style='columns:2;column-gap:10pt'><p>First</p><p style='break-before:page'>Second</p></div><p>After</p>",
        100
      )

    assert Enum.map(result, &texts/1) == [["First"], ["Second", "After"]]
  end

  test "automatic column fill keeps the last set sequential while balance shortens it" do
    content = Enum.map_join(1..24, "<br>", &"Line#{&1}")

    for {fill, expected} <- [{"auto", [16, 8, 0]}, {"balance", [16, 8]}, {"balance-all", [16, 8]}] do
      result =
        pages(
          "<div style='columns:2;column-fill:#{fill};column-gap:10pt;orphans:1;widows:1'>#{content}</div><p>After</p>",
          100
        )

      assert Enum.map(result, fn page ->
               Enum.count(texts(page), &String.starts_with?(&1, "Line"))
             end) == expected

      assert "After" in texts(List.last(result))
    end
  end

  test "columns consume the remaining first page before using full continuation pages" do
    content = Enum.map_join(1..24, "<br>", &"Entry#{&1}")

    result =
      pages(
        "<div style='height:30pt'>Heading</div><div style='columns:2;column-fill:auto;column-gap:10pt;orphans:1;widows:1'>#{content}</div><p>After</p>",
        100
      )

    assert Enum.map(result, fn page ->
             Enum.count(texts(page), &String.starts_with?(&1, "Entry"))
           end) == [10, 14, 0]

    assert Enum.flat_map(result, &texts/1) ==
             ["Heading"] ++ Enum.map(1..24, &"Entry#{&1}") ++ ["After"]
  end

  test "column entry uses placed preceding flow rather than source height modulo the page" do
    content = Enum.map_join(1..12, "<br>", &"Entry#{&1}")

    result =
      pages(
        "<div style='height:70pt;break-inside:avoid'>First</div><div style='height:70pt;break-inside:avoid'>Second</div><div style='columns:2;column-fill:auto;orphans:1;widows:1'>#{content}</div>",
        100
      )

    assert Enum.map(result, fn page ->
             Enum.count(texts(page), &String.starts_with?(&1, "Entry"))
           end) == [0, 4, 8]
  end

  test "short first-page column sets balance within the available space and defer oversized atoms" do
    for fill <- ["auto", "balance"] do
      result =
        pages(
          "<div style='height:30pt'>Heading</div><div style='columns:2;column-fill:#{fill}'>Short</div><p>After</p>",
          100
        )

      assert Enum.map(result, &texts/1) == [["Heading", "Short", "After"]]
    end

    for fill <- ["auto", "balance"] do
      content = Enum.map_join(1..6, "<br>", &"Short#{&1}")

      result =
        pages(
          "<div style='height:30pt'>Heading</div><div style='columns:2;column-fill:#{fill};orphans:1;widows:1'>#{content}</div>",
          100
        )

      assert Enum.flat_map(result, &texts/1) == ["Heading"] ++ Enum.map(1..6, &"Short#{&1}")
    end

    result =
      pages(
        "<div style='height:70pt'>Heading</div><div style='columns:2'><div style='height:40pt;break-inside:avoid'>Atom</div></div>",
        100
      )

    assert Enum.map(result, &texts/1) == [["Heading"], ["Atom"]]
  end

  test "rebalancing a final column set retains divisible blank space" do
    result =
      pages("<div style='columns:2'><div style='height:250pt'>Title</div></div><p>After</p>", 100)

    assert Enum.flat_map(result, &texts/1) == ["Title", "After"]
    assert length(result) == 2
  end

  test "parallel line fragments keep internal link destinations on the actual continuation page" do
    content =
      Enum.map_join(1..10, "<br>", fn index ->
        if index == 9, do: "<span id='late'>Line9</span>", else: "Line#{index}"
      end)

    result =
      pages(
        "<div style='display:flex'><div style='width:100pt'>#{content}</div></div><p><a href='#late'>Jump</a></p>",
        50
      )

    links =
      Enum.flat_map(result, fn page -> descendants(page.boxes) end)
      |> Enum.filter(&Map.has_key?(&1, :link_destination))

    assert [%{link_destination: {:xyz, 3, x, y}}] = links
    assert_in_delta x, 0, 0.01
    assert_in_delta y, 50, 0.01
    assert {:ok, _} = NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render(result)
  end

  test "oversized avoided parallel groups still continue at complete source lines" do
    result =
      pages("<div style='display:flex;break-inside:avoid'>#{paragraph("Tall", 10)}</div>", 50)

    assert Enum.flat_map(result, &texts/1) == Enum.map(1..10, &"Tall#{&1}")
    assert length(result) == 3
  end

  test "malformed continuation descriptors return diagnostic failures" do
    alias NativeElixirPdfUtilities.Validators.LayoutFragmentationValidator, as: Validator

    for box <- [
          %{parallel_item_bounds: {0, 1}},
          %{parallel_item_bounds: :forged},
          %{
            type: :flow_marker,
            y: 0,
            height: 0,
            parallel_item_bounds: {1, 0},
            parallel_source_flow: :forged
          },
          %{
            type: :flow_marker,
            y: 0,
            height: 0,
            parallel_item_bounds: {1, 0},
            parallel_source_flow: %{flow_id: :p, fragment_id: {:line, :p, 0}, orphans: 0}
          }
        ] do
      assert {:error, {:invalid_layout, %{stage: :pagination, message: message}}} =
               Validator.prepare_parallel_units([box], 50)

      assert message != ""
    end

    assert {:error, {:invalid_layout, _}} =
             Validator.validate_style(%{break_inside: :page}, "break")

    assert {:ok, :avoid_column} = Validator.prepare_break("break-inside", "avoid-column")
    assert {:ok, :avoid_page} = Validator.prepare_break("break-inside", "avoid-page")
  end

  test "parallel reservations merge runs on one line and preserve fitting widow/orphan groups" do
    alias NativeElixirPdfUtilities.Validators.LayoutFragmentationValidator, as: Validator

    lines =
      for index <- 0..5 do
        %{
          type: :text,
          y: 91 - index * 12,
          font_size: 10,
          line_height: 12,
          line_baseline_depth: 9,
          line_box_height: 12,
          parallel_item_bounds: {100, 28},
          parallel_source_flow: %{
            flow_id: :p,
            fragment_id: {:line, :p, index},
            orphans: 2,
            widows: 2
          }
        }
      end

    {:ok, units} = Validator.prepare_parallel_units([%{type: :rect} | [hd(lines) | lines]], 50)
    assert {100, 76} in units
    assert {52, 28} in units
    assert Validator.whole_unit_cut(units, 100, 88) == 100
    assert Validator.whole_unit_cut(units, 100, 64) == 64
  end

  defp pages(content, height) do
    html =
      "<style>body{margin:0;font-size:10pt;line-height:12pt}p{margin:0}</style><body>#{content}</body>"

    {:ok, dom} = HtmlParser.parse(html)
    {:ok, styled} = Style.compute(dom, system_font_discovery: false)
    {:ok, layout} = Layout.layout(styled, page_size: {200, height}, margin: 0)
    {:ok, pages} = Pagination.paginate(layout)
    pages
  end

  defp paragraph(prefix, count),
    do:
      "<div style='width:100pt'>" <>
        Enum.map_join(1..count, "<br>", &"#{prefix}#{&1}") <> "</div>"

  defp texts(page) do
    Enum.flat_map(page.boxes, fn box ->
      case box do
        %{type: :text, text: text} -> [text]
        %{boxes: children} -> texts(%{boxes: children})
        _ -> []
      end
    end)
  end

  defp descendants(boxes),
    do: Enum.flat_map(boxes, fn box -> [box | descendants(Map.get(box, :boxes, []))] end)
end
