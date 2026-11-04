defmodule NativeElixirPdfUtilities.HtmlToPdf.TableBorderEdgeProfilesTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, PdfWriter, Style}
  alias NativeElixirPdfUtilities.Validators.{CssValueValidator, HtmlTableValidator}
  alias NativeElixirPdfUtilities.TestSupport.TableGeometry

  test "legacy frame hints prepare each authored outer-side selection" do
    for {frame, sides} <- [
          {"void", []},
          {"above", [:top]},
          {"below", [:bottom]},
          {"hsides", [:top, :bottom]},
          {"lhs", [:left]},
          {"rhs", [:right]},
          {"vsides", [:left, :right]},
          {"box", [:top, :right, :bottom, :left]},
          {"border", [:top, :right, :bottom, :left]}
        ] do
      assert {:ok, hints} =
               HtmlTableValidator.presentation_hints(
                 %{tag: "table", attributes: %{"frame" => frame}},
                 [],
                 %{}
               )

      assert hints.border_widths == CssValueValidator.edges(0.75)

      assert hints.border_styles ==
               Map.new(
                 [:top, :right, :bottom, :left],
                 &{&1, if(&1 in sides, do: :solid, else: :hidden)}
               )
    end
  end

  test "prepared legacy cell colors use inherited canonical border colors" do
    colors = CssValueValidator.edges({1, 0, 0})

    assert {:ok, hints} =
             HtmlTableValidator.presentation_hints(
               %{tag: "td", attributes: %{}},
               [%{tag: "table", attributes: %{"border" => "1"}}],
               %{border_colors: colors}
             )

    assert hints.border_colors == colors
    assert hints.border_styles == CssValueValidator.edges(:inset)
  end

  test "none and groups rules suppress cell borders while groups decorate the shared axes" do
    for rules <- ["none", "groups"] do
      table = %{tag: "table", attributes: %{"rules" => rules}}

      assert {:ok, hints} =
               HtmlTableValidator.presentation_hints(%{tag: "td", attributes: %{}}, [table], %{})

      assert hints.border_styles == CssValueValidator.edges(:none)
    end

    table = %{tag: "table", attributes: %{"rules" => "groups"}}

    for tag <- ["thead", "tbody", "tfoot"] do
      assert {:ok, hints} =
               HtmlTableValidator.presentation_hints(%{tag: tag, attributes: %{}}, [table], %{})

      assert hints.border_widths == CssValueValidator.edges(0.75)
      assert hints.border_styles == %{top: :solid, right: :none, bottom: :solid, left: :none}
    end

    assert {:ok, hints} =
             HtmlTableValidator.presentation_hints(
               %{tag: "colgroup", attributes: %{}},
               [table],
               %{}
             )

    assert hints.border_widths == CssValueValidator.edges(0.75)
    assert hints.border_styles == %{top: :none, right: :solid, bottom: :none, left: :solid}
  end

  test "collapsed relief maps table outlines and cell edges to CSS ridge and groove" do
    for {authored, painted} <- [{"inset", :ridge}, {"outset", :groove}] do
      html =
        "<table style='border-collapse:collapse;border:4pt #{authored} red'><tr><td style='border:2pt #{authored} blue'>Outline</td></tr></table><table style='border-collapse:collapse'><tr><td style='border:2pt #{authored} blue'>Cell</td></tr></table>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute(dom)
      assert {:ok, layout} = Layout.layout(styled)

      borders =
        Enum.filter(layout.boxes, &(Map.get(&1, :role) in [:table_border, :table_outline]))

      assert length(borders) >= 2

      assert Enum.all?(
               borders,
               &Enum.all?(Map.values(&1.border_styles), fn style -> style == painted end)
             )

      assert {:ok, pdf} =
               PdfWriter.render([%{size: layout.page_size, boxes: borders}],
                 compress_streams: false
               )

      assert pdf =~ " rg"
    end
  end

  test "rowspanned left edge keeps distinct row winners and clips each vertical track" do
    html =
      "<table style='border-collapse:collapse;width:120pt'><tr style='border-left:2pt solid blue'><td rowspan=2 style='border-left:1pt solid red;vertical-align:top'>Span</td><td style='height:20pt'>Top</td></tr><tr style='border-left:3pt solid green'><td style='height:20pt'>Bottom</td></tr></table>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom)
    assert {:ok, layout} = Layout.layout(styled)

    borders =
      Enum.filter(
        TableGeometry.original_cell_artwork(layout.boxes),
        &(Map.get(&1, :role) == :table_border and Map.has_key?(&1, :clip) and
            &1.border_widths.left > 0)
      )

    assert length(borders) == 2
    assert Enum.sort(Enum.map(borders, & &1.border_widths.left)) == [1.5, 3.0]

    assert Enum.sort(Enum.map(borders, & &1.border_colors.left)) ==
             Enum.sort([{0, 0, 1}, {0, 0.5019607843, 0}])

    for border <- borders do
      assert border.clip.x == border.x - border.stroke_width
      assert border.clip.width == border.width + 2 * border.stroke_width
      assert border.clip.height > 0 and border.clip.height < border.height
    end

    [upper, lower] = Enum.sort_by(borders, & &1.clip.y, :desc)
    assert_in_delta lower.clip.y + lower.clip.height, upper.clip.y, 0.0001
  end

  test "dashed collapsed table left edge retains its source-specific raster alignment" do
    box = %{
      type: :rect,
      role: :table_border,
      x: 12.0,
      y: 20.0,
      width: 40.0,
      height: 24.0,
      fill_color: nil,
      stroke_color: {0, 0, 1},
      stroke_width: 0.75,
      border_widths: %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.75},
      border_styles: %{top: :none, right: :none, bottom: :none, left: :dashed},
      border_colors: CssValueValidator.edges({0, 0, 1}),
      border_radius: 0.0
    }

    assert {:ok, pdf} = PdfWriter.render([%{size: {120, 120}, boxes: [box]}], [])
    assert pdf =~ "[2 1]"
    assert pdf =~ "15.875 26.6667 m 15.875 58.6667 l"
  end
end
