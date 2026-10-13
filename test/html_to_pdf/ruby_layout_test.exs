defmodule NativeElixirPdfUtilities.HtmlToPdf.RubyLayoutTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, RubyLayout, Style}

  test "relative ruby shifts base and annotation together and retains positioned children" do
    boxes = fn position ->
      html =
        "<p><ruby style='position:#{position};left:8pt;top:3pt'>Base<rt>Reading</rt><span style='position:absolute;left:0;top:0'>ABS</span></ruby> tail</p>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled)
      layout.boxes
    end

    ordinary = boxes.("static")
    shifted = boxes.("relative")

    for text <- ["Base", "Reading"] do
      before = Enum.find(ordinary, &(Map.get(&1, :text) == text))
      after_box = Enum.find(shifted, &(Map.get(&1, :text) == text))
      assert_in_delta after_box.x - before.x, 8, 0.001
      assert_in_delta after_box.y - before.y, -3, 0.001
    end

    assert Enum.any?(shifted, &(Map.get(&1, :text) == "ABS"))
  end

  test "an authored annotation display override uses ordinary inline fallback" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p><ruby>Base<rt style='display:inline'>Reading</rt></ruby></p>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    base = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Base"))
    reading = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Reading"))
    assert_in_delta base.y, reading.y, 0.001
    assert reading.font_size == base.font_size / 2
  end

  test "ruby bases and annotations retain text style and suppress fallback parentheses" do
    html = "<p>Before <ruby>Base<rp>(</rp><rt>Reading</rt><rp>)</rp></ruby> After</p>"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    base = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Base"))
    annotation = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Reading"))
    assert annotation.font_size == base.font_size / 2
    assert annotation.y > base.y
    assert_in_delta base.x + base.width / 2, annotation.x + annotation.width / 2, 0.001
    refute Enum.any?(layout.boxes, &(Map.get(&1, :text) in ["(", ")"]))
  end

  test "ruby groups wrap as complete base annotation pairs and paginate on their line" do
    html =
      "<body style='margin:0'><p style='margin:0;width:36pt'><ruby>First <rt>one</rt>Second <rt>two</rt>Third<rt>three</rt></ruby></p></body>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {70, 48}, margin: 4)

    assert {:ok, pages} = NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(layout)

    page_boxes =
      Enum.flat_map(Enum.with_index(pages), fn {page, index} ->
        Enum.map(page.boxes, &Map.put(&1, :page, index))
      end)

    pairs =
      Enum.map([{"First", "one"}, {"Second", "two"}, {"Third", "three"}], fn {base, reading} ->
        base = Enum.find(page_boxes, &(String.trim(Map.get(&1, :text, "")) == base))
        reading = Enum.find(page_boxes, &(Map.get(&1, :text) == reading))
        assert base.page == reading.page
        assert reading.y > base.y
        {base.page, base.y}
      end)

    assert length(Enum.uniq(pairs)) == 3
  end

  test "legacy annotation containers preserve ordinary inline fallback content" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p><ruby><rb>A</rb><rb>B</rb><rtc><rt>a</rt><rt>b</rt></rtc></ruby></p>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)

    [a, b, lower_a, lower_b] =
      Enum.map(~w(A B a b), fn text -> Enum.find(layout.boxes, &(Map.get(&1, :text) == text)) end)

    assert a.y == b.y
    assert lower_a.y == lower_b.y and lower_a.y == a.y
    assert lower_a.font_size == a.font_size
    assert lower_a.x < lower_b.x
  end

  test "ruby failure uses the shared actionable diagnostics contract" do
    assert {:error, {:invalid_layout, diagnostic}} = RubyLayout.measure(%{}, nil)
    assert diagnostic.stage == :layout
    assert diagnostic.reason == :invalid_layout
    assert diagnostic.operation == :measure_ruby
    assert diagnostic.source == "ruby"
    assert diagnostic.message =~ "styled ruby"
  end

  test "CSS list items use ordinal markers outside native list containers" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='display:list-item;list-style-type:decimal'>First</div><div style='display:list-item;list-style-type:decimal'>Second</div>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert Enum.any?(layout.boxes, &(Map.get(&1, :text) == "1."))
    assert Enum.any?(layout.boxes, &(Map.get(&1, :text) == "First"))
  end

  test "legacy menu and dir retain native list layout" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<menu><li>Menu</li></menu><dir><li>Directory</li></dir>")

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert Enum.count(layout.boxes, &(Map.get(&1, :text) == "•")) == 2
  end

  @marker_image "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

  test "marker images use native image geometry inside and outside list items" do
    for position <- ~w(inside outside) do
      html =
        "<ul style='list-style-image:url(#{@marker_image});list-style-position:#{position}'><li>Image marker</li></ul>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled)
      [image] = Enum.filter(layout.boxes, &(&1.type == :image))
      text = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Image marker"))
      assert image.width == 12.0 and image.height == 6.0
      assert image.x < text.x
      refute Enum.any?(layout.boxes, &(Map.get(&1, :text) == "•"))
    end
  end

  test "list shorthand resets inherited marker images and accepts URL plus ordinal position" do
    html =
      "<ul style='list-style-image:url(#{@marker_image})'><li style='list-style:none'>None</li><li style='list-style:decimal inside url(#{@marker_image})'>Picture</li></ul>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)
    assert Enum.count(layout.boxes, &(&1.type == :image)) == 1
  end

  test "ruby atoms retain annotations as block, flex and grid items" do
    for container <- ["<div style='display:flex'>", "<div style='display:grid'>", "<div>"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 container <> "<ruby style='display:block'>Base<rt>Reading</rt></ruby></div>"
               )

      assert {:ok, styled} = Style.compute_detailed(dom)
      assert {:ok, layout} = Layout.layout(styled)
      base = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Base"))
      annotation = Enum.find(layout.boxes, &(Map.get(&1, :text) == "Reading"))
      assert annotation.y > base.y
      assert annotation.font_size == base.font_size / 2
    end
  end

  test "malformed caller supplied marker images return actionable layout diagnostics" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<ul><li>Marker</li></ul>")
    assert {:ok, %{children: [list]} = styled} = Style.compute_detailed(dom)
    list = %{list | style: Map.put(list.style, :list_style_image, %{width: -1, height: 6})}
    assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(%{styled | children: [list]})
    assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
    assert diagnostic.message =~ "layout"
  end

  test "explicit base sequences assign direct annotations in order" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p><ruby><rb>A</rb><rb>B</rb><rt>a</rt><rt>b</rt></ruby></p>"
             )

    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled)

    boxes =
      Map.new(Enum.filter(layout.boxes, &(Map.get(&1, :text) in ~w(A B a b))), &{&1.text, &1})

    assert_in_delta (boxes["A"].x + boxes["B"].x + boxes["B"].annotation_width) / 2,
                    boxes["a"].x + boxes["a"].annotation_width / 2,
                    0.001

    assert boxes["b"].x >= boxes["B"].x + boxes["B"].annotation_width
  end

  test "caller supplied ruby measurements are rejected before native painting" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<ruby>Base<rt>Reading</rt></ruby>")
    assert {:ok, %{children: [ruby]} = styled} = Style.compute_detailed(dom)
    ruby = %{ruby | style: Map.put(ruby.style, :_ruby_box, %{})}
    assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(%{styled | children: [ruby]})
    assert diagnostic.source == "ruby"
    assert diagnostic.message =~ "internal inline state"
  end

  test "nested inline wrappers preserve ruby overhang exactly once" do
    text = "Wide <ruby>X<rt>annotation</rt></ruby> end."

    geometries =
      for content <- [text, "<span><span>#{text}</span></span>"] do
        assert {:ok, dom} = HtmlParser.parse_detailed("<p>#{content}</p>")
        assert {:ok, styled} = Style.compute_detailed(dom)
        assert {:ok, layout} = Layout.layout(styled)

        layout.boxes
        |> Enum.filter(&(&1.type == :text))
        |> Enum.map(&{&1.text, &1.x, &1.y, &1.width})
      end

    assert Enum.at(geometries, 0) == Enum.at(geometries, 1)
  end

  test "Ruby optional tag parsing produces the same native base and reading boxes" do
    for {omitted, explicit} <- [
          {"<ruby><rb>A<rb>B<rt>a<rt>b</ruby>",
           "<ruby><rb>A</rb><rb>B</rb><rt>a</rt><rt>b</rt></ruby>"},
          {"<ruby>A<rp>(<rt>a<rp>)</ruby>", "<ruby>A<rp>(</rp><rt>a</rt><rp>)</rp></ruby>"},
          {"<ruby><rb>A<rtc><rt>a<rt>b<rtc>other</ruby>",
           "<ruby><rb>A</rb><rtc><rt>a</rt><rt>b</rt></rtc><rtc>other</rtc></ruby>"}
        ] do
      boxes =
        for html <- [omitted, explicit] do
          assert {:ok, dom} = HtmlParser.parse_detailed("<p>#{html}</p>")
          assert {:ok, styled} = Style.compute_detailed(dom)
          assert {:ok, layout} = Layout.layout(styled)

          layout.boxes
          |> Enum.filter(&(&1.type == :text))
          |> Enum.map(&Map.take(&1, [:text, :x, :y, :annotation_width, :font_size]))
        end

      assert Enum.at(boxes, 0) == Enum.at(boxes, 1)
    end
  end

  test "anywhere wrapping partitions base and reading text without losing glyph content" do
    html =
      "<p style='margin:0;width:70pt;line-height:30pt;line-break:anywhere'><ruby>Alpha<rt>first</rt>Beta<rt>second</rt>Gamma<rt>third</rt>Delta<rt>fourth</rt></ruby></p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    assert {:ok, layout} = Layout.layout(styled, page_size: {100, 100}, margin: 10)
    text = Enum.filter(layout.boxes, &(&1.type == :text))
    bases = Enum.filter(text, &(&1.font_size == 12.0))
    readings = Enum.filter(text, &(&1.font_size == 6.0))
    assert Enum.map_join(bases, & &1.text) == "AlphaBetaGammaDelta"
    assert Enum.map_join(readings, & &1.text) == "firstsecondthirdfourth"
    assert Enum.any?(bases, &(&1.text == "Del"))
    assert Enum.any?(readings, &(&1.text == "four"))
    assert Enum.any?(bases, &(&1.text == "ta"))
    assert Enum.any?(readings, &(&1.text == "th"))
    assert {:ok, pages} = NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(layout)

    for page <- pages do
      assert Enum.any?(page.boxes, &(Map.get(&1, :role) == :ruby_line))
    end
  end

  test "unconstrained inherited anywhere measurement preserves an annotation word on one line" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<ruby style='line-break:anywhere'>Gamma<rt>third</rt></ruby>"
             )

    assert {:ok, %{children: [ruby]}} = Style.compute_detailed(dom)
    assert {:ok, [measured]} = RubyLayout.measure(ruby, nil)
    readings = Enum.filter(measured.boxes, &(Map.get(&1, :font_size) == 6.0))
    assert Enum.map(readings, & &1.text) == ["third"]
  end

  test "Ruby caller input and internal clearance metadata fail through validators" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<ruby>Base<rt>Reading</rt></ruby>")
    assert {:ok, %{children: [ruby]} = styled} = Style.compute_detailed(dom)
    broken = %{ruby | children: [%{type: :text, text: <<255>>, style: ruby.style}]}
    assert {:error, {:invalid_layout, diagnostic}} = RubyLayout.measure(broken, nil)
    assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
    injected = %{ruby | style: Map.put(ruby.style, :_ruby_clearance, :invalid)}

    assert {:error, {:invalid_layout, diagnostic}} =
             Layout.layout(%{styled | children: [injected]})

    assert diagnostic.message =~ "internal inline state"
  end

  test "zero-size ruby remains unsplittable without division by zero" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<ruby style='font-size:0;line-break:anywhere'>Letters<rt>reading</rt></ruby>"
             )

    assert {:ok, %{children: [ruby]}} = Style.compute_detailed(dom)
    assert {:ok, [measured]} = RubyLayout.measure(ruby, nil)
    assert measured.width == 0
    assert {:ok, nil} = RubyLayout.split(measured, 0)
  end
end
