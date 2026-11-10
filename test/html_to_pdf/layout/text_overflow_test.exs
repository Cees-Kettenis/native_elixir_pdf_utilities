defmodule NativeElixirPdfUtilities.HtmlToPdf.TextOverflowTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}
  alias NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator
  alias NativeElixirPdfUtilities.TestSupport.CssStyles

  # CSS Overflow 3, 7 October 2025, section 5.1, and Overflow 4,
  # 21 March 2023, sections 4 and 5. Graphemes and selected ligatures are indivisible.
  test "ellipsis consumes selected shaping clusters and retains only visible link text" do
    html =
      "<div style='width:70pt;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;font:12pt \"DejaVu Sans\"'><a href='https://example.com'>office office office</a></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, result} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
    text = Enum.filter(result.boxes, &(&1.type == :text))
    assert Enum.map_join(text, & &1.text) =~ "…"
    refute Enum.map_join(text, & &1.text) =~ "office office office"
    marker = Enum.find(text, &(&1.text == "…"))
    refute Map.get(marker, :link_url)
    linked = Enum.filter(text, &(Map.get(&1, :link_url) == "https://example.com"))
    assert linked != []
    assert Enum.all?(linked, &(&1.x + &1.annotation_width <= marker.x + 0.01))

    style = CssStyles.by_id("<span id=a style='font:12pt \"DejaVu Sans\"'></span>")["a"]

    assert NativeGlyphSourceValidator.cluster_boundaries(%{text: "fiA\u0301Z", style: style}) == [
             0,
             2,
             5,
             6
           ]

    assert NativeGlyphSourceValidator.cluster_boundaries(%{text: "", style: style}) == [0]
  end

  test "clamping discards trailing shaped lines and following flow uses the retained line height" do
    html =
      "<body style='margin:0;font:10pt \"DejaVu Sans\";line-height:12pt'><div style='width:60pt;line-clamp:2'>First line<br>Second line<br>Third line</div><p style='margin:0'>After</p></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "First line" and text =~ "…" and text =~ "After"
    refute text =~ "Third"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, result} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
    after_text = Enum.find(result.boxes, &(Map.get(&1, :text) == "After"))
    first = Enum.find(result.boxes, &(Map.get(&1, :text) == "First line"))
    assert_in_delta first.y - after_text.y, 24, 0.01
  end

  test "a discard budget crosses ordinary descendant blocks and retains the last line's font metrics" do
    html =
      "<body style='margin:0;font:10pt \"DejaVu Sans\";line-height:12pt'><div style='width:120pt;line-clamp:3'><div>First<br>Second</div><p style='margin:0;font-size:20pt;line-height:24pt'>Third</p><p style='margin:0'>Discarded</p></div><p style='margin:0'>After</p></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "First" and text =~ "Second" and text =~ "Third" and text =~ "…"
    refute text =~ "Discarded"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, result} = Layout.layout(styled, page_size: {200, 200}, margin: 0)
    after_box = Enum.find(result.boxes, &(Map.get(&1, :text) == "After"))
    first = Enum.find(result.boxes, &(Map.get(&1, :text) == "First"))
    assert_in_delta first.y - after_box.y, 48, 0.01
  end

  test "an exact last line needs no marker and independent contexts do not consume outer lines" do
    for content <- [
          "<div>First</div><div>Last</div>",
          "<div>First</div><div style='display:flow-root'>Inner one<br>Inner two<br>Inner three</div><div>Last</div>"
        ] do
      html = "<div style='line-clamp:2'>#{content}</div>"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "First" and text =~ "Last"
      refute text =~ "…"
    end
  end

  test "nested discard limits count the same line boxes as the published Overflow 4 examples" do
    for {maximum, expected, absent} <- [
          {5, ["a: line 1", "a: line 2", "b: line 1", "b: line 2…", "a: line 3…"],
           ["b: line 3", "a: line 4"]},
          {3, ["a: line 1", "a: line 2", "b: line 1…"], ["b: line 2", "a: line 3"]}
        ] do
      html =
        "<div style='width:200pt;line-clamp:#{maximum}'>a: line 1<br>a: line 2<br><div style='line-clamp:2'>b: line 1<br>b: line 2<br>b: line 3<br>b: line 4<br></div>a: line 3<br>a: line 4<br></div>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      for line <- expected, do: assert(text =~ line)
      for line <- absent, do: refute(text =~ line)
    end
  end

  test "max-lines without a capturing discard container does not invent page breaks" do
    html =
      "<style>@page{size:200pt 200pt;margin:0}</style><div style='max-lines:1;block-ellipsis:auto'>First<br>Second<br>Third</div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "First" and text =~ "Second" and text =~ "Third"
    refute text =~ "…"
    assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
  end

  test "block ellipses use soft wrap boundaries and none suppresses only the marker" do
    for {marker, expected} <- [{"auto", "Second…"}, {"none", "Second line"}] do
      html =
        "<div style='font:10pt \"DejaVu Sans\";line-height:12pt;width:60pt;line-clamp:2 #{marker}'>First line<br>Second line<br>Third line</div>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ expected
      refute text =~ "Third"
    end
  end

  test "nonoverflowing and visible-overflow text keeps its original source and markers stay absent" do
    for {width, overflow} <- [{200, "hidden"}, {20, "visible"}] do
      html =
        "<div style='width:#{width}pt;white-space:nowrap;overflow:#{overflow};text-overflow:ellipsis'>Unchanged source</div>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Unchanged source"
      refute text =~ "…"
    end
  end

  test "narrow overflow retains its first cluster and RTL ellipses select the physical left marker" do
    for {extra, marker} <- [
          {"width:1pt", false},
          {"width:35pt;direction:rtl;text-overflow:'L' 'R'", "L"},
          {"width:35pt;text-overflow:clip ellipsis", "…"},
          {"width:35pt;direction:rtl;text-overflow:clip ellipsis", false}
        ] do
      html =
        ~s(<div style="font:12pt sans-serif;overflow:hidden;white-space:nowrap;text-overflow:ellipsis;#{extra}">ABCDEFGH</div>)

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(tree, page_size: {200, 200}, margin: 0)
      text = Enum.filter(result.boxes, &(&1.type == :text)) |> Enum.map_join(& &1.text)

      cond do
        extra == "width:1pt" ->
          first = Enum.find(result.boxes, &(Map.get(&1, :text) == "A"))
          assert first != nil and first.width > 0
          ellipsis = Enum.find(result.boxes, &(Map.get(&1, :text) == "…"))
          assert ellipsis.x >= 1

        marker ->
          assert text =~ marker

        true ->
          refute text =~ "…"
      end
    end
  end

  test "block markers handle shaped scripts custom strings and atomic descendants without losing following flow" do
    for {content, declaration} <- [
          {"office office office<br>office office office", "line-clamp:1 'END'"},
          {"אבג דהו זחט<br>אבג דהו זחט", "direction:rtl;line-clamp:1"},
          {"<span style='display:inline-block;width:80pt'>Atomic</span><br>Lost", "line-clamp:1"},
          {"ABC<br>DEF", "line-clamp:1 ''"},
          {"<span style='font-family:monospace'>ABC</span><br>DEF", "line-clamp:1 '😀'"}
        ] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 ~s(<div style="width:60pt;#{declaration}">#{content}</div><p>After</p>),
                 system_font_discovery: false
               )

      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "After"
      refute text =~ "Lost"
    end
  end

  test "discard containers break at their used height and place one marker on the final retained paragraph" do
    for limit <- [
          "height:24pt",
          "max-height:24pt",
          "max-height:30pt;box-sizing:border-box;padding:3pt",
          "height:30pt",
          "height:30pt;box-sizing:border-box;padding:3pt"
        ] do
      for content <- [
            "First<br>Second<br>Third<br>Fourth",
            "<p style='margin:0'>First</p><p style='margin:0'>Second</p><p style='margin:0'>Third</p><p style='margin:0'>Fourth</p>",
            "<div style='display:flow-root'>First<br>Second<br>Third<br>Fourth</div>"
          ] do
        html =
          ~s(<div style="font:10pt sans-serif;line-height:12pt;continue:discard;block-ellipsis:auto;#{limit}">#{content}</div><p>After</p>)

        assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
        assert {:ok, text} = Text.extract(pdf)
        assert text =~ "First" and text =~ "Second" and text =~ "After"
        refute text =~ "Third" or text =~ "Fourth"
        assert length(String.split(text, "…")) == 2
      end
    end
  end

  test "ellipsis tests painted positions after relative movement and atomic transforms" do
    for content <- [
          "<span style='position:relative;left:30pt'>ABCDEFGH</span>",
          "A <span style='display:inline-block;width:55pt;transform:translateX(30pt)'>Atomic</span>"
        ] do
      html =
        "<div style='font:10pt sans-serif;width:60pt;overflow:hidden;white-space:nowrap;text-overflow:ellipsis'>#{content}</div><p>After</p>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, result} = Layout.layout(tree, page_size: {200, 200}, margin: 0)
      marker = Enum.find(result.boxes, &(Map.get(&1, :text) == "…"))
      assert marker != nil
      assert marker.x >= 0 and marker.x < 60
      assert {:ok, _pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    end
  end

  test "discard boundaries ignore noncontent and restore line state after a failed child" do
    alias NativeElixirPdfUtilities.HtmlToPdf.LineClamp
    style = %{continue: :discard, max_lines: 1}
    assert {:error, :child_failed} = LineClamp.layout(style, fn -> {:error, :child_failed} end)
    assert {[[:line], [:second]], []} = LineClamp.take([[:line], [:second]], 10, fn _ -> 1 end)

    for {child, expected} <- [
          {%{style: %{display: :none}}, "original"},
          {%{style: %{_collapses_through: true}}, "original"},
          {%{style: %{_static_position_anchor: make_ref()}}, "original"},
          {%{type: :text, text: " "}, "original"},
          {%{type: :inline_flow, children: [%{style: %{display: :none}}]}, "original"},
          {%{type: :inline_flow, children: [%{type: :text, text: "", style: %{}}]}, "original"},
          {%{type: :inline_flow, children: [%{type: :element, style: %{display: :inline_block}}]},
           "ellipsis"}
        ] do
      assert {:ok, [%{text: ^expected}], 1} =
               LineClamp.layout(style, fn ->
                 {[_], ids} = LineClamp.take([[:line]], 10, fn _ -> 1 end)
                 LineClamp.discard(child)

                 {:ok,
                  [
                    %{text: "original", _line_clamp_choice: {ids, false}},
                    %{text: "ellipsis", _line_clamp_choice: {ids, true}}
                  ], 1}
               end)
    end
  end

  test "mixed and narrow overflow keeps shaped boundaries for both physical edges and fallback markers" do
    for {style, content} <- [
          {"width:50pt;direction:rtl;text-overflow:'L' 'R'",
           "<span style='position:relative;left:-30pt'>ABCDEFGHIJKLMN</span>"},
          {"width:50pt;text-align:center;text-overflow:ellipsis ellipsis",
           "<span style='position:relative;left:1pt'>ABCDEFGHIJKLMN</span>"},
          {"width:30pt", "A<span style='position:relative;left:100pt'>Hidden</span>"},
          {"width:100pt", "<span style='position:relative;left:1pt'>Fits</span>"},
          {"width:40pt",
           "<span style='display:inline-block;position:relative;left:20pt;width:30pt'>Atomic</span>"},
          {"width:10pt;direction:rtl", "ABCDEFGH"},
          {"width:40pt", "A <span style='display:inline-block;width:40pt'>Atomic</span>"},
          {"width:50pt;font-family:math;text-overflow:'😀'", "ABCDEFGHIJKLMN"},
          {"width:30pt;line-clamp:1", "One<wbr>two<wbr>three<wbr>four five six"}
        ] do
      html =
        "<div style=\"font:10pt sans-serif;line-height:12pt;overflow:hidden;white-space:nowrap;text-overflow:ellipsis;#{style}\">#{content}</div>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert is_binary(pdf)
    end
  end

  test "discard regions break before replaced blocks that cannot fit after retained text" do
    image =
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

    for content <- [
          "<img src='#{image}' style='display:block;width:24pt;height:24pt'>",
          "<svg style='display:block;width:24pt;height:24pt'><rect width=24 height=24 /></svg>"
        ] do
      html =
        "<div style='height:24pt;continue:discard;block-ellipsis:auto;font:10pt sans-serif;line-height:12pt'>First#{content}<p>Discarded</p></div><p>After</p>"

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "First" and text =~ "…" and text =~ "After"
      refute text =~ "Discarded"
    end

    alias NativeElixirPdfUtilities.HtmlToPdf.LineClamp
    assert LineClamp.place_atomic(10, 0)

    assert {:ok, [], 1} =
             LineClamp.layout(
               %{continue: :discard},
               fn ->
                 assert LineClamp.place_atomic(10, -10)
                 {:ok, [], 1}
               end,
               %{top: 10, height: 5}
             )
  end
end
