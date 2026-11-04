defmodule NativeElixirPdfUtilities.HtmlToPdf.ListCounterTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "list items under the same ordered list share numbering through other elements" do
    styled =
      style("<ol><li>First<span style='display:list-item'>Second</span></li><li>Third</li></ol>")

    assert markers(styled) == ["1.", "2.", "3."]
  end

  test "nested lists restore their enclosing list counter" do
    styled = style("<ol><li>Outer<ol><li>Inner</li><li>Inner</li></ol></li><li>Outer</li></ol>")
    assert markers(styled) == ["1.", "1.", "2.", "2."]
  end

  test "CSS list items without an HTML list use their nearest box owner" do
    styled =
      style(
        "<section style='list-style-type:decimal'><div style='display:list-item'>A<span style='display:list-item'>B</span></div><div style='display:list-item'>C</div></section>"
      )

    assert markers(styled) == ["1.", "1.", "2."]
  end

  test "removing the list reset preserves the counter from a preceding list" do
    styled =
      style(
        "<ol><li>A</li><li>B</li></ol><ol style='counter-reset:none'><li>C</li><li>D</li></ol>"
      )

    assert markers(styled) == ["1.", "2.", "3.", "4."]
  end

  test "authored counter-set overrides value hints after incrementing" do
    styled =
      style(
        "<style>li:first-child{counter-set:list-item 7}li::before{content:counter(list-item)}</style><ol><li value=12>A</li><li>B</li></ol>"
      )

    assert markers(styled) == ["7.", "8."]
    assert generated(styled) == ["7", "8"]
  end

  test "counter-set survives public source preparation and text extraction" do
    alias NativeElixirPdfUtilities.{HtmlToPdf, Text}

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<style>li:first-child{counter-set:list-item 7}</style><ol><li>First</li><li>Second</li></ol>"
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "7. First"
    assert text =~ "8. Second"
  end

  test "invalid counter-set declarations return actionable public diagnostics" do
    alias NativeElixirPdfUtilities.HtmlToPdf

    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render(
               "<style>li{counter-set:list-item 1 2}</style><ol><li>First</li></ol>"
             )

    assert diagnostic.reason == :invalid_css
    assert diagnostic.stage == :css
    assert diagnostic.operation == :render
    assert diagnostic.module == HtmlToPdf
    assert diagnostic.source =~ "counter-set"
    assert diagnostic.line == 1
    assert diagnostic.column > 0
  end

  test "markers and generated content read the implicit list-item counter" do
    styled =
      style("<style>li::before{content:counter(list-item)}</style><ol><li>A</li><li>B</li></ol>")

    assert markers(styled) == ["1.", "2."]
    assert generated(styled) == ["1", "2"]
  end

  test "explicit list-item increments replace the implicit increment" do
    for {increment, expected} <- [
          {"list-item 2", ["2", "4"]},
          {"list-item 0", ["0", "0"]},
          {"none", ["1", "2"]},
          {"other", ["1", "2"]}
        ] do
      styled =
        style(
          "<style>li{counter-increment:#{increment}}li::before{content:counter(list-item)}</style><ol><li>A</li><li>B</li></ol>"
        )

      assert markers(styled) == Enum.map(expected, &(&1 <> "."))
      assert generated(styled) == expected
    end
  end

  test "authored resets override the HTML list starting hint" do
    styled =
      style(
        "<style>ol{counter-reset:list-item 7}li::before{content:counter(list-item)}</style><ol start=20><li>A</li><li>B</li></ol>"
      )

    assert markers(styled) == ["8.", "9."]
    assert generated(styled) == ["8", "9"]
  end

  test "an item reset leaves the parent counter available to following siblings" do
    styled =
      style(
        "<style>li::before{content:counter(list-item)}</style><ol><li style='counter-reset:list-item 7'>A</li><li>B</li></ol>"
      )

    assert markers(styled) == ["8.", "1."]
    assert generated(styled) == ["8", "1"]

    assert markers(
             style(
               "<ol reversed><li>A</li><li style='counter-reset:list-item 7'>B</li><li>C</li></ol>"
             )
           ) == ["2.", "8.", "1."]
  end

  test "start and value attributes update the same counter seen by generated content" do
    styled =
      style(
        "<style>li::before{content:counter(list-item)}</style><ol start=5><li>A</li><li value=9>B</li><li>C</li></ol>"
      )

    assert markers(styled) == ["5.", "9.", "10."]
    assert generated(styled) == ["5", "9", "10"]
  end

  test "reversed lists count owned items through descendants and exclude hidden items" do
    styled =
      style(
        "<style>.item::before{content:counter(list-item)}</style><ol reversed><li class=item>A<span class=item style='display:list-item'>B</span></li><li style='display:none'>Hidden</li><li class=item>C</li></ol>"
      )

    assert markers(styled) == ["3.", "2.", "1."]
    assert generated(styled) == ["3", "2", "1"]
  end

  test "explicit reversed starts and value overrides preserve descending state" do
    styled =
      style(
        "<style>li::before{content:counter(list-item)}</style><ol reversed start=5><li>A</li><li value=0>B</li><li>C</li></ol>"
      )

    assert markers(styled) == ["5.", "0.", "-1."]
    assert generated(styled) == ["5", "0", "-1"]
  end

  test "automatic reversed starts follow authored increments and counter sets" do
    for {increment, expected} <- [
          {"list-item 0", ["0.", "0."]},
          {"list-item -2", ["4.", "2."]},
          {"list-item 2", ["-4.", "-2."]},
          {"other", ["2.", "1."]}
        ] do
      assert markers(
               style(
                 "<style>li{counter-increment:#{increment}}</style><ol reversed><li>A</li><li>B</li></ol>"
               )
             ) == expected
    end

    assert markers(style("<ol reversed><li>A</li><li value=9>B</li><li>C</li></ol>")) ==
             ["10.", "9.", "8."]
  end

  test "reversed preparation isolates nested resets and includes unreset nested lists" do
    assert markers(
             style(
               "<ol reversed><li>A<div style='counter-reset:list-item 7'><span style='display:list-item'>B</span></div></li><li>C</li></ol>"
             )
           ) == ["2.", "8.", "1."]

    assert markers(
             style(
               "<ol reversed><li>A<ol style='counter-reset:none'><li>B</li></ol></li><li>C</li></ol>"
             )
           ) == ["3.", "2.", "1."]

    assert markers(
             style("<ol reversed><li>A<ol reversed><li>B</li><li>C</li></ol></li><li>D</li></ol>")
           ) == ["2.", "2.", "1.", "1."]

    assert markers(
             style(
               "<ol reversed><li>A<div style='display:contents;counter-reset:list-item 7'><span style='display:list-item'>B</span></div></li><li>C</li></ol>"
             )
           ) == ["3.", "2.", "1."]
  end

  test "reversed lists include open disclosure items but not summary increments or closed content" do
    for {open, expected} <- [{"", ["2", "1"]}, {"open", ["3", "2", "1"]}] do
      styled =
        style(
          "<style>.read::before{content:counter(list-item)}</style><ol reversed><li class=read>A<details #{open}><summary>Summary</summary><span class=read style='display:list-item'>B</span></details></li><li class=read>C</li></ol>"
        )

      assert generated(styled) == expected
    end
  end

  test "automatic reversed preparation propagates invalid pseudo-element styles" do
    for selector <- ["ol::before", "li::before", "li::after"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<style>#{selector}{content:'X';display:invalid}</style><ol reversed><li>A</li></ol>"
               )

      assert Style.compute(dom) == {:error, :invalid_document}
    end
  end

  test "generated list items share their enclosing counter and keep empty markers" do
    styled =
      style(
        "<style>li:first-child::after{display:list-item;content:'Generated'}li:last-child::after{display:list-item;content:''}</style><ol><li>A</li><li>B</li></ol>"
      )

    assert markers(styled) == ["1.", "2.", "3.", "4."]
  end

  test "reversed starting values include rendered generated list items" do
    styled =
      style(
        "<style>li:first-child::after{display:list-item;content:'Generated'}</style><ol reversed><li>A</li><li>B</li></ol>"
      )

    assert markers(styled) == ["3.", "2.", "1."]
  end

  test "counter declarations on suppressed pseudo-elements do not affect later items" do
    for rule <- ["content:none", "content:'Hidden';display:none", "color:red"] do
      styled =
        style(
          "<style>li:first-child::before{counter-increment:list-item 10;#{rule}}</style><ol><li>A</li><li>B</li></ol>"
        )

      assert markers(styled) == ["1.", "2."]
    end
  end

  test "boxless containers cannot change counters but their children can" do
    styled =
      style(
        "<style>.read::before{content:counter(item)}.boxless{display:contents;counter-reset:item 9;counter-increment:item 5}</style><div style='counter-reset:item'><span class=boxless>A</span><p class=read>B</p><span class=boxless><i style='counter-increment:item'></i></span><p class=read>C</p></div>"
      )

    assert generated(styled) == ["0", "1"]
  end

  test "empty generated content still changes counters when it creates a box" do
    styled =
      style(
        "<style>.first::before{content:'';counter-increment:item}.read::before{content:counter(item)}</style><div style='counter-reset:item'><p class=first>A</p><p class=read>B</p></div>"
      )

    assert generated(styled) == ["", "1"]
  end

  test "boxless HTML lists use the closest ancestor box as their item owner" do
    styled =
      style(
        "<section><ol style='display:contents'><li>A<span style='display:list-item'>B</span></li><li>C</li></ol></section>"
      )

    assert markers(styled) == ["1.", "2.", "3."]
  end

  test "empty reversed lists keep a zero counter and invalid prefetched styles return errors" do
    assert generated(
             style("<style>ol::after{content:counter(list-item)}</style><ol reversed></ol>")
           ) == ["0"]

    assert {:ok, dom} =
             HtmlParser.parse_detailed("<ol reversed><li style='display:invalid'>A</li></ol>")

    assert Style.compute(dom) == {:error, :invalid_document}
  end

  test "reversed preparation resolves each selected image once" do
    png = File.read!(Paths.fixture("html_to_pdf/browser_parity/assets/initial-small.png"))

    owner = self()

    assert {:ok, _pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<ol reversed><li>A<img src='image.png' width=1 height=1></li><li>B</li></ol>",
               asset_resolver: fn request ->
                 send(owner, {:list_image, request})
                 {:ok, png}
               end
             )

    assert_receive {:list_image, _request}
    refute_receive {:list_image, _request}
  end

  test "skipped reversed lists do not resolve their descendants" do
    assert {:ok, _pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<ol reversed hidden=until-found><li><img src='unused.png'></li></ol><p>Visible</p>",
               asset_resolver: fn _request -> flunk("skipped list resource was resolved") end
             )
  end

  defp style(html) do
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute(dom)
    styled
  end

  defp markers(node) do
    Enum.map(Map.get(Map.get(node, :style, %{}), :_list_marker, []), & &1.text) ++
      Enum.flat_map(Map.get(node, :children, []), &markers/1)
  end

  defp generated(node) do
    if Map.get(node, :tag) in ["::before", "::after"] do
      Enum.map(node.children, & &1.text)
    else
      Enum.flat_map(Map.get(node, :children, []), &generated/1)
    end
  end
end
