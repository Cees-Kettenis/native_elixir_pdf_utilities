defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialLetterTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.CssInitialLetterValidator

  test "short initial ink can leave a later exclusion band empty" do
    result =
      layout(
        "<p style='font:12pt/16pt serif;width:120pt'><span style='initial-letter:3;initial-letter-wrap:all'>-</span>Alphabet beta gamma delta epsilon zeta eta theta.</p>"
      )

    assert Enum.any?(result.boxes, &(Map.get(&1, :text) == "-"))

    assert {:ok, _} =
             HtmlToPdf.render(
               "<p><span style='initial-letter:3;initial-letter-wrap:all'>-</span>Text</p>",
               system_font_discovery: false
             )
  end

  test "independent formatting contexts enclose their painted initial and clear its exclusion" do
    for contents <- ["text", "<p style='margin:0'>text</p>"] do
      html =
        "<div style='display:flow-root;width:100pt;font:12pt/16pt serif'><span style='initial-letter:3;background:red;border:1pt solid blue'>A</span>#{contents}</div><p style='margin:0'>After</p>"

      boxes = layout(html).boxes
      initial = Enum.find(boxes, &(Map.get(&1, :text) == "A"))
      following = Enum.find(boxes, &(Map.get(&1, :text) == "After"))
      assert initial.font_size > 24
      assert following.y < initial.y
      assert Enum.any?(boxes, &(Map.get(&1, :fill_color) == {1.0, 0.0, 0.0}))
    end
  end

  test "preserved leading text stays outside the first-letter selection" do
    html =
      "<style>p{white-space:pre;font:12pt serif;margin:0}p::first-letter{color:red;font-size:24pt}</style><p>A text</p>"

    boxes = layout(html).boxes |> Enum.filter(&(&1.type == :text))
    assert Enum.find(boxes, &(&1.text == "A")).font_size == 24.0
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "A text"} = Text.extract(pdf, layout: false)
  end

  test "an independent parent encloses a short child paragraph's initial" do
    boxes =
      layout(
        "<div style='display:flow-root;font:12pt/16pt serif'><p style='margin:0'><span style='initial-letter:3'>A</span></p></div><p>After</p>"
      ).boxes

    initial = Enum.find(boxes, &(Map.get(&1, :text) == "A"))
    following = Enum.find(boxes, &(Map.get(&1, :text) == "After"))
    assert initial.font_size > 24
    assert following.y < initial.y
  end

  test "standalone child measurement retains an initial without inventing a pagination owner" do
    alias NativeElixirPdfUtilities.HtmlToPdf.ResourceContext
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               HtmlParser.parse("<style>p::first-letter{initial-letter:3}</style><p>A</p>")

      assert {:ok, %{children: [paragraph]}} =
               Style.compute_detailed(dom, system_font_discovery: false)

      assert {:ok, measured} = Layout.measure_child(paragraph, context)
      marker = Enum.find(measured.boxes, &(Map.get(&1, :role) == :initial_letter))
      assert marker.height > paragraph.style.line_height
      refute Map.has_key?(marker, :fragment_id)
      assert Enum.any?(measured.boxes, &(Map.get(&1, :text) == "A"))
    end)
  end

  test "initial-letter controls reject unknown declarations and multi-token wrap lengths" do
    assert :error = CssInitialLetterValidator.prepare(%{}, "unknown", "normal")
    assert :error = CssInitialLetterValidator.prepare(%{}, "initial-letter-wrap", "1pt 2pt")
  end

  test "initial sizing, sinking and alignment grammar has one admission owner" do
    for {source, expected} <- [
          {"2.5", {2.5, 2}},
          {"raise 3", {3.0, 1}},
          {"3 drop", {3.0, 3}},
          {"2 4", {2.0, 4}},
          {"normal", :normal}
        ] do
      assert {:ok, %{initial_letter: ^expected}} =
               CssInitialLetterValidator.prepare(%{}, "initial-letter", source)
    end

    for source <- ["0", "0.5", "2 0", "2 1.5", "raise", "2 raise drop", "2 3 raise"] do
      assert :error = CssInitialLetterValidator.prepare(%{}, "initial-letter", source)
    end

    assert {:ok, %{initial_letter_align: {:ideographic, true}}} =
             CssInitialLetterValidator.prepare(
               %{},
               "initial-letter-align",
               "border-box ideographic"
             )

    assert :error =
             CssInitialLetterValidator.prepare(
               %{},
               "initial-letter-align",
               "ideographic border-box"
             )

    assert {:ok, %{initial_letter_wrap: length}} =
             CssInitialLetterValidator.prepare(%{}, "initial-letter-wrap", "-40%")

    assert {:ok, -4.0} =
             NativeElixirPdfUtilities.Validators.CssLengthValidator.resolve(length, 10.0, %{})

    assert :ok = CssInitialLetterValidator.validate_style(%{}, "p")

    assert {:error, {:invalid_layout, diagnostic}} =
             CssInitialLetterValidator.validate_style(%{initial_letter: {2, 0}}, "p")

    assert diagnostic.stage == :layout and diagnostic.source == "p"
  end

  test "initial alignment fallbacks and rejected prepared controls retain their contract" do
    assert "initial-letter-align" in CssInitialLetterValidator.properties()

    assert {:ok, %{initial_letter_align: {:alphabetic, true}}} =
             CssInitialLetterValidator.prepare(%{}, "initial-letter-align", "border-box")

    for style <- [
          %{initial_letter: :bad},
          %{initial_letter_align: :bad},
          %{initial_letter_wrap: :bad},
          %{_first_letter_style: :bad},
          %{_first_letter_style: %{font_size: 10, line_height: 12, _first_letter_style: %{}}}
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               CssInitialLetterValidator.validate_style(style, "p")

      assert diagnostic.stage == :layout and diagnostic.source == "p"
    end

    for wrap <- [-2.0, {:percent, 0.2}] do
      assert :ok = CssInitialLetterValidator.validate_style(%{initial_letter_wrap: wrap}, "p")
    end

    font = %{
      type: :built_in,
      family: "Helvetica",
      pdf_name: "Helvetica",
      ascent: 800,
      descent: -200,
      units_per_em: 1000
    }

    for {mode, expected} <- [
          {:ideographic, {0.8, -0.2}},
          {:leading, {0.8, -0.2}},
          {:alphabetic, {0.66, 0.0}}
        ] do
      assert {:ok, ^expected} = CssInitialLetterValidator.alignment_metrics(font, mode, 12)
    end
  end

  test "first-letter styling retains surrounding punctuation and source text" do
    html =
      "<style>p{font:12pt serif;margin:0}p::first-letter{color:red;font-size:24pt}</style><p>\"<span>A</span>\"lphabet text</p>"

    boxes = layout(html).boxes |> Enum.filter(&(&1.type == :text))
    red = Enum.filter(boxes, &(&1.color == {1.0, 0.0, 0.0}))
    assert Enum.map_join(red, & &1.text) == "\"A\""
    assert Enum.all?(red, &(&1.font_size == 24.0))
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "\"A\"lphabet text"} = Text.extract(pdf, layout: false)
  end

  test "drop initials use cap height and reserve the first three lines" do
    html =
      "<style>p{font:12pt serif;line-height:16pt;margin:0;width:150pt}p::first-letter{initial-letter:3}</style><p>" <>
        String.duplicate("Alphabet text continues across the paragraph. ", 6) <> "</p>"

    boxes = layout(html).boxes |> Enum.filter(&(&1.type == :text))
    [initial | ordinary] = boxes
    assert initial.text == "A"
    assert initial.font_size > 36 and initial.font_size < 72
    lines = ordinary |> Enum.group_by(& &1.y) |> Enum.sort_by(fn {y, _} -> -y end)
    first_x = Enum.map(lines, fn {_, line} -> Enum.min(Enum.map(line, & &1.x)) end)
    assert length(first_x) > 3
    assert Enum.all?(Enum.take(first_x, 3), &(&1 > 0))
    assert Enum.at(first_x, 3) == 0.0
    [{first_baseline, _} | _] = lines
    assert_in_delta initial.y, first_baseline - 32, 0.001
  end

  test "definite initial widths honor center and right content alignment" do
    # CSS Inline 3, 2024-12-18, section 7.5.6 uses inline-axis bearings
    # inside a definite initial box: https://www.w3.org/TR/2024/WD-css-inline-3-20241218/#initial-letter-content-align
    positions =
      for alignment <- ["left", "center", "right"], into: %{} do
        html =
          "<p style='font:12pt/16pt sans-serif;margin:0;width:150pt'><span style='initial-letter:3;width:90pt;text-align:#{alignment}'>A</span>lphabet beta gamma delta.</p>"

        initial = layout(html).boxes |> Enum.find(&(&1.type == :text and &1.text == "A"))
        {alignment, initial}
      end

    initial = positions["left"]

    advance =
      NativeElixirPdfUtilities.HtmlToPdf.Font.css_text_width(
        "A",
        initial.font_face,
        initial.font_size
      )

    assert_in_delta positions["center"].x - initial.x, (90 - advance) / 2, 0.00001
    assert_in_delta positions["right"].x - initial.x, 90 - advance, 0.00001
  end

  test "a size below its sink uses over alignment and grid wrapping rounds the exclusion" do
    # CSS Inline 3, 2024-12-18, section 7.6.1 chooses over alignment for
    # size < sink: https://www.w3.org/TR/2024/WD-css-inline-3-20241218/#initial-letter-block-position
    html =
      "<style>p{font:12pt/16pt sans-serif;margin:0;width:150pt}p::first-letter{initial-letter:2 3}</style><p>Alphabet beta gamma delta epsilon zeta.</p>"

    [initial, ordinary | _] = layout(html).boxes |> Enum.filter(&(&1.type == :text))
    assert_in_delta initial.y, ordinary.y - 16, 0.75 / 64

    starts =
      for wrap <- ["none", "grid"], into: %{} do
        html =
          "<style>p{font:12pt/16pt sans-serif;margin:0;width:150pt}p::first-letter{initial-letter:3;initial-letter-wrap:#{wrap}}</style><p>Alphabet beta gamma delta epsilon zeta eta theta.</p>"

        [first | _] =
          layout(html).boxes |> Enum.filter(&(&1.type == :text and &1.font_size == 12.0))

        {wrap, first.x}
      end

    # DejaVu Sans has no water ideograph; the CSS ic fallback is one em.
    assert_in_delta starts["grid"] / 12, round(starts["grid"] / 12), 0.00001
    assert starts["grid"] >= starts["none"] and starts["grid"] < starts["none"] + 12
  end

  test "inherited RTL exclusions reserve the distance from the actual right line edge" do
    alias NativeElixirPdfUtilities.HtmlToPdf.InitialLetter

    style = %{
      direction: :rtl,
      line_height: 10,
      _initial_exclusion_region: %{bottom: -35, left: 80, right: 110}
    }

    run = %{text: "אב", style: style, source_owner: :paragraph}
    [marker, retained] = InitialLetter.inherit_exclusion([run], style, 10, 0, 100)
    assert marker._inline_padding_width == 30
    assert marker._initial_exclusion == %{remaining: 3, widths: [30, 30, 30]}
    assert retained == run
  end

  test "explicit initials only apply at the start of an inline formatting context" do
    html =
      "<div style='font:12pt serif;line-height:16pt'><span style='initial-letter:2'>A</span>lpha <span style='initial-letter:3'>B</span>eta</div>"

    boxes = layout(html).boxes |> Enum.filter(&(&1.type == :text))
    assert hd(boxes).font_size > 24
    assert Enum.find(boxes, &String.contains?(&1.text, "B")).font_size == 12.0
  end

  test "contours shorten each affected line and explicit adjustments only change the first" do
    body = String.duplicate("Alphabet beta gamma delta epsilon zeta eta theta. ", 5)

    html = fn wrap ->
      "<style>p{font:12pt sans-serif;line-height:16pt;margin:0;width:150pt}p::first-letter{initial-letter:3;initial-letter-wrap:#{wrap}}</style><p>#{body}</p>"
    end

    ordinary_starts = fn wrap ->
      layout(html.(wrap)).boxes
      |> Enum.filter(&(&1.type == :text and &1.font_size == 12.0))
      |> Enum.group_by(& &1.y)
      |> Enum.sort_by(fn {y, _} -> -y end)
      |> Enum.map(fn {_, boxes} -> Enum.min(Enum.map(boxes, & &1.x)) end)
    end

    none = ordinary_starts.("none")
    all = ordinary_starts.("all")
    first = ordinary_starts.("first")
    adjusted = ordinary_starts.("-25%")
    assert hd(all) < hd(none)
    assert hd(first) == hd(all)
    assert Enum.at(first, 1) == Enum.at(none, 1)
    assert hd(adjusted) < hd(none)
    assert Enum.at(adjusted, 1) == Enum.at(none, 1)
  end

  test "selection retains subsequent forced breaks and zero line-height initials remain bounded" do
    html =
      "<style>p{font:12pt sans-serif;margin:0}p::first-letter{color:red}</style><p>A<br>B</p>"

    boxes = layout(html).boxes |> Enum.filter(&(&1.type == :text))
    assert Enum.map(boxes, & &1.text) == ["A", "B"]
    assert hd(boxes).y > List.last(boxes).y

    assert {:ok, _} =
             HtmlToPdf.render(
               "<div style='font-size:0;line-height:0'><span style='initial-letter:3'>A</span></div>",
               system_font_discovery: false
             )
  end

  test "language defaults precede author overrides and forged pseudo styles return diagnostics" do
    html =
      "<p lang='ja'>A</p><p lang='hi'>A</p><p lang='ja-Latn'>A</p><p lang='ja' style='initial-letter-align:leading'>A</p>"

    assert {:ok, dom} = HtmlParser.parse(html)

    assert {:ok, %{children: nodes} = styled} =
             Style.compute_detailed(dom, system_font_discovery: false)

    assert Enum.map(nodes, & &1.style.initial_letter_align) == [
             {:ideographic, false},
             {:hanging, false},
             {:alphabetic, false},
             {:leading, false}
           ]

    [first | rest] = nodes

    first = %{
      first
      | style: Map.put(first.style, :_first_letter_style, %{font_size: :invalid, line_height: 12})
    }

    assert {:error, {:invalid_layout, diagnostic}} =
             Layout.layout(%{styled | children: [first | rest]}, margin: 0)

    assert diagnostic.stage == :layout and diagnostic.source == "p"
    assert diagnostic.message =~ "initial-letter"

    first = %{
      first
      | style:
          Map.delete(first.style, :_first_letter_style)
          |> Map.put(:_initial_exclusion_region, %{bottom: 0, right: :invalid})
    }

    assert {:error, {:invalid_layout, _}} =
             Layout.layout(%{styled | children: [first | rest]}, margin: 0)
  end

  test "initials keep affected lines together at page boundaries and retain paragraph extraction" do
    text = String.duplicate("Alphabet beta gamma delta epsilon. ", 5) |> String.trim()

    html =
      "<style>p{font:12pt/16pt sans-serif;margin:0;width:150pt}p::first-letter{initial-letter:3}</style><div style='height:40pt'>Before</div><p>#{text}</p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, system_font_discovery: false, page_size: {180, 80}, margin: 0)

    assert {:ok, document} = Text.extract_spans(pdf, actual_text: true)
    [first, second | _] = document.pages
    assert Enum.map_join(first.spans, & &1.text) == "Before"
    assert Enum.map_join(second.spans, & &1.text) =~ "Alphabet"
    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    assert String.replace(extracted, ~r/\s+/u, " ") == "Before " <> text
  end

  test "short paragraphs carry their exclusion into following blocks and clear independent contexts" do
    css =
      "<style>p{font:12pt/16pt sans-serif;margin:0;width:150pt}.initial::first-letter{initial-letter:3}</style>"

    body =
      "<p class='initial'>A</p><p>beta gamma delta epsilon zeta eta theta iota kappa lambda mu.</p>"

    for wrapper <- [false, true] do
      html = css <> if(wrapper, do: "<section>#{body}</section>", else: body)
      boxes = layout(html).boxes |> Enum.filter(&(&1.type == :text and &1.font_size == 12.0))
      lines = boxes |> Enum.group_by(& &1.y) |> Enum.sort_by(fn {y, _} -> -y end)
      [{_, first}, {_, second}, {_, third} | _] = lines
      assert hd(first).x > 0
      assert hd(second).x > 0
      assert hd(third).x == 0.0
    end

    boxes =
      layout(
        css <>
          "<p class='initial'>A</p><div style='display:flow-root;font:12pt/16pt sans-serif'>Below</div>"
      ).boxes

    initial = Enum.find(boxes, &(&1.type == :text and &1.text == "A"))
    below = Enum.find(boxes, &(&1.type == :text and &1.text == "Below"))
    assert below.x == 0.0
    assert below.y < initial.y
  end

  test "an ancestor first-letter selector reaches its first formatted block and keeps the original owner" do
    html =
      "<style>section::first-letter{color:red;initial-letter:2}p{font:12pt/16pt sans-serif;margin:0}</style><section> <div><p>Alphabet</p><p>Beta</p></div></section>"

    boxes = layout(html).boxes |> Enum.filter(&(&1.type == :text))

    assert hd(boxes).text == "A" and hd(boxes).font_size > 24,
           inspect(Enum.map(boxes, &Map.take(&1, [:text, :font_size, :color])))

    assert hd(boxes).color == {1.0, 0.0, 0.0}
    assert Enum.find(boxes, &(&1.text == "Beta")).font_size == 12.0
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "Alphabet Beta"} = Text.extract(pdf, layout: false)
  end

  defp layout(html) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, page_size: {240, 400}, margin: 0)
    layout
  end
end
