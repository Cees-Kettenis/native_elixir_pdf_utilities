defmodule NativeElixirPdfUtilities.HtmlToPdf.TextDecorationTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.CssTextDecorationValidator

  test "double line grammar and paint retain originating color across styled descendants" do
    assert {:ok, %{text_decoration_style: :double}} =
             HtmlValidator.parse_css_text_decoration(
               "text-decoration-style",
               "double",
               12,
               {0, 0, 0}
             )

    for line <- ["underline", "overline", "line-through"] do
      html =
        "<p style='font:18pt/28pt sans-serif;margin:0;text-decoration:#{line} double 1.5pt blue'>ABC<span style='color:red'>DEF</span></p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, margin: 0)
      lines = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_decoration))
      assert length(lines) == 4
      assert Enum.all?(lines, &(&1.fill_color == {0, 0, 1} and &1.height == 1.5))

      for [first, second] <- Enum.chunk_every(lines, 2) do
        assert first.x == second.x and first.width == second.width
        assert_in_delta abs(first.y - second.y), 2.25, 0.0001
      end

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "ABCDEF"} = Text.extract(pdf, layout: false)
    end
  end

  test "underline placement preserves each canonical side and rejects unrelated properties" do
    assert CssTextDecorationValidator.properties() ==
             ~w(text-underline-offset text-underline-position text-decoration-skip-ink)

    for {value, expected} <- [
          {"auto", [:auto]},
          {"from-font", [:from_font]},
          {"left", [:left]},
          {"right", [:right]},
          {"under left", [:left, :under]}
        ] do
      assert {:ok, %{text_underline_position: ^expected} = style} =
               CssTextDecorationValidator.prepare(%{}, "text-underline-position", value)

      assert :ok = CssTextDecorationValidator.validate_style(style, "p")
    end

    assert :error = CssTextDecorationValidator.prepare(%{}, "unrelated", "auto")
  end

  test "underline controls admit signed offsets and canonical placement keywords" do
    assert {:ok, %{text_underline_offset: :auto}} =
             CssTextDecorationValidator.prepare(%{}, "text-underline-offset", "auto")

    assert {:ok, %{text_underline_offset: length}} =
             CssTextDecorationValidator.prepare(%{}, "text-underline-offset", "25%")

    assert {:ok, 3.0} =
             NativeElixirPdfUtilities.Validators.CssLengthValidator.resolve(length, 12.0, %{})

    assert {:ok, %{text_underline_position: [:right, :under]}} =
             CssTextDecorationValidator.prepare(%{}, "text-underline-position", "under right")

    for value <- ["auto under", "left right", "under under", "from-font left", "unknown"] do
      assert :error = CssTextDecorationValidator.prepare(%{}, "text-underline-position", value)
    end

    assert :ok = CssTextDecorationValidator.validate_style(%{}, "p")

    assert {:error, {:invalid_layout, diagnostic}} =
             CssTextDecorationValidator.validate_style(%{text_underline_offset: :invalid}, "p")

    assert diagnostic.stage == :layout and diagnostic.source == "p"
  end

  test "underline offsets belong to each decorating origin across styled descendants" do
    html =
      "<p style='font:12pt/16pt sans-serif;margin:0;text-decoration:underline 1.5pt blue;text-underline-offset:4pt'>ABC<span style='text-underline-offset:8pt;text-decoration:underline 1.5pt red'>DEF</span></p>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, margin: 0)
    lines = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_decoration))
    assert length(lines) == 3

    assert Enum.filter(lines, &(&1.fill_color == {0, 0, 1}))
           |> Enum.all?(&(&1.text_baseline_offset == -5.5))

    assert Enum.find(lines, &(&1.fill_color == {1, 0, 0})).text_baseline_offset == -9.5
  end

  test "from-font positions and physical vertical sides share the same underline geometry" do
    metrics = %{underline: %{position: -100, thickness: 50}}
    style = %{font_face: %{descent: -200}, font_size: 12, text_underline_position: [:from_font]}
    assert {:ok, -1.2} = CssTextDecorationValidator.underline(style, metrics, 0.012, 1.5)

    assert {:ok, -3.15} =
             CssTextDecorationValidator.underline(
               %{style | text_underline_position: [:under]},
               metrics,
               0.012,
               1.5
             )

    vertical = put_in(style.font_face, %{descent: -200, _vertical_text: %{mode: :vertical_rl}})

    assert {:ok, 2.25} =
             CssTextDecorationValidator.underline(
               %{vertical | text_underline_position: [:right]},
               metrics,
               0.012,
               1.5
             )

    assert {:ok, -0.75} =
             CssTextDecorationValidator.underline(
               %{vertical | text_underline_position: [:left]},
               metrics,
               0.012,
               1.5
             )
  end

  test "wavy lines are clipped vector strokes and retain phase across color fragments" do
    html =
      "<p style='font:18pt/28pt sans-serif;margin:0;text-decoration:underline wavy 0.75pt blue'>ABC<span style='color:red'>DEF</span></p>"

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
    assert {:ok, layout} = Layout.layout(styled, margin: 0)
    [first, second] = Enum.filter(layout.boxes, &(Map.get(&1, :shape) == :wave))
    assert first.height == 3.75 and first.stroke_width == 0.75
    assert first.wave_offset == 0.0
    assert_in_delta second.wave_offset, first.x - second.x, 0.0001

    assert {:ok, pdf} =
             HtmlToPdf.render(html, system_font_discovery: false, compress_streams: false)

    assert pdf =~ " c "
    refute pdf =~ "/Subtype /Image"
    assert {:ok, "ABCDEF"} = Text.extract(pdf, layout: false)
  end

  test "wavy overlines and strikes retain their own phase alongside a solid ancestor" do
    for line <- ["overline", "line-through"] do
      html =
        "<p style='font:18pt/28pt sans-serif;margin:0;text-decoration:underline solid blue'><span style='text-decoration:#{line} wavy 0.75pt red'>ABC</span></p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, margin: 0)
      [wave] = Enum.filter(layout.boxes, &(Map.get(&1, :shape) == :wave))
      [text] = Enum.filter(layout.boxes, &(&1.type == :text))
      assert wave.stroke_color == {1, 0, 0} and wave.stroke_width == 0.75
      assert wave.wave_offset == 0 and wave.y > text.y

      assert Enum.any?(
               layout.boxes,
               &(Map.get(&1, :role) == :text_decoration and &1.fill_color == {0, 0, 1})
             )

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "ABC"} = Text.extract(pdf, layout: false)
    end
  end

  test "skip ink interrupts descenders while none and strike remain continuous" do
    for {mode, interrupted?} <- [{"auto", true}, {"all", true}, {"none", false}] do
      html =
        "<p style='font:24pt/32pt sans-serif;margin:0;text-decoration:underline solid 0.75pt blue;text-decoration-skip-ink:#{mode}'>gyp</p>"

      assert {:ok, dom} = HtmlParser.parse(html)
      assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)
      assert {:ok, layout} = Layout.layout(styled, margin: 0)
      lines = Enum.filter(layout.boxes, &(Map.get(&1, :role) == :text_decoration))
      assert length(lines) > 1 == interrupted?
      assert Enum.all?(lines, &(&1.width > 0))
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "gyp"} = Text.extract(pdf, layout: false)
    end

    assert :error = CssTextDecorationValidator.prepare(%{}, "text-decoration-skip-ink", "objects")

    assert {:error, {:invalid_layout, diagnostic}} =
             CssTextDecorationValidator.validate_style(
               %{text_decoration_skip_ink: :objects},
               "span"
             )

    assert diagnostic.source == "span"
  end
end
