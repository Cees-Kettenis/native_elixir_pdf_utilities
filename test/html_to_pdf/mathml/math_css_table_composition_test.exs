defmodule NativeElixirPdfUtilities.HtmlToPdf.MathCssTableCompositionTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, MathLayoutValidator}

  defp measured(html) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, system_font_discovery: false) do
        {:ok, node.style._math_box}
      end
    end)
  end

  test "fixed tables allocate the definite width independently of later row contents" do
    for layout <- ["table-layout:fixed", "table-layout:fixed;border-collapse:collapse"] do
      html =
        "<math><mtable style='#{layout};width:120pt;border-spacing:0'><mtr><mtd style='width:30pt;background:yellow'><mn>1</mn></mtd><mtd><mn>2</mn></mtd></mtr><mtr><mtd><mn>123456789</mn></mtd><mtd><mfrac><mn>3</mn><mn>4</mn></mfrac></mtd></mtr></mtable></math>"

      assert {:ok, box} = measured(html)
      assert_in_delta box.width, 120, 0.012
      assert box.actual_text == "1212345678934"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "1212345678934"} = Text.extract(pdf, layout: false)
    end
  end

  test "anonymous rows and cells retain real children, CSS display roles and logical text once" do
    for content <- [
          "<mn>1</mn><mn>2</mn><mtr><mn>3</mn><mtd columnspan='2'><mn>4</mn></mtd></mtr>",
          "<mtd><mn>1</mn><mn>2</mn></mtd><mtr><mtd><mn>3</mn><mn>4</mn></mtd></mtr>",
          "<mrow style='display:table-row'><mn style='display:table-cell'>12</mn><mtd><mn>34</mn></mtd></mrow>",
          "<mtr><mtd style='display:inline math'><mn>12</mn></mtd><mtd><mn>34</mn></mtd></mtr>"
        ] do
      html = "<math><mtable>#{content}<mtr></mtr></mtable></math>"
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "1234"} = Text.extract(pdf, layout: false)
    end
  end

  test "table CSS displays retain their actual source owner outside the Math table algorithm" do
    for tag <- ~w(mtable mtr mtd), display <- ~w(inline-block flex grid) do
      html =
        "<math><#{tag} style='display:#{display};width:60pt;min-height:30pt;padding:2pt;background:yellow'><mn>12</mn></#{tag}></math>"

      assert {:ok, box} = measured(html)
      assert_in_delta box.width, 64, 0.012
      assert box.ascent + box.descent >= 34
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end
  end

  test "non-root destinations use the selected child on its final PDF page" do
    for {tag, content} <- [
          {"mfrac", "<mn>1</mn><mn>2</mn>"},
          {"msup", "<mn>1</mn><mn>2</mn>"},
          {"mpadded", "<mn>12</mn>"},
          {"mtable", "<mtr><mtd><mn>12</mn></mtd></mtr>"},
          {"mtr", "<mtd><mn>12</mn></mtd>"},
          {"mtd", "<mn>12</mn>"}
        ] do
      target = "<#{tag} id='destination'>#{content}</#{tag}>"

      equation =
        case tag do
          "mtr" -> "<mtable>#{target}</mtable>"
          "mtd" -> "<mtable><mtr>#{target}</mtr></mtable>"
          _ -> target
        end

      html =
        "<style>html,body,p{margin:0}</style><p><a href='#destination'>Go</a></p><div style='break-before:page;padding:6pt'><math><mn>0</mn>#{equation}<mn>3</mn></math></div>"

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 page_size: {200, 160},
                 margin: 10,
                 system_font_discovery: false
               )

      assert {:ok, document} = Reader.read_validated(pdf)
      assert length(document.pages) == 2

      links =
        for {_, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.document.objects,
            do: link

      assert [link] = links
      assert [{:ref, page}, {:name, "XYZ"}, x, y, nil] = link["Dest"]
      assert page == Enum.at(document.pages, 1).ref
      assert x > 16 and x < 120
      assert y > 20 and y <= 150, "#{tag}: destination #{x}, #{y}"
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text =~ "Go" and text =~ "0123"
    end
  end

  test "logical first-ID selection keeps hidden Math targets from binding to later duplicates" do
    html =
      "<a href='#same'>Go</a><math><mfrac id='same' style='display:none'><mn>1</mn><mn>2</mn></mfrac><msup id='same'><mn>3</mn><mn>4</mn></msup></math>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, document} = Reader.read_validated(pdf)

    refute Enum.any?(document.document.objects, fn {_, value} ->
             match?(%{value: %{"Subtype" => {:name, "Link"}}}, value)
           end)

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Go" and text =~ "34"
    refute text =~ "12"
  end

  test "invalid child target identities retain the shared actionable diagnostics" do
    assert {:error, {:invalid_layout, diagnostic}} =
             MathLayoutValidator.validate_link_style(
               %{html_target: {:invented, :scope, :node}},
               "mfrac"
             )

    assert diagnostic.stage == :layout
    assert diagnostic.operation == :measure_math
    assert diagnostic.source == "mfrac"
    assert diagnostic.message =~ "validated"
  end

  test "out-of-flow matrix cells keep mathematical paint outside column allocation" do
    for position <- ~w(absolute fixed) do
      html = """
      <style>html,body{margin:0}math{font-family:math;font-size:16pt}</style>
      <math><mtable><mtr>
      <mtd style='position:#{position};left:30pt;top:40pt'><mfrac><mn>1</mn><mn>2</mn></mfrac></mtd>
      <mtd><mn>34</mn></mtd></mtr></mtable></math>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text == "3412"
      assert {:ok, document} = Text.extract_spans(pdf)
      spans = Enum.flat_map(document.pages, & &1.spans)

      for digit <- ~w(1 2) do
        span = Enum.find(spans, &(&1.text == digit))
        assert span.x >= 30 and span.x < 50
        assert span.y > 40 and span.y < 80
      end

      assert pdf =~ "/ActualText"
    end
  end

  test "empty fixed and collapsed matrices preserve a following equation" do
    for css <- ["table-layout:fixed;width:120pt", "border-collapse:collapse"] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<math><mtable style='#{css}'><mtr></mtr></mtable><mn>7</mn></math>",
                 system_font_discovery: false
               )

      assert {:ok, "7"} = Text.extract(pdf, layout: false)
    end
  end

  test "positioned child destinations refer to the painted owner coordinates" do
    for position <- ~w(absolute fixed) do
      html =
        "<style>html,body{margin:0}</style><a href='#t'>Go</a><math><msup id='t' style='position:#{position};left:40pt;top:40pt'><mn>1</mn><mn>2</mn></msup></math>"

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 page_size: {200, 160},
                 margin: 0,
                 system_font_discovery: false
               )

      assert {:ok, document} = Reader.read_validated(pdf)

      links =
        for {_, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.document.objects,
            do: link

      assert [link] = links
      assert [{:ref, page}, {:name, "XYZ"}, x, y, nil] = link["Dest"]
      assert page == hd(document.pages).ref
      assert_in_delta x, 40, 0.012
      assert_in_delta y, 120, 0.012
      assert {:ok, "Go 12"} = Text.extract(pdf, layout: false)
    end
  end

  test "positioned cells resolve percentages against the actual matrix padding box" do
    html =
      "<style>html,body{margin:0}math{font-family:math;font-size:18pt}</style><math><mtable style='position:relative;width:120pt;height:70pt;border:1pt solid blue'><mtr style='position:relative'><mtd style='position:absolute;left:50%;top:25%;padding:3pt;background:yellow'><msqrt><mn>4</mn></msqrt></mtd><mtd><mn>5</mn></mtd></mtr></mtable></math>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, "54"} = Text.extract(pdf, layout: false)
    assert {:ok, document} = Text.extract_spans(pdf)
    spans = Enum.flat_map(document.pages, & &1.spans)
    four = Enum.find(spans, &(&1.text == "4"))
    assert four.x > 70 and four.x < 90
    assert four.y > 20 and four.y < 70
  end

  test "relative table and row owners move paint without moving their flow measurement" do
    for layout <- ["", "table-layout:fixed;width:80pt;"], owner <- ~w(mtable mtr) do
      spans =
        for {left, top} <- [{0, 0}, {7, 5}] do
          table_css =
            layout <>
              if(owner == "mtable", do: "position:relative;left:#{left}pt;top:#{top}pt", else: "")

          row_css =
            if(owner == "mtr", do: "position:relative;left:#{left}pt;top:#{top}pt", else: "")

          html =
            "<style>html,body{margin:0}body{padding:10pt}</style><math><mtable #{if(owner == "mtable", do: "id=target", else: "")} style='#{table_css}'><mtr #{if(owner == "mtr", do: "id=target", else: "")} style='#{row_css}'><mtd><mn>1</mn></mtd></mtr></mtable></math><a href='#target'>Go</a>"

          assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
          assert {:ok, document} = Text.extract_spans(pdf)
          span = Enum.find(hd(document.pages).spans, &(&1.text == "1"))
          assert {:ok, context} = Reader.read_validated(pdf)

          links =
            for {_, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- context.document.objects,
                do: link

          assert [link] = links
          assert [{:ref, _}, {:name, "XYZ"}, x, y, nil] = link["Dest"]
          {span, x, y}
        end

      [{normal, normal_x, normal_y}, {shifted, shifted_x, shifted_y}] = spans
      assert_in_delta shifted.x - normal.x, 7, 0.0001
      assert_in_delta shifted.y - normal.y, 5, 0.0001
      assert_in_delta shifted_x - normal_x, 7, 0.0001
      assert_in_delta shifted_y - normal_y, -5, 0.0001
    end
  end

  test "fixed clipped cells constrain actual URI annotation geometry" do
    html =
      "<style>html,body{margin:0}</style><math><mtable style='table-layout:fixed;width:30pt;border-spacing:0'><mtr><mtd style='padding:0;overflow:hidden'><mtext><a href='https://matrix.test/clip'>123456789</a></mtext></mtd></mtr></mtable></math>"

    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert {:ok, document} = Reader.read_validated(pdf)

    links =
      for {_, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.document.objects,
          do: link

    assert [link] = links
    assert link["A"]["URI"] == {:string, "https://matrix.test/clip"}
    assert [left, bottom, right, top] = link["Rect"]
    assert left >= 0 and right <= 30.001 and bottom >= 0 and top > bottom
    assert_in_delta right - left, 30, 0.001
    assert {:ok, "123456789"} = Text.extract(pdf, layout: false)
  end
end
