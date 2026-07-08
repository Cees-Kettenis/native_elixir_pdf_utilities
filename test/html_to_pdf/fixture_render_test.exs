defmodule NativeElixirPdfUtilities.HtmlToPdf.FixtureRenderTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  @fixtures_dir Path.expand("../fixtures/html_to_pdf", __DIR__)
  @long_trim_description "SP27-ACG-#3 CE REVERSE COIL ZIPPER-AUTOLOCKING WITH PULLER FOR ATTACHMENT-DABCR7-MATTE ENAMEL FINISH; ZIPPER; CLOSED END; APPROVED; TRIM-COIL ZIPPERS; ADDTL RISKS; PASSES METAL DETECTION; TRUE; VENDOR #: CFC-36 DABCR7 EFJ BA12 GREEN-F KENSIN N-ANTI P-TB REVERSE; CARE: WASH IN NET, WASH INSIDE OUT, TUMBLE DRY INSIDE OUT; PRIMARY SM: YES; 49% ZINC, 42% POLYESTER, 9% POLYESTER (MECHANICALLY RECYCLED); # OF COLORS: 1; APPLICATION TECHNIQUE: SEW ON SLIDER/PULL; PULL CODE: DABCR7; SLIDER QTY: 1; LOCKING: AUTO-LOCK TAPE; KNIT; W (MM): 12.00 TEETH; COIL; STANDARD; SIZE: 3; REVERSE; FINISH: COATING: ENAMELED STOP; PLASTIC TOP STOP, PLASTIC BOTTOM STOP; COLOR EFFECT: 1ST COLOR; NIKE SOLID COLOR; 2ND COLOR; NIKE SOLID COLOR; 3RD COLOR; NIKE SOLID COLOR; 4TH COLOR; NIKE SOLID COLOR; 5TH COLOR; 91B"

  test "renders scrubbed purchase order fixture" do
    html = fixture_html("purchase_order.html")

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: :a4)
    assert_valid_pdf(pdf)
    assert pdf =~ "/Count 1"
    assert pdf =~ "/Subtype /Image"

    assert {:ok, layout_tree} = layout_fixture(html, page_size: :a4)

    assert_layout_text(layout_tree, "PURCHASE ORDER")
    assert_layout_text(layout_tree, "PO-X-0726-00421")
    assert_layout_text(layout_tree, "Supplier Address")
    assert_layout_text(layout_tree, "GGPHJ5376SU27")
    assert_layout_text(layout_tree, "Total: 40Y - POLAR")
  end

  test "renders scrubbed stock sticker fixture at custom label size" do
    html = fixture_html("stock_sticker.html")

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {4.92126, 1.49606}, margin: 0)
    assert_valid_pdf(pdf)
    assert pdf =~ "/MediaBox [0 0 354.3307 107.7163]"
    assert pdf =~ "/Subtype /Image"
    refute pdf =~ "#RIGHTITEMQRCODE#"

    assert {:ok, layout_tree} = layout_fixture(html, page_size: {4.92126, 1.49606}, margin: 0)
    [image] = Enum.filter(layout_tree.boxes, &(&1.type == :image))

    assert_in_delta image.width, 53.149608, 0.0001
    assert_in_delta image.height, 53.149608, 0.0001
    assert image.y >= 0.0

    assert_layout_text(layout_tree, "Product Item: 001764")
    assert_layout_text(layout_tree, "Transaction Date: 08/07/2026 07:25")
    assert_layout_text(layout_tree, "PO Number: PO-X-0726-00421")
  end

  test "renders scrubbed material requisition fixture with page break" do
    html = fixture_html("material_requisition.html")

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: :a4)
    assert_valid_pdf(pdf)
    assert pdf_page_count(pdf) >= 2
    assert pdf =~ "/Subtype /Image"

    assert {:ok, layout_tree} = layout_fixture(html, page_size: :a4)

    assert_layout_text(layout_tree, "MATERIAL")
    assert_layout_text(layout_tree, "REQUISITION")
    assert_layout_text(layout_tree, "MR-A-0726-0001")
    assert_layout_text(layout_tree, "AGS-NB0100")
    assert_layout_text(layout_tree, "Requested By:")
    assert_layout_text(layout_tree, "Issued By:")
    assert_layout_text(layout_tree, "Received By:")
  end

  test "renders scrubbed trim card fixture with separated wrapped header rows" do
    html = fixture_html("trim_card.html")

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {841.89, 595.28}, margin: 0)
    assert_valid_pdf(pdf)
    assert pdf =~ "/MediaBox [0 0 841.89 595.28]"

    assert {:ok, layout_tree} = layout_fixture(html, page_size: {841.89, 595.28}, margin: 0)

    assert_layout_text(layout_tree, "Master Trimcard")
    assert_layout_text(layout_tree, "GGPIX7736FA27")
    assert_layout_text(layout_tree, "ORDER QTY")
    assert_layout_text(layout_tree, "SEASON")
    assert_layout_text(layout_tree, "TRIM (1066261)")

    texts = Enum.filter(layout_tree.boxes, &(&1.type == :text))
    style_tail = Enum.find(texts, &String.contains?(&1.text, "PANT"))
    order_qty = Enum.find(texts, &(&1.text == "ORDER QTY"))
    season = Enum.find(texts, &(&1.text == "SEASON"))

    assert style_tail
    assert order_qty
    assert season
    assert order_qty.y < style_tail.y - order_qty.font_size
    assert season.y < style_tail.y - season.font_size

    card_box =
      layout_tree.boxes
      |> Enum.filter(&(&1.type == :rect))
      |> Enum.find(&(&1.width > 180.0 and &1.width < 220.0 and &1.height > 300.0))

    assert card_box
  end

  test "renders long trim card descriptions without bleeding into metrics" do
    html =
      "trim_card.html"
      |> fixture_html()
      |> String.replace(
        "Elastic waistband tape for scrubbed trim-card fixture.",
        @long_trim_description
      )

    assert {:ok, layout_tree} = layout_fixture(html, page_size: {841.89, 595.28}, margin: 0)

    texts = Enum.filter(layout_tree.boxes, &(&1.type == :text))
    lines = Enum.map(texts, & &1.text)
    last_description_line = Enum.find(texts, &String.contains?(&1.text, "COLOR; 91B"))
    metric_label = Enum.find(texts, &(&1.text == "M. USED"))

    assert "ZIPPER-AUTOLOCKING WITH PULLER FOR " in lines
    assert last_description_line
    assert metric_label
    assert metric_label.y < last_description_line.y - last_description_line.line_height

    head_box =
      layout_tree.boxes
      |> Enum.filter(&(&1.type == :rect))
      |> Enum.find(&(&1.x > 8.0 and &1.x < 10.0 and &1.width > 190.0 and &1.height > 250.0))

    assert head_box
    assert head_box.y < last_description_line.y

    header_border_box =
      layout_tree.boxes
      |> Enum.filter(&(&1.type == :rect))
      |> Enum.find(&(&1.border_widths == %{top: 3.0, right: 0.0, bottom: 0.75, left: 0.0}))

    assert header_border_box

    assert header_border_box.border_colors.top ==
             {0.13333333333333333, 0.20392156862745098, 0.2901960784313726}

    assert header_border_box.border_colors.bottom ==
             {0.8431372549019608, 0.8745098039215686, 0.9137254901960784}
  end

  defp fixture_html(name) do
    @fixtures_dir
    |> Path.join(name)
    |> File.read!()
  end

  defp layout_fixture(html, opts) do
    with {:ok, dom} <- HtmlParser.parse(html),
         {:ok, styled_tree} <- Style.compute(dom, opts),
         {:ok, layout_tree} <- Layout.layout(styled_tree, opts) do
      {:ok, layout_tree}
    end
  end

  defp assert_valid_pdf(pdf) do
    assert String.starts_with?(pdf, "%PDF-1.4")
    assert pdf =~ "/Type /Catalog"
    assert pdf =~ "/Type /Page"
    assert pdf =~ "xref"
    assert pdf =~ "trailer"
    assert String.ends_with?(pdf, "%%EOF\n")
  end

  defp pdf_page_count(pdf) do
    case Regex.run(~r/\/Count\s+(\d+)/, pdf) do
      [_match, count] -> String.to_integer(count)
      _ -> flunk("PDF page count not found")
    end
  end

  defp assert_layout_text(layout_tree, expected_text) do
    text =
      layout_tree.boxes
      |> Enum.filter(&(&1.type == :text))
      |> Enum.map_join(" ", & &1.text)

    assert text =~ expected_text
  end
end
