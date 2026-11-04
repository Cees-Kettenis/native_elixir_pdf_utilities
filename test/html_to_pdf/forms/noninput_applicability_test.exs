defmodule NativeElixirPdfUtilities.HtmlToPdf.NoninputApplicabilityTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Forms}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, PdfWriter}
  alias NativeElixirPdfUtilities.Validators.HtmlFormValidator

  test "inactive boolean metadata preserves complete static and interactive output" do
    cases = [
      {"<p checked disabled readonly multiple required selected alpha>Visible</p>",
       "<p>Visible</p>"},
      {"<button checked readonly multiple required selected alpha>Label</button>",
       "<button>Label</button>"},
      {"<textarea name=field checked multiple selected alpha>Value</textarea>",
       "<textarea name=field>Value</textarea>"},
      {"<select name=field checked readonly selected alpha><option>A</option><option>B</option></select>",
       "<select name=field><option>A</option><option>B</option></select>"},
      {"<fieldset checked readonly multiple required selected alpha><legend disabled>Caption</legend><input name=field value=Value></fieldset>",
       "<fieldset><legend>Caption</legend><input name=field value=Value></fieldset>"}
    ]

    for {html, reference} <- cases, mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode)
      assert {:ok, equivalent} = HtmlToPdf.render(reference, forms: mode)
      assert pdf == equivalent
    end
  end

  test "raw boolean selectors remain visible while effective states preserve applicability" do
    html =
      "<style>p[disabled]:not(:disabled){color:blue}select[readonly]:enabled{color:blue}</style><p disabled>Ordinary</p><select name=choice readonly checked multiple><option selected>A</option><option>B</option></select><textarea name=text readonly multiple>Value</textarea>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [_style, paragraph, select, textarea] = HtmlFormValidator.prepare_initial_nodes(dom.children)
    assert paragraph.attributes == %{"disabled" => ""}
    assert select.attributes["readonly"] == ""
    assert select.attributes["checked"] == ""

    assert HtmlFormValidator.initial_control_attributes(select) == %{
             "name" => "choice",
             "multiple" => ""
           }

    assert HtmlFormValidator.initial_control_attributes(textarea) == %{
             "name" => "text",
             "readonly" => ""
           }

    assert {:ok, %{children: [paragraph, select, _textarea]}} = Style.compute_detailed(dom)
    assert paragraph.style.color == {0, 0, 1}
    assert select.style.color == {0, 0, 1}
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, fields} = Forms.fields(pdf)
    assert Enum.find(fields, &(&1.name == "text")).read_only
    refute Enum.find(fields, &(&1.name == "choice")).read_only
  end

  test "explicit resize suppression uses the static editing profile" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<textarea style='resize:NoNe'>Value</textarea>")

    assert {:ok, %{children: [control]}} = Style.compute_detailed(dom)
    assert control.style.resize == :none
    assert {:ok, pdf} = HtmlToPdf.render("<textarea style='resize:none'>Value</textarea>")
    assert {:ok, reference} = HtmlToPdf.render("<textarea>Value</textarea>")
    refute pdf == reference

    assert {:ok, none_layout} =
             Layout.layout(%{type: :document, children: [control]},
               page_size: {200, 100},
               margin: 0
             )

    assert {:ok, reference_dom} = HtmlParser.parse_detailed("<textarea>Value</textarea>")
    assert {:ok, reference_tree} = Style.compute_detailed(reference_dom)

    assert {:ok, reference_layout} =
             Layout.layout(reference_tree, page_size: {200, 100}, margin: 0)

    none_strokes = Enum.filter(none_layout.boxes, &(Map.get(&1, :role) == :textarea_resizer))

    default_strokes =
      Enum.filter(reference_layout.boxes, &(Map.get(&1, :role) == :textarea_resizer))

    assert none_strokes == []
    assert length(default_strokes) == 4

    assert Enum.all?(
             default_strokes,
             &(&1.shape == {:diagonal, :up} and elem(&1.stroke_color, 3) == 0.45)
           )

    assert {:ok, stroke_pdf} =
             PdfWriter.render([%{size: {200, 100}, boxes: default_strokes}],
               forms: :static,
               compress_streams: false
             )

    assert stroke_pdf =~ "0 w"
    assert {:ok, stroke_document} = NativeElixirPdfUtilities.Pdf.Reader.read(stroke_pdf)

    assert Enum.any?(stroke_document.objects, fn {_id, object} ->
             is_map(object.value) and Map.get(object.value, "CA") == 0.45
           end)

    assert {:ok, none_pdf} =
             PdfWriter.render([%{size: {200, 100}, boxes: none_strokes}],
               forms: :static,
               compress_streams: false
             )

    assert {:ok, none_document} = NativeElixirPdfUtilities.Pdf.Reader.read(none_pdf)

    refute Enum.any?(none_document.objects, fn {_id, object} ->
             is_map(object.value) and Map.get(object.value, "CA") == 0.45
           end)

    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render("<textarea style='resize:horizontal'>Value</textarea>")

    assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
    assert diagnostic.source == "resize:horizontal"
    assert diagnostic.line == 1 and diagnostic.column >= 1
  end

  test "inactive metadata cannot admit invalid active content or script handlers" do
    for html <- [
          "<button readonly><input></button>",
          "<select checked><input></select>",
          "<p disabled onclick=active>Text</p>"
        ] do
      assert {:error, {reason, diagnostic}} = HtmlToPdf.render(html)
      assert reason in [:unsupported_html, :invalid_html]
      assert diagnostic.stage == :html
      assert diagnostic.reason == reason
      assert is_binary(diagnostic.message) and diagnostic.message != ""
      assert diagnostic.source != nil
      assert diagnostic.line == 1 and diagnostic.column >= 1
    end
  end
end
