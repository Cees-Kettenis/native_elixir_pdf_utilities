defmodule NativeElixirPdfUtilities.HtmlToPdf.EmbeddedFormCompositionTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{EmbeddedViewport, Layout, PdfWriter, ResourceContext}

  alias NativeElixirPdfUtilities.Validators.{
    EmbeddedFormValidator,
    HtmlFormValidator,
    HtmlValidator
  }

  setup do
    root =
      Path.join(System.tmp_dir!(), "native-child-forms-#{System.unique_integer([:positive])}")

    File.mkdir_p!(root)

    File.write!(
      Path.join(root, "child.html"),
      "<body style='margin:0;overflow:hidden'><input name='shared' form='parent' value='Child' style='display:block;width:100px;height:24px'></body>"
    )

    File.write!(
      Path.join(root, "nested.html"),
      "<body style='margin:0;overflow:hidden'><object data='child.html' type='text/html' style='display:block;width:100px;height:40px'></object></body>"
    )

    limits = Limits.effective()

    on_exit(fn ->
      Limits.install(limits)
      File.rm_rf!(root)
    end)

    %{root: root}
  end

  test "sibling, nested and parent documents retain independent named fields and initial ownership",
       %{root: root} do
    html =
      "<body style='margin:0'><form id='parent'><input name='shared' value='Parent'><input name='CHILD_1_shared' value='Collision'></form><iframe src='child.html' style='display:block;border:0;width:120px;height:50px'></iframe><iframe src='nested.html' style='display:block;border:0;width:120px;height:60px'></iframe></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: root, page_size: {240, 240}, margin: 0)
    assert {:ok, fields} = Forms.fields(pdf)
    values = Map.new(fields, &{&1.name, &1.value})

    assert values == %{
             "shared" => "Parent",
             "CHILD_1_shared" => "Collision",
             "CHILD_2_shared" => "Child",
             "CHILD_3_shared" => "Child"
           }

    assert {:ok, changed} = Forms.fill(pdf, %{"CHILD_2_shared" => "Changed child"})
    assert {:ok, fields} = Forms.fields(changed)
    values = Map.new(fields, &{&1.name, &1.value})
    assert values["shared"] == "Parent"
    assert values["CHILD_2_shared"] == "Changed child" and values["CHILD_3_shared"] == "Child"
  end

  test "a partially clipped child field keeps the widget rectangle and cropped appearance" do
    child =
      "<body style='margin:0;overflow:hidden'><input name='clipped' value='Alpha' style='display:block;width:160px;height:24px;margin-left:-10px'></body>"

    html =
      "<body style='margin:0'><div style='height:20px'></div><iframe style='display:block;border:0;width:120px;height:60px' srcdoc='#{escape(child)}'></iframe></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {180, 120}, margin: 0)
    assert {:ok, [field]} = Forms.fields(pdf)
    assert field.name == "CHILD_1_clipped" and field.value == "Alpha"
    assert field.widgets == [%{page: 1, rect: [0, 82.5, 90, 105]}]
    assert {:ok, flattened} = Forms.flatten(pdf)
    assert {:ok, []} = Forms.fields(flattened)
    assert {:ok, text} = Text.extract(flattened)
    assert text =~ "Alpha"
  end

  test "checkbox and independent radio document states remain editable fields", %{root: root} do
    File.write!(
      Path.join(root, "buttons.html"),
      "<body style='margin:0;overflow:hidden'><input type='checkbox' name='tick' checked><input type='radio' name='pick' value='a' checked><input type='radio' name='pick' value='b'></body>"
    )

    html =
      "<body style='margin:0'><iframe src='buttons.html' style='display:block;border:0;width:140px;height:50px'></iframe><iframe src='buttons.html' style='display:block;border:0;width:140px;height:50px'></iframe></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: root)
    assert {:ok, fields} = Forms.fields(pdf)

    assert Enum.map(fields, & &1.name) == [
             "CHILD_1_tick",
             "CHILD_1_pick",
             "CHILD_2_tick",
             "CHILD_2_pick"
           ]

    assert Enum.filter(fields, &(&1.type == :checkbox)) |> Enum.all?(& &1.value)

    for field <- Enum.filter(fields, &(&1.type == :radio)) do
      assert field.value == "a" and length(field.widgets) == 2
      assert Enum.sort(field.export_values) == ["a", "b"]
    end

    assert {:ok, changed} = Forms.fill(pdf, %{"CHILD_1_tick" => false, "CHILD_1_pick" => "b"})
    assert {:ok, fields} = Forms.fields(changed)
    values = Map.new(fields, &{&1.name, &1.value})
    assert values["CHILD_1_tick"] == false and values["CHILD_1_pick"] == "b"
    assert values["CHILD_2_tick"] == true and values["CHILD_2_pick"] == "a"
  end

  test "issued field appearances support positive axis-aligned parent scaling" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc,
                  "<body style='margin:0'><input name='scaled' value='Scale' style='width:40px;height:20px'></body>"},
                 parent
               )

      assert {:ok, surface} = EmbeddedViewport.style_viewport(prepared, 90, 60)
      assert {:ok, layout} = Layout.layout_viewport(surface.tree, 90, 60)

      assert {:ok, [_page], fields} =
               HtmlFormValidator.prepare_scoped(
                 [%{size: {90, 60}, boxes: layout.boxes}],
                 [],
                 surface.context
               )

      assert {:ok, descriptor} = EmbeddedFormValidator.issue(surface.context, fields)

      group = %{
        type: :drawing_group,
        width: 90,
        height: 60,
        transform: {2, 0, 0, 3, 10, 20},
        clip: %{x: 0, y: 0, width: 90, height: 60},
        boxes: [],
        embedded_widgets: descriptor
      }

      assert {:ok, pdf} = PdfWriter.render([%{size: {240, 240}, boxes: [group]}])
      assert {:ok, [field]} = Forms.fields(pdf)
      assert [%{rect: [left, bottom, right, top]}] = field.widgets
      [l, b, r, t] = hd(fields).rect
      assert {left, bottom, right, top} == {2 * l + 10, 3 * b + 20, 2 * r + 10, 3 * t + 20}
      assert field.value == "Scale"
    end)
  end

  test "choice, multiline, password, Number and readonly scalar child fields retain shared policies" do
    child = """
    <body style='margin:0;overflow:hidden'>
    <style>input,textarea,select{display:block;width:140px;margin:0}</style>
    <input name='text' value='Text'><input name='password' type='password' value='Secret'>
    <input name='number' type='number' value='2.5'>
    <textarea name='notes'>One\nTwo</textarea>
    <select name='choice'><option value='a'>A</option><option value='b' selected>B</option></select>
    <select name='list' multiple size='3'><option value='a' selected>A</option><option value='b' selected>B</option><option value='c'>C</option></select>
    <input name='range' type='range' value='25' disabled>
    <input name='date' type='date' value='2026-10-13' readonly>
    <input name='locked' value='Locked' disabled>
    </body>
    """

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<iframe style='display:block;border:0;width:180px;height:500px' srcdoc='#{escape(child)}'></iframe>",
               page_size: {240, 450},
               margin: 0
             )

    assert {:ok, fields} = Forms.fields(pdf)
    fields = Map.new(fields, &{&1.name, &1})
    assert fields["CHILD_1_text"].value == "Text"
    assert fields["CHILD_1_password"].value == "Secret"
    assert fields["CHILD_1_number"].value == "2.5"
    assert fields["CHILD_1_notes"].value == "One\nTwo"
    assert fields["CHILD_1_choice"].value == "b"
    assert fields["CHILD_1_list"].value == ["a", "b"]
    assert fields["CHILD_1_range"].read_only and fields["CHILD_1_range"].value == "25"
    assert fields["CHILD_1_date"].read_only and fields["CHILD_1_date"].value == "2026-10-13"
    assert fields["CHILD_1_locked"].read_only

    assert {:error, {:read_only_form_field, diagnostic}} =
             Forms.fill(pdf, %{"CHILD_1_locked" => "Changed"})

    assert diagnostic.operation == :fill and diagnostic.source == "CHILD_1_locked"
  end

  test "input target markers survive child field appearance extraction" do
    child =
      "<body style='margin:0;overflow:hidden'><a href='#field'>Jump to field</a><input id='field' name='target' value='Target' style='display:block;width:80px'></body>"

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<iframe style='display:block;border:0;width:140px;height:80px' srcdoc='#{escape(child)}'></iframe>",
               page_size: {240, 180},
               margin: 0
             )

    assert {:ok, [_field]} = Forms.fields(pdf)
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      for {_, %{value: %{"Subtype" => {:name, "Link"}} = link}} <- document.objects, do: link

    assert [%{"Dest" => [_page, {:name, "XYZ"}, x, y, nil]}] = links
    assert x >= 0 and y >= 0 and y <= 180
  end

  test "cumulative child and parent field ceilings reject before PDF field allocation", %{
    root: root
  } do
    Limits.install(Map.put(Limits.effective(), :max_pdf_form_fields, 1))

    for source <- [
          "<iframe src='child.html'></iframe><iframe src='child.html'></iframe>",
          "<input name='root'><iframe src='child.html'></iframe>"
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(source, base_url: root)

      assert diagnostic.message =~ "max_pdf_form_fields"
    end
  end

  test "zero-area fields still prepare source policy while supported fields produce no annotation" do
    for dimension <- ["width:0;height:40px", "width:100px;height:0"] do
      for child <- ["<input name='zero' value='Hidden'>", "<input type='range'>"] do
        html =
          "<iframe style='display:block;border:0;#{dimension}' srcdoc='#{escape(child)}'></iframe>"

        if child =~ "range" do
          assert {:error, {:unsupported_form, diagnostic}} = HtmlToPdf.render(html)
          assert diagnostic.message =~ "Range"
        else
          assert {:ok, pdf} = HtmlToPdf.render(html)
          assert {:ok, []} = Forms.fields(pdf)
        end
      end
    end
  end

  test "child appearances share widget and text allocations before clipping", %{root: root} do
    original = Limits.effective()
    source = "<iframe src='child.html'></iframe><iframe src='child.html'></iframe>"

    for {limit, maximum} <- [{:max_appearance_widgets, 3}, {:max_appearance_text_bytes, 8}] do
      Limits.install(Map.put(original, limit, maximum))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(source, base_url: root)

      assert diagnostic.message =~ Atom.to_string(limit)
      assert diagnostic.stage == :forms
    end

    Limits.install(Map.put(original, :max_pdf_form_fields, 0))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               "<iframe src='child.html' style='width:0;height:0;border:0'></iframe>",
               base_url: root
             )

    assert diagnostic.message =~ "max_pdf_form_fields"
  end

  defp escape(text) do
    text
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace("'", "&#39;")
  end
end
