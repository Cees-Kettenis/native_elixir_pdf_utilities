defmodule NativeElixirPdfUtilities.HtmlToPdf.ControlMeasurementTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, WriterValidator}

  @fixture Path.expand(
             "../fixtures/html_to_pdf/browser_parity/html_inline_select_controls.html",
             __DIR__
           )

  test "authored inline menus and listboxes measure their complete host in issued contexts" do
    for css <- ["", "border:2pt dashed red;border-radius:3pt", "appearance:none;padding:2pt"],
        attrs <- ["", "size=3"] do
      html =
        "<select name=pick #{attrs} style='display:inline;width:105pt;#{css}'><option value=a selected>Alpha</option><option value=b>Beta</option></select>"

      ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
        assert {:ok, dom} = HtmlParser.parse_detailed(html)
        assert {:ok, styled} = Style.compute_detailed(dom, [], context.image_budget)
        node = hd(styled.children)
        assert node.attributes == hd(dom.children).attributes
        assert node.style.display == :inline_block

        for available <- [nil, 20.0, 105.0, 300.0] do
          # Caller-supplied initial style delegates to the same canonical used box.
          supplied = put_in(node.style.display, :inline)

          assert {:ok, measured} =
                   Layout.measure_child(supplied, context, %{available_width: available})

          assert_in_delta measured.width, 105.0, 0.75 / 64
          assert_in_delta measured.ascent + measured.descent, node.style.height, 0.75 / 64
          hosts = Enum.filter(measured.boxes, &is_map(&1[:form_control]))
          assert length(hosts) == 1 and hd(hosts).form_control.attributes["name"] == "pick"
          assert hd(hosts).height == measured.ascent + measured.descent
          assert Enum.any?(measured.boxes, &(&1.type == :text and &1.text == "Alpha"))

          assert Enum.any?(measured.boxes, &(Map.get(&1, :role) == :select_arrow)) ==
                   (attrs == "" and not String.contains?(css, "appearance:none"))
        end
      end)
    end
  end

  test "raw inline initial controls explain the missing page owner while real documents render" do
    for html <- [
          "<select style='display:inline'><option>A</option></select>",
          "<input type=range style='appearance:none'>"
        ] do
      assert {:ok, raw} = HtmlParser.parse_detailed(html)
      assert {:ok, styled} = Style.compute_detailed(raw)
      assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(styled)
      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
      assert diagnostic.operation == :layout and diagnostic.source in ["input", "select"]
      assert diagnostic.message =~ "parse_document_detailed"
      assert {:ok, document} = HtmlParser.parse_document_detailed(html)
      assert {:ok, tree} = Style.compute_detailed(document)
      assert {:ok, _layout} = Layout.layout(tree)
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static)
      assert {:ok, _} = Reader.read_validated(pdf)
    end
  end

  test "source owners, complete options, initial values and real widget bounds survive fill and flatten" do
    assert {:ok, pdf} = HtmlToPdf.render(File.read!(@fixture), forms: :interactive)
    assert {:ok, fields} = Forms.fields(pdf)
    by_name = Map.new(fields, &{&1.name, &1})
    assert Map.keys(by_name) |> Enum.sort() == ~w(external list native owned plain rounded)

    for {name, field} <- by_name do
      assert field.value == if(name == "list", do: "b", else: "a")
      assert length(field.choices) == if(name == "list", do: 4, else: 2)
      assert field.read_only == (name == "external")

      assert Enum.all?(field.widgets, fn widget ->
               [left, bottom, right, top] = widget.rect

               right > left and top > bottom and left >= 0 and bottom >= 0 and right <= 400 and
                 top <= 340
             end)
    end

    assert {:ok, filled} =
             Forms.fill(pdf, %{
               "native" => "b",
               "rounded" => "b",
               "plain" => "b",
               "list" => "c",
               "owned" => "b"
             })

    assert {:ok, updated} = Forms.fields(filled)

    for field <- updated do
      expected =
        case field.name do
          "external" -> "a"
          "list" -> "c"
          _ -> "b"
        end

      assert field.value == expected
    end

    assert {:ok, flat} = Forms.flatten(filled)
    assert {:ok, []} = Forms.fields(flat)
    assert {:ok, text} = Text.extract(flat)

    assert text =~ "Beta" and text =~ "Plain second" and text =~ "Owned second" and
             text =~ "After row"
  end

  test "disabled initial paint uses one isolated transparency Form and obeys shared object limits" do
    html =
      "<p style='background:#cceeff'>Before <select disabled name=pick style='display:inline;color:blue;border-radius:3pt'><option selected>Alpha</option></select> After</p>"

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode, compress_streams: false)
      assert {:ok, document} = Reader.read_validated(pdf)

      assert Enum.any?(document.document.objects, fn {_id, object} ->
               is_map(object.value) and
                 object.value["Group"] == %{
                   "S" => {:name, "Transparency"},
                   "I" => true,
                   "CS" => {:name, "DeviceRGB"}
                 }
             end)

      assert Enum.any?(document.document.objects, fn {_id, object} ->
               is_map(object.value) and object.value["ca"] == 0.7 and object.value["CA"] == 0.7
             end)

      assert {:ok, flat} = Forms.flatten(pdf)
      assert {:ok, []} = Forms.fields(flat)
      assert {:ok, text} = Text.extract(flat)
      assert text =~ "Before" and text =~ "Alpha" and text =~ "After"
    end

    previous = Limits.effective()

    failure =
      try do
        Limits.install(Map.put(previous, :max_pdf_objects, 6))
        HtmlToPdf.render(html, forms: :static)
      after
        Limits.install(previous)
      end

    assert {:error, {:resource_limit_exceeded, diagnostic}} = failure

    assert diagnostic.reason == :resource_limit_exceeded and
             diagnostic.message =~ "max_pdf_objects"

    invalid = %{type: :rect, initial_select_opacity: %{owner: make_ref(), alpha: 2}}
    assert {:error, {_reason, diagnostic}} = WriterValidator.validate_native_boxes([invalid])
    assert is_atom(diagnostic.stage) and is_binary(diagnostic.message)
  end
end
