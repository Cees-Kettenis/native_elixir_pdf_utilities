defmodule NativeElixirPdfUtilities.HtmlToPdf.FileInputTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout, FileArtwork}
  alias NativeElixirPdfUtilities.Validators.{HtmlFileValidator, FormValidator}
  alias NativeElixirPdfUtilities.Pdf.Reader

  @opts [
    forms: :static,
    system_font_discovery: false,
    fonts: [
      %{family: "Arial", path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf")},
      %{family: "DejaVu Sans", path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf")},
      %{family: "DejaVu Sans Mono", path: Path.expand("priv/fonts/dejavu/DejaVuSansMono.ttf")}
    ]
  ]

  test "empty chooser uses selected button and filename metrics without reading authored paths" do
    for multiple <- ["", "multiple"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<p><input type=file #{multiple} value='/private/secret.txt' style='font-family:DejaVu Sans;font-size:10.5pt'></p>"
               )

      assert {:ok, %{children: [paragraph]} = styled} = Style.compute_detailed(dom, @opts)
      node = hd(paragraph.children)
      display = node.style._file_display
      assert display.button_text == if(multiple == "", do: "Choose File", else: "Choose Files")
      assert display.filename == "No file chosen"

      assert display.button_face.family == "Arial" and
               node.style.font_face.family == "DejaVu Sans"

      assert display.intrinsic_width == 227.25 and display.intrinsic_height == 16.5

      assert node.style._file_control.initial_state.value == "" and
               node.style._file_control.initial_state.files == []

      assert node.style._file_control.attributes["value"] == "/private/secret.txt"
      assert node.children == [] and HtmlFileValidator.validate_initial_layout(node) == :ok
      assert {:ok, layout} = Layout.layout(styled)

      assert Enum.map(Enum.filter(layout.boxes, &(Map.get(&1, :role) == :file_label)), & &1.text) ==
               [display.button_text, display.filename]

      refute Enum.any?(
               layout.boxes,
               &(Map.has_key?(&1, :form_control) and &1.form_control != nil)
             )
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<p><input type=file value='/private/secret.txt'></p>",
               Keyword.put(@opts, :asset_resolver, fn request ->
                 flunk("file resolved #{inspect(request)}")
               end)
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Choose File" and text =~ "No file chosen"
    refute text =~ "secret"
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: []}} = FormValidator.inspect_document(context)
  end

  test "selected mono filename preserves every literal advance without optional Latin substitution" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=file style='font-family:DejaVu Sans Mono;font-size:10.5pt'>"
             )

    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, @opts)
    [_, _, filename] = FileArtwork.boxes(node.style, 0, 0, node.style.width, node.style.height)
    assert filename.text == "No file chosen"
    assert_in_delta filename.width, 88.50146484375, 0.000001
    assert node.style._file_control.initial_state.value == ""

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<p><input type=file style='font-family:DejaVu Sans Mono;font-size:10.5pt'></p>",
               @opts
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "No file chosen"
  end

  test "mutable file actions are diagnosed and disabled static labels use native button alpha" do
    assert {:error, {:unsupported_form, diagnostic}} =
             HtmlToPdf.render("<input type=file>", Keyword.delete(@opts, :forms))

    assert diagnostic.stage == :forms and diagnostic.source == "input/file" and
             diagnostic.message =~ "forms: :static"

    assert {:ok, dom} =
             HtmlParser.parse_detailed("<fieldset disabled><input type=file multiple></fieldset>")

    assert {:ok, %{children: [fieldset]}} =
             Style.compute_detailed(dom, Keyword.delete(@opts, :forms))

    node = hd(fieldset.children)
    assert node.style._file_display.disabled
    [button | labels] = FileArtwork.boxes(node.style, 0, 0, node.style.width, node.style.height)
    assert elem(button.stroke_color, 3) == 0.3
    assert hd(labels).color == {16 / 255, 16 / 255, 16 / 255, 0.3}
    assert List.last(labels).color == {84 / 255, 84 / 255, 84 / 255}
  end

  test "narrow status uses middle ellipsis without changing empty logical state" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=file style='font-family:DejaVu Sans;font-size:10.5pt;width:120pt'>"
             )

    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, @opts)
    available = node.style.width - node.style._file_display.button_width - 3.0

    assert Enum.map(HtmlFileValidator.filename_parts(node.style, available), & &1.text) == [
             "No",
             "…",
             "sen"
           ]

    assert node.style._file_control.initial_state.value == "" and
             node.style._file_display.filename == "No file chosen"

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<p><input type=file style='font-family:DejaVu Sans;font-size:10.5pt;width:120pt'></p>",
               @opts
             )

    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "No…sen"
  end

  test "file default artwork admits zero font size and hidden state without a selected display font" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<input type=file style='font-size:0'>")
    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, @opts)
    assert node.style.width == 15 and node.style.height == 4.5

    assert {:ok, pdf} =
             HtmlToPdf.render("<p>Before<input type=file style='font-size:0'>After</p>", @opts)

    assert {:ok, text} = Text.extract(pdf, layout: false)
    refute text =~ "Choose" or text =~ "chosen"

    assert {:ok, _} =
             HtmlToPdf.render("<input type=file hidden value='/private/secret.txt'>",
               system_font_discovery: false
             )

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<p lang=fr><input type=file></p>", @opts)

    assert diagnostic.source == "input/file" and diagnostic.message =~ "English US"
  end

  test "file artwork consumes existing text and layout budgets with recoverable diagnostics" do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(%{previous | max_rendered_text_bytes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p><input type=file></p>", @opts)

    assert diagnostic.stage == :style and diagnostic.message =~ "max_rendered_text_bytes"
    Limits.install(%{previous | max_layout_boxes: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p><input type=file></p>", @opts)

    assert diagnostic.stage == :writer and diagnostic.message =~ "max_layout_boxes"
  end

  test "file layout rejects forged files and altered geometry with actionable failures" do
    assert {:ok, dom} = HtmlParser.parse_detailed("<input type=file>")
    assert {:ok, %{children: [node]} = styled} = Style.compute_detailed(dom, @opts)

    for forged <- [
          put_in(node.style._file_control.initial_state.files, ["/secret"]),
          put_in(node.style._file_control.initial_state.value, "fakepath"),
          put_in(node.style._file_display.button_width, 1),
          put_in(node.style._file_display, nil),
          put_in(node.style.font_face.units_per_em, 0),
          put_in(node.style.font_face.ascent, nil),
          put_in(node.style.font_face.cmap, nil),
          put_in(node.style._file_display.button_face.cmap, nil),
          %{node | style: Map.delete(node.style, :font_size)},
          put_in(node.style._file_display.button_face, nil),
          %{node | style: Map.delete(node.style, :_file_display)},
          %{node | style: node.style |> Map.delete(:_file_display) |> Map.delete(:_file_control)},
          put_in(node.style._file_control.attributes, nil)
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.layout(%{styled | children: [forged]})

      assert diagnostic.stage == :layout and diagnostic.source == "input/file" and
               diagnostic.message =~ "compute styles"
    end
  end
end
