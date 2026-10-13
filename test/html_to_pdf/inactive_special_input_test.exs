defmodule NativeElixirPdfUtilities.HtmlToPdf.InactiveSpecialInputTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.FormValidator
  @image Path.expand("../fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png", __DIR__)
  @opts [
    forms: :interactive,
    system_font_discovery: false,
    fonts: [
      %{family: "Arial", path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf")},
      %{family: "DejaVu Sans", path: Path.expand("priv/fonts/dejavu/DejaVuSans.ttf")}
    ]
  ]

  test "inert initial color preserves enabled-looking artwork without PDF picker actions" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<div inert><input type=color value=red name=ink></div>")

    assert {:ok, %{children: [container]} = styled} = Style.compute_detailed(dom, @opts)
    [node] = container.children
    refute node.style._color_disabled
    refute node.style._color_control.initial_state.disabled
    refute Map.has_key?(node.style, :_form_control)
    assert node.style._color_control.initial_state.value == "#ff0000"
    assert {:ok, _} = Layout.layout(styled)

    assert {:ok, pdf} =
             HtmlToPdf.render("<div inert><input type=color value=red name=ink></div>", @opts)

    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: []}} = FormValidator.inspect_document(context)
  end

  test "inert empty file chooser never resolves authored value paths or creates PDF fields" do
    html =
      "<div inert><input type=file name=secret value='/private/unreadable.txt' multiple></div>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        send(self(), {:unexpected_asset, request})
        {:error, :denied}
      end)

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [container]}} = Style.compute_detailed(dom, opts)
    [node] = container.children
    assert node.style._file_control.attributes["value"] == "/private/unreadable.txt"
    assert node.style._file_control.initial_state.value == ""
    assert node.style._file_control.initial_state.files == []
    refute node.style._file_control.initial_state.disabled
    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: []}} = FormValidator.inspect_document(context)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Choose Files" and text =~ "No file chosen"
    refute text =~ "unreadable"
    refute_receive {:unexpected_asset, _}
  end

  test "inert image remains caller-authorized and paints without submit coordinates" do
    html = "<div inert><input type=image src=tile name=send width=80 height=40></div>"
    bytes = File.read!(@image)

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        send(self(), {:asset, request})
        {:ok, bytes}
      end)

    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert_receive {:asset, %{kind: :image, reference: "tile"}}
    refute_receive {:asset, _}
    assert {:ok, context} = Reader.read_validated(pdf)
    assert {:ok, %{fields: []}} = FormValidator.inspect_document(context)
    assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :asset and diagnostic.operation == :render
    assert diagnostic.source == "tile"
    assert diagnostic.message =~ "authorized local file"
  end

  test "active file and image controls still diagnose unavailable mutable PDF actions" do
    for {html, source} <- [
          {"<input type=file>", "input/file"},
          {"<input type=image src=tile>", "input/image"}
        ] do
      assert {:error, {:unsupported_form, diagnostic}} =
               HtmlToPdf.render(
                 html,
                 Keyword.put(@opts, :assets, %{"tile" => {:bytes, File.read!(@image)}})
               )

      assert diagnostic.stage == :forms and diagnostic.operation == :compute
      assert diagnostic.source == source
    end
  end

  test "font-configured browser input cleans its temporary source sibling after Chromium failure" do
    alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

    fixture =
      Path.expand(
        "../fixtures/html_to_pdf/browser_parity/html_inert_special_initial_controls.html",
        __DIR__
      )

    directory = Path.dirname(fixture)
    original = File.read!(fixture)
    before_files = Path.wildcard(Path.join(directory, ".chromium-input-*.html"), match_dot: true)

    failure =
      assert_raise ExUnit.AssertionError, ~r/Chromium PDF rendering failed/, fn ->
        PdfVisualCompare.assert_browser_match!(fixture,
          render_opts: @opts,
          chromium_bin: "/bin/false",
          artifact_dir: "tmp/browser_parity/inactive-source-context-failure"
        )
      end

    assert failure.message =~ ".chromium-input-"
    assert failure.message =~ "file://#{directory}/"
    assert File.read!(fixture) == original

    assert Path.wildcard(Path.join(directory, ".chromium-input-*.html"), match_dot: true) ==
             before_files

    copied = File.read!("tmp/browser_parity/inactive-source-context-failure/chromium-input.html")
    assert copied =~ ~s(src="assets/iframe-bars.png")
    refute copied =~ "<base"
  end
end
