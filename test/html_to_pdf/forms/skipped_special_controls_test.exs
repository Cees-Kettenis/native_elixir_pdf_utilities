alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.SkippedSpecialControlsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Forms, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}

  test "skipped temporal boxes retain canonical logical values without selected-font picker requirements" do
    for {kind, value} <- [
          {"date", "2026-10-13"},
          {"month", "2026-10"},
          {"week", "2026-W42"},
          {"time", "14:30"},
          {"datetime-local", "2026-10-13T14:30"}
        ] do
      html = "<input hidden=until-found type=#{kind} value=#{value} lang=de name=secret>"
      assert {:ok, source} = HtmlParser.parse_detailed("<body>" <> html <> "</body>")
      assert {:ok, styled} = Style.compute_detailed(source, system_font_discovery: false)
      [node] = hd(styled.children).children
      assert node._initial_form_state.value == value
      assert node.attributes["value"] == value
      assert node.node_id == hd(hd(source.children).children).node_id
      assert node.style.width == 0.0 and node.style.height == 0.0
      assert node.style._html_content_skipped
      refute Map.has_key?(node.style, :_temporal_display)
      refute Map.has_key?(node.style, :_form_control)
      assert {:ok, _} = Layout.layout(styled)
      assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
      assert {:ok, []} = Forms.fields(pdf)
    end
  end

  test "skipped color and range preserve source state while excluding internal swatch and track paint" do
    html =
      "<input hidden=until-found type=color value=red style='width:4pt;height:4pt'><input hidden=until-found type=range value=30>"

    assert {:ok, dom} = HtmlParser.parse_detailed("<body>" <> html <> "</body>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    [color, range] = hd(styled.children).children
    assert color._initial_form_state.value == "#ff0000"
    assert color.style.width == 4.0 and color.style.height == 4.0
    assert range._initial_form_state.value == "30"
    assert range.style.height == 0.0 and range.style.width == 96.75
    assert {:ok, _} = Layout.layout(styled)

    assert color.style._color_state == color._initial_form_state.color_input
    assert range.style._range_state == range._initial_form_state.range
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled)
    refute Enum.any?(boxes, &(Map.get(&1, :role) in [:color_swatch, :range_thumb, :range_value]))

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, []} = Forms.fields(pdf)
  end

  test "skipped file retains empty selection without reading authored value paths or painting labels" do
    html = "<input hidden=until-found type=file value='/private/secret.txt' multiple lang=de>"
    opts = [asset_resolver: fn request -> flunk("unexpected asset #{inspect(request)}") end]
    assert {:ok, dom} = HtmlParser.parse_detailed("<body>" <> html <> "</body>")
    assert {:ok, %{children: [%{children: [node]}]}} = Style.compute_detailed(dom, opts)
    assert node._initial_form_state.files == [] and node._initial_form_state.value == ""
    assert node.attributes["value"] == "/private/secret.txt"
    assert node.style.height == 0.0 and node.style.width > 0
    refute Map.has_key?(node.style, :_file_display)
    assert {:ok, pdf} = HtmlToPdf.render(html <> "<p>Visible</p>", opts)
    assert {:ok, []} = Forms.fields(pdf)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Visible"
    refute text =~ "secret" or text =~ "Choose" or text =~ "chosen"
  end

  test "skipped gauges retain state and explicit frames without value shadow or pseudo paint" do
    html =
      "<style>progress::before{content:'Secret'}</style><progress hidden=until-found value=.5 style='width:80pt;height:18pt;border:1pt solid red'></progress><meter hidden=until-found value=.3></meter><p>Visible</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed("<body>" <> html <> "</body>")
    assert {:ok, styled} = Style.compute_detailed(dom)
    [progress, meter, _] = hd(styled.children).children
    assert progress._initial_gauge_state.value == 0.5
    assert progress.style.width == 80.0 and progress.style.height == 18.0
    assert meter._initial_gauge_state.value == 0.3
    assert {:ok, %{boxes: boxes}} = Layout.layout(styled)

    refute Enum.any?(
             boxes,
             &(Map.get(&1, :role) in [:gauge_value, :gauge_indeterminate])
           )

    assert Enum.any?(boxes, &(Map.get(&1, :role) == :gauge_track))
    assert Enum.any?(boxes, &(Map.get(&1, :stroke_color) == {1.0, 0.0, 0.0}))
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Visible"
    refute text =~ "Secret"
  end

  test "skipped image removes replaced pixels while retaining authored dimension hints and authority" do
    bytes =
      File.read!(Path.expand("test/fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png"))

    opts = [
      asset_resolver: fn request ->
        send(self(), {:asset, request.reference})
        {:ok, bytes}
      end
    ]

    html =
      "<input hidden=until-found type=image src=tile style='display:block'><input hidden=until-found type=image src=tile width=64 height=16 style='display:inline-block'>"

    assert {:ok, dom} = HtmlParser.parse_detailed("<body>" <> html <> "</body>")
    assert {:ok, styled} = Style.compute_detailed(dom, opts)
    [auto, fixed] = hd(styled.children).children
    assert auto.style.width == 0.0 and auto.style.height == 0.0
    assert fixed.style.width == 48.0 and fixed.style.height == 12.0
    assert fixed.style.display == :inline_block

    for node <- hd(styled.children).children do
      refute Map.has_key?(node.style, :image)
      assert node._initial_form_state.image_input.src == "tile"
    end

    assert_receive {:asset, "tile"}
    assert {:ok, _} = Layout.layout(styled)
    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert {:ok, []} = Forms.fields(pdf)

    assert {:error, {_, diagnostic}} =
             HtmlToPdf.render(html,
               asset_resolver: fn _ -> {:error, :denied} end
             )

    assert diagnostic.stage == :asset and diagnostic.source == "tile"
  end

  test "authored dimensions override collapsed intrinsic sizes and visible cascade overrides restore UI" do
    for kind <- ~w(range file date month week time datetime-local) do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<body><input hidden=until-found type=#{kind} style='width:72pt;height:18pt;border:1pt solid red;padding:2pt'></body>"
               )

      assert {:ok, %{children: [%{children: [node]}]} = styled} = Style.compute_detailed(dom)
      assert node.style.width == 72.0 and node.style.height == 18.0
      assert node.style.padding.left == 2.0 and node.style.border_widths.top == 0.75
      assert {:ok, _} = Layout.layout(styled, page_size: {420, 420})
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input hidden=until-found type=color value=red style='content-visibility:visible'>"
             )

    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, forms: :static)
    refute Map.has_key?(node.style, :_html_content_skipped)
    assert node.style._color_state.rgb == {1.0, 0.0, 0.0}
  end

  test "skipped file intrinsic width obeys the existing shared numeric ceiling" do
    alias NativeElixirPdfUtilities.Limits
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(%{previous | max_css_numeric_magnitude: 100})
    assert {:ok, dom} = HtmlParser.parse_detailed("<input hidden=until-found type=file>")
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Style.compute_detailed(dom)
    assert diagnostic.stage == :style
    assert diagnostic.message =~ "max_css_numeric_magnitude"
  end

  @tag :browser_parity
  @tag :skipped_special_inputs
  test "original skipped host artwork matches Chromium with caller fonts and empty PDF actions" do
    alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare

    fonts =
      for family <- ["Arial", "DejaVu Sans", "DejaVu Sans Mono"] do
        %{
          family: family,
          path:
            Path.expand(
              "priv/fonts/dejavu/" <>
                if(family == "DejaVu Sans Mono", do: "DejaVuSansMono.ttf", else: "DejaVuSans.ttf")
            ),
          weight: 400,
          style: :normal
        }
      end

    stats =
      PdfVisualCompare.assert_browser_match!(
        Paths.fixture("html_to_pdf/browser_parity/html_skipped_special_controls.html"),
        render_opts: [forms: :interactive, fonts: fonts, system_font_discovery: false],
        artifact_dir: "tmp/browser_parity/skipped-special-public-proof",
        max_changed_ratio: 0.01,
        max_average_delta: 0.01
      )

    assert stats.page_count == 2
    assert length(stats.font_sources) == 3

    for source <- stats.font_sources do
      assert source.sha256 ==
               Base.encode16(:crypto.hash(:sha256, File.read!(source.path)), case: :lower)
    end

    for renderer <- ["native", "chromium"] do
      bytes = File.read!(Path.join(stats.artifact_dir, renderer <> ".pdf"))
      assert {:ok, text} = Text.extract(bytes)

      for caption <- [
            "color",
            "range",
            "file",
            "date",
            "month",
            "week",
            "time",
            "image",
            "gauges"
          ] do
        assert text =~ caption
      end

      refute text =~ "secret" or text =~ "2026" or text =~ "chosen" or text =~ "Secret"
    end

    assert {:ok, []} = Forms.fields(File.read!(Path.join(stats.artifact_dir, "native.pdf")))
  end

  test "forged skipped paint state returns an actionable public layout diagnostic" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed("<body><input hidden=until-found type=color></body>")

    assert {:ok, styled} = Style.compute_detailed(dom)
    [node] = hd(styled.children).children
    invalid = %{node | style: Map.put(node.style, :_color_state, %{rgb: {1, 0, 0}})}

    assert {:error, {reason, diagnostic}} =
             Layout.layout(%{styled | children: [%{hd(styled.children) | children: [invalid]}]})

    assert reason in [:invalid_document, :invalid_layout]
    assert diagnostic.stage == :layout and diagnostic.operation == :layout
    assert diagnostic.source in ["input", "color"]
    assert is_binary(diagnostic.message) and byte_size(diagnostic.message) > 20
  end
end
