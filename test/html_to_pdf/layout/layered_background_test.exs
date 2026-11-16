defmodule NativeElixirPdfUtilities.HtmlToPdf.LayeredBackgroundTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, Pagination, Style}
  alias NativeElixirPdfUtilities.TestSupport.PngFixture

  test "gradient layers paint independently above the color and below physical borders" do
    html =
      "<div style='width:100pt;height:40pt;background:linear-gradient(to right,red,blue) left top/20pt 20pt no-repeat content-box, image(lime) border-box; border:6pt solid black; padding:6pt'></div>"

    {styled, layout} = layout(html)
    assert length(hd(styled.children).style.background_images) == 2
    images = Enum.filter(layout.boxes, &(&1.type == :image))
    assert length(images) == 2
    [solid, gradient] = images
    assert solid.image.data == <<0, 255, 0>>
    assert gradient.width == 20 and gradient.height == 20
    assert gradient.clip.x == 12
    assert gradient.clip.width == 100
    assert {:ok, "%PDF-" <> _} = HtmlToPdf.render(html, system_font_discovery: false)
  end

  test "root gradients suppress body canvas propagation and paint once per page" do
    html =
      "<html><head><style>html{background-image:linear-gradient(to right,red,blue)}body{margin:0;background:lime}</style></head><body><p>Text</p></body></html>"

    {styled, layout} = layout(html)
    refute styled.document_canvas.body_background_propagated
    assert [%{kind: :gradient}] = styled.document_canvas.style.background_images
    images = Enum.filter(layout.boxes, &(&1.type == :image))
    assert Enum.any?(images, &(&1.width == 200 and &1.height == 100))
  end

  test "space and round repeat use per-axis tile geometry and corrected natural aspect ratios" do
    png = PngFixture.build(4, 2, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    uri = "data:image/png;base64," <> Base.encode64(png)

    for {repeat, expected_x, expected_width} <- [
          {"space no-repeat", [0.0, 35.0, 70.0], 30.0},
          {"round no-repeat", [0.0, 100 / 3, 200 / 3], 100 / 3}
        ] do
      {_, layout} =
        layout(
          "<div style='width:100pt;height:40pt;background-image:url(#{uri});background-size:30pt auto;background-repeat:#{repeat}'></div>"
        )

      images = Enum.filter(layout.boxes, &(&1.type == :image))
      assert length(images) == 3

      for {box, x} <- Enum.zip(images, expected_x) do
        assert_in_delta box.x, x, 0.000001
        assert_in_delta box.width, expected_width, 0.000001
        assert_in_delta box.height, expected_width / 2, 0.000001
      end
    end
  end

  test "only the selected CSS image-set candidate is loaded and its density corrects natural size" do
    owner = self()
    png = PngFixture.build(2, 2, 2, 8, 0, fn _, _ -> [0, 0, 255] end)

    html =
      "<div style='width:40pt;height:20pt;background-image:image-set(url(one) 1x,url(two) 2x);background-repeat:no-repeat'></div>"

    {_, layout} =
      layout(html,
        image_selection_density: 1.5,
        asset_resolver: fn request ->
          send(owner, {:loaded, request.reference})
          {:ok, png}
        end
      )

    assert_received {:loaded, "two"}
    refute_received {:loaded, "one"}
    [image] = Enum.filter(layout.boxes, &(&1.type == :image))
    assert image.width == 0.75 and image.height == 0.75
  end

  test "fixed backgrounds use each final page viewport while preserving fragment clipping" do
    html =
      "<div style='height:230pt;background-image:linear-gradient(to right,red,blue);background-attachment:fixed'><p>First</p></div>"

    {_, layout} = layout(html)
    assert layout.css_fixed_backgrounds
    assert {:ok, pages} = Pagination.paginate(layout)
    assert length(pages) >= 3
    images = Enum.map(pages, fn page -> Enum.filter(page.boxes, &(&1.type == :image)) end)
    assert Enum.all?(images, &(&1 != []))
    assert Enum.map(images, fn [box | _] -> box.image.data end) |> Enum.uniq() |> length() == 1

    for tiles <- images, tile <- tiles do
      assert tile.x == 0 and tile.y == 0 and tile.width == 200 and tile.height == 100
    end
  end

  test "forged layer structures return an actionable public layout diagnostic" do
    {styled, _} = layout("<div>Text</div>")
    [node] = styled.children

    for fields <- [
          %{background_images: [nil, :other]},
          %{background_positions: []},
          %{background_sizes: [{:other, 2}]},
          %{background_repeats: [:invalid]},
          %{background_origins: [:margin_box]},
          %{background_clips: [:none]},
          %{background_attachments: [:sticky]},
          %{background_images: [%{kind: :gradient}]}
        ] do
      forged = %{node | style: Map.merge(node.style, fields)}

      assert {:error, {:invalid_layout, %{stage: :layout, source: "div", message: message}}} =
               Layout.layout(%{styled | children: [forged]})

      assert message =~ "background layers"
    end
  end

  test "advanced backgrounds retain intrinsic aspect ratios across fit and per-axis repetition" do
    png = PngFixture.build(4, 2, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    for {size, repeat, width, height, count} <- [
          {"contain", "no-repeat", 80.0, 40.0, 1},
          {"cover", "no-repeat", 100.0, 50.0, 1},
          {"auto 20pt", "no-repeat", 40.0, 20.0, 1},
          {"auto 15pt", "no-repeat round", 80 / 3, 40 / 3, 3},
          {"200pt 20pt", "space no-repeat", 200.0, 20.0, 1}
        ] do
      {_, layout} =
        layout(
          "<div style='width:100pt;height:40pt;background-image:url(tile);background-size:#{size};background-repeat:#{repeat};background-clip:padding-box'></div>",
          assets: %{"tile" => {:bytes, png}}
        )

      images = Enum.filter(layout.boxes, &(&1.type == :image))
      assert length(images) == count

      for image <- images do
        assert_in_delta image.width, width, 0.000001
        assert_in_delta image.height, height, 0.000001
      end
    end

    for repeat <- ["repeat-x", "repeat-y"] do
      {_, layout} =
        layout(
          "<div style='width:30pt;height:20pt;background-image:url(tile);background-size:10pt 10pt;background-repeat:#{repeat};background-clip:padding-box'></div>",
          assets: %{"tile" => {:bytes, png}}
        )

      assert Enum.count(layout.boxes, &(&1.type == :image)) ==
               if(repeat == "repeat-x", do: 3, else: 2)
    end
  end

  test "inline strips group gradient paint and decorations across words and line fragments" do
    html =
      "<p style='width:80pt;margin:0'><span style='background:linear-gradient(to right,red,blue);border-radius:8pt / 3pt;outline:1pt solid black;box-shadow:1pt 1pt 0 1pt rgba(0,0,0,.4)'>one two three four five six seven eight</span></p>"

    {_, layout} = layout(html)
    images = Enum.filter(layout.boxes, &(&1.type == :image))
    shadows = Enum.filter(images, &(Map.get(&1, :css_paint_role) == :box_shadow))
    assert length(shadows) >= 2

    assert length(shadows) ==
             Enum.count(layout.boxes, &(Map.get(&1, :css_paint_role) == :css_outline))

    assert Enum.any?(images, &is_map(get_in(&1, [:clip, :inner_clip, :border_radii])))

    assert {:ok, _} = HtmlToPdf.render(html, system_font_discovery: false)
  end

  test "nested fixed backgrounds resolve in their child viewport and retain the outer page scope" do
    child =
      "<div style='height:40pt;background-image:linear-gradient(red,blue);background-attachment:fixed'>Child</div>"

    html =
      "<div style='height:80pt;background-image:linear-gradient(red,blue);background-attachment:fixed'><iframe width='60' height='40' srcdoc=\"#{child}\"></iframe></div>"

    assert {:ok, _} = HtmlToPdf.render(html, system_font_discovery: false)

    {_, layout} =
      layout(
        "<div style='background-color:red;background-origin:content-box;background-clip:padding-box;width:20pt;height:10pt'></div>"
      )

    assert Enum.any?(layout.boxes, &(&1.type == :rect and &1.fill_color == {1, 0, 0}))
  end

  test "page canvas layers use the page area with nonzero margins" do
    html =
      "<html style='background-image:linear-gradient(red,blue)'><body style='margin:0'>Text</body></html>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, system_font_discovery: false)

    assert {:ok, layout} =
             Layout.layout(styled,
               page_size: {200, 100},
               margin: %{top: 10, right: 20, bottom: 15, left: 5}
             )

    canvas = Enum.filter(layout.boxes, &(Map.get(&1, :position_anchor) == :canvas))
    assert canvas != []

    for image <- canvas do
      assert {image.x, image.y, image.width, image.height} == {5, 15, 175, 75}
    end
  end

  test "space repetition extends its pattern from the content origin across the border painting area" do
    png = PngFixture.build(2, 2, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    {_, layout} =
      layout(
        "<div style='width:100pt;height:20pt;padding:6pt;border:6pt solid transparent;background-image:url(tile);background-origin:content-box;background-clip:border-box;background-size:30pt 10pt;background-repeat:space no-repeat'></div>",
        assets: %{"tile" => {:bytes, png}}
      )

    images = Enum.filter(layout.boxes, &(&1.type == :image))
    assert Enum.map(images, & &1.x) == [-23.0, 12.0, 47.0, 82.0, 117.0]
    assert Enum.all?(images, &(&1.clip.width == 124))
  end

  defp layout(html, opts \\ []) do
    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, Keyword.put(opts, :system_font_discovery, false))
    assert {:ok, layout} = Layout.layout(styled, page_size: {200, 100}, margin: 0)
    {styled, layout}
  end

  test "generated cover sizing and plain inline images retain clipping and nested fixed scopes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.CssBackground

    for size <- ["cover", "contain"] do
      {_, plan} =
        layout(
          "<div style='width:40pt;height:20pt;background-image:image(red);background-size:#{size}'></div>"
        )

      assert [%{width: 40.0, height: 20.0}] = Enum.filter(plan.boxes, &(&1.type == :image))
    end

    assert CssBackground.layered?(%{background_attachments: [:fixed]})
    png = PngFixture.build(2, 2, 2, 8, 0, fn _, _ -> [0, 255, 0] end)

    {_, plan} =
      layout("<p><span style='background-image:url(tile)'>Plain image</span></p>",
        assets: %{"tile" => {:bytes, png}}
      )

    assert Enum.any?(plan.boxes, &(&1.type == :image))

    assert {:ok, dom} =
             HtmlParser.parse(
               "<div style='width:20pt;height:20pt;background-image:image(red);background-attachment:fixed'></div>"
             )

    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)

    assert {:ok, outer} =
             CssBackground.with_layout(
               fn ->
                 {:ok, plan} = Layout.layout(styled, page_size: {50, 50}, margin: 0)
                 # Mark the containing scope before a nested viewport enters its own scope.
                 CssBackground.boxes(hd(styled.children).style, %{
                   type: :rect,
                   x: 0,
                   y: 0,
                   width: 20,
                   height: 20,
                   border_radius: 0,
                   fill_color: nil,
                   stroke_color: nil,
                   stroke_width: 0,
                   border_widths: %{top: 0, right: 0, bottom: 0, left: 0}
                 })

                 assert {:ok, _} = Layout.layout(styled, page_size: {30, 30}, margin: 0)
                 {:ok, plan}
               end,
               nil
             )

    assert outer.css_fixed_backgrounds
    [fixed] = Enum.filter(outer.boxes, &Map.has_key?(&1, :css_fixed_background))

    assert [%{boxes: []}] =
             CssBackground.resolve_fixed_pages([
               %{size: {50, 50}, boxes: [%{fixed | width: 0, height: 0}]}
             ])
  end

  test "inline gradients preserve scalar corners and empty clip fragments" do
    for css <- [
          "border-radius:4pt",
          "border-radius:0 0 4pt 0 / 0 0 2pt 0",
          "border-radius:calc(4000000000%) / 2pt"
        ] do
      html =
        "<p style='width:80pt;margin:0'><span style='background:linear-gradient(red,blue);border:1pt solid black;#{css}'>One two three four five six seven eight</span></p>"

      if String.contains?(css, "calc") do
        assert {:error, {_, %{stage: :layout}}} =
                 HtmlToPdf.render(html, system_font_discovery: false)
      else
        assert {:ok, _} = HtmlToPdf.render(html, system_font_discovery: false)
      end
    end
  end
end
