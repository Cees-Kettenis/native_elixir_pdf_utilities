defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgForeignObjectTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Validators.SvgFormSlotValidator

  test "native HTML drawing forms paint at their scene positions before ancestor vector effects" do
    html =
      File.read!(
        Path.expand(
          "../fixtures/html_to_pdf/browser_parity/inline_svg_foreign_object.html",
          __DIR__
        )
      )

    assert {:ok, pdf} = HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Image"
    assert pdf =~ "/NativeHtmlSurface"
    assert pdf =~ "/SMask"
    assert pdf =~ "/ca 0.6"
    assert {:ok, text} = Text.extract(pdf, layout: false)

    for original <- [
          "selected café Ω",
          "Two native lines",
          "Rotated HTML",
          "Genuine glyphs",
          "Masked native HTML",
          "Partial selection"
        ] do
      assert text =~ original
    end
  end

  test "HTML namespaces remain genuine and unsupported declarations diagnose through HTML input authority" do
    for namespace <- ["urn:other", "javascript:run()"] do
      html =
        "<svg><foreignObject width='100' height='40'><div xmlns='#{namespace}'>Text</div></foreignObject></svg>"

      assert {:error, {:unsupported_html, %{stage: :html, source: source, message: message}}} =
               HtmlToPdf.render(html)

      assert source =~ "xmlns"
      assert message != ""
    end
  end

  test "selected HTML byte fonts survive a foreignObject-only scene" do
    html =
      "<svg width='200' height='50'><foreignObject x='5' y='5' width='190' height='40'><div style='font-family:DejaVu Sans;font-size:14px'>Native café Ω</div></foreignObject></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text == "Native café Ω"
    assert pdf =~ "/FontFile2"
    refute pdf =~ "/Subtype /Image"
  end

  test "HTML URI links use genuine form-painted content and retain clipped scene geometry" do
    html =
      "<svg width='200' height='80'><foreignObject x='10' y='10' width='100' height='50'><div style='font-family:DejaVu Sans;font-size:14px'><a href='https://example.test/native'>Native link café</a></div></foreignObject></svg>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: "200pt 100pt", margin: 0)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text == "Native link café"
    refute pdf =~ "/Subtype /Image"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    links =
      document.objects
      |> Map.values()
      |> Enum.filter(&(is_map(&1.value) and &1.value["Subtype"] == {:name, "Link"}))

    assert links != []

    for link <- links do
      assert link.value["A"]["URI"] == {:string, "https://example.test/native"}
      [left, bottom, right, top] = link.value["Rect"]
      assert left >= 7.5 and right <= 82.5
      assert top <= 92.5 and bottom >= 55
    end
  end

  test "annotation-only writer regions reject malformed coordinates and nonnavigation links" do
    alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter

    base = %{
      type: :link_region,
      x: 1,
      y: 1,
      width: 20,
      height: 10,
      link_url: "https://example.test/native"
    }

    for region <- [
          Map.put(base, :width, -1),
          Map.put(base, :x, 1.0e100),
          Map.put(base, :link_url, "javascript:run()"),
          Map.put(base, :text, "Invisible surrogate")
        ] do
      assert {:error, {reason, %{stage: stage, reason: diagnostic_reason, message: message}}} =
               PdfWriter.render([%{size: {100, 50}, boxes: [region]}])

      assert reason == diagnostic_reason
      assert stage == :pdf
      assert message != ""
    end
  end

  test "foreign scene cache hits retain genuine Forms links and selected Unicode and charge real layout" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext, Style, SvgScene}
    alias NativeElixirPdfUtilities.Validators.HtmlValidator
    alias NativeElixirPdfUtilities.Limits
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    # Foreign Forms use vector preparation and never need a scene raster probe.
    Limits.install(Map.put(limits, :max_aggregate_svg_surface_pixels, 1))

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<svg width=160 height=40><foreignObject width=160 height=40><div><a href='https://example.test/cache'>Cached café Ω</a></div></foreignObject></svg>"
               )

      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, [], context.image_budget)
      scene = node.style.svg_scene
      assert {:ok, first} = SvgScene.paint(scene, 120, 30)
      cached = Process.get(context.scope)

      assert Enum.count(cached, fn {key, _} ->
               is_tuple(key) and elem(key, 0) == :svg_scene_paint
             end) == 1

      used = Process.get({HtmlValidator, :render_budget})[:max_layout_boxes]
      assert {:ok, ^first} = SvgScene.paint(scene, 120, 30)
      assert Process.get(context.scope) == cached
      assert Process.get({HtmlValidator, :render_budget})[:max_layout_boxes] > used
      assert {:ok, text} = Text.extract(first.image.data, layout: false)
      assert text == "Cached café Ω"
      assert first.links != []
      assert first.image.data =~ "/NativeHtmlSurface"
      refute first.image.data =~ "/Subtype /Image"

      [{id, slot}] = Map.to_list(scene.foreign_slots)
      forged = put_in(scene.foreign_slots[id].context.authorization_root, "/unissued")

      assert {:error, {_, %{stage: :resource, message: message}}} =
               SvgScene.paint(forged, 120, 30)

      assert message =~ "immutable scope"

      for replacement <- [
            %{slot | forms: :static},
            %{slot | document: %{type: :document, children: []}},
            %{slot | permission: make_ref()}
          ] do
        changed = %{scene | foreign_slots: %{id => replacement}}

        assert {:error, {:invalid_document, %{message: message, operation: :prepare}}} =
                 SvgScene.paint(changed, 120, 30)

        assert message =~ "unchanged issued"
      end

      assert {:error, {:invalid_document, %{source: "replaced-id"}}} =
               SvgScene.paint(%{scene | foreign_slots: %{"replaced-id" => slot}}, 120, 30)

      assert {:ok, %{children: [fresh]}} =
               Style.compute_detailed(dom, [forms: :static], context.image_budget)

      assert {:ok, _} = SvgScene.paint(fresh.style.svg_scene, 120, 30)

      assert Enum.count(Process.get(context.scope), fn {key, _} ->
               is_tuple(key) and elem(key, 0) == :svg_scene_paint
             end) == 2

      used = Process.get({HtmlValidator, :render_budget})[:max_layout_boxes]
      Limits.install(Map.put(limits, :max_layout_boxes, used))

      assert {:error, {:resource_limit_exceeded, %{message: message}}} =
               HtmlValidator.with_render_budget(fn -> SvgScene.paint(scene, 120, 30) end)

      assert message =~ "max_layout_boxes"
    end)
  end

  test "freshly issued HTML byte font and viewport identities miss the painted Form cache" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext, Style, SvgScene}
    alias NativeElixirPdfUtilities.Validators.HtmlValidator
    font_dir = Path.expand("../../priv/fonts/dejavu", __DIR__)
    regular = File.read!(Path.join(font_dir, "DejaVuSans.ttf"))
    bold = File.read!(Path.join(font_dir, "DejaVuSans-Bold.ttf"))

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      html =
        "<svg width='160' height='40'><foreignObject width='160' height='40'><div style='font-family:Issued'>Identity café Ω</div></foreignObject></svg>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)

      scenes =
        Enum.map([regular, bold], fn bytes ->
          assert {:ok, %{children: [node]}} =
                   Style.compute_detailed(
                     dom,
                     [
                       fonts: [%{family: "Issued", data: bytes}],
                       default_font: "Issued",
                       system_font_discovery: false
                     ],
                     context.image_budget
                   )

          scene = node.style.svg_scene
          [{_, slot}] = Map.to_list(scene.foreign_slots)
          [div] = slot.document.children
          assert div.style.font_face.data == bytes
          scene
        end)

      [regular_scene, bold_scene] = scenes
      assert {:ok, regular_form} = SvgScene.paint(regular_scene, 90, 22.5)
      assert {:ok, ^regular_form} = SvgScene.paint(regular_scene, 90, 22.5)
      assert {:ok, bold_form} = SvgScene.paint(bold_scene, 90, 22.5)
      refute regular_form.image.data == bold_form.image.data

      flexible = %{
        regular_scene
        | attributes: %{},
          style:
            Map.merge(regular_scene.style, %{
              svg_width: {:percent, 1.0},
              svg_height: {:percent, 1.0}
            })
      }

      assert {:ok, enlarged} = SvgScene.paint(flexible, 180, 45)
      refute regular_form.image.data == enlarged.image.data
      assert {:ok, ^enlarged} = SvgScene.paint(flexible, 180, 45)

      for form <- [regular_form, bold_form, enlarged] do
        assert {:ok, text} = Text.extract(form.image.data, layout: false)
        assert text == "Identity café Ω"
      end

      assert Enum.count(Process.get(context.scope), fn {key, _} ->
               is_tuple(key) and elem(key, 0) == :svg_scene_paint
             end) == 3
    end)
  end

  test "native slots cannot be composed with an unissued HTML subtree" do
    assert {:module, _} = NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.load_native()

    assert {:ok, source} =
             NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.vectorize_slots(
               "<svg width='100' height='40'><rect id='slot' width='100' height='40'/></svg>",
               [],
               ["slot"]
             )

    assert {:error, {:invalid_document, %{source: "slot", operation: :compose}}} =
             SvgFormSlotValidator.prepare(source, %{})
  end
end
