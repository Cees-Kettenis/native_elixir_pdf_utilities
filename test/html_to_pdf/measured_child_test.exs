defmodule NativeElixirPdfUtilities.HtmlToPdf.MeasuredChildTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  defp measured(html, constraints \\ %{}) do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      with {:ok, dom} <- HtmlParser.parse_detailed(html),
           {:ok, %{children: [node]}} <- Style.compute_detailed(dom, [], context.image_budget) do
        before = ResourceContext.current()
        result = Layout.measure_child(node, context, constraints)
        assert ResourceContext.current() == before
        result
      end
    end)
  end

  test "inline child paint retains backgrounds, decorations, links and original Unicode" do
    assert {:ok, child} =
             measured(
               "<span style='background:yellow;font-size:12pt;line-height:30pt'>A <a href='https://example.test/' style='color:red'>café</a></span>"
             )

    assert child.alphabetic_baseline == 0
    assert child.ascent + child.descent == 30
    assert child.min_content_width <= child.max_content_width
    assert Enum.any?(child.boxes, &(&1.type == :rect))
    text = Enum.filter(child.boxes, &(&1.type == :text))
    assert Enum.any?(text, &String.contains?(&1.text, "café"))
    assert Enum.any?(text, &(Map.get(&1, :link_url) == "https://example.test/"))
    assert Enum.all?(text, &Map.has_key?(&1, :font_face))
    assert {:ok, plain} = measured("<span style='font-size:12pt;line-height:30pt'>Ink</span>")
    assert plain.ink_ascent != plain.ascent or plain.ink_descent != plain.descent
  end

  test "forced breaks and intrinsic unbroken widths retain native text layout" do
    assert {:ok, child} =
             measured("<span style='font-size:12pt;line-height:18pt'>Alpha<br>Beta Gamma</span>")

    assert child.descent > 18
    assert child.forced_breaks != []
    assert child.min_content_width < child.max_content_width
    assert {:ok, wrapped} = measured("<span>Alpha Beta Gamma</span>", %{available_width: 40})
    # An unbreakable word may exceed the available width without clipping.
    assert wrapped.width <= wrapped.max_content_width
    assert wrapped.descent > 0

    assert {:ok, unbroken} =
             measured("<span style='white-space:nowrap'>Unbroken content</span>", %{
               available_width: 20
             })

    assert unbroken.width > 20
    refute Enum.any?(unbroken.boxes, &Map.has_key?(&1, :clip))
  end

  test "prepared math retains preferred widths independently of its advance and baseline" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      html = """
      <math style="font-family:'STIX Two Math';font-size:20pt"><mo>(</mo><mspace width="4pt" height="28pt" depth="8pt"/><mo>)</mo></math>
      """

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [atom]}} = Style.compute_detailed(dom, [], context.image_budget)
      geometry = atom.style._math_box
      assert geometry.min_content_width > geometry.width
      assert geometry.max_content_width == geometry.min_content_width

      for constraints <- [%{}, %{available_width: 1}] do
        assert {:ok, child} = Layout.measure_child(atom, context, constraints)
        assert_in_delta child.width, geometry.width, 1.0e-8
        assert_in_delta child.min_content_width, geometry.min_content_width, 1.0e-8
        assert_in_delta child.max_content_width, geometry.max_content_width, 1.0e-8
        assert_in_delta child.ascent, geometry.ascent, 1.0e-8
        assert_in_delta child.descent, geometry.descent, 1.0e-8
        assert_in_delta child.ink_ascent, geometry.ink_ascent, 1.0e-8
        assert_in_delta child.ink_descent, geometry.ink_descent, 1.0e-8
        assert child.first_alphabetic_baseline == 0
        assert [%{type: :glyph_group, actual_text: "()", boxes: glyphs}] = child.boxes

        for {glyph, measured} <- Enum.zip(glyphs, geometry.glyphs) do
          assert glyph.glyph_ids == [measured.glyph_id]
          assert glyph.font_face == measured.font_face
          assert_in_delta glyph.x, measured.x, 1.0e-8
          assert_in_delta glyph.y, measured.y, 1.0e-8
        end
      end
    end)
  end

  test "malformed prepared math intrinsic fields return validator diagnostics" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, dom} = HtmlParser.parse_detailed("<math><mn>1</mn></math>")
      assert {:ok, %{children: [atom]}} = Style.compute_detailed(dom, [], context.image_budget)
      geometry = atom.style._math_box

      for broken <- [
            nil,
            Map.delete(geometry, :min_content_width),
            Map.delete(geometry, :max_content_width),
            %{geometry | min_content_width: :invalid},
            %{geometry | max_content_width: nil},
            %{geometry | min_content_width: -1},
            %{geometry | max_content_width: geometry.min_content_width - 1},
            %{geometry | drawing_items: []}
          ] do
        node = %{atom | style: Map.put(atom.style, :_math_box, broken)}
        assert {:error, {:invalid_document, diagnostic}} = Layout.measure_child(node, context)
        assert diagnostic.reason == :invalid_document
        assert diagnostic.stage == :layout
        assert diagnostic.operation == :measure_math
        assert diagnostic.source == "math"
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.MathLayoutValidator
        assert diagnostic.message =~ "math"
      end
    end)
  end

  test "atomic child baseline follows content independently of the declared height" do
    assert {:ok, child} =
             measured(
               "<span style='display:inline-block;width:81pt;height:60pt;font-size:12pt;line-height:18pt;padding:2.25pt;border:0.75pt solid red'>First<br>Last</span>"
             )

    assert child.width == 87

    assert {:ok, constrained} =
             measured(
               "<span style='display:inline-block;width:81pt;height:60pt;font-size:12pt;line-height:18pt;padding:2.25pt;border:0.75pt solid red'>First<br>Last</span>",
               %{available_width: 40}
             )

    assert constrained.width == 87
    refute Enum.any?(constrained.boxes, &Map.has_key?(&1, :clip))
    assert child.ascent + child.descent == 66
    assert child.ascent < 60
    assert child.first_alphabetic_baseline == 18
    assert Enum.any?(child.boxes, &(&1.type == :rect))
    assert Enum.any?(child.boxes, &(Map.get(&1, :text) == "Last"))
  end

  test "empty atomic child and zero available width remain accounted for" do
    assert {:ok, child} =
             measured(
               "<span style='display:inline-block;width:21pt;height:6pt;margin-bottom:2.25pt'></span>"
             )

    assert child.ascent == 8.25 and child.descent == 0
    assert child.first_alphabetic_baseline == 0
    assert child.width == 21

    assert {:ok, fractional} =
             measured(
               "<span style='display:inline-block;width:20pt;height:5.5pt;margin-bottom:2.25pt'></span>"
             )

    # Fixed CSS layout precision is 1/64 pixel, with one pixel equal to .75pt.
    expected_height = Float.floor(5.5 / 0.75 * 64) / 64 * 0.75 + 2.25
    assert_in_delta fractional.ascent, expected_height, 1.0e-8
    assert fractional.descent == 0
    assert {:ok, empty} = measured("<span></span>", %{available_width: 0})
    assert empty.width == 0
    assert empty.boxes == []
  end

  test "image links retain their own border rectangle and bottom-edge baseline" do
    bytes =
      File.read!(
        Path.expand("../fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png", __DIR__)
      )

    html =
      "<span><a href='https://example.test/'><img src='data:image/png;base64,#{Base.encode64(bytes)}' style='width:21pt;height:12pt;padding:1.5pt;border:0.75pt solid black;object-fit:contain'></a></span>"

    assert {:ok, child} = measured(html)
    image = Enum.find(child.boxes, &(&1.type == :image))
    assert image.link_url == "https://example.test/"
    assert image.link_rect.width == 25.5
    assert image.link_rect.height == 16.5
    assert image.link_rect.width > image.width or image.link_rect.height > image.height
    assert image.link_rect.y >= 0

    assert {:ok, block} =
             measured(
               "<img src='data:image/png;base64,#{Base.encode64(bytes)}' style='display:block;width:21pt;height:12pt'>"
             )

    assert block.ascent == 12 and block.descent == 0
    assert hd(block.boxes).type == :image

    assert {:ok, percentage} =
             measured(
               "<img src='data:image/png;base64,#{Base.encode64(bytes)}' style='width:50%;height:12pt'>",
               %{available_width: 80}
             )

    assert percentage.width == 40
    # The existing nil-width intrinsic pass caches the image's natural
    # percentage width independently of its constrained painting width.
    assert percentage.max_content_width == 48
    assert hd(percentage.boxes).width == 40
  end

  test "font fallback uses the caller's requested policy" do
    assert {:error, {:unsupported_glyph, diagnostic}} =
             measured("<span>𐍈</span>", %{unsupported_glyphs: :error})

    assert diagnostic.reason == :unsupported_glyph

    assert {:error, {:invalid_options, _}} =
             measured("<span>Text</span>", %{unsupported_glyphs: :invalid})
  end

  test "outline ink validation diagnoses malformed text transport and budgets" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font
    alias NativeElixirPdfUtilities.Validators.FontValidator
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    box = %{text: "A", font_face: font, font_size: 12, y: 0.0}
    assert {:ok, {top, bottom}} = FontValidator.prepare_text_ink_bounds(box)
    assert top > bottom
    assert {:ok, nil} = FontValidator.prepare_text_ink_bounds(%{box | text: " "})

    assert {:error, {_, diagnostic}} =
             FontValidator.prepare_text_ink_bounds(%{box | font_face: %{font | data: <<0>>}})

    assert diagnostic.stage == :font

    for invalid <- [
          nil,
          %{},
          %{box | text: <<255>>},
          Map.put(box, :glyph_ids, [0]),
          Map.put(box, :glyph_ids, nil),
          Map.put(box, :glyph_ids, []),
          %{box | font_size: -1}
        ] do
      assert {:error, {_, diagnostic}} = FontValidator.prepare_text_ink_bounds(invalid)
      assert diagnostic.stage == :font and diagnostic.operation == :measure_child
    end

    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)
    NativeElixirPdfUtilities.Limits.install(%{limits | max_font_math_work: 1})
    assert {:error, {:resource_limit_exceeded, _}} = FontValidator.prepare_text_ink_bounds(box)
  end

  test "block, flex and grid children use their existing layout and atomic baselines" do
    for display <- ["block", "flex", "grid"] do
      assert {:ok, child} =
               measured(
                 "<div style='display:#{display};width:60pt;background:#eee'><span>First</span><span>Last</span></div>"
               )

      assert child.width == 60
      assert child.boxes != []
      assert child.first_alphabetic_baseline >= 0
    end

    assert {:ok, hidden} = measured("<span style='display:none'>Hidden</span>")
    assert hidden.boxes == [] and hidden.width == 0
    assert hidden.ascent == 0 and hidden.descent == 0
    assert {:ok, blank} = measured("<span style='white-space:pre'>   </span>")
    assert blank.ink_ascent == 0 and blank.ink_descent == 0
    assert {:ok, broken} = measured("<br>")
    assert broken.forced_breaks != []
  end

  test "anonymous block descendants retain native line flow and the first baseline" do
    assert {:ok, child} =
             measured(
               "<div style='display:inline'>Before<div style='margin:0'>Middle</div>After</div>"
             )

    assert child.first_alphabetic_baseline == 0
    assert child.descent > 20

    assert Enum.map(Enum.filter(child.boxes, &(&1.type == :text)), & &1.text) == [
             "Before",
             "Middle",
             "After"
           ]

    assert child.min_content_width <= child.max_content_width
  end

  test "bounded embedded artwork remains clipped and uses its issued child scope" do
    assert {:ok, child} =
             measured(
               ~s|<iframe style='width:60pt;height:30pt;border:0' srcdoc="<body style='margin:0'><span>Child</span></body>"></iframe>|
             )

    [group] = Enum.filter(child.boxes, &(&1.type == :drawing_group))
    assert group.clip.width == 60 and group.clip.height == 30
    assert group.transform == {1, 0, 0, 1, 0.0, 0.0}
  end

  test "math-auto in painted children preserves original nodes and selected font glyphs" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn _ ->
      {:ok, dom} =
        HtmlParser.parse_detailed(
          "<math><mi style='display:inline-block'>f<span style='background:yellow'>i</span></mi></math>"
        )

      {:ok, %{children: [atom]}} = Style.compute_detailed(dom)

      texts =
        for %{kind: :native, box: %{type: :glyph_group, boxes: boxes}} <-
              atom.style._math_box.drawing_items,
            box <- boxes,
            do: box

      assert Enum.map(texts, & &1.text) == ["𝑓", "𝑖"]

      assert Enum.map(texts, &Map.fetch!(&1.font_face.cmap, hd(String.to_charlist(&1.text)))) == [
               3331,
               3334
             ]
    end)
  end

  test "validated native group transport preserves optional clips and logical metadata" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      {:ok, dom} = HtmlParser.parse_detailed("<span>Seed</span>")
      {:ok, %{children: [node]}} = Style.compute_detailed(dom)

      text = %{
        type: :text,
        text: "café",
        font_face: node.style.font_face,
        font: NativeElixirPdfUtilities.HtmlToPdf.Font.pdf_name(node.style.font_face),
        font_size: 12,
        color: {0, 0, 0},
        x: 4.0,
        y: 8.0,
        width: 25.0,
        line_height: 15.0,
        link_url: "https://example.test/",
        link_rect: %{x: 4.0, y: 2.0, width: 25.0, height: 15.0}
      }

      group = %{
        type: :drawing_group,
        x: 20.0,
        y: 30.0,
        width: 50.0,
        height: 30.0,
        baseline: 10.0,
        transform: {2, 0, 0, 2, 20, 30},
        boxes: [text],
        html_targets: [%{identity: :target, dx: 3.0, dy: 5.0}]
      }

      geometry = %{
        width: 120.0,
        min_content_width: 120.0,
        max_content_width: 120.0,
        ascent: 90.0,
        descent: 0.0,
        ink_ascent: 90.0,
        ink_descent: 0.0,
        italic_correction: 0.0,
        glyphs: [],
        actual_text: "café",
        drawing_items: [%{kind: :native, box: group, x: 0.0, y: 0.0}]
      }

      atom = %{
        node
        | style: node.style |> Map.put(:display, :inline_block) |> Map.put(:_math_box, geometry),
          children: []
      }

      assert {:ok, child} = Layout.measure_child(atom, context)
      [paint] = child.boxes
      assert paint.transform == group.transform and not Map.has_key?(paint, :clip)
      assert paint.html_targets == group.html_targets
      assert hd(paint.boxes).link_rect == text.link_rect
      assert child.ink_ascent > 46 and child.ink_descent < 0
      broken_font = %{text.font_face | data: <<0>>}
      broken_group = %{group | boxes: [%{text | font_face: broken_font}]}

      broken_geometry = %{
        geometry
        | drawing_items: [%{kind: :native, box: broken_group, x: 0.0, y: 0.0}]
      }

      assert {:error, {_, diagnostic}} =
               Layout.measure_child(
                 %{atom | style: Map.put(atom.style, :_math_box, broken_geometry)},
                 context
               )

      assert diagnostic.stage == :font
      clipped = Map.put(group, :clip, %{x: 0, y: 0, width: 50, height: 2})
      geometry = %{geometry | drawing_items: [%{kind: :native, box: clipped, x: 0.0, y: 0.0}]}

      assert {:ok, clipped_child} =
               Layout.measure_child(
                 %{atom | style: Map.put(atom.style, :_math_box, geometry)},
                 context
               )

      assert clipped_child.ink_ascent == 0 and clipped_child.ink_descent == 0
      zero = %{group | width: 0.0}
      geometry = %{geometry | drawing_items: [%{kind: :native, box: zero, x: 0.0, y: 0.0}]}

      assert {:ok, zero_child} =
               Layout.measure_child(
                 %{atom | style: Map.put(atom.style, :_math_box, geometry)},
                 context
               )

      assert zero_child.ink_ascent == 0 and zero_child.ink_descent == 0
      bad = %{group | transform: {1, 1, 0, 1, 0, 0}}
      geometry = %{geometry | drawing_items: [%{kind: :native, box: bad, x: 0.0, y: 0.0}]}

      assert {:error, {:invalid_pdf_input, diagnostic}} =
               Layout.measure_child(
                 %{atom | style: Map.put(atom.style, :_math_box, geometry)},
                 context
               )

      assert diagnostic.stage == :layout and diagnostic.message =~ "skews"

      assert {:error, {:invalid_layout, _}} =
               HtmlValidator.validate_measured_child_paint(nil, node)

      assert {:error, {:invalid_layout, _}} =
               HtmlValidator.validate_measured_child_paint([%{type: :layout_error}], node)

      assert {:error, {:invalid_pdf_input, _}} =
               HtmlValidator.validate_measured_child_paint([%{type: :unknown}], node)

      assert {:error, {_, _}} =
               NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes(nil)

      assert {:error, {_, diagnostic}} =
               HtmlValidator.validate_measured_child_paint([%{type: :unknown}], nil)

      assert diagnostic.source == "HTML child"
    end)
  end

  test "measurement catches font budgets at its own API boundary without resetting the scope" do
    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      {:ok, dom} = HtmlParser.parse_detailed("<span>Budget</span>")
      {:ok, %{children: [node]}} = Style.compute_detailed(dom)
      NativeElixirPdfUtilities.Limits.install(%{limits | max_font_math_work: 1})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               Layout.measure_child(node, context)

      assert diagnostic.stage == :font
      assert ResourceContext.current() == context
      send(self(), :returned_after_child_budget)
    end)

    assert_received :returned_after_child_budget
  end

  test "native paint validation returns resource diagnostics outside a document scope" do
    limits = NativeElixirPdfUtilities.Limits.effective()
    on_exit(fn -> NativeElixirPdfUtilities.Limits.install(limits) end)
    NativeElixirPdfUtilities.Limits.install(%{limits | max_layout_boxes: 0})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.Validators.WriterValidator.validate_native_boxes([
               %{type: :unknown}
             ])

    assert diagnostic.stage == :writer
  end

  test "invalid context, styled input and measurement constraints return diagnostics" do
    assert {:error, {_, diagnostic}} = Layout.measure_child(%{}, nil, %{})
    assert diagnostic.stage == :resource

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:error, {:invalid_layout, diagnostic}} = Layout.measure_child(nil, context)
      assert diagnostic.operation == :measure_child
      {:ok, dom} = HtmlParser.parse_detailed("<span>Text</span>")
      {:ok, %{children: [node]}} = Style.compute_detailed(dom)

      for constraints <- [nil, %{available_width: -1}, %{available_height: :auto}, %{unknown: 1}] do
        assert {:error, {:invalid_layout, diagnostic}} =
                 Layout.measure_child(node, context, constraints)

        assert diagnostic.stage == :layout and diagnostic.source == "span"
      end

      assert {:error, {:invalid_layout, _}} =
               Layout.measure_child(%{node | children: [nil]}, context)

      invalid_break = %{node | style: Map.put(node.style, :display, :soft_break)}

      assert {:error, {:invalid_layout, diagnostic}} =
               Layout.measure_child(invalid_break, context)

      assert diagnostic.message =~ "wbr"

      invalid_decoration = %{node | style: Map.put(node.style, :_text_decorations, :invalid)}
      assert {:error, {:invalid_layout, _}} = Layout.measure_child(invalid_decoration, context)

      block_image = %{
        node
        | children: [
            %{
              type: :element,
              tag: "img",
              style: Map.merge(node.style, %{display: :image, image_display: :block}),
              children: []
            }
          ]
      }

      assert {:error, {_, _}} = Layout.measure_child(block_image, context)

      image_bytes =
        File.read!(
          Path.expand("../fixtures/html_to_pdf/browser_parity/assets/iframe-bars.png", __DIR__)
        )

      {:ok, image_dom} =
        HtmlParser.parse_detailed(
          "<img src='data:image/png;base64,#{Base.encode64(image_bytes)}' style='vertical-align:middle'>"
        )

      {:ok, %{children: [image]}} = Style.compute_detailed(image_dom)
      corrupt_face = %{image.style.font_face | data: <<0>>}

      assert {:error, {_, diagnostic}} =
               Layout.measure_child(
                 %{image | style: %{image.style | font_face: corrupt_face}},
                 context
               )

      assert diagnostic.stage == :font
      assert ResourceContext.current() == context
      invalid = %{node | style: %{node.style | display: :table}}
      assert {:error, {:unsupported_html, diagnostic}} = Layout.measure_child(invalid, context)
      assert diagnostic.message =~ "shared layout adapter"
    end)
  end
end
