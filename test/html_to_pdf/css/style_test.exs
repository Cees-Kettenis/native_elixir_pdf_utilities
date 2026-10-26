alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.StyleTest do
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.CounterStyle
  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  use ExUnit.Case

  import NativeElixirPdfUtilities.HtmlTreeHelpers

  test "processing instructions neither paint nor affect element sibling selectors and counters" do
    NativeElixirPdfUtilities.Validators.HtmlRecoveryValidator.with_options(
      [html_recovery: true],
      fn ->
        alias NativeElixirPdfUtilities.HtmlToPdf.Style

        html =
          "<style>p{counter-increment:item}p::before{content:counter(item)}p:first-child{color:red}p:nth-child(2){color:blue}p:last-child{font-weight:bold}</style><div><?marker><p>First</p><?start><p>Second</p><?end></div>"

        assert {:ok, dom} = HtmlParser.parse_detailed(html)
        assert {:ok, %{children: [%{children: [first, second]}]}} = Style.compute_detailed(dom)
        assert first.style.color == {1.0, 0.0, 0.0}
        assert second.style.color == {0.0, 0.0, 1.0}
        assert second.style.font_weight == 700
        assert hd(hd(first.children).children).text == "1"
        assert hd(hd(second.children).children).text == "2"

        assert {:ok, dom} = HtmlParser.parse_detailed("<?marker name='unused.png'?>")
        assert {:ok, %{children: []}} = Style.compute_detailed(dom)

        assert {:ok, dom} =
                 HtmlParser.parse_detailed(
                   "<ul><?marker><li>One</li><?end><li>Two</li></ul><select><?marker><option>A<?end><option selected>B</select>"
                 )

        assert {:ok, %{children: [list, select]}} = Style.compute_detailed(dom)
        assert length(list.children) == 2
        assert select.children == [%{type: :text, text: "B", style: hd(select.children).style}]
      end
    )
  end

  test "direct processing instruction nodes validate before nonpainting selection" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    marker = %{type: :processing_instruction, target: "marker", data: "name=A"}
    assert {:ok, %{children: []}} = Style.compute_detailed(%{type: :document, children: [marker]})

    for extra <- [
          %{children: [%{type: :element, tag: "script", attributes: %{}, children: []}]},
          %{attributes: %{"onclick" => "run()"}},
          %{tag: "img"},
          %{data: "x>y"},
          %{source: %{text: <<255>>, line: 1, column: 1}}
        ] do
      invalid = Map.merge(marker, extra)

      for children <- [
            [invalid],
            [
              %{
                type: :element,
                tag: "template",
                attributes: %{},
                children: [],
                content: %{type: :document_fragment, children: [invalid]}
              }
            ]
          ] do
        assert {:error, {:invalid_document, diagnostic}} =
                 Style.compute_detailed(%{type: :document, children: children})

        assert diagnostic.stage == :style and diagnostic.source == "processing_instruction"
      end
    end

    for tag <- ~w(style title textarea script xmp plaintext noembed) do
      attrs = if tag == "script", do: %{"type" => "text/plain"}, else: %{}

      dom = %{
        type: :document,
        children: [%{type: :element, tag: tag, attributes: attrs, children: [marker]}]
      }

      assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(dom)
      assert diagnostic.stage == :style
      assert diagnostic.source == tag
      assert diagnostic.message =~ "text-only"
    end

    for tag <- ~w(input br wbr img col meta hr) do
      dom = %{
        type: :document,
        children: [%{type: :element, tag: tag, attributes: %{}, children: [marker]}]
      }

      assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(dom)
      assert diagnostic.stage == :style and diagnostic.source == tag
      assert diagnostic.message =~ "empty"
    end

    foreign = %{
      type: :element,
      tag: "style",
      namespace: :svg,
      attributes: %{},
      children: [marker]
    }

    template = %{
      type: :element,
      tag: "template",
      attributes: %{},
      children: [],
      content: %{type: :document_fragment, children: [foreign]}
    }

    assert {:ok, _} = Style.compute_detailed(%{type: :document, children: [template]})
  end

  test "xmp and plaintext use preformatted defaults without dropping their first newline" do
    NativeElixirPdfUtilities.Validators.HtmlRecoveryValidator.with_options(
      [html_recovery: true],
      fn ->
        alias NativeElixirPdfUtilities.HtmlToPdf.Style

        for tag <- ~w(xmp plaintext) do
          suffix = if tag == "xmp", do: "</xmp></div>", else: ""

          assert {:ok, dom} =
                   HtmlParser.parse_detailed(
                     "<div style='font-size:15pt;font-weight:bold'><#{tag}>\r\n  é &amp;\rNext" <>
                       suffix
                   )

          assert {:ok, %{children: [%{children: [node]}]}} = Style.compute_detailed(dom)
          assert node.style.display == :block and node.style.white_space == :pre
          assert node.style.font_face.family == "DejaVu Sans Mono"
          assert node.style.font_size == 15.0 and node.style.font_weight == 700
          assert node.style.margin.top == 15.0 and node.style.margin.bottom == 15.0
          assert [%{text: "\n  é &amp;\nNext"}] = node.children

          html =
            "<#{tag} style='font-family:DejaVu Sans;white-space:normal;margin:0;font-size:9pt'>é &amp;" <>
              if(tag == "xmp", do: "</xmp>", else: "")

          assert {:ok, dom} = HtmlParser.parse_detailed(html)
          assert {:ok, %{children: [overridden]}} = Style.compute_detailed(dom)
          assert overridden.style.white_space == :normal and overridden.style.font_size == 9.0
          assert overridden.style.font_face.family == "DejaVu Sans"
          assert overridden.style.margin == %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
        end
      end
    )
  end

  test "noembed stays raw and produces no box even with authored display overrides" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<style>noembed.shown{display:block!important;color:red;background-image:url(unused-override.png)}</style><noembed style='background-image:url(unused.png)'><img src=denied.png><style>p{color:blue}</style></noembed><noembed class=shown>é &amp; <b>Literal</b></noembed><p>After</p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [hidden, shown, after_node]}} =
             Style.compute_detailed(dom,
               asset_resolver: fn request ->
                 flunk("literal resource resolved #{inspect(request)}")
               end
             )

    assert hidden.tag == "noembed" and hidden.style.display == :none and hidden.children == []
    refute Map.has_key?(hidden.style, :background_image)
    refute Map.has_key?(hidden.style, :background_image_source)
    assert shown.tag == "noembed" and shown.style.display == :none and shown.children == []
    assert shown.style.color == {1, 0, 0} and shown.style.white_space == :normal
    refute Map.has_key?(shown.style, :background_image_source)
    assert [%{text: "é &amp; <b>Literal</b>"}] = Enum.at(dom.children, 2).children
    assert after_node.style.color == {0, 0, 0}
  end

  test "indeterminate selector consumes existing radio ownership and never guesses checkbox state" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<style>:indeterminate{color:red}</style><input type=radio name=a><input type=radio name=a><input type=radio name=b checked><input type=radio name=b><input type=radio><input type=radio checked><input type=checkbox><form><input type=radio name=b></form>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: nodes}} = Style.compute_detailed(dom)

    assert Enum.map(Enum.take(nodes, 7), & &1.style.color) == [
             {1, 0, 0},
             {1, 0, 0},
             {0, 0, 0},
             {0, 0, 0},
             {1, 0, 0},
             {0, 0, 0},
             {0, 0, 0}
           ]

    assert hd(List.last(nodes).children).style.color == {1, 0, 0}
  end

  test "gauge styles consume prepared state and preserve authored selector attributes and fallback semantics" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<style>progress[value='.5']{font-size:20pt;width:90pt;background:red}progress:indeterminate{height:18pt}meter{appearance:none}meter:indeterminate{color:red}</style><progress value=.5><span>Fallback</span></progress><progress></progress><meter value=.5></meter>"
             )

    assert {:ok, %{children: [determinate, unknown, meter]}} = Style.compute_detailed(dom)
    assert determinate.style._gauge.position == 0.5
    assert determinate.style.width == 90 and determinate.style.height == 20
    assert determinate.style.vertical_align == -4
    assert determinate.style.background_color == {1, 0, 0}
    assert [%{tag: "span", children: [%{text: "Fallback"}]}] = determinate.style._gauge_fallback
    assert determinate.children == []
    assert unknown.style.height == 18 and unknown.style._gauge.position == -1
    assert meter.style.width == 60 and meter.style.appearance == :none
    refute meter.style.color == {1, 0, 0}
    refute Map.has_key?(meter.style, :_form_control)

    for value <- [".1", ".9", ".1"] do
      assert {:ok, dom} = HtmlParser.parse_detailed("<progress value=#{value}></progress>")

      forged =
        put_in(dom.children, [Map.put(hd(dom.children), :_initial_gauge_state, %{position: 99})])

      assert {:ok, %{children: [node]}} = Style.compute_detailed(forged)
      assert node.style._gauge.position == String.to_float("0" <> value)
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='accent-color:red'><progress value=.5></progress></div>"
             )

    assert {:ok, %{children: [%{children: [node]}]}} = Style.compute_detailed(dom)
    assert node.style.accent_color == {1, 0, 0}

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='accent-color:red'><progress value=.5 style='accent-color:AUTO;display:inline;width:48pt'></progress></div>"
             )

    assert {:ok, %{children: [%{children: [node]}]}} = Style.compute_detailed(dom)
    refute Map.has_key?(node.style, :accent_color)
    assert node.style._gauge_native
    assert node.style.display == :inline_block and node.style.width == 48
  end

  test "direct gauge trees must obey content models attributes and hidden executable policy" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    assert {:ok, dom} = HtmlParser.parse_detailed("<progress><span>Fallback</span></progress>")
    progress = hd(dom.children)

    for invalid <- [
          put_in(progress.children, [%{progress | children: []}]),
          put_in(progress.children, [%{hd(progress.children) | tag: "div"}])
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               Style.compute_detailed(%{dom | children: [invalid]})

      assert diagnostic.stage == :style and diagnostic.reason == :invalid_document
      assert is_binary(diagnostic.message) and is_binary(diagnostic.source)
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             Style.compute_detailed(%{
               dom
               | children: [
                   %{progress | attributes: %{"onclick" => "run()"}, attribute_order: ["onclick"]}
                 ]
             })

    assert diagnostic.reason == :unsupported_html and diagnostic.source == "progress"

    for css <- ["appearance:button", "accent-color:invalid", "vertical-align:invalid"] do
      assert {:ok, dom} = HtmlParser.parse_detailed("<meter style='#{css}'></meter>")
      assert {:error, _} = Style.compute_detailed(dom)
    end
  end

  test "editability selectors use own and inherited state without changing authored attributes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html = ~S"""
    <style>:read-only{color:red}:read-write{color:blue}
    .escape:\72 ead-write:not(:read-only){color:green}
    .escape{color:red}</style>
    <div contenteditable=TRUE><p>Inherited</p><div contenteditable='false'><p>Locked</p>
    <p contenteditable='plaintext-only'>New host</p></div><p contenteditable=' false '>Invalid inherits</p>
    <p class=escape>Escaped specificity</p><input name=locked readonly value=Readonly>
    <input name=active contenteditable=false value=Editable><textarea disabled>Disabled</textarea>
    <button disabled>Editable region button</button></div><p>Outside</p>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [host, outside]}} = Style.compute_detailed(dom)

    [inherited, locked, invalid, escaped, readonly, input, textarea, button] =
      Enum.filter(host.children, &(&1.type == :element))

    assert host.style.color == {0, 0, 1}
    assert inherited.style.color == {0, 0, 1}
    assert locked.style.color == {1, 0, 0}

    assert Enum.at(Enum.filter(locked.children, &(&1.type == :element)), 0).style.color ==
             {1, 0, 0}

    assert Enum.at(Enum.filter(locked.children, &(&1.type == :element)), 1).style.color ==
             {0, 0, 1}

    assert invalid.style.color == {0, 0, 1}
    assert {0, green, 0} = escaped.style.color
    assert_in_delta green, 128 / 255, 1.0e-10
    assert readonly.style.color == {1, 0, 0}
    assert input.style.color == {0, 0, 1}
    assert textarea.style.color == {1, 0, 0}
    assert button.style.color == {0, 0, 1}
    assert outside.style.color == {1, 0, 0}
    [authored_host | _] = Enum.filter(dom.children, &(&1.type == :element and &1.tag == "div"))
    assert authored_host.attributes["contenteditable"] == "TRUE"

    assert Enum.any?(
             authored_host.children,
             &(Map.get(&1, :attributes, %{})["contenteditable"] == " false ")
           )

    assert input.style._form_control.attributes["contenteditable"] == "false"
  end

  test "editing cache is isolated between computations and inert template trees" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    for editable <- [true, false, true] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<style>p:read-write{color:blue}p:read-only{color:red}</style><div contenteditable=#{editable}><p>Visible</p><template><div contenteditable=#{not editable}><p>Unused</p></div></template></div>"
               )

      assert {:ok, %{children: [%{children: [node | _]}]}} = Style.compute_detailed(dom)
      assert node.style.color == if(editable, do: {0, 0, 1}, else: {1, 0, 0})
      assert {:ok, %{children: [%{children: [again | _]}]}} = Style.compute_detailed(dom)
      assert again.style.color == node.style.color
    end
  end

  test "direct picture trees reject invalid ordering and source ownership before resolution" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    assert {:ok, %{children: [picture]}} =
             HtmlParser.parse_detailed("<picture><source srcset=tile><img src=tile></picture>")

    picture = tree_content(picture)
    [source, image] = picture.children

    for node <- [
          source,
          %{picture | children: [image, source]},
          %{picture | children: [image, image]}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               Style.compute_detailed(%{type: :document, children: [node]})

      assert diagnostic.stage == :style and diagnostic.message =~ "picture"
    end
  end

  test "responsive source dimensions and density correction precede author CSS without mutating cached decodes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    bytes =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(80, 40, 2, 8, 0, fn _, _ ->
        [10, 80, 200]
      end)

    html =
      "<style>picture>img{width:30pt;height:auto}source{display:block;background-image:url(denied)}</style>" <>
        "<picture style='display:block'><source type=image/png media=print srcset='tile 2x' width=100 height=50><img src=denied width=200 height=200></picture>" <>
        "<img srcset='tile 2x'><img src=tile>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [picture, corrected, ordinary]}} =
             Style.compute_detailed(dom, assets: %{"tile" => {:bytes, bytes}})

    [source, image] = picture.children
    assert source.style.display == :none
    refute Map.has_key?(source.style, :background_image_source)
    assert image.style.width == 30.0 and image.style.height == :auto
    assert image.style._image_selection.dimensions == %{width: 75.0, height: 37.5}
    assert image.style._image_selection.source == "source"
    assert image.style.image.width == 30.0 and image.style.image.height == 15.0
    assert corrected.style.image.width == 30.0 and ordinary.style.image.width == 60.0
    assert corrected.style.image.width_px == ordinary.style.image.width_px
    assert ordinary.style.image.width_px == 80 and ordinary.style.image.height_px == 40

    assert {:ok, %{children: [plain_picture]}} =
             Style.compute_detailed(%{dom | children: [Enum.at(dom.children, 1)]},
               assets: %{"tile" => {:bytes, bytes}}
             )

    assert plain_picture.style.display == :block
    assert List.last(plain_picture.children).style.image_display == :inline

    for source_dimensions <- ["", "width=100", "height=50"] do
      html =
        "<picture style='display:block'><source srcset=tile #{source_dimensions}><img src=denied width=200 height=200></picture>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)

      assert {:ok, %{children: [%{children: [_, image]}]}} =
               Style.compute_detailed(dom, assets: %{"tile" => {:bytes, bytes}})

      expected =
        case source_dimensions do
          "" -> %{width: 150.0, height: 150.0}
          "width=100" -> %{width: 75.0}
          "height=50" -> %{height: 37.5}
        end

      assert image.style._image_selection.dimensions == expected
    end

    svg =
      "<svg xmlns='http://www.w3.org/2000/svg' width='80' height='40'><rect width='80' height='40' fill='blue'/></svg>"

    data = "data:image/svg+xml;base64," <> Base.encode64(svg)
    assert {:ok, dom} = HtmlParser.parse_detailed("<img srcset='#{data} 2x'>")
    assert {:ok, %{children: [image]}} = Style.compute_detailed(dom)
    assert image.style.image.width == 30 and image.style.image.height == 15
    assert {:ok, dom} = HtmlParser.parse_detailed("<picture><img src=tile></picture>")

    assert {:ok, %{children: [image]}} =
             Style.compute_detailed(dom, assets: %{"tile" => {:bytes, bytes}})

    assert image.style.display == :inline
  end

  test "computed image display preserves replaced-image identity and inline or block intent" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    for {display, expected} <- [
          {"", :inline},
          {"display:inline", :inline},
          {"display:inline-block", :inline},
          {"display:block", :block}
        ] do
      assert {:ok, dom} =
               HtmlParser.parse(
                 "<body><a href='https://example.com'><img src='data:image/png;base64,#{Base.encode64(png_fixture(1, 1))}' style='#{display}'></a></body>"
               )

      assert {:ok, tree} = Style.compute(dom)
      [body] = tree.children
      [anchor] = body.children
      [image] = anchor.children
      assert image.style.display == :image
      assert image.style.image_display == expected
      assert image.style.link_url == "https://example.com"
    end
  end

  test "metadata selectors match authored values while translation mode inherits independently" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    html = """
    <style>[itemscope]{color:red}[itemtype='urn:example:Record']{font-weight:700}[itemprop~=name]{color:blue}[translate=' no ']{background-color:yellow}[translate=NO]{font-style:italic}</style>
    <div itemscope itemtype='urn:example:Record' translate=NO><span itemprop='name Name'>Inherited</span><x-box translate=' no ' arbitrary=unchanged>Padded</x-box><b translate>Enabled</b><i translate=YES>Case</i><u translate=invalid>Invalid</u></div>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [_style, parent] = HtmlValidator.assign_render_metadata(dom.children)
    refute parent._html_translate
    [inherited, padded, empty, yes, invalid] = parent.children
    refute inherited._html_translate or padded._html_translate or invalid._html_translate
    assert empty._html_translate and yes._html_translate
    assert parent.attributes["translate"] == "NO"
    refute HtmlValidator.attribute_selector_matches?({:equals, "translate", "no"}, parent)

    assert HtmlValidator.attribute_selector_matches?(
             {:equals, "translate", "no", :insensitive},
             parent
           )

    refute HtmlValidator.attribute_selector_matches?(
             {:equals, "itemtype", "urn:example:record"},
             parent
           )

    assert padded.attributes["translate"] == " no "
    assert padded.attributes["arbitrary"] == "unchanged"
    assert {:ok, %{children: [styled]}} = Style.compute_detailed(dom)
    assert styled.style.color == {1, 0, 0} and styled.style.font_weight == 700
    [inherited, padded | _] = styled.children
    assert inherited.style.color == {0, 0, 1}
    assert padded.style.background_color == {1, 1, 0}
  end

  test "translation inheritance crosses foreign hosts and resets at HTML integration points" do
    alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    assert {:ok, %{children: [parent]}} =
             HtmlParser.parse_detailed(
               "<div translate=no><svg translate=yes><foreignObject translate=yes><p translate=yes>HTML</p></foreignObject></svg><math translate=yes><mtext translate=yes><span translate>HTML</span></mtext></math></div>"
             )

    [parent] = HtmlValidator.assign_render_metadata([parent])
    refute parent._html_translate

    for foreign <- parent.children do
      refute foreign._html_translate
      [integration] = foreign.children
      refute integration._html_translate
      assert hd(integration.children)._html_translate
    end

    [forged] = HtmlValidator.assign_render_metadata([Map.put(parent, :_html_translate, true)])
    refute forged._html_translate
  end

  test "namespace math defaults and style hints precede authored CSS" do
    html = """
    <math display=block mathcolor=blue mathbackground=yellow mathsize=40pt
      displaystyle=false scriptlevel=2 style='font-size:20pt;color:red;math-depth:0;math-style:normal;math-shift:compact'>
      <mi mathvariant=normal style='text-transform:math-auto'>x</mi><mn>2</mn>
    </math>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom)
    [block] = styled.children
    [atom] = block.children
    assert block.style.display == :block
    assert block.style.math_style == :normal
    assert block.style.math_depth == 0
    assert block.style.math_shift == :compact
    assert block.style.font_size == 20.0
    assert block.style.color == {1, 0, 0}
    assert block.style.background_color == {1, 1, 0}
    assert atom.style.font_face.family == "STIX Two Math"
    [x, number] = atom.style._math_box.glyphs
    assert x.glyph_id == atom.style.font_face.cmap[0x1D465]
    assert number.glyph_id == atom.style.font_face.cmap[?2]
  end

  test "math depth uses selected-font scaling and explicit size resets" do
    for {attributes, size, _depth} <- [
          {"scriptlevel=1", 14.0, 1},
          {"scriptlevel=2", 11.0, 2},
          {"scriptlevel=+3", 7.81, 3},
          {"style='math-depth:auto-add'", 14.0, 1},
          {"scriptlevel=2 style='font-size:30pt'", 30.0, 2},
          {"mathsize=150%", 30.0, 0},
          {"mathsize=2em", 40.0, 0},
          {"scriptlevel=bogus mathsize=bogus mathcolor=bogus displaystyle=bogus", 20.0, 0}
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<math style='font-size:20pt'><mn #{attributes}>2</mn></math>"
               )

      assert {:ok, styled} = Style.compute_detailed(dom)
      [atom] = styled.children
      [glyph] = atom.style._math_box.glyphs
      assert_in_delta glyph.font_size, size, 0.00001
      assert atom.style.math_depth == 0
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<math style='font-size:20pt'><mrow scriptlevel=2><mn scriptlevel=0>2</mn></mrow></math>"
             )

    assert {:ok, %{children: [atom]}} = Style.compute_detailed(dom)
    [glyph] = atom.style._math_box.glyphs
    assert_in_delta glyph.font_size, 20.0, 0.00001
  end

  test "authored math display keywords retain namespace layout and font size units" do
    for {display, expected} <- [
          {"math", :block},
          {"block math", :block},
          {"inline math", :inline_block}
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<math style='display:#{display};font-size:2rem'><mn>12</mn></math>"
               )

      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom)
      assert node.style.display == expected
      atom = if expected == :block, do: hd(node.children), else: node
      [number | _] = atom.style._math_box.glyphs
      assert number.font_size == 24.0
    end
  end

  test "ordinary token flow retains its CSS baseline while ordinary row flow diagnoses" do
    for display <- ["block", "inline", "inline-block"] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<math><mtext style='display:#{display}'><span>a</span><span>b</span></mtext></math>"
               )

      assert {:ok, %{children: [atom]}} = Style.compute_detailed(dom)
      assert length(atom.style._math_box.glyphs) == 2
      assert Enum.map(atom.style._math_box.glyphs, & &1.y) == [0.0, 0.0]
      assert atom.style._math_box.ascent + atom.style._math_box.descent == atom.style.line_height

      assert {:ok, dom} =
               HtmlParser.parse_detailed("<math style='display:#{display}'><mi>x</mi></math>")

      assert {:error, {:unsupported_html, diagnostic}} = Style.compute_detailed(dom)
      assert diagnostic.source == "math"
    end

    assert {:error, {:invalid_css, _}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               "<div style='display:math'>Unsupported math formatting</div>"
             )
  end

  test "disclosures select their first summary and preserve authored selector positions" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html = """
    <style>summary:nth-child(2){color:red}summary{font-weight:bold}
    summary::before{content:"Authored "}</style>
    <details><span>Closed before</span><summary>Closed legend</summary><p>Closed body</p></details>
    <details open=false><span>Open before</span><summary>Open legend</summary>
    <summary>Additional summary</summary><p>Open body</p></details>
    <details></details><details open><p>Fallback body</p></details>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [closed, open, empty, fallback]}} = Style.compute_detailed(dom)
    assert closed.style.display == :block
    assert length(closed.children) == 1
    [legend] = closed.children
    assert legend.style.display == :list_item
    assert legend.style.list_style_position == :inside
    assert legend.style.list_marker_type == :disclosure_closed
    assert legend.style.color == {1, 0, 0}
    assert legend.style.font_weight == 700
    assert length(open.children) == 5
    assert hd(open.children).style.list_marker_type == :disclosure_open

    assert Enum.find(Enum.drop(open.children, 1), &(&1.type == :element and &1.tag == "summary")).style.display ==
             :block

    assert Enum.map(hd(empty.children).children, & &1.text) == ["Details"]
    assert hd(empty.children).style.font_weight == 400
    assert hd(fallback.children).style.list_marker_type == :disclosure_open
    assert length(fallback.children) == 2
  end

  test "summary marker overrides use existing list CSS and author display" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    for {css, display, marker} <- [
          {"list-style:none", :list_item, :none},
          {"list-style:outside square", :list_item, :square},
          {"list-style-type:disclosure-open", :list_item, :disclosure_open},
          {"display:block", :block, :disclosure_closed},
          {"display:none", :none, :disclosure_closed}
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<details><summary style='#{css}'>Legend</summary>Hidden</details>"
               )

      assert {:ok, %{children: [%{children: [summary]}]}} = Style.compute_detailed(dom)
      assert summary.style.display == display
      assert summary.style.list_marker_type == marker
    end
  end

  test "requirement selectors preserve individual radio attributes and initial state" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html = """
    <style>:required{color:red}:optional{color:blue}</style>
    <input id=a required=false><input id=b readonly required><input id=c disabled required>
    <textarea id=d required></textarea><select id=e required><option>Choice</option></select>
    <input id=f type=hidden required><input id=g type=button required value=Button>
    <input id=h type=radio name=group required><input id=i type=radio name=group checked>
    <input id=j><textarea id=k></textarea><select id=l></select>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, tree} = Style.compute_detailed(dom)
    elements = Enum.filter(tree.children, &(&1.type == :element))
    assert length(elements) == 12
    nodes = Map.new(Enum.zip(~w(a b c d e f g h i j k l), elements))
    for id <- ~w(a b c d e h), do: assert(nodes[id].style.color == {1, 0, 0})
    for id <- ~w(i j k l), do: assert(nodes[id].style.color == {0, 0, 1})
    for id <- ~w(f g), do: assert(nodes[id].style.color == {0, 0, 0})
    assert nodes["a"].style._form_control.attributes["required"] == "false"
    refute nodes["h"].style._form_control.initial_state.checked
    assert nodes["i"].style._form_control.initial_state.checked
    refute Map.has_key?(nodes["i"].style._form_control.attributes, "required")
  end

  test "horizontal rules resolve inherited font margins and legacy hints" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    for {attributes, expected} <- [
          {"",
           %{
             style: :inset,
             left: true,
             right: true,
             width: :auto,
             height: :auto,
             borders: %{top: 0.75, right: 0.75, bottom: 0.75, left: 0.75}
           }},
          {"align=LEFT width=25% size=1",
           %{
             style: :inset,
             left: false,
             right: true,
             width: {:percent, 0.25},
             height: :auto,
             borders: %{top: 0.75, right: 0.75, bottom: 0.0, left: 0.75}
           }},
          {"align=right size=5",
           %{
             style: :inset,
             left: true,
             right: false,
             width: :auto,
             height: 2.25,
             borders: %{top: 0.75, right: 0.75, bottom: 0.75, left: 0.75}
           }},
          {"color=transparent noshade=false size=0",
           %{
             style: :solid,
             left: true,
             right: true,
             width: :auto,
             height: :auto,
             borders: %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
           }}
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<div style='font-size:10pt;color:blue'><hr #{attributes}></div>"
               )

      assert {:ok, %{children: [container]}} = Style.compute_detailed(dom)
      [rule] = container.children
      assert rule.style.display == :block
      assert rule.style.overflow == :hidden
      assert rule.style.font_size == 10
      assert rule.style.color == {0.5019607843, 0.5019607843, 0.5019607843}
      assert rule.style.margin == %{top: 4.9921875, right: 0.0, bottom: 4.9921875, left: 0.0}
      assert rule.style.margin_inline_auto == Map.take(expected, [:left, :right])
      assert Map.get(rule.style, :width, :auto) == expected.width
      assert Map.get(rule.style, :height, :auto) == expected.height
      assert rule.style.border_widths == expected.borders
      assert Enum.all?(rule.style.border_styles, fn {_side, value} -> value == expected.style end)
    end
  end

  test "author styles override horizontal-rule presentation hints and automatic margins" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<hr color=red noshade width=90 height=999 size=8 align=right style='width:40pt;margin:1pt 2pt 3pt 4pt;color:blue;border:2pt dashed green;overflow:visible'>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [rule]}} = Style.compute_detailed(dom)
    assert rule.style.width == 40
    refute Map.has_key?(rule.style, :height)
    assert rule.style.margin == %{top: 1.0, right: 2.0, bottom: 3.0, left: 4.0}
    refute Map.has_key?(rule.style, :margin_inline_auto)
    assert rule.style.color == {0, 0, 1}
    assert rule.style.overflow == :visible
    assert rule.style.border_widths == %{top: 1.5, right: 1.5, bottom: 1.5, left: 1.5}
    assert Enum.all?(rule.style.border_styles, fn {_side, value} -> value == :dashed end)

    assert Enum.all?(rule.style.border_colors, fn {_side, value} ->
             value == {0, 0.5019607843, 0}
           end)

    assert {:ok, dom} = HtmlParser.parse_detailed("<div style='margin:auto 2pt'>Text</div>")
    assert {:ok, %{children: [block]}} = Style.compute_detailed(dom)
    assert block.style.margin == %{top: 0.0, right: 2.0, bottom: 0.0, left: 2.0}
    assert block.style.margin_after == 0.0
    refute Map.has_key?(block.style, :margin_inline_auto)
  end

  test "named colors flow through foreground backgrounds borders and inherited currentcolor" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html = """
    <div style='color:RebeccaPurple;background-color:aliceblue;border:2pt solid goldenrod'>
    <span style='color:currentcolor;background:khaki;text-decoration:underline darkslategrey'>Text</span>
    </div>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [container]}} = Style.compute_detailed(dom)
    assert container.style.color == {0.4, 0.2, 0.6}
    assert container.style.background_color == {240 / 255, 248 / 255, 1}
    assert container.style.border_colors.top == {218 / 255, 165 / 255, 32 / 255}
    child = Enum.find(container.children, &match?(%{tag: "span"}, &1))
    assert child.style.color == container.style.color
    assert child.style.background_color == {240 / 255, 230 / 255, 140 / 255}
  end

  test "closed popovers match static selectors and skip unused image resources" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html = """
    <style>
    :popover-open {color:red}
    [popover]:not(:popover-open) {color:blue;background-image:url(missing.png)}
    p {color:green}
    </style>
    <div popover=auto><img src=missing.png><input name=closed value=Unused></div>
    <img popover=hint src=missing.png><wbr popover><input popover name=closed2 value=Unused>
    <p>Visible</p>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, tree} =
             Style.compute(dom,
               asset_resolver: fn request ->
                 flunk("closed popover resolved #{inspect(request)}")
               end
             )

    [container, image, break, input, visible] = tree.children

    for node <- [container, image, break, input] do
      assert node.style.display == :none
      assert node.children == []
      refute Map.has_key?(node.style, :_popover_state)
    end

    assert container.style.color == {0, 0, 1}
    assert visible.style.color == {0, 0.5019607843, 0}
  end

  test "author-exposed popovers retain UA geometry and hidden inputs remain hidden" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    for tag <- ["div", "x-box", "wbr"] do
      ending = if tag in ["wbr", "input"], do: "", else: "</#{tag}>"

      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<#{tag} popover style='display:block!important'>" <> ending
               )

      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom)
      assert node.style.display == if(tag == "wbr", do: :soft_break, else: :block)
      assert node.style.position == :fixed
      assert node.style.width == :fit_content and node.style.height == :fit_content
      assert node.style.overflow == :auto

      assert Enum.all?(node.style.padding, fn {_side, value} ->
               value == node.style.font_size * 0.25
             end)
    end

    for markup <- [
          "<input size=12>",
          "<input type=checkbox>",
          "<textarea cols=12 rows=3>Value</textarea>",
          "<select><option>Choice</option></select>",
          "<button>Submit</button>"
        ] do
      assert {:ok, normal_dom} = HtmlParser.parse_detailed(markup)
      exposed = String.replace(markup, ">", " popover style='display:block'>", global: false)
      assert {:ok, exposed_dom} = HtmlParser.parse_detailed(exposed)
      assert {:ok, %{children: [normal]}} = Style.compute_detailed(normal_dom)
      assert {:ok, %{children: [popover]}} = Style.compute_detailed(exposed_dom)

      unless String.starts_with?(markup, ["<select", "<button"]) do
        assert Map.get(popover.style, :width, :auto) == Map.get(normal.style, :width, :auto),
               markup

        assert Map.get(popover.style, :height, :auto) == Map.get(normal.style, :height, :auto),
               markup
      end

      refute Map.has_key?(popover.style, :_popover_intrinsic_dimensions)
      assert {:ok, _} = NativeElixirPdfUtilities.HtmlToPdf.render(exposed)
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input popover style='display:block;width:24pt;height:12pt'>"
             )

    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom)
    assert node.style.width == 24.0 and node.style.height == 12.0

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div popover style='display:block;position:fixed;overflow:auto;font-size:20pt;color:blue'>Visible</div>"
             )

    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom)
    assert node.style.padding == %{top: 5.0, right: 5.0, bottom: 5.0, left: 5.0}
    assert node.style.border_colors.top == node.style.color
    assert node.style.position == :fixed and node.style.overflow == :auto

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div popover style='display:block;position:absolute;width:48pt;height:12pt;padding:2pt;margin:1pt;overflow:hidden'>Visible</div>"
             )

    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom)
    assert node.style.position == :absolute and node.style.overflow == :hidden
    assert node.style.width == 48.0 and node.style.height == 12.0
    assert node.style.padding == %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0}

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<input type=hidden popover style='display:block!important'>"
             )

    assert {:ok, %{children: [%{style: %{display: :none}}]}} = Style.compute(dom)

    assert {:ok, dom} = HtmlParser.parse_detailed("<dialog popover open>Visible dialog</dialog>")
    assert {:ok, %{children: [%{style: %{display: :block}}]}} = Style.compute(dom)

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{"popover" => ""},
          children: [
            %{
              type: :element,
              tag: "img",
              attributes: %{"src" => "javascript:run()"},
              children: []
            }
          ]
        }
      ]
    }

    assert {:error, {:unsupported_html, %{stage: :html, source: "src"}}} =
             Style.compute_detailed(dom)
  end

  test "option enabled and disabled selectors use inherited groups without changing attributes" do
    alias NativeElixirPdfUtilities.Validators.{HtmlFormValidator, HtmlValidator}

    assert {:ok, %{children: children}} =
             HtmlParser.parse_detailed("""
             <select><optgroup label='Closed' disabled><script type='text/plain'>Unused data</script><template><option selected>Unused option</option></template><option>A</option><option selected>B</option></optgroup>
             <optgroup label='Open'><option disabled>C</option><option>D</option></optgroup></select>
             """)

    [select] = HtmlFormValidator.prepare_initial_nodes(children)
    groups = Enum.filter(select.children, &match?(%{tag: "optgroup"}, &1))
    [closed, open] = groups
    assert HtmlValidator.control_state_matches?(:disabled, closed)
    assert HtmlValidator.control_state_matches?(:enabled, open)
    [a, b, c, d] = HtmlValidator.select_options(select.children)
    refute Map.has_key?(a.attributes, "disabled")
    assert HtmlValidator.control_state_matches?(:disabled, a)
    assert HtmlValidator.control_state_matches?(:disabled, b)
    assert HtmlValidator.control_state_matches?(:disabled, c)
    assert HtmlValidator.control_state_matches?(:enabled, d)
    assert HtmlValidator.control_state_matches?(:checked, b)
    refute HtmlValidator.control_state_matches?(:checked, d)
    assert select._initial_form_state.selection.value == "B"
    assert length(select._initial_form_state.selection.disabled_ids) == 3
    assert HtmlValidator.select_options([Map.put(a, :namespace, :svg)]) == []
  end

  test "checked selectors use effective radio, checkbox and option state" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    alias NativeElixirPdfUtilities.Validators.{HtmlValidator, HtmlFormValidator}

    html = """
    <form><input type='radio' name='mode' checked><input type='radio' name='mode' checked>
    <input type='checkbox' checked><input type='checkbox'><input type='text'>
    <select><option selected>Alpha</option><option selected>Beta</option></select></form>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [prepared] = HtmlFormValidator.prepare_initial_nodes(dom.children)

    [first, last, checked, unchecked, text, select] =
      Enum.filter(prepared.children, &match?(%{type: :element}, &1))

    refute HtmlValidator.control_state_matches?(:checked, first)
    assert HtmlValidator.control_state_matches?(:checked, last)
    assert HtmlValidator.control_state_matches?(:checked, checked)
    refute HtmlValidator.control_state_matches?(:checked, unchecked)
    refute HtmlValidator.control_state_matches?(:checked, text)
    refute HtmlValidator.control_state_matches?(:checked, prepared)
    refute HtmlValidator.control_state_matches?(:checked, Map.put(checked, :namespace, :svg))
    [alpha, beta] = select.children

    assert Map.has_key?(alpha.attributes, "selected") and
             Map.has_key?(beta.attributes, "selected")

    refute HtmlValidator.control_state_matches?(:checked, alpha)
    assert HtmlValidator.control_state_matches?(:checked, beta)
    assert select._initial_form_state.selection.value == "Beta"

    assert {:ok, %{children: [form]}} =
             Style.compute_detailed(dom,
               stylesheets: [{:css, "input:checked{color:red}input:not(:checked){color:blue}"}]
             )

    assert Enum.map(
             form.children |> Enum.filter(&match?(%{type: :element}, &1)) |> Enum.take(5),
             & &1.style.color
           ) ==
             [{0, 0, 1}, {1, 0, 0}, {1, 0, 0}, {0, 0, 1}, {0, 0, 1}]
  end

  test "initial select state preserves authored attributes and dormant template isolation" do
    alias NativeElixirPdfUtilities.Validators.{HtmlFormValidator, HtmlValidator}

    for {html, expected} <- [
          {"<select></select>", ""},
          {"<select><option disabled>A</option><option>B</option></select>", "B"},
          {"<select><option disabled>A</option></select>", ""},
          {"<select><option disabled selected>A</option><option>B</option></select>", "A"},
          {"<select><template><option selected>Unused</option></template><option></option></select>",
           ""}
        ] do
      assert {:ok, %{children: children}} = HtmlParser.parse_detailed(html)
      [select] = HtmlFormValidator.prepare_initial_nodes(children)
      assert select._initial_form_state.selection.value == expected

      for option <- HtmlValidator.select_options(select.children) do
        assert HtmlValidator.control_state_matches?(:checked, option) ==
                 (option._selector_id == select._initial_form_state.selection.id)
      end
    end
  end

  test "initial radio groups use owners and preserve authored checked selectors" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<form id='one'><input type='radio' name='mode' value='a' checked><input type='radio' name='mode' value='b' checked></form><form id='two'><input type='radio' name='mode' value='c' checked><input type='radio' name='mode' value='d'></form>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [one, two]}} =
             Style.compute_detailed(dom,
               stylesheets: [{:css, "input[checked]{color:red}"}],
               system_font_discovery: false
             )

    [a, b] = one.children
    [c, d] = two.children
    assert one.style.display == :block and two.style.display == :block

    assert Enum.map([a, b, c, d], & &1.style._form_control.initial_state.checked) == [
             false,
             true,
             true,
             false
           ]

    assert a.style._form_control.initial_state.owner == b.style._form_control.initial_state.owner
    refute a.style._form_control.initial_state.owner == c.style._form_control.initial_state.owner
    assert a.style.color == {1, 0, 0} and b.style.color == {1, 0, 0}
    assert Map.has_key?(a.style._form_control.attributes, "checked")
    assert Map.has_key?(b.style._form_control.attributes, "checked")
  end

  test "ownership uses first matching IDs, unresolved explicit owners and scoped label targets" do
    alias NativeElixirPdfUtilities.Validators.HtmlFormValidator

    html =
      "<input form='late' name='forward'><div id='blocked'></div><form id='late'><input id='inside' type='radio' name='mode' value='a' checked><input type='radio' name='mode' value='b' form='missing' checked><input name='wrong' form='blocked'></form><form id='blocked'></form><label for='inside'>Explicit</label><label><input type='hidden' id='secret'><input id='visible'>Implicit</label>"

    assert {:ok, %{children: children}} = HtmlParser.parse_detailed(html)

    [forward, blocker, form, duplicate, explicit, implicit] =
      HtmlFormValidator.prepare_initial_nodes(children)

    assert forward._initial_form_state.owner == form._selector_id
    [inside, missing, wrong] = form.children
    assert inside._initial_form_state.owner == form._selector_id
    assert missing._initial_form_state.owner == nil
    assert wrong._initial_form_state.owner == nil
    assert blocker.tag == "div" and duplicate.tag == "form"
    assert explicit._initial_label_control == inside._selector_id
    [hidden, visible, _] = implicit.children
    refute implicit._initial_label_control == hidden._selector_id
    assert implicit._initial_label_control == visible._selector_id

    key = {HtmlFormValidator, :initial_context}
    previous = Process.get(key)
    retained = %{ids: [], controls: [], labels: []}

    try do
      Process.put(key, retained)

      assert [%{_initial_form_state: %{checked: false}}] =
               HtmlFormValidator.prepare_initial_nodes([
                 %{
                   type: :element,
                   tag: "input",
                   attributes: %{"type" => "radio", "name" => "mode"},
                   children: []
                 }
               ])

      assert Process.get(key) == retained
    after
      if is_nil(previous), do: Process.delete(key), else: Process.put(key, previous)
    end
  end

  test "unowned and nameless groups, empty labels and template contents keep their initial states" do
    alias NativeElixirPdfUtilities.Validators.HtmlFormValidator

    html =
      "<input type='radio' name='x' checked><input type='radio' name='x' checked>" <>
        "<input type='radio' checked><input type='radio' name='' checked>" <>
        "<label for='absent'>Missing</label><label for='block'>Wrong kind</label>" <>
        "<label>Empty</label><div id='block'></div>" <>
        "<template><input type='radio' name='x' checked></template>" <>
        "<label for='hidden'>Hidden</label><input type='hidden' id='hidden'>" <>
        "<label for='duplicate'>First ID wins</label><span id='duplicate'></span><input id='duplicate'>"

    assert {:ok, %{children: children}} = HtmlParser.parse_detailed(html)

    [
      first,
      last,
      nameless,
      empty_name,
      missing,
      wrong_kind,
      empty_label,
      _,
      template,
      hidden_label,
      _,
      duplicate_label,
      _,
      _
    ] = HtmlFormValidator.prepare_initial_nodes(children)

    refute first._initial_form_state.checked
    assert last._initial_form_state.checked
    assert nameless._initial_form_state.checked and empty_name._initial_form_state.checked
    assert first._initial_form_state.owner == nil

    assert Enum.all?(
             [missing, wrong_kind, empty_label, hidden_label, duplicate_label],
             &is_nil(&1._initial_label_control)
           )

    assert template.children == []
    assert template.content.children != []

    foreign = %{
      type: :element,
      namespace: :svg,
      tag: "text",
      attributes: %{"id" => "foreign"},
      children: []
    }

    label = %{type: :element, tag: "label", attributes: %{"for" => "foreign"}, children: []}

    assert [_, %{_initial_label_control: nil}] =
             HtmlFormValidator.prepare_initial_nodes([foreign, label])
  end

  test "language selectors inherit nearest language and preserve empty and literal XML states" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<div lang='en-US'><p>Inherited</p><p lang='de-Latn-DE'>German</p>" <>
        "<p lang=''>Unknown<span>Still unknown</span></p><p xml:lang='fr'>Literal</p>" <>
        "<p lang='und'>Undetermined</p><p lang=' en '>Padded</p><p lang='qaa'>Private</p></div>"

    css =
      "p,span{color:black}:lang(en){color:red}:lang('de-DE'){color:blue}" <>
        ":lang(''){color:green}:lang(und){color:#800080}:lang(qaa){color:#ffa500}"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [container]}} =
             Style.compute_detailed(dom, stylesheets: [{:css, css}])

    [inherited, german, blank, literal, undetermined, padded, private] = container.children
    assert inherited.style.color == {1, 0, 0}
    assert german.style.color == {0, 0, 1}
    assert_style_value(blank.style.color, {0, 128 / 255, 0})
    assert List.last(blank.children).style.color == blank.style.color
    assert literal.style.color == {1, 0, 0}
    assert_style_value(undetermined.style.color, {128 / 255, 0, 128 / 255})
    assert padded.style.color == {0, 0, 0}
    assert_style_value(private.style.color, {1, 165 / 255, 0})
  end

  test "content language pragmas use tree order and ignore comma, empty and inert values" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<html><head><meta http-equiv='Content-Language' content='de extra'>" <>
        "<noscript><meta http-equiv='content-language' content='en-US later'></noscript>" <>
        "<meta http-equiv='content-language' content='fr,de'><meta http-equiv='content-language' content=' '>" <>
        "<meta http-equiv='content-language'><template><meta http-equiv='content-language' content='fr'></template>" <>
        "</head><body><p>Default</p><p lang=''>Explicit empty</p></body></html>"

    css = "p{color:black}p:lang(en){color:red}p:lang(''){color:blue}"
    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [body]}} = Style.compute_detailed(dom, stylesheets: [{:css, css}])
    assert Enum.map(body.children, & &1.style.color) == [{1, 0, 0}, {0, 0, 1}]

    assert {:ok, fragment} =
             HtmlParser.parse_detailed(
               "<meta http-equiv='content-language' content='fr'><p>Fragment</p>"
             )

    assert {:ok, %{children: [paragraph]}} =
             Style.compute_detailed(fragment, stylesheets: [{:css, "p:lang(fr){color:red}"}])

    assert paragraph.style.color == {1, 0, 0}
  end

  test "negated language selectors receive the document default context" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<meta http-equiv='content-language' content='fr'><p>Default French</p><p lang='en'>Explicit English</p>"
             )

    assert {:ok, %{children: [french, english]}} =
             Style.compute_detailed(dom, stylesheets: [{:css, "p:not(:lang(en)){color:blue}"}])

    assert french.style.color == {0, 0, 1}
    assert english.style.color == {0, 0, 0}
  end

  test "cached stylesheets do not share default language across style computations" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{RenderCache, Style}
    css = "p:lang(en){color:red}p:lang(fr){color:blue}"

    RenderCache.with_stylesheets(fn ->
      for {language, expected} <- [{"en", {1, 0, 0}}, {"fr", {0, 0, 1}}, {"en", {1, 0, 0}}] do
        assert {:ok, dom} =
                 HtmlParser.parse_detailed(
                   "<meta http-equiv='content-language' content='#{language}'><p>Current document</p>"
                 )

        assert {:ok, %{children: [paragraph]}} =
                 Style.compute_detailed(dom, stylesheets: [{:css, css}])

        assert paragraph.style.color == expected
      end
    end)
  end

  test "language cache belongs to each style computation and source attributes are unchanged" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    css = "p:lang(en){color:red}p:lang(fr){color:blue}"

    for {language, expected} <- [{"en", {1, 0, 0}}, {"fr", {0, 0, 1}}, {"en", {1, 0, 0}}] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed("<div lang='#{language}'><p>Current document</p></div>")

      supplied = update_in(dom, [:children, Access.at(0)], &Map.put(&1, :_selector_id, :forged))

      assert {:ok, %{children: [container]}} =
               Style.compute_detailed(supplied, stylesheets: [{:css, css}])

      assert hd(container.children).style.color == expected
      assert hd(dom.children).attributes["lang"] == language
    end

    assert {:ok, dom} = HtmlParser.parse_detailed("<p>Text</p>")

    for css <- ["p:lang(){color:red}", "p:lang(en,){color:red}"] do
      assert {:error, {:invalid_css, diagnostic}} =
               Style.compute_detailed(dom, stylesheets: [{:css, css}])

      assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end
  end

  test "heading offsets carry computed levels without changing heading or descendant text styles" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<div headingoffset='2'><h1>Three <span>plain</span></h1>" <>
        "<section headingoffset='1'><h2>Five</h2><h6>Cap</h6></section>" <>
        "<div headingreset='false' headingoffset='1'><h2>Reset</h2></div></div><h2>Outside</h2>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [container, outside]}} = Style.compute_detailed(dom)
    [three, section, reset] = container.children
    assert three.style.heading_level == 3
    assert Enum.map(section.children, & &1.style.heading_level) == [5, 9]
    assert hd(reset.children).style.heading_level == 3
    refute Map.has_key?(container.style, :heading_level)
    refute Map.has_key?(outside.style, :heading_level)
    refute Map.has_key?(List.last(three.children).style, :heading_level)
    assert three.style.font_size == 24.0
    assert outside.style.font_size == 18.0
  end

  test "supplied DOM heading flags are recomputed from canonical attributes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    assert {:ok, dom} = HtmlParser.parse_detailed("<h2>Heading</h2>")
    supplied = update_in(dom, [:children, Access.at(0)], &Map.put(&1, :_heading_level, 9))
    assert {:ok, %{children: [heading]}} = Style.compute_detailed(supplied)
    refute Map.has_key?(heading.style, :heading_level)

    supplied =
      update_in(
        supplied,
        [:children, Access.at(0), :attributes],
        &Map.put(&1, "headingoffset", "1")
      )

    supplied = put_in(supplied, [:children, Access.at(0), :attribute_order], ["headingoffset"])

    assert {:ok, %{children: [heading]}} = Style.compute_detailed(supplied)
    assert heading.style.heading_level == 3
  end

  test "word break pseudo content and own counter effects create no boxes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div style='counter-reset:item'><wbr style='counter-increment:item'><span class=result>Visible</span></div>"
             )

    css =
      "wbr::before{content:'Before'}wbr::after{content:'After'}.result::before{content:counter(item)}"

    assert {:ok, %{children: [%{children: [marker, result]}]}} =
             Style.compute_detailed(dom, stylesheets: [{:css, css}])

    assert marker.children == [] and marker.style.display == :soft_break
    [pseudo, _] = result.children
    assert hd(pseudo.children).text == "0"
  end

  test "legacy nobr and void word breaks apply defaults below author CSS" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<nobr>A<span><wbr></span>B</nobr><wbr hidden><wbr style='display:block;font-size:30pt;white-space:nowrap'>"
             )

    assert {:ok, %{children: [nobr, hidden, authored]}} = Style.compute_detailed(dom)
    assert nobr.style.white_space == :nowrap
    [_, span, _] = nobr.children
    assert hd(span.children).style.white_space == :normal
    assert hd(span.children).style.display == :soft_break
    assert hidden.style.display == :none
    assert authored.style.display == :soft_break
    assert authored.style.font_size == 30 and authored.style.white_space == :nowrap

    assert {:ok, %{children: [%{style: %{white_space: :normal}}, %{style: %{display: :none}}, _]}} =
             Style.compute_detailed(dom,
               stylesheets: [{:css, "nobr{white-space:normal}wbr{display:none}"}]
             )
  end

  test "line-break and overflow-wrap retain separate inherited behavior" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p style='line-break:anywhere;overflow-wrap:normal;word-break:normal'><span>Text</span></p>"
             )

    assert {:ok, %{children: [%{style: parent, children: [%{style: child}]}]}} =
             Style.compute_detailed(dom)

    for style <- [parent, child] do
      assert style.line_break == :anywhere
      assert style.overflow_wrap == :normal
      assert style.word_break == :normal
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p style='overflow-wrap:anywhere'><span style='line-break:normal'>Text</span></p>"
             )

    assert {:ok, %{children: [%{children: [%{style: style}]}]}} = Style.compute_detailed(dom)
    assert style.line_break == :normal and style.overflow_wrap == :anywhere
  end

  test "unvisited links underline below author CSS while placeholder anchors inherit" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<p style='color:red'><a>Placeholder</a><a href='https://example.com'>Active <span>Nested</span></a><a inert href='mailto:test@example.com'>Held</a></p>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [%{children: [placeholder, active, held]}]}} =
             Style.compute_detailed(dom)

    assert placeholder.style.color == {1, 0, 0}
    assert Map.get(placeholder.style, :text_decoration_line, []) == []
    refute Map.has_key?(placeholder.style, :link_url)
    assert active.style.color == {0, 0, 238 / 255}
    assert active.style.text_decoration_line == [:underline]
    assert active.style.link_url == "https://example.com"
    assert hd(active.children).style.color == active.style.color
    assert List.last(active.children).style.color == active.style.color
    assert held.style.color == active.style.color
    assert held.style.text_decoration_line == [:underline]
    refute Map.has_key?(held.style, :link_url)

    assert {:ok, %{children: [%{children: [_, active, held]}]}} =
             Style.compute_detailed(dom,
               stylesheets: [{:css, "a[href]{color:green;text-decoration:none}"}]
             )

    for node <- [active, held] do
      assert node.style.color == {0, 0.5019607843, 0}
      assert node.style.text_decoration_line == []
    end

    assert active.style.link_url == "https://example.com"
    refute Map.has_key?(held.style, :link_url)
  end

  test "highlight colors and modification decorations share existing CSS precedence" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<p style='color:red'><mark>Marked</mark><ins>Added</ins><del>Removed</del><strike>Legacy</strike></p>"
             )

    assert {:ok, %{children: [%{children: [mark, ins, del, strike]}]}} =
             Style.compute_detailed(dom)

    assert mark.style.background_color == {1, 1, 0} and mark.style.color == {0, 0, 0}
    assert ins.style.text_decoration_line == [:underline]
    assert del.style.text_decoration_line == [:line_through]
    assert strike.style.text_decoration_line == [:line_through]
    assert ins.style.color == {1, 0, 0} and del.style.color == {1, 0, 0}

    assert {:ok, %{children: [%{children: [mark, ins, del, strike]}]}} =
             Style.compute_detailed(dom,
               stylesheets: [
                 {:css, "mark{background:blue;color:red} ins,del,strike{text-decoration:none}"}
               ]
             )

    assert mark.style.background_color == {0, 0, 1} and mark.style.color == {1, 0, 0}
    for node <- [ins, del, strike], do: assert(node.style.text_decoration_line == [])

    for tag <- ["mark", "ins", "del", "strike"] do
      parent = if tag in ["ins", "del"], do: "p", else: tag

      inner =
        if parent == tag,
          do: %{type: :element, tag: "div", attributes: %{}, children: []},
          else: %{
            type: :element,
            tag: tag,
            attributes: %{},
            children: [%{type: :element, tag: "div", attributes: %{}, children: []}]
          }

      root = %{type: :element, tag: parent, attributes: %{}, children: [inner]}

      assert {:error,
              {:invalid_document, %{stage: :style, operation: :compute, message: message}}} =
               Style.compute_detailed(%{type: :document, children: [root]})

      assert message =~ "content"
    end
  end

  test "inert descendants keep artwork and selector states while losing active links and fields" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<div><x-box inert=false><input name=held value=Printed><a href='https://example.com'>Link <span>Nested</span></a></x-box><input name=active value=Initial></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [%{children: [held, active]}]}} =
             Style.compute_detailed(dom,
               stylesheets: [{:css, "input:enabled{color:red} input:disabled{color:blue}"}]
             )

    assert [
             %{tag: "input", style: input_style},
             %{tag: "a", style: link_style, children: [text, nested]}
           ] = held.children

    assert input_style.color == {1, 0, 0}
    refute Map.has_key?(input_style, :_form_control)
    refute Map.has_key?(link_style, :link_url)
    refute Map.has_key?(text.style, :link_url)
    refute Map.has_key?(nested.style, :link_url)
    assert active.style._form_control.attributes["name"] == "active"
    assert active.style.color == {1, 0, 0}

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<a href='https://example.com'>Active<span inert>Held</span>Active</a>"
             )

    assert {:ok, %{children: [%{children: [first, held, last]}]}} = Style.compute_detailed(dom)
    assert first.style.link_url == "https://example.com"
    refute Map.has_key?(held.style, :link_url)
    assert last.style.link_url == "https://example.com"

    node = %{
      type: :element,
      tag: "x-box",
      attributes: %{"inert" => "false"},
      children: [
        %{
          type: :element,
          tag: "input",
          attributes: %{"name" => "held", "value" => "Printed"},
          children: []
        }
      ]
    }

    assert {:ok, %{children: [%{children: [input]}]}} =
             Style.compute_detailed(%{type: :document, children: [node]})

    refute Map.has_key?(input.style, :_form_control)

    for stale <- [true, false, :invalid] do
      input = %{
        type: :element,
        tag: "input",
        attributes: %{"name" => "active", "value" => "Initial"},
        children: [],
        _html_inert: stale
      }

      link = %{
        type: :element,
        tag: "a",
        attributes: %{"href" => "https://example.com"},
        children: [%{type: :text, text: "Active"}],
        _html_inert: stale
      }

      root = %{
        type: :element,
        tag: "div",
        attributes: %{},
        children: [input, link],
        _html_inert: stale
      }

      assert {:ok, %{children: [%{children: [input, link]}]}} =
               Style.compute_detailed(%{type: :document, children: [root]})

      assert input.style._form_control.attributes["name"] == "active"
      assert link.style.link_url == "https://example.com"
      assert hd(link.children).style.link_url == "https://example.com"
    end
  end

  test "hidden defaults keep selector siblings and permit author display overrides" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<div><p hidden=false>Gone</p><p hidden style='display:block'>Shown</p><p>After</p></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [%{children: [gone, shown, after_node]}]}} =
             Style.compute_detailed(dom,
               stylesheets: [{:css, "p:nth-child(2)[hidden]{color:red}"}]
             )

    assert gone.style.display == :none
    assert gone.children == []
    assert shown.style.display == :block
    assert shown.style.color == {1, 0, 0}
    assert [%{text: "Shown"}] = shown.children
    assert [%{text: "After"}] = after_node.children

    for tag <- ["p", "x-box"] do
      node = %{type: :element, tag: tag, attributes: %{"hidden" => "until-found"}, children: []}

      assert {:ok, %{children: [styled]}} =
               Style.compute_detailed(%{type: :document, children: [node]})

      assert styled.attributes["hidden"] == "until-found"
      assert Map.get(styled.style, :_html_content_skipped, false) == (tag == "p")
    end
  end

  test "custom containers inherit inline styles and keep arbitrary attributes as metadata" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<div><x-É custom-note='Active' disabled readonly multiple href='metadata' src='metadata'><span>Text</span><input name='actual' value='Initial'></x-É></div>"

    css =
      ~S|x-É[custom-\6e ote=\41 ctive] {color:red} x-É:disabled, x-É:enabled {color:blue} x-é {color:blue}|

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    assert {:ok, %{children: [%{children: [%{children: [custom]}]}]}} =
             Style.compute_detailed(dom, stylesheets: [{:css, css}])

    assert custom.tag == "x-É"
    assert custom.style.display == :inline
    assert custom.style.color == {1, 0, 0}
    refute Map.has_key?(custom.style, :link)
    refute Map.has_key?(custom.style, :form_control)
    assert [%{tag: "span", style: style}, %{tag: "input"}] = custom.children
    assert style.color == {1, 0, 0}

    for {name, selector} <- [{~S|x-"quote|, ~S|x-\"quote|}, {"x-<angle", ~S|x-\<angle|}] do
      assert {:ok, dom} = HtmlParser.parse_detailed("<" <> name <> ">Literal</" <> name <> ">")

      assert {:ok, %{children: [%{tag: ^name, style: %{color: {1, 0, 0}}}]}} =
               Style.compute_detailed(dom, stylesheets: [{:css, selector <> "{color:red}"}])
    end
  end

  test "supplied custom DOMs delegate name context namespace and active attribute failures" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    text = %{type: :text, text: "Text"}
    custom = %{type: :element, tag: "x-box", attributes: %{}, children: [text]}
    assert {:ok, _} = Style.compute_detailed(%{type: :document, children: [custom]})

    for node <- [
          %{custom | tag: "invalid"},
          %{custom | tag: "x-Upper"},
          %{custom | attributes: %{"onclick" => "run()"}},
          Map.put(custom, :namespace, :invalid)
        ] do
      assert {:error, {_reason, diagnostic}} =
               Style.compute_detailed(%{type: :document, children: [node]})

      assert diagnostic.operation == :compute
      assert diagnostic.source == node.tag
      assert is_binary(diagnostic.message)
    end

    block = %{type: :element, tag: "div", attributes: %{}, children: [text]}

    parent = %{
      type: :element,
      tag: "p",
      attributes: %{},
      children: [%{custom | children: [block]}]
    }

    assert {:error, {:invalid_document, %{stage: :style, source: "div", message: message}}} =
             Style.compute_detailed(%{type: :document, children: [parent]})

    assert message =~ "transparent"
  end

  test "escaped selectors match Unicode names without Unicode case folding or whitespace splitting" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<div><p class='café' id='123' data-kind='Active'>First</p><p class='e\u0301 a\u00A0b'>Second</p><p class='É' id='CAFÉ'>Third</p></div>"

    css =
      ~S|\70 .\63 afé#\31 23[data-\6b ind="\41 ctive"] {color:red} .é.a\a0 b {color:blue} .É#CAFÉ {color:green} .é#café {color:red}|

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)

    assert {:ok, %{children: [%{children: [%{children: [first, second, third]}]}]}} =
             Style.compute_detailed(dom, stylesheets: [{:css, css}])

    assert first.style.color == {1, 0, 0}
    assert second.style.color == {0, 0, 1}
    assert {0, green, 0} = third.style.color
    assert_in_delta green, 128 / 255, 1.0e-10
  end

  test "noscript participates in selectors and style collection while unused templates stay inert" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<html><head><noscript><style>noscript[data-state='Fallback']{color:red} noscript>b{font-weight:700}</style></noscript></head><body><p><noscript data-state=Fallback>Text <b>Bold</b></noscript></p><template><noscript><style>p{color:blue}</style><img src=denied.png></noscript></template></body></html>"

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, %{children: [%{children: [p, _template]}]}} = Style.compute_detailed(dom)
    assert [%{tag: "noscript", style: style, children: [text, bold]}] = p.children
    assert style.display == :inline
    assert style.color == {1, 0, 0}
    assert text.text == "Text "
    assert bold.style.font_weight == 700
  end

  test "supplied noscript trees delegate head and nested-content validation" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    text = %{type: :text, text: "Fallback"}
    noscript = %{type: :element, tag: "noscript", attributes: %{}, children: [text]}

    for dom <- [
          %{
            type: :document,
            children: [%{type: :element, tag: "head", attributes: %{}, children: [noscript]}]
          },
          %{
            type: :document,
            children: [
              %{
                noscript
                | children: [
                    %{type: :element, tag: "span", attributes: %{}, children: [noscript]}
                  ]
              }
            ]
          }
        ] do
      assert {:error,
              {:invalid_document,
               %{
                 stage: :style,
                 reason: :invalid_document,
                 operation: :compute,
                 source: "noscript",
                 message: message
               }}} = Style.compute_detailed(dom)

      assert message =~ "noscript"
    end
  end

  test "inactive supplied DOM attributes use the same ASCII event policy as SVG assets" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    for {namespace, tag} <- [{:svg, "path"}, {:mathml, "mi"}, {:html, "span"}],
        name <- ~w(onclick ONCLICK OnClick) do
      child = %{
        type: :element,
        namespace: namespace,
        tag: tag,
        attributes: %{name => "run()"},
        children: []
      }

      template = %{
        type: :element,
        tag: "template",
        attributes: %{},
        children: [],
        content: %{type: :document_fragment, children: [child]}
      }

      assert {:error,
              {:unsupported_html,
               %{
                 stage: :html,
                 reason: :unsupported_html,
                 operation: :compute,
                 source: ^tag,
                 message: message
               }}} = Style.compute_detailed(%{type: :document, children: [template]})

      assert message =~ "event handlers"
    end
  end

  test "selects use canonical options without changing script-supporting sibling identity" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    inert =
      "<script type='text/plain'>Data</script><template><option selected>Unused</option></template>"

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<style>li:nth-child(3),td:nth-child(3){color:red}</style><ol reversed>#{inert}<li>One</li>#{inert}<li>Two</li></ol><select>#{inert}<option>First</option>#{inert}<option selected>Second</option></select>"
             )

    assert {:ok, %{children: [list, select]}} = Style.compute_detailed(dom)
    assert Enum.map(list.children, & &1.tag) == ~w(script template li script template li)
    items = Enum.filter(list.children, &(&1.tag == "li"))

    assert Enum.map(items, &Enum.map_join(&1.style._list_marker, fn marker -> marker.text end)) ==
             ["2.", "1."]

    assert hd(items).style.color == {1, 0, 0}
    assert select.children == [%{type: :text, text: "Second", style: hd(select.children).style}]

    assert Enum.map(select.style._form_control.children, & &1.tag) ==
             ~w(script template option script template option)

    assert {:ok, %{children: [select]}} =
             HtmlParser.parse("<select>#{inert}<option>First</option></select>")

    document = %{type: :document, children: [select]}

    for children <- [
          [hd(select.children), %{type: :text, text: "Invalid"}],
          [
            %{
              type: :element,
              tag: "option",
              attributes: %{},
              children: [%{type: :element, tag: "button", attributes: %{}, children: []}]
            }
          ]
        ] do
      assert {:error,
              {:invalid_document,
               %{
                 stage: :style,
                 reason: :invalid_document,
                 operation: :compute,
                 source: "select",
                 message: message
               }}} =
               Style.compute_detailed(
                 tree_content(%{document | children: [%{select | children: children}]})
               )

      assert message =~ "option"
    end
  end

  test "positional caches stay tied to each computation's current sibling list" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{Style, RenderCache}

    assert {:ok, %{children: [parent]} = dom} =
             HtmlParser.parse("<div><p>One</p><p>Two</p></div>")

    third = %{
      type: :element,
      tag: "p",
      attributes: %{},
      children: [%{type: :text, text: "Three"}]
    }

    changed = %{dom | children: [%{parent | children: parent.children ++ [third]}]}
    opts = [stylesheets: [{:css, "p:nth-last-of-type(2){color:red}"}]]

    RenderCache.with_stylesheets(fn ->
      assert {:ok, %{children: [first]}} = Style.compute_detailed(dom, opts)
      assert {:ok, %{children: [second]}} = Style.compute_detailed(tree_content(changed), opts)
      assert Enum.map(first.children, & &1.style.color) == [{1, 0, 0}, {0, 0, 0}]
      assert Enum.map(second.children, & &1.style.color) == [{0, 0, 0}, {1, 0, 0}, {0, 0, 0}]

      assert {:ok, roots} = HtmlParser.parse("<p>One</p><p>Two</p>")
      assert {:ok, first_roots} = Style.compute_detailed(roots, opts)
      changed_roots = %{roots | children: roots.children ++ [third]}
      assert {:ok, second_roots} = Style.compute_detailed(tree_content(changed_roots), opts)
      assert Enum.map(first_roots.children, & &1.style.color) == [{1, 0, 0}, {0, 0, 0}]

      assert Enum.map(second_roots.children, & &1.style.color) == [
               {0, 0, 0},
               {1, 0, 0},
               {0, 0, 0}
             ]
    end)
  end

  test "positional selectors count element siblings, type positions and reverse ranges" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    markup =
      "<div>text<!--ignored--><p id=p1>One</p><span>Other</span><p id=p2>Two</p><template><p>Unused</p></template><p id=p3>Three</p><p id=p4>Four</p><p id=p5>Five</p></div>"

    for {selector, expected} <- [
          {"p:nth-child(2n+1)", ~w(p1 p2 p3 p5)},
          {"p:nth-last-child(2)", ~w(p4)},
          {"p:nth-of-type(-n+3)", ~w(p1 p2 p3)},
          {"p:nth-last-of-type(-n+2)", ~w(p4 p5)},
          {"p:nth-of-type(-2n+5)", ~w(p1 p3 p5)},
          {"p:nth-of-type(0n+2)", ~w(p2)},
          {"p:nth-of-type(0)", []},
          {"p:nth-of-type(-3)", []},
          {"p:nth-of-type(0n)", []},
          {"p:not(:nth-of-type(-n+3))", ~w(p4 p5)},
          {"p:only-child", []},
          {"p:only-of-type", []}
        ] do
      assert {:ok, dom} =
               HtmlParser.parse("<style>" <> selector <> "{color:red}</style>" <> markup)

      assert {:ok, %{children: [container]}} = Style.compute_detailed(dom)

      paragraphs = Enum.filter(container.children, &match?(%{tag: "p"}, &1))

      matched =
        for {paragraph, index} <- Enum.with_index(paragraphs, 1),
            paragraph.style.color == {1, 0, 0},
            do: "p#{index}"

      assert matched == expected, selector
    end

    assert {:ok, dom} =
             HtmlParser.parse(
               "<style>p:only-child{color:red}span:only-of-type{color:blue}</style><div>text<p>Only</p><!--ignored--></div><div><p>One</p><span>Only type</span><p>Two</p></div>"
             )

    assert {:ok, %{children: [first, second]}} = Style.compute_detailed(dom)
    assert Enum.find(first.children, &match?(%{tag: "p"}, &1)).style.color == {1, 0, 0}
    assert Enum.find(second.children, &match?(%{tag: "span"}, &1)).style.color == {0, 0, 1}
  end

  test "inert template fragments never collect styles, fonts, assets or fields" do
    callback_owner = self()
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    source =
      "<template id=unused><style>p{color:red} @font-face{font-family:Unused;src:url(denied.ttf)}</style><img src=denied.png><input name=same value=Hidden><svg><image href='https://invalid.test/image'/></svg></template><input name=same value=Visible><p>After</p>"

    resolver = fn request ->
      send(callback_owner, {:unexpected_template_asset, request})
      {:error, :denied}
    end

    assert {:ok, dom} = HtmlParser.parse_detailed(source)

    assert {:ok, %{children: [template, input, paragraph]}} =
             Style.compute_detailed(dom, asset_resolver: resolver)

    assert template.style.display == :none
    assert template.children == []
    assert input.style._form_control.attributes["value"] == "Visible"
    assert paragraph.style.color == {0, 0, 0}
    refute_received {:unexpected_template_asset, _}

    assert {:ok, %{children: [shown, _, _]}} =
             Style.compute_detailed(dom,
               asset_resolver: resolver,
               stylesheets: [
                 {:css, "template#unused{display:block;border:1px solid blue;padding:3pt}"}
               ]
             )

    assert shown.style.display == :block
    assert shown.children == []
    refute_received {:unexpected_template_asset, _}
  end

  test "template DOM callers must retain fragment shape, UTF-8 and static input policy" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    element = %{
      type: :element,
      tag: "template",
      attributes: %{},
      children: [],
      content: %{type: :document_fragment, children: []}
    }

    for malformed <- [
          Map.delete(element, :content),
          %{element | children: [%{type: :text, text: "lost"}]},
          %{element | content: %{type: :document, children: []}},
          %{element | content: %{type: :document_fragment, children: "bad"}}
        ] do
      assert {:error,
              {:invalid_document,
               %{stage: :style, operation: :compute, source: "template", message: message}}} =
               Style.compute_detailed(%{type: :document, children: [malformed]})

      assert message =~ "document_fragment"
    end

    assert {:error,
            {:unsupported_html,
             %{message: event_message, operation: :compute, source: "template"}}} =
             Style.compute_detailed(%{
               type: :document,
               children: [%{element | attributes: %{"onclick" => "run()"}}]
             })

    assert event_message =~ "event handlers"

    for child <- [
          %{type: :text, text: <<255>>},
          %{type: :element, tag: "script", attributes: %{}, children: []},
          %{type: :element, tag: "span", attributes: %{"onclick" => "run()"}, children: []},
          %{
            type: :element,
            namespace: :svg,
            tag: "path",
            attributes: %{"onload" => "run()"},
            children: []
          }
        ] do
      supplied = %{element | content: %{type: :document_fragment, children: [child]}}

      assert {:error, {reason, %{message: message, operation: :compute, source: source}}} =
               Style.compute_detailed(%{type: :document, children: [supplied]})

      assert source in ["text", "script", "span", "path"]

      assert reason in [:invalid_document, :unsupported_html]
      assert message =~ "UTF-8" or message =~ "script" or message =~ "event"
    end

    assert {:error, {:invalid_document, %{stage: :style, operation: :compute, message: message}}} =
             Style.compute_detailed(%{type: :document, children: [%{type: :text, text: <<255>>}]})

    assert message =~ "UTF-8"
  end

  test "data scripts default hidden and author styles expose literal text only" do
    callback_owner = self()
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html =
      "<script type='text/plain' src='denied.json'>p{color:red}<img src='denied.png'>&amp;</script><p>Visible</p>"

    resolver = fn request ->
      send(callback_owner, {:unexpected_script_asset, request})
      {:error, :denied}
    end

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, %{children: [script, paragraph]}} =
             Style.compute_detailed(dom, asset_resolver: resolver)

    assert script.style.display == :none
    assert script.children == []
    assert paragraph.style.color == {0, 0, 0}

    assert {:ok, %{children: [shown, _]}} =
             Style.compute_detailed(dom,
               asset_resolver: resolver,
               stylesheets: [
                 {:css, "script[type='TEXT/PLAIN']{display:block;white-space:pre;color:blue}"}
               ]
             )

    assert shown.style.display == :block
    assert shown.style.color == {0, 0, 1}
    assert [%{type: :text, text: "p{color:red}<img src='denied.png'>&amp;"}] = shown.children
    refute_received {:unexpected_script_asset, _}

    for {attrs, children} <- [
          {%{}, []},
          {%{"type" => "module", "style" => "display:none"}, []},
          {%{"type" => "text/plain", "onclick" => "x"}, []},
          {%{"type" => "text/plain"},
           [%{type: :element, tag: "img", attributes: %{"src" => "denied.png"}, children: []}]}
        ] do
      supplied = %{
        type: :document,
        children: [%{type: :element, tag: "script", attributes: attrs, children: children}]
      }

      assert {:error, {reason, %{message: message}}} =
               Style.compute_detailed(supplied, asset_resolver: resolver)

      assert reason in [:unsupported_html, :invalid_document]
      assert message =~ "script"
      refute_received {:unexpected_script_asset, _}
    end
  end

  test "text decorations preserve originating metrics, color, and propagation boundaries" do
    html = """
    <p style='color:red;text-decoration:underline solid 1pt'>A
    <span style='color:blue;text-decoration:none;font-size:20pt'>B</span>
    <s>C</s><span style='display:inline-block'>D</span><u style='text-decoration:none'>E</u></p>
    """

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = NativeElixirPdfUtilities.HtmlToPdf.Style.compute(dom)
    [paragraph] = styled.children
    [origin] = paragraph.style._text_decorations
    assert origin.lines == [:underline]
    assert origin.color == {1, 0, 0}
    assert origin.thickness == 0.75

    [_, span, strike, atomic, cleared] =
      Enum.reject(paragraph.children, fn node ->
        node.type == :text and String.trim(node.text) == ""
      end)

    assert span.style._text_decorations == [origin]
    assert length(strike.style._text_decorations) == 2
    refute Map.has_key?(atomic.style, :_text_decorations)
    assert cleared.style._text_decorations == [origin]

    for {css, thickness} <- [
          {"font-size:20pt;text-decoration-thickness:0.1em", 2.25},
          {"text-decoration-thickness:0.1em;font-size:20pt", 2.25},
          {"text-decoration-thickness:10%;font-size:20pt", 2.25},
          {"text-decoration-thickness:0", 0.75}
        ] do
      assert {:ok, dom} = HtmlParser.parse("<u style='#{css}'>A</u>")
      assert {:ok, %{children: [element]}} = NativeElixirPdfUtilities.HtmlToPdf.Style.compute(dom)
      assert hd(element.style._text_decorations).thickness == thickness
    end
  end

  test "counter representations cover alphabetic transitions, Roman ranges and signed padding" do
    for {style, number, expected} <- [
          {:decimal, -2, "-2."},
          {:decimal_leading_zero, -3, "-3."},
          {:decimal_leading_zero, 0, "00."},
          {:decimal_leading_zero, 123, "123."},
          {:lower_alpha, 1, "a."},
          {:lower_alpha, 26, "z."},
          {:lower_alpha, 27, "aa."},
          {:upper_alpha, 703, "AAA."},
          {:lower_alpha, 0, "0."},
          {:upper_alpha, -1, "-1."},
          {:lower_roman, 4, "iv."},
          {:upper_roman, 944, "CMXLIV."},
          {:upper_roman, 3999, "MMMCMXCIX."},
          {:lower_roman, 4000, "4000."},
          {:upper_roman, 0, "0."},
          {:disc, 1, "•"},
          {:circle, 1, "◦"},
          {:square, 1, "▪"},
          {:none, 1, ""}
        ] do
      assert CounterStyle.marker(style, number) == expected
    end
  end

  test "list numbering follows styled ownership and preserves children" do
    html = """
    <ol reversed start=' +27 trailing' type='A'>
      <li>First</li><li style='display:none' value='100'>Hidden</li>
      <li style='display:block'>Block</li><li value='4'>Reset</li><li>Last</li>
    </ol>
    """

    assert {:ok, dom} = NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse(html)
    assert {:ok, styled} = NativeElixirPdfUtilities.HtmlToPdf.Style.compute(dom)
    [list] = styled.children
    [first, hidden, block, reset, last] = list.children
    assert [%{text: "First"}] = first.children
    assert hd(first.style._list_marker).text == "AA."
    assert hidden.style.display == :none
    assert block.style.display == :block
    refute Map.has_key?(block.style, :_list_marker)
    assert hd(reset.style._list_marker).text == "D."
    assert hd(last.style._list_marker).text == "C."

    for {attributes, expected} <- [
          {"reversed", ["2.", "1."]},
          {"start='bad'", ["1.", "2."]},
          {"start='-1'", ["-1.", "0."]}
        ] do
      assert {:ok, dom} =
               NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse(
                 "<ol #{attributes}><li>A</li><li style='display:none'>H</li><li>B</li></ol>"
               )

      assert {:ok, styled} = NativeElixirPdfUtilities.HtmlToPdf.Style.compute(dom)

      markers =
        hd(styled.children).children
        |> Enum.reject(&(&1.style.display == :none))
        |> Enum.map(&hd(&1.style._list_marker).text)

      assert markers == expected
    end
  end

  test "list presentational hints yield to author CSS and marker declarations reject ambiguity" do
    for type <- ["1", "a", "A", "i", "I", "invalid"] do
      assert {:ok, dom} =
               NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse(
                 "<ol type='#{type}' style='list-style: square inside'><li type='i' style='list-style-type:none'>A</li></ol>"
               )

      assert {:ok, styled} = NativeElixirPdfUtilities.HtmlToPdf.Style.compute(dom)
      [list] = styled.children
      assert list.style.list_marker_type == :square
      assert list.style.list_style_position == :inside
      assert hd(list.children).style._list_marker == []
    end

    for {tag, type} <- [
          {"ul", "disc"},
          {"ul", "circle"},
          {"ul", "square"},
          {"ul", "A"},
          {"ol", "disc"}
        ] do
      assert {:ok, dom} =
               NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse(
                 "<#{tag} type='#{type}'><li>A</li></#{tag}>"
               )

      assert {:ok, _} = NativeElixirPdfUtilities.HtmlToPdf.Style.compute(dom)
    end

    for value <- [
          "decimal",
          "decimal-leading-zero",
          "lower-alpha",
          "upper-alpha",
          "lower-latin",
          "upper-latin",
          "lower-roman",
          "upper-roman",
          "disc",
          "circle",
          "square",
          "none"
        ] do
      assert {:ok, _} =
               NativeElixirPdfUtilities.Validators.HtmlValidator.parse_css_list_style(
                 "list-style-type",
                 value
               )
    end

    assert {:ok, %{list_style_image_source: "marker.png"}} =
             NativeElixirPdfUtilities.Validators.HtmlValidator.parse_css_list_style(
               "list-style",
               "url(marker.png)"
             )

    for {property, value} <- [
          {"list-style", "circle square"},
          {"list-style-type", "inside"},
          {"list-style-position", "square"},
          {"list-style", "inside outside"},
          {"list-style", "url(marker.png) url(other.png)"},
          {"list-style-type", ""}
        ] do
      assert :error =
               NativeElixirPdfUtilities.Validators.HtmlValidator.parse_css_list_style(
                 property,
                 value
               )

      assert {:error, {:invalid_css, %{stage: :css, message: message}}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<ul style='#{property}:#{value}'><li>A</li></ul>"
               )

      assert message != ""
    end

    assert {:ok, %{list_style_position: :outside}} =
             NativeElixirPdfUtilities.Validators.HtmlValidator.parse_css_list_style(
               "list-style-position",
               "outside"
             )
  end

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.TestSupport.JpegFixture

  test "static semantic and legacy defaults inherit fonts and allow author overrides" do
    html = """
    <address>Address</address><center>Center</center><blockquote cite='source'>Quote</blockquote>
    <figure><figcaption>Caption</figcaption>Figure</figure><hgroup><h2>Title</h2></hgroup><search>Search</search>
    <dl><dt>Term</dt><dd>Description</dd></dl><pre>  Code</pre><listing>Listing</listing>
    <p><abbr>AB</abbr><cite>Cite</cite><dfn>Definition</dfn><data value='7'>Seven</data>
    <time datetime='2026-10-06'>Today</time><code>Code</code><samp>Sample</samp><kbd>Key</kbd>
    <tt>Type</tt><var>Variable</var><big>Big</big><small>Small</small></p>
    <pre style='font-family:DejaVu Sans;white-space:normal;margin:0'>Override</pre>
    """

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, styled} = Style.compute(dom, system_font_discovery: false)

    [address, center, quote, figure, _hgroup, _search, dl, pre, listing, paragraph, override] =
      styled.children

    assert address.style.font_style == :italic
    assert center.style.text_align == :center
    assert quote.style.margin.left == 30.0
    assert figure.style.margin.right == 30.0
    assert Enum.at(dl.children, 1).style.margin.left == 30.0
    assert pre.style.white_space == :pre
    assert listing.style.font_face.family == "DejaVu Sans Mono"
    assert pre.style.font_face.source == :bundled
    assert override.style.white_space == :normal
    assert override.style.font_face.family == "DejaVu Sans"
    assert override.style.margin.top == 0

    assert Enum.find(paragraph.children, &(Map.get(&1, :tag) == "cite")).style.font_style ==
             :italic

    assert Enum.find(paragraph.children, &(Map.get(&1, :tag) == "small")).style.font_size <
             paragraph.style.font_size
  end

  test "zero-margin bodies retain padding, backgrounds, and layout containers" do
    for {fixture, display, padding} <- [
          {"body_padding_background.html", :block, 30.0},
          {"body_grid.html", :grid, 12.0}
        ] do
      html = File.read!("test/fixtures/html_to_pdf/browser_parity/" <> fixture)
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [%{tag: "body", style: style}]}} = Style.compute_detailed(dom)
      assert style.display == display
      assert style.margin == edges(0.0)
      assert style.padding == edges(padding)
      assert style.background_color != nil
    end
  end

  test "oversized CSS numbers return diagnostics in every numeric value family" do
    alias NativeElixirPdfUtilities.HtmlToPdf
    number = String.duplicate("9", 400)

    for declaration <- [
          "margin-left: #{number}pt",
          "padding: #{number}in",
          "width: #{number}%",
          "grid-template-columns: #{number}fr",
          "color: rgb(#{number}%, 0%, 0%)",
          "color: rgba(0, 0, 0, #{number}%)",
          "letter-spacing: #{number}em",
          "letter-spacing: #{number}rem",
          "letter-spacing: #{number}px",
          "background-position: #{number}% 0%",
          "font-size: #{number}rem",
          "line-height: #{number}",
          "aspect-ratio: #{number} / 1",
          # Representable as a float, but not as a usable CSS length.
          "margin-left: #{String.duplicate("9", 308)}in",
          "--distance: #{number}pt; margin-left: var(--distance)"
        ] do
      assert {:error, {reason, diagnostic}} = HtmlToPdf.render("<p style='#{declaration}'>A</p>")
      assert reason in [:invalid_css, :invalid_document]
      assert diagnostic.reason == reason
      assert diagnostic.stage in [:css, :style]
      assert is_binary(diagnostic.message)
      assert diagnostic.module == HtmlToPdf
    end

    assert {:ok, _pdf} =
             HtmlToPdf.render("<p style='margin-left: 1in; letter-spacing: 0.5pt'>A</p>")
  end

  test "JPEG Adobe markers preserve CMYK polarity before and after the frame" do
    for file <- ["adobe_cmyk.jpg", "adobe_ycck.jpg"] do
      data = File.read!(Paths.fixture("images/jpeg/" <> file))
      assert {:ok, style} = image_style("data:image/jpeg;base64," <> Base.encode64(data), [])
      assert style.image.color_space == :device_cmyk
      assert style.image.inverted_cmyk
    end

    alias NativeElixirPdfUtilities.Validators.HtmlValidator
    frame = <<255, 192, 0, 20, 8, 1::16, 1::16, 4, 1, 17, 0, 2, 17, 0, 3, 17, 0, 4, 17, 0>>
    adobe = <<255, 238, 0, 14, "Adobe", 100::16, 0::16, 0::16, 0>>

    <<255, 216, ^frame::binary, tail::binary>> = JpegFixture.baseline(1, 1, 4)

    for body <- [adobe <> frame, frame <> adobe, <<255>> <> adobe <> frame] do
      assert {:ok, %{inverted_cmyk: true}} =
               HtmlValidator.jpeg_metadata(<<255, 216>> <> body <> tail)
    end

    assert HtmlValidator.jpeg_metadata("") == :error
    assert HtmlValidator.jpeg_metadata(<<255, 216, 255, 217>>) == :error
    assert HtmlValidator.jpeg_metadata(<<255, 216, 255, 192, 0, 3, 0, 255, 217>>) == :error
  end

  test "RGB PNG transparent colors generate alpha masks and malformed tRNS fails" do
    signature = <<137, 80, 78, 71, 13, 10, 26, 10>>
    header = png_chunk("IHDR", <<2::32, 1::32, 8, 2, 0, 0, 0>>)
    pixels = png_chunk("IDAT", :zlib.compress(<<0, 255, 0, 0, 0, 0, 255>>))
    ending = png_chunk("IEND", "")
    png = signature <> header <> png_chunk("tRNS", <<255::16, 0::16, 0::16>>) <> pixels <> ending
    assert {:ok, style} = image_style("data:image/png;base64," <> Base.encode64(png), [])
    assert style.image.data == <<255, 0, 0, 0, 0, 255>>
    assert style.image.alpha_data == <<0, 255>>

    for invalid <- [<<1>>, <<256::16, 0::16, 0::16>>] do
      png = signature <> header <> png_chunk("tRNS", invalid) <> pixels <> ending
      html = "<img src='data:image/png;base64,#{Base.encode64(png)}'>"

      assert {:error, {:invalid_document, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html)

      assert diagnostic.stage == :style
      assert diagnostic.message =~ "tRNS"
    end
  end

  test "descendants inherit computed custom properties and invalid values stay invalid" do
    assert {:ok, dom} =
             HtmlParser.parse(
               "<div style='--base:red;--alias:var(--base);--unused:var(--missing)'><p style='--base:blue;color:var(--alias)'>Inherited</p><p style='--base:green;--alias:var(--base);color:var(--alias)'>Local</p></div>"
             )

    assert {:ok, %{children: [parent]}} = Style.compute_detailed(dom)
    [inherited, local] = parent.children
    assert inherited.style.color == {1, 0, 0}
    assert local.style.color == {0, 0.5019607843, 0}
    assert parent.style._custom_properties["--alias"] == "red"
    assert inherited.style._custom_properties["--unused"] == nil

    assert {:ok, dom} =
             HtmlParser.parse(
               "<div style='--alias:var(--missing)'><p style='--missing:red;color:var(--alias)'>Invalid</p></div>"
             )

    assert {:error, _} = Style.compute_detailed(dom)
  end

  test "variable functions inside quoted strings remain literal" do
    alias NativeElixirPdfUtilities.Validators.HtmlValidator

    for value <- [~S|"var(--missing)"|, ~S|'var(--missing)'|, ~S|"escaped \" var(--missing)"|] do
      assert {:ok, ^value} = HtmlValidator.resolve_css_variables(value, %{}, %{})
    end

    assert {:ok, ~S|"literal var(--ink)" red blue|} =
             HtmlValidator.resolve_css_variables(
               ~S|"literal var(--ink)" var(--ink) var(--other)|,
               %{"--ink" => "red", "--other" => "blue"},
               %{}
             )

    assert {:ok, _pdf} =
             NativeElixirPdfUtilities.HtmlToPdf.render(
               ~S|<style>p::before{content:"var(--missing)"}</style><p></p>|
             )
  end

  test "ordinary blocks inherit weight while explicit and semantic weights remain effective" do
    for tag <- ["div", "section", "p", "article", "aside", "header"] do
      assert {:ok, dom} =
               HtmlParser.parse(
                 "<div style='font-weight:700'><#{tag}>Inherited</#{tag}><p style='font-weight:400'>Normal</p><strong>Strong</strong></div>"
               )

      assert {:ok, %{children: [parent]}} = Style.compute_detailed(dom)
      [inherited, normal, strong] = parent.children
      assert inherited.style.font_weight == 700
      assert normal.style.font_weight == 400
      assert strong.style.font_weight == 700
    end
  end

  test "font resolution cache respects weight, style and separate configured registries" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <div style="font-family: 'Provided Sans'">
             <p style="font-family: 'Provided Sans'">Normal</p><p style="font-weight: bold">Bold</p>
             <p style="font-style: italic">Italic</p><p style="font-family: 'Provided Sans'">Repeated</p>
             </div>
             """)

    directory = Application.app_dir(:native_elixir_pdf_utilities, "priv/fonts/dejavu")

    faces = [
      %{family: "Provided Sans", path: Path.join(directory, "DejaVuSans.ttf"), weight: 400},
      %{family: "Provided Sans", path: Path.join(directory, "DejaVuSans-Bold.ttf"), weight: 700},
      %{
        family: "Provided Sans",
        path: Path.join(directory, "DejaVuSans-Oblique.ttf"),
        weight: 400,
        style: :italic
      }
    ]

    assert {:ok, %{children: children}} = Style.compute_detailed(dom, fonts: faces)
    [container] = Enum.filter(children, &(&1.type == :element))
    [normal, bold, italic, repeated] = Enum.filter(container.children, &(&1.type == :element))
    assert normal.style.font_face.weight == 400
    assert bold.style.font_face.weight == 700
    assert italic.style.font_face.style == :italic
    assert repeated.style.font_face == normal.style.font_face

    replacement = [
      %{family: "Provided Sans", path: Path.join(directory, "DejaVuSans-Bold.ttf"), weight: 400}
    ]

    assert {:ok, %{children: children}} = Style.compute_detailed(dom, fonts: replacement)
    [other] = Enum.filter(children, &(&1.type == :element))
    assert other.style.font_face.id != container.style.font_face.id
  end

  test "class matching preserves whitespace, duplicates, case and ancestor negations" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <style>
             .outer .A.B:not(.excluded) { color: red; }
             .outer:not(.missing) > .excluded { color: blue; }
             .a { font-weight: bold; }
             </style><div class="outer outer"><p class="A\t A\nB">First</p><p class="A B excluded">Second</p></div>
             """)

    assert {:ok, %{children: [container]}} = Style.compute_detailed(dom)
    [first, second] = container.children
    assert first.style.color == {1, 0, 0}
    assert second.style.color == {0, 0, 1}
    assert first.style.font_weight == 400
    assert second.style.font_weight == 400
    refute Map.has_key?(first, :_selector_classes)
  end

  test "positional selectors count top-level source elements without an element parent" do
    markup =
      "text<!--ignored--><p id=p1>One</p><span id=s1>Other</span><template><p>Unused</p></template><p id=p2>Two</p><p id=p3>Three</p>"

    for {selector, expected} <- [
          {"p:first-child", ~w(p1)},
          {"p:last-child", ~w(p3)},
          {"p:first-of-type", ~w(p1)},
          {"p:last-of-type", ~w(p3)},
          {"p:only-child", []},
          {"p:only-of-type", []},
          {"span:only-of-type", ~w(s1)},
          {"p:nth-child(4)", ~w(p2)},
          {"p:nth-last-child(2)", ~w(p2)},
          {"p:nth-of-type(2)", ~w(p2)},
          {"p:nth-last-of-type(2)", ~w(p2)},
          {"p:nth-child(2n+1)", ~w(p1 p3)},
          {"p:nth-last-of-type(-n+2)", ~w(p2 p3)},
          {"p:nth-of-type(0)", []},
          {"p:nth-child(-2)", []},
          {"p:not(:first-child)", ~w(p2 p3)},
          {"* > p:first-child", []},
          {"div p:last-child", []}
        ] do
      assert {:ok, dom} = HtmlParser.parse(markup)

      assert {:ok, styled} =
               Style.compute_detailed(dom, stylesheets: [{:css, selector <> "{color:red}"}])

      matched =
        for node <- styled.children,
            match?(%{type: :element}, node),
            node.style.color == {1, 0, 0},
            do:
              Map.fetch!(
                %{"One" => "p1", "Other" => "s1", "Two" => "p2", "Three" => "p3"},
                hd(node.children).text
              )

      assert matched == expected, selector
    end

    for selector <-
          ~w|first-child last-child first-of-type last-of-type only-child only-of-type nth-child(1) nth-last-child(1) nth-of-type(1) nth-last-of-type(1)| do
      assert {:ok, dom} = HtmlParser.parse("<p>Single root</p>")

      assert {:ok, %{children: [paragraph]}} =
               Style.compute_detailed(dom,
                 stylesheets: [{:css, "p:" <> selector <> "{color:red}"}]
               )

      assert paragraph.style.color == {1, 0, 0}, selector
    end

    # Hidden source style elements still occupy a position in the fragment.
    assert {:ok, dom} =
             HtmlParser.parse(
               "<style>p:first-child{color:red}p:nth-child(2){font-weight:bold}</style><p>After style</p>"
             )

    assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(dom)
    assert paragraph.style.color == {0, 0, 0}
    assert paragraph.style.font_weight == 700
  end

  test "document elements match singleton child and type positions" do
    for selector <-
          ~w|first-child last-child first-of-type last-of-type only-child only-of-type nth-child(1) nth-last-child(1) nth-of-type(1) nth-last-of-type(1) nth-child(odd) nth-last-child(-n+1)| do
      assert {:ok, dom} =
               HtmlParser.parse(
                 "<!doctype html><html><head></head><body><p>Root</p></body></html>"
               )

      assert {:ok, %{children: [html]}} =
               Style.compute_detailed(dom,
                 stylesheets: [{:css, "html:" <> selector <> "{color:red}"}]
               )

      assert html.style.color == {1, 0, 0}, selector
    end

    for selector <-
          ~w|nth-child(0) nth-child(2) nth-last-child(even) nth-of-type(-1) not(:only-child)| do
      assert {:ok, dom} = HtmlParser.parse("<html><head></head><body><p>Root</p></body></html>")

      assert {:ok, %{children: [html]}} =
               Style.compute_detailed(dom,
                 stylesheets: [{:css, "html:" <> selector <> "{color:red}"}]
               )

      assert html.style.color == {0, 0, 0}, selector
    end
  end

  test "the virtual fragment body is outside the source sibling table" do
    opts = [
      stylesheets: [{:css, "body:first-child{color:red}body:nth-child(1){font-weight:bold}"}]
    ]

    assert {:ok, fragment} = HtmlParser.parse("<p>Fragment</p>")
    assert {:ok, %{children: [paragraph]}} = Style.compute_detailed(fragment, opts)
    assert paragraph.style.color == {0, 0, 0}
    assert paragraph.style.font_weight == 400

    assert {:ok, document} = HtmlParser.parse("<body><p>Explicit body</p></body>")
    assert {:ok, %{children: [body]}} = Style.compute_detailed(document, opts)
    assert body.style.color == {1, 0, 0}
    assert body.style.font_weight == 700
  end

  test "selector candidates preserve grouped specificity, source order and general selectors" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <style>
             * { color: black; }
             [data-tone] { background-color: blue; }
             :not(.excluded) { letter-spacing: 1pt; }
             .selected, #target { color: red; padding: 1pt; padding: 2pt; }
             .scope .selected { color: blue; }
             .missing #target { color: green; }
             .selected { font-size: 11pt; }
             .selected { font-size: 12pt; }
             .scope > p.selected[data-tone] { font-weight: bold; }
             </style><div class="scope"><p id="target" class="selected selected" data-tone="yes">First</p><p class="selected excluded">Second</p><span>Third</span></div>
             """)

    assert {:ok, %{children: [container]}} = Style.compute_detailed(dom)
    [first, second, third] = container.children
    assert first.style.color == {1, 0, 0}
    assert second.style.color == {0, 0, 1}
    assert first.style.background_color == {0, 0, 1}
    assert second.style.background_color == nil
    assert first.style.padding.left == 2.0
    assert first.style.font_size == 12.0
    assert first.style.font_weight == 700
    assert second.style.font_weight == 400
    assert third.style.color == {0, 0, 0}
  end

  test "mixed normal and pseudo selectors keep their own specificity and counters" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <style>
             div { counter-reset: item; }
             p, #target::before { color: red; }
             #target { color: blue; }
             p::before, p::after { content: counter(item); counter-increment: item; }
             p::before { color: green; }
             </style><div><p id="target">Body</p><p>Next</p></div>
             """)

    assert {:ok, %{children: [container]}} = Style.compute_detailed(dom)
    [first, second] = container.children
    [before, body, after_content] = first.children
    assert first.style.color == {0, 0, 1}
    assert body.style.color == {0, 0, 1}
    assert before.style.color == {1, 0, 0}
    assert hd(before.children).text == "1"
    assert hd(after_content.children).text == "2"
    assert hd(hd(second.children).children).text == "3"
  end

  test "repeated inline declarations resolve against each element and each render" do
    html = """
    <div style="--ink: red; --pad: 10pt; font-size: 10pt"><p style="color: var(--ink); padding: var(--pad)">First</p></div>
    <div style="--ink: blue; --pad: 20pt; font-size: 20pt"><p style="color: var(--ink); padding: var(--pad)">Second</p></div>
    <div style="display: none"><p style="not valid CSS">Skipped</p></div>
    """

    for source <- [html, String.replace(html, "--ink: red", "--ink: green")] do
      assert {:ok, dom} = HtmlParser.parse(source)
      assert {:ok, %{children: children}} = Style.compute_detailed(dom)
      [first, second, hidden] = Enum.filter(children, &(&1.type == :element))
      assert hidden.children == []
      assert hidden.style.display == :none
      [first_text] = first.children
      [second_text] = second.children
      assert first_text.style.color != second_text.style.color
      assert first_text.style.padding.left == 10.0
      assert second_text.style.padding.left == 20.0
    end

    assert {:ok, invalid} = HtmlParser.parse("<p style='not valid CSS'>Invalid</p>")

    assert {:error, {:invalid_css, %{stage: :css, reason: :invalid_css, message: message}}} =
             Style.compute_detailed(invalid)

    assert is_binary(message)
  end

  test "compute applies default paragraph styles" do
    dom = %{
      type: :document,
      children: [
        %{type: :element, tag: "p", attributes: %{}, children: [%{type: :text, text: "Hello"}]}
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.display == :block
    assert paragraph.style.font_family == "DejaVu Sans"
    assert paragraph.style.font_size == 12.0
    assert paragraph.style.color == {0, 0, 0}
    assert paragraph.style.break_inside == :auto
  end

  test "an explicit generic family on html resolves from the system and remains inherited" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <style>html { font-family: sans-serif; }</style>
             <html><p>some text</p></html>
             """)

    assert {:ok, styled_tree} = Style.compute(dom)
    [paragraph] = styled_tree.children
    [text] = paragraph.children

    assert paragraph.style.font_families == ["sans-serif"]
    assert text.style.font_families == ["sans-serif"]
    assert paragraph.style.font_face.source == :system
    assert text.style.font_family == paragraph.style.font_family
    assert text.style.font_face.id == paragraph.style.font_face.id
  end

  test "compute treats section and article as block containers" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "section",
          attributes: %{"style" => "display: flex"},
          children: [
            %{
              type: :element,
              tag: "article",
              attributes: %{},
              children: [%{type: :text, text: "Trim"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [section] = styled_tree.children
    [article] = section.children

    assert section.style.display == :flex
    assert article.style.display == :block
  end

  test "HTML type selectors fold ASCII case without altering authored attributes" do
    html = """
    <style>
      input[type="SEARCH"] { color: blue; }
      input[value="AB"] { color: red; }
      input[data-kind="NOTE"] { background: #ffff00; }
    </style>
    <div><input type="search" value="A&#10;B" data-kind="note"><input type="SeArCh" value="AB" data-kind="NOTE"></div>
    """

    assert {:ok, dom} = HtmlParser.parse(html)
    assert {:ok, tree} = Style.compute(dom)
    [container] = tree.children
    [raw, literal] = container.children
    assert raw.style._form_control.attributes["type"] == "search"
    assert literal.style._form_control.attributes["type"] == "SeArCh"
    assert raw.style.color == {0, 0, 1}
    assert literal.style.color == {1, 0, 0}
    assert raw.style.background_color == {1, 1, 1}
    assert literal.style.background_color == {1, 1, 0}
    assert raw.style._form_control.initial_state.value == "AB"
    assert raw.style._form_control.attributes["value"] == "A\nB"
    assert [%{text: "AB"}] = raw.children
  end

  test "attribute operators and case flags participate in cascade and negation" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <style>
               p[data-kind^="ab" i] { color:red; }
               p[data-kind$="EF" I] { background-color:blue; }
               p[data-kind*="Cd" s] { font-weight:bold; }
               p[title~="two" i]:not([title="one Two three" s]) { color:blue; }
               p[title~="one Two"] { color:red; }
               p[data-lang|=EN i] { color:blue; }
               p[data-lang|=EN S] { background-color:red; }
               p[data-kind^=""] { font-size:99pt; }
             </style>
             <p data-kind="AbCd-Ef">Substring</p>
             <p title="one&#9;Two&#10;three">Words</p>
             <p data-lang="en-US">Language</p>
             <p>Missing</p>
             """)

    assert {:ok, tree} = Style.compute(dom)
    [substring, words, language, missing] = tree.children
    assert substring.style.color == {1, 0, 0}
    assert substring.style.background_color == {0, 0, 1}
    assert substring.style.font_weight == 700
    assert substring.style.font_size != 99
    assert words.style.color == {0, 0, 1}
    assert language.style.color == {0, 0, 1}
    assert Map.get(language.style, :background_color) == nil
    assert missing.style.color == {0, 0, 0}
  end

  test "caller supplied trees reject non-string and invalid UTF-8 attributes before matching" do
    for attributes <- [
          %{"lang" => nil},
          %{"lang" => 42},
          %{lang: "en"},
          %{"lang" => <<255>>},
          %{<<255>> => "en"}
        ] do
      dom = %{
        type: :document,
        children: [
          %{
            type: :element,
            tag: "p",
            attributes: attributes,
            children: [%{type: :text, text: "Text"}]
          }
        ]
      }

      assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(dom)
      assert diagnostic.stage == :style
      assert diagnostic.reason == :invalid_document
      assert diagnostic.operation == :compute
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlValidator
      assert diagnostic.source == "p"
      assert diagnostic.message =~ "UTF-8 string names and values"
    end
  end

  test "caller supplied trees reject malformed attribute namespace maps" do
    for namespaces <- [nil, [], "namespace", 42] do
      assert {:ok, dom} = HtmlParser.parse(~s(<input type="text" value="Text">))

      dom =
        update_in(dom, [:children, Access.at(0)], &Map.put(&1, :attribute_namespaces, namespaces))

      assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(dom)
      assert diagnostic.stage == :style
      assert diagnostic.reason == :invalid_document
      assert diagnostic.operation == :compute
      assert diagnostic.source == "input"
      assert diagnostic.message == "attribute_namespaces must be a map"
    end
  end

  test "text and search values sanitize after selectors match the original attribute" do
    for type <- ["text", "search", "SEARCH"] do
      html = """
      <style>input[value="AB"] { color: red; }</style>
      <div><input type="#{type}" value="A&#13;&#10;B"><input type="#{type}" value="AB"></div>
      """

      assert {:ok, dom} = HtmlParser.parse(html)
      assert [%{tag: "style"}, %{children: [raw, _reference]}] = dom.children
      assert raw.attributes["value"] == "A\r\nB"
      assert {:ok, tree} = Style.compute(dom)
      [container] = tree.children
      [prepared, reference] = container.children
      assert prepared.style.color == {0, 0, 0}
      assert reference.style.color == {1, 0, 0}
      assert prepared.style._form_control.initial_state.value == "AB"
      assert prepared.style._form_control.attributes["value"] == "A\r\nB"
      assert reference.style._form_control.initial_state.value == "AB"
      assert [%{text: "AB"}] = prepared.children
    end
  end

  test "compute turns form state and values into static styled content" do
    html = """
    <style>
      input[disabled] { color: #777777; }
      select { width: 90pt; }
    </style>
    <div>
      <input type="text" value="Alice">
      <input type="checkbox" checked disabled>
      <input type="checkbox">
      <input type="radio" checked>
      <input type="radio">
      <select><option>Pending</option><option selected>Approved</option></select>
      <select><option>First</option><option>Second</option></select>
      <textarea>Line one
    Line two</textarea>
      <textarea value="Fallback"></textarea>
      <button><strong>Save</strong></button>
      <button value="Fallback button"></button>
    </div>
    """

    assert {:ok, dom} = HtmlParser.parse(html)
    # Pin the bundled DejaVu fallback used by the metric assertions below.
    assert {:ok, styled_tree} = Style.compute(dom, system_font_discovery: false)
    [container] = styled_tree.children

    [
      text,
      checked_box,
      empty_box,
      checked_radio,
      empty_radio,
      selected,
      first,
      textarea,
      textarea_fallback,
      button,
      button_fallback
    ] = Enum.reject(container.children, &match?(%{type: :text}, &1))

    assert text.style.form_kind == :text
    # The original default DejaVu source typeface allocates 179 CSS pixels.
    # Its six-point native border/padding leaves this content allocation.
    assert text.style.width == 179 * 0.75 - 6.0
    assert Enum.map(text.children, & &1.text) == ["Alice"]
    assert Enum.map(checked_box.children, & &1.text) == ["☒"]
    assert checked_box.style.color == {0.4666666666666667, 0.4666666666666667, 0.4666666666666667}
    assert Enum.map(empty_box.children, & &1.text) == ["☐"]
    assert Enum.map(checked_radio.children, & &1.text) == ["●"]
    assert Enum.map(empty_radio.children, & &1.text) == ["○"]
    assert Enum.map(selected.children, & &1.text) == ["Approved"]
    assert selected.style.width == 90.0
    assert selected.style.padding.left == 3.0
    assert selected.style.padding.right == 12.0
    assert Enum.map(first.children, & &1.text) == ["First"]
    assert Enum.map(textarea.children, & &1.text) == ["Line one\nLine two"]
    assert textarea.style.white_space == :pre_wrap
    assert Enum.map(textarea_fallback.children, & &1.text) == [""]
    assert [%{tag: "strong", children: [%{text: "Save"}]}] = button.children
    assert button.style.font_weight == 400
    assert button.style.font_size == 10.0
    assert button.style.box_sizing == :border_box
    assert button.style.font_families == ["Arial", "sans-serif"]
    assert button_fallback.children == []
  end

  test "button author typography overrides the browser defaults" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <div style="font-size: 20pt; line-height: 30pt; font-style: italic">
               <button style="font-family: serif; font-size: 14pt; font-weight: 700; line-height: 18pt">Save</button>
             </div>
             """)

    assert {:ok, tree} = Style.compute(dom)
    [container] = tree.children
    [button] = Enum.reject(container.children, &match?(%{type: :text}, &1))
    assert button.style.font_families == ["serif"]
    assert button.style.font_size == 14.0
    assert button.style.font_weight == 700
    assert button.style.font_style == :normal
    assert button.style.line_height == 18.0
  end

  test "compute honors display none on static form controls" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<div><input type="text" value="Secret" style="display: none"></div>)
             )

    assert {:ok, %{children: [%{children: [%{style: %{display: :none}, children: []}]}]}} =
             Style.compute(dom)
  end

  test "compute does not load a directly hidden image" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<div><img src="data:image/png;base64,#{Base.encode64(png_fixture(1, 1))}" style="display: none" /></div>)
             )

    assert {:ok, %{children: [%{children: [%{style: %{display: :none}}]}]}} = Style.compute(dom)
  end

  test "hidden inputs suppress author display, generated content, and image resolution" do
    callback_owner = self()

    for type <- ["hidden", "HIDDEN", "HiDdEn"] do
      html = """
      <style>
      input { display: block !important; background-image: url(missing.png); }
      input::before { content: attr(value); }
      input::after { content: "After"; }
      </style>
      <div><input type="#{type}" name="reference" value="Secret" disabled
        style="display:inline-block!important;width:90pt;height:60pt;border:3pt solid red"></div>
      """

      assert {:ok, dom} = HtmlParser.parse(html)

      resolver = fn source ->
        send(callback_owner, {:hidden_image_resolved, source})
        {:error, :missing_asset}
      end

      assert {:ok, tree} = Style.compute(dom, asset_resolver: resolver)
      [container] = tree.children
      [input] = container.children
      assert input.style.form_kind == :hidden
      assert input.style.display == :none
      assert input.children == []
      refute Map.has_key?(input.style, :_form_control)
      refute_received {:hidden_image_resolved, _source}
    end
  end

  test "compute applies body inherited styles to document fragments" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: "body { font-family: 'Fixture Sans', Helvetica; color: #102038; }"
            }
          ]
        },
        %{
          type: :element,
          tag: "section",
          attributes: %{},
          children: [%{type: :text, text: "Fragment"}]
        }
      ]
    }

    assert {:ok, styled_tree} =
             Style.compute(dom, fonts: [%{family: "Fixture Sans", path: ttf_font_path!()}])

    [section] = styled_tree.children
    [text] = section.children

    assert section.style.font_family == "Fixture Sans"
    assert text.style.font_face.id == section.style.font_face.id
    assert section.style.color == {0.06274509803921569, 0.12549019607843137, 0.2196078431372549}
  end

  test "compute honors display none on implicit fragment body" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "body { display: none; }"}]
        },
        %{
          type: :element,
          tag: "section",
          attributes: %{},
          children: [%{type: :text, text: "Hidden"}]
        }
      ]
    }

    assert {:ok, %{children: []}} = Style.compute(dom, [])
  end

  test "compute accepts print CSS properties and transformed text" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "section",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "style",
              attributes: %{},
              children: [
                %{
                  type: :text,
                  text:
                    "* { box-sizing: border-box; } .label:last-child { page-break-after: auto; } .label span:nth-child(2) { display: block; margin-bottom: 2px; }"
                }
              ]
            },
            %{
              type: :element,
              tag: "div",
              attributes: %{
                "class" => "label",
                "style" =>
                  "background: #ffffff; border: 1px dashed #ccd6e1; width: min(100%, 360px); word-break: break-word; white-space: pre-line; letter-spacing: 0.05em; page-break-inside: avoid; text-transform: uppercase"
              },
              children: [
                %{
                  type: :element,
                  tag: "span",
                  attributes: %{},
                  children: [%{type: :text, text: "one"}]
                },
                %{
                  type: :element,
                  tag: "span",
                  attributes: %{},
                  children: [%{type: :text, text: "two"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [section] = styled_tree.children
    [label] = section.children
    [first, second] = label.children
    [first_text] = first.children

    assert label.style.background_color == {1.0, 1.0, 1.0}
    assert label.style.border_widths == %{top: 0.75, right: 0.75, bottom: 0.75, left: 0.75}
    assert label.style.break_after == :auto
    assert label.style.word_break == :break_word
    assert label.style.width == {:min, [{:percent, 1.0}, 270.0]}
    assert first_text.text == "ONE"
    assert second.style.display == :block
    assert second.style.margin_after == 1.5
  end

  test "compute skips descendants for display none elements" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{type: :text, text: ".hidden { display: none; }"}
          ]
        },
        %{
          type: :element,
          tag: "div",
          attributes: %{"class" => "hidden"},
          children: [
            %{
              type: :element,
              tag: "img",
              attributes: %{"src" => "data:image/png;base64,#RIGHTITEMQRCODE#"},
              children: []
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [hidden] = styled_tree.children

    assert hidden.style.display == :none
    assert hidden.children == []

    hidden_body_dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "body",
          attributes: %{"style" => "display: none"},
          children: [
            %{
              type: :element,
              tag: "img",
              attributes: %{"src" => "data:image/png;base64,#RIGHTITEMQRCODE#"},
              children: []
            }
          ]
        }
      ]
    }

    assert {:ok, %{children: []}} = Style.compute(hidden_body_dom, [])
  end

  test "leading decimal lengths apply to borders, spacing and sizes" do
    style =
      style_for!(
        "div",
        "font-size: 10pt; border: .5px solid black; padding: .5pt; margin: -.5pt; width: .5in; letter-spacing: .05em"
      )

    assert style.border_widths == edges(0.75)
    assert style.padding == edges(0.5)
    assert style.margin == edges(-0.5)
    assert style.width == 36.0
    assert style.letter_spacing == 0.5
  end

  test "compute covers print CSS alternate values and invalid branches" do
    assert style_for!("div", "background: none").background_color == nil
    assert style_for!("div", "display: grid; justify-self: center").justify_self == :center
    assert style_for!("div", "letter-spacing: normal").letter_spacing == 0.0
    assert style_for!("div", "letter-spacing: 0").letter_spacing == 0.0
    assert style_for!("div", "letter-spacing: 2px").letter_spacing == 1.5
    assert style_for!("div", "font-size: 10pt; letter-spacing: 0.05em").letter_spacing == 0.5
    assert style_for!("div", "white-space: nowrap").white_space == :nowrap
    assert style_for!("div", "word-wrap: normal").overflow_wrap == :normal
    assert style_for!("div", "overflow-wrap: anywhere").overflow_wrap == :anywhere
    assert style_for!("div", "break-inside: avoid").break_inside == :avoid
    assert style_for!("div", "page-break-inside: auto").break_inside == :auto
    assert style_for!("div", "border: 0").border_widths == edges(0.0)
    assert style_for!("div", "border-right: 0").border_widths.right == 0.0
    assert style_for!("div", "display: grid; grid-template-columns: 1fr  auto").display == :grid

    minmax =
      style_for!(
        "div",
        "display: grid; grid-template-columns: minmax(80pt, 1fr) minmax(auto, 120pt)"
      )

    assert minmax.grid_template_columns == [
             {:minmax, {:length, 80.0}, {:fr, 1.0}},
             {:minmax, :auto, {:length, 120.0}}
           ]

    assert style_for!("div", "width: min(100%, min(10px, 20px))").width ==
             {:min, [{:percent, 1.0}, {:min, [7.5, 15.0]}]}

    lowercase = text_for_style("text-transform: lowercase", "MiXeD")
    capitalized = text_for_style("text-transform: capitalize", "hello world")
    unchanged = text_for_style("text-transform: none", "MiXeD")

    assert lowercase == "mixed"
    assert capitalized == "Hello World"
    assert unchanged == "MiXeD"

    invalid_styles = [
      "box-sizing: padding-box",
      "text-transform: sideways",
      "letter-spacing: wide",
      "white-space: invalid",
      "page-break-inside: always",
      "float: left",
      "display: grid; grid-template-columns: minmax()",
      "display: grid; grid-template-columns: minmax(1fr, 2fr)",
      "display: grid; grid-template-columns: minmax(minmax(0, 1fr), 2fr)",
      "display: grid; grid-template-columns: repeat(2, bad)",
      "width: min(100%, nope)",
      "width: min()",
      "width: min",
      "background: linear-gradient(red, blue)"
    ]

    for style <- invalid_styles do
      assert style_for("div", style) == {:error, :invalid_document}
    end
  end

  test "compute resolves dependent values against final font size and color" do
    early_letter_spacing = style_for!("div", "letter-spacing: 1em; font-size: 20pt")
    late_letter_spacing = style_for!("div", "font-size: 20pt; letter-spacing: 1em")

    assert early_letter_spacing.letter_spacing == 20.0
    assert late_letter_spacing.letter_spacing == 20.0

    final_font_size =
      style_for!(
        "div",
        "font-size: 20pt; letter-spacing: 1em !important; font-size: 10pt"
      )

    assert final_font_size.letter_spacing == 10.0

    early_current_color = style_for!("div", "background: currentColor; color: red")
    late_current_color = style_for!("div", "color: red; background: currentColor")

    assert early_current_color.background_color == {1, 0, 0}
    assert late_current_color.background_color == {1, 0, 0}

    important_color =
      style_for!("div", "background: currentColor; color: red !important; color: blue")

    assert important_color.background_color == {1, 0, 0}

    implicit_border_color =
      style_for!("div", "border-width: 1pt; border-style: solid; color: blue")

    assert implicit_border_color.border_color == {0, 0, 1}
    assert implicit_border_color.border_colors == edges({0, 0, 1})

    inherited_current_color = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{"style" => "color: red"},
          children: [
            %{
              type: :element,
              tag: "a",
              attributes: %{
                "href" => "mailto:team@example.com",
                "style" => "color: currentColor"
              },
              children: [%{type: :text, text: "Inherited link color"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(inherited_current_color)
    [parent] = styled_tree.children
    [link] = parent.children
    assert link.style.color == {1, 0, 0}
  end

  test "compute resolves rem lengths against the root element's final font size" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "html",
          attributes: %{"style" => "font-size: 2rem; line-height: 1rem"},
          children: [
            %{
              type: :element,
              tag: "body",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "div",
                  attributes: %{
                    "style" => """
                    width: 1rem;
                    height: 2rem;
                    min-width: 3rem;
                    max-height: 4rem;
                    padding: 0.5rem;
                    margin: 0.25rem;
                    border: 0.1rem solid;
                    border-radius: 0.2rem;
                    display: grid;
                    gap: 0.3rem;
                    grid-template-columns: 1rem minmax(2rem, 3rem);
                    font-size: 0.5rem;
                    line-height: 1rem;
                    letter-spacing: 0.1rem;
                    """
                  },
                  children: [%{type: :text, text: "Root-relative lengths"}]
                },
                %{
                  type: :element,
                  tag: "p",
                  attributes: %{"style" => "margin: 0"},
                  children: [%{type: :text, text: "Root element rem"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom)
    assert [%{tag: "body"} = body] = styled_tree.children
    [element, root_relative_line_height] = body.children

    assert element.style.width == 24.0
    assert element.style.height == 48.0
    assert element.style.min_width == 72.0
    assert element.style.max_height == 96.0
    assert element.style.padding == edges(12.0)
    assert element.style.margin == edges(6.0)

    Enum.each(element.style.border_widths, fn {_side, width} ->
      assert_in_delta width, 2.25, 0.0001
    end)

    assert_in_delta element.style.border_radius, 4.8, 0.0001
    assert_in_delta element.style.row_gap, 7.2, 0.0001
    assert_in_delta element.style.column_gap, 7.2, 0.0001

    assert element.style.grid_template_columns == [
             {:length, 24.0},
             {:minmax, {:length, 48.0}, {:length, 72.0}}
           ]

    assert element.style.font_size == 12.0
    assert element.style.line_height == 24.0
    assert_in_delta element.style.letter_spacing, 2.4, 0.0001
    assert root_relative_line_height.style.line_height == 24.0
  end

  test "unstyled descendants retain resolved root lengths and semantic defaults" do
    html = """
    <html style="font-size:2rem;line-height:1rem;--spacing:0.5rem">
    <body><div style="padding:var(--spacing);font-size:0.75rem">
    <h2>Heading</h2><p>Text <span>inside</span></p>
    <ol><li>First</li><li>Second</li></ol>
    <table><thead><tr><th>Header</th></tr></thead>
    <tbody><tr><td>Value</td></tr></tbody></table>
    </div></body></html>
    """

    explicit =
      Regex.replace(~r/<(body|h2|p|span|ol|li|table|thead|tr|th|tbody|td)>/, html, fn _, tag ->
        "<#{tag} style=\"letter-spacing:0\">"
      end)

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, explicit_dom} = HtmlParser.parse_document_detailed(explicit)
    assert {:ok, styled} = Style.compute(tree_content(dom))
    assert {:ok, explicit_styled} = Style.compute(tree_content(explicit_dom))

    rendering = fn recurse, node ->
      Map.take(node, [:type, :tag, :text, :style, :document_canvas])
      |> Map.put(:children, Enum.map(Map.get(node, :children, []), &recurse.(recurse, &1)))
    end

    assert rendering.(rendering, styled) == rendering.(rendering, explicit_styled)
    [body] = styled.children
    [container] = Enum.reject(body.children, &(&1.type == :text))
    assert container.style.padding == edges(12.0)
    assert container.style.font_size == 18.0
    paragraph = Enum.find(container.children, &(Map.get(&1, :tag) == "p"))
    assert paragraph.style.line_height == 24.0
    assert paragraph.style.margin.bottom == 18.0
  end

  test "rem resolution scales without traversing runtime font payloads per element" do
    cell =
      ~s(<td style="font-size: 0.9rem; padding: 0.25rem; border-width: 0.05rem"><span>Item</span></td>)

    row = "<tr>" <> String.duplicate(cell, 6) <> "</tr>"

    reductions_for = fn row_count ->
      html = "<table><tbody>" <> String.duplicate(row, row_count) <> "</tbody></table>"
      assert {:ok, dom} = HtmlParser.parse(html)
      {:reductions, before_reductions} = Process.info(self(), :reductions)
      assert {:ok, _styled_tree} = Style.compute(dom, [])
      {:reductions, after_reductions} = Process.info(self(), :reductions)
      after_reductions - before_reductions
    end

    one_row_reductions = reductions_for.(1)
    twelve_row_reductions = reductions_for.(12)

    assert twelve_row_reductions - one_row_reductions < 5_000_000
    assert twelve_row_reductions < 20_000_000
  end

  test "compute resolves font-relative semantic margins against final font size" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "font-size: 24pt"},
          children: [%{type: :text, text: "Paragraph"}]
        },
        %{
          type: :element,
          tag: "h1",
          attributes: %{"style" => "font-size: 30pt"},
          children: [%{type: :text, text: "Heading"}]
        },
        %{
          type: :element,
          tag: "ul",
          attributes: %{"style" => "font-size: 18pt"},
          children: [
            %{
              type: :element,
              tag: "li",
              attributes: %{},
              children: [%{type: :text, text: "List item"}]
            }
          ]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "font-size: 24pt; margin-bottom: 5pt"},
          children: [%{type: :text, text: "Explicit margin"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom)
    [paragraph, heading, list, explicit_margin] = styled_tree.children

    assert paragraph.style.margin_after == 24.0
    assert paragraph.style.margin.bottom == 24.0
    assert_in_delta heading.style.margin_after, 20.1, 0.0001
    assert list.style.margin_after == 18.0
    assert explicit_margin.style.margin_after == 5.0
    assert explicit_margin.style.margin.bottom == 5.0
  end

  test "compute applies heading defaults and inline style colors" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "h1",
          attributes: %{"style" => "color: #336699"},
          children: [%{type: :text, text: "Title"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [heading] = styled_tree.children
    [title] = heading.children

    assert heading.style.display == :block
    assert heading.style.font_size == 24.0
    assert heading.style.font_weight == 700
    assert heading.style.color == {0.2, 0.4, 0.6}
    assert title.style.color == {0.2, 0.4, 0.6}
  end

  test "compute applies explicit line-height declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "font-size: 10pt; line-height: 12px"},
          children: [%{type: :text, text: "Absolute"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "line-height: 1.5; font-size: 10pt"},
          children: [%{type: :text, text: "Unitless"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "font-size: 10pt; line-height: normal"},
          children: [%{type: :text, text: "Normal"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [absolute, unitless, normal] = styled_tree.children

    assert absolute.style.line_height == 9.0
    assert unitless.style.line_height == 15.0
    assert normal.style.line_height == 11.25

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{"style" => "line-height: tight"},
                   children: [%{type: :text, text: "Invalid"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}
  end

  test "compute preserves inherited unitless, normal, and absolute line heights" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{"style" => "font-size: 12pt; line-height: 2"},
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "font-size: 20pt"},
              children: [%{type: :text, text: "Unitless"}]
            }
          ]
        },
        %{
          type: :element,
          tag: "section",
          attributes: %{"style" => "font-size: 12pt; line-height: normal"},
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "font-size: 20pt"},
              children: [%{type: :text, text: "Normal"}]
            }
          ]
        },
        %{
          type: :element,
          tag: "aside",
          attributes: %{"style" => "font-size: 12pt; line-height: 15pt"},
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "font-size: 20pt"},
              children: [%{type: :text, text: "Absolute"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom)
    [unitless_parent, normal_parent, absolute_parent] = styled_tree.children
    [unitless_child] = unitless_parent.children
    [normal_child] = normal_parent.children
    [absolute_child] = absolute_parent.children

    assert unitless_parent.style.line_height == 24.0
    assert unitless_child.style.line_height == 40.0
    assert_in_delta normal_parent.style.line_height, 14.25, 0.0001
    assert normal_child.style.line_height == 23.25
    assert absolute_parent.style.line_height == 15.0
    assert absolute_child.style.line_height == 15.0
  end

  test "compute derives normal line-height from embedded font metrics" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{
            "style" => "font-family: 'Fixture Sans'; font-size: 10pt; line-height: normal"
          },
          children: [%{type: :text, text: "Metric"}]
        }
      ]
    }

    assert {:ok, styled_tree} =
             Style.compute(dom, fonts: [%{family: "Fixture Sans", path: ttf_font_path!()}])

    [paragraph] = styled_tree.children
    %{ascent: ascent, descent: descent, units_per_em: units_per_em} = paragraph.style.font_face

    expected_line_height =
      (round(ascent / units_per_em * 10.0 / 0.75) +
         round(-descent / units_per_em * 10.0 / 0.75)) * 0.75

    assert_in_delta paragraph.style.line_height, expected_line_height, 0.0001
  end

  test "normal line-height includes a font's positive line gap at larger sizes" do
    font_path =
      [
        "/usr/share/fonts/liberation/LiberationSans-Regular.ttf",
        "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
      ]
      |> Enum.find(&File.regular?/1)

    assert font_path

    assert {:ok, dom} =
             HtmlParser.parse(
               "<p style=\"font-family:'Fixture Sans';font-size:14pt;line-height:normal\">Line gap</p>"
             )

    assert {:ok, styled} =
             Style.compute(dom, fonts: [%{family: "Fixture Sans", path: font_path}])

    [paragraph] = styled.children
    assert paragraph.style.font_face.line_gap > 0
    assert paragraph.style.line_height == 16.5
  end

  test "compute accepts normal line-break declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "line-break: normal"},
          children: [%{type: :text, text: "Normal break"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.line_break == :normal
  end

  test "compute applies inline bold italic and inherited color" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "color: red"},
          children: [
            %{type: :text, text: "Hello "},
            %{
              type: :element,
              tag: "strong",
              attributes: %{},
              children: [%{type: :text, text: "bold"}]
            },
            %{
              type: :element,
              tag: "em",
              attributes: %{},
              children: [%{type: :text, text: "italic"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children
    [plain, strong, em] = paragraph.children
    [bold] = strong.children
    [italic] = em.children

    assert plain.style.color == {1, 0, 0}
    assert strong.style.font_weight == 700
    assert bold.style.font_weight == 700
    assert bold.style.color == {1, 0, 0}
    assert em.style.font_style == :italic
    assert italic.style.font_style == :italic
  end

  test "compute applies block box styles from inline declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{
            "style" =>
              "margin: 2pt 4pt 6pt 8pt; padding: 3pt 5pt; border: 1pt solid #336699; border-radius: 2pt; background-color: #eeeeee; box-sizing: border-box"
          },
          children: [%{type: :text, text: "Boxed"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.margin == %{top: 2.0, right: 4.0, bottom: 6.0, left: 8.0}
    assert paragraph.style.margin_after == 6.0
    assert paragraph.style.padding == %{top: 3.0, right: 5.0, bottom: 3.0, left: 5.0}
    assert paragraph.style.border_widths == %{top: 0.75, right: 0.75, bottom: 0.75, left: 0.75}
    assert paragraph.style.border_color == {0.2, 0.4, 0.6}
    assert paragraph.style.border_colors == edges({0.2, 0.4, 0.6})
    assert paragraph.style.border_radius == 2.0
    assert paragraph.style.box_sizing == :border_box
    assert_in_delta elem(paragraph.style.background_color, 0), 0.9333, 0.0001
  end

  test "compute preserves side-specific border colors" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" => "border-top: 4px solid #22344a; border-bottom: 1px solid #d7dfe9"
          },
          children: [%{type: :text, text: "Head"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [element] = styled_tree.children

    assert element.style.border_widths == %{top: 3.0, right: 0.0, bottom: 0.75, left: 0.0}

    assert element.style.border_colors.top ==
             {0.13333333333333333, 0.20392156862745098, 0.2901960784313726}

    assert element.style.border_colors.bottom ==
             {0.8431372549019608, 0.8745098039215686, 0.9137254901960784}
  end

  test "compute applies manual page break declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "break-before: page; page-break-after: always"},
          children: [%{type: :text, text: "Break"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.break_before == :page
    assert paragraph.style.break_after == :page

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{"style" => "break-before: left"},
                   children: [%{type: :text, text: "Bad"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}
  end

  test "compute applies list defaults and propagates link URLs to text" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "ol",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "li",
              attributes: %{},
              children: [
                %{type: :text, text: "Read "},
                %{
                  type: :element,
                  tag: "a",
                  attributes: %{"href" => "mailto:team@example.com"},
                  children: [%{type: :text, text: "email"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [list] = styled_tree.children
    [item] = list.children
    [_plain, link] = item.children
    [link_text] = link.children

    assert list.style.display == :list
    assert list.style.list_marker_type == :decimal
    assert item.style.display == :list_item
    assert link.style.display == :inline
    assert link.style.color == {0, 0, 238 / 255}
    assert link.style.link_url == "mailto:team@example.com"
    assert link_text.style.link_url == "mailto:team@example.com"
  end

  test "compute applies table defaults and header cell behavior" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "table",
          attributes: %{
            "style" => "border-spacing: 6pt 3pt; table-layout: fixed; border-collapse: separate"
          },
          children: [
            %{
              type: :element,
              tag: "caption",
              attributes: %{},
              children: [%{type: :text, text: "Summary"}]
            },
            %{
              type: :element,
              tag: "colgroup",
              attributes: %{"style" => "width: 25%"},
              children: [
                %{
                  type: :element,
                  tag: "col",
                  attributes: %{"span" => "2", "style" => "width: 20%"},
                  children: []
                }
              ]
            },
            %{
              type: :element,
              tag: "thead",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "tr",
                  attributes: %{},
                  children: [
                    %{
                      type: :element,
                      tag: "th",
                      attributes: %{},
                      children: [%{type: :text, text: "Name"}]
                    },
                    %{
                      type: :element,
                      tag: "th",
                      attributes: %{},
                      children: [%{type: :text, text: "Count"}]
                    }
                  ]
                }
              ]
            },
            %{
              type: :element,
              tag: "tbody",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "tr",
                  attributes: %{},
                  children: [
                    %{
                      type: :element,
                      tag: "td",
                      attributes: %{
                        "colspan" => "2",
                        "rowspan" => "3",
                        "style" => "padding: 2pt; border: 2pt solid blue"
                      },
                      children: [%{type: :text, text: "Alpha"}]
                    },
                    %{
                      type: :element,
                      tag: "td",
                      attributes: %{},
                      children: [%{type: :text, text: "2"}]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [table] = styled_tree.children
    [caption, column_group, head, body] = table.children
    [column] = column_group.children
    [header_row] = head.children
    [name, count] = header_row.children
    [body_row] = body.children
    [alpha, amount] = body_row.children

    assert table.style.display == :table
    assert table.style.border_spacing == {6.0, 3.0}
    assert table.style.table_layout == :fixed
    assert caption.style.display == :table_caption
    assert caption.style.text_align == :center
    assert column_group.style.display == :table_column_group
    assert column_group.style.width == {:percent, 0.25}
    assert column.style.display == :table_column
    assert column.style.span == 2
    assert column.style.width == {:percent, 0.2}
    assert head.style.display == :table_row_group
    assert head.style.table_section == :head
    assert body.style.table_section == :body
    assert name.style.display == :table_cell
    assert name.style.font_weight == 700
    assert name.style.text_align == :center
    assert count.style.background_color == nil
    assert count.style.padding == %{top: 0.75, right: 0.75, bottom: 0.75, left: 0.75}
    assert count.style.border_widths == %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    assert count.style.border_styles == %{top: :none, right: :none, bottom: :none, left: :none}
    assert alpha.style.padding == %{top: 2.0, right: 2.0, bottom: 2.0, left: 2.0}
    assert alpha.style.border_widths == %{top: 1.5, right: 1.5, bottom: 1.5, left: 1.5}
    assert alpha.style.border_color == {0, 0, 1}
    assert amount.style.text_align == :left
    assert alpha.style.colspan == 2
    assert alpha.style.rowspan == 3
  end

  test "compute applies configured stylesheets before style tag rules" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "p { color: green; }"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Hello"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, stylesheets: [{:css, "p { color: red; }"}])
    [paragraph] = styled_tree.children
    [text] = paragraph.children

    assert paragraph.style.color == {0, 0.5019607843, 0}
    assert text.style.color == {0, 0.5019607843, 0}
  end

  test "compute applies configured stylesheet files" do
    stylesheet_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-style-test.css")
    File.write!(stylesheet_path, "p { color: #336699; }")

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Hello"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, stylesheets: [{:file, stylesheet_path}])
    [paragraph] = styled_tree.children

    assert paragraph.style.color == {0.2, 0.4, 0.6}
  after
    stylesheet_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-style-test.css")
    File.rm(stylesheet_path)
  end

  test "compute accepts empty and comment-only CSS and file paths containing braces" do
    stylesheet_path =
      Path.join(System.tmp_dir!(), "native-elixir-pdf-style-{print}-test.css")

    File.write!(stylesheet_path, "p { color: #336699; }")

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Hello"}]
        }
      ]
    }

    assert {:ok, styled_tree} =
             Style.compute(dom,
               stylesheets: [
                 {:css, ""},
                 {:css, "/* intentionally empty */"},
                 {:file, stylesheet_path}
               ]
             )

    [paragraph] = styled_tree.children
    assert paragraph.style.color == {0.2, 0.4, 0.6}
  after
    File.rm(Path.join(System.tmp_dir!(), "native-elixir-pdf-style-{print}-test.css"))
  end

  test "compute loads local fonts declared by embedded font-face rules" do
    font_path = ttf_font_path!()

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: """
              @font-face {
                font-family: "CSS Fixture";
                src: url("#{font_path}") format("truetype");
              }
              @media print { p { font-family: "CSS Fixture"; color: red; } }
              @media screen { p { color: blue; } }
              """
            }
          ]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Printed"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, base_url: Path.dirname(font_path))
    [paragraph] = styled_tree.children

    assert paragraph.style.font_family == "CSS Fixture"
    assert paragraph.style.font_face.type == :embedded
    assert paragraph.style.color == {1, 0, 0}
  end

  test "compute resolves font-face URLs relative to stylesheet files and base_url" do
    fixture_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-css-font-test")
    css_dir = Path.join(fixture_dir, "css")
    font_dir = Path.join(fixture_dir, "fonts")
    stylesheet_path = Path.join(css_dir, "print.css")
    copied_font_path = Path.join(font_dir, "fixture.ttf")
    File.mkdir_p!(css_dir)
    File.mkdir_p!(font_dir)
    File.cp!(ttf_font_path!(), copied_font_path)

    File.write!(
      stylesheet_path,
      "@font-face { font-family: FileFixture; src: url('../fonts/fixture.ttf'); } p { font-family: FileFixture; }"
    )

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "From file"}]
        }
      ]
    }

    assert {:ok, styled_from_file} =
             Style.compute(dom, stylesheets: [{:file, stylesheet_path}])

    [paragraph] = styled_from_file.children
    assert paragraph.style.font_face.type == :embedded

    File.write!(
      stylesheet_path,
      "@font-face { font-family: AbsoluteFileFixture; src: url('#{copied_font_path}'); } p { font-family: AbsoluteFileFixture; }"
    )

    assert {:ok, styled_from_absolute_file} =
             Style.compute(dom, stylesheets: [{:file, stylesheet_path}])

    [paragraph] = styled_from_absolute_file.children
    assert paragraph.style.font_face.type == :embedded

    inline_css =
      "@font-face { font-family: BaseFixture; src: url('fonts/fixture.ttf'); } p { font-family: BaseFixture; }"

    assert {:ok, styled_from_base} =
             Style.compute(dom, stylesheets: [{:css, inline_css}], base_url: fixture_dir)

    [paragraph] = styled_from_base.children
    assert paragraph.style.font_face.type == :embedded

    assert {:ok, styled_from_file_url} =
             Style.compute(dom,
               stylesheets: [{:css, inline_css}],
               base_url: "file://#{fixture_dir}"
             )

    [paragraph] = styled_from_file_url.children
    assert paragraph.style.font_face.type == :embedded

    assert Style.compute(dom, stylesheets: [{:css, inline_css}]) ==
             {:error, :invalid_document}

    assert Style.compute(dom,
             stylesheets: [{:css, inline_css}],
             base_url: "https://example.com"
           ) ==
             {:error, :invalid_document}

    invalid_face_css =
      "@font-face { font-family: Bad; src: url('font.woff2') format('woff2'); }"

    assert Style.compute(dom, stylesheets: [{:css, invalid_face_css}], base_url: fixture_dir) ==
             {:error, :invalid_document}

    invalid_font_path = Path.join(font_dir, "invalid.ttf")
    File.write!(invalid_font_path, "not a font")

    invalid_font_css =
      "@font-face { font-family: InvalidFixture; src: url('fonts/invalid.ttf'); }"

    assert Style.compute(dom, stylesheets: [{:css, invalid_font_css}], base_url: fixture_dir) ==
             {:error, :invalid_document}

    invalid_stylesheet_path = Path.join(css_dir, "invalid.css")
    File.write!(invalid_stylesheet_path, "p { color: not-a-color; }")

    assert {:error, {:invalid_css, %{stage: :css}}} =
             Style.compute_detailed(dom, stylesheets: [{:file, invalid_stylesheet_path}])
  after
    File.rm_rf(Path.join(System.tmp_dir!(), "native-elixir-pdf-css-font-test"))
  end

  test "compute resolves font-family fallback against embedded fonts" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          attributes: %{"style" => "font-family: Missing, 'Fixture Sans', Helvetica"},
          children: [%{type: :text, text: "Hello"}]
        }
      ]
    }

    assert {:ok, styled_tree} =
             Style.compute(dom,
               fonts: [%{family: "Fixture Sans", path: ttf_font_path!()}],
               default_font: ["Missing", "Helvetica"]
             )

    [paragraph] = styled_tree.children
    [text] = paragraph.children

    assert paragraph.style.font_family == "Fixture Sans"
    assert paragraph.style.font_families == ["Missing", "Fixture Sans", "Helvetica"]
    assert paragraph.style.font_face.type == :embedded
    assert text.style.font_face.id == paragraph.style.font_face.id
  end

  test "compute applies selector specificity before source order" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: """
              #intro { color: blue; }
              p.copy { color: green; }
              p { color: red; }
              """
            }
          ]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"id" => "intro", "class" => "copy"},
          children: [%{type: :text, text: "Specific"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.color == {0, 0, 1}
  end

  test "compute applies source order when specificity matches" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "p { color: red; } p { color: blue; }"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Later wins"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.color == {0, 0, 1}
  end

  test "compute applies descendant and child selectors with inheritance" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: "body > p .note { color: #336699; font-size: 16pt; font-weight: bold; }"
            }
          ]
        },
        %{
          type: :element,
          tag: "body",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "p",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "span",
                  attributes: %{"class" => "note"},
                  children: [%{type: :text, text: "Inherited"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert [%{tag: "body"} = body] = styled_tree.children
    [paragraph] = body.children
    [span] = paragraph.children
    [text] = span.children

    assert span.style.color == {0.2, 0.4, 0.6}
    assert span.style.font_size == 16.0
    assert span.style.font_weight == 700
    assert span.style.line_height == 18.75
    assert text.style.color == {0.2, 0.4, 0.6}
    assert text.style.font_size == 16.0
  end

  test "compute ignores child selectors when the required parent is absent" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "div > p { color: red; }"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "No parent"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children
    assert paragraph.style.color == {0, 0, 0}
  end

  test "compute applies first-child pseudo selectors" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "div p:first-child { color: red; }"}]
        },
        %{
          type: :element,
          tag: "div",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "p",
              attributes: %{},
              children: [%{type: :text, text: "First"}]
            },
            %{
              type: :element,
              tag: "p",
              attributes: %{},
              children: [%{type: :text, text: "Second"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [container] = styled_tree.children
    [first, second] = container.children

    assert first.style.color == {1, 0, 0}
    assert second.style.color == {0, 0, 0}
  end

  test "compute applies attribute, negation, odd/even, and type-position selectors" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: """
              [data-kind][role=note]:not(.muted) { color: red; }
              div > p:nth-child(odd) { font-weight: bold; }
              div > p:nth-child(even) { font-style: italic; }
              p:first-of-type { font-size: 14pt; }
              p:last-of-type { text-align: right; }
              """
            }
          ]
        },
        %{
          type: :element,
          tag: "div",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{},
              children: [%{type: :text, text: "Lead"}]
            },
            %{
              type: :element,
              tag: "p",
              attributes: %{"data-kind" => "callout", "role" => "note"},
              children: [%{type: :text, text: "First paragraph"}]
            },
            %{
              type: :element,
              tag: "p",
              attributes: %{
                "class" => "muted",
                "data-kind" => "callout",
                "role" => "note"
              },
              children: [%{type: :text, text: "Last paragraph"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [container] = styled_tree.children
    [_span, first, last] = container.children

    assert first.style.color == {1, 0, 0}
    assert first.style.font_style == :italic
    assert first.style.font_size == 14.0
    assert last.style.color == {0, 0, 0}
    assert last.style.font_weight == 700
    assert last.style.text_align == :right
  end

  test "compute distinguishes identical siblings for positional selectors" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <style>
               p:first-child { color: red; }
               p:last-child { background-color: blue; }
               p:first-of-type { font-weight: bold; }
               p:last-of-type { font-style: italic; }
               p:nth-child(odd) { letter-spacing: 1pt; }
               p:nth-child(even) { text-align: center; }
               p:nth-child(2) { font-size: 14pt; }
             </style>
             <div><p>X</p><p>X</p><p>X</p></div>
             """)

    assert {:ok, styled_tree} = Style.compute(dom)
    [container] = styled_tree.children
    [first, second, third] = container.children

    assert first.style.color == {1, 0, 0}
    assert first.style.font_weight == 700
    assert first.style.font_style == :normal
    assert first.style.letter_spacing == 1.0
    assert Map.get(first.style, :background_color) == nil

    assert second.style.color == {0, 0, 0}
    assert second.style.font_weight == 400
    assert second.style.font_style == :normal
    assert second.style.letter_spacing == 0.0
    assert second.style.text_align == :center
    assert second.style.font_size == 14.0

    assert third.style.color == {0, 0, 0}
    assert third.style.font_weight == 400
    assert third.style.font_style == :italic
    assert third.style.letter_spacing == 1.0
    assert third.style.background_color == {0, 0, 1}
  end

  test "compute inserts before and after content without applying pseudo styles to the element" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: """
              [data-label]::before { content: "Label: " attr(data-label) " "; color: red; }
              p::after { content: "."; font-weight: bold; }
              """
            }
          ]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"data-label" => "Status"},
          children: [%{type: :text, text: "Ready"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children
    [before, text, after_content] = paragraph.children
    [before_text] = before.children
    [after_text] = after_content.children

    assert before.tag == "::before"
    assert before_text.text == "Label: Status "
    assert before_text.style.color == {1, 0, 0}
    assert text.text == "Ready"
    assert text.style.color == {0, 0, 0}
    assert after_content.tag == "::after"
    assert after_text.text == "."
    assert after_text.style.font_weight == 700
    assert paragraph.style.color == {0, 0, 0}
  end

  test "compute preserves CSS syntax characters inside quoted selector and content strings" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~S|<style>[data-label="a b,c\"d"]::before { content: "A/*B*/C } { , \"quoted\"; tail"; }</style><p data-label='a b,c"d'>Body</p>|
             )

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children
    [before, body] = paragraph.children

    assert hd(before.children).text == ~S|A/*B*/C } { , "quoted"; tail|
    assert body.text == "Body"
  end

  test "compute evaluates reset and increment counters in document order" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: """
              body { counter-reset: section 0 figure 4; }
              h1::before { counter-increment: section; content: "Section " counter(section) ": "; }
              .figure { counter-increment: figure; }
              .figure::before { content: "Figure " counter(figure) " — "; }
              """
            }
          ]
        },
        %{
          type: :element,
          tag: "body",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "h1",
              attributes: %{},
              children: [%{type: :text, text: "Overview"}]
            },
            %{
              type: :element,
              tag: "h1",
              attributes: %{},
              children: [%{type: :text, text: "Details"}]
            },
            %{
              type: :element,
              tag: "div",
              attributes: %{"class" => "figure"},
              children: [%{type: :text, text: "Architecture"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert [%{tag: "body"} = body] = styled_tree.children
    [first_heading, second_heading, figure] = body.children
    [first_number | _children] = first_heading.children
    [second_number | _children] = second_heading.children
    [figure_number | _children] = figure.children

    assert hd(first_number.children).text == "Section 1: "
    assert hd(second_number.children).text == "Section 2: "
    assert hd(figure_number.children).text == "Figure 5 — "
  end

  test "compute restores an enclosing counter after a nested reset" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <style>
               .reset { counter-reset: item; }
               .item::before { counter-increment: item; content: counter(item); }
             </style>
             <div class="reset">
               <p class="item"> outer</p>
               <div class="reset">
                 <p class="item"> inner</p>
                 <p class="item"> inner</p>
               </div>
               <p class="item"> outer</p>
             </div>
             """)

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [outer] = styled_tree.children
    [first_outer, inner, last_outer] = Enum.filter(outer.children, &(&1.type == :element))
    [first_inner, second_inner] = Enum.filter(inner.children, &(&1.type == :element))

    assert hd(hd(first_outer.children).children).text == "1"
    assert hd(hd(first_inner.children).children).text == "1"
    assert hd(hd(second_inner.children).children).text == "2"
    assert hd(hd(last_outer.children).children).text == "2"
  end

  test "compute carries counters created by preceding siblings and pseudo-elements" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <style>
               .seed { counter-reset: carried 4 carried 5; }
               .read::before { counter-increment: carried; content: counter(carried); }
               .trail::after { counter-increment: trail; content: counter(trail); }
               .missing::before { content: counter(missing); }
             </style>
             <div class="seed"></div>
             <div class="seed"></div>
             <p class="read"> carried</p>
             <p class="trail">trail </p>
             <p class="trail">trail </p>
             <p class="missing"> missing</p>
             """)

    assert {:ok, styled_tree} = Style.compute(dom, [])

    [_first_seed, _second_seed, carried, first_trail, second_trail, missing] =
      Enum.filter(styled_tree.children, &(&1.type == :element))

    assert hd(hd(carried.children).children).text == "6"
    assert hd(List.last(first_trail.children).children).text == "1"
    assert hd(List.last(second_trail.children).children).text == "2"
    assert hd(hd(missing.children).children).text == "0"
  end

  test "compute rejects malformed generated content and counter values" do
    for css <- [
          ~S|p::before { content: attr(); }|,
          ~S|p::before { content: counters(section); }|,
          ~S|p { counter-reset: section nope!; }|,
          ~S|p { counter-increment: 1; }|,
          ~S|p { counter-reset: section 1 2; }|,
          ~S|p { counter-increment: none 1; }|
        ] do
      dom = %{
        type: :document,
        children: [
          %{
            type: :element,
            tag: "style",
            attributes: %{},
            children: [%{type: :text, text: css}]
          },
          %{
            type: :element,
            tag: "p",
            attributes: %{},
            children: [%{type: :text, text: "Text"}]
          }
        ]
      }

      assert Style.compute(dom, []) == {:error, :invalid_document}
    end
  end

  test "compute omits none, empty, and display-none generated content" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: """
              p::before { content: "Hidden"; display: none; }
              p::after { content: ""; }
              span::before { content: none; }
              """
            }
          ]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{},
          children: [%{type: :text, text: "Paragraph"}]
        },
        %{
          type: :element,
          tag: "div",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{},
              children: [%{type: :text, text: "Span"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph, container] = styled_tree.children
    [span] = container.children

    assert Enum.map(paragraph.children, &Map.get(&1, :text)) == ["Paragraph"]
    assert Enum.map(span.children, &Map.get(&1, :text)) == ["Span"]
    assert style_for!("p", "counter-reset: none").counter_reset == []
    assert style_for!("p", "counter-increment: none").counter_increment == []
  end

  test "compute accepts root custom properties and table cell layout hints" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "html",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "style",
              attributes: %{},
              children: [
                %{
                  type: :text,
                  text:
                    ":root { --row-height: 8rem; } .none { display: none !important; } .approval-section { display: flex; } td { line-break: anywhere; vertical-align: top; } .item-row td { min-height: var(--row-height); height: var(--row-height); }"
                }
              ]
            },
            %{
              type: :element,
              tag: "body",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "div",
                  attributes: %{"class" => "none approval-section"},
                  children: [%{type: :text, text: "Hidden"}]
                },
                %{
                  type: :element,
                  tag: "table",
                  attributes: %{},
                  children: [
                    %{
                      type: :element,
                      tag: "tr",
                      attributes: %{"class" => "item-row"},
                      children: [
                        %{
                          type: :element,
                          tag: "td",
                          attributes: %{},
                          children: [%{type: :text, text: "Cell"}]
                        }
                      ]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert [%{tag: "body"} = body] = styled_tree.children
    hidden = Enum.find(body.children, &match?(%{tag: "div"}, &1))
    table = Enum.find(body.children, &match?(%{tag: "table"}, &1))
    [row] = table.children
    [cell] = row.children

    assert hidden.style.display == :none
    assert table.style.display == :table
    assert cell.style.display == :table_cell
    assert cell.style.height == 96.0
    assert cell.style.line_break == :anywhere
    assert cell.style.min_height == 96.0
  end

  test "compute resolves custom properties inside supported compound values" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "html",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "style",
              attributes: %{},
              children: [
                %{
                  type: :text,
                  text:
                    ":root { --pad: 6pt; --accent: #0f766e; } section { padding: var(--pad); border-left: 3pt solid var(--accent); }"
                }
              ]
            },
            %{
              type: :element,
              tag: "body",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "section",
                  attributes: %{},
                  children: [%{type: :text, text: "Panel"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    assert [%{tag: "body"} = body] = styled_tree.children
    [section] = body.children

    assert section.style.padding == %{top: 6.0, right: 6.0, bottom: 6.0, left: 6.0}
    assert section.style.border_widths.left == 3.0
    assert_style_value(section.style.border_colors.left, {0.0588235294, 0.462745098, 0.431372549})
  end

  test "compute resolves ordinary declarations against final custom properties" do
    forward_reference =
      style_for!("p", "color: var(--brand); --brand: red")

    later_override =
      style_for!("p", "--brand: blue; color: var(--brand); --brand: red")

    assert forward_reference.color == {1, 0, 0}
    assert later_override.color == {1, 0, 0}
  end

  test "compute preserves case-sensitive custom properties in stylesheets and inline styles" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text:
                ".stylesheet { --Accent: red; --accent: blue; color: var(--Accent); background-color: var(--accent); }"
            }
          ]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"class" => "stylesheet"},
          children: [%{type: :text, text: "Stylesheet"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{
            "style" =>
              "--Accent: green; --accent: blue; color: var(--Accent); background-color: var(--accent)"
          },
          children: [%{type: :text, text: "Inline"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [stylesheet, inline] = styled_tree.children

    assert stylesheet.style.color == {1, 0, 0}
    assert stylesheet.style.background_color == {0, 0, 1}
    assert inline.style.color == {0, 0.5019607843, 0}
    assert inline.style.background_color == {0, 0, 1}
  end

  test "compute recursively resolves custom properties and rejects cycles" do
    nested =
      style_for!(
        "p",
        """
        --base: red;
        --accent: var(--base);
        --foreground: var(--accent);
        color: var(--foreground);
        """
      )

    assert nested.color == {1, 0, 0}

    assert style_for(
             "p",
             "--first: var(--second); --second: var(--first); color: var(--first)"
           ) == {:error, :invalid_document}
  end

  test "compute cascades custom properties before resolving ordinary declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text: """
              .normal { --brand: blue; color: var(--brand); }
              .normal { --brand: red; }
              .important { --brand: blue !important; color: var(--brand); }
              """
            }
          ]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"class" => "normal"},
          children: [%{type: :text, text: "Later source order"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"class" => "important", "style" => "--brand: red"},
          children: [%{type: :text, text: "Important stylesheet variable"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"class" => "important", "style" => "--brand: red !important"},
          children: [%{type: :text, text: "Important inline variable"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [normal, stylesheet_important, inline_important] = styled_tree.children

    assert normal.style.color == {1, 0, 0}
    assert stylesheet_important.style.color == {0, 0, 1}
    assert inline_important.style.color == {1, 0, 0}
  end

  test "compute applies child selectors with first-child pseudo classes" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text:
                ".address-section > .section { color: #336699; } .address-section > .section p { margin-top: 4pt; } .address-section > .section p:first-child { margin-top: 0; color: red; }"
            }
          ]
        },
        %{
          type: :element,
          tag: "div",
          attributes: %{"class" => "address-section"},
          children: [
            %{
              type: :element,
              tag: "div",
              attributes: %{"class" => "section"},
              children: [
                %{
                  type: :element,
                  tag: "p",
                  attributes: %{},
                  children: [%{type: :text, text: "First"}]
                },
                %{
                  type: :element,
                  tag: "p",
                  attributes: %{},
                  children: [%{type: :text, text: "Second"}]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [section_wrapper] = styled_tree.children
    [section] = section_wrapper.children
    [first, second] = section.children

    assert section.style.color == {0.2, 0.4, 0.6}
    assert first.style.color == {1, 0, 0}
    assert first.style.margin.top == 0.0
    assert second.style.margin.top == 4.0
    assert second.style.color == {0.2, 0.4, 0.6}
  end

  test "compute gives inline style declarations the highest priority" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "#intro { color: blue; }"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"id" => "intro", "style" => "color: red"},
          children: [%{type: :text, text: "Inline"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.color == {1, 0, 0}
  end

  test "compute gives important stylesheet declarations priority over normal inline declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "#intro { color: blue !important; }"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"id" => "intro", "style" => "color: red"},
          children: [%{type: :text, text: "Stylesheet important"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.color == {0, 0, 1}
  end

  test "compute gives important inline declarations priority over important stylesheet declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [%{type: :text, text: "#intro { color: blue !important; }"}]
        },
        %{
          type: :element,
          tag: "p",
          attributes: %{"id" => "intro", "style" => "color: red !important"},
          children: [%{type: :text, text: "Inline important"}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [paragraph] = styled_tree.children

    assert paragraph.style.color == {1, 0, 0}
  end

  test "compute applies flex container and item declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "display: flex; flex-direction: row-reverse; flex-wrap: wrap; gap: 6pt; justify-content: space-between; align-items: center"
          },
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{
                "style" =>
                  "order: 2; flex-grow: 1; flex-shrink: 0; flex-basis: 20pt; align-self: flex-end"
              },
              children: [%{type: :text, text: "A"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "order: 1; flex: 2 1 10pt"},
              children: [%{type: :text, text: "B"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [container] = styled_tree.children
    [first, second] = container.children

    assert container.style.display == :flex
    assert container.style.flex_direction == :row_reverse
    assert container.style.flex_wrap == :wrap
    assert container.style.row_gap == 6.0
    assert container.style.column_gap == 6.0
    assert container.style.justify_content == :space_between
    assert container.style.align_items == :center

    assert first.style.order == 2
    assert first.style.flex_grow == 1.0
    assert first.style.flex_shrink == 0.0
    assert first.style.flex_basis == 20.0
    assert first.style.align_self == :flex_end

    assert second.style.order == 1
    assert second.style.flex_grow == 2.0
    assert second.style.flex_shrink == 1.0
    assert second.style.flex_basis == 10.0
  end

  test "compute applies grid container and item declarations" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "display: grid; grid-template-columns: 30pt 1fr; grid-template-rows: 20pt auto; grid-auto-columns: 12pt; grid-auto-rows: 18pt; gap: 4pt 6pt; justify-items: center; align-items: end; justify-content: space-between; align-content: center"
          },
          children: [
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "grid-column: 2 / span 2; grid-row: 1 / 3"},
              children: [%{type: :text, text: "A"}]
            },
            %{
              type: :element,
              tag: "span",
              attributes: %{"style" => "grid-area: 2 / 1 / 3 / 2; align-self: start"},
              children: [%{type: :text, text: "B"}]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [container] = styled_tree.children
    [first, second] = container.children

    assert container.style.display == :grid
    assert container.style.grid_template_columns == [{:length, 30.0}, {:fr, 1.0}]
    assert container.style.grid_template_rows == [{:length, 20.0}, :auto]
    assert container.style.grid_auto_columns == {:length, 12.0}
    assert container.style.grid_auto_rows == {:length, 18.0}
    assert container.style.row_gap == 4.0
    assert container.style.column_gap == 6.0
    assert container.style.justify_items == :center
    assert container.style.align_items == :flex_end
    assert container.style.justify_content == :space_between
    assert container.style.align_content == :center

    assert first.style.grid_column_start == 2
    assert first.style.grid_column_end == {:span, 2}
    assert first.style.grid_row_start == 1
    assert first.style.grid_row_end == 3

    assert second.style.grid_row_start == 2
    assert second.style.grid_column_start == 1
    assert second.style.grid_row_end == 3
    assert second.style.grid_column_end == 2
    assert second.style.align_self == :flex_start
  end

  test "compute loads data URI PNG and base_url relative JPEG images" do
    base_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-image-style-test")
    File.mkdir_p!(base_dir)
    File.write!(Path.join(base_dir, "photo.jpg"), jpeg_fixture(3, 2))

    png_src = "data:image/png;base64,#{Base.encode64(png_fixture(2, 1))}"

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "img",
          attributes: %{"src" => png_src, "style" => "width: 20pt"},
          children: []
        },
        %{
          type: :element,
          tag: "img",
          attributes: %{"src" => "photo.jpg", "style" => "height: 12pt"},
          children: []
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, base_url: base_dir)
    [png, jpeg] = styled_tree.children

    assert png.style.display == :image
    assert png.style.width == 20.0
    assert png.style.image.format == :png
    assert png.style.image.width_px == 2
    assert png.style.image.height_px == 1
    assert png.style.image.color_space == :device_rgb

    assert jpeg.style.display == :image
    assert jpeg.style.height == 12.0
    assert jpeg.style.image.format == :jpeg
    assert jpeg.style.image.width_px == 3
    assert jpeg.style.image.height_px == 2
  after
    base_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-image-style-test")
    File.rm_rf(base_dir)
  end

  test "compute rejects missing sources unsupported formats and unsafe relative images" do
    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{type: :element, tag: "img", attributes: %{}, children: []}
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "table",
                   attributes: %{},
                   children: [
                     %{
                       type: :element,
                       tag: "tr",
                       attributes: %{},
                       children: [
                         %{
                           type: :element,
                           tag: "td",
                           attributes: %{"colspan" => 2},
                           children: [%{type: :text, text: "Bad span shape"}]
                         }
                       ]
                     }
                   ]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "img",
                   attributes: %{"src" => "data:image/gif;base64,R0lGODlhAQABAAAAACw="},
                   children: []
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert {:ok, _tree} =
             Style.compute(
               %{
                 type: :document,
                 children: [
                   %{
                     type: :element,
                     tag: "table",
                     attributes: %{},
                     children: [
                       %{
                         type: :element,
                         tag: "tr",
                         attributes: %{},
                         children: [
                           %{
                             type: :element,
                             tag: "td",
                             attributes: %{"colspan" => "0"},
                             children: [%{type: :text, text: "Bad span"}]
                           }
                         ]
                       }
                     ]
                   }
                 ]
               },
               []
             )

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "img",
                   attributes: %{"src" => "../photo.png"},
                   children: []
                 }
               ]
             },
             base_url: System.tmp_dir!()
           ) == {:error, :invalid_document}
  end

  test "compute rejects unsupported document trees" do
    assert Style.compute(%{tag: "p"}, []) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{"style" => "background: linear-gradient(red, blue)"},
                   children: [%{type: :text, text: "Hello"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "style",
                   attributes: %{},
                   children: [
                     %{type: :text, text: "p { background: linear-gradient(red, blue); }"}
                   ]
                 },
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{},
                   children: [%{type: :text, text: "Hello"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{},
                   children: [
                     %{
                       type: :element,
                       tag: "a",
                       attributes: %{"href" => "javascript:alert(1)"},
                       children: [%{type: :text, text: "bad"}]
                     }
                   ]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{"style" => "display: flex; justify-content: baseline"},
                   children: [%{type: :text, text: "Bad"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "div",
                   attributes: %{
                     "style" => "display: grid; grid-template-columns: repeat(0, 1fr)"
                   },
                   children: [%{type: :text, text: "Bad"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}
  end

  test "compute_detailed returns document diagnostics" do
    assert {:error,
            {:invalid_document,
             %{
               stage: :style,
               reason: :invalid_document,
               message: "document tree must be a parsed HTML document"
             }}} = Style.compute_detailed(%{tag: "p"})

    assert {:ok, %{type: :document}} =
             Style.compute_detailed(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{},
                   children: [%{type: :text, text: "Detailed"}]
                 }
               ]
             })

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               source: "display: table-row-group"
             }}} =
             Style.compute_detailed(%{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "style",
                   attributes: %{},
                   children: [%{type: :text, text: "body { display: table-row-group; }"}]
                 },
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{},
                   children: [%{type: :text, text: "Detailed"}]
                 }
               ]
             })
  end

  test "compute_detailed preserves semicolons inside URLs when locating invalid declarations" do
    data_url = "data:image/png;base64,AAAA"
    invalid_declaration = "display: table-row-group"

    for html <- [
          ~s|<p style="background-image: url(#{data_url}); #{invalid_declaration}">Inline</p>|,
          ~s|<style>p { background-image: url(#{data_url}); #{invalid_declaration}; }</style><p>Stylesheet</p>|
        ] do
      assert {:ok, dom} = HtmlParser.parse(html)

      assert {:error,
              {:invalid_css,
               %{
                 stage: :css,
                 reason: :invalid_css,
                 line: 1,
                 source: ^invalid_declaration,
                 message:
                   ~s(line 1: declaration "display: table-row-group" is invalid or unsupported)
               }}} = Style.compute_detailed(dom)
    end
  end

  test "compute accepts wrapper elements heading levels and inline aliases" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "html",
          attributes: %{},
          children: [
            %{type: :element, tag: "head", attributes: %{}, children: []},
            %{
              type: :element,
              tag: "body",
              attributes: %{},
              children:
                Enum.map(["h2", "h3", "h4", "h5", "h6"], fn tag ->
                  %{
                    type: :element,
                    tag: tag,
                    attributes: %{},
                    children: [%{type: :text, text: tag}]
                  }
                end) ++
                  [
                    %{
                      type: :element,
                      tag: "p",
                      attributes: %{},
                      children: [
                        %{
                          type: :element,
                          tag: "b",
                          attributes: %{},
                          children: [%{type: :text, text: "bold"}]
                        },
                        %{
                          type: :element,
                          tag: "i",
                          attributes: %{},
                          children: [%{type: :text, text: "italic"}]
                        }
                      ]
                    },
                    %{
                      type: :element,
                      tag: "table",
                      attributes: %{},
                      children: [
                        %{
                          type: :element,
                          tag: "tfoot",
                          attributes: %{},
                          children: []
                        }
                      ]
                    }
                  ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom)
    assert [%{tag: "body"} = body] = styled_tree.children
    headings = Enum.take(body.children, 5)
    paragraph = Enum.at(body.children, 5)
    table = Enum.at(body.children, 6)

    assert Enum.map(headings, & &1.style.font_size) == [
             18.0,
             14.04,
             12.0,
             9.959999999999999,
             8.040000000000001
           ]

    [bold, italic] = paragraph.children
    assert bold.style.font_weight == 700
    assert italic.style.font_style == :italic
    [foot] = table.children
    assert foot.style.table_section == :foot
  end

  test "compute inherits body font size into document blocks tables and headings" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "html",
          attributes: %{},
          children: [
            %{
              type: :element,
              tag: "head",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "style",
                  attributes: %{},
                  children: [%{type: :text, text: "body { font-size: 10px; }"}]
                }
              ]
            },
            %{
              type: :element,
              tag: "body",
              attributes: %{},
              children: [
                %{
                  type: :element,
                  tag: "p",
                  attributes: %{},
                  children: [%{type: :text, text: "P"}]
                },
                %{
                  type: :element,
                  tag: "h2",
                  attributes: %{},
                  children: [%{type: :text, text: "H2"}]
                },
                %{
                  type: :element,
                  tag: "h4",
                  attributes: %{},
                  children: [%{type: :text, text: "H4"}]
                },
                %{
                  type: :element,
                  tag: "table",
                  attributes: %{},
                  children: [
                    %{
                      type: :element,
                      tag: "tr",
                      attributes: %{},
                      children: [
                        %{
                          type: :element,
                          tag: "td",
                          attributes: %{},
                          children: [%{type: :text, text: "Cell"}]
                        }
                      ]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom)
    assert [%{tag: "body"} = body] = styled_tree.children
    [paragraph, h2, h4, table] = body.children
    [row] = table.children
    [cell] = row.children

    assert paragraph.style.font_size == 7.5
    assert h2.style.font_size == 11.25
    assert h4.style.font_size == 7.5
    assert table.style.font_size == 7.5
    assert cell.style.font_size == 7.5
  end

  test "compute accepts supported display and box value variants" do
    assertions = [
      {"display: block", :display, :block},
      {"display: inline", :display, :inline},
      {"display: inline-block", :display, :inline_block},
      {"display: none", :display, :none},
      {"display: inline-flex", :display, :inline_flex},
      {"display: inline-grid", :display, :inline_grid},
      {"box-sizing: content-box", :box_sizing, :content_box},
      {"margin: 1pt 2pt 3pt", :margin, %{top: 1.0, right: 2.0, bottom: 3.0, left: 2.0}},
      {"padding-top: 2pt", :padding, %{top: 2.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"padding-right: 2pt", :padding, %{top: 0.0, right: 2.0, bottom: 0.0, left: 0.0}},
      {"padding-bottom: 2pt", :padding, %{top: 0.0, right: 0.0, bottom: 2.0, left: 0.0}},
      {"padding-left: 2pt", :padding, %{top: 0.0, right: 0.0, bottom: 0.0, left: 2.0}},
      {"margin-left: 2pt", :margin, %{top: 12.0, right: 0.0, bottom: 12.0, left: 2.0}},
      {"border-width: 2pt", :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border: none", :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border-right: none", :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border-left: 2pt solid red", :border_widths,
       %{top: 0.0, right: 0.0, bottom: 0.0, left: 1.5}},
      {"border-top-width: 3pt", :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border-bottom-color: rgb(51, 102, 153)", :border_colors,
       %{top: {0, 0, 0}, right: {0, 0, 0}, bottom: {0.2, 0.4, 0.6}, left: {0, 0, 0}}},
      {"border-left-style: none", :border_widths,
       %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border-style: none", :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}},
      {"border: 1pt solid", :border_color, {0, 0, 0}},
      {"border: 1pt solid transparent", :border_color, nil},
      {"border-color: transparent", :border_color, nil},
      {"border-top: 2pt solid transparent", :border_colors,
       %{top: nil, right: {0, 0, 0}, bottom: {0, 0, 0}, left: {0, 0, 0}}},
      {"border-right-color: transparent", :border_colors,
       %{top: {0, 0, 0}, right: nil, bottom: {0, 0, 0}, left: {0, 0, 0}}},
      {"color: #abc", :color, {0.6666666667, 0.7333333333, 0.8}},
      {"color: #00000070", :color, {0, 0, 0, 112 / 255}},
      {"color: rgba(255, 128, 0, 0.5)", :color, {1.0, 128 / 255, 0.0, 0.5}},
      {"color: rgba(255, 128, 0, 50%)", :color, {1.0, 128 / 255, 0.0, 0.5}},
      {"color: rgba(255, 128, 0, 1)", :color, {1.0, 128 / 255, 0.0}},
      {"color: rgb(100%, 50%, 0%)", :color, {1.0, 0.5, 0.0}},
      {"color: currentColor", :color, {0, 0, 0}},
      {"color: transparent", :color, {0, 0, 0, 0.0}},
      {"background-color: #33669980", :background_color, {0.2, 0.4, 0.6, 128 / 255}},
      {"background-color: transparent", :background_color, nil},
      {"font-size: 10px", :font_size, 7.5},
      {"width: 10mm", :width, 72.0 / 25.4 * 10},
      {"width: 100%", :width, {:percent, 1.0}},
      {"min-width: 20pt", :min_width, 20.0},
      {"max-width: 30pt", :max_width, 30.0},
      {"height: 1cm", :height, 72.0 / 2.54},
      {"height: 25%", :height, {:percent, 0.25}},
      {"max-height: 40pt", :max_height, 40.0},
      {"width: 1in", :width, 72.0},
      {"height: 0", :height, 0.0},
      {"position: relative", :display, :block},
      {"overflow: hidden", :overflow, :hidden},
      {"overflow: visible", :overflow, :visible},
      {"aspect-ratio: 16 / 9", :aspect_ratio, 16 / 9},
      {"text-align: left", :text_align, :left},
      {"text-align: center", :text_align, :center},
      {"text-align: right", :text_align, :right},
      {"vertical-align: baseline", :vertical_align, :baseline},
      {"vertical-align: top", :vertical_align, :top},
      {"vertical-align: middle", :vertical_align, :middle},
      {"vertical-align: bottom", :vertical_align, :bottom},
      {"font-weight: normal", :font_weight, 400},
      {"font-weight: 900", :font_weight, 900},
      {"font-style: normal", :font_style, :normal},
      {"font-style: italic", :font_style, :italic},
      {"border-color: red", :border_color, {1, 0, 0}},
      {"border-top: 2pt solid red", :border_color, {0, 0, 0}},
      {"border-collapse: collapse", :border_collapse, :collapse},
      {"border-collapse: separate", :border_collapse, :separate},
      {"table-layout: auto", :table_layout, :auto},
      {"aspect-ratio: 1.5", :aspect_ratio, 1.5},
      {"margin-bottom: 5pt", :margin_after, 5.0},
      {"margin-top: -2pt", :margin, %{top: -2.0, right: 0.0, bottom: 12.0, left: 0.0}}
    ]

    Enum.each(assertions, fn {style, key, expected} ->
      assert {:ok, computed} = style_for("p", style)
      assert_style_value(Map.fetch!(computed, key), expected)
    end)
  end

  test "side border none keeps the existing border color" do
    assert {:ok, style} = style_for("td", "border: 1px solid #ccc; border-right: none")

    assert style.border_widths == %{top: 0.75, right: 0.0, bottom: 0.75, left: 0.75}
    assert_style_value(style.border_color, {0.8, 0.8, 0.8})
  end

  test "all border styles cascade independently on every side" do
    border_styles = [
      :none,
      :hidden,
      :dotted,
      :dashed,
      :solid,
      :double,
      :groove,
      :ridge,
      :inset,
      :outset
    ]

    Enum.each(border_styles, fn border_style ->
      assert {:ok, style} = style_for("div", "border: 3pt #{border_style} #336699")

      assert style.border_styles == %{
               top: border_style,
               right: border_style,
               bottom: border_style,
               left: border_style
             }

      expected_width =
        case border_style do
          border_style when border_style in [:none, :hidden] -> 0.0
          _ -> 3.0
        end

      assert style.border_widths == %{
               top: expected_width,
               right: expected_width,
               bottom: expected_width,
               left: expected_width
             }

      assert style.border_colors == %{
               top: {0.2, 0.4, 0.6},
               right: {0.2, 0.4, 0.6},
               bottom: {0.2, 0.4, 0.6},
               left: {0.2, 0.4, 0.6}
             }
    end)

    assert {:ok, sides} =
             style_for(
               "div",
               "border-width: thin medium thick 4pt; " <>
                 "border-style: dotted dashed double groove; " <>
                 "border-color: rgb(255, 0, 0) green transparent #fff"
             )

    assert sides.border_widths == %{top: 0.75, right: 2.25, bottom: 3.75, left: 3.75}
    assert sides.border_styles == %{top: :dotted, right: :dashed, bottom: :double, left: :groove}

    assert sides.border_colors == %{
             top: {1.0, 0.0, 0.0},
             right: {0, 0.5019607843, 0},
             bottom: nil,
             left: {1.0, 1.0, 1.0}
           }

    assert {:ok, two_values} =
             style_for(
               "div",
               "border-width: 1pt 2pt; border-style: dotted dashed; border-color: red blue"
             )

    assert two_values.border_widths == %{top: 0.75, right: 1.5, bottom: 0.75, left: 1.5}

    assert two_values.border_styles == %{
             top: :dotted,
             right: :dashed,
             bottom: :dotted,
             left: :dashed
           }

    assert two_values.border_colors == %{
             top: {1, 0, 0},
             right: {0, 0, 1},
             bottom: {1, 0, 0},
             left: {0, 0, 1}
           }

    assert {:ok, three_values} =
             style_for(
               "div",
               "border-width: 1pt 2pt 3pt; border-style: solid double ridge; " <>
                 "border-color: red green blue"
             )

    assert three_values.border_widths == %{top: 0.75, right: 1.5, bottom: 3.0, left: 1.5}

    assert three_values.border_styles == %{
             top: :solid,
             right: :double,
             bottom: :ridge,
             left: :double
           }

    assert three_values.border_colors == %{
             top: {1, 0, 0},
             right: {0, 0.5019607843, 0},
             bottom: {0, 0, 1},
             left: {0, 0.5019607843, 0}
           }

    assert {:ok, reordered} =
             style_for("div", "border-style: none; border-width: 2pt; border-style: dashed")

    assert reordered.border_widths == %{top: 1.5, right: 1.5, bottom: 1.5, left: 1.5}

    assert reordered.border_styles == %{
             top: :dashed,
             right: :dashed,
             bottom: :dashed,
             left: :dashed
           }

    assert {:ok, transparent} = style_for("div", "border: 2pt solid transparent")
    assert transparent.border_widths == %{top: 1.5, right: 1.5, bottom: 1.5, left: 1.5}
    assert transparent.border_colors == %{top: nil, right: nil, bottom: nil, left: nil}
  end

  test "table cells default to browser middle vertical alignment" do
    assert {:ok, style} = style_for("td", "")

    assert style.vertical_align == :middle
  end

  test "compute accepts supported flex and grid value variants" do
    assertions = [
      {"display: flex; flex-direction: row", :flex_direction, :row},
      {"display: flex; flex-direction: column-reverse", :flex_direction, :column_reverse},
      {"display: flex; flex-wrap: nowrap", :flex_wrap, :nowrap},
      {"display: flex; justify-content: start", :justify_content, :flex_start},
      {"display: flex; justify-content: end", :justify_content, :flex_end},
      {"display: flex; justify-content: stretch", :justify_content, :stretch},
      {"display: flex; justify-content: space-around", :justify_content, :space_around},
      {"display: flex; justify-content: space-evenly", :justify_content, :space_evenly},
      {"display: flex; align-items: stretch", :align_items, :stretch},
      {"display: flex; align-items: start", :align_items, :flex_start},
      {"display: flex; align-self: auto", :align_self, :auto},
      {"display: flex; align-self: stretch", :align_self, :stretch},
      {"display: flex; align-self: start", :align_self, :flex_start},
      {"display: flex; align-self: end", :align_self, :flex_end},
      {"display: flex; flex-basis: auto", :flex_basis, :auto},
      {"display: flex; flex: none", :flex_grow, 0.0},
      {"display: flex; flex: auto", :flex_grow, 1.0},
      {"display: flex; flex: initial", :flex_grow, 0.0},
      {"display: flex; flex: 2", :flex_grow, 2.0},
      {"display: flex; flex: 2 0", :flex_shrink, 0.0},
      {"display: flex; row-gap: 2pt", :row_gap, 2.0},
      {"display: flex; column-gap: 3pt", :column_gap, 3.0},
      {"display: grid; grid-template-columns: auto", :grid_template_columns, [:auto]},
      {"display: grid; grid-column-start: auto", :grid_column_start, :auto},
      {"display: grid; grid-column-end: span 2", :grid_column_end, {:span, 2}},
      {"display: grid; grid-row-start: 2", :grid_row_start, 2},
      {"display: grid; grid-row-end: 3", :grid_row_end, 3},
      {"display: grid; grid-column: 2", :grid_column_start, 2},
      {"display: grid; justify-items: stretch", :justify_items, :stretch},
      {"display: grid; justify-items: start", :justify_items, :flex_start},
      {"display: grid; justify-items: end", :justify_items, :flex_end}
    ]

    Enum.each(assertions, fn {style, key, expected} ->
      assert {:ok, computed} = style_for("div", style)
      assert_style_value(Map.fetch!(computed, key), expected)
    end)
  end

  test "compute rejects invalid style branches and resolves original font-relative width" do
    invalid_styles = [
      "display: unknown",
      "flex-direction: sideways",
      "flex-wrap: reverse",
      "align-items: baseline",
      "align-self: baseline",
      "justify-items: baseline",
      "order: first",
      "flex-grow: -1",
      "flex: 1 1 1pt extra",
      "grid-template-columns: bad",
      "grid-column: 1 / 2 / 3",
      "grid-area: 1 / 2 / 3",
      "text-align: justify",
      "font-weight: 1000",
      "font-weight: heavy",
      "font-style: oblique",
      "gap: 1pt 2pt 3pt",
      "padding: 1pt 2pt 3pt 4pt 5pt",
      "padding-top: -1pt",
      "width: 2em",
      "width: -10%",
      "height: -1pt",
      "height: var(--missing)",
      "max-width: nope",
      "min-height: var(--missing)",
      "min-height: nope",
      "overflow: scroll",
      "border-top-color: nope",
      "border-color: red nope",
      "border-style: solid nope",
      "border-width: 1pt nope",
      "color: rgb(1, 2)",
      "color: rgb()",
      "color: rgb(nope, 0, 0)",
      "color: rgba(0, 0, 0, nope)",
      "vertical-align: sideways",
      "line-break: strict",
      "color: #0000007",
      "aspect-ratio: 0 / 1",
      "aspect-ratio: 1 / 2 / 3",
      "border-collapse: maybe",
      "border-spacing: -1pt",
      "border-spacing: 1pt 2pt 3pt",
      "table-layout: balanced",
      "border: nonsense"
    ]

    Enum.each(invalid_styles, fn style ->
      case style do
        "width: 2em" ->
          assert {:ok, computed} = style_for("div", style)
          assert computed.width == 2 * computed.font_size

        _ ->
          assert style_for("div", style) == {:error, :invalid_document}
      end
    end)

    assert style_for("div", "break-before: auto") ==
             {:ok, style_for!("div", "break-before: auto")}

    assert style_for("div", "flex: 1 1 auto") == {:ok, style_for!("div", "flex: 1 1 auto")}
    assert style_for("div", "grid-template-columns: ") == {:error, :invalid_document}
    assert style_for("div", "grid-column: x") == {:error, :invalid_document}
    assert {:ok, missing_font_style} = style_for("div", "font-family: Missing")
    assert missing_font_style.font_family == "DejaVu Sans"
    assert style_for("div", "background-color: nope") == {:error, :invalid_document}
  end

  test "compute normalizes positioned layout, image fitting, and background image properties" do
    assert {:ok, positioned} =
             style_for(
               "div",
               "position: absolute; left: 10%; top: 8px; right: auto; bottom: 2pt; z-index: -3"
             )

    assert positioned.position == :absolute
    assert positioned.offsets.left == {:percent, 0.1}
    assert positioned.offsets.top == 6.0
    assert positioned.offsets.right == :auto
    assert positioned.offsets.bottom == 2.0
    assert positioned.z_index == -3

    png = png_fixture(4, 2)

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "background-image: url('asset:paper'); background-size: cover; background-position: right bottom; background-repeat: no-repeat"
          },
          children: []
        },
        %{
          type: :element,
          tag: "img",
          attributes: %{
            "src" => "asset:photo",
            "style" => "width: 40pt; height: 40pt; object-fit: contain; object-position: left top"
          },
          children: []
        }
      ]
    }

    assert {:ok, styled_tree} =
             Style.compute(dom,
               assets: %{
                 "asset:paper" => {:bytes, png},
                 "asset:photo" => {:bytes, png}
               }
             )

    [background, image] = styled_tree.children
    assert background.style.background_image.format == :png
    assert background.style.background_size == :cover
    assert background.style.background_position == {{:percent, 1.0}, {:percent, 1.0}}
    assert background.style.background_repeat == :no_repeat
    assert image.style.object_fit == :contain
    assert image.style.object_position == {{:percent, 0.0}, {:percent, 0.0}}
  end

  test "background shorthand resets earlier background image longhands without resolving the image" do
    parent = self()
    png = png_fixture(2, 1)

    resolver = fn request ->
      send(parent, {:asset_request, request})

      case request.reference do
        "asset:live" -> {:ok, png}
        _reference -> :not_found
      end
    end

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "background-image: url('https://example.com/stale-none.png'); background-size: cover; background-position: right bottom; background-repeat: no-repeat; background: none"
          },
          children: []
        },
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "background-image: url('https://example.com/stale-color.png'); background-size: 20pt 10pt; background-position: 5pt 6pt; background-repeat: repeat-x; background: red"
          },
          children: []
        },
        %{
          type: :element,
          tag: "div",
          attributes: %{
            "style" =>
              "background: none; background-image: url('asset:live'); background-size: contain; background-position: center; background-repeat: no-repeat"
          },
          children: []
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute_detailed(dom, asset_resolver: resolver)
    [none, color, live] = styled_tree.children

    for style <- [none.style, color.style] do
      assert style.background_image == nil
      assert style.background_size == {:auto, :auto}
      assert style.background_position == {{:percent, 0.0}, {:percent, 0.0}}
      assert style.background_repeat == :repeat
      refute Map.has_key?(style, :background_image_source)
    end

    assert none.style.background_color == nil
    assert color.style.background_color == {1, 0, 0}
    assert live.style.background_image.format == :png
    assert live.style.background_size == :contain
    assert live.style.background_position == {{:percent, 0.5}, {:percent, 0.5}}
    assert live.style.background_repeat == :no_repeat

    assert_received {:asset_request, %{reference: "asset:live", kind: :background_image}}
    refute_received {:asset_request, %{reference: "https://example.com/stale-none.png"}}
    refute_received {:asset_request, %{reference: "https://example.com/stale-color.png"}}
  end

  test "compute covers the supported positioning and image painting value forms" do
    assert style_for!("div", "background-image: none").background_image == nil
    assert style_for!("div", "background-size: contain").background_size == :contain
    assert style_for!("div", "background-size: auto").background_size == {:auto, :auto}
    assert style_for!("div", "background-size: 12pt").background_size == {12.0, :auto}
    assert style_for!("div", "background-size: auto 8pt").background_size == {:auto, 8.0}

    assert style_for!("div", "background-size: 25% 8pt").background_size ==
             {{:percent, 0.25}, 8.0}

    assert style_for!("div", "background-size: min(10pt, 50%)").background_size ==
             {{:min, [10.0, {:percent, 0.5}]}, :auto}

    assert style_for!("div", "background-size: min(10pt, 50%) min(8pt, 50%)").background_size ==
             {{:min, [10.0, {:percent, 0.5}]}, {:min, [8.0, {:percent, 0.5}]}}

    assert style_for!("div", "background-repeat: repeat").background_repeat == :repeat
    assert style_for!("div", "background-repeat: repeat-x").background_repeat == :repeat_x
    assert style_for!("div", "background-repeat: repeat-y").background_repeat == :repeat_y
    assert style_for!("div", "position: static").position == :static
    assert style_for!("div", "z-index: auto").z_index == :auto
    assert style_for!("div", "object-fit: fill").object_fit == :fill
    assert style_for!("div", "object-fit: cover").object_fit == :cover
    assert style_for!("div", "object-fit: none").object_fit == :none
    assert style_for!("div", "object-fit: scale-down").object_fit == :scale_down

    assert style_for!("div", "object-position: left").object_position ==
             {{:percent, 0.0}, {:percent, 0.5}}

    assert style_for!("div", "object-position: top").object_position ==
             {{:percent, 0.5}, {:percent, 0.0}}

    assert style_for!("div", "object-position: center").object_position ==
             {{:percent, 0.5}, {:percent, 0.5}}

    assert style_for!("div", "object-position: 7pt").object_position ==
             {7.0, {:percent, 0.5}}

    assert style_for!("div", "object-position: top left").object_position ==
             {{:percent, 0.0}, {:percent, 0.0}}

    assert style_for!("div", "object-position: center left").object_position ==
             {{:percent, 0.0}, {:percent, 0.5}}

    for invalid <- [
          "background-image: gradient(red, blue)",
          "background-size: auto auto auto",
          "background-repeat: space",
          "position: fixed",
          "z-index: front",
          "object-fit: invalid",
          "object-position: nowhere",
          "object-position: left right",
          "object-position: left top center"
        ] do
      assert style_for("div", invalid) == {:error, :invalid_document}
    end
  end

  test "compute resolves caller-supplied remote image and font bytes without fetching" do
    png = png_fixture(2, 1)
    font = File.read!(ttf_font_path!())
    parent = self()

    resolver = fn request ->
      send(parent, {:asset_request, request})

      case request.kind do
        :image -> {:ok, png}
        :font -> {:ok, font}
      end
    end

    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "style",
          attributes: %{},
          children: [
            %{
              type: :text,
              text:
                "@font-face { font-family: RemoteFixture; src: url('https://example.com/font.ttf'); }"
            }
          ]
        },
        %{
          type: :element,
          tag: "img",
          attributes: %{"src" => "https://example.com/photo.png"},
          children: []
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute_detailed(dom, asset_resolver: resolver)
    assert [%{style: %{image: %{format: :png}}}] = styled_tree.children

    assert_received {:asset_request, %{reference: "https://example.com/font.ttf", kind: :font}}

    assert_received {:asset_request, %{reference: "https://example.com/photo.png", kind: :image}}

    assert {:error,
            {:invalid_document, %{stage: :asset, source: "https://example.com/photo.png"}}} =
             Style.compute_detailed(
               %{
                 type: :document,
                 children: [
                   %{
                     type: :element,
                     tag: "img",
                     attributes: %{"src" => "https://example.com/photo.png"},
                     children: []
                   }
                 ]
               },
               []
             )
  end

  test "compute loads authorized file URI and absolute images and rejects malformed sources" do
    base_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-image-style-branches")
    File.mkdir_p!(base_dir)
    png_path = Path.join(base_dir, "photo.png")
    jpg_path = Path.join(base_dir, "photo.jpg")
    File.write!(png_path, png_rgba_fixture(1, 1, 4))
    File.write!(jpg_path, jpeg_fixture(4, 3, 1))
    cmyk_path = Path.join(base_dir, "cmyk.jpg")
    File.write!(cmyk_path, jpeg_fixture(2, 2, 4))
    chunked_png_path = Path.join(base_dir, "chunked.png")
    File.write!(chunked_png_path, png_with_extra_chunk_fixture())

    assert {:ok, png_style} = image_style(png_path, base_url: base_dir)
    assert png_style.image.format == :png
    assert png_style.image.data == <<255, 0, 0>>
    refute Map.has_key?(png_style.image, :alpha_data)

    transparent_png_path = Path.join(base_dir, "transparent.png")
    File.write!(transparent_png_path, png_rgba_fixture(1, 1, 0, <<0, 0, 0, 0>>))

    assert {:ok, transparent_png_style} =
             image_style(transparent_png_path, base_url: base_dir)

    assert transparent_png_style.image.data == <<0, 0, 0>>
    assert transparent_png_style.image.alpha_data == <<0>>

    assert {:ok, jpeg_style} = image_style("photo.jpg", base_url: "file://" <> base_dir)
    assert jpeg_style.image.format == :jpeg
    assert jpeg_style.image.color_space == :device_gray

    assert {:ok, cmyk_style} = image_style("cmyk.jpg", base_url: base_dir)
    assert cmyk_style.image.color_space == :device_cmyk

    assert {:ok, chunked_png_style} = image_style("chunked.png", base_url: base_dir)
    assert chunked_png_style.image.format == :png

    large_png_src = "data:image/png;base64,#{Base.encode64(png_fixture(100, 100))}"
    assert {:ok, large_png_style} = image_style(large_png_src, [])
    assert large_png_style.image.width_px == 100
    assert large_png_style.image.height_px == 100

    for filter <- [1, 2, 3] do
      filter_path = Path.join(base_dir, "filter-#{filter}.png")
      File.write!(filter_path, png_rgba_fixture(1, 2, filter))

      assert {:ok, filter_style} = image_style("filter-#{filter}.png", base_url: base_dir)
      assert filter_style.image.format == :png
    end

    for {name, fixture} <- [
          {"paeth-up.png", paeth_up_png_fixture()},
          {"paeth-up-left.png", paeth_up_left_png_fixture()}
        ] do
      File.write!(Path.join(base_dir, name), fixture)

      assert {:ok, paeth_style} = image_style(name, base_url: base_dir)
      assert paeth_style.image.format == :png
    end

    data_jpeg_src = "data:image/jpeg;base64,#{Base.encode64(jpeg_fixture(1, 1))}"
    assert {:ok, data_jpeg_style} = image_style(data_jpeg_src, [])
    assert data_jpeg_style.image.format == :jpeg

    repeated_jpeg_dom = %{
      type: :document,
      children:
        Enum.map(1..2, fn _ ->
          %{
            type: :element,
            tag: "img",
            attributes: %{"src" => data_jpeg_src},
            children: []
          }
        end)
    }

    assert {:ok, %{children: repeated_jpegs}} = Style.compute(repeated_jpeg_dom, [])
    assert Enum.map(repeated_jpegs, & &1.style.image.format) == [:jpeg, :jpeg]

    restart_jpeg_src = "data:image/jpeg;base64,#{Base.encode64(restart_jpeg_fixture())}"
    assert {:ok, restart_jpeg_style} = image_style(restart_jpeg_src, [])
    assert restart_jpeg_style.image.format == :jpeg

    svg_src =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          ~s(<svg xmlns="http://www.w3.org/2000/svg" width="2" height="1"><rect width="2" height="1" fill="red"/></svg>)
        )

    assert {:ok, svg_style} = image_style(svg_src, [])
    assert svg_style.display == :image
    assert svg_style.image.format == :png
    assert svg_style.image.width_px == 4
    assert svg_style.image.height_px == 2

    transparent_svg_src =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          ~s(<svg xmlns="http://www.w3.org/2000/svg" width="2" height="1"><rect width="1" height="1" fill="red"/></svg>)
        )

    assert {:ok, transparent_svg_style} = image_style(transparent_svg_src, [])
    assert byte_size(transparent_svg_style.image.alpha_data) == 8

    doctype_svg_src =
      "data:image/svg+xml;base64," <>
        Base.encode64("""
        <?xml version="1.0" standalone="no"?>
        <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 20010904//EN"
          "http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
        <svg version="1.0" xmlns="http://www.w3.org/2000/svg" width="3" height="2">
          <path d="M0 0h3v2H0z" fill="#b9251b"/>
        </svg>
        """)

    assert {:error, :invalid_document} = image_style(doctype_svg_src, [])

    sized_svg_src =
      "data:image/svg+xml;base64," <>
        Base.encode64(
          ~s(<svg xmlns="http://www.w3.org/2000/svg" width="1000" height="1000"><rect width="1000" height="1000" fill="red"/></svg>)
        )

    sized_svg_dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "img",
          attributes: %{"src" => sized_svg_src, "style" => "width: 10px; aspect-ratio: 1"},
          children: []
        }
      ]
    }

    assert {:ok, sized_svg_tree} = Style.compute(sized_svg_dom, [])
    [sized_svg] = sized_svg_tree.children
    assert sized_svg.style.image.format == :png
    assert sized_svg.style.image.width_px == 20
    assert sized_svg.style.image.height_px == 20

    height_sized_svg_dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "img",
          attributes: %{"src" => sized_svg_src, "style" => "height: 10px; aspect-ratio: 2"},
          children: []
        }
      ]
    }

    assert {:ok, height_sized_svg_tree} = Style.compute(height_sized_svg_dom, [])
    [height_sized_svg] = height_sized_svg_tree.children
    assert height_sized_svg.style.image.format == :png
    assert height_sized_svg.style.aspect_ratio == 2.0
    assert height_sized_svg.style.height == 7.5
    assert height_sized_svg.style.image.width_px == 40
    assert height_sized_svg.style.image.height_px == 20

    assert {:ok, %{boxes: height_sized_boxes}} =
             NativeElixirPdfUtilities.HtmlToPdf.Layout.layout(height_sized_svg_tree)

    [height_sized_paint] = Enum.filter(height_sized_boxes, &(&1.type == :image))
    assert height_sized_paint.width == 15.0 and height_sized_paint.height == 7.5

    malformed_sources = [
      {"data:image/svg+xml;base64,#{Base.encode64("<svg></svg>")}"},
      {"data:image/svg+xml;base64,#{Base.encode64(~s(<svg width="1" height="1"><))}"},
      {"data:image/svg+xml;base64,#{Base.encode64(<<255>>)}"},
      {"data:image/png;base64,%%%"},
      {"data:image/png;base64,#{Base.encode64(jpeg_fixture(1, 1))}"},
      {"data:image/png;base64,#{Base.encode64("not png")}"},
      {"data:image/png;base64,#{Base.encode64(<<137, 80, 78, 71, 13, 10, 26, 10, 1, 2, 3>>)}"},
      {"data:image/png;base64,#{Base.encode64(bad_idat_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(truncated_idat_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(malformed_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(short_row_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(invalid_filter_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(trailing_png_fixture())}"},
      {"data:image/png;base64,#{Base.encode64(oversized_png_fixture())}"},
      {"data:image/jpeg;base64,#{Base.encode64(<<1, 2, 3>>)}"},
      {"data:image/jpeg;base64,#{Base.encode64(<<255, 216, 255, 192, 0, 17, 7, 0, 1, 0, 1, 3>>)}"},
      {"data:image/jpeg;base64,#{Base.encode64(<<255, 216, 255, 217>>)}"},
      {"missing.png"},
      {"bad\u0000name.png"}
    ]

    Enum.each(malformed_sources, fn {src} ->
      assert image_style(src, base_url: base_dir) == {:error, :invalid_document},
             "expected malformed image source to be rejected: #{inspect(src)}"
    end)

    assert image_style("relative.png", []) == {:error, :invalid_document}

    assert image_style("photo.jpg", base_url: "https://example.com") ==
             {:error, :invalid_document}
  after
    base_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-image-style-branches")
    File.rm_rf(base_dir)
  end

  test "compute rejects malformed document children and stylesheet options" do
    assert Style.compute(%{type: :document, children: [%{type: :bogus}]}, []) ==
             {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{type: :element, tag: "unknown", attributes: %{}, children: []}
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert {:ok, link_tree} =
             Style.compute(
               %{
                 type: :document,
                 children: [
                   %{
                     type: :element,
                     tag: "a",
                     attributes: %{},
                     children: [%{type: :text, text: "x"}]
                   }
                 ]
               },
               []
             )

    [link] = link_tree.children
    assert link.style.display == :inline
    assert link.style.color == {0, 0, 0}
    refute Map.has_key?(link.style, :link_url)

    assert Style.compute(%{type: :document, children: []}, stylesheets: :bad) ==
             {:error, :invalid_document}

    assert Style.compute(
             %{type: :document, children: []},
             stylesheets: [{:file, "missing-file.css"}]
           ) ==
             {:error, :invalid_document}

    assert Style.compute(%{type: :document, children: []}, stylesheets: [123]) ==
             {:error, :invalid_document}

    for invalid_entry <- ["p { color: red; }", {:css, 123}, {:file, 123}] do
      assert Style.compute_detailed(
               %{type: :document, children: []},
               stylesheets: [invalid_entry]
             ) ==
               {:error,
                {:invalid_options,
                 %{
                   stage: :options,
                   reason: :invalid_options,
                   message:
                     "stylesheets option must be a list of {:css, css} or {:file, path} tuples"
                 }}}
    end

    assert Style.compute_detailed(%{type: :document, children: []}, [:not_options]) ==
             {:error,
              {:invalid_document,
               %{
                 stage: :style,
                 reason: :invalid_document,
                 message: "style options must be a keyword list"
               }}}

    assert {:error,
            {:invalid_document, %{stage: :style, reason: :invalid_document, message: message}}} =
             Style.load_stylesheets(:not_a_document, [])

    assert message =~ "document tree"

    assert Style.compute(%{type: :document, children: []}, fonts: :bad) ==
             {:error, :invalid_document}

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "a",
                   attributes: %{"href" => 123},
                   children: [%{type: :text, text: "bad"}]
                 }
               ]
             },
             []
           ) == {:error, :invalid_document}

    assert {:ok, %{type: :document, children: []}} =
             Style.compute(%{type: :document, children: []}, default_font: "Missing")

    assert Style.compute(
             %{
               type: :document,
               children: [
                 %{
                   type: :element,
                   tag: "style",
                   attributes: %{},
                   children: [%{type: :element, tag: "span", attributes: %{}, children: []}]
                 },
                 %{
                   type: :element,
                   tag: "p",
                   attributes: %{"style" => ":"},
                   children: [%{type: :text, text: "Bad inline"}]
                 }
               ]
             },
             stylesheets: [{:css, "aside { color: red; }"}]
           ) == {:error, :invalid_document}
  end

  defp ttf_font_path! do
    [
      Paths.project("priv/fonts/dejavu/DejaVuSans.ttf"),
      "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
      "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
      "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
    ]
    |> Enum.find(&File.exists?/1)
    |> case do
      nil -> flunk("No local TTF font fixture found")
      path -> path
    end
  end

  defp style_for(tag, style) do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: tag,
          attributes: %{"style" => style},
          children: [%{type: :text, text: "Styled"}]
        }
      ]
    }

    case Style.compute(dom, []) do
      {:ok, styled_tree} ->
        [element] = styled_tree.children
        {:ok, element.style}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp text_for_style(style, text) do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "div",
          attributes: %{"style" => style},
          children: [%{type: :text, text: text}]
        }
      ]
    }

    assert {:ok, styled_tree} = Style.compute(dom, [])
    [element] = styled_tree.children
    [text_node] = element.children
    text_node.text
  end

  defp edges(value) do
    %{top: value, right: value, bottom: value, left: value}
  end

  defp style_for!(tag, style) do
    assert {:ok, computed} = style_for(tag, style)
    computed
  end

  defp image_style(src, opts) do
    dom = %{
      type: :document,
      children: [
        %{type: :element, tag: "img", attributes: %{"src" => src}, children: []}
      ]
    }

    case Style.compute(dom, opts) do
      {:ok, styled_tree} ->
        [image] = styled_tree.children
        {:ok, image.style}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp assert_style_value(actual, expected) do
    case {actual, expected} do
      {{a, b, c}, {x, y, z}} ->
        assert_in_delta a, x, 0.0001
        assert_in_delta b, y, 0.0001
        assert_in_delta c, z, 0.0001

      _ ->
        assert actual == expected
    end
  end

  defp png_fixture(width, height) do
    png_fixture(width, height, 2, 0)
  end

  defp png_rgba_fixture(width, height, filter) do
    png_fixture(width, height, 6, filter)
  end

  defp png_rgba_fixture(width, height, filter, pixel) do
    png_fixture(width, height, 6, filter, pixel)
  end

  defp png_fixture(width, height, color_type, filter) do
    pixel =
      case color_type do
        2 -> <<255, 0, 0>>
        6 -> <<255, 0, 0, 255>>
      end

    png_fixture(width, height, color_type, filter, pixel)
  end

  defp png_fixture(width, height, color_type, filter, pixel) do
    row = :binary.copy(pixel, width)
    rows = Enum.map_join(1..height, "", fn _index -> <<filter>> <> row end)

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<width::32, height::32, 8, color_type, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(rows)) <>
      png_chunk("IEND", "")
  end

  defp png_chunk(type, data) do
    crc = :erlang.crc32(type <> data)
    <<byte_size(data)::32, type::binary, data::binary, crc::32>>
  end

  defp png_with_extra_chunk_fixture do
    row = <<0, 255, 0, 0>>

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("tEXt", "ignored") <>
      png_chunk("IDAT", :zlib.compress(row)) <>
      png_chunk("IEND", "")
  end

  defp malformed_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      <<1::32, "IHDR", 0, 0, 0, 0, 0::32>>
  end

  defp invalid_filter_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(<<9, 255, 0, 0>>)) <>
      png_chunk("IEND", "")
  end

  defp short_row_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<2::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(<<0, 255, 0, 0>>)) <>
      png_chunk("IEND", "")
  end

  defp paeth_up_png_fixture do
    rows =
      <<0, 100, 0, 0>> <>
        <<4, 0, 0, 0>>

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 2::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(rows)) <>
      png_chunk("IEND", "")
  end

  defp paeth_up_left_png_fixture do
    rows =
      <<0, 25, 0, 0, 50, 0, 0>> <>
        <<4, 231, 0, 0, 0, 0, 0>>

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<2::32, 2::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(rows)) <>
      png_chunk("IEND", "")
  end

  defp bad_idat_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", "not zlib") <>
      png_chunk("IEND", "")
  end

  defp truncated_idat_png_fixture do
    compressed = :zlib.compress(<<0, 255, 0, 0>>)
    truncated = binary_part(compressed, 0, byte_size(compressed) - 1)

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", truncated) <>
      png_chunk("IEND", "")
  end

  defp trailing_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<1::32, 1::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(<<0, 255, 0, 0, 1>>)) <>
      png_chunk("IEND", "")
  end

  defp oversized_png_fixture do
    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<100_000::32, 100_000::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress("")) <>
      png_chunk("IEND", "")
  end

  defp jpeg_fixture(width, height, components \\ 3),
    do: JpegFixture.baseline(width, height, components)

  defp restart_jpeg_fixture do
    data = JpegFixture.baseline(16, 8, 1)
    [header, _scan] = :binary.split(data, <<255, 218>>)
    scan = JpegFixture.segment(218, <<1, 1, 0, 0, 63, 0>>)
    header <> JpegFixture.segment(221, <<1::16>>) <> scan <> <<63, 255, 208, 63, 255, 217>>
  end

  test "list defaults use outside markers and browser item spacing" do
    NativeElixirPdfUtilities.Validators.HtmlRecoveryValidator.with_options(
      [html_recovery: true],
      fn ->
        assert {:ok, dom} = HtmlParser.parse("<ul><li>First<li>Second</ul>")
        assert {:ok, %{children: [list]}} = Style.compute(dom, [])
        assert list.style.padding.left == 30.0
        assert list.style.margin.top == list.style.font_size
        assert list.style.margin.bottom == list.style.font_size
        assert Enum.all?(list.children, &(&1.style.margin.bottom == 0.0))
      end
    )
  end

  test "readonly selector values remain authored and disabled styling stays independent" do
    assert {:ok, dom} =
             HtmlParser.parse("""
             <style>
               input[readonly=READONLY] { color:blue; }
               input[disabled] { background-color:#eeeeee; }
             </style>
             <input readonly="readonly" value="A&#10;B">
             <input readonly="readonly" disabled value="A&#10;B">
             <input readonly="false" value="C">
             """)

    assert {:ok, tree} = Style.compute(dom)
    [readonly, both, literal] = tree.children
    assert readonly.style.color == {0, 0, 1}
    assert readonly.style.background_color == {1.0, 1.0, 1.0}
    assert both.style.color == {0, 0, 1}
    assert both.style.background_color == {238 / 255, 238 / 255, 238 / 255}
    assert literal.style.color == {0, 0, 0}
    assert literal.style._form_control.attributes["readonly"] == "false"
    assert readonly.style._form_control.initial_state.value == "AB"
    assert readonly.style._form_control.attributes["value"] == "A\nB"
  end

  test "readonly caller trees delegate applicability to the validator" do
    for {tag, attributes} <- [
          {"button", %{"readonly" => ""}},
          {"select", %{"readonly" => ""}},
          {"p", %{"readonly" => ""}}
        ] do
      dom = %{
        type: :document,
        children: [%{type: :element, tag: tag, attributes: attributes, children: []}]
      }

      assert {:ok, styled} = Style.compute_detailed(dom)
      assert hd(styled.children).attributes == attributes
    end
  end

  test "contact input selectors use authored values before sanitation" do
    for type <- ["tel", "URL", "EmAiL"] do
      assert {:ok, dom} =
               HtmlParser.parse(
                 ~s(<input type="#{type}" value=" &#9;A&#13;B&#10;C " readonly="false">)
               )

      css = ~s([value^=" "][readonly="FALSE" i] { color:blue; })
      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, stylesheets: [{:css, css}])
      assert node.style.color == {0.0, 0.0, 1.0}
      assert hd(dom.children).attributes["value"] == " \tA\rB\nC "
      assert node.style._form_control.attributes["readonly"] == "false"
      expected = if type == "tel", do: " \tABC ", else: "ABC"
      assert node.style._form_control.initial_state.value == expected
      assert node.style._form_control.attributes["value"] == " \tA\rB\nC "
      assert node.style.form_kind == :text
    end
  end

  test "multiple email selectors retain authored list values before sanitation" do
    assert {:ok, dom} =
             HtmlParser.parse(
               ~s(<input type="EMAIL" multiple="false" readonly name="contacts" value=" A ,&#10; B ">)
             )

    css = ~s([multiple="FALSE" i][value^=" "] { color:blue; })
    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, stylesheets: [{:css, css}])
    assert node.style.color == {0, 0, 1}
    assert node.style._form_control.initial_state.value == "A,B"
    assert node.style._form_control.attributes["value"] == " A ,\n B "
    assert hd(dom.children).attributes["value"] == " A ,\n B "

    for tag <- ["textarea", "button", "p"] do
      attributes =
        if tag == "input",
          do: %{"type" => "text", "multiple" => "", "readonly" => ""},
          else: %{"multiple" => ""}

      invalid = %{
        type: :document,
        children: [%{type: :element, tag: tag, attributes: attributes, children: []}]
      }

      assert {:ok, styled} = Style.compute_detailed(invalid)
      assert hd(styled.children).attributes == attributes
    end
  end

  test "caller input trees share the same effective state validation and defaults" do
    for type <- ["", "unknown", "hidden ", "checKbox"] do
      dom = %{
        type: :document,
        children: [
          %{
            type: :element,
            tag: "input",
            attributes: %{"type" => type, "value" => "A\rB\nC"},
            children: []
          }
        ]
      }

      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom)
      assert node.style.form_kind == :text
      assert node.style._form_control.attributes["type"] == type
      assert node.style._form_control.initial_state.value == "ABC"
      assert node.style._form_control.attributes["value"] == "A\rB\nC"
    end

    for {attributes, children} <- [
          {%{}, [%{type: :text, text: "Invalid child"}]}
        ] do
      dom = %{
        type: :document,
        children: [%{type: :element, tag: "input", attributes: attributes, children: children}]
      }

      assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(dom)
      assert diagnostic.stage == :style and diagnostic.operation == :compute
      assert diagnostic.source == "input" and diagnostic.message =~ "effective type"
    end
  end

  test "button inputs use static button styles without reserving PDF fields" do
    for {type, label} <- [{"button", ""}, {"RESET", "Reset"}, {"SuBmIt", "Submit"}] do
      assert {:ok, dom} = HtmlParser.parse(~s(<input type="#{type}" disabled name="reserved">))
      css = ~s(input[type="#{type}"] { color:blue; } input[value] { color:red; })
      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, stylesheets: [{:css, css}])
      assert node.style.form_kind == :input_button and node.style.text_align == :center
      assert node.style.white_space == :pre
      assert node.style.box_sizing == :border_box and node.style.color == {0.0, 0.0, 1.0}
      assert Enum.map(node.children, & &1.text) == [label]
      refute Map.has_key?(node.style, :_form_control)
      refute Map.has_key?(node.style, :_form_owner)

      assert {:ok, %{children: [overridden]}} =
               Style.compute_detailed(dom,
                 stylesheets: [{:css, "input { text-align:left; box-sizing:content-box; }"}]
               )

      assert overridden.style.text_align == :left and overridden.style.box_sizing == :content_box
      inactive = put_in(dom, [:children, Access.at(0), :attributes, "readonly"], "")

      inactive =
        update_in(inactive, [:children, Access.at(0), :attribute_order], &(&1 ++ ["readonly"]))

      assert {:ok, %{children: [preserved]}} = Style.compute_detailed(inactive)
      assert preserved.style.form_kind == :input_button
      assert Enum.map(preserved.children, & &1.text) == [label]
      refute Map.has_key?(preserved.style, :_form_control)
    end

    assert {:ok, dom} = HtmlParser.parse(~s(<button value="Submission value"></button>))
    assert {:ok, %{children: [empty]}} = Style.compute_detailed(dom)
    assert empty.children == []
  end

  test "enabled disabled selectors use initial state without treating readonly or aria as disabled" do
    html =
      ~s(<input type="unknown" readonly value="Readonly"><input disabled="false" value="Disabled"><button aria-disabled="true">Enabled button</button><p role="button" aria-disabled="true">Ordinary text</p>)

    assert {:ok, dom} = HtmlParser.parse(html)

    css =
      "*:enabled { color:blue; } *:disabled { color:red; } input:enabled:not([readonly]) { color:green; }"

    assert {:ok, %{children: [readonly, disabled, button, paragraph]}} =
             Style.compute_detailed(dom, stylesheets: [{:css, css}])

    assert readonly.style.color == {0.0, 0.0, 1.0}
    assert disabled.style.color == {1.0, 0.0, 0.0}
    assert button.style.color == {0.0, 0.0, 1.0}
    assert paragraph.style.color == {0.0, 0.0, 0.0}
    assert disabled.style._form_control.attributes["disabled"] == "false"
    assert readonly.style._form_control.attributes["type"] == "unknown"
  end

  test "image hints precede authored CSS and keep loaded natural aspect ratio" do
    png =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(8, 4, 2, 8, 0, fn _, _ ->
        [20, 80, 160]
      end)

    for {attributes, width, height} <- [
          {~s(width="24" height="12"), 18.0, 9.0},
          {~s(width="24.5%" height="bad"), {:percent, 0.245}, nil},
          {~s(width="+24"), nil, nil}
        ] do
      assert {:ok, dom} = HtmlParser.parse("<img src='tile' #{attributes}>")

      assert {:ok, %{children: [node]}} =
               Style.compute_detailed(dom, assets: %{"tile" => {:bytes, png}})

      assert Map.get(node.style, :width) == width
      assert Map.get(node.style, :height) == height
    end

    assert {:ok, dom} = HtmlParser.parse(~s(<img src="tile" width="24" height="48">))
    css = ~s(img[width="24"][height="48"] { width:30pt; height:auto; })

    assert {:ok, %{children: [node]}} =
             Style.compute_detailed(dom,
               assets: %{"tile" => {:bytes, png}},
               stylesheets: [{:css, css}]
             )

    assert node.style.width == 30.0 and node.style.height == :auto
    refute Map.has_key?(node.style, :aspect_ratio)
  end

  test "password artwork obscures graphemes while fields preserve sanitized values and raw CSS" do
    assert {:ok, dom} =
             HtmlParser.parse(~s(<input type="PASSWORD" name="secret" value="A&#13;é🦊">))

    css = ~s(input[type="password"][value$="🦊"] {color:red;})
    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, stylesheets: [{:css, css}])
    assert node.style.form_kind == :text and node.style.color == {1.0, 0.0, 0.0}
    assert node.style._form_control.initial_state.value == "Aé🦊"
    assert node.style._form_control.attributes["value"] == "A\ré🦊"
    assert Enum.map(node.children, & &1.text) == ["•••"]
  end

  test "fieldset hidden boxes and legend alignment defaults retain authored overrides" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    for {attribute, expected} <- [
          {"align=right", :flex_end},
          {"align=RIGHT", :flex_end},
          {"align=center", :center},
          {"align=unknown", :flex_start}
        ] do
      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<fieldset><legend #{attribute}>Caption</legend></fieldset><fieldset hidden></fieldset>"
               )

      assert {:ok, %{children: [visible, hidden]}} = Style.compute_detailed(dom)
      assert hd(visible.children).style.justify_self == expected
      assert hidden.style.display == :none
    end
  end

  test "fieldset defaults and author overrides preserve effective selector state" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    html = """
    <style>fieldset:disabled{color:red}fieldset:enabled{color:blue}
    input:disabled{background-color:red}input:enabled{background-color:blue}
    input[disabled]{color:green}legend:disabled{color:green}</style>
    <fieldset disabled=false style='font-size:10pt'><legend align=CENTER>Caption<input></legend>
    <input value=Locked><fieldset><legend>Nested<input></legend></fieldset></fieldset>
    <fieldset style='font-size:20pt;display:flex;padding:0;border:0'><legend>Other</legend>Body</fieldset>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    assert {:ok, %{children: [disabled, enabled]}} = Style.compute_detailed(dom)
    assert disabled.style.display == :block and disabled.style._fieldset
    assert disabled.style.border_widths == %{top: 1.5, right: 1.5, bottom: 1.5, left: 1.5}
    assert disabled.style.border_styles.top == :groove
    assert disabled.style.padding == %{top: 3.4921875, right: 7.5, bottom: 6.24609375, left: 7.5}
    assert disabled.style.margin == %{top: 0.0, right: 1.5, bottom: 0.0, left: 1.5}
    assert disabled.style.color == {1, 0, 0}
    [legend, input, nested] = Enum.filter(disabled.children, &(&1.type == :element))
    assert legend.style.justify_self == :center
    assert legend.style.padding.left == 1.5
    assert legend.style.color == {1, 0, 0}
    exempt = List.last(legend.children)
    assert exempt.style.background_color == {0, 0, 1}
    refute exempt.style._form_control.initial_state.disabled
    assert input.style.background_color == {1, 0, 0}
    assert input.style._form_control.initial_state.disabled
    refute Map.has_key?(input.style._form_control.attributes, "disabled")
    assert nested.style.color == {1, 0, 0}
    assert enabled.style.color == {0, 0, 1}
    assert enabled.style.display == :block and enabled.style._fieldset_content_display == :flex
    assert enabled.style.padding.top == 0.0 and enabled.style.border_widths.top == 0.0
  end

  test "fieldset disabling preserves owner-scoped radio winners including disabled and hidden controls" do
    alias NativeElixirPdfUtilities.Validators.HtmlFormValidator

    html = """
    <form id=owner><fieldset disabled><legend><input type=radio name=mode value=A checked></legend>
    <input type=radio name=mode value=B checked><input type=hidden name=secret>
    <template><input type=radio name=mode value=Ignored checked></template></fieldset></form>
    <input type=radio name=mode value=C checked form=owner><input type=radio name=mode value=D checked>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    [form, external, independent] = HtmlFormValidator.prepare_initial_nodes(dom.children)
    [fieldset] = form.children
    [legend, disabled, hidden, template] = Enum.filter(fieldset.children, &(&1.type == :element))
    [exempt] = legend.children
    refute exempt._initial_form_state.checked
    refute disabled._initial_form_state.checked
    assert external._initial_form_state.checked and independent._initial_form_state.checked
    assert external._initial_form_state.owner == form._selector_id
    assert independent._initial_form_state.owner == nil
    refute exempt._initial_form_state.disabled
    assert disabled._initial_form_state.disabled and hidden._initial_form_state.disabled
    assert template.children == []

    [next] =
      HtmlFormValidator.prepare_initial_nodes([
        %{type: :element, tag: "input", attributes: %{}, children: []}
      ])

    refute next._initial_form_state.disabled
  end

  test "fieldset editability and gauge labels consume canonical state across hidden legends and nested scopes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    alias NativeElixirPdfUtilities.Validators.HtmlFormValidator

    html = """
    <style>input:read-write,textarea:read-write{color:blue}input:read-only,textarea:read-only{color:red}</style>
    <fieldset disabled><legend hidden><input id=hidden_exempt></legend>
    <legend><input id=later value=Later></legend><input id=body value=Body>
    <fieldset><legend><textarea id=nested>Nested</textarea></legend></fieldset>
    <label for=progress>Gauge label</label><progress id=progress value=.5></progress>
    <label for=meter>Meter label</label><meter id=meter value=.75></meter>
    <input type=radio name=empty><input type=radio name=empty></fieldset>
    <fieldset disabled><legend><textarea id=exempt>Exempt</textarea>
    <input id=own disabled value=Own></legend></fieldset>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    [first, second] =
      HtmlFormValidator.prepare_initial_nodes(
        Enum.filter(dom.children, &(Map.get(&1, :tag) == "fieldset"))
      )

    hidden_input = hd(hd(first.children).children)
    refute hidden_input._initial_form_state.disabled
    [exempt, own] = Enum.filter(hd(second.children).children, &(&1.type == :element))
    refute exempt._initial_form_state.disabled
    assert own._initial_form_state.disabled

    [_, _, _, _, label, progress, meter_label, meter, _, _] =
      Enum.filter(first.children, &(&1.type == :element))

    assert label._initial_label_control == progress._selector_id
    assert meter_label._initial_label_control == meter._selector_id
    refute Map.has_key?(progress, :_initial_form_state)
    refute Map.has_key?(meter, :_initial_form_state)
    assert {:ok, %{children: [first, second]}} = Style.compute_detailed(dom)

    [_, later, body, nested, _label, progress, _meter_label, meter, radio_a, radio_b] =
      Enum.filter(first.children, &(&1.type == :element))

    for node <- [hd(later.children), body, hd(hd(nested.children).children)] do
      assert node.style.color == {1, 0, 0}
      assert node.style._form_control.initial_state.disabled
      refute Map.has_key?(node.style._form_control.attributes, "disabled")
    end

    assert progress.style._gauge.position == 0.5
    assert meter.style._gauge.position == 0.75
    refute Map.has_key?(progress, :_initial_form_state)
    refute Map.has_key?(meter, :_initial_form_state)
    assert radio_a.style._form_control.initial_state.indeterminate
    assert radio_b.style._form_control.initial_state.indeterminate
    [exempt, own] = Enum.filter(hd(second.children).children, &(&1.type == :element))
    assert exempt.style.color == {0, 0, 1}
    assert own.style.color == {1, 0, 0}
  end
end
