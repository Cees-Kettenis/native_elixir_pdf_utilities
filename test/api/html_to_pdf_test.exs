alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdfTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Split
  alias NativeElixirPdfUtilities.Text

  test "token HTML children retain independent box painting and placement" do
    for style <- [
          "background-color:red",
          "padding:2pt",
          "border:1pt solid red",
          "position:relative;left:2pt",
          "width:30pt"
        ] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<math><mtext><span style='#{style}'>Visible</span></mtext></math>"
               )

      assert {:ok, "Visible"} = Text.extract(pdf, layout: false)
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><mn>12</mn><mo>+</mo><mo>⁢</mo><mtext><a href='https://example.test/'>Link</a></mtext><mo>=</mo><mn>34</mn></math>"
             )

    assert {:ok, "12+⁢Link=34"} = Text.extract(pdf, layout: false)
    assert pdf =~ "/URI"
    assert pdf =~ "https://example.test/"
  end

  test "native token children omit hidden boxes and diagnose unavailable required text" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><mtext><span style='background:yellow'>Shown</span><span style='display:none'>Hidden</span></mtext></math>"
             )

    assert {:ok, "Shown"} = Text.extract(pdf, layout: false)

    assert {:error, {:unsupported_glyph, diagnostic}} =
             HtmlToPdf.render(
               "<math><mtext><span style='background:yellow'>𐍈</span></mtext></math>",
               unsupported_glyphs: :error,
               system_font_discovery: false
             )

    assert diagnostic.message =~ "glyph" and diagnostic.stage == :font
  end

  test "same-document math child controls retain their form owner and native geometry" do
    html =
      "<form id='owner'><math><mtext><input name='math_child' value='Value'></mtext></math></form>"

    assert {:ok, static} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(static)
    assert {:ok, "Value"} = Text.extract(static, layout: false)
    assert {:ok, interactive} = HtmlToPdf.render(html)
    assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(interactive)
    assert field.name == "math_child" and field.value == "Value"
  end

  test "HTML token children use their selected ordinary font without requiring MATH data" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><mtext><span style='font-family:DejaVu Sans Mono'>Override</span></mtext></math>"
             )

    assert {:ok, "Override"} = Text.extract(pdf, layout: false)
    assert pdf =~ "DejaVuSansMono"
  end

  test "painted mi HTML descendants retain original-node math-auto and extraction" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><mi style='display:inline-block'>f<span style='background:yellow'>i</span></mi></math>"
             )

    assert {:ok, "fi"} = Text.extract(pdf, layout: false)
    assert {:ok, pdf} = HtmlToPdf.render("<math><mtext><span>Alpha<br>Beta</span></mtext></math>")
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.replace(text, "\n", "") == "AlphaBeta"
  end

  test "processing instructions preserve combined gauge table metadata and first-base output" do
    html = """
    <?marker href=unused.png?><html><head><?marker data=Invisible?>
    <base target=_blank><?marker?><base href='https://example.test/reports/'>
    <base href='https://ignored.test/'>
    <style>td:nth-child(2){color:blue}progress:indeterminate{accent-color:red}</style>
    </head><body><section itemscope itemref=outside translate=no>
    <label for=g>Gauge label</label><table><tbody><?marker?>
    <tr><td rowspan=0><progress id=g><?marker src=unused.png?><span>Hidden fallback</span></progress></td>
    <?marker?><td><a href='first?view=print'>First link</a></td></tr>
    <?marker?><tr style='height:20pt'></tr><tr><td>
    <meter min=-1 max=1 value=0 style='appearance:none'><?marker?>Hidden meter fallback</meter>
    <a href=last>Last link</a></td></tr></tbody></table></section>
    <p id=outside>Metadata target</p><template><?marker src=unused.png?>
    <span>Inert content</span></template></body></html>
    """

    reference = Regex.replace(~r/<\?.*?>/s, html, "")

    for mode <- [:static, :interactive] do
      opts = [
        forms: mode,
        compress_streams: false,
        asset_resolver: fn request -> flunk("nonpainting data requested #{inspect(request)}") end
      ]

      assert {:ok, pdf} = HtmlToPdf.render(html, opts)
      assert {:ok, ^pdf} = HtmlToPdf.render(reference, opts)
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
      assert {:ok, text} = Text.extract(pdf)

      for caption <- ["Gauge label", "First link", "Last link", "Metadata target"],
          do: assert(text =~ caption)

      for hidden <- ["Hidden fallback", "Hidden meter fallback", "Inert content", "Invisible"],
          do: refute(text =~ hidden)

      assert pdf =~ "/URI (https://example.test/reports/first?view=print)"
      assert pdf =~ "/URI (https://example.test/reports/last)"
      assert length(Regex.scan(~r{/Subtype /Link}, pdf)) == 2
      assert pdf =~ "/Shading << /Gauge"
    end
  end

  test "processing instructions never alter first-base image resolution or metadata references" do
    png =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(8, 4, 2, 8, 0, fn _, _ ->
        [255, 0, 0]
      end)

    html = """
    <?marker?><head><template><?marker?><base href='https://inert.test/'></template>
    <base target=_blank><?marker href='https://pi.test/'?><base href='https://chosen.test/assets/'>
    <base href='https://ignored.test/'></head><body><div itemscope itemref=target>
    <?marker src=denied.png?><a href=photo><img src=chosen.png style='width:24pt;height:12pt'></a>
    </div><?marker?><p id=target>Image metadata target</p></body>
    """

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 forms: mode,
                 compress_streams: false,
                 asset_resolver: fn request ->
                   send(self(), {:combined_image_request, request})
                   {:ok, png}
                 end
               )

      assert_receive {:combined_image_request,
                      %{reference: "https://chosen.test/assets/chosen.png", kind: :image}}

      refute_receive {:combined_image_request, _}
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Image metadata target"
      assert pdf =~ "/URI (https://chosen.test/assets/photo)"
      assert length(Regex.scan(~r{/Subtype /Link}, pdf)) == 1
      assert length(Regex.scan(~r{ Do\b}, pdf)) == 1
    end
  end

  test "processing instructions cannot conceal combined table overlap or metadata failures" do
    html = """
    <table><?marker?><tbody><tr><td>First</td><td rowspan=2>
    <meter value=.5><?marker?>Hidden fallback</meter></td></tr>
    <?marker?><tr><td colspan=2>Overlap</td></tr></tbody></table>
    """

    assert {:error, {:invalid_layout, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
    assert diagnostic.operation == :layout
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlTableValidator
    assert diagnostic.source == "td"
    assert diagnostic.message =~ "row 2, column 1"
    assert diagnostic.message =~ "prior rowspan in column 2"
    assert diagnostic.message =~ "reduce"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<?marker?><div itemscope itemref=missing><progress><?marker?></progress></div>"
             )

    assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
    assert diagnostic.operation == :compute and diagnostic.source == "div"
    assert diagnostic.message =~ "same authored tree"
  end

  test "XML PI getters retain inert behavior and standalone template-for diagnostic ownership" do
    html =
      "<?marker name='A&amp;B' src='missing.png' onclick='run()'?>" <>
        "<p>Visible</p><?start name='A' name='B'?>" <>
        "<template><?marker name='inert'?><img src=unused.png></template><?end?>"

    for mode <- [:static, :interactive] do
      options = [
        forms: mode,
        system_font_discovery: false,
        asset_resolver: fn request -> flunk("PI/inert resource fetched #{inspect(request)}") end
      ]

      assert {:ok, actual} = HtmlToPdf.render(html, options)
      assert {:ok, expected} = HtmlToPdf.render("<p>Visible</p>", options)
      assert actual == expected
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(actual)
    end

    token = "<template for='A&amp;B'>"
    source = "<?marker name='A&amp;B'?>\r\n  " <> token <> "<p>Replacement</p></template>"
    assert {:ok, pdf} = HtmlToPdf.render(source)
    # This marker belongs to the implied head, which remains non-painted.
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf)

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_document_detailed(source)

    assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
    assert diagnostic.operation == :parse and diagnostic.source == token
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.TemplatePatchValidator
    assert diagnostic.line == 2 and diagnostic.column == 3
    assert diagnostic.message =~ "no original source has been discarded"
  end

  test "token-time omitted document and table parents preserve the public PDF result" do
    implied =
      "<title>Metadata</title><?head></head><p>Opening<table><col><col></colgroup>" <>
        "<tr><td>First<td>Second<?cell></tbody><tr><td>Third<td>Fourth</table>" <>
        "<template><img src=unused.png><p>Inert</p></template>"

    explicit =
      "<html><head><title>Metadata</title><?head></head><body><p>Opening</p>" <>
        "<table><colgroup><col><col></colgroup><tbody><tr><td>First</td>" <>
        "<td>Second<?cell></td></tr></tbody><tbody><tr><td>Third</td>" <>
        "<td>Fourth</td></tr></tbody></table>" <>
        "<template><img src=unused.png><p>Inert</p></template></body></html>"

    for mode <- [:static, :interactive] do
      options = [
        forms: mode,
        system_font_discovery: false,
        compress_streams: false,
        asset_resolver: fn request -> flunk("inert resource fetched #{inspect(request)}") end
      ]

      assert {:ok, actual} = HtmlToPdf.render(implied, options)
      assert {:ok, expected} = HtmlToPdf.render(explicit, options)
      assert actual == expected
      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(actual)
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(actual)
      assert_pdf_text(actual, ["Opening", "First", "Second", "Third", "Fourth"])
      assert {:ok, text} = Text.extract(actual)
      refute text =~ "Inert"
    end
  end

  test "token-time implied parents preserve strict public diagnostics and original locations" do
    for {source, token, line, column, operation} <- [
          {"<title>T</title>\r\n<head></head>", "<head>", 2, 1, :render},
          {"<body></body>\r<head></head>", "<head>", 2, 1, :render},
          {"<p>e\u0301</p>\r\n  <template for=missing><p>Inert</p></template>",
           "<template for=missing>", 2, 3, :parse},
          {"<table><tr><td>A</table>\r\n  <script>run()</script>", "<script>", 2, 3, :render}
        ] do
      result =
        if operation == :parse do
          assert {:ok, actual} = HtmlToPdf.render(source)
          assert {:ok, expected} = HtmlToPdf.render("<p>e\u0301</p>")
          assert actual == expected
          NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_document_detailed(source)
        else
          HtmlToPdf.render(source)
        end

      assert {:error, {:unsupported_html, diagnostic}} = result
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.stage == :html and diagnostic.operation == operation

      if operation == :parse do
        assert diagnostic.module == NativeElixirPdfUtilities.Validators.TemplatePatchValidator
        assert diagnostic.message =~ "inert fallback, no earlier matching marker"
        assert diagnostic.message =~ "no original source has been discarded"
      end

      assert diagnostic.source == token
      assert diagnostic.line == line and diagnostic.column == column
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end
  end

  test "processing instruction data creates no resources, boxes or PDF fields" do
    html =
      File.read!(Paths.fixture("html_to_pdf/browser_parity/html_processing_instructions.html"))

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 forms: mode,
                 system_font_discovery: false,
                 asset_resolver: fn request -> flunk("PI data fetched #{inspect(request)}") end
               )

      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)

      assert_pdf_text(pdf, [
        "Processing instruction markers",
        "First content",
        "Second content",
        "Table content",
        "List content",
        "Literal <?marker name=Raw?>"
      ])

      assert {:ok, text} = Text.extract(pdf)
      refute text =~ "Invisible PI data"
      refute text =~ "Inert template content"
      refute text =~ "name=unused.png"
    end

    for html <- [
          "<?marker><template><script>run()</script></template>",
          "<?marker><template><img src='javascript:run()'></template>",
          "<?marker><p onclick='run()'>Active</p>"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :html and is_binary(diagnostic.source)
      assert diagnostic.line == 1 and diagnostic.column > 1
    end
  end

  test "gauges preserve captions across pages and create no interactive fields or fallback requests" do
    html =
      Enum.map_join(1..8, fn index ->
        "<p style='margin:0;height:30pt'>Gauge #{index} <progress value=.5></progress> <meter value=.75></meter></p>"
      end) <>
        "<label for=p>Final label</label><progress id=p><img src='missing.png' alt='Hidden fallback'><span>Hidden prose</span></progress>"

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 forms: mode,
                 page_size: {300, 100},
                 margin: 5,
                 asset_resolver: fn _ -> flunk("fallback resources must not be requested") end
               )

      assert {:ok, pages} = NativeElixirPdfUtilities.Info.page_count(pdf)
      assert pages >= 3
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
      assert {:ok, text} = Text.extract(pdf)
      for index <- 1..8, do: assert(text =~ "Gauge #{index}")
      assert text =~ "Final label"
      refute text =~ "Hidden prose"
      refute text =~ "Hidden fallback"
    end
  end

  test "hidden and inert gauge descendants cannot bypass executable content policy" do
    for tag <- ~w(progress meter),
        content <- [
          "<script>run()</script>",
          "<span onclick='run()'>Hidden</span>",
          "<a href='javascript:run()'>Hidden</a>"
        ] do
      for html <- [
            "<#{tag} hidden>#{content}</#{tag}>",
            "<template><#{tag}>#{content}</#{tag}></template>"
          ] do
        assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
        assert diagnostic.reason == :unsupported_html and diagnostic.stage == :html
        assert is_binary(diagnostic.source) and diagnostic.line >= 1
      end
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<progress><script type=application/json>{\"value\":1}</script><template><span>Unused</span></template></progress><meter hidden></meter>"
             )

    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
  end

  test "legacy raw text renders literal content without reading resources or creating fields" do
    html =
      File.read!(Paths.fixture("html_to_pdf/browser_parity/html_legacy_raw_text.html"))

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 forms: mode,
                 system_font_discovery: false,
                 asset_resolver: fn request ->
                   flunk("raw content fetched #{inspect(request)}")
                 end
               )

      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)

      assert_pdf_text(pdf, [
        "Before raw text",
        "Café &amp; &#13;",
        "<img src=denied.png>",
        "<script>literal()</script>",
        "</xmp-suffix> boundary",
        "After hidden fallback",
        "Before plaintext",
        "Plaintext é &amp;",
        "<input name=literal value=Literal>",
        "</plaintext>",
        "<div>Still literal</div>"
      ])

      assert {:ok, text} = Text.extract(pdf)
      refute text =~ "Unused raw fallback"
      refute text =~ "Shown &lt; <b>raw</b>"
      refute text =~ "Inert raw text"
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<div><p>Before inert plaintext</p><template><plaintext>Unused <img src=denied.png></template></div>",
               asset_resolver: fn request ->
                 flunk("inert plaintext fetched #{inspect(request)}")
               end
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before inert plaintext"
    refute text =~ "Unused"

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<p>Before override</p><noembed style='display:block;background-image:url(missing.png)'>Unused override</noembed><p>After override</p>",
               system_font_discovery: false,
               asset_resolver: fn request ->
                 flunk("noembed override fetched #{inspect(request)}")
               end
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before override" and text =~ "After override"
    refute text =~ "Unused override"
  end

  test "legacy raw text never weakens executable node policy or original-source diagnostics" do
    for tag <- ~w(xmp noembed plaintext) do
      for attribute <- ["onclick=bad", "style='background-image:url(javascript:bad)'"] do
        assert {:error, {:unsupported_html, diagnostic}} =
                 HtmlToPdf.render("<#{tag} #{attribute}>Literal")

        assert diagnostic.stage in [:html, :css] and diagnostic.reason == :unsupported_html
        assert diagnostic.operation == :render and is_binary(diagnostic.message)
      end
    end

    for tag <- ~w(xmp noembed), newline <- ["\r", "\r\n", "\n"] do
      literal = "<#{tag}><script>Literal</script>#{newline}</#{tag}>#{newline}  "
      token = "<script title='A#{newline}B'>"

      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(literal <> token <> "run()</script>")

      assert diagnostic.stage == :html and diagnostic.operation == :render
      assert diagnostic.line == 3 and diagnostic.column == 3 and diagnostic.source == token

      for after_raw <- ["<img onerror=bad>", "<a href=javascript:bad>Active</a>"] do
        assert {:error, {:unsupported_html, detail}} = HtmlToPdf.render(literal <> after_raw)
        assert detail.stage == :html and detail.operation == :render
        assert detail.line == 3 and detail.column == 3
      end
    end

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render("<svg><noembed><script>Active</script></noembed></svg>")

    assert diagnostic.stage == :html and diagnostic.operation == :render

    for tag <- ~w(xmp noembed plaintext) do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render("<svg><#{tag}><path/></#{tag}></svg>")

      assert diagnostic.stage == :layout and diagnostic.operation == :prepare
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgSceneValidator
      assert diagnostic.source == tag
      assert diagnostic.message =~ "supported source policy"
    end
  end

  test "oversized legacy literal input returns public shared diagnostics" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    for tag <- ~w(xmp noembed plaintext) do
      source = "<#{tag}>é\r\n" <> String.duplicate("<img src=denied.png>", 20)
      Limits.install(%{limits | max_html_source_bytes: byte_size(source) - 1})
      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(source)
      assert diagnostic.stage == :html and diagnostic.operation == :render
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.message =~ "max_html_source_bytes"
    end
  end

  test "editing selectors preserve public PDF artwork pages and field mutability" do
    html = """
    <div contenteditable><p data-editable>Host prose</p><div contenteditable=false><p>Locked prose</p>
    <p contenteditable=plaintext-only data-editable>Nested host</p></div><p contenteditable=' true ' data-editable>Inherited invalid</p>
    <input name=editable contenteditable=false data-editable value=Writable>
    <input name=readonly readonly contenteditable=true value=Readonly>
    <textarea name=disabled disabled contenteditable=true>Disabled</textarea></div><p>Outside prose</p>
    """

    css =
      "p:read-write,input:read-write,textarea:read-write{color:blue}p:read-only,input:read-only,textarea:read-only{color:red}"

    reference = "p,input,textarea{color:red}[data-editable]{color:blue}"

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode, stylesheets: [{:css, css}])
      assert {:ok, ^pdf} = HtmlToPdf.render(html, forms: mode, stylesheets: [{:css, reference}])
      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
      assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(pdf)
      assert {:ok, text} = Text.extract(flattened)

      for caption <- [
            "Host prose",
            "Locked prose",
            "Nested host",
            "Inherited invalid",
            "Writable",
            "Readonly",
            "Disabled",
            "Outside prose"
          ],
          do: assert(text =~ caption)

      assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(pdf)

      if mode == :static do
        assert fields == []
      else
        assert Enum.map(fields, &{&1.name, &1.value}) == [
                 {"editable", "Writable"},
                 {"readonly", "Readonly"},
                 {"disabled", "Disabled"}
               ]

        assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

        assert {:ok, validated} =
                 NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

        assert Enum.map(validated.fields, & &1.flags) == [0, 1, 4097]
      end
    end
  end

  test "mixed anchor and button contents preserve visible text, pages and field state" do
    html =
      File.read!(Paths.fixture("html_to_pdf/browser_parity/html_mixed_link_button_content.html"))

    opts = [
      system_font_discovery: false,
      compress_streams: false,
      asset_resolver: fn request -> flunk("inert content fetched #{inspect(request)}") end
    ]

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, Keyword.put(opts, :forms, mode))
      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
      assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(pdf)

      assert_pdf_text(flattened, [
        "Anchor block",
        "Nested prose",
        "List first",
        "List second",
        "Code",
        "Marked",
        "Added",
        "Removed",
        "Placeholder",
        "Line first",
        "Line second",
        "Attribute",
        "Initial",
        "After"
      ])

      assert {:ok, text} = Text.extract(flattened)
      refute text =~ "Inert caption"
      assert pdf =~ "https://example.com/mixed"
      assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(pdf)

      assert Enum.map(fields, &{&1.name, &1.value}) ==
               if(mode == :interactive, do: [{"active", "Initial"}], else: [])
    end
  end

  test "transparent block anchors retain prose and its PDF link" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<div><a href='https://example.com'><p>Retained prose</p></a></div>"
             )

    assert_pdf_text(pdf, "Retained prose")
    assert pdf =~ "/Subtype /Link" and pdf =~ "https://example.com"
  end

  test "public newline diagnostics preserve original caller tokens and locations" do
    for newline <- ["\r", "\r\n", "\n"] do
      token = "<span title='A#{newline}B' onclick=bad>"

      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render("<div>é#{newline}  " <> token <> "Text</span></div>")

      assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
      assert diagnostic.operation == :render
      assert diagnostic.line == 2 and diagnostic.column == 3 and diagnostic.source == token
      assert diagnostic.message =~ "attributes"

      assert {:error, {:unsupported_html, active}} =
               HtmlToPdf.render("<div>#{newline}  <a href='javascript:bad'>Bad</a></div>")

      assert active.stage == :html and active.operation == :render
      assert active.line == 2 and active.column == 3 and active.source == "href"
    end
  end

  test "microdata and translation metadata produce identical complete PDFs without resource requests" do
    owner = self()

    resolver = fn request ->
      send(owner, {:metadata_asset, request})
      :not_found
    end

    html = """
    <div itemscope=false itemtype='https://schema.org/Person' itemid=' ./people/42 ' itemref=extra translate=No>
    <h1 itemprop=name>Metadata record</h1><p itemprop='description https://example.org/label'>Visible prose</p>
    <input itemprop=identifier name=identifier value=Initial>
    <a itemprop=url href='https://example.org/record'>Existing destination</a>
    <x-box itemscope itemtype='urn:example:Record' arbitrary=unchanged translate=yes>Custom content</x-box>
    <span translate=' yes '>Inherited prose</span></div><p id=extra itemprop=note>Referenced prose</p>
    <script type=application/json itemscope itemtype='urn:example:Data' translate=no>{}</script>
    <template itemscope itemtype='urn:example:Inert' translate=no><svg itemtype=invalid translate=yes></svg><input name=unused value=Unused></template>
    """

    reference =
      String.replace(
        html,
        ~r/ (?:itemscope|itemtype|itemid|itemprop|itemref|translate)(?:=(?:'[^']*'|[^\s>]+))?/,
        ""
      )

    for forms <- [:static, :interactive] do
      opts = [forms: forms, asset_resolver: resolver, page_size: {280, 300}, margin: 12]
      assert {:ok, pdf} = HtmlToPdf.render(html, opts)
      assert {:ok, ^pdf} = HtmlToPdf.render(reference, opts)
      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
      assert {:ok, flat} = NativeElixirPdfUtilities.Forms.flatten(pdf)
      assert {:ok, text} = Text.extract(flat, layout: false)

      for caption <- [
            "Metadata record",
            "Visible prose",
            "Initial",
            "Existing destination",
            "Custom content",
            "Inherited prose",
            "Referenced prose"
          ],
          do: assert(text =~ caption)

      refute text =~ "Unused" or text =~ "schema.org" or text =~ "people/42"
      assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(pdf)

      assert Enum.map(fields, & &1.name) ==
               if(forms == :interactive, do: ["identifier"], else: [])

      refute_received {:metadata_asset, _}
    end
  end

  test "metadata conformance failures use public diagnostics before asset resolution" do
    owner = self()

    resolver = fn request ->
      send(owner, {:invalid_metadata_asset, request})
      :not_found
    end

    for {html, message, located?} <- [
          {"<div itemtype=relative><img src=denied></div>", "itemscope", true},
          {"<div itemscope itemtype=relative><img src=denied></div>", "absolute URL", true},
          {"<div itemid='#id'><img src=denied></div>", "itemid requires", true},
          {"<div itemprop='bad.name'><img src=denied></div>", "full stop", true},
          {"<div itemscope itemref='missing'><img src=denied></div>", "same authored tree",
           false},
          {"<div itemscope itemref='in-template'></div><template><p id=in-template>Inert</p></template>",
           "same authored tree", false},
          {"<p id=outside>Outside</p><template><div itemscope itemref=outside></div></template>",
           "same authored tree", false}
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render(html, asset_resolver: resolver)

      assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlMetadataValidator
      assert diagnostic.message =~ message
      assert diagnostic.operation in [:render, :compute]
      if located?, do: assert(diagnostic.line == 1 and is_integer(diagnostic.column))
      assert is_binary(diagnostic.source)
      refute_received {:invalid_metadata_asset, _}
    end

    assert {:ok, _} =
             HtmlToPdf.render(
               "<div itemscope itemref='later self' id=self>Item</div><div><p id=later>Later</p></div><template><div itemscope itemref=local></div><p id=local>Inert reference</p></template>"
             )
  end

  test "itemref lookup consumes the existing shared work budget before asset resolution" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_css_work: 1})
    owner = self()

    resolver = fn request ->
      send(owner, {:metadata_budget_asset, request})
      :not_found
    end

    html = "<div itemscope itemref=last>Item</div><p id=last>Target</p><img src=denied>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(html, asset_resolver: resolver)

    assert diagnostic.stage == :html and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.operation == :render and diagnostic.message =~ "max_css_work"
    refute_received {:metadata_budget_asset, _}
  end

  test "default inline pictures compose selected density and dimension hints with links wrapping and pagination" do
    bytes =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(80, 40, 2, 8, 0, fn _, _ ->
        [10, 80, 200]
      end)

    prefix =
      "<style>@page{size:160pt 120pt;margin:10pt}body{margin:0;font-size:10pt;line-height:13pt}p{margin:0}a{color:black;text-decoration:none}</style>"

    for {source, image, expected} <- [
          {"srcset='chosen 2x'", "", "src=chosen style='width:30pt;height:15pt'"},
          {"srcset='denied 20w, chosen 80w' sizes=40px", "",
           "src=chosen style='width:30pt;height:15pt'"},
          {"srcset='chosen 2x' width=80 height=40", "width=200 height=200",
           "src=chosen width=80 height=40"},
          {"srcset='chosen 80w' sizes=0", "", "src=chosen style='width:0;height:0'"},
          {"srcset='chosen 80w' sizes=0",
           "style='width:30pt;height:30pt;object-fit:contain;border:1pt solid blue'",
           "src=chosen style='width:30pt;height:30pt;object-fit:contain;border:1pt solid blue'"}
        ] do
      picture =
        "<picture><source media=screen srcset=denied><source type=image/webp srcset=denied><source media=print type=image/png #{source}><img src=denied #{image}></picture>"

      body =
        "<div style='height:75pt'>Prior page.</div><p style='width:65pt'><span><a href='https://example.com/inline'>Before#{picture}after words.</a></span></p><p>End.</p>"

      opts = [assets: %{"chosen" => {:bytes, bytes}}, compress_streams: false]
      assert {:ok, actual} = HtmlToPdf.render(prefix <> body, opts)

      assert {:ok, reference} =
               HtmlToPdf.render(
                 prefix <> String.replace(body, picture, "<img #{expected}>"),
                 opts
               )

      assert actual == reference
      assert {:ok, pages} = NativeElixirPdfUtilities.Info.page_count(actual)
      assert pages >= 2
      assert {:ok, text} = Text.extract(actual)

      for content <- ["Prior page.", "Before", "after", "words.", "End."],
          do: assert(text =~ content)

      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(actual)
    end
  end

  test "block pictures and responsive images load only chosen resources and preserve content and fields" do
    bytes =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(80, 40, 2, 8, 0, fn _, _ ->
        [10, 80, 200]
      end)

    owner = self()

    resolver = fn %{reference: reference} ->
      send(owner, {:responsive_load, reference})
      if reference == "chosen", do: {:ok, bytes}, else: :not_found
    end

    html =
      "<p>Before selection</p><picture style='display:block'>" <>
        "<source media=screen srcset=screen><source type=image/webp srcset=unsupported>" <>
        "<source type=image/png media=print srcset='unchosen 1x, chosen 2x' width=80 height=40>" <>
        "<img src=fallback width=200 height=200></picture><p>After selection</p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, asset_resolver: resolver, image_selection_density: 2)

    assert_received {:responsive_load, "chosen"}
    refute_received {:responsive_load, _}
    assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Before selection" and text =~ "After selection"
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)

    assert {:ok, reference} =
             HtmlToPdf.render(
               "<p>Before selection</p><img src=chosen width=80 height=40><p>After selection</p>",
               assets: %{"chosen" => {:bytes, bytes}}
             )

    assert pdf == reference

    for {attrs, css} <- [
          {"srcset='chosen 2x'", "width:30pt;height:15pt"},
          {"src=fallback srcset='unchosen 20w, chosen 80w' sizes=40px", "width:30pt;height:15pt"},
          {"srcset='chosen 80w' sizes=0", "width:0;height:0"}
        ] do
      assert {:ok, actual} =
               HtmlToPdf.render("<img #{attrs}><p>After</p>", asset_resolver: resolver)

      assert {:ok, expected} =
               HtmlToPdf.render("<img src=chosen style='#{css}'><p>After</p>",
                 assets: %{"chosen" => {:bytes, bytes}}
               )

      assert actual == expected
      assert_received {:responsive_load, "chosen"}
      refute_received {:responsive_load, _}
    end
  end

  test "zero responsive source size retains aspect ratio inside an explicit fitted image box" do
    bytes =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(80, 40, 2, 8, 0, fn _, _ ->
        [10, 80, 200]
      end)

    for fit <- ["fill", "contain", "cover"],
        dimension <- ["width:60pt;height:60pt", "height:60pt"] do
      image_style = "#{dimension};object-fit:#{fit}"

      assert {:ok, actual} =
               HtmlToPdf.render(
                 "<img srcset='tile 80w' sizes=0 style='#{image_style}'><p>After</p>",
                 assets: %{"tile" => {:bytes, bytes}}
               )

      assert {:ok, expected} =
               HtmlToPdf.render("<img src=tile style='#{image_style}'><p>After</p>",
                 assets: %{"tile" => {:bytes, bytes}}
               )

      assert actual == expected
    end
  end

  test "responsive chosen-resource failures are strict and carry the selected reference" do
    for {source, response, reason} <- [
          {"missing", :not_found, :invalid_document},
          {"denied", {:error, :denied}, :invalid_document},
          {"invalid", {:ok, "not an image"}, :invalid_document}
        ] do
      owner = self()

      resolver = fn %{reference: reference} ->
        send(owner, {:chosen_failure, reference})
        response
      end

      html =
        "<picture style='display:block'><source media=print srcset='#{source} 2x'><img src=fallback></picture>"

      assert {:error, {^reason, diagnostic}} = HtmlToPdf.render(html, asset_resolver: resolver)
      assert diagnostic.reason == reason and diagnostic.source == source
      assert diagnostic.stage in [:asset, :style]
      assert diagnostic.operation in [:render, :compute] and diagnostic.message != ""
      assert_received {:chosen_failure, ^source}
      refute_received {:chosen_failure, "fallback"}
    end

    svg = "<svg xmlns='http://www.w3.org/2000/svg'><path d='invalid'/></svg>"

    assert {:error, {reason, diagnostic}} =
             HtmlToPdf.render("<img srcset='chosen 2x'>", assets: %{"chosen" => {:bytes, svg}})

    assert reason in [:invalid_document, :unsupported_html]
    assert diagnostic.source != nil and diagnostic.message != ""

    for opts <- [[print_viewport: {0, 600}], [image_selection_density: 0]] do
      assert {:error, {:invalid_options, diagnostic}} =
               HtmlToPdf.render("<img srcset='denied 2x'>", opts)

      assert diagnostic.stage == :options and diagnostic.operation == :render
    end

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<img srcset='zero 0x'>")

    assert diagnostic.source == "zero" and diagnostic.message =~ "infinite natural"
  end

  test "document bases resolve relative links without fetching and preserve source selectors" do
    html = """
    <head><title>Authored</title><base target=_blank><base href='https://example.test/docs/'>
    <base href='https://ignored.test/'><style>a[href='chapter.html']{color:red}</style></head>
    <body><a href='chapter.html' rel='next' target=_self hreflang=en download=chapter>Chapter</a>
    <a href='#target'>Remote fragment</a><a href='//cdn.test/doc'>CDN</a>
    <a href='mailto:test@example.test'>Mail</a><input name=initial value=Initial></body>
    """

    opts = [
      compress_streams: false,
      system_font_discovery: false,
      base_url: "https://fallback.test/original.html",
      metadata: [title: "Caller"],
      asset_resolver: fn request -> flunk("link fetched #{inspect(request)}") end
    ]

    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert pdf =~ "https://example.test/docs/chapter.html"
    assert pdf =~ "https://example.test/docs/#target"
    assert pdf =~ "https://cdn.test/doc"
    assert pdf =~ "mailto:test@example.test"
    refute pdf =~ "ignored.test"
    assert_pdf_text(pdf, ["Chapter", "Remote fragment", "CDN", "Mail"])

    assert {:ok, [%{name: "initial", value: "Initial"}]} =
             NativeElixirPdfUtilities.Forms.fields(pdf)

    assert {:ok, info} = NativeElixirPdfUtilities.Info.get(pdf)
    assert info.title == "Caller"

    assert {:ok, dom} =
             NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_document_detailed(html)

    assert {:ok, styled} = NativeElixirPdfUtilities.HtmlToPdf.Style.compute_detailed(dom, opts)
    [body] = styled.children
    assert hd(body.children).style.color == {1, 0, 0}
    assert hd(body.children).style.link_url == "https://example.test/docs/chapter.html"
  end

  test "base resolution changes resource selection but never the authorization root" do
    directory = Path.join(System.tmp_dir!(), "native-elixir-pdf-base-links")
    root = Path.join(directory, "root")
    nested = Path.join(root, "nested")
    File.mkdir_p!(nested)
    File.write!(Path.join(nested, "image.png"), png_fixture(1, 1))
    File.write!(Path.join(directory, "outside.png"), png_fixture(1, 1))
    File.cp!(ttf_font_path!(), Path.join(nested, "font.ttf"))
    :ok = File.ln_s(Path.join(directory, "outside.png"), Path.join(nested, "linked.png"))
    on_exit(fn -> File.rm_rf!(directory) end)

    html = """
    <head><base href='nested/'><style>@font-face{font-family:Based;src:url(font.ttf)}
    p{font-family:Based}</style></head><body><p>Based</p><img src='image.png'>
    <a href='chapter.html'>Local chapter</a></body>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: root, compress_streams: false)
    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "file://#{nested}/chapter.html"
    assert_pdf_text(pdf, ["Based", "Local chapter"])

    for {base, src} <- [
          {"../", "outside.png"},
          {"file://#{directory}/", "outside.png"},
          {"nested/", "linked.png"},
          {"nested/", "../../outside.png"}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               HtmlToPdf.render(
                 "<head><base href='#{base}'></head><body><img src='#{src}'></body>",
                 base_url: root
               )

      assert diagnostic.stage == :asset and diagnostic.operation == :render
      assert diagnostic.message =~ "authorized"
    end

    remote = "<head><base href='https://assets.test/v1/'></head><body><img src=image.png></body>"

    assert {:error,
            {:invalid_document, %{stage: :asset, source: "https://assets.test/v1/image.png"}}} =
             HtmlToPdf.render(remote, base_url: root)

    for key <- ["image.png", "https://assets.test/v1/image.png"] do
      assert {:ok, approved} =
               HtmlToPdf.render(remote,
                 base_url: root,
                 assets: %{key => {:bytes, png_fixture(1, 1)}},
                 asset_resolver: fn _ -> flunk("explicit asset did not take precedence") end
               )

      assert approved =~ "/Subtype /Image"
    end

    assert {:ok, _pdf} =
             HtmlToPdf.render(remote,
               asset_resolver: fn request ->
                 assert request == %{reference: "https://assets.test/v1/image.png", kind: :image}
                 {:ok, png_fixture(1, 1)}
               end
             )
  end

  test "wrapped relative links retain every line and annotation on multiple pages" do
    text = Enum.map_join(1..35, " ", &"Word#{&1}")

    html =
      "<head><base href='https://example.test/docs/'></head><body style='font-size:10pt;line-height:14pt;margin:0'><a href=chapter.html>#{text}</a><input name=initial value=Initial></body>"

    opts = [page_size: {150, 100}, margin: 8, compress_streams: false]
    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert {:ok, count} = NativeElixirPdfUtilities.Info.page_count(pdf)
    assert count > 1
    assert {:ok, extracted} = Text.extract(pdf, layout: false)

    assert Enum.map(Regex.scan(~r/Word(\d+)/, extracted), fn [_, index] ->
             String.to_integer(index)
           end) == Enum.to_list(1..35)

    assert length(Regex.scan(~r{/Subtype /Link}, pdf)) > count

    assert {:ok, [%{name: "initial", value: "Initial"}]} =
             NativeElixirPdfUtilities.Forms.fields(pdf)
  end

  test "MathML tokens retain selected glyphs and default rows follow Chrome text admission" do
    html = """
    <p>Before <math><mrow><mi> x </mi><mn>12</mn><ms lquote='[' rquote=']' mathbackground='yellow'>literal</ms>
    <mspace width='1em' height='2pt' depth='1pt'/><mtext> a <span style='color:red'>b</span> c&nbsp;d </mtext>
    <mi></mi><mn/><mtext/><ms/><mrow/><mi style='display:none;padding:10pt'>Hidden</mi></mrow>tail</math> After</p>
    <math display=block><mi mathvariant=normal>z</mi><mi>exp</mi><mn>42</mn></math>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
    assert {:ok, text} = Text.extract(pdf, layout: false)

    for expected <- ["Before", "x12literal", "a b c", "After", "zexp42"],
        do: assert(String.contains?(text, expected))

    refute text =~ "tail"
    refute text =~ "[literal]"
    refute text =~ "𝑥"
    refute text =~ "Hidden"
  end

  test "native MathML selection and annotations preserve visible source content under Chrome admission" do
    for tag <- ~w(semantics maction unknown-math) do
      assert {:ok, pdf} = HtmlToPdf.render("<math><#{tag}><mn>12</mn></#{tag}></math>")
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
    end

    for tag <- ~w(annotation annotation-xml) do
      assert {:ok, pdf} = HtmlToPdf.render("<math><#{tag}>α</#{tag}></math>")
      assert {:error, {:no_extractable_text, diagnostic}} = Text.extract(pdf, layout: false)
      assert diagnostic.stage == :text_extraction
      assert diagnostic.reason == :no_extractable_text
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><semantics><mn>12</mn><annotation>Hidden</annotation><annotation-xml><unknown-math>Hidden</unknown-math></annotation-xml></semantics></math>"
             )

    assert {:ok, "12"} = Text.extract(pdf, layout: false)

    html =
      "<math><annotation-xml encoding='text/html'><a href='https://annotation.test/'>α</a></annotation-xml></math>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:error, {:no_extractable_text, _}} = Text.extract(pdf, layout: false)
    refute pdf =~ "https://annotation.test/"

    assert {:ok, pdf} =
             HtmlToPdf.render("<style>annotation-xml {display:inline-block}</style>" <> html)

    assert {:ok, "α"} = Text.extract(pdf, layout: false)
    assert pdf =~ "https://annotation.test/"
    assert {:ok, pdf} = HtmlToPdf.render("<math><mi>α</mi><mtext>β</mtext></math>")
    assert {:ok, "αβ"} = Text.extract(pdf, layout: false)
  end

  test "native six script schemata render base and script logical content" do
    for tag <- ~w(msub msup msubsup munder mover munderover) do
      children =
        "<mn>1</mn><mn>2</mn>" <> if(tag in ~w(msubsup munderover), do: "<mn>3</mn>", else: "")

      assert {:ok, pdf} = HtmlToPdf.render("<math><#{tag}>#{children}</#{tag}></math>")
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text == if(tag in ~w(msubsup munderover), do: "123", else: "12")
      assert {:ok, empty} = HtmlToPdf.render("<math><#{tag}/></math>")
      assert {:error, {:no_extractable_text, diagnostic}} = Text.extract(empty, layout: false)
      assert diagnostic.stage == :text_extraction
    end
  end

  test "MathML diagnoses detached table rows and unsupported token composition" do
    alias NativeElixirPdfUtilities.Validators.MathLayoutValidator

    for tag <-
          ~w(mtr mtd) do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render("<math><#{tag}/></math>")

      assert diagnostic.stage == :layout
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.operation == :measure_math
      assert diagnostic.module == MathLayoutValidator
      assert diagnostic.source == tag
      assert diagnostic.message =~ "containing mtable"
    end

    for content <- ["<br/>", "<span style='display:inline-block'>atomic</span>"] do
      assert {:ok, pdf} = HtmlToPdf.render("<math><mtext>before#{content}after</mtext></math>")
      assert {:ok, text} = Text.extract(pdf, layout: false)

      assert text ==
               "before" <>
                 if(String.contains?(content, "atomic"), do: "atomic", else: "") <> "after"
    end

    assert {:error, {_, %{message: message}}} =
             HtmlToPdf.render("<math><mtext><script/></mtext></math>")

    assert message =~ "script"

    for {text, style} <- [
          {"é", ""},
          {"אב", ""},
          {"x", "text-decoration:underline"},
          {"ab", "letter-spacing:1pt"}
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlToPdf.render("<math><mtext style='#{style}'>#{text}</mtext></math>")

      assert diagnostic.source == text
      assert diagnostic.message =~ "text/composition"
    end

    for style <- [
          "width:100pt",
          "padding:1pt",
          "position:relative",
          "overflow:hidden",
          "min-width:20pt",
          "box-sizing:border-box"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render("<math style='#{style}'><mn>12</mn></math>")
      assert {:ok, "12"} = Text.extract(pdf, layout: false)
      assert pdf =~ "/CIDFontType2" and pdf =~ "/FontFile2" and pdf =~ "STIXTwoMath"
      assert {:ok, %{pages: [%{spans: spans}]}} = Text.extract_spans(pdf)
      assert Enum.any?(spans, &(&1.paints_text? and &1.end_x > &1.x and &1.font_size > 0))
    end

    assert {:ok, percentage_token} =
             HtmlToPdf.render("<math><mn style='width:50%'>12</mn></math>")

    assert {:ok, "12"} = Text.extract(percentage_token, layout: false)
    assert percentage_token =~ "STIXTwoMath" and percentage_token =~ "/FontFile2"
    assert {:ok, %{pages: [%{spans: token_spans}]}} = Text.extract_spans(percentage_token)
    assert Enum.any?(token_spans, &(&1.paints_text? and &1.end_x > &1.x))

    assert {:ok, pdf} =
             HtmlToPdf.render("<math><mtext style='white-space:pre'> a  b </mtext></math>")

    assert {:ok, "a b"} = Text.extract(pdf, layout: false)

    assert {:ok, pdf} =
             HtmlToPdf.render("<math><mtext style='white-space:pre'>a\nb</mtext></math>")

    assert {:ok, "a b"} = Text.extract(pdf, layout: false)

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<math><mtext><span style='white-space:pre'>a\nb</span></mtext></math>"
             )

    assert diagnostic.source == "a\nb"

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<p style='font-family:STIX Two Math;text-transform:math-auto'>x</p>"
             )

    assert {:ok, "𝑥"} = Text.extract(pdf, layout: false)
  end

  test "original independently painted MathML child cases retain successful content and links" do
    for style <- [
          "background-color:red",
          "padding:2pt",
          "border:1pt solid red",
          "position:relative;left:2pt",
          "width:30pt"
        ] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<math><mtext><span style='#{style}'>Visible</span></mtext></math>"
               )

      assert {:ok, "Visible"} = Text.extract(pdf, layout: false)
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<math><mtext><a href='https://example.test/' style='text-decoration:none'>Link</a></mtext></math>"
             )

    assert {:ok, "Link"} = Text.extract(pdf, layout: false)
    assert pdf =~ "https://example.test/"
  end

  test "math font overrides must supply mathematical glyphs and usable metrics" do
    stix = File.read!(Paths.project("priv/fonts/stix/STIXTwoMath-Regular.ttf"))

    assert {:ok, pdf} =
             HtmlToPdf.render("<math style='font-family:Caller Math'><mi>x</mi></math>",
               fonts: [%{family: "Caller Math", data: stix}]
             )

    assert {:ok, "x"} = Text.extract(pdf, layout: false)

    assert {:error, {:unsupported_glyph, diagnostic}} =
             HtmlToPdf.render("<math><mtext>😀</mtext></math>")

    assert diagnostic.source == "😀"
    assert diagnostic.message =~ "selected math font"

    assert {:error, {_, diagnostic}} =
             HtmlToPdf.render("<math style='font-family:DejaVu Sans Mono'><mi>x</mi></math>")

    assert diagnostic.message =~ "MATH"
  end

  test "oversized atomic equations diagnose and repeated equations retain text across pages" do
    for source <- [
          "<math style='font-size:300pt'><mi>x</mi></math>",
          "<math><mtext>#{String.duplicate("Wide", 40)}</mtext></math>"
        ] do
      assert {:error, {:invalid_layout, diagnostic}} =
               HtmlToPdf.render(source, page_size: {120, 100}, margin: 10)

      assert diagnostic.stage == :layout
      assert diagnostic.source == "math"
      assert diagnostic.message =~ "increase the page size"
    end

    html =
      "<style>p{margin:0;line-height:20pt}</style>" <>
        Enum.map_join(1..12, fn index ->
          "<p>Line #{index} <math><mi>x</mi><mn>#{index}</mn></math></p>"
        end)

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {180, 100}, margin: 10)
    assert {:ok, 3} = NativeElixirPdfUtilities.Info.page_count(pdf)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    for index <- 1..12, do: assert(text =~ "x#{index}")
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert {:ok, _} = HtmlToPdf.render("<math><mi/></math><p>After empty math</p>")
  end

  test "open disclosures paginate every body paragraph while closed bodies consume no pages" do
    body = Enum.map_join(1..18, fn index -> "<p>Disclosure line #{index}</p>" end)

    style =
      "<style>html,body{margin:0;padding:0;font-size:10pt;line-height:13pt}p{margin:0}</style>"

    for {open, count} <- [{"", 1}, {"open", 3}] do
      html = style <> "<details #{open}><summary>Legend</summary>#{body}</details><p>After</p>"
      assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {150, 110}, margin: 5)
      assert {:ok, ^count} = NativeElixirPdfUtilities.Info.page_count(pdf)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Legend" and text =~ "After"

      for index <- 1..18,
          do: assert(String.contains?(text, "Disclosure line #{index}") == (open == "open"))
    end
  end

  test "disclosures preserve visible content and omit closed assets and fields" do
    html = """
    <details><summary>Closed legend</summary><p>Hidden prose</p>
    <img src=missing.png><input name=closed value=Hidden></details>
    <details open=false><summary>Open legend</summary><p>Visible prose</p>
    <input name=visible value=Present></details><details></details>
    """

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: mode)
      assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(pdf)
      assert {:ok, text} = Text.extract(flattened)

      for caption <- ["Closed legend", "Open legend", "Visible prose", "Present", "Details"],
          do: assert(text =~ caption)

      refute text =~ "Hidden"
      assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(pdf)
      assert Enum.map(fields, & &1.name) == if(mode == :interactive, do: ["visible"], else: [])
    end
  end

  test "named disclosure groups validate exclusivity and isolate template fragments" do
    valid = """
    <details name=A open><summary>First</summary>Shown</details>
    <div><details name=A><summary>Second</summary>Hidden</details></div>
    <details name=a open><summary>Case-sensitive</summary>Independent</details>
    <template><details name=A open>Inert first</details></template>
    <template><details name=A open>Inert second</details></template>
    """

    assert {:ok, _} = HtmlToPdf.render(valid)

    assert {:ok, _} =
             HtmlToPdf.render(
               "<details name=outer><details name=inner open>Nested</details></details>"
             )

    for {html, message} <- [
          {"<details name=''></details>", "nonempty"},
          {"<details name=a open></details><div><details name=a open></details></div>",
           "only one"},
          {"<details name=a><details name=a></details></details>", "cannot nest"},
          {"<template><details name=a open></details><details name=a open></details></template>",
           "only one"}
        ] do
      assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :html
      assert diagnostic.reason == :invalid_document
      assert diagnostic.operation == :compute
      assert diagnostic.source =~ "details name="
      assert diagnostic.message =~ message
    end
  end

  test "closed disclosures still diagnose executable and unsupported input" do
    for html <- [
          "<details><summary>Legend</summary><script>alert(1)</script></details>",
          "<details><summary onclick='alert(1)'>Legend</summary></details>",
          "<details><summary>Legend</summary><a href='javascript:alert(1)'>Link</a></details>",
          "<div><summary>Orphan</summary></div>",
          "<details><summary><div>Invalid summary block</div></summary></details>"
        ] do
      assert {:error, {reason, diagnostic}} = HtmlToPdf.render(html)
      assert reason in [:unsupported_html, :invalid_document]
      assert diagnostic.reason == reason
      assert diagnostic.stage in [:html, :style]
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end
  end

  test "closed file input preserves empty initial state without painting a selector or fetching value" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<details><summary>Legend</summary><input type=file name=upload value=/etc/passwd></details>",
               asset_resolver: fn request -> flunk("file scalar fetched #{inspect(request)}") end
             )

    assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
    assert text =~ "Legend"
    refute text =~ "passwd"
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
  end

  test "required controls retain empty initial values and PDF requirement flags" do
    for {html, flags, value} <- [
          {~s(<input name=field required value="">), 2, ""},
          {~s(<input name=field required=false value=Initial>), 2, "Initial"},
          {~s(<input name=field type=password required value=Secret>), 8194, "Secret"},
          {~s(<input name=field required readonly>), 1, ""},
          {~s(<input name=field required disabled>), 1, ""},
          {~s(<textarea name=field required></textarea>), 4098, ""},
          {~s(<textarea name=field required readonly></textarea>), 4097, ""},
          {~s(<select name=field required><option value="">Choose</option></select>), 131_074,
           ""},
          {~s(<select name=field required disabled></select>), 131_073, nil},
          {~s(<input name=field type=checkbox required>), 2, false},
          {~s(<input name=field type=radio required>), 32770, "Off"},
          {~s(<input name=field type=unknown required>), 2, ""}
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: :interactive)
      assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

      assert {:ok, %{fields: [field]}} =
               NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

      assert field.name == "field"
      assert field.flags == flags
      assert field.value == value
      assert {:ok, _} = HtmlToPdf.render(html, forms: :static)
    end

    for type <- ["hidden", "button", "submit", "reset"] do
      assert {:ok, pdf} = HtmlToPdf.render("<input name=ignored type=#{type} required>")
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    end
  end

  test "required radio groups share PDF flags without changing per-radio CSS state" do
    html = """
    <form id=owner><input name=mode type=radio value=A required>
    <input name=between value=Text><input name=mode type=radio value=B checked></form>
    <input name=mode type=radio value=C form=owner>
    <input name=other type=radio value=Other checked>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    assert {:ok, %{fields: fields}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    assert {:ok, summaries} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert Enum.find(summaries, &(&1.name == "mode")).export_values == ["A", "B", "C"]
    fields = Map.new(fields, &{&1.name, &1})
    assert fields["mode"].flags == 32770
    assert fields["mode"].value == "B"
    assert fields["other"].flags == 32768
    assert fields["between"].flags == 0

    assert {:ok, pdf} = HtmlToPdf.render("<input type=radio required><input type=radio>")
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    assert {:ok, %{fields: fields}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    assert Enum.map(fields, & &1.flags) == [32770, 32768]

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<input name=locked type=radio required disabled value=A><input name=locked type=radio disabled value=B>"
             )

    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    assert {:ok, %{fields: [field]}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    assert field.flags == 32769
  end

  test "requirement selector PDFs match equivalent rules in both form modes" do
    controls = """
    <input name=a required value=Required><input name=b value=Optional>
    <input name=c readonly required value=Readonly><input name=d disabled required value=Disabled>
    <textarea name=e required>Notes</textarea><select name=f required><option value="">Choose</option></select>
    <input type=hidden required><input type=button required value=Button>
    <input name=mode type=radio required value=Yes><input name=mode type=radio value=No checked>
    """

    css = ":required{color:red}:optional{color:blue}"

    equivalent =
      "input,textarea,select{color:blue}input[required],textarea[required],select[required]{color:red}input[type=hidden],input[type=button]{color:black}"

    for forms <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render("<style>#{css}</style>" <> controls, forms: forms)

      assert {:ok, reference} =
               HtmlToPdf.render("<style>#{equivalent}</style>" <> controls, forms: forms)

      assert pdf == reference
    end

    for selector <- [":required()", ":optionalness", ":optıonal"] do
      html = "<style>input#{selector}{color:red}</style><input required>"
      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :css and diagnostic.operation == :render
      assert diagnostic.reason == :invalid_css and diagnostic.message != ""
    end

    for tag <- ["button", "option"] do
      html = "<#{tag} required>Invalid</#{tag}>"
      reference = "<#{tag}>Invalid</#{tag}>"
      html = if tag == "option", do: "<select>" <> html <> "</select>", else: html
      reference = if tag == "option", do: "<select>" <> reference <> "</select>", else: reference
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, reference} = HtmlToPdf.render(reference)
      assert pdf == reference
    end
  end

  test "horizontal rule limits and automatic-margin contexts return public results" do
    opts = [system_font_discovery: false]

    for html <- [
          "<div style='display:flex'><div style='width:60pt;margin:0 auto'>Text</div></div>",
          "<div style='display:grid'><div style='width:60pt;margin:0 auto'>Text</div></div>",
          "<div style='position:absolute;width:60pt;margin:0 auto'>Text</div>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html, opts)
      assert {:ok, "Text"} = Text.extract(pdf, layout: false)
    end

    assert {:ok, _pdf} = HtmlToPdf.render("<hr style='display:flex'>", opts)

    magnitude = trunc(Limits.get(:max_css_numeric_magnitude)) + 1

    for attribute <- ["width", "size"] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render("<hr #{attribute}=#{magnitude}>", opts)

      assert diagnostic.stage == :style and diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :render
      assert diagnostic.source == "#{attribute}=#{magnitude}"
      assert diagnostic.message =~ "max_css_numeric_magnitude"
    end

    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render("<hr style='padding:auto'>", opts)

    assert diagnostic.stage == :css and diagnostic.operation == :render
    assert diagnostic.message =~ "padding"
  end

  test "named color artwork equals numeric reference artwork across all palette entries" do
    path = Paths.fixture("html_to_pdf/browser_parity/css_named_colors.html")
    html = File.read!(path)
    # Independent numerical reference uses the published hex channels in source order.
    palette =
      ~w(#f0f8ff #faebd7 #00ffff #7fffd4 #f0ffff #f5f5dc #ffe4c4 #000000 #ffebcd #0000ff #8a2be2 #a52a2a #deb887 #5f9ea0 #7fff00 #d2691e #ff7f50 #6495ed #fff8dc #dc143c #00ffff #00008b #008b8b #b8860b #a9a9a9 #006400 #a9a9a9 #bdb76b #8b008b #556b2f #ff8c00 #9932cc #8b0000 #e9967a #8fbc8f #483d8b #2f4f4f #2f4f4f #00ced1 #9400d3 #ff1493 #00bfff #696969 #696969 #1e90ff #b22222 #fffaf0 #228b22 #ff00ff #dcdcdc #f8f8ff #ffd700 #daa520 #808080 #008000 #adff2f #808080 #f0fff0 #ff69b4 #cd5c5c #4b0082 #fffff0 #f0e68c #e6e6fa #fff0f5 #7cfc00 #fffacd #add8e6 #f08080 #e0ffff #fafad2 #d3d3d3 #90ee90 #d3d3d3 #ffb6c1 #ffa07a #20b2aa #87cefa #778899 #778899 #b0c4de #ffffe0 #00ff00 #32cd32 #faf0e6 #ff00ff #800000 #66cdaa #0000cd #ba55d3 #9370db #3cb371 #7b68ee #00fa9a #48d1cc #c71585 #191970 #f5fffa #ffe4e1 #ffe4b5 #ffdead #000080 #fdf5e6 #808000 #6b8e23 #ffa500 #ff4500 #da70d6 #eee8aa #98fb98 #afeeee #db7093 #ffefd5 #ffdab9 #cd853f #ffc0cb #dda0dd #b0e0e6 #800080 #663399 #ff0000 #bc8f8f #4169e1 #8b4513 #fa8072 #f4a460 #2e8b57 #fff5ee #a0522d #c0c0c0 #87ceeb #6a5acd #708090 #708090 #fffafa #00ff7f #4682b4 #d2b48c #008080 #d8bfd8 #ff6347 #40e0d0 #ee82ee #f5deb3 #ffffff #f5f5f5 #ffff00 #9acd32)

    names =
      Regex.scan(~r/background-color:([a-z]+)/, html, capture: :all_but_first) |> List.flatten()

    assert length(names) == 148 and length(palette) == 148

    reference =
      Enum.zip(names, palette)
      |> Enum.reduce(html, fn {name, hex}, source ->
        String.replace(source, ":" <> name <> ";", ":" <> hex <> ";")
      end)

    opts = [system_font_discovery: false, compress_streams: false, forms: :static]
    assert {:ok, expected} = HtmlToPdf.render(reference, opts)
    assert {:ok, ^expected} = HtmlToPdf.render(html, opts)
    assert {:ok, text} = Text.extract(expected)
    for name <- names, do: assert(text =~ name)

    for name <- ["Khaki", "rebecca-purple", "\u00a0khaki", "khaki\u00a0", "\u2003red\u2003"],
        property <- [
          "color",
          "background",
          "border",
          "border-color",
          "border-top-color",
          "text-decoration-color",
          "text-decoration"
        ] do
      assert {:error, {:invalid_css, diagnostic}} =
               HtmlToPdf.render("<p style='#{property}:#{name}'>Text</p>", opts)

      assert diagnostic.stage == :css and diagnostic.operation == :render
      assert diagnostic.source =~ property <> ":" <> name
      assert diagnostic.message =~ name
    end
  end

  test "closed popovers omit paint and fields while retaining DOM radio selection" do
    hidden =
      "<div popover><p>Unused text</p><img src=missing.png>" <>
        "<input name=closed value=Unused></div><img popover=hint src=missing.png>" <>
        "<x-box popover=invalid>Unused custom</x-box><wbr popover>"

    opts = [
      system_font_discovery: false,
      compress_streams: false,
      asset_resolver: fn request -> flunk("closed content resolved #{inspect(request)}") end
    ]

    assert {:ok, metadata} =
             HtmlToPdf.render(
               "<x-box href='javascript:metadata' src='vbscript:metadata'>Metadata</x-box>",
               opts
             )

    assert {:ok, ^metadata} = HtmlToPdf.render("<x-box>Metadata</x-box>", opts)

    for forms <- [:static, :interactive] do
      options = Keyword.put(opts, :forms, forms)
      assert {:ok, expected} = HtmlToPdf.render("<p>Before</p><p>After</p>", options)

      assert {:ok, ^expected} =
               HtmlToPdf.render("<p>Before</p>" <> hidden <> "<p>After</p>", options)

      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(expected)
      assert {:ok, blank} = HtmlToPdf.render(hidden, options)
      assert {:ok, ^blank} = HtmlToPdf.render("", options)
      assert {:ok, reference} = HtmlToPdf.render("<input type=radio name=mode>", options)

      assert {:ok, ^reference} =
               HtmlToPdf.render(
                 "<input type=radio name=mode checked><div popover><input type=radio name=mode checked></div>",
                 options
               )
    end

    for html <- [
          "<p popover style='display:block'>Exposed</p>",
          "<style>[popover]{display:block!important}</style><p popover>Exposed</p>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html, opts)
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
      assert text =~ "Exposed"
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    end

    for html <- [
          "<div popover><a href='javascript:run()'>Closed</a></div>",
          "<div popover><img src='vbscript:run()'></div>",
          ~S|<div popover style="background-image:url('\6a avascript:run')">Closed</div>|
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, opts)
      assert diagnostic.operation == :render
      assert diagnostic.stage in [:html, :css]
      assert diagnostic.message =~ "non-executable reference"
    end
  end

  test "exposed popover images retain actionable resource diagnostics" do
    for html <- [
          "<img popover src=missing.png style='display:block'>",
          "<div popover style='display:block;background-image:url(missing.png)'>Visible</div>"
        ] do
      assert {:error, {reason, diagnostic}} = HtmlToPdf.render(html)
      assert reason == diagnostic.reason
      assert diagnostic.stage == :asset and diagnostic.operation == :render
      assert diagnostic.source == "missing.png"
      assert diagnostic.message =~ "not an authorized local file"
    end
  end

  test "single-line inline block baselines preserve equivalent complete PDFs with links and initial fields" do
    raw =
      "<div style='font-family:Fixture Sans;font-size:10pt;line-height:12pt;margin:0'>Before<span style='display:inline-block'><a href='https://example.com/baseline'>Inside</a></span>After</div><input name=initial value=Initial>"

    reference = String.replace(raw, "display:inline-block", "display:inline")

    for forms <- [:static, :interactive] do
      opts = [
        forms: forms,
        fonts: [%{family: "Fixture Sans", path: ttf_font_path!()}],
        system_font_discovery: false
      ]

      assert {:ok, actual} = HtmlToPdf.render(raw, opts)
      assert {:ok, expected} = HtmlToPdf.render(reference, opts)
      assert actual == expected
      assert {:ok, text} = Text.extract(actual)
      assert text =~ "BeforeInsideAfter"
      if forms == :static, do: assert(text =~ "Initial")

      if forms == :interactive do
        assert {:ok, [%{name: "initial", value: "Initial"}]} =
                 NativeElixirPdfUtilities.Forms.fields(actual)

        assert {:ok, filled} =
                 NativeElixirPdfUtilities.Forms.fill(actual, %{"initial" => "Updated"})

        assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(filled)
        assert {:ok, updated} = Text.extract(flattened)
        assert updated =~ "Updated"
      end
    end
  end

  test "heading bookmark titles retain printed inert text and omit hidden descendants" do
    html =
      "<h1 headingoffset='2'>Visible<span hidden>Hidden</span><span inert> inert</span><br>tail<template>Unused</template></h1>"

    assert {:ok, pdf} = HtmlToPdf.render(html, outlines: :headings, system_font_discovery: false)
    assert {:ok, [%{title: "Visible inert tail"}]} = NativeElixirPdfUtilities.Outlines.get(pdf)
  end

  test "empty inline and atomic headings do not create outline entries" do
    html =
      "<h1>Root</h1><div headingoffset='2'><h2 style='display:inline'> </h2><h2 style='display:inline-block'> </h2><h2 style='display:inline'>Child</h2></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, outlines: :headings, system_font_discovery: false)

    assert {:ok, [%{title: "Root", children: [%{title: "Child"}]}]} =
             NativeElixirPdfUtilities.Outlines.get(pdf)
  end

  test "inline headings retain one computed bookmark across styles and page wraps" do
    html =
      "<h1>Root</h1><div headingoffset='2' style='width:45pt;font-size:10pt;line-height:12pt'><h1 style='display:inline;font-size:10pt;line-height:12pt'>Inline <strong>wrapped</strong> heading across lines</h1> tail<h2 style='display:inline-block;font-size:10pt;line-height:12pt;width:40pt'>Atomic heading</h2><h1 style='display:inline;font-size:10pt;line-height:12pt'>Inline duplicate</h1><h1 style='display:inline;font-size:10pt;line-height:12pt'>Inline duplicate</h1></div>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               outlines: :headings,
               page_size: {120, 100},
               margin: 6,
               system_font_discovery: false
             )

    assert {:ok, outlines} = NativeElixirPdfUtilities.Outlines.get(pdf)
    options = [outlines: false, page_size: {120, 100}, margin: 6, system_font_discovery: false]
    reference = String.replace(html, " headingoffset='2'", "")
    assert {:ok, visible} = HtmlToPdf.render(html, options)
    assert {:ok, ^visible} = HtmlToPdf.render(reference, options)
    assert [%{title: "Root", children: children}] = outlines

    assert [
             %{
               title: "Inline wrapped heading across lines",
               children: [%{title: "Atomic heading"}]
             },
             %{title: "Inline duplicate"},
             %{title: "Inline duplicate"}
           ] = children
  end

  test "heading offsets alter automatic outline nesting while preserving complete visible PDFs" do
    alias NativeElixirPdfUtilities.Outlines

    html =
      "<h1>Root</h1><x-box headingoffset='2' style='display:block'>" <>
        "<h1>Child</h1><h2>Grandchild</h2><h6 headingoffset='80'>Nine</h6>" <>
        "<h5 headingoffset='80'>Also nine</h5></x-box>" <>
        "<div headingoffset='4'><h2 headingreset='false'>Reset</h2></div>" <>
        "<h1 hidden headingoffset='8'>Hidden</h1><template><h1>Template</h1></template>" <>
        "<h1 inert>End</h1><input name=initial value=Initial><a href='https://example.com/heading'>Link</a>"

    reference = String.replace(html, ~r/ headingoffset='[^']*'| headingreset='[^']*'/, "")

    for forms <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: forms, outlines: false)
      assert {:ok, expected} = HtmlToPdf.render(reference, forms: forms, outlines: false)
      assert pdf == expected
    end

    assert {:ok, pdf} = HtmlToPdf.render(html, outlines: :headings)
    assert {:ok, [root, ending]} = Outlines.get(pdf)
    assert root.title == "Root" and ending.title == "End"
    [child, reset] = root.children
    assert child.title == "Child" and reset.title == "Reset"
    [grandchild] = child.children
    assert grandchild.title == "Grandchild"
    assert Enum.map(grandchild.children, & &1.title) == ["Nine", "Also nine"]
    assert reset.children == [] and ending.children == []
    assert {:ok, text} = Text.extract(pdf)
    refute text =~ "Hidden" or text =~ "Template"

    explicit = [%{title: "Caller", page: 1, children: []}]
    assert {:ok, pdf} = HtmlToPdf.render(html, outlines: explicit)
    assert {:ok, [%{title: "Caller", children: []}]} = Outlines.get(pdf)
  end

  test "heading numeric resource failures return actionable diagnostics before asset resolution" do
    owner = self()

    resolver = fn request ->
      send(owner, {:heading_asset, request})
      :not_found
    end

    value = String.duplicate("0", Limits.get(:max_html_numeric_token_bytes) + 1)
    html = "<div headingoffset='#{value}'><h1>Heading</h1><img src='denied'></div>"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(html, asset_resolver: resolver)

    assert diagnostic.stage == :html and diagnostic.operation == :render
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.source == "headingoffset=" <> value
    assert diagnostic.message =~ "max_html_numeric_token_bytes"
    refute_received {:heading_asset, _}

    assert {:ok, dom} = NativeElixirPdfUtilities.HtmlToPdf.HtmlParser.parse_detailed(html)
    located = update_in(dom, [:children, Access.at(0)], &Map.merge(&1, %{line: 7, column: 4}))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.Style.compute_detailed(located)

    assert diagnostic.source == "headingoffset=" <> value
    assert diagnostic.line == 7 and diagnostic.column == 4
  end

  test "word breaks and nobr retain full reference PDFs with links and initial fields" do
    raw =
      "<div style='font-size:10pt;line-height:12pt'><p style='width:40pt;margin:0'><a href='https://example.com'>ABC<wbr><strong>DEF</strong><wbr>GHI</a></p><p style='width:40pt;margin:0'><nobr>ABC DEF GHI</nobr></p><input name='field' value='Initial'><p>After</p></div>"

    reference =
      raw
      |> String.replace(
        "ABC<wbr><strong>DEF</strong><wbr>GHI",
        "ABC<br><strong>DEF</strong><br>GHI"
      )
      |> String.replace("<nobr>", "<span style='white-space:nowrap'>")
      |> String.replace("</nobr>", "</span>")

    for mode <- [:static, :interactive] do
      opts = [
        system_font_discovery: false,
        forms: mode,
        asset_resolver: fn _ -> flunk("word breaks and links must not fetch resources") end
      ]

      assert {:ok, expected} = NativeElixirPdfUtilities.HtmlToPdf.render(reference, opts)
      assert {:ok, ^expected} = NativeElixirPdfUtilities.HtmlToPdf.render(raw, opts)

      for source <- ["https://example.com/unused.png", "../unused.png", "unused.png"] do
        ignored_background =
          String.replace(raw, "<wbr>", "<wbr style='background-image:url(" <> source <> ")'>")

        assert {:ok, ^expected} =
                 NativeElixirPdfUtilities.HtmlToPdf.render(ignored_background, opts)
      end

      assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(expected)

      visible =
        if mode == :interactive do
          assert [%{name: "field", value: "Initial"}] = fields
          assert {:ok, visible} = NativeElixirPdfUtilities.Forms.flatten(expected)
          visible
        else
          assert fields == []
          expected
        end

      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(visible, layout: false)
      assert text =~ "Initial" and text =~ "After" and text =~ "ABC DEF GHI"
      assert expected =~ "https://example.com"
    end
  end

  test "empty optional and forced lines preserve identical paginated PDFs" do
    raw =
      "<div style='font-size:10pt;line-height:12pt'><div style='height:96pt'>First</div>" <>
        "<p style='margin:0'><wbr><wbr style='font-size:30pt;line-height:36pt'></p>" <>
        "<a href='https://example.com'>After</a><input name='field' value='Initial'></div>"

    reference =
      String.replace(raw, "<wbr><wbr style='font-size:30pt;line-height:36pt'>", "<br>")

    for forms <- [:static, :interactive] do
      opts = [page_size: {200, 96}, margin: 0, forms: forms, system_font_discovery: false]
      assert {:ok, expected} = HtmlToPdf.render(reference, opts)
      assert {:ok, ^expected} = HtmlToPdf.render(raw, opts)
      assert {:ok, 2} = NativeElixirPdfUtilities.Info.page_count(expected)
    end
  end

  test "styled word wrapping preserves complete PDFs, initial fields, and URI regions" do
    content = "go <a href='https://example.com'>ABC<strong>DEF</strong>GHI</a>"

    html =
      "<p style='width:40pt;font-size:10pt;line-height:12pt;margin:0'>" <>
        content <> "</p><input name=active value=Initial>"

    reference = String.replace(html, "go <a", "go<br><a", global: false)

    opts = [
      system_font_discovery: false,
      compress_streams: false,
      asset_resolver: fn request -> flunk("text wrapping fetched #{inspect(request)}") end
    ]

    for mode <- [:static, :interactive] do
      assert {:ok, expected} = HtmlToPdf.render(reference, Keyword.put(opts, :forms, mode))
      assert {:ok, ^expected} = HtmlToPdf.render(html, Keyword.put(opts, :forms, mode))
      assert expected =~ "https://example.com"
      assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(expected)

      if mode == :interactive,
        do: assert([%{name: "active", value: "Initial"}] = fields),
        else: assert(fields == [])
    end

    for css <- ["word-break:anywhere", "line-break:break-word", "overflow-wrap:invalid"] do
      assert {:error, {:invalid_css, diagnostic}} =
               HtmlToPdf.render("<p style='" <> css <> "'>Text</p>", opts)

      assert diagnostic.stage == :css and diagnostic.operation == :render
      assert diagnostic.reason == :invalid_css
      assert is_binary(diagnostic.message) and byte_size(diagnostic.message) > 0
      assert diagnostic.source == css and diagnostic.line == 1 and diagnostic.column == 1
    end
  end

  test "anchor defaults retain complete authored output and inert annotation policy" do
    html =
      "<div style='color:red'><a>Placeholder</a><a href='https://example.com'>Active <strong>nested</strong></a><a inert href='mailto:test@example.com'>Held</a><a href='http://example.com' style='color:red;text-decoration:none'>Authored</a></div>"

    reference =
      "<div style='color:red'><a>Placeholder</a><a href='https://example.com' style='color:#0000ee;text-decoration:underline'>Active <strong>nested</strong></a><a inert href='mailto:test@example.com' style='color:#0000ee;text-decoration:underline'>Held</a><a href='http://example.com' style='color:red;text-decoration:none'>Authored</a></div>"

    opts = [
      system_font_discovery: false,
      compress_streams: false,
      asset_resolver: fn request -> flunk("anchor fetched a dependency #{inspect(request)}") end
    ]

    for mode <- [:static, :interactive] do
      assert {:ok, expected} = HtmlToPdf.render(reference, Keyword.put(opts, :forms, mode))
      assert {:ok, ^expected} = HtmlToPdf.render(html, Keyword.put(opts, :forms, mode))
      assert_pdf_text(expected, ["Placeholder", "Active", "nested", "Held", "Authored"])
      assert expected =~ "https://example.com"
      assert expected =~ "http://example.com"
      refute expected =~ "mailto:test@example.com"
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(expected)
    end

    for {href, expected_reason} <- [
          {"relative.html", :invalid_document}
        ] do
      assert {:error, {reason, diagnostic}} =
               HtmlToPdf.render("<a href='" <> href <> "'>Link</a>", opts)

      assert reason == expected_reason
      assert diagnostic.stage == :style and diagnostic.operation == :render
      assert is_binary(diagnostic.message) and byte_size(diagnostic.message) > 0
    end

    assert {:ok, file_link_pdf} =
             HtmlToPdf.render("<a href='file:///tmp/private'>Link</a>", opts)

    assert_pdf_text(file_link_pdf, "Link")
    assert file_link_pdf =~ "/URI (file:///tmp/private)"
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(file_link_pdf)

    assert {:ok, fragment_pdf} = HtmlToPdf.render("<a href='#fragment'>Link</a>", opts)
    assert_pdf_text(fragment_pdf, "Link")
    refute fragment_pdf =~ "/Subtype /Link"
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(fragment_pdf)
  end

  test "static semantic inline defaults preserve complete reference PDFs and active fields" do
    html =
      "<div>Before <mark>Marked <strong>bold</strong></mark> <ins cite='https://example.com/change' datetime='2026-10-09T00:00Z'>Added <span>nested</span></ins> <del datetime=invalid>Removed</del> <strike>Legacy</strike> <input name=active value=Initial> After<template><mark><img src=denied.png></mark></template></div>"

    reference =
      "<div>Before <span style='background:#ffff00;color:black'>Marked <strong>bold</strong></span> <u>Added <span>nested</span></u> <s>Removed</s> <s>Legacy</s> <input name=active value=Initial> After<template><span><img src=denied.png></span></template></div>"

    opts = [
      system_font_discovery: false,
      asset_resolver: fn request ->
        flunk("inert semantic resource resolved #{inspect(request)}")
      end
    ]

    for mode <- [:static, :interactive] do
      assert {:ok, expected} = HtmlToPdf.render(reference, Keyword.put(opts, :forms, mode))
      assert {:ok, ^expected} = HtmlToPdf.render(html, Keyword.put(opts, :forms, mode))
      assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(expected)

      if mode == :interactive,
        do: assert([%{name: "active", value: "Initial"}] = fields),
        else: assert(fields == [])
    end

    for tag <- ["ins", "del"] do
      decoration = if tag == "ins", do: "underline", else: "line-through"
      html = "<div>Before<" <> tag <> "><p>Block</p>After</" <> tag <> "><p>Last</p></div>"

      reference =
        "<div>Before<noscript style='text-decoration:" <>
          decoration <> "'><p>Block</p>After</noscript><p>Last</p></div>"

      assert {:ok, expected} = HtmlToPdf.render(reference, opts)
      assert {:ok, ^expected} = HtmlToPdf.render(html, opts)
    end
  end

  test "initial inert content preserves complete static artwork and ordinary resource failures" do
    content =
      "<div><input name=held value=Printed><textarea name=area>Area</textarea><select name=choice><option selected>Selected</option></select><a href='https://example.com'>Link</a></div>"

    reference =
      String.replace(
        content,
        " href='https://example.com'",
        " style='color:#0000ee;text-decoration:underline'"
      )

    opts = [system_font_discovery: false]
    assert {:ok, expected} = HtmlToPdf.render(reference, Keyword.put(opts, :forms, :static))

    for value <- ["", "false", "anything"], mode <- [:static, :interactive] do
      html = String.replace(content, "<div>", "<div inert='" <> value <> "'>", global: false)
      assert {:ok, ^expected} = HtmlToPdf.render(html, Keyword.put(opts, :forms, mode))
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(expected)
      refute expected =~ "/Subtype /Link"
    end

    assert_pdf_text(expected, ["Printed", "Area", "Selected", "Link"])

    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<div inert><input name=same value=Held></div><input name=same value=Active>",
               opts
             )

    assert {:ok, [%{name: "same", value: "Active"}]} = NativeElixirPdfUtilities.Forms.fields(pdf)

    assert {:error, {:invalid_document, %{stage: :asset, source: "asset:denied"}}} =
             HtmlToPdf.render("<div inert><img src='asset:denied'></div>",
               asset_resolver: fn request ->
                 send(self(), {:inert_asset, request})
                 {:error, :denied}
               end
             )

    assert_received {:inert_asset, %{reference: "asset:denied", kind: :image}}
  end

  test "initial hidden containers preserve complete visible PDFs fields and dormant resource policy" do
    html =
      "<div><p hidden=false>Unused</p><input hidden name=actual value=Unused><x-box hidden><img src=denied.png><input name=actual value=Unused></x-box><p hidden style='display:block'>Shown</p><input name=actual value=Initial><p>After</p></div>"

    reference =
      "<div><p style='display:block'>Shown</p><input name=actual value=Initial><p>After</p></div>"

    opts = [
      system_font_discovery: false,
      asset_resolver: fn request -> flunk("hidden resource resolved #{inspect(request)}") end
    ]

    for mode <- [:static, :interactive] do
      assert {:ok, expected} = HtmlToPdf.render(reference, Keyword.put(opts, :forms, mode))
      assert {:ok, ^expected} = HtmlToPdf.render(html, Keyword.put(opts, :forms, mode))
      assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(expected)

      if mode == :interactive,
        do: assert([%{name: "actual", value: "Initial"}] = fields),
        else: assert(fields == [])
    end

    assert {:ok, blank} = HtmlToPdf.render("<p hidden=until-found>Unprinted content</p>")
    assert {:error, {:no_extractable_text, _}} = NativeElixirPdfUtilities.Text.extract(blank)

    assert {:error, {:invalid_document, %{stage: :asset, source: "asset:denied"}}} =
             HtmlToPdf.render("<img hidden style='display:block' src='asset:denied'>",
               asset_resolver: fn request ->
                 send(self(), {:visible_hidden_asset, request})
                 {:error, :denied}
               end
             )

    assert_received {:visible_hidden_asset, %{reference: "asset:denied", kind: :image}}

    # The existing strict img source contract also applies to the hidden image itself.
    assert {:error, {:invalid_document, %{stage: :asset, source: "asset:denied"}}} =
             HtmlToPdf.render("<img hidden src='asset:denied'>",
               asset_resolver: fn request ->
                 send(self(), {:strict_hidden_image, request})
                 {:error, :denied}
               end
             )

    assert_received {:strict_hidden_image, %{reference: "asset:denied", kind: :image}}

    for tag <- ["html", "body", "div"] do
      hidden = "<" <> tag <> " hidden><p>Unused</p><img src=denied.png></" <> tag <> ">"

      equivalent =
        "<" <> tag <> " style='display:none'><p>Unused</p><img src=denied.png></" <> tag <> ">"

      assert {:ok, expected} = HtmlToPdf.render(equivalent, opts)
      assert {:ok, ^expected} = HtmlToPdf.render(hidden, opts)
    end
  end

  test "unregistered custom containers preserve full reference PDFs editable fields and resource policy" do
    html =
      "<x-box readonly multiple disabled src='https://denied.example/custom' href='#unused' arbitrary='metadata'>Before <span>Inline</span><div>Block</div><input is='x-field' name='active' value='Initial'> After</x-box><template><x-box><style>x-box{color:red}</style><img src=denied.png><input name='active' value=Unused></x-box></template>"

    reference =
      "<noscript>Before <span>Inline</span><div>Block</div><input name='active' value='Initial'> After</noscript><template><noscript><style>noscript{color:red}</style><img src=denied.png><input name='active' value=Unused></noscript></template>"

    opts = [
      system_font_discovery: false,
      asset_resolver: fn request ->
        flunk("metadata/inert resource resolved #{inspect(request)}")
      end
    ]

    for mode <- [:static, :interactive] do
      assert {:ok, expected} = HtmlToPdf.render(reference, Keyword.put(opts, :forms, mode))
      assert {:ok, ^expected} = HtmlToPdf.render(html, Keyword.put(opts, :forms, mode))
      assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(expected)

      if mode == :interactive,
        do: assert([%{name: "active", value: "Initial"}] = fields),
        else: assert(fields == [])
    end
  end

  test "escaped CSS identifiers preserve complete PDFs and generated content" do
    html =
      "<div><p class='café' id='123' data-kind='Active'>First</p><p class='a,b'>Second</p><p class='a>b'>Third</p><p class='a{b'>Fourth</p><p class='e\u0301 a\u00A0b'>Fifth</p></div>"

    css =
      ~S|\70 .\63 afé#\31 23[data-\6b ind="\41 ctive"] {color:red} .a\,b {color:blue} .a\>b {color:red} .a\{b {color:blue} .é.a\a0 b {color:red} p:\66 irst-child::\62 efore {content:'Prefix '}|

    reference_css =
      "p.café[id='123'][data-kind='Active'] {color:red} [class='a,b'] {color:blue} [class='a>b'] {color:red} [class='a{b'] {color:blue} .é.a\u00A0b {color:red} p:first-child::before {content:'Prefix '}"

    assert {:ok, expected} =
             HtmlToPdf.render(html,
               stylesheets: [{:css, reference_css}],
               system_font_discovery: false
             )

    assert {:ok, ^expected} =
             HtmlToPdf.render(html, stylesheets: [{:css, css}], system_font_discovery: false)
  end

  test "decorated inline noscript block contents retain complete fallback text" do
    for html <- [
          "<body><noscript style='background:red'><p>Block fallback</p></noscript></body>",
          "<body>Before<noscript style='position:relative'><span><span style='display:block'>Block fallback</span></span></noscript>After</body>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Block fallback"
      assert length(Regex.scan(~r/Block fallback/, text)) == 1
    end
  end

  test "anonymous inline block flow retains complete PDFs and active controls" do
    for {html, reference} <- [
          {"<body><noscript><p>Block fallback</p></noscript></body>",
           "<body><p>Block fallback</p></body>"},
          {"<body>Before<noscript>Start<p>Block fallback</p>End</noscript>After</body>",
           "<body>Before<span>Start</span><p>Block fallback</p><span>End</span>After</body>"},
          {"<body><span style='color:red'>Start<span><span style='display:block'>Block</span></span>End</span></body>",
           "<body><span style='color:red'>Start</span><span style='display:block;color:red'>Block</span><span style='color:red'>End</span></body>"},
          {"<body><noscript>Before<p>Block</p><input name=fallback value=Initial>After</noscript><template><input name=fallback value=Unused></template></body>",
           "<body><span>Before</span><p>Block</p><input name=fallback value=Initial><span>After</span><template></template></body>"}
        ] do
      for options <- [[], [forms: :static]] do
        assert {:ok, pdf} = HtmlToPdf.render(html, options)
        assert {:ok, ^pdf} = HtmlToPdf.render(reference, options)
      end
    end
  end

  test "anonymous blocks preserve whitespace, percentage sizes and sibling margins" do
    for inner <- [
          "<span style='display:block;width:50%;background:#eee'>First</span>",
          "<span style='display:block;margin-bottom:6pt'>First</span>  <span style='display:block;margin-top:3pt'>Second</span>",
          "<span style='display:block;margin-bottom:-2pt'>First</span><span style='display:block;margin-top:4pt'>Second</span>"
        ],
        context <- [:block, :table_cell, :inline_block] do
      raw = "Before<span><span>#{inner}</span></span>After"
      reference = "Before#{inner}After"

      wrap = fn content ->
        case context do
          :block ->
            "<body><div style='width:100pt'>#{content}</div></body>"

          :table_cell ->
            "<body><table style='width:100pt'><tr><td>#{content}</td></tr></table></body>"

          :inline_block ->
            "<body><span style='display:inline-block;width:100pt'>#{content}</span></body>"
        end
      end

      assert {:ok, pdf} = HtmlToPdf.render(wrap.(raw))
      assert {:ok, ^pdf} = HtmlToPdf.render(wrap.(reference))
    end
  end

  test "anonymous inline flow preserves images and their ancestor background" do
    image = "data:image/png;base64,#{Base.encode64(png_fixture(1, 1))}"

    for tag <- ["span", "noscript"] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<body><#{tag} style='background:red'>Before<img src='#{image}' width=20 height=10>After</#{tag}></body>"
               )

      assert pdf =~ "/Subtype /Image"
      assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf)
      assert text =~ "Before" and text =~ "After"
    end
  end

  test "anonymous boundary blocks collapse margins through empty inline ancestors" do
    for inset <- ["padding:0", "padding:1pt", "border:1pt solid blue"],
        margin <- ["margin-top:6pt", "margin-bottom:6pt", "margin:6pt 0", "margin:-2pt 0"],
        content <- ["Block", ""] do
      parent = "background:#eee;#{inset}"
      block = "<span style='display:block;#{margin}'>#{content}</span>"

      raw =
        "<body><div style='#{parent}'><span></span><span> <span>#{block}</span> </span><span></span></div><p>Following</p></body>"

      reference = "<body><div style='#{parent}'>#{block}</div><p>Following</p></body>"
      assert {:ok, pdf} = HtmlToPdf.render(raw)
      assert {:ok, ^pdf} = HtmlToPdf.render(reference)
    end
  end

  test "anonymous fragments preserve text through ancestor painting and positioning" do
    for style <- [
          "background:red",
          "padding:1pt",
          "margin:1pt",
          "border:1pt solid red",
          "position:relative",
          "vertical-align:top",
          "overflow:hidden"
        ] do
      html =
        "<body><span style='#{style}'>Before<span><span style='display:block'>Block</span></span>After</span></body>"

      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, text} = Text.extract(pdf)

      for label <- ["Before", "Block", "After"],
          do: assert(length(Regex.scan(Regex.compile!(label), text)) == 1)
    end

    assert {:ok, linked} =
             HtmlToPdf.render(
               "<body><a href='https://example.test'><span style='display:block'>Linked block</span></a></body>"
             )

    assert_pdf_text(linked, "Linked block")
    assert linked =~ "https://example.test"
  end

  test "head noscript metadata preserves source order and caller precedence" do
    html =
      "<html><head><title>Fallback</title><meta name=author content=Before><noscript><meta name=author content=Active><meta name=description content=Subject><meta name=keywords content='one, two'></noscript><meta name=author content=After></head><body><p>Visible</p><template><noscript><meta name=author content=Unused></noscript></template></body></html>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "Before, Active, After"
    assert pdf =~ "Subject"
    assert pdf =~ "one, two"
    refute pdf =~ "Unused"
    assert {:ok, override} = HtmlToPdf.render(html, metadata: [author: "Caller"])
    assert override =~ "Caller"
    refute override =~ "Before, Active, After"
  end

  test "noscript active text and controls preserve full PDF values and resource policy" do
    html =
      "<body><p>Before <noscript>Fallback <b>Bold</b></noscript> after</p><noscript style='display:block'><p>Block</p><input name=fallback value=Initial></noscript><template><noscript><input name=fallback value=Unused><img src=denied.png></noscript></template></body>"

    reference =
      "<body><p>Before <span>Fallback <b>Bold</b></span> after</p><div style='display:block'><p>Block</p><input name=fallback value=Initial></div><template></template></body>"

    resolver = fn request ->
      send(self(), {:unexpected_noscript_resource, request})
      {:error, :denied}
    end

    for forms <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: forms, asset_resolver: resolver)
      assert {:ok, clean} = HtmlToPdf.render(reference, forms: forms)
      assert pdf == clean

      visible =
        if forms == :interactive do
          assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert [%{name: "fallback", value: "Initial"}] = fields

          assert {:ok, filled} =
                   NativeElixirPdfUtilities.Forms.fill(pdf, %{"fallback" => "Updated"})

          assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(filled)
          flattened
        else
          pdf
        end

      assert {:ok, text} = Text.extract(visible)
      for label <- ~w(Before Fallback Bold after Block), do: assert(text =~ label)
      refute text =~ "Unused"
    end

    refute_received {:unexpected_noscript_resource, _}

    assert {:error, {_, diagnostic}} =
             HtmlToPdf.render(
               "<body><noscript><img src='https://invalid.test/fallback.png'></noscript></body>",
               asset_resolver: resolver
             )

    assert is_binary(diagnostic.message)
    assert diagnostic.operation == :render
    assert diagnostic.reason != nil

    assert_received {:unexpected_noscript_resource,
                     %{reference: "https://invalid.test/fallback.png", kind: :image}}

    assert {:error, {_, ordinary_diagnostic}} =
             HtmlToPdf.render("<body><img src='https://invalid.test/fallback.png'></body>",
               asset_resolver: resolver
             )

    assert diagnostic == ordinary_diagnostic

    assert_received {:unexpected_noscript_resource,
                     %{reference: "https://invalid.test/fallback.png", kind: :image}}
  end

  test "specialized script-supporting content retains complete PDFs and select editing" do
    inert =
      "<script type='application/json' src='https://invalid.test/data'>{}</script><template><style>li{color:red}</style><img src=denied.png><input name=choice value=Unused></template>"

    raw =
      "<body><ol reversed>#{inert}<li>One</li>#{inert}<li>Two</li></ol><dl>#{inert}<dt>Term</dt>#{inert}<dd>Description</dd></dl><table><col>#{inert}<tr>#{inert}<td>Cell</td></tr>#{inert}<tr><td>Last</td></tr></table><button>Label#{inert}</button><select name=choice>#{inert}<option value=A>First</option>#{inert}<option value=B selected>Second</option></select></body>"

    clean =
      "<body><ol reversed><li>One</li><li>Two</li></ol><dl><dt>Term</dt><dd>Description</dd></dl><table><col><tr><td>Cell</td></tr><tr><td>Last</td></tr></table><button>Label</button><select name=choice><option value=A>First</option><option value=B selected>Second</option></select></body>"

    resolver = fn request ->
      send(self(), {:unexpected_specialized_resource, request})
      {:error, :denied}
    end

    for forms <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(raw, forms: forms, asset_resolver: resolver)
      assert {:ok, reference} = HtmlToPdf.render(clean, forms: forms)
      assert pdf == reference
      assert_pdf_text(pdf, "One")

      visible =
        if forms == :interactive do
          assert {:ok, flattened} = NativeElixirPdfUtilities.Forms.flatten(pdf)
          flattened
        else
          pdf
        end

      assert {:ok, text} = Text.extract(visible)
      for label <- ~w(Two Term Description Cell Last Label Second), do: assert(text =~ label)
      refute text =~ "Unused"
      refute text =~ "{}"

      if forms == :interactive do
        assert {:ok, [%{name: "choice", value: "B"}]} = NativeElixirPdfUtilities.Forms.fields(pdf)
        assert {:ok, filled} = NativeElixirPdfUtilities.Forms.fill(pdf, %{"choice" => "A"})
        assert {:ok, flat} = NativeElixirPdfUtilities.Forms.flatten(filled)
        assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(flat)
        assert_pdf_text(flat, "First")
      else
        assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
      end
    end

    refute_received {:unexpected_specialized_resource, _}
  end

  test "visible script data and inert templates retain content through list and table formation" do
    html =
      "<body><ul><li>Before</li><script type='text/plain' style='display:block'>Visible data</script><template style='display:block;border:1px solid blue;padding:2pt'><p>Unused</p></template><li>After</li></ul></body>"

    reference =
      "<body><ul><li>Before</li><script type='text/plain' style='display:block'>Visible data</script><template style='display:block;border:1px solid blue;padding:2pt'></template><li>After</li></ul></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, expected} = HtmlToPdf.render(reference, forms: :static)
    assert pdf == expected
    assert_pdf_text(pdf, "Visible data")
    assert_pdf_text(pdf, "Before")
    assert_pdf_text(pdf, "After")

    for {prefix, suffix} <- [
          {"<table>", "<tr><td>Cell</td></tr></table>"},
          {"<table><tbody>", "<tr><td>Cell</td></tr></tbody></table>"},
          {"<table><tr>", "<td>Cell</td></tr></table>"},
          {"<table><colgroup>", "<col></colgroup><tr><td>Cell</td></tr></table>"}
        ],
        tag <- ["script", "template"],
        not (tag == "script" and prefix == "<table><colgroup>") do
      type = if tag == "script", do: "type='text/plain'", else: ""

      assert {:ok, table_pdf} =
               HtmlToPdf.render(
                 prefix <> "<#{tag} #{type} style='display:block'>Visible</#{tag}>" <> suffix
               )

      assert_pdf_text(table_pdf, "Cell")
      if tag == "script", do: assert_pdf_text(table_pdf, "Visible")
    end
  end

  test "positional rules preserve complete PDF content and match explicit style references" do
    raw =
      "<style>p:nth-of-type(-n+3){color:red}p:nth-last-of-type(2n){font-weight:bold}</style><body><div><p>One</p><span>Other</span><p>Two</p><template><p>Unused</p></template><p>Three</p><p>Four</p><p>Five</p></div></body>"

    explicit =
      "<body><div><p style='color:red'>One</p><span>Other</span><p style='color:red;font-weight:bold'>Two</p><template><p>Unused</p></template><p style='color:red'>Three</p><p style='font-weight:bold'>Four</p><p>Five</p></div></body>"

    assert {:ok, pdf} = HtmlToPdf.render(raw, forms: :static, system_font_discovery: false)

    assert {:ok, reference} =
             HtmlToPdf.render(explicit, forms: :static, system_font_discovery: false)

    assert pdf == reference
    assert_pdf_text(pdf, "One Other Two Three Four Five")
    assert {:ok, [_]} = Split.by_page(pdf)
  end

  test "unused templates preserve complete static output and active form ownership" do
    html =
      "<template><style>p{color:red}</style><img src=denied.png><input name=same value=Hidden><svg><path d='M0 0'/></svg></template><input name=same value=Visible><p>After</p>"

    resolver = fn request ->
      send(self(), {:unexpected_inert_resource, request})
      {:error, :denied}
    end

    assert {:ok, pdf} = HtmlToPdf.render(html, asset_resolver: resolver)
    assert {:ok, reference} = HtmlToPdf.render("<input name=same value=Visible><p>After</p>")
    assert pdf == reference
    assert {:ok, [%{name: "same", value: "Visible"}]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    refute_received {:unexpected_inert_resource, _}
    css = "display:block;border:1px solid blue;padding:3pt;margin:0"

    assert {:ok, shown} =
             HtmlToPdf.render(
               "<body><template style='#{css}'><p>Unused</p></template><p>After</p></body>",
               forms: :static
             )

    assert {:ok, literal} =
             HtmlToPdf.render("<body><div style='#{css}'></div><p>After</p></body>",
               forms: :static
             )

    assert shown == literal
    assert_pdf_text(shown, "After")
    assert {:ok, [_page]} = Split.by_page(shown)
  end

  test "inactive templates count nodes and nesting against the unchanged limits" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_html_nodes: 5})

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :html, reason: :resource_limit_exceeded, message: message}}} =
             HtmlToPdf.render("<template><p>A</p><p>B</p></template><p>Visible</p>")

    assert message =~ "max_html_nodes"
    Limits.install(%{limits | max_html_depth: 2})

    assert {:error, {:resource_limit_exceeded, %{message: depth_message}}} =
             HtmlToPdf.render(
               "<template><template><p>Hidden</p></template></template><p>Visible</p>"
             )

    assert depth_message =~ "max_html_depth"
  end

  test "data blocks do not load resources or styles and shown raw data matches literal references" do
    html =
      "<script type='application/json' src='https://invalid.test/data'><img src='denied.png'><style>p{color:red}</style>&amp;</script><p>Visible</p>"

    resolver = fn request ->
      send(self(), {:unexpected_script_resource, request})
      {:error, :denied}
    end

    assert {:ok, pdf} = HtmlToPdf.render(html, asset_resolver: resolver)
    assert {:ok, reference} = HtmlToPdf.render("<p>Visible</p>")
    assert pdf == reference
    refute_received {:unexpected_script_resource, _}
    raw = "<!--<script>inner</script>tail--> &amp;"
    css = "display:block;white-space:pre;font-family:DejaVu Sans;font-size:10pt;line-height:13pt"

    assert {:ok, shown} =
             HtmlToPdf.render(
               "<body><script type='text/plain' style='#{css}'>#{raw}</script><p>After</p></body>"
             )

    assert {:ok, literal} =
             HtmlToPdf.render(
               "<body><div style='#{css}'>&lt;!--&lt;script&gt;inner&lt;/script&gt;tail--&gt; &amp;amp;</div><p>After</p></body>"
             )

    assert shown == literal
    assert_pdf_text(shown, [raw, "After"])
    assert {:ok, [_page]} = Split.by_page(shown)
  end

  test "data scripts retain shared HTML source and rendered text limits" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_html_source_bytes: 100})

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :html, reason: :resource_limit_exceeded, message: source_message}}} =
             HtmlToPdf.render("<script type='text/plain'>#{String.duplicate("x", 100)}</script>")

    assert source_message =~ "max_html_source_bytes"
    Limits.install(%{limits | max_rendered_text_bytes: 3})

    assert {:ok, _} =
             HtmlToPdf.render("<script type='text/plain'>hidden content</script><p>OK</p>")

    assert {:error,
            {:resource_limit_exceeded, %{reason: :resource_limit_exceeded, message: text_message}}} =
             HtmlToPdf.render(
               "<body><script type='text/plain' style='display:block'>shown content</script></body>"
             )

    assert text_message =~ "max_rendered_text_bytes"
  end

  test "zero-size foreignObject returns an explicit converter diagnostic" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<div contenteditable><svg contenteditable=false><foreignObject><p>Foreign</p></foreignObject></svg></div>"
             )

    assert diagnostic.source == "foreignObject"
    assert diagnostic.stage == :layout and diagnostic.reason == :unsupported_html
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.message =~ "cannot be omitted"
  end

  test "bounded foreignObject diagnoses unsupported HTML without omitting its content" do
    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(
               "<svg width='200' height='50'><foreignObject width='200' height='50'><p style='font-family:DejaVu Sans;font-size:14px'>Foreign café Ω</p></foreignObject></svg>"
             )

    assert diagnostic.source == "foreignObject"
    assert diagnostic.stage == :layout and diagnostic.reason == :unsupported_html
    assert diagnostic.module == NativeElixirPdfUtilities.Validators.SvgConverterValidator
    assert diagnostic.message =~ "cannot be omitted"
  end

  test "supported inline SVG geometry renders through the public API" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<svg width=20 height=20><path d='M0 0 H20 V20 Z' fill=red /></svg>"
             )

    assert pdf =~ "/Subtype /Image"
  end

  test "previously unsupported SVG path and MathML token inputs retain successful output" do
    assert {:ok, empty_path_pdf} = HtmlToPdf.render("<svg><path d='M0 0'/></svg>")
    assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(empty_path_pdf)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(empty_path_pdf)
    assert empty_path_pdf =~ "/Subtype /Image"
    assert {:error, {:no_extractable_text, _}} = Text.extract(empty_path_pdf, layout: false)

    assert {:ok, math_pdf} = HtmlToPdf.render("<math><mi>x</mi></math>")
    assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(math_pdf)
    assert {:ok, "x"} = Text.extract(math_pdf, layout: false)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(math_pdf)
  end

  test "original anonymous inline image inputs retain image and ancestor artwork" do
    image = "data:image/png;base64,#{Base.encode64(png_fixture(1, 1))}"

    for tag <- ["span", "noscript"] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<body><#{tag} style='background:red'><img src='#{image}'></#{tag}></body>",
                 compress_streams: false
               )

      assert pdf =~ "/Subtype /Image"
      assert pdf =~ "1 0 0 rg"
      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    end
  end

  test "HTML fragment links and targets survive inline SVG composition" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<p><a href=#scene>Scene</a> <a href=#target><svg id=scene width=20 height=20><path d='M0 0 H20 V20 Z' fill=red /></svg></a></p><p id=target>Target</p>"
             )

    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    destinations =
      for {_ref, %{value: %{"Subtype" => {:name, "Link"}, "Dest" => destination}}} <-
            document.objects,
          do: destination

    assert length(destinations) == 2

    assert Enum.all?(destinations, fn [{:ref, page}, {:name, "XYZ"}, x, y, nil] ->
             page == hd(document.pages).ref and x >= 0 and y >= 0
           end)

    assert pdf =~ "/Subtype /Image"
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert text =~ "Target" and text =~ "Scene"
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
  end

  test "supplementary Unicode glyphs survive subsetting and PDF text extraction" do
    html = "<p style='font-family:STIX Two Math'>A 𝑥 𝑦 𝛼 ∑ √</p>"

    for subset <- [true, false] do
      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 subset_fonts: subset,
                 system_font_discovery: false,
                 unsupported_glyphs: :error
               )

      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "A 𝑥 𝑦 𝛼 ∑ √"
    end
  end

  test "render preserves visible text with comments and unquoted attributes" do
    html = "<div class=copy><p>Joined<!-- hidden -->words &amp; text</p></div>"
    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Joinedwords & text"
    refute text =~ "hidden"
  end

  test "render returns actionable diagnostics for invalid comments and attributes" do
    for {html, message} <- [
          {"<div>\n  <!-- nested <!-- invalid -->\n</div>", "comment"},
          {"<div>\n  <p id=first ID='second'>Text</p>\n</div>", "attributes"}
        ] do
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :html
      assert diagnostic.reason == :unsupported_html
      assert diagnostic.operation == :render
      assert diagnostic.module == HtmlToPdf
      assert diagnostic.line == 2
      assert diagnostic.column == 3
      assert diagnostic.message =~ message
      assert diagnostic.source != ""
    end
  end

  test "comments consume the shared HTML token budget" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_html_nodes: 4})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<!-- one --><!-- two --><!-- three --><p>Text</p>")

    assert diagnostic.stage == :html
    assert diagnostic.operation == :render
    assert diagnostic.message =~ "max_html_nodes"
  end

  test "resources inside comments do not invoke the asset resolver" do
    resolver = fn request ->
      send(self(), {:comment_asset_request, request})
      :not_found
    end

    assert {:ok, _pdf} =
             HtmlToPdf.render("<!-- <img src=unused> --><p>Visible</p>",
               asset_resolver: resolver
             )

    refute_received {:comment_asset_request, _request}
  end

  test "tags in valid comments do not replace a later syntax failure's source" do
    for source <- ["<p id=first ID=second>", "</script>", "<p title=\"unterminated"] do
      html = "<!-- <canvas><script>example</script></canvas> -->\n<div>#{source}</div>"

      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :html
      assert diagnostic.operation == :render
      assert diagnostic.line == 2
      assert diagnostic.column == 6
      assert String.starts_with?(diagnostic.source, source)
    end
  end

  test "literal comment text in metadata does not replace an invalid comment's location" do
    comment = "<!-- nested <!-- invalid -->"
    html = "<p title=\"#{comment}\">Visible</p>\n#{comment}"

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.stage == :html
    assert diagnostic.line == 2
    assert diagnostic.column == 1
    assert diagnostic.source == comment
    assert diagnostic.message =~ "comment"
  end

  test "unsupported tags and attributes use their source after comments" do
    for {source, message} <- [
          {"<p title=\"<canvas>\" id=x ID=y>", "attributes"},
          {"<video>", "unsupported"}
        ] do
      html = "<!-- <script>example</script> -->\n  #{source}"
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.line == 2
      assert diagnostic.column == 3
      assert diagnostic.source == source
      assert diagnostic.message =~ message
    end
  end

  test "empty and comment-only documents render a blank page" do
    for html <- ["", " \n", "<!-- <script>literal comment text</script> -->", "<!DOCTYPE html>"] do
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:error, {:no_extractable_text, _diagnostic}} = Text.extract(pdf)
      assert {:ok, [_page]} = Split.by_page(pdf)
    end
  end

  test "omitted table and list tags preserve visible content and links" do
    html =
      "<ul><li>First<li>Second</ul><table><caption>Totals<tbody><tr><td>A<td>B<tr><td>C<td><a href='https://example.com'>Link</a></table><p>Last"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, text} = Text.extract(pdf)

    for content <- ["First", "Second", "Totals", "A", "B", "C", "Link", "Last"] do
      assert text =~ content
    end

    assert pdf =~ "https://example.com"
  end

  test "valid empty table structures render without losing following content" do
    for html <- [
          "<table></table>",
          "<table><colgroup></colgroup></table>",
          "<table><tbody><tr></tr></tbody></table>",
          "<table><caption>Caption</caption></table>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html <> "<p>After</p>")
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "After"
      if html =~ "Caption", do: assert(text =~ "Caption")
    end
  end

  test "empty headings do not create PDF outline entries" do
    assert {:ok, pdf} = HtmlToPdf.render("<h1></h1><h2> </h2><h3>Title</h3>", outlines: :headings)
    assert {:ok, [%{title: "Title"}]} = NativeElixirPdfUtilities.Outlines.get(pdf)
  end

  test "background tile limits preserve their actionable public diagnostic" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_background_image_tiles: 4})
    source = "data:image/png;base64,#{Base.encode64(png_fixture(1, 1))}"

    for tile_size <- ["1pt", "0.1pt"], tag <- ["div", "body"] do
      html =
        ~s|<#{tag} style="width: 4pt; height: 4pt; background-image: url('#{source}'); background-size: #{tile_size} #{tile_size}"></#{tag}>|

      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.stage == :limits
      assert diagnostic.operation == :render
      assert diagnostic.module == HtmlToPdf
      assert diagnostic.message == "background image tile count exceeds the 4-tile limit"
    end
  end

  test "oversized numeric page options return diagnostics before normalization arithmetic" do
    for value <- [Integer.pow(10, 400), 1.0e308],
        options <- [
          [margin: value],
          [margin: %{left: value}],
          [page_size: {value, 100}],
          [page_size: {100, value}]
        ] do
      assert {:error, {reason, diagnostic}} = HtmlToPdf.render("<p>Test</p>", options)
      assert reason in [:invalid_margin, :invalid_page_size]
      assert diagnostic.reason == reason
      assert diagnostic.operation == :render
      assert diagnostic.stage == :layout
      assert is_binary(diagnostic.message)
    end
  end

  test "render diagnoses unrepresentable aspect-ratio dimensions" do
    for ratio <- ["1e-310", "1e-320", "1e-20"],
        width <- ["", "width: 120pt;"],
        sizing <- ["content-box", "border-box"] do
      html = ~s(<div style="#{width} box-sizing: #{sizing}; aspect-ratio: #{ratio}">test</div>)

      assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :layout
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :render
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlValidator
      assert diagnostic.message =~ ~r/aspect-ratio|max_css_numeric_magnitude/
    end

    for ratio <- ["1 / 1e-320", "1e9 / 1e-10"] do
      assert {:error, {:resource_limit_exceeded, %{stage: :css, operation: :render}}} =
               HtmlToPdf.render(~s(<div style="aspect-ratio: #{ratio}">test</div>))
    end

    for ratio <- ["0", "-1", "invalid", "1e-320 / 1e9"] do
      assert {:error, {:invalid_css, %{operation: :render}}} =
               HtmlToPdf.render(~s(<div style="aspect-ratio: #{ratio}">test</div>))
    end

    assert {:ok, _pdf} =
             HtmlToPdf.render(~s(<div style="height: 20pt; aspect-ratio: 1e-320">test</div>))
  end

  test "render checks derived image aspect-ratio dimensions before layout or rasterization" do
    png = "data:image/png;base64,#{Base.encode64(png_fixture(1, 1))}"

    svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="2" height="1"><rect width="2" height="1"/></svg>)

    svg = "data:image/svg+xml;base64,#{Base.encode64(svg)}"

    for {source, stage} <- [{png, :layout}, {svg, :css}],
        style <- ["width: 20pt; aspect-ratio: 1e-320", "height: 20pt; aspect-ratio: 1e9"] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(~s(<img src="#{source}" style="#{style}">))

      assert diagnostic.stage == stage
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :render
      assert diagnostic.message =~ ~r/aspect-ratio|max_css_numeric_magnitude/
    end
  end

  test "render quantizes CSS viewport width and margin origins without changing the PDF page" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<div style='height:10pt;background:red'></div>",
               page_size: {100, 100},
               margin: 28
             )

    assert pdf =~ "/MediaBox [0 0 100 100]"
    assert pdf =~ "27.7499 62.5001 44.25 9.75 re"
  end

  test "render converts a simple paragraph to a valid PDF binary" do
    assert {:ok, pdf} = HtmlToPdf.render("<p>Hello</p>")

    assert String.starts_with?(pdf, "%PDF-1.4")
    assert pdf =~ "/Type /Catalog"
    assert pdf =~ "/Type /Page"
    assert_pdf_text(pdf, "Hello")
    assert pdf =~ "xref"
    assert pdf =~ "trailer"
    assert String.ends_with?(pdf, "%%EOF\n")
  end

  test "stream compression reduces file bytes without changing extracted text" do
    html = "<h1>Report</h1><p>" <> String.duplicate("Repeated synthetic detail. ", 30) <> "</p>"

    assert {:ok, plain} = HtmlToPdf.render(html, compress_streams: false, subset_fonts: false)
    assert {:ok, compressed} = HtmlToPdf.render(html, subset_fonts: false)
    assert byte_size(compressed) < byte_size(plain)
    assert compressed =~ "/Filter /FlateDecode"
    assert Text.extract(compressed, layout: false) == Text.extract(plain, layout: false)
  end

  test "font subsetting keeps extraction and can combine with stream compression" do
    html =
      "<h1>Subset report</h1><p>" <> String.duplicate("Synthetic café α notes. ", 20) <> "</p>"

    assert {:ok, plain} = HtmlToPdf.render(html, subset_fonts: false, compress_streams: false)
    assert {:ok, subset} = HtmlToPdf.render(html, compress_streams: false)
    assert {:ok, compact} = HtmlToPdf.render(html)

    assert byte_size(subset) < byte_size(plain) * 0.5
    assert byte_size(compact) < byte_size(subset)
    assert subset =~ ~r|/BaseFont /[A-Z]{6}\+|
    assert Text.extract(subset, layout: false) == Text.extract(plain, layout: false)
    assert Text.extract(compact, layout: false) == Text.extract(plain, layout: false)
  end

  test "compact output supports interactive forms and page splitting" do
    html =
      "<html><body><div><input type='text' name='reference' value='Synthetic value'></div><div style='break-after:page'>First page</div><p>Second page</p></body></html>"

    assert {:ok, pdf} = HtmlToPdf.render(html, subset_fonts: true, compress_streams: true)
    assert pdf =~ "/AcroForm"
    assert {:ok, pages} = Split.by_page(pdf)
    assert length(pages) == 2
    assert Enum.all?(pages, fn page -> match?({:ok, _text}, Text.extract(page)) end)
  end

  test "render writes form controls as static PDF content" do
    html = """
    <div>
      <input type="text" value="Applicant Alice">
      <input type="checkbox" checked>
      <input type="radio">
      <select><option>Pending</option><option selected>Approved</option></select>
      <textarea>Inspection notes</textarea>
      <button>Submit application</button>
    </div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static)

    assert_pdf_text(pdf, ["Applicant Alice", "Approved", "Inspection notes", "Submit application"])

    refute pdf =~ "/AcroForm"
    refute pdf =~ "/Widget"

    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    assert extracted =~ "Applicant Alice"
    assert extracted =~ "Approved"
    assert extracted =~ "Inspection notes"
    assert extracted =~ "Submit application"
  end

  test "hidden inputs produce no content, spacing, or PDF fields in either form mode" do
    hidden =
      ~s(<input type="hidden" name="duplicate.name" value="Secret">) <>
        ~s(<input type="HIDDEN" name="duplicate.name" disabled value="Another secret" style="display:block!important;position:absolute;width:200pt;height:200pt;background:red">) <>
        ~s(<input type="hidden">)

    for forms <- [:static, :interactive] do
      # Keep the same text-node boundary without a control in the reference.
      assert {:ok, expected} = HtmlToPdf.render("<p>Left<span></span>Right</p>", forms: forms)

      assert {:ok, pdf} =
               HtmlToPdf.render("<p>Left" <> String.trim(hidden) <> "Right</p>", forms: forms)

      assert pdf == expected
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert {:ok, ^text} = Text.extract(expected, layout: false)
      assert {:ok, %{pages: pages}} = Text.extract_spans(pdf)

      assert Enum.map_join(pages, "", &Enum.map_join(&1.spans, "", fn span -> span.text end)) ==
               "LeftRight"

      refute pdf =~ "/AcroForm"
      refute pdf =~ "/Widget"

      assert {:ok, blank} = HtmlToPdf.render(hidden, forms: forms)
      assert {:ok, reference_blank} = HtmlToPdf.render("", forms: forms)
      assert blank == reference_blank
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(blank)
      assert Text.extract(blank, layout: false) == Text.extract(reference_blank, layout: false)
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(hidden <> ~s(<input name="visible" value="Printed">))

    assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert field.name == "visible"
    assert field.value == "Printed"
  end

  test "text and search sanitization matches static artwork and interactive field values" do
    for type <- ["", ~s( type="text"), ~s( type="TEXT"), ~s( type="search"), ~s( type="SEARCH")],
        forms <- [:static, :interactive],
        {authored, expected} <- [{"A&#13;B&#10;C&#13;&#10;D", "ABCD"}, {"&#13;&#10;", ""}] do
      html = ~s(<input#{type} name="reference" value="#{authored}">)
      equivalent = ~s(<input#{type} name="reference" value="#{expected}">)
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: forms)
      assert {:ok, reference} = HtmlToPdf.render(equivalent, forms: forms)
      assert pdf == reference

      case forms do
        :static ->
          assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)

          case expected do
            "" ->
              assert {:error, {:no_extractable_text, %{stage: :text_extraction}}} =
                       Text.extract(pdf, layout: false)

            _ ->
              assert {:ok, text} = Text.extract(pdf, layout: false)
              assert String.trim(text) == expected
          end

        :interactive ->
          assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert field.value == expected
          assert field.read_only == false
      end
    end
  end

  test "case-insensitive HTML type selectors match equivalent authored colors" do
    for type <- ["search", "SEARCH", "SeArCh"], forms <- [:static, :interactive] do
      html =
        ~s|<style>input[type="SEARCH"] { color:blue; }</style><input type="#{type}" name="query" value="Printed">|

      reference = ~s(<input type="#{type}" name="query" value="Printed" style="color:blue">)
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: forms)
      assert {:ok, expected} = HtmlToPdf.render(reference, forms: forms)
      assert pdf == expected
    end
  end

  test "HTML attribute case folding consumes the shared CSS work budget" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_css_work: 6000})
    selector_value = String.duplicate("a", 1000)
    css = ~s(<style>input[disabled="#{selector_value}"] { color:red; }</style>)
    exact = css <> ~s(<input disabled="#{selector_value}" value="Text">)
    mixed = css <> ~s(<input disabled="#{String.upcase(selector_value)}" value="Text">)
    assert {:ok, _pdf} = HtmlToPdf.render(exact)
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(mixed)
    assert diagnostic.stage == :css
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.operation == :render
    assert diagnostic.message =~ "max_css_work"
  end

  test "attribute operators and case overrides preserve complete public PDF output" do
    for {selector, attributes, matches?} <- [
          {~s([title="AbC" s]), ~s(title="AbC"), true},
          {~s([title="abc" I]), ~s(title="AbC"), true},
          {~s([title~="TWO" i]), ~s(title="one&#9;Two&#10;three"), true},
          {~s([data-lang|="EN" i]), ~s(data-lang="en-US"), true},
          {~s([title^="ab" i]), ~s(title="AbCd"), true},
          {~s([title$="CD" I]), ~s(title="AbCd"), true},
          {~s([title*="bC" s]), ~s(title="AbCd"), true},
          {~s([title~="one Two" i]), ~s(title="one Two"), false},
          {~s([data-lang|="EN" s]), ~s(data-lang="en-US"), false},
          {~s([title^=""]), ~s(title="AbCd"), false},
          {~s([title$=""]), ~s(title="AbCd"), false},
          {~s([title*=""]), ~s(title="AbCd"), false}
        ] do
      html = ~s|<style>p#{selector} { color:blue; }</style><p #{attributes}>Complete text</p>|
      color = if matches?, do: "blue", else: "black"
      reference = ~s|<p #{attributes} style="color:#{color}">Complete text</p>|
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, expected} = HtmlToPdf.render(reference)
      assert pdf == expected
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert String.trim(text) == "Complete text"
    end

    for forms <- [:static, :interactive] do
      html =
        ~s|<style>input[type="SEARCH" s] { color:blue; }</style><input type="search" name="query" value="Printed">|

      assert {:ok, pdf} = HtmlToPdf.render(html, forms: forms)

      assert {:ok, reference} =
               HtmlToPdf.render(~s(<input type="search" name="query" value="Printed">),
                 forms: forms
               )

      assert pdf == reference
    end
  end

  test "malformed selector flags return actionable CSS diagnostics" do
    for selector <- ["[title i]", "[title=abc q]", "[title==abc]", "[title=42]"] do
      assert {:error, {:invalid_css, diagnostic}} =
               HtmlToPdf.render("<style>p" <> selector <> " {color:red}</style><p>Text</p>")

      assert diagnostic.stage == :css
      assert diagnostic.reason == :invalid_css
      assert diagnostic.operation == :render
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end
  end

  test "root positional selectors preserve complete explicitly styled PDF output" do
    for forms <- [:static, :interactive] do
      html = "<html><head></head><body><p>Document root</p></body></html>"

      reference =
        "<html style=\"color:red\"><head></head><body><p>Document root</p></body></html>"

      selector = "html:first-child:last-child:only-of-type:nth-child(1):nth-last-of-type(1)"

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 forms: forms,
                 stylesheets: [{:css, selector <> "{color:red}"}]
               )

      assert {:ok, expected} = HtmlToPdf.render(reference, forms: forms)
      assert pdf == expected

      fragment = "<p>First</p><span>Other</span><p>Second</p>"

      reference =
        "<p style=\"color:red\">First</p><span>Other</span><p style=\"font-weight:bold\">Second</p>"

      css = "p:first-child{color:red}p:nth-last-of-type(1){font-weight:bold}"
      assert {:ok, pdf} = HtmlToPdf.render(fragment, forms: forms, stylesheets: [{:css, css}])
      assert {:ok, expected} = HtmlToPdf.render(reference, forms: forms)
      assert pdf == expected
    end
  end

  test "all positional sibling scans consume the shared CSS work budget" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_css_work: 1000})
    roots = String.duplicate("<p>Text</p>", 600)

    for selector <-
          ~w|first-child last-child first-of-type last-of-type only-child only-of-type nth-child(1)| do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(roots, stylesheets: [{:css, "p:" <> selector <> "{color:red}"}])

      assert diagnostic.stage == :css
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :render
      assert diagnostic.message =~ "max_css_work"
    end
  end

  test "attribute token and substring work consumes the shared CSS budget" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_css_work: 1000})
    value = String.duplicate("a", 2000)

    assert {:ok, _pdf} =
             HtmlToPdf.render(
               ~s|<style>p[data-kind=a] {color:red}</style><p data-kind="#{value}">Text</p>|
             )

    for selector <- [
          "[data-kind~=a]",
          "[data-kind|=a]",
          "[data-kind^=a]",
          "[data-kind$=a]",
          "[data-kind*=a]",
          "[data-kind=a i]"
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(
                 ~s|<style>p#{selector} {color:red}</style><p data-kind="#{value}">Text</p>|
               )

      assert diagnostic.stage == :css
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.operation == :render
      assert diagnostic.message =~ "max_css_work"
    end
  end

  test "search inputs use editable text fields, generated names and disabled flags" do
    for type <- ["search", "SEARCH"], attributes <- ["", "disabled", ~s(name="query")] do
      html = ~s(<input type="#{type}" #{attributes} value="A&#10;B">)
      equivalent = ~s(<input type="text" #{attributes} value="AB">)

      for forms <- [:interactive, :static] do
        assert {:ok, pdf} = HtmlToPdf.render(html, forms: forms)
        assert {:ok, reference} = HtmlToPdf.render(equivalent, forms: forms)
        assert pdf == reference

        if forms == :interactive do
          assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
          assert field.type == :text
          assert field.name == if(attributes == ~s(name="query"), do: "query", else: "TEXT_1_1")
          assert field.read_only == (attributes == "disabled")
          assert field.value == "AB"
        end
      end
    end
  end

  test "search controls report unsupported attributes through HTML diagnostics" do
    for attributes <- [~s|onclick="run()"|] do
      assert {:error, {reason, diagnostic}} =
               HtmlToPdf.render(~s(<input type="search" #{attributes}>))

      assert diagnostic.stage == :html
      assert diagnostic.reason == reason
      assert diagnostic.operation == :render
      assert is_binary(diagnostic.message) and diagnostic.message != ""
      assert diagnostic.line == 1
      assert is_integer(diagnostic.column)
    end
  end

  test "hidden input validation still reports unsupported attributes and CSS" do
    for {html, stage} <- [
          {~s|<input type="hidden" onclick="run()">|, :html},
          {~s(<input type="hidden" style="unsupported-property:1">), :css}
        ] do
      assert {:error, {reason, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.reason == reason
      assert diagnostic.stage == stage
      assert diagnostic.operation == :render
      assert is_binary(diagnostic.message) and diagnostic.message != ""
      assert diagnostic.line == 1
      assert is_integer(diagnostic.column)
    end
  end

  test "render uses the HTML title as PDF metadata unless explicitly overridden" do
    html = "<html><head><title>  HTML Report  </title></head><body><p>Hello</p></body></html>"

    assert {:ok, default_pdf} = HtmlToPdf.render(html)
    assert default_pdf =~ "/Title (HTML Report)"
    assert default_pdf =~ "/Info "

    assert {:ok, explicit_pdf} =
             HtmlToPdf.render(html,
               metadata: [title: "Caller title", author: "Finance", subject: "Monthly"]
             )

    assert explicit_pdf =~ "/Title (Caller title)"
    refute explicit_pdf =~ "/Title (HTML Report)"
    assert explicit_pdf =~ "/Author (Finance)"
    assert explicit_pdf =~ "/Subject (Monthly)"

    assert {:ok, map_pdf} = HtmlToPdf.render(html, metadata: %{author: "Operations"})
    assert map_pdf =~ "/Title (HTML Report)"
    assert map_pdf =~ "/Author (Operations)"
  end

  test "document metadata maps head names and preserves explicit caller values" do
    html = """
    <head><title>  Annual\t report\n 2026 &amp; accounts </title>
    <meta name=AUTHOR content="Finance &amp; Operations">
    <meta name=author content="Accounting">
    <meta name=description content="Report summary">
    <meta name=keywords content="invoice, report, 2026">
    <meta name=keywords content="report, ,accounts,invoice">
    <meta name=viewport content="width=device-width">
    <meta property=og:title content="Social title">
    <meta charset=UTF-8>
    </head><body><p>Printed report</p></body>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, info} = NativeElixirPdfUtilities.Info.get(pdf)
    assert info.title == "Annual report 2026 & accounts"
    assert info.author == "Finance & Operations, Accounting"
    assert info.subject == "Report summary"
    assert info.keywords == "invoice, report, 2026, accounts"
    assert {:ok, text} = Text.extract(pdf, layout: false)
    assert String.trim(text) == "Printed report"

    for metadata <- [
          %{author: "Caller", subject: "Explicit", keywords: ["one", "two"]},
          [author: "Caller", subject: "Explicit", keywords: ["one", "two"]]
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html, metadata: metadata)
      assert {:ok, info} = NativeElixirPdfUtilities.Info.get(pdf)
      assert info.author == "Caller"
      assert info.subject == "Explicit"
      assert info.keywords == "one, two"
      assert info.title == "Annual report 2026 & accounts"
    end

    assert {:ok, pdf} = HtmlToPdf.render("<meta name=description content=\"\"><p>Visible</p>")
    assert {:ok, %{subject: ""}} = NativeElixirPdfUtilities.Info.get(pdf)
    assert {:ok, pdf} = HtmlToPdf.render("<title>&nbsp; A &nbsp;</title><p>Visible</p>")
    assert {:ok, %{title: "\u00A0 A \u00A0"}} = NativeElixirPdfUtilities.Info.get(pdf)
  end

  test "metadata defaults leave malformed caller values to the PDF validator" do
    html = "<meta name=author content=HTML><p>Visible</p>"

    for metadata <- [:bad, [:bad], [author: "first", author: "second"], %{unknown: "field"}] do
      assert {:error, {:invalid_pdf_input, diagnostic}} =
               HtmlToPdf.render(html, metadata: metadata)

      assert diagnostic.stage == :pdf
      assert diagnostic.operation == :write_pdf
      assert diagnostic.message == "PDF metadata must use supported fields and value types"
    end
  end

  test "unsupported meta directives return source diagnostics even without painted content" do
    for directive <-
          ~w(refresh CONTENT-SECURITY-POLICY content-security-policy-report-only set-cookie) do
      token = "<meta http-equiv=\"#{directive}\" content=\"0\" style=\"display:none\">"
      html = "<!-- #{token} -->\n<body>#{token}<p>Visible</p></body>"
      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :html
      assert diagnostic.operation == :render
      assert diagnostic.module == NativeElixirPdfUtilities.Validators.HtmlValidator
      assert diagnostic.source == token
      assert diagnostic.line == 2
      assert diagnostic.column == 7
      assert diagnostic.message =~ "outside the static rendering profile"
    end

    assert {:error,
            {:unsupported_html, %{message: message, source: "<meta charset=windows-1252>"}}} =
             HtmlToPdf.render("<meta charset=windows-1252><p>Visible</p>")

    assert message =~ "UTF-8"

    assert {:ok, _} =
             HtmlToPdf.render(
               "<meta http-equiv=X-UA-Compatible content=\"IE=edge\"><p>Visible</p>"
             )
  end

  test "decoration depth limits are shared and preserve ordinary failures" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(%{limits | max_text_decoration_layers: 1})
    assert {:ok, pdf} = HtmlToPdf.render("<p><u>A</u> <s>B</s></p>")
    assert_pdf_text(pdf, ["A", "B"])

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p><u><s>A</s></u></p>")

    assert diagnostic.stage == :style
    assert diagnostic.operation == :render
    assert diagnostic.message =~ "max_text_decoration_layers"
  end

  test "decorations preserve content and use caller-selected font metrics" do
    html =
      "<p><u>Underlined</u> <s>Struck</s> <u style='text-decoration-thickness:0'>Minimum stroke</u></p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               default_font: "Caller",
               fonts: [%{family: "Caller", path: ttf_font_path!()}],
               system_font_discovery: false
             )

    assert_pdf_text(pdf, ["Underlined", "Struck", "Minimum stroke"])
    assert pdf =~ "Caller"
  end

  test "text decoration CSS failures keep actionable diagnostics" do
    for css <- [
          "text-decoration-line:blink",
          "text-decoration-line:underline underline",
          "text-decoration:underline none",
          "text-decoration:none overline",
          "text-decoration:underline red blue",
          "text-decoration:underline solid solid",
          "text-decoration:underline 1pt 2pt",
          "text-decoration:wavy",
          "text-decoration:",
          "text-decoration-thickness:-1pt",
          "text-decoration-color:bad",
          "text-decoration-style:wavy"
        ] do
      assert {:error, {:invalid_css, diagnostic}} =
               HtmlToPdf.render("<u style='#{css}'>Visible</u>")

      assert diagnostic.stage == :css
      assert diagnostic.source =~ "text-decoration"
      assert diagnostic.message =~ "invalid or unsupported"
    end
  end

  test "render supports CSS-declared local fonts inside print media" do
    font_path = ttf_font_path!()

    html = """
    <style>
      @font-face {
        font-family: "CSS Fixture";
        src: url("#{font_path}") format("truetype");
      }
      @media screen { p { color: blue; } }
      @media print { p { color: red; font-family: "CSS Fixture"; } }
    </style>
    <p>CSS font</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: Path.dirname(font_path))
    assert pdf =~ "/Subtype /Type0"
    assert pdf =~ "1 0 0 rg"
    refute pdf =~ "0 0 1 rg"
  end

  test "render tries resolver-backed font-face sources in order" do
    valid_font = File.read!(ttf_font_path!())
    parent = self()

    resolver = fn %{kind: :font, reference: reference} ->
      send(parent, {:font_request, reference})

      case reference do
        "https://example.com/corrupt.ttf" -> {:ok, "not a font"}
        "https://example.com/valid.ttf" -> {:ok, valid_font}
      end
    end

    html = """
    <style>
      @font-face {
        font-family: "Resolver Fallback";
        src: url("https://example.com/corrupt.ttf"), url("https://example.com/valid.ttf");
      }
      p { font-family: "Resolver Fallback"; }
    </style>
    <p>Resolver font</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, asset_resolver: resolver)
    assert pdf =~ "/Subtype /Type0"
    assert_received {:font_request, "https://example.com/corrupt.ttf"}
    assert_received {:font_request, "https://example.com/valid.ttf"}
  end

  test "render supports commas inside quoted CSS font URLs" do
    fixture_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-comma-font-url")
    font_path = Path.join(fixture_dir, "report,sans.ttf")
    File.mkdir_p!(fixture_dir)
    File.cp!(ttf_font_path!(), font_path)

    html = """
    <style>
      @font-face {
        font-family: "Comma Fixture";
        src: url("report,sans.ttf") format("truetype");
      }
      p { font-family: "Comma Fixture"; }
    </style>
    <p>Comma font</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: fixture_dir)
    assert pdf =~ "/Subtype /Type0"
  after
    File.rm_rf(Path.join(System.tmp_dir!(), "native-elixir-pdf-comma-font-url"))
  end

  test "render identifies invalid font-face descriptors" do
    html = """
    <style>
      @font-face {
        font-family: Broken;
        src: url("broken.ttf");
        font-style: oblique;
      }
    </style>
    <p>Broken font</p>
    """

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               source: "font-style: oblique",
               message:
                 ~s(line 5: @font-face declaration "font-style: oblique" is invalid or unsupported)
             }}} = HtmlToPdf.render(html)
  end

  test "render rejects malformed PDF metadata through the diagnostics contract" do
    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :pdf,
               reason: :invalid_pdf_input,
               operation: :write_pdf,
               module: NativeElixirPdfUtilities.HtmlToPdf.PdfWriter,
               message: "PDF metadata must use supported fields and value types"
             }}} = HtmlToPdf.render("<title>Fallback</title><p>Hello</p>", metadata: :bad)

    assert {:error, {:invalid_pdf_input, %{stage: :pdf}}} =
             HtmlToPdf.render("<title>Fallback</title><p>Hello</p>", metadata: [:bad])
  end

  test "render converts headings and inline styled text to PDF runs" do
    html =
      ~s(<h1 style="color: #336699">Title</h1><p>Hello <strong>bold</strong> <em style="color: blue">italic</em></p>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert_pdf_text(pdf, ["Title", "Hello", "bold", "italic"])
    assert pdf =~ ~r|/BaseFont /[A-Z]{6}\+DejaVuSans|
    assert pdf =~ "0.2 0.4 0.6 rg"
    assert pdf =~ "0 0 1 rg"
  end

  test "render writes generated attributes and counters into PDF text runs" do
    html = """
    <style>
      body { counter-reset: section 0; }
      h2::before { counter-increment: section; content: "Section " counter(section) ": "; }
      [data-status=ready]:not(.hidden)::after { content: " [" attr(data-status) "]"; }
      .less-than::before { content: "< "; }
    </style>
    <h2 data-status="ready">Overview</h2>
    <h2 data-status="ready">Details</h2>
    <p class="less-than">Limit</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    assert extracted =~ "Section 1:"
    assert extracted =~ "Overview"
    assert extracted =~ "Section 2:"
    assert extracted =~ "Details"
    assert length(Regex.scan(~r/\[ready\]/, extracted)) == 2
    assert extracted =~ "<"
    assert extracted =~ "Limit"
  end

  test "render converts block box styling to PDF drawing commands" do
    html =
      ~s(<p style="margin: 2pt; padding: 3pt; border: 1pt solid red; border-radius: 2pt; background-color: #eeeeee">Boxed</p>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "0.9333 0.9333 0.9333 rg"
    assert pdf =~ "0.75 0 0 0.75 0 0 cm 1 0 0 RG 1 w"
    assert_pdf_text(pdf, "Boxed")
  end

  test "render accepts print template semantics and inch page sizes" do
    html =
      ~s(<section class="sheet"><style>@page { size: A4 landscape; margin: 7mm; }.sheet { height: 196mm; }</style><article><img src="data:image/png;base64,#{png_fixture_base64()}" alt="QR Code"><p>Sticker</p></article></section>)

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {4.92126, 1.49606})
    assert pdf =~ "/MediaBox [0 0 354.3307 107.7163]"
    assert_pdf_text(pdf, "Sticker")
    assert pdf =~ "/Subtype /Image"
  end

  test "render uses page CSS defaults unless caller overrides them" do
    html =
      ~s(<style>@page { size: A4 landscape; margin: 7mm; }</style><div style="height: 10pt; border: 1pt solid #000000">Page CSS</div>)

    assert {:ok, css_pdf} = HtmlToPdf.render(html)
    assert css_pdf =~ "/MediaBox [0 0 841.89 595.28]"
    assert_stroked_rectangle(css_pdf, {19.875, 564.905, 801.75, 10.5})

    assert {:ok, override_pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 0)
    assert override_pdf =~ "/MediaBox [0 0 200 100]"
    assert_stroked_rectangle(override_pdf, {0.375, 89.125, 199.5, 10.5})
  end

  test "render applies complete page geometry and explicit option precedence" do
    html = """
    <style>
      @page {
        size: A5 landscape;
        margin: 10pt 20pt 30pt 40pt;
        margin-left: 50pt;
      }
    </style>
    <div style="height: 10pt; border: 1pt solid #000000">Geometry</div>
    """

    assert {:ok, css_pdf} = HtmlToPdf.render(html)
    assert css_pdf =~ "/MediaBox [0 0 595.28 419.53]"
    assert_stroked_rectangle(css_pdf, {50.625, 398.905, 525, 10.5})

    assert {:ok, override_pdf} =
             HtmlToPdf.render(html,
               page_size: {200, 100},
               margin: "1pt 2pt 3pt 4pt"
             )

    assert override_pdf =~ "/MediaBox [0 0 200 100]"
    assert_stroked_rectangle(override_pdf, {4.125, 88.375, 193.5, 10.5})
  end

  test "render uses page CSS defaults from configured stylesheets" do
    assert {:ok, inline_pdf} =
             HtmlToPdf.render("<p>Configured page</p>",
               stylesheets: [{:css, "@media print { @page { size: letter; margin: 0; } }"}]
             )

    assert inline_pdf =~ "/MediaBox [0 0 612 792]"

    stylesheet_path =
      Path.join(System.tmp_dir!(), "native-elixir-pdf-configured-page-options.css")

    File.write!(stylesheet_path, "@page { size: A4 landscape; margin: 0; }")

    assert {:ok, file_pdf} =
             HtmlToPdf.render("<p>Configured file page</p>",
               stylesheets: [{:file, stylesheet_path}]
             )

    assert file_pdf =~ "/MediaBox [0 0 841.89 595.28]"
  after
    File.rm(Path.join(System.tmp_dir!(), "native-elixir-pdf-configured-page-options.css"))
  end

  @tag :tmp_dir
  test "furniture keeps the render stylesheet snapshot when a file changes", %{tmp_dir: directory} do
    path = Path.join(directory, "snapshot.css")
    css = "@page {size:200pt 200pt;margin:36pt} .accent {color:red}"
    File.write!(path, css)
    png = png_fixture(1, 1)

    resolver = fn %{kind: :image} ->
      File.write!(path, String.replace(css, "red", "blue"))
      {:ok, png}
    end

    opts = [
      stylesheets: [{:file, path}],
      asset_resolver: resolver,
      compress_streams: false,
      page_furniture: [header: "<span class='accent'>Header {{page}}</span>"]
    ]

    html =
      "<img src='https://example.com/snapshot.png'><p>Body</p><p style='break-before:page'>Next</p>"

    assert {:ok, first} = HtmlToPdf.render(html, opts)
    assert first =~ "1 0 0 rg"
    refute first =~ "0 0 1 rg"
    assert {:ok, text} = Text.extract(first)
    assert text =~ "Header 1"
    assert text =~ "Header 2"

    assert {:ok, second} = HtmlToPdf.render(html, opts)
    assert second =~ "0 0 1 rg"
    refute second =~ "1 0 0 rg"
  end

  test "render cascades page-margin longhands across configured and embedded stylesheets" do
    html = """
    <style>
      @page { margin-left: 40pt; margin-bottom: 30pt; }
    </style>
    <div style="height: 10pt; border: 1pt solid #000000">Cascade</div>
    """

    assert {:ok, pdf} =
             HtmlToPdf.render(html,
               page_size: {200, 100},
               stylesheets: [{:css, "@page { margin: 10pt 20pt; }"}]
             )

    assert_stroked_rectangle(pdf, {40.125, 79.375, 139.5, 10.5})
  end

  test "render returns detailed diagnostics for invalid page declarations" do
    html = """
    <style>
    @page {
      nonsense: value;
      margin: bananas;
    }
    </style>
    <p>Invalid page CSS</p>
    """

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               line: 3,
               column: 3,
               source: "nonsense: value",
               message: ~s(line 3: declaration "nonsense: value" is invalid or unsupported)
             }}} = HtmlToPdf.render(html)
  end

  test "render returns detailed diagnostics for unsupported page selectors" do
    html = """
    <style>
    @page :first { size: A5; margin: 1pt; }
    </style>
    <p>Unsupported page selector</p>
    """

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               line: 2,
               column: 1,
               source: "@page :first",
               message: ~s(line 2: page rule "@page :first" is invalid or unsupported)
             }}} = HtmlToPdf.render(html)
  end

  test "render accepts valid unused page-context properties" do
    html = """
    <style>
    @page {
      margin-top: 100px;
      margin-right: auto;
      page-orientation: upright;
      background: white;
      border: 1px solid black;
      padding: 5mm;
      color: black;
    }
    </style>
    <p>Valid page CSS</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert_pdf_text(pdf, "Valid page CSS")
  end

  test "render returns diagnostics for malformed embedded media rules" do
    html = "<style>@media print { @media print { p { color: red; } }</style><p>x</p>"

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf
             }}} = HtmlToPdf.render(html)
  end

  test "render falls back across CSS font sources and reports failed candidates" do
    font_path = ttf_font_path!()
    missing_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-missing-css-font.ttf")

    html = """
    <style>
      @font-face {
        font-family: FallbackFixture;
        src: url("#{missing_path}"), url("#{font_path}") format("truetype");
      }
      p { font-family: FallbackFixture; }
    </style>
    <p>Fallback font</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, base_url: Path.dirname(font_path))
    assert pdf =~ "/Subtype /Type0"

    failed_html = """
    <style>
      @font-face { font-family: MissingFixture; src: url("#{missing_path}"); }
      p { font-family: MissingFixture; }
    </style>
    <p>Missing font</p>
    """

    assert {:error,
            {:invalid_document,
             %{
               stage: :asset,
               reason: :invalid_document,
               source: ^missing_path,
               message: message
             }}} = HtmlToPdf.render(failed_html, base_url: Path.dirname(font_path))

    assert message =~ "not an authorized local file"
    refute message =~ "font-family"
  end

  test "render lays out grid containers nested inside flex items" do
    html = """
    <section style="display: flex; flex-direction: column; width: 400pt">
      <div>Header</div>
      <div style="display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 8px; flex: 1">
        <article style="display: flex; flex-direction: column; border: 1px solid #ccd6e1">
          <div style="display: block; text-transform: uppercase">thread</div>
          <div>AGSYIKOFP1</div>
        </article>
      </div>
    </section>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {600, 300})
    assert_pdf_text(pdf, ["Header", "THREAD", "AGSYIKOFP1"])
  end

  test "render applies configured and embedded CSS before writing PDF output" do
    html =
      ~s(<style>p.notice { color: #336699; font-weight: bold; }</style><p class="notice">Styled</p>)

    assert {:ok, pdf} = HtmlToPdf.render(html, stylesheets: [{:css, "p { color: red; }"}])
    assert_pdf_text(pdf, "Styled")
    assert pdf =~ ~r|/BaseFont /[A-Z]{6}\+DejaVuSans|
    assert pdf =~ "0.2 0.4 0.6 rg"
  end

  test "render converts lists and links to PDF text and annotations" do
    html =
      ~s(<ul><li>Read <a href="https://example.com">docs</a></li><li>Ship</li></ul>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert_pdf_text(pdf, ["•", "Read", "docs", "Ship"])
    assert pdf =~ "/Subtype /Link"
    assert pdf =~ "/URI (https://example.com)"

    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               source: "href",
               message: message
             }}} = HtmlToPdf.render(~s[<p><a href="javascript:alert(1)">bad</a></p>])

    assert message =~ "non-executable reference"
  end

  test "render returns detailed unsupported HTML diagnostics" do
    html = """
    <p>Before</p>
    <script>alert("bad")</script>
    """

    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               line: 2,
               source: ~s(<script>),
               message: message
             }}} = HtmlToPdf.render(html)

    assert message =~ "classic script elements"
    assert message =~ "only non-executable data blocks"
  end

  test "render returns detailed invalid selector diagnostics" do
    html = """
    <style>
    p > { color: red; }
    </style>
    <p>Hello</p>
    """

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 2,
               source: "p >",
               message: message
             }}} = HtmlToPdf.render(html)

    assert message == ~s(line 2: selector "p >" is invalid or unsupported)
  end

  test "render returns detailed inline CSS diagnostics" do
    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 1,
               source: "display: table-row-group",
               message: message
             }}} = HtmlToPdf.render(~s(<p style="display: table-row-group">Bad</p>))

    assert message == ~s(line 1: declaration "display: table-row-group" is invalid or unsupported)
  end

  test "render returns detailed layout option diagnostics" do
    assert {:error,
            {:invalid_page_size,
             %{
               stage: :layout,
               reason: :invalid_page_size,
               message: "layout failed: invalid_page_size"
             }}} = HtmlToPdf.render("<p>Hello</p>", page_size: {0, 100})

    assert {:error,
            {:invalid_margin,
             %{
               stage: :layout,
               reason: :invalid_margin,
               message:
                 "layout failed: margin must be non-negative and leave a positive printable area"
             }}} = HtmlToPdf.render("<p>Hello</p>", margin: -1)

    assert {:error,
            {:invalid_margin,
             %{
               stage: :layout,
               reason: :invalid_margin,
               message:
                 "layout failed: margin must be non-negative and leave a positive printable area"
             }}} = HtmlToPdf.render("<p>Hello</p>", page_size: {100, 100}, margin: 50)

    assert {:error, {:invalid_margin, %{stage: :layout, reason: :invalid_margin}}} =
             HtmlToPdf.render("<p>Hello</p>", page_size: {100, 100}, margin: 60)
  end

  test "render asserts detailed failure shapes for public error categories" do
    assert {:error,
            {:invalid_html,
             %{
               stage: :html,
               reason: :invalid_html,
               message: "HTML input must be a string"
             }}} = HtmlToPdf.render(:not_html)

    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               line: 1,
               column: 1,
               source: "<video>",
               message: ~s(line 1: HTML tag "<video>" is unsupported)
             }}} = HtmlToPdf.render("<video></video>")

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 1,
               column: 1,
               source: "color",
               message: ~s(line 1: declaration "color" is invalid or unsupported)
             }}} = HtmlToPdf.render(~s(<p style="color">Bad CSS</p>))

    bad_background_html = """
    <style>p { background: linear-gradient(red, blue); }</style>
    <p>Bad CSS value</p>
    """

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 1,
               column: 5,
               source: "background: linear-gradient(red, blue)",
               message:
                 ~S|line 1: declaration "background: linear-gradient(red, blue)" is invalid or unsupported|
             }}} =
             HtmlToPdf.render(bad_background_html)

    for invalid_stylesheets <- [
          [:not_css],
          ["p { color: red; }"],
          [{:css, 123}],
          [{:file, 123}],
          :not_a_list
        ] do
      assert {:error,
              {:invalid_options,
               %{
                 stage: :options,
                 reason: :invalid_options,
                 message:
                   "stylesheets option must be a list of {:css, css} or {:file, path} tuples"
               }}} = HtmlToPdf.render("<p>Hello</p>", stylesheets: invalid_stylesheets)
    end

    missing_stylesheet =
      Path.join(System.tmp_dir!(), "native-elixir-pdf-missing-stylesheet.css")

    File.rm(missing_stylesheet)

    assert {:error,
            {:invalid_document,
             %{
               stage: :style,
               reason: :invalid_document,
               message: "configured stylesheet file could not be read"
             }}} =
             HtmlToPdf.render("<p>Hello</p>",
               stylesheets: [{:file, missing_stylesheet}]
             )

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               message: "render options must be a keyword list"
             }}} = HtmlToPdf.render("<p>Hello</p>", [:not_options])

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               message: "render options contain unsupported keys: [:margni]"
             }}} = HtmlToPdf.render("<p>Hello</p>", margni: 10)

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               operation: :render,
               message: "compress_streams must be a boolean"
             }}} = HtmlToPdf.render("<p>Hello</p>", compress_streams: :auto)

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               operation: :render,
               message: "subset_fonts must be a boolean"
             }}} = HtmlToPdf.render("<p>Hello</p>", subset_fonts: :auto)

    assert {:error,
            {:invalid_path,
             %{
               stage: :file,
               reason: :invalid_path,
               operation: :render_file,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               message: "input and output paths must be strings"
             }}} = HtmlToPdf.render_file(:not_a_path, "/tmp/native-elixir-pdf-failure.pdf")

    missing_input = "/tmp/native-elixir-pdf-missing-input.html"

    assert {:error,
            {:enoent,
             %{
               stage: :file,
               reason: :enoent,
               operation: :read,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               source: ^missing_input,
               message: "file read failed: enoent"
             }}} =
             HtmlToPdf.render_file(missing_input, "/tmp/native-elixir-pdf-failure.pdf")
  end

  test "render identifies an invalid declaration after a semicolon-containing URL" do
    html =
      ~s|<p style="background-image: url(data:image/png;base64,AAAA); display: table-row-group">Bad display</p>|

    assert {:error,
            {:invalid_css,
             %{
               stage: :css,
               reason: :invalid_css,
               line: 1,
               source: "display: table-row-group",
               message:
                 ~s(line 1: declaration "display: table-row-group" is invalid or unsupported)
             }}} = HtmlToPdf.render(html)
  end

  test "render converts tables to PDF text boxes and cell borders" do
    html =
      ~s(<style>th,td{border:1pt solid black;padding:4pt}th{background:#eee}</style><table><caption>Summary</caption><thead><tr><th>Name</th><th>Docs</th></tr></thead><tbody><tr><td>Alpha</td><td><a href="https://example.com">Link</a></td></tr></tbody></table>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert_pdf_text(pdf, ["Summary", "Name", "Docs", "Alpha", "Link"])
    assert pdf =~ ~r|/BaseFont /[A-Z]{6}\+DejaVuSans|
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    streams =
      for {ref, %{stream: raw}} <- document.objects, is_binary(raw) do
        assert {:ok, stream} =
                 NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(document, {:ref, ref})

        stream
      end

    assert Enum.any?(streams, &String.contains?(&1, "0.9333 0.9333 0.9333 rg"))
    assert Enum.any?(streams, &String.contains?(&1, "0.75 0 0 0.75 0 0 cm 0 0 0 RG 1 w"))
    assert pdf =~ "/FontFile2"
    assert {:ok, ^pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Link"
    assert pdf =~ "/URI (https://example.com)"
  end

  test "render paints a collapsed table background image once" do
    source = "data:image/png;base64,#{png_fixture_base64()}"

    html = """
    <table style="width: 40pt; border-collapse: collapse; border: 1pt solid black; background-image: url(#{source}); background-size: 10pt 10pt; background-repeat: no-repeat">
      <tr><td style="padding: 4pt; border: 1pt solid black">Cell</td></tr>
    </table>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert length(Regex.scan(~r/\/Im\d+ Do/, pdf)) == 1
  end

  test "render supports fixed column groups and separate border spacing" do
    html = """
    <table style="width: 240pt; table-layout: fixed; border-collapse: separate; border-spacing: 8pt 4pt">
      <colgroup><col span="2" style="width: 25%"><col style="width: 50%"></colgroup>
      <tbody>
        <tr><th scope="col">Code</th><th scope="col">Qty</th><th scope="col">Description</th></tr>
        <tr><td colspan="2">A-100 / 12</td><td>Trim hardware</td></tr>
      </tbody>
    </table>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert_pdf_text(pdf, ["Code", "Description", "Trim hardware"])
  end

  test "render paginates overflowing content into multiple PDF pages" do
    rows =
      1..3
      |> Enum.map(fn index ->
        "<tr><td>Alpha #{index}</td><td>#{index}</td></tr>"
      end)
      |> Enum.join()

    html =
      "<style>th,td{padding:4pt}</style><table><thead><tr><th>Name</th><th>Count</th></tr></thead><tbody>" <>
        rows <> "</tbody></table>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 10)
    assert pdf =~ "/Type /Pages"
    assert pdf =~ "/Count 2"
    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    assert length(Regex.scan(~r/Name/, extracted)) == 2
    assert extracted =~ "Alpha 3"
  end

  test "render keeps root-positioned content on the first page" do
    html = """
    <div style="height: 20pt; page-break-after: always">First page</div>
    <div style="height: 20pt">Second page</div>
    <div style="position: absolute; left: 0; top: 0; width: 80pt; height: 10pt">Root marker</div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {100, 100}, margin: 0)
    assert {:ok, %{page_count: 2, pages: [first_page, second_page]}} = Text.extract_spans(pdf)

    assert Enum.map_join(first_page.spans, & &1.text) == "First pageRoot marker"
    assert Enum.map_join(second_page.spans, & &1.text) == "Second page"
  end

  test "render fragments a paragraph taller than the printable page" do
    lines =
      1..13
      |> Enum.map_join("<br>", fn index ->
        "Paragraph line #{String.pad_leading(Integer.to_string(index), 2, "0")}"
      end)

    html = """
    <p style="font-size: 10pt; line-height: 12pt; margin: 0">#{lines}</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 10)
    assert pdf =~ "/Count 3"

    for index <- 1..13 do
      label = "Paragraph line #{String.pad_leading(Integer.to_string(index), 2, "0")}"
      assert_pdf_text(pdf, label)
    end
  end

  test "render adds opt-in running page furniture without changing default rendering" do
    html = """
    <p>First body page</p>
    <div style="page-break-after: always"></div>
    <p>Second body page</p>
    <div style="page-break-after: always"></div>
    <p>Third body page</p>
    """

    assert {:ok, default_pdf} =
             HtmlToPdf.render(html, page_size: {200, 100}, margin: 20)

    refute_pdf_text(default_pdf, ["Report header", "Page 1 of 3"])

    assert {:ok, furnished_pdf} =
             HtmlToPdf.render(html,
               page_size: {200, 100},
               margin: 20,
               page_furniture: [
                 header: [
                   default: "<div style=\"font-size: 8pt\">Report header</div>",
                   first: false,
                   odd: "<div style=\"font-size: 8pt\">Odd header</div>",
                   even: "<div style=\"font-size: 8pt\">Even header</div>"
                 ],
                 footer:
                   "<div style=\"font-size: 8pt; text-align: right\">Page {{page}} of {{pages}}</div>"
               ]
             )

    assert furnished_pdf =~ "/Count 3"
    refute_pdf_text(furnished_pdf, "Report header")

    assert_pdf_text(furnished_pdf, [
      "Odd header",
      "Even header",
      "Page 1 of 3",
      "Page 2 of 3",
      "Page 3 of 3"
    ])
  end

  test "render shares the image budget across repeated page furniture" do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    Limits.install(Map.put(original_limits, :max_image_count, 1))

    image = "data:image/png;base64,#{Base.encode64(png_fixture(1, 1))}"

    html = """
    <p>First body page</p>
    <div style="page-break-after: always"></div>
    <p>Second body page</p>
    """

    assert {:error,
            {:resource_limit_exceeded,
             %{
               stage: :limits,
               reason: :resource_limit_exceeded,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               message: "image count exceeds the limit"
             }}} =
             HtmlToPdf.render(html,
               page_size: {200, 100},
               margin: 20,
               page_furniture: [
                 header: ~s(<img src="#{image}" style="width: 1pt; height: 1pt">)
               ]
             )
  end

  test "cached PNG placements still consume aggregate decoded-image budget" do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    image = "data:image/png;base64,#{Base.encode64(png_fixture(1, 1))}"
    html = ~s(<img src="#{image}"><img src="#{image}">)

    Limits.install(%{
      original_limits
      | max_decoded_image_bytes: 3,
        max_aggregate_decoded_image_bytes: 6
    })

    assert {:ok, _pdf} = HtmlToPdf.render(html)

    Limits.install(%{
      original_limits
      | max_decoded_image_bytes: 3,
        max_aggregate_decoded_image_bytes: 5
    })

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :limits, reason: :resource_limit_exceeded, message: message}}} =
             HtmlToPdf.render(html)

    assert message =~ "aggregate decoded image bytes"
  end

  test "repeated SVG placements keep raster dimensions and aggregate limits" do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)

    svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"><rect width="1" height="1" fill="red"/></svg>)

    source = "data:image/svg+xml;base64,#{Base.encode64(svg)}"
    repeated = ~s(<img src="#{source}"><img src="#{source}">)

    raster_bytes = 4 * Limits.get(:svg_raster_scale) ** 2

    Limits.install(%{
      original_limits
      | max_decoded_image_bytes: raster_bytes,
        max_aggregate_decoded_image_bytes: 2 * raster_bytes
    })

    assert {:ok, _pdf} = HtmlToPdf.render(repeated)

    Limits.install(%{
      original_limits
      | max_decoded_image_bytes: raster_bytes,
        max_aggregate_decoded_image_bytes: 2 * raster_bytes - 1
    })

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :limits, reason: :resource_limit_exceeded, message: message}}} =
             HtmlToPdf.render(repeated)

    assert message =~ "aggregate decoded image bytes"
  end

  test "render reports invalid page furniture through the shared diagnostics contract" do
    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               operation: :decorate_pages,
               module: NativeElixirPdfUtilities.HtmlToPdf.PageFurniture,
               message: "page_furniture contains unsupported keys: [:watermark]"
             }}} =
             HtmlToPdf.render("<p>Hello</p>",
               page_furniture: [watermark: "Draft"]
             )

    assert {:error,
            {:invalid_layout,
             %{
               stage: :layout,
               reason: :invalid_layout,
               operation: :decorate_pages,
               module: NativeElixirPdfUtilities.HtmlToPdf.PageFurniture,
               message: "footer page furniture height 14.25pt exceeds the 0.0pt page margin"
             }}} =
             HtmlToPdf.render("<p>Hello</p>", page_furniture: [footer: "Page {{page}}"])
  end

  test "page furniture renders a supported flex list header" do
    assert {:ok, pdf} =
             HtmlToPdf.render("<p>Hello</p>",
               page_size: {200, 100},
               margin: 20,
               page_furniture: [
                 header: ~s(<div style="display: flex"><ul><li>Invalid layout</li></ul></div>)
               ]
             )

    assert_pdf_text(pdf, ["Hello", "Invalid layout"])
  end

  test "render honors an empty manual page break element" do
    html = ~s(<p>First</p><div style="page-break-after: always"></div><p>Second</p>)

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 10)
    assert pdf =~ "/Count 2"
    assert_pdf_text(pdf, ["First", "Second"])

    html = ~s(<p>First</p><div style="page-break-before: always"></div><p>Second</p>)

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {200, 100}, margin: 10)
    assert pdf =~ "/Count 2"
    assert_pdf_text(pdf, ["First", "Second"])
  end

  test "render converts a flex layout subset to PDF text boxes" do
    html =
      ~s(<div style="display: flex; width: 80pt; gap: 8pt"><span style="order: 2">Second</span><span style="order: 1">First</span></div>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert_pdf_text(pdf, ["First", "Second"])
  end

  test "render converts row flex items with block children and percentage widths" do
    html = """
    <html>
      <head>
        <style>
          .header { display: flex; }
          .address-section {
            display: flex;
            width: 100%;
            justify-content: space-between;
            align-items: center;
            flex-wrap: wrap;
          }
        </style>
      </head>
      <body>
        <div class="header">
          <div style="width: 80%"><p>Left</p></div>
          <div style="width: 20%"><p>Right</p></div>
        </div>
        <div class="address-section">
          <div class="section"><h4>Supplier Address</h4><p>Supplier</p></div>
          <div class="section"><h4>Buyer Address</h4><p>Buyer</p></div>
          <div class="section"><h4>Consignee Address</h4><p>Consignee</p></div>
        </div>
      </body>
    </html>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: :a4)
    assert_pdf_text(pdf, ["Left", "Right", "Supplier Address", "Buyer Address", "Consignee"])
  end

  test "render converts a grid layout subset to PDF text boxes" do
    html =
      ~s(<div style="display: grid; width: 80pt; grid-template-columns: 30pt 30pt; gap: 8pt"><span style="grid-column: 2 / 3">Second</span><span style="grid-column: 1 / 2">First</span></div>)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert_pdf_text(pdf, ["First", "Second"])
  end

  test "render rejects excessive grid and table cardinalities before layout" do
    excessive_documents = [
      ~s|<div style="display: grid; grid-template-columns: repeat(1000000000, 1pt)">Grid</div>|,
      ~s|<div style="display: grid; grid-template-columns: repeat(600, 1pt) repeat(600, 1pt)">Grid</div>|,
      ~s|<div style="display: grid; grid-column: span 1001">Grid</div>|,
      ~s|<table><tr><td rowspan="1001">Cell</td></tr></table>|
    ]

    for html <- excessive_documents do
      assert {:error,
              {:resource_limit_exceeded,
               %{
                 stage: :limits,
                 reason: :resource_limit_exceeded,
                 operation: :render,
                 module: NativeElixirPdfUtilities.HtmlToPdf,
                 message: message
               }}} = HtmlToPdf.render(html)

      assert message =~ "1000-item limit"
    end

    for html <- [
          ~s|<table><tr><td colspan="1001">Cell</td></tr></table>|,
          ~s|<table><colgroup><col span="1001"></colgroup><tr><td>Cell</td></tr></table>|,
          ~s|<table><colgroup><col span="many"></colgroup><tr><td>Cell</td></tr></table>|
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert_pdf_text(pdf, "Cell")
    end
  end

  test "render converts a PNG data URI image to a PDF image object" do
    src = "data:image/png;base64,#{Base.encode64(png_fixture(2, 1))}"
    html = ~s(<img src="#{src}" style="width: 20pt">)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"
    assert pdf =~ "/Width 2"
    assert pdf =~ "/Height 1"
    assert pdf =~ "/ColorSpace /DeviceRGB"
    assert pdf =~ "/Filter /FlateDecode"
    assert pdf =~ "/Im1 Do"
  end

  test "render confines document-selected images and fonts to base_url" do
    fixture_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-resource-confinement")
    resource_root = Path.join(fixture_dir, "root")
    inside_image = Path.join(resource_root, "inside.png")
    outside_image = Path.join(fixture_dir, "outside.png")
    outside_font = Path.join(fixture_dir, "outside.ttf")
    linked_image = Path.join(resource_root, "linked.png")
    linked_font = Path.join(resource_root, "linked.ttf")
    File.mkdir_p!(resource_root)
    File.write!(inside_image, png_fixture(1, 1))
    File.write!(outside_image, png_fixture(1, 1))
    File.cp!(ttf_font_path!(), outside_font)
    :ok = File.ln_s(outside_image, linked_image)
    :ok = File.ln_s(outside_font, linked_font)

    assert {:ok, authorized_pdf} =
             HtmlToPdf.render(~s|<img src="#{inside_image}">|, base_url: resource_root)

    assert authorized_pdf =~ "/Subtype /Image"

    for source <- [inside_image, outside_image, "../outside.png", "linked.png"] do
      opts = if source == inside_image, do: [], else: [base_url: resource_root]

      assert {:error,
              {:invalid_document,
               %{
                 stage: stage,
                 reason: :invalid_document,
                 operation: :render,
                 module: NativeElixirPdfUtilities.HtmlToPdf
               }}} = HtmlToPdf.render(~s|<img src="#{source}">|, opts)

      assert stage in [:asset, :file]
    end

    for {source, opts} <- [
          {outside_font, []},
          {outside_font, [base_url: resource_root]},
          {"../outside.ttf", [base_url: resource_root]},
          {"linked.ttf", [base_url: resource_root]}
        ] do
      html = """
      <style>
        @font-face { font-family: Confined; src: url("#{source}"); }
        p { font-family: Confined; }
      </style>
      <p>Confined font</p>
      """

      assert {:error, {:invalid_document, %{stage: stage}}} =
               HtmlToPdf.render(html, opts)

      assert stage in [:asset, :file]
    end

    assert {:ok, trusted_pdf} =
             HtmlToPdf.render("<p>Trusted font</p>",
               fonts: [%{family: "Trusted", path: outside_font}],
               default_font: "Trusted"
             )

    assert trusted_pdf =~ "/Subtype /Type0"
  after
    File.rm_rf(Path.join(System.tmp_dir!(), "native-elixir-pdf-resource-confinement"))
  end

  test "render consumes explicit and resolver-provided asset bytes" do
    png = png_fixture(2, 1)

    assert {:ok, explicit_pdf} =
             HtmlToPdf.render(
               ~s(<img src="asset:logo" style="width: 20pt; height: 20pt; object-fit: contain">),
               assets: %{"asset:logo" => {:bytes, png}}
             )

    assert explicit_pdf =~ "/Subtype /Image"

    resolver = fn
      %{reference: "https://example.com/paper.png", kind: :background_image} -> {:ok, png}
      _request -> :not_found
    end

    assert {:ok, resolver_pdf} =
             HtmlToPdf.render(
               ~s|<div style="width: 30pt; height: 20pt; background-image: url('https://example.com/paper.png'); background-size: cover; background-repeat: no-repeat"></div>|,
               asset_resolver: resolver
             )

    assert resolver_pdf =~ "/Subtype /Image"

    assert {:error,
            {:invalid_document, %{stage: :asset, source: "https://example.com/paper.png"}}} =
             HtmlToPdf.render(
               ~s|<div style="background-image: url('https://example.com/paper.png')"></div>|
             )
  end

  test "render does not load a background image reset by a later shorthand" do
    html = """
    <div style="width: 30pt; height: 20pt; background-image: url('https://example.com/stale.png'); background-size: cover; background-position: right bottom; background-repeat: no-repeat; background: red"></div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    refute pdf =~ "/Subtype /Image"
  end

  test "render supports min() background image sizes" do
    source = "data:image/png;base64,#{png_fixture_base64()}"

    html = """
    <div style="width: 40pt; height: 20pt; background-image: url(#{source}); background-size: min(10pt,50%); background-repeat: no-repeat"></div>
    <div style="width: 40pt; height: 20pt; background-image: url(#{source}); background-size: min(10pt, 50%); background-repeat: no-repeat"></div>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"
  end

  test "render omits background images with a zero-sized tile" do
    source = "data:image/png;base64,#{png_fixture_base64()}"

    for background_size <- ["0 0", "0 auto", "auto 0"] do
      html = """
      <div style="width: 40pt; height: 20pt; background: red; background-image: url(#{source}); background-size: #{background_size}; background-repeat: repeat"></div>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html)
      refute pdf =~ "/Subtype /Image"
    end
  end

  test "render rejects aggregate decoded PNG bytes before inflating the excess image" do
    src = "data:image/png;base64,#{Base.encode64(png_fixture(2_000, 2_000))}"
    image = ~s(<img src="#{src}" style="width: 20pt">)

    assert {:error,
            {:resource_limit_exceeded,
             %{
               stage: :limits,
               reason: :resource_limit_exceeded,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               message: "aggregate decoded image bytes exceed the limit"
             }}} = HtmlToPdf.render(String.duplicate(image, 7))
  end

  test "render rejects excess image sources during style traversal" do
    src = "data:image/png;base64,#{Base.encode64(png_fixture(1, 1))}"
    image = ~s(<img src="#{src}" style="width: 1pt">)

    assert {:error,
            {:resource_limit_exceeded,
             %{
               stage: :limits,
               reason: :resource_limit_exceeded,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               message: "image count exceeds the limit"
             }}} = HtmlToPdf.render(String.duplicate(image, 1_001))
  end

  test "render rasterizes an SVG data URI image to a PDF image object" do
    svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="2" height="1"><rect width="2" height="1" fill="red"/></svg>)

    src = "data:image/svg+xml;base64,#{Base.encode64(svg)}"
    html = ~s(<img src="#{src}" style="width: 20pt">)

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert pdf =~ "/Subtype /Image"
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    [image] =
      for {_ref, %{value: %{"Subtype" => {:name, "Image"}} = value}} <- document.objects,
          do: value

    assert image["Width"] == ceil(20 / 0.75 * Limits.get(:svg_raster_scale))
    assert image["Height"] == ceil(10 / 0.75 * Limits.get(:svg_raster_scale))
    assert pdf =~ "/ColorSpace /DeviceRGB"
    assert pdf =~ "/Filter /FlateDecode"
  end

  test "render rejects SVG resources before the native resolver can read local files" do
    directory =
      Path.join(System.tmp_dir!(), "svg-authorization-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    image_path = Path.join(directory, "private.png")
    File.write!(image_path, png_fixture(1, 1))

    nested_svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"><image href="#{image_path}" width="1" height="1"/></svg>)

    nested_path = Path.join(directory, "nested.svg")
    File.write!(nested_path, nested_svg)

    for reference <- [
          image_path,
          "../#{Path.basename(directory)}/private.png",
          "file://#{image_path}",
          nested_path,
          "data:image/svg+xml;base64,#{Base.encode64(nested_svg)}",
          "data:image/png;base64,#{Base.encode64(png_fixture(1, 1))}",
          String.replace(image_path, "/", "&#47;"),
          "&#35;internal"
        ],
        attribute <- ["href", "xlink:href"] do
      svg =
        ~s(<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="1" height="1"><image #{attribute}='#{reference}' width="1" height="1"/></svg>)

      src = "data:image/svg+xml;base64,#{Base.encode64(svg)}"

      for html <- [
            ~s(<img src="#{src}">),
            ~s|<div style="width:1px;height:1px;background-image:url('#{src}')"></div>|
          ] do
        assert {:error,
                {:invalid_document,
                 %{
                   stage: :style,
                   reason: :invalid_document,
                   operation: :render,
                   module: NativeElixirPdfUtilities.HtmlToPdf,
                   message: message
                 }}} = HtmlToPdf.render(html)

        assert message =~ "SVG resource references are not authorized"
      end
    end

    for separator <- ["<?xml version='1.0'?>", "<!DOCTYPE svg>"] do
      svg =
        ~s(<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1"><image hr#{separator}ef="#{image_path}" width="1" height="1"/></svg>)

      src = "data:image/svg+xml;base64,#{Base.encode64(svg)}"

      assert {:error, {:invalid_document, %{operation: :render, reason: :invalid_document}}} =
               HtmlToPdf.render(~s(<img src="#{src}">))
    end
  end

  test "render rejects SVG raster dimensions over the native allocation budget" do
    svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="2" height="1"><rect width="2" height="1" fill="red"/></svg>)

    src = "data:image/svg+xml;base64,#{Base.encode64(svg)}"
    html = ~s(<img src="#{src}" style="width: 8193px; aspect-ratio: 1">)

    assert {:error,
            {:resource_limit_exceeded,
             %{
               stage: :limits,
               reason: :resource_limit_exceeded,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               message: message
             }}} = HtmlToPdf.render(html)

    assert message =~ "8192-pixel per-axis limit"

    intrinsic_svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="4294967295" height="1"><rect width="1" height="1" fill="red"/></svg>)

    intrinsic_src = "data:image/svg+xml;base64,#{Base.encode64(intrinsic_svg)}"

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlToPdf.render(~s(<img src="#{intrinsic_src}">))
  end

  test "render embeds configured TTF fonts for Unicode text" do
    html = ~s(<p style="font-family: 'Fixture Sans', Helvetica">Café</p>)

    assert {:ok, pdf} =
             HtmlToPdf.render(html, fonts: [%{family: "Fixture Sans", path: ttf_font_path!()}])

    assert pdf =~ "/Subtype /Type0"
    assert pdf =~ "/Subtype /CIDFontType2"
    assert pdf =~ "/FontFile2"
    assert pdf =~ "/ToUnicode"
    assert pdf =~ "<"
    refute pdf =~ "(Café) Tj"
  end

  test "render uses bundled DejaVu for unstyled Unicode text without corrupting glyphs" do
    assert {:ok, pdf} = HtmlToPdf.render("<p>café © α €</p>")

    assert pdf =~ "/Subtype /Type0"
    assert pdf =~ "/FontFile2"
    assert pdf =~ "/ToUnicode"

    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    assert extracted =~ "é"
    assert extracted =~ "©"
    assert extracted =~ "α"
    assert extracted =~ "€"
  end

  test "render rejects invalid UTF-8 and replaces unavailable glyphs by default" do
    assert {:error,
            {:invalid_encoding,
             %{
               stage: :html,
               reason: :invalid_encoding,
               operation: :render,
               module: HtmlToPdf,
               message: "HTML input must be valid UTF-8"
             }}} = HtmlToPdf.render(<<"<p>", 255, "</p>">>)

    assert {:ok, pdf} = HtmlToPdf.render("<p>can\u0092t 漢</p>")
    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    assert String.graphemes(extracted) |> Enum.count(&(&1 == "\uFFFD")) == 2

    assert {:error,
            {:unsupported_glyph,
             %{
               stage: :font,
               reason: :unsupported_glyph,
               operation: :resolve_fonts,
               module: NativeElixirPdfUtilities.HtmlToPdf.FontFallback,
               source: "漢"
             }}} = HtmlToPdf.render("<p>漢</p>", unsupported_glyphs: :error)

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               operation: :render,
               module: HtmlToPdf,
               message: "unsupported_glyphs must be :replace or :error"
             }}} = HtmlToPdf.render("<p>漢</p>", unsupported_glyphs: :ignore)
  end

  test "render_file writes a PDF for a supported paragraph" do
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.html")
    output_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.pdf")

    File.write!(input_path, "<p>Hello</p>")

    assert HtmlToPdf.render_file(input_path, output_path) == :ok
    assert_pdf_text(File.read!(output_path), "Hello")

    assert {:error, {:enoent, %{stage: :file, reason: :enoent, operation: :read}}} =
             HtmlToPdf.render_file(input_path <> ".missing", output_path)

    assert {:error,
            {:invalid_path, %{stage: :file, reason: :invalid_path, operation: :render_file}}} =
             HtmlToPdf.render_file(:bad_input, output_path)
  after
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.html")
    output_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.pdf")

    File.rm(input_path)
    File.rm(output_path)
  end

  test "render_file returns diagnostics for render and write failures" do
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-bad-test.html")
    File.write!(input_path, "<video></video>")

    assert {:error,
            {:unsupported_html,
             %{
               stage: :html,
               reason: :unsupported_html,
               operation: :render,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               source: "<video>",
               message: ~s(line 1: HTML tag "<video>" is unsupported)
             }}} = HtmlToPdf.render_file(input_path, input_path <> ".pdf")

    File.write!(input_path, "<p>Hello</p>")
    output_path = System.tmp_dir!()

    assert {:error,
            {reason,
             %{
               stage: :file,
               reason: reason,
               operation: :write,
               module: NativeElixirPdfUtilities.HtmlToPdf,
               source: ^output_path
             }}} = HtmlToPdf.render_file(input_path, output_path)
  after
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-bad-test.html")
    File.rm(input_path)
    File.rm(input_path <> ".pdf")
  end

  defp png_fixture(width, height) do
    row = :binary.copy(<<255, 0, 0>>, width)
    rows = Enum.map_join(1..height, "", fn _index -> <<0>> <> row end)

    <<137, 80, 78, 71, 13, 10, 26, 10>> <>
      png_chunk("IHDR", <<width::32, height::32, 8, 2, 0, 0, 0>>) <>
      png_chunk("IDAT", :zlib.compress(rows)) <>
      png_chunk("IEND", "")
  end

  defp png_fixture_base64 do
    1
    |> png_fixture(1)
    |> Base.encode64()
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

  defp assert_pdf_text(pdf, expected) do
    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    Enum.each(List.wrap(expected), &assert(extracted =~ &1))
  end

  defp assert_stroked_rectangle(pdf, expected) do
    assert pdf =~ "0.75 0 0 0.75 0 0 cm"

    assert [_, x, y, width, height] =
             Regex.run(
               ~r/(-?\d+(?:\.\d+)?) (-?\d+(?:\.\d+)?) (-?\d+(?:\.\d+)?) (-?\d+(?:\.\d+)?) re S/,
               pdf
             )

    actual = Enum.map([x, y, width, height], &(&1 |> Float.parse() |> elem(0) |> Kernel.*(0.75)))

    Enum.zip(actual, Tuple.to_list(expected))
    |> Enum.each(fn {value, target} -> assert_in_delta value, target, 0.01 end)
  end

  defp refute_pdf_text(pdf, expected) do
    assert {:ok, extracted} = Text.extract(pdf, layout: false)
    Enum.each(List.wrap(expected), &refute(extracted =~ &1))
  end

  defp png_chunk(type, data) do
    crc = :erlang.crc32(type <> data)
    <<byte_size(data)::32, type::binary, data::binary, crc::32>>
  end

  test "readonly static artwork matches editable controls with all values retained" do
    for tag <- ["input", "textarea"],
        value <- ["", "Printed", "A&#10;B"],
        disabled <- ["", "disabled"] do
      element =
        if tag == "input",
          do: ~s(<input readonly #{disabled} name="field" value="#{value}">),
          else: ~s(<textarea readonly #{disabled} name="field">#{value}</textarea>)

      assert {:ok, readonly} = HtmlToPdf.render(element, forms: :static)

      assert {:ok, editable} =
               HtmlToPdf.render(String.replace(element, "readonly ", ""), forms: :static)

      assert readonly == editable
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(readonly)
    end
  end

  test "readonly on inapplicable input states preserves complete effective PDFs" do
    for type <- ["hidden", "checkbox", "radio"], forms <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(~s(<input type="#{type}" readonly>), forms: forms)
      assert {:ok, reference} = HtmlToPdf.render(~s(<input type="#{type}">), forms: forms)
      assert pdf == reference
    end
  end

  test "telephone URL and email preserve exact text field artwork and initial values" do
    for type <- ["tel", "TEL", "url", "URL", "email", "EmAiL"],
        forms <- [:static, :interactive],
        {value, expected} <- [
          {"A&#13;B&#10;C", "ABC"},
          {" &#9;A B&#12; ", if(String.downcase(type) == "tel", do: " \tA B\f ", else: "A B")},
          {"&#13;&#10;", ""},
          {"&#160;AB&#160;", "\u00A0AB\u00A0"},
          {"not a valid URL or email", "not a valid URL or email"}
        ] do
      html = ~s(<input type="#{type}" name="contact" value="#{value}">)
      encoded = expected |> String.replace("\t", "&#9;") |> String.replace("\f", "&#12;")
      reference = ~s(<input type="text" name="contact" value="#{encoded}">)
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: forms)
      assert {:ok, equivalent} = HtmlToPdf.render(reference, forms: forms)
      assert pdf == equivalent

      if forms == :interactive do
        assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
        assert field.type == :text and field.value == expected and not field.read_only
      else
        assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
      end
    end

    for type <- ["tel", "url", "email"] do
      assert {:ok, pdf} = HtmlToPdf.render(~s(<input type="#{type}">))
      assert {:ok, [%{name: "TEXT_1_1", value: ""}]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    end
  end

  test "telephone URL and email inactive attributes preserve their complete PDFs" do
    for type <- ["tel", "url", "email"],
        attribute <- if(type == "email", do: ["checked"], else: ["checked", "multiple"]),
        forms <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render(~s(<input type="#{type}" #{attribute}>), forms: forms)
      assert {:ok, reference} = HtmlToPdf.render(~s(<input type="#{type}">), forms: forms)
      assert pdf == reference
    end
  end

  test "telephone URL and email patterns preserve empty initial field values" do
    for type <- ["tel", "url", "email"], forms <- [:static, :interactive] do
      html = ~s(<input type="#{type}" pattern='x'>)
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: forms)
      assert {:ok, reference} = HtmlToPdf.render(~s(<input type="#{type}">), forms: forms)
      assert pdf == reference

      if forms == :interactive do
        assert {:ok, [%{type: :text, value: ""}]} = NativeElixirPdfUtilities.Forms.fields(pdf)
      else
        assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
      end
    end
  end

  test "multiple email initial lists match exact sanitized text PDFs" do
    for value <- [" a@example.com ,&#9; b@example.com ", " , , ", " &#9;&#10; "],
        forms <- [:static, :interactive] do
      expected =
        case value do
          " a@example.com ,&#9; b@example.com " -> "a@example.com,b@example.com"
          " , , " -> ",,"
          _ -> ""
        end

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 ~s(<input type="email" multiple="false" name="contacts" value="#{value}">),
                 forms: forms
               )

      assert {:ok, reference} =
               HtmlToPdf.render(~s(<input type="text" name="contacts" value="#{expected}">),
                 forms: forms
               )

      assert pdf == reference

      if forms == :interactive do
        assert {:ok, [%{value: ^expected}]} = NativeElixirPdfUtilities.Forms.fields(pdf)
      end
    end

    for html <- [
          "<textarea multiple>A</textarea>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert {:ok, reference} = HtmlToPdf.render(String.replace(html, " multiple", ""))
      assert pdf == reference
    end
  end

  test "missing empty and unknown input types render exact text-default PDFs" do
    for type <- ["", " ", "unknown", "hidden ", "checKbox", "ſearch", "datetime"],
        forms <- [:static, :interactive],
        value <- ["A&#13;B&#10;C", ""] do
      assert {:ok, pdf} =
               HtmlToPdf.render(~s(<input type="#{type}" name="fallback" value="#{value}">),
                 forms: forms
               )

      assert {:ok, reference} =
               HtmlToPdf.render(~s(<input name="fallback" value="#{value}">), forms: forms)

      assert pdf == reference
    end

    for type <- ["unknown", "hidden ", "checKbox"], forms <- [:static, :interactive] do
      html =
        ~s|<style>input[type="#{type}"] { color:blue; }</style><input type="#{type}" name="fallback" value="Printed">|

      assert {:ok, pdf} = HtmlToPdf.render(html, forms: forms)

      assert {:ok, reference} =
               HtmlToPdf.render(~s(<input name="fallback" value="Printed" style="color:blue">),
                 forms: forms
               )

      assert pdf == reference
    end
  end

  test "special initial controls do not fetch authored scalar or file values" do
    owner = self()

    resolver = fn source ->
      send(owner, {:unexpected_input_asset, source})
      {:error, :not_found}
    end

    for forms <- [:static, :interactive] do
      assert {:ok, _} =
               HtmlToPdf.render("<input type=file hidden value='/path/not-to-read'>",
                 forms: forms,
                 asset_resolver: resolver
               )

      if forms == :static do
        assert {:ok, pdf} =
                 HtmlToPdf.render("<input type=FILE value='/path/not-to-read'>",
                   forms: forms,
                   asset_resolver: resolver
                 )

        assert {:ok, text} = NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
        assert text =~ "Choose File" and text =~ "No file chosen"
        refute text =~ "/path/not-to-read"
        assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
      else
        assert {:error, {:unsupported_form, diagnostic}} =
                 HtmlToPdf.render("<input type=FILE value='/path/not-to-read'>",
                   forms: forms,
                   asset_resolver: resolver
                 )

        assert diagnostic.stage == :forms and diagnostic.operation == :compute
        assert diagnostic.source == "input/file" and diagnostic.message =~ "forms: :static"
      end
    end

    assert {:ok, _} =
             HtmlToPdf.render("<input type=color value='/path/not-to-read'>",
               forms: :static,
               asset_resolver: resolver
             )

    assert {:error, {:unsupported_form, diagnostic}} =
             HtmlToPdf.render("<input type=color value='/path/not-to-read'>",
               asset_resolver: resolver
             )

    assert diagnostic.stage == :forms and diagnostic.source == "color"
    refute_received {:unexpected_input_asset, _}
  end

  test "button inputs match equivalent button content and preserve raw selectors" do
    for {type, default} <- [{"button", ""}, {"reset", "Reset"}, {"submit", "Submit"}],
        value <- [nil, "", "Custom"],
        forms <- [:static, :interactive] do
      attribute = if value == nil, do: "", else: ~s( value="#{value}")
      label = if value == nil, do: default, else: value

      assert {:ok, pdf} =
               HtmlToPdf.render(~s(<input type="#{String.upcase(type)}"#{attribute}>),
                 forms: forms
               )

      reference_html =
        if label == "", do: ~s(<input type="button" value="">), else: "<button>#{label}</button>"

      assert {:ok, reference} = HtmlToPdf.render(reference_html, forms: forms)
      assert pdf == reference
    end

    for forms <- [:static, :interactive] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 ~s(<style>input[type=submit][disabled] { color:blue; } input[value] { color:red; }</style><input type="SUBMIT" disabled="false">),
                 forms: forms
               )

      assert {:ok, reference} =
               HtmlToPdf.render(~s(<button style="color:blue">Submit</button>), forms: forms)

      assert pdf == reference
    end

    owner = self()

    resolver = fn source ->
      send(owner, {:unexpected_button_asset, source})
      :not_found
    end

    for type <- ["button", "reset", "submit"],
        attribute <- ["readonly", "checked", "multiple", "src='denied.png'"] do
      html = ~s(<input type="#{type}" #{attribute}>)

      assert {:ok, pdf} = HtmlToPdf.render(html, asset_resolver: resolver)
      assert {:ok, reference} = HtmlToPdf.render(~s(<input type="#{type}">))
      assert pdf == reference
    end

    refute_received {:unexpected_button_asset, _}
  end

  test "button labels preserve whitespace and paginate without creating fields" do
    alias NativeElixirPdfUtilities.Forms

    css =
      "input,button { display:block;width:100pt;font-size:10pt;line-height:15pt;padding:1pt;border:1pt solid black;margin:0 0 5pt; }"

    inputs = Enum.map_join(1..8, fn i -> ~s(<input type="submit" value="Label #{i}">) end)
    buttons = Enum.map_join(1..8, fn i -> "<button>Label #{i}</button>" end)
    opts = [page_size: {160, 80}, margin: 8]
    assert {:ok, pdf} = HtmlToPdf.render("<style>#{css}</style>" <> inputs, opts)
    assert {:ok, reference} = HtmlToPdf.render("<style>#{css}</style>" <> buttons, opts)
    assert pdf == reference
    assert {:ok, []} = Forms.fields(pdf)
    assert {:ok, text} = Text.extract(pdf, layout: false)
    for i <- 1..8, do: assert(text =~ "Label #{i}")
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    assert length(context.pages) > 1

    for forms <- [:static, :interactive] do
      assert {:ok, pdf} =
               HtmlToPdf.render(~s(<input type="button" value=" A  B&#10;C ">), forms: forms)

      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert text =~ "A  B" and text =~ "C"
      assert {:ok, []} = Forms.fields(pdf)
    end
  end

  test "initial control state selector PDFs match authored equivalents in both form modes" do
    controls =
      ~s(<input name="a" value="Enabled"><input name="b" disabled="false" value="Disabled"><textarea name="notes" readonly>Readonly</textarea><button>Enabled button</button>)

    css =
      "input:enabled,textarea:enabled,button:not(:disabled) { color:blue; } input:disabled { color:red; }"

    equivalent = "input,textarea,button { color:blue; } [disabled] { color:red; }"

    for forms <- [:static, :interactive] do
      assert {:ok, pdf} = HtmlToPdf.render("<style>#{css}</style>" <> controls, forms: forms)

      assert {:ok, reference} =
               HtmlToPdf.render("<style>#{equivalent}</style>" <> controls, forms: forms)

      assert pdf == reference

      if forms == :interactive do
        assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(pdf)

        assert Enum.map(fields, &{&1.name, &1.read_only}) == [
                 {"a", false},
                 {"b", true},
                 {"notes", true}
               ]
      end
    end

    for selector <- ["input:enabledness", "input:diſabled", "input:disabled()"] do
      html = "<style>#{selector} { color:red; }</style><input value='Initial'>"
      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :css and diagnostic.operation == :render
      assert diagnostic.message != "" and diagnostic.reason == :invalid_css
    end
  end

  test "hidden initial control states cannot reveal content or request background assets" do
    owner = self()

    resolver = fn source ->
      send(owner, {:unexpected_state_asset, source})
      :not_found
    end

    for disabled <- ["", "disabled"] do
      html =
        ~s|<style>input:enabled,input:disabled { display:block !important; background-image:url('denied.png'); }</style><p>Visible</p><input type="hidden" #{disabled} value="Secret">|

      assert {:ok, pdf} = HtmlToPdf.render(html, asset_resolver: resolver)
      assert {:ok, reference} = HtmlToPdf.render("<p>Visible</p>")
      assert pdf == reference
    end

    refute_received {:unexpected_state_asset, _}
  end

  test "image dimension hints match exact CSS equivalents including zero extents" do
    png =
      NativeElixirPdfUtilities.TestSupport.PngFixture.build(8, 4, 2, 8, 0, fn _, _ ->
        [20, 80, 160]
      end)

    opts = [assets: %{"tile" => {:bytes, png}}, page_size: {120, 120}, margin: 0]

    for {attributes, css} <- [
          {~s(width="24"), "width:18pt"},
          {~s(height="12"), "height:9pt"},
          {~s(width="24" height="48"), "width:18pt;height:36pt"},
          {~s(width="24.5%junk"), "width:24.5%"},
          {~s(width="+10" height="bad"), ""},
          {~s(width="0"), "width:0"},
          {~s(width="0" height="12"), "width:0;height:9pt"},
          {~s(width="12" height="0"), "width:9pt;height:0"}
        ],
        parent <- ["", "display:flex", "display:grid"],
        fit <- ["fill", "cover", "contain"] do
      html =
        "<div style='#{parent}'><img src='tile' #{attributes} style='object-fit:#{fit}'></div><p>After image</p>"

      reference =
        "<div style='#{parent}'><img src='tile' style='#{css};object-fit:#{fit}'></div><p>After image</p>"

      assert {:ok, pdf} = HtmlToPdf.render(html, opts)
      assert {:ok, expected} = HtmlToPdf.render(reference, opts)
      assert pdf == expected
      assert {:ok, text} = Text.extract(pdf, layout: false)
      assert String.trim(text) == "After image"
    end

    assert {:ok, pdf} =
             HtmlToPdf.render(
               ~s(<img src="tile" width="24" height="48" style="width:30pt;height:auto">),
               opts
             )

    assert {:ok, reference} = HtmlToPdf.render(~s(<img src="tile" style="width:30pt">), opts)
    assert pdf == reference
  end

  test "image dimension resource failures precede asset resolution and zero sizes preserve strict asset errors" do
    owner = self()

    resolver = fn source ->
      send(owner, {:dimension_asset, source})
      :not_found
    end

    for value <- [
          "1000000001",
          String.duplicate("0", Limits.get(:max_html_numeric_token_bytes) + 1)
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(~s(<img src="denied" width="#{value}">), asset_resolver: resolver)

      assert diagnostic.stage == :style and diagnostic.operation == :render
      assert diagnostic.source == "width=" <> value
      assert diagnostic.message =~ "max_"
    end

    refute_received {:dimension_asset, _}

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(~s(<img src="denied" width="0">), asset_resolver: resolver)

    assert diagnostic.stage == :asset and diagnostic.operation == :render and
             diagnostic.source == "denied"

    assert_received {:dimension_asset, %{reference: "denied", kind: :image}}
  end

  test "invalid dimension hints retain asset failures and invalid author sizes retain CSS diagnostics" do
    owner = self()

    resolver = fn request ->
      send(owner, {:invalid_dimension_asset, request})
      :not_found
    end

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(~s(<img src="denied" width="bad">), asset_resolver: resolver)

    assert diagnostic.stage == :asset and diagnostic.operation == :render and
             diagnostic.source == "denied"

    assert_received {:invalid_dimension_asset, %{reference: "denied", kind: :image}}

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(~s(<img src="denied" height="1000000001">),
               asset_resolver: resolver
             )

    assert diagnostic.stage == :style and diagnostic.source == "height=1000000001"
    refute_received {:invalid_dimension_asset, _}

    assert {:error, {:invalid_css, diagnostic}} =
             HtmlToPdf.render(~s(<img src="tile" width="24" style="height:-1pt">),
               assets: %{
                 "tile" =>
                   {:bytes,
                    NativeElixirPdfUtilities.TestSupport.PngFixture.build(8, 4, 2, 8, 0, fn _,
                                                                                            _ ->
                      [20, 80, 160]
                    end)}
               },
               asset_resolver: resolver
             )

    assert diagnostic.stage == :css and diagnostic.operation == :render
    assert diagnostic.source == "height:-1pt" and is_integer(diagnostic.line)
    refute_received {:invalid_dimension_asset, _}
  end

  test "password static artwork matches complete bullet text references" do
    for value <- ["", "ABC", " Aé🦊 ", "A&#13;B&#10;C"],
        parent <- ["", "display:flex", "display:grid"] do
      expected =
        case value do
          "" -> ""
          "ABC" -> "•••"
          " Aé🦊 " -> "•••••"
          "A&#13;B&#10;C" -> "•••"
        end

      html =
        "<div style='#{parent}'><input type='PASSWORD' value='#{value}'></div><p>After password</p>"

      reference = "<div style='#{parent}'><input value='#{expected}'></div><p>After password</p>"
      assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static)
      assert {:ok, equivalent} = HtmlToPdf.render(reference, forms: :static)
      assert pdf == equivalent
    end
  end

  test "password display byte limits fail with an actionable style diagnostic" do
    saved = Limits.effective()
    on_exit(fn -> Limits.install(saved) end)
    Limits.install(Map.put(saved, :max_rendered_text_bytes, 8))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(~s(<input type="password" value="ABC">), forms: :static)

    assert diagnostic.stage == :style and diagnostic.operation == :render
    assert diagnostic.message =~ "max_rendered_text_bytes"
    refute diagnostic.message =~ "ABC"
  end

  test "internal destinations render and invalid base structure returns actionable diagnostics" do
    for html <- [
          "<a href='#later'>Jump</a><p id=later>Target</p>",
          "<head><base href=''></head><body><a href='#'>Top</a></body>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert pdf =~ "/Dest ["
      assert pdf =~ "/XYZ"
      refute pdf =~ "/S /URI"
    end

    for html <- ["<head><base></head><body>Text</body>", "<body><base href='/'>Text</body>"] do
      assert {:error, {_reason, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.reason in [:invalid_document, :unsupported_html]
      assert is_binary(diagnostic.message) and diagnostic.operation in [:render, :parse]
    end

    for href <- ["javascript:run()", "java&#10;script:run()", "vbscript:run()"] do
      assert {:error, {:unsupported_html, %{stage: :html, source: "href"}}} =
               HtmlToPdf.render("<head><base href='#{href}'></head><body>Text</body>")
    end
  end

  test "invalid editing selectors retain public diagnostics" do
    for selector <- [":read-only()", ":read-write(1)", ":read-onlyness", ":read-wrıte"] do
      html = "<style>p#{selector}{color:red}</style><p contenteditable>Visible</p>"
      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
      assert diagnostic.operation == :render and is_binary(diagnostic.message)
      assert diagnostic.source =~ selector
    end
  end

  test "long fieldset groups preserve every paragraph across page boundaries" do
    paragraphs = Enum.map_join(1..20, fn index -> "<p>Line #{index}</p>" end)

    html =
      "<style>html,body{margin:0;padding:0;font-size:10pt;line-height:12pt}p{margin:0}</style><fieldset><legend>Caption</legend>#{paragraphs}</fieldset><p>After</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {150, 120}, margin: 10, forms: :static)
    assert {:ok, 3} = NativeElixirPdfUtilities.Info.page_count(pdf)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Caption" and text =~ "After"
    for index <- 1..20, do: assert(text =~ "Line #{index}")
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
  end

  test "mixed effective radio disabling is diagnosed for interactive fields and preserved in static artwork" do
    html =
      "<fieldset disabled><legend>Caption<input type=radio name=mode checked value=A></legend><input type=radio name=mode checked value=B></fieldset>"

    assert {:error, {:unsupported_form, diagnostic}} = HtmlToPdf.render(html, forms: :interactive)
    assert diagnostic.stage == :forms and diagnostic.source == "mode"

    assert diagnostic.message =~ "mixes enabled and disabled" and
             diagnostic.message =~ "forms: :static"

    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Caption"
  end

  test "fieldset inheritance controls PDF flags without changing authored values or static content" do
    html = """
    <fieldset disabled=false><legend>Group <input name=toggle required value=Editable></legend>
    <input name=locked required value=Locked><textarea name=notes required>Notes</textarea>
    <select name=choice required><option selected value=A>Choice</option></select>
    <fieldset><legend>Nested <input name=caption required value=Caption></legend>
    <input name=body required value=Body></fieldset></fieldset>
    <input name=outside required value=Outside>
    <template><fieldset disabled><legend>Unused</legend><input name=unused><img src=denied.png></fieldset></template>
    """

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 forms: mode,
                 asset_resolver: fn source -> flunk("unused resource read: #{source}") end
               )

      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
      assert {:ok, flat} = NativeElixirPdfUtilities.Forms.flatten(pdf)
      assert {:ok, text} = Text.extract(flat)

      for caption <- ~w(Group Editable Locked Notes Choice Nested Caption Body Outside),
          do: assert(text =~ caption)

      refute text =~ "Unused"
      assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

      assert {:ok, %{fields: fields}} =
               NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

      if mode == :interactive do
        fields = Map.new(fields, &{&1.name, &1})
        assert map_size(fields) == 7
        assert fields["toggle"].flags == 2 and fields["outside"].flags == 2
        assert fields["locked"].flags == 1 and fields["locked"].value == "Locked"
        assert fields["notes"].flags == 4097 and fields["notes"].value == "Notes"
        assert fields["choice"].flags == 131_073 and fields["choice"].value == "A"
        assert fields["caption"].flags == 1 and fields["body"].flags == 1
      else
        assert fields == []
      end
    end

    for html <- [
          "<fieldset><legend onclick='go()'>Caption</legend></fieldset>",
          "<template><fieldset><script>alert(1)</script></fieldset></template>",
          "<fieldset disabled><img src=denied.png></fieldset>"
        ] do
      assert {:error, {reason, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.reason == reason and diagnostic.stage in [:html, :asset]
      assert diagnostic.message != ""
    end
  end

  test "disabled fieldset radios retain the last initial selection and PDF read-only flags" do
    html =
      "<form><fieldset disabled><legend>Radio group</legend><input type=radio name=mode value=A checked><input type=radio name=mode value=B checked></fieldset></form>"

    assert {:ok, pdf} = HtmlToPdf.render(html)
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)

    assert {:ok, %{fields: [field]}} =
             NativeElixirPdfUtilities.Validators.FormValidator.inspect_document(context)

    assert field.flags == 32769 and field.value == "B"
    assert {:ok, [summary]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert summary.export_values == ["A", "B"]

    for mode <- [:static, :interactive] do
      assert {:ok, pdf} =
               HtmlToPdf.render(
                 "<fieldset><legend></legend></fieldset><fieldset></fieldset><fieldset style='padding:0;border:0;width:0;height:0'><legend style='font-size:0;line-height:0;padding:0'></legend></fieldset>",
                 forms: mode
               )

      assert {:ok, 1} = NativeElixirPdfUtilities.Info.page_count(pdf)
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    end
  end

  test "invalid editing selectors retain public diagnostics and fieldsets render" do
    for selector <- [":read-only()", ":read-write(1)", ":read-onlyness", ":read-wrıte"] do
      html = "<style>p#{selector}{color:red}</style><p contenteditable>Visible</p>"
      assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :css and diagnostic.reason == :invalid_css
      assert diagnostic.operation == :render and is_binary(diagnostic.message)
      assert diagnostic.source =~ selector
    end

    assert {:ok, pdf} = HtmlToPdf.render("<fieldset disabled><input></fieldset>")
    assert pdf =~ "%PDF-"
    assert {:ok, [field]} = NativeElixirPdfUtilities.Forms.fields(pdf)
    assert field.read_only
  end

  test "internal destinations resolve and invalid base structure returns actionable diagnostics" do
    for html <- [
          "<a href='#later'>Jump</a><p id=later>Target</p>",
          "<head><base href=''></head><body><a href='#'>Top</a></body>"
        ] do
      assert {:ok, pdf} = HtmlToPdf.render(html)
      assert pdf =~ "/Dest [" and pdf =~ "/XYZ"
    end

    for html <- ["<head><base></head><body>Text</body>", "<body><base href='/'>Text</body>"] do
      assert {:error, {_reason, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.reason in [:invalid_document, :unsupported_html]
      assert is_binary(diagnostic.message) and diagnostic.operation in [:render, :parse]
    end

    for href <- ["javascript:run()", "java&#10;script:run()", "vbscript:run()"] do
      assert {:error, {:unsupported_html, %{stage: :html, source: "href"}}} =
               HtmlToPdf.render("<head><base href='#{href}'></head><body>Text</body>")
    end
  end
end
