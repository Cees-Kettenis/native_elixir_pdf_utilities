defmodule NativeElixirPdfUtilities.HtmlToPdf.StylesheetSelectionTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.{
    HtmlParser,
    Style,
    RenderCache,
    EmbeddedViewport,
    ResourceContext
  }

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    HtmlMetadataValidator,
    PrintMediaValidator
  }

  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}

  test "responsive sizes and reusable print predicates use the validated environment" do
    alias NativeElixirPdfUtilities.Validators.PrintMediaValidator

    assert {:ok, context} =
             PrintMediaValidator.context(print_viewport: {800, 600}, image_selection_density: 2)

    for query <- [
          "",
          "all",
          "PRINT",
          "only print",
          "print /*note*/ and (width: 800px)",
          "(400px < width <= 800px)",
          "(1000px >= width > 400px)",
          "(800px = width)",
          "screen, print",
          "print and (min-width: 600px)",
          "not screen and (width: 900px)",
          "(width >= 800px)",
          "(height < 700px)",
          "(height <= 600px)",
          "(width > 700px)",
          "(width = 800px)",
          "(orientation: landscape)",
          "(aspect-ratio: 4/3)",
          "(min-resolution: 192dpi)",
          "(resolution: 2dppx)",
          "(resolution: 2x)",
          "(max-resolution: 76dpcm)",
          "((min-width: 1000px) or (height: 600px))",
          "not (width: 900px)",
          "(width)",
          "(color)"
        ] do
      assert PrintMediaValidator.matches?(query, context), query
    end

    for query <- [
          "screen",
          "speech",
          "unknown-medium",
          "not print nonsense",
          "only (width: 800px)",
          "(400px < width > 800px)",
          "(bad < width < 800px)",
          "(bad = width)",
          "print and (min-width: 900px)",
          "(unknown: yes)",
          "(unknown)",
          "(orientation: portrait)",
          "(resolution: rubbish)",
          "(width: rubbish)",
          "(aspect-ratio: 1/0)",
          "(nonsense > 4px)",
          "not print and (width: 800px)"
        ] do
      refute PrintMediaValidator.matches?(query, context), query
    end

    assert {:ok, portrait} = PrintMediaValidator.context(print_viewport: {600, 800})
    assert PrintMediaValidator.matches?("(orientation: portrait)", portrait)
    assert {:ok, page} = PrintMediaValidator.context(page_size: {300, 150}, margin: 15)
    assert page.width == 360 and page.height == 160 and page.density == 1
    assert {:ok, default} = PrintMediaValidator.context([])
    assert default.width > 790 and default.height > 1100

    for opts <- [
          [print_viewport: :bad],
          [print_viewport: {0, 1}],
          [print_viewport: {"a", 1}],
          [image_selection_density: 0],
          [image_selection_density: :bad],
          [image_selection_density: -1]
        ] do
      assert {:error, {:invalid_options, diagnostic}} = PrintMediaValidator.context(opts)
      assert diagnostic.stage == :options and diagnostic.message =~ "positive"
    end

    for opts <- [[page_size: :bad], [page_size: {100, 100}, margin: 60], [margin: -1]] do
      assert {:error, {:invalid_options, diagnostic}} = PrintMediaValidator.context(opts)
      assert diagnostic.message =~ "page area"
    end

    for {length, expected} <- [
          {"1in", 96},
          {"1pc", 16},
          {"3pt", 4},
          {"2em", 32},
          {"2rem", 32},
          {"2.54cm", 96},
          {"25.4mm", 96},
          {"101.6q", 96},
          {"50vh", 300},
          {"50dvw", 400},
          {"50vi", 400},
          {"50svh", 300},
          {"50vb", 300},
          {"50lvmin", 300},
          {"50dvmax", 400},
          {"50vmin", 300},
          {"50vmax", 400},
          {"0px", 0},
          {"0", 0},
          {"calc(100vw - 20px - 10px)", 770},
          {"min(50vw, 200px)", 200},
          {"max(20px, 40px)", 40},
          {"clamp(20px, 50vw, 200px)", 200},
          {"calc((100vw - 20px) / 2)", 390},
          {"calc(2*20px + 10px*3)", 70}
        ] do
      assert {:ok, value} = PrintMediaValidator.source_length(length, context)
      assert_in_delta value, expected, 0.000001
    end

    for length <- [
          "2",
          "-1px",
          "20%",
          "calc(1px + 2)",
          "calc(1px*2px)",
          "calc(1px,2px)",
          "clamp(1px,2px)",
          "min()",
          "min(bad,2px)",
          "calc(+ 1px)",
          "1px + 2px",
          "calc(1px +)",
          "url(file)"
        ] do
      assert PrintMediaValidator.source_length(length, context) == :error, length
    end
  end

  test "infinite media arithmetic diagnoses rather than silently matching" do
    alias NativeElixirPdfUtilities.HtmlToPdf

    for sizes <- ["calc(1px/1e-320)", "calc(1000000000px*1000000000)"] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(
                 "<style>@media (min-width:#{sizes}){p{color: red}}</style><p>Visible</p>"
               )

      assert diagnostic.stage == :style and diagnostic.message != ""
    end
  end

  test "style type, media, preferred set and source order select before font resources" do
    html = """
    <head><style type='text/css; charset=utf-8'>INVALID CSS</style>
    <style type=' text/css'>INVALID CSS</style><style type='text/plain'>INVALID CSS</style>
    <style title='First' media='screen'>p{color: red}</style>
    <style title='Second'>INVALID CSS</style>
    <style>p{color: blue}</style><style title='First' type='TEXT/CSS'>p{color: green}</style>
    <style disabled media='print and (min-width: 100px)'>p{color: red}</style>
    <style media='screen'>@font-face{font-family:Denied;src: url(https://denied.test/font.ttf)}</style>
    </head><body title='Parent'><p>Chosen</p><style>p{color: blue}</style></body>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)

    assert {:ok, tree} =
             Style.compute_detailed(dom,
               print_viewport: {200, 100},
               stylesheets: [{:css, "p{color: red}"}]
             )

    [body] = tree.children
    [p] = Enum.filter(body.children, &(&1.type == :element))
    assert p.style.color == {0, 0, 1}
    [head, _] = dom.children
    assert Enum.at(head.children, 7).attributes["disabled"] == ""

    assert {:ok, pdf} = HtmlToPdf.render(html, print_viewport: {200, 100})
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Chosen"
    refute text =~ "INVALID"
  end

  test "default-style meta changes titled sets case sensitively in tree order" do
    for {meta, expected} <- [
          {"", {1, 0, 0}},
          {"<meta http-equiv=default-style content=''>", {1, 0, 0}},
          {"<meta http-equiv=DEFAULT-STYLE content='Second'>", {0, 0, 1}},
          {"<meta http-equiv=default-style content='second'>", {0, 0, 0}},
          {"<meta http-equiv=default-style content='Second'><meta http-equiv=default-style content='First'>",
           {1, 0, 0}}
        ] do
      html =
        "<head><style title=First>p{color: red}</style><style title=Second>p{color: blue}</style>#{meta}</head><body><p>Set</p></body>"

      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [body]}} = Style.compute_detailed(dom)
      assert hd(body.children).style.color == expected
    end
  end

  test "template, foreign and other logical scopes never contribute document styles" do
    style = %{
      type: :element,
      tag: "style",
      attributes: %{},
      children: [%{type: :text, text: "p{color: red}"}]
    }

    nodes = [
      %{style | attributes: %{"type" => "text/plain", "title" => "Excluded"}},
      %{style | attributes: %{"media" => "print"}},
      Map.put(style, :namespace, :svg),
      Map.merge(style, %{document_id: :other, tree_scope_id: :other}),
      %{type: :element, tag: "template", attributes: %{}, children: [style]},
      %{
        type: :element,
        tag: "div",
        attributes: %{},
        children: [%{type: :text, text: "Content"}, style]
      }
    ]

    nodes = [Map.merge(hd(nodes), %{document_id: :root, tree_scope_id: :root}) | tl(nodes)]
    assert HtmlMetadataValidator.selected_style_nodes(nodes, nil) == [Enum.at(nodes, 1), style]

    namespaced =
      Map.put(style, :attribute_namespaces, %{"type" => "urn: foreign"})
      |> Map.put(:attributes, %{"type" => "text/plain"})

    assert [%{attributes: %{}}] = HtmlMetadataValidator.selected_style_nodes([namespaced], nil)
  end

  test "media blocks nest and preserve source order, strings and escaped markers" do
    css =
      ~S|p{color: red} @media print and (width: 200px){@media (height: 100px){p{color: blue}} @media screen{p{color: red}}} p{font-weight: bold}|

    assert {:ok, context} = PrintMediaValidator.context(print_viewport: {200, 100})

    assert {:ok, prepared} =
             NativeElixirPdfUtilities.HtmlToPdf.CssParser.prepare_stylesheet(css, context)

    assert length(prepared.rules) == 3

    assert Enum.map(prepared.rules, & &1.declarations) == [
             [{"color", "red"}],
             [{"color", "blue"}],
             [{"font-weight", "bold"}]
           ]

    assert {:ok, stripped} =
             PrintMediaValidator.select_rules(
               ~S|p{content:"@media print{}"} @media screen{p{color: red}}|,
               context
             )

    assert stripped == ~S|p{content:"@media print{}"} |

    for css <- [
          "@media {p{color:red}}",
          "@media print{p{color: red}",
          "@media print; p{color: red}",
          "@media bad;@media print{p{color: red}}"
        ] do
      assert {:error, {:invalid_css, diagnostic}} =
               HtmlToPdf.render("<style>#{css}</style><p>Visible</p>")

      assert diagnostic.stage == :css and diagnostic.message != ""
    end
  end

  test "stylesheet caches keep viewport, document and selected rules separate" do
    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<style>@media (width: 100px){p{color: red}} @media (width: 200px){p{color: blue}}</style><p>Cache</p>"
             )

    RenderCache.with_stylesheets(fn ->
      for {width, expected} <- [{100, {1, 0, 0}}, {200, {0, 0, 1}}, {100, {1, 0, 0}}] do
        assert {:ok, %{children: [p]}} = Style.compute_detailed(dom, print_viewport: {width, 100})
        assert p.style.color == expected
      end
    end)
  end

  test "effective page defaults and explicit options precede media-selected asset I/O" do
    html =
      "<style>@page{size: 150pt 90pt;margin: 0} @media (width: 200px){.bad{display: none}} @media (width: 300px){.good{display: none}} p{margin: 0}</style><p class=good>Effective</p><p class=bad><img src=missing.png></p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, system_font_discovery: false, print_viewport: {200, 120})

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Effective"

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(html, print_viewport: {300, 120})

    assert diagnostic.stage == :asset
    assert {:error, {:invalid_document, _}} = HtmlToPdf.render(html, page_size: {225, 90})

    for opts <- [
          [print_viewport: {0, 100}],
          [image_selection_density: 0],
          [print_viewport: :bad],
          [__client_css_viewport__: %{width: 0, height: 0}]
        ] do
      assert {:error, {:invalid_options, diagnostic}} = HtmlToPdf.render("<p>Input</p>", opts)
      assert diagnostic.stage == :options and diagnostic.message != ""
    end
  end

  test "root and child media selection excludes unauthorized inactive fonts and images" do
    child =
      "<style>body{margin: 0;overflow: hidden} .inactive{display: none} @media (max-width: 130px){.chosen{display: none}} @media (min-width: 131px){.chosen{display: block}} @media (min-width: 131px){@font-face{font-family:Denied;src: url(https://denied.test/font.ttf)}}</style><p>Child</p><div class=chosen><img src=missing.png></div>"

    html =
      "<iframe style='border: 0;width: 120px;height: 60px' srcdoc='#{String.replace(child, "'", "&#39;")}'></iframe><p>Parent</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html, forms: :static)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Child" and text =~ "Parent"
    html = String.replace(html, "width: 120px", "width: 150px")
    assert {:error, {:resource_denied, diagnostic}} = HtmlToPdf.render(html, forms: :static)
    assert diagnostic.stage == :resource and diagnostic.source =~ "denied.test"
  end

  test "scrollbar restyling changes selected rules and preserves nested issued documents" do
    leaf = "<body style=\"margin: 0;overflow: hidden\"><p>Leaf</p></body>"

    child =
      "<style>body{margin: 0}p{margin: 0;color: red} @media (max-width: 110px){p{color: blue}}</style><div style='height: 100px'><p>Client</p><iframe style='border: 0;width: 30px;height: 20px' srcdoc='#{leaf}'></iframe></div>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document({:srcdoc, child}, parent)
      assert {:ok, first} = EmbeddedViewport.style_viewport(prepared, 90, 45)
      [body] = first.tree.children
      [div] = body.children
      [p, iframe] = Enum.filter(div.children, &(&1.type == :element))
      assert p.style.color == {1, 0, 0}
      first_leaf = iframe.style.embedded_document.context
      assert {:ok, second} = EmbeddedViewport.style_viewport(first, 78.75, 45)
      [body] = second.tree.children
      [div] = body.children
      [p, iframe] = Enum.filter(div.children, &(&1.type == :element))
      assert p.style.color == {0, 0, 1}
      assert iframe.style.embedded_document.context == first_leaf
      assert first.context == second.context and first.prepared_dom == second.prepared_dom
      assert {:ok, group} = EmbeddedViewport.layout(prepared, 90, 45)
      assert hd(group.boxes).client_rect.width == 78.75
      texts = Enum.filter(hd(group.boxes).boxes, &(&1.type == :text and &1.text == "Client"))
      assert [%{color: {1, 0, 0}}] = texts
    end)
  end

  test "zero child media dimensions never use a page fallback or divide by zero" do
    assert {:ok, context} = PrintMediaValidator.client_context([], {0, 0})
    assert PrintMediaValidator.matches?("(width: 0px) and (height: 0px)", context)
    refute PrintMediaValidator.matches?("(width)", context)
    refute PrintMediaValidator.matches?("(height)", context)
    refute PrintMediaValidator.matches?("(aspect-ratio)", context)
    refute PrintMediaValidator.matches?("(aspect-ratio: 1/1)", context)
    assert PrintMediaValidator.matches?("(orientation: portrait)", context)
    assert {:error, {:invalid_layout, _}} = PrintMediaValidator.client_context([], {-1, 0})

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      child =
        "<style>body{margin: 0;overflow: hidden} @media (width: 0px){div{display: none}}</style><div><img src=missing.png></div>"

      assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document({:srcdoc, child}, parent)
      assert {:ok, styled} = EmbeddedViewport.style_viewport(prepared, 0, 0)
      assert styled.client_css_viewport == %{width: 0.0, height: 0.0}
      assert {:ok, group} = EmbeddedViewport.layout(styled, 0, 0)
      assert group.clip.width == 0 and group.clip.height == 0

      assert {:error, {:not_implemented, diagnostic}} =
               EmbeddedViewport.prepare_html({:srcdoc, child}, parent)

      assert diagnostic.stage == :style and diagnostic.message =~ "explicit client viewport"
    end)
  end

  test "stylesheet media depth and source work have centralized diagnostic budgets" do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(Map.put(original, :max_media_query_depth, 2))
    css = "@media print{@media print{@media print{p{color: red}}}}"

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<style>#{css}</style><p>Bounded</p>")

    assert diagnostic.stage == :css and diagnostic.message =~ "max_media_query_depth"
    Limits.install(Map.put(original, :max_aggregate_css_source_bytes, 20))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<style>p{color: red} p{color: blue}</style><p>Bounded</p>")

    assert diagnostic.stage == :css and diagnostic.message =~ "max_aggregate_css_source_bytes"
  end

  test "page defaults prepare before context and viewport-qualified page negotiation diagnoses" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<style>@page{size: 1000pt 1000pt}p{margin: 0}</style><p>Large page</p>",
               margin: 320
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Large page"

    for html <- [
          "<style>@media (width: 200px){@page{size: 150pt 100pt}}</style><p>Page</p>",
          "<style media='(width: 200px)'>@page{margin: 10pt}</style><p>Page</p>"
        ] do
      assert {:error, {:not_implemented, diagnostic}} = HtmlToPdf.render(html)
      assert diagnostic.stage == :style and diagnostic.message =~ "page/media negotiation"
    end

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, eager} =
               EmbeddedViewport.prepare_iframe(
                 {:srcdoc, "<body style='margin: 0'><div style='height: 100px'></div></body>"},
                 parent
               )

      assert {:ok, group} = EmbeddedViewport.layout(eager, 90, 45)
      assert hd(group.boxes).client_rect.width == 78.75
    end)
  end

  test "root stylesheet dimensions describe the requested page box before CSS size and margins" do
    assert {:ok, context} =
             PrintMediaValidator.renderer_context(page_size: {300, 150}, margin: 15)

    assert context.width == 400 and context.height == 200

    html =
      "<style>@page{size: 150pt 90pt;margin: 10pt}p{color: black} @media (width: 400px) and (height: 200px){p{color: blue}} @media (width: 360px){p{color: red}} @media (width: 200px){p{color: green}}</style><p>Media box</p>"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, page_size: {300, 150}, margin: 15, compress_streams: false)

    assert pdf =~ "0 0 1 rg"
    refute pdf =~ "1 0 0 rg"

    assert {:ok, pdf} =
             HtmlToPdf.render(html, print_viewport: {400, 200}, compress_streams: false)

    assert pdf =~ "/MediaBox [0 0 150 90]"
    assert pdf =~ "0 0 1 rg"
    assert {:ok, pdf} = HtmlToPdf.render(html, compress_streams: false)
    assert pdf =~ "/MediaBox [0 0 150 90]"
    refute pdf =~ "0 0 1 rg"
    refute pdf =~ "0 0.502 rg"
  end

  test "outer iframe media dimensions include gutters while layout retains the client" do
    child =
      "<style>body{margin: 0}p{margin: 0;color: black} @media (width: 120px){p{color: blue}} @media (width: 105px){p{color: red}}</style><div style='height: 100px'><p>Outer</p></div>"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document({:srcdoc, child}, parent)
      assert {:ok, styled} = EmbeddedViewport.style_viewport(prepared, 78.75, 45, {90, 45})
      assert styled.client_css_viewport == %{width: 105.0, height: 60.0}
      assert styled.media_css_viewport == %{width: 120.0, height: 60.0}
      [body] = styled.tree.children
      assert hd(hd(body.children).children).style.color == {0, 0, 1}
      assert {:ok, group} = EmbeddedViewport.layout(styled, 90, 45)
      assert hd(group.boxes).client_rect.width == 78.75

      assert [%{color: {0, 0, 1}}] =
               Enum.filter(hd(group.boxes).boxes, &(Map.get(&1, :text) == "Outer"))

      for media <- [{30, 45}, {90, -1}, :bad, false] do
        assert {:error, {:invalid_layout, diagnostic}} =
                 EmbeddedViewport.style_viewport(prepared, 78.75, 45, media)

        assert diagnostic.stage == :layout and diagnostic.message != ""
      end

      assert {:ok, dense} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc,
                  "<html><style>@media (resolution: 2dppx){html{color: blue}}</style><body><p>Density</p></body></html>"},
                 parent,
                 image_selection_density: 2
               )

      assert dense.options[:image_selection_density] == 2
      assert {:ok, styled} = EmbeddedViewport.style_viewport(dense, 90, 45)
      assert hd(styled.tree.children).style.color == {0, 0, 1}
    end)
  end

  test "legacy child style viewport arity and CSS font failure paths remain recoverable" do
    alias NativeElixirPdfUtilities.HtmlToPdf.CssParser
    assert CssParser.font_faces("@media print") == {:error, :invalid_css}

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc, "<p>Legacy viewport</p>"},
                 parent
               )

      ResourceContext.with_document(prepared.context, fn ->
        assert {:ok, tree} =
                 Style.compute_child_detailed(
                   prepared.prepared_dom,
                   prepared.options,
                   prepared.context.image_budget,
                   prepared.document_url,
                   {90, 45}
                 )

        assert tree.type == :document
      end)
    end)
  end

  test "restyling a media-selected font family cannot reuse another viewport's face" do
    root = Path.expand(".")
    sans = Path.join(root, "priv/fonts/dejavu/DejaVuSans.ttf")
    mono = Path.join(root, "priv/fonts/dejavu/DejaVuSansMono.ttf")

    child =
      "<style>@media (width:120px){@font-face{font-family:MediaFace;src:url(#{sans})}} @media (width:150px){@font-face{font-family:MediaFace;src:url(#{mono})}}</style><body style='margin:0;font-family:MediaFace'><p>Selected font</p></body>"

    ResourceContext.run([base_url: root], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} = EmbeddedViewport.prepare_iframe_document({:srcdoc, child}, parent)

      for {width, file} <- [{90, sans}, {112.5, mono}, {90, sans}] do
        assert {:ok, styled} = EmbeddedViewport.style_viewport(prepared, width, 45)
        [body] = styled.tree.children
        assert body.style.font_face.data == File.read!(file)
        assert styled.context == prepared.context

        ResourceContext.with_document(prepared.context, fn ->
          assert {:ok, direct} =
                   Style.compute_child_detailed(
                     prepared.prepared_dom,
                     prepared.options,
                     prepared.context.image_budget,
                     prepared.document_url,
                     {width, 45},
                     {width, 45}
                   )

          [body] = direct.children
          assert body.style.font_face.data == File.read!(file)
        end)
      end
    end)
  end

  test "media dimensions fail before configured child font resource work" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn parent ->
      assert {:ok, prepared} =
               EmbeddedViewport.prepare_iframe_document(
                 {:srcdoc, "<p>Validate viewport first</p>"},
                 parent,
                 fonts: [%{family: "Configured", path: "../denied.ttf"}]
               )

      for media <- [false, :invalid, {0, 0}, {-1, 45}] do
        assert {:error, {:invalid_layout, diagnostic}} =
                 EmbeddedViewport.style_viewport(prepared, 90, 45, media)

        assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
        assert diagnostic.message != ""
      end

      assert {:error, {:resource_denied, diagnostic}} =
               EmbeddedViewport.style_viewport(prepared, 90, 45)

      assert diagnostic.stage == :resource and diagnostic.source == "../denied.ttf"
    end)
  end
end
