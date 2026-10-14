defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateExecutionTest do
  use ExUnit.Case, async: false
  Code.require_file("../support/png_fixture.ex", __DIR__)
  alias NativeElixirPdfUtilities.TestSupport.PngFixture
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, TemplateExecutionValidator}

  @opts [system_font_discovery: false, compress_streams: false]

  test "real renderer replaces an earlier range and preserves later ordinary source" do
    html =
      "<div><?start name='A'><p>Removed original</p><?end?></div><template for='A'><p>Inserted actual</p></template><p>After template</p>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Inserted actual" and text =~ "After template"
    refute text =~ "Removed original"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "successive replacement preserves lexical source ownership and real earlier placements" do
    html =
      "<body><div><?start name='A'><p>Old A</p><?end?></div><section><?start name='B'><p>Old B</p><?end?></section><template for='A'><p>New A</p></template><template for='B'><p>New B</p></template><p>After both</p></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "New A" and text =~ "New B" and text =~ "After both"
    refute text =~ "Old A" or text =~ "Old B"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "valid original inert inline and local stylesheet contexts can actually replace" do
    html =
      "<body><div><?start name='A'><p>Removed</p><?end?></div><template for='A'><style>p{color:red}</style><p style='font-size:14px'>Inert validated</p></template><p>After</p></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Inert validated" and text =~ "After"
    refute text =~ "Removed"
  end

  test "one source CSS budget spans original detached fragments instead of resetting per view" do
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)
    fragment = "<template for='A'><p style='--x:3pt;width:var(--x)'>Original</p></template>"
    html = "<body><img src='pixel.png'><?marker name='A'>#{fragment}#{fragment}</body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn _ ->
        frame = Process.get({TemplateExecutionValidator, :execution})
        assert frame.source_view == nil
        assert :atomics.get(frame.css_budget, 1) == if(frame.mode == :original, do: 18, else: 36)
        send(self(), {:completed_source_budget, frame.css_budget})
        {:ok, png}
      end)

    assert {:ok, _} = HtmlToPdf.render(html, opts)
    assert_received {:completed_source_budget, budget}
    assert :atomics.get(budget, 1) == 36
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "nested ordinary template CSS keeps its own original fragment scope" do
    html =
      "<body><?marker name='A'><template for='A'><template><p style='font-size:15px'>Nested inert</p></template><p style='color:blue'>Outer actual</p></template></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Outer actual"
    refute text =~ "Nested inert"
  end

  test "validated inert CSS cannot hide an unresolved resource or borrow active fetch evidence" do
    html =
      "<body><?marker name='A'><template for='A'><div style='display:none;background-image:url(hidden.png)'><p>Hidden</p></div><p>Actual</p></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        flunk("hidden original source fetched #{inspect(request)}")
      end)

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == :asset and diagnostic.operation == :parse
    assert diagnostic.source == "<div style='display:none;background-image:url(hidden.png)'>"
    assert diagnostic.message =~ "authorization and contents remain pending"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "an inert declaration cannot borrow a prospective host custom property" do
    html =
      "<body style='--host:8px'><div><?marker name='A'></div><template for='A'><p style='width:var(--host)'>Inert</p></template></body>"

    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.source == "width:var(--host)"
    assert diagnostic.message =~ "declaration/context policy failed"
    assert diagnostic.operation == :source_style_contexts

    assert {:ok, _} =
             HtmlToPdf.render(
               "<body style='--host:8px'><p style='width:var(--host)'>Ordinary inherited value</p></body>",
               @opts
             )

    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "excluded original stylesheet links cannot disappear through replacement or fetch" do
    html =
      "<body><?marker name='A'><template for='A'><link rel='stylesheet' href='original.css'><p>Actual</p></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        flunk("excluded original stylesheet fetched #{inspect(request)}")
      end)

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == :html and diagnostic.operation == :render
    assert diagnostic.source == "<link rel='stylesheet' href='original.css'>"
    assert diagnostic.line == 1 and diagnostic.column == 43
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
    assert {:error, {:unsupported_html, _}} = HtmlParser.parse_document_detailed(html)
  end

  test "invalid unmatched original inert CSS cannot disappear through a matched insertion" do
    html =
      "<body><?marker name='A'><template for='A'><style>.absent{width:bananas}</style><p>Good</p></template></body>"

    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.source == "width:bananas"
    assert diagnostic.message =~ "authored literal declaration"
    assert diagnostic.stage == :css and diagnostic.operation == :source_style_contexts
  end

  test "a consumed range cannot become a reusable target for a later template" do
    html =
      "<body><?start name='A'><p>Old</p><?end?><template for='A'><p>First inserted</p></template><template for='A'><p>Second inserted</p></template><p>After replacement</p></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "First inserted" and text =~ "After replacement"
    refute text =~ "Second inserted" or text =~ "Old"
  end

  test "a later candidate cannot use bytes after its real closing token as an original prefix" do
    html =
      "<body><?marker name='A'><template for='A'><p>First</p></template><?marker name='B'><template for='B'><p>Second</p></template><p>Future</p></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "First" and text =~ "Second" and text =~ "Future"
    assert TemplateExecutionValidator.original_prefix_provenance(%{}, 0) == :not_owned
  end

  test "successive replacement cannot discard invalid later original source" do
    html =
      "<body><?marker name='A'><template for='A'><p>First</p></template><?start name='B'><p style='width:bananas'>Invalid later original</p><?end?><template for='B'><p>Second</p></template></body>"

    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.source == "width:bananas"
    assert diagnostic.stage == :css and diagnostic.operation == :source_style_contexts

    assert diagnostic.line == 1 and
             diagnostic.column == String.length(hd(String.split(html, "width:bananas"))) + 1

    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "no earlier target retains an ordinary inert template after original validation" do
    assert {:ok, pdf} =
             HtmlToPdf.render(
               "<template for='later'><p>Inert text</p></template><p>Visible</p><?marker name='later'>",
               @opts
             )

    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Visible"
    refute text =~ "Inert text"
  end

  test "invalid removed CSS remains an actionable original-source failure" do
    html =
      "<?start name='A'><p style='width:bananas'>Bad</p><?end?><template for='A'><p>Good</p></template>"

    assert {:error, {:invalid_css, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.reason == :invalid_css
    assert diagnostic.source == "width:bananas"
    assert is_integer(diagnostic.line) and is_integer(diagnostic.column)
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "original removed resource is authorized and decoded before mutation" do
    html =
      "<?start name='A'><img src='missing.png'><?end?><template for='A'><p>Good</p></template>"

    assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :asset and diagnostic.reason == :invalid_document
    assert diagnostic.source == "missing.png"
    assert diagnostic.message =~ "not an authorized local file"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "CRLF and Unicode source advances skip exactly the consumed closing token" do
    html =
      "<body><?marker name='A'>\r\n<template for='A'>\r\n<p>Unicode α</p>\r\n</template>\r\n<p>After β</p></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Unicode" and text =~ "After"
  end

  test "a future target cannot activate and invalid inert original input remains diagnosed" do
    html =
      "<template for='future'><img src='missing-inert.png'></template><?marker name='future'>"

    assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :asset and diagnostic.source == "missing-inert.png"
  end

  test "an unmatched literal source declaration permits actual replacement without a fake element" do
    html =
      "<style>.unmatched{width:3px}</style><body><?marker name='A'><template for='A'><p>Good</p></template></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, reference} = HtmlToPdf.render("<p>Good</p>", @opts)
    assert pdf == reference
    assert {:ok, "Good"} = Text.extract(pdf)
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "the original head-target input retains its genuine tokenizer-context failure" do
    html =
      "<style>.unmatched{width:3px}</style><?marker name='A'><template for='A'><p>Good</p></template>"

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage == :html and diagnostic.operation == :render
    assert diagnostic.source == "<p>" and diagnostic.column == 73
    refute diagnostic.message =~ "unmatched_original_context"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "inactive and original detached unmatched literals complete before real insertion" do
    html =
      "<style>@media screen{.absent{height:4pt}}</style><body><?marker name='A'><template for='A'><style>.absent{width:3px}</style><p>Good</p></template></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, reference} = HtmlToPdf.render("<p>Good</p>", @opts)
    assert pdf == reference
  end

  test "original head tokenizer context stays invalid after unapplied symbolic source checks" do
    for declaration <- [
          "width:2rem",
          "height:3em",
          "width:25%",
          "font-family:Missing",
          "background-image:url(missing.png)",
          "width:var(--x)"
        ] do
      html =
        "<style>.absent{#{declaration}}</style><?marker name='A'><template for='A'><p>Good</p></template>"

      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, @opts)
      assert diagnostic.operation == :render and diagnostic.source == "<p>"
      assert diagnostic.stage == :html
      refute diagnostic.message =~ "unmatched_original_context"
    end
  end

  test "renderer exception cleans the source frame and cannot leave mutation authority" do
    html = "<?marker name='A'><template for='A'><img src='throws.png'></template>"
    opts = Keyword.put(@opts, :asset_resolver, fn _ -> raise "caller callback failure" end)
    assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.message == "caller-provided asset resolver could not resolve the asset"
    assert diagnostic.source == "throws.png"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
    assert {:error, {:unsupported_html, _}} = HtmlParser.parse_document_detailed(html)
  end

  test "hidden discarded resource cannot borrow completion from active preparation" do
    html =
      "<body><?start name='A'><div hidden><img src='hidden.png'></div><?end?><template for='A'><p>Good</p></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        flunk("unproven hidden source was fetched #{inspect(request)}")
      end)

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == :asset and diagnostic.operation == :parse
    assert diagnostic.source == "<img src='hidden.png'>"
    assert diagnostic.message =~ "authorization and contents remain pending"
  end

  test "real resource callbacks cannot reenter parser action authority during any preparation" do
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    html =
      "<body><?start name='A'><img src='pixel.png'><?end?><template for='A'><img src='pixel.png'><p>Actual</p></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        assert request.reference == "pixel.png"
        assert Process.get({TemplateExecutionValidator, :execution}).busy

        assert {:error, {:unsupported_html, diagnostic}} =
                 HtmlParser.parse_document_detailed(html)

        assert diagnostic.module == NativeElixirPdfUtilities.Validators.TemplatePatchValidator
        assert {:ok, _nested_pdf} = HtmlToPdf.render("<p>Nested independent renderer</p>", @opts)
        assert Process.get({TemplateExecutionValidator, :execution}).busy
        {:ok, png}
      end)

    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Actual"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "live source projection rejects malformed, changed and out-of-prefix facts without issuing authority" do
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    html =
      "<body><?marker name='A'><template for='A'><img src='pixel.png'><p>Actual</p></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn _ ->
        frame = Process.get({TemplateExecutionValidator, :execution})

        if frame.mode == :execute and frame.parser do
          assert {:error, {:invalid_document, malformed}} =
                   TemplateExecutionValidator.original_prefix_provenance(%{}, 0)

          assert malformed.stage == :html and
                   malformed.operation == :template_candidate_projection

          assert malformed.source == "source_prefix"

          assert {:error, {:invalid_document, boundary}} =
                   TemplateExecutionValidator.original_prefix_provenance(frame.original, -1)

          assert boundary.source == "source_prefix" and boundary.message =~ "original byte offset"

          assert {:error, {:invalid_document, future}} =
                   TemplateExecutionValidator.original_prefix_provenance(frame.original, 0)

          assert future.message =~ "protected original token identity"
          node = Enum.find(Map.values(frame.original.nodes), &(&1[:tag] == "img"))

          altered = %{
            frame.original
            | nodes: Map.put(frame.original.nodes, node.node_id, %{node | tag: "p"})
          }

          assert {:error, {:invalid_document, changed}} =
                   TemplateExecutionValidator.original_prefix_provenance(altered, byte_size(html))

          assert changed.source == "<img src='pixel.png'>" and
                   changed.message =~ "lexical ownership"

          send(self(), :checked_prospective_source_projection)
        end

        {:ok, png}
      end)

    assert {:ok, _pdf} = HtmlToPdf.render(html, opts)
    assert_received :checked_prospective_source_projection
    assert TemplateExecutionValidator.original_prefix_provenance(%{}, 0) == :not_owned
  end

  test "original source prefix boundaries use UTF-8 byte offsets and shared diagnostics" do
    alias NativeElixirPdfUtilities.Validators.HtmlCssSourceValidator
    assert {:ok, "α"} = HtmlCssSourceValidator.validate_source_prefix("αβ", 2)

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlCssSourceValidator.validate_source_prefix("αβ", 1)

    assert diagnostic.stage == :html and diagnostic.reason == :invalid_document
    assert diagnostic.source == "source_prefix" and diagnostic.message =~ "UTF-8 token boundary"

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlCssSourceValidator.validate_source_prefix("αβ", 5)

    assert diagnostic.message =~ "original byte offset"
  end

  test "a changed preparation policy cannot activate even with permissive counters" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    html =
      "<body><?start name='A'><img src='pixel.png'><?end?><template for='A'><p>Good</p></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn _ ->
        Limits.install(Map.update!(limits, :max_css_numeric_magnitude, &(&1 + 1)))
        {:ok, png}
      end)

    assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.operation == :parse and diagnostic.source == "template execution"
    assert diagnostic.message =~ "preparation configuration"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "original selected glyph failure cannot disappear with a removed range" do
    html =
      "<body><?start name='A'><p>\u{10FFFF}</p><?end?><template for='A'><p>Good</p></template></body>"

    assert {:error, {:unsupported_glyph, diagnostic}} =
             HtmlToPdf.render(html, Keyword.put(@opts, :unsupported_glyphs, :error))

    assert diagnostic.stage == :font and diagnostic.reason == :unsupported_glyph
    assert diagnostic.message != ""
  end

  test "discarded conditional and final-width resources retain explicit original obligations" do
    for content <- [
          "<object data='accepted.png'><img src='fallback.png'></object>",
          "<img loading='lazy' sizes='auto' src='lazy.png'>"
        ] do
      html =
        "<body><?start name='A'>#{content}<?end?><template for='A'><p>Good</p></template></body>"

      opts =
        Keyword.put(@opts, :asset_resolver, fn request ->
          flunk("pending source fetched #{inspect(request)}")
        end)

      assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, opts)
      assert diagnostic.stage == :asset and diagnostic.operation == :parse
      assert diagnostic.message =~ "conditional or final-width resource context"
    end
  end

  test "execution reuses the same input charge while charging both parser passes' work" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    html = "<body><?marker name='A'><template for='A'><p>Bounded</p></template></body>"
    Limits.install(Map.put(limits, :max_aggregate_html_source_bytes, byte_size(html)))
    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Bounded"
    assert TemplateExecutionValidator.source_charge(html) == byte_size(html)
  end

  test "an inert base cannot lend its future resolution to original fragment validation" do
    html =
      "<body><?marker name='A'><template for='A'><base href='future/'><img src='pixel.png'></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        flunk("unproven base fetched #{inspect(request)}")
      end)

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == :html and diagnostic.operation == :parse
    assert diagnostic.source == "<base href='future/'>"
    assert diagnostic.message =~ "original detached document URL context"
  end

  test "original control glyph failure survives deletion under the error policy" do
    opts = Keyword.put(@opts, :unsupported_glyphs, :error)
    content = "<input type='button' value='&#x10FFFF;'>"
    assert {:error, {reason, ordinary}} = HtmlToPdf.render("<body>#{content}</body>", opts)

    html =
      "<body><?start name='A'>#{content}<?end?><template for='A'><p>Good</p></template></body>"

    assert {:error, {^reason, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == ordinary.stage and diagnostic.reason == ordinary.reason
    assert diagnostic.module == ordinary.module and diagnostic.operation == ordinary.operation
    assert diagnostic.message == ordinary.message and diagnostic.source == ordinary.source
  end

  test "direct parser cannot replace and a stale renderer frame cannot authorize" do
    html = "<?marker name='A'><template for='A'><p>Good</p></template>"
    assert {:error, {:unsupported_html, diagnostic}} = HtmlParser.parse_document_detailed(html)
    assert diagnostic.operation == :parse and diagnostic.message =~ "original_source_policy"
    budget = HtmlValidator.new_image_budget()

    assert {:ok, _} =
             ResourceContext.run([], budget, fn _ ->
               TemplateExecutionValidator.prepare_source("<p>ordinary</p>", @opts, budget)
             end)

    assert {:error, {:unsupported_html, _}} = HtmlParser.parse_document_detailed(html)
  end
end
