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

  test "an unmatched source declaration keeps original policy pending" do
    html =
      "<style>.unmatched{width:3px}</style><?marker name='A'><template for='A'><p>Good</p></template>"

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.operation == :parse
    assert diagnostic.message =~ "unmatched_original_context"
    assert diagnostic.source == "width:3px"
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
