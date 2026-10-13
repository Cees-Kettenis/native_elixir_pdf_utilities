defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateSourceResourceContextTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Validators.TemplateExecutionValidator
  @opts [system_font_discovery: false, compress_streams: false, forms: :static]

  test "discarded image-input reference under a hidden ancestor cannot evade original resource validation" do
    html =
      "<body><?start name='A'><div hidden><input type='image' src='missing-original.png'></div><?end?><template for='A'><p>Good</p></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        flunk("unprepared original reference fetched #{inspect(request)}")
      end)

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == :asset and diagnostic.operation == :parse
    assert diagnostic.source == "<input type='image' src='missing-original.png'>"
    assert diagnostic.message =~ "authorization and contents remain pending"

    assert diagnostic.line == 1 and
             diagnostic.column == String.length(hd(String.split(html, "<input"))) + 1

    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "discarded generated control text under a hidden ancestor cannot evade selected-glyph policy" do
    html =
      "<body><?start name='A'><div hidden><input type='button' value='&#x10FFFF;'></div><?end?><template for='A'><p>Good</p></template></body>"

    assert {:error, {:unsupported_html, diagnostic}} =
             HtmlToPdf.render(html, Keyword.put(@opts, :unsupported_glyphs, :error))

    assert diagnostic.stage == :font and diagnostic.operation == :parse
    assert diagnostic.source == "<input type='button' value='&#x10FFFF;'>"
    assert diagnostic.message =~ "original control"

    assert diagnostic.line == 1 and
             diagnostic.column == String.length(hd(String.split(html, "<input"))) + 1

    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "conditional control text cannot borrow completion from an accepted parent resource" do
    html =
      "<body><?start name='A'><object data='accepted.png'><input type='button' value='&#x10FFFF;'></object><?end?><template for='A'><p>Good</p></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        flunk("conditional source used parent resource evidence #{inspect(request)}")
      end)

    assert {:error, {:unsupported_html, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == :font and diagnostic.operation == :parse
    assert diagnostic.source == "<input type='button' value='&#x10FFFF;'>"
    assert diagnostic.message =~ "selected glyph and runtime obligations remain pending"
  end

  test "an actual Hidden input has no generated glyph or image obligation from its inert value" do
    html =
      "<body><?start name='A'><div hidden><input type='hidden' value='&#x10FFFF;'></div><?end?><template for='A'><p>Good</p></template></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, Keyword.put(@opts, :unsupported_glyphs, :error))
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Good"
  end

  test "inapplicable text-input source metadata stays inert while normal original control glyphs prepare" do
    html =
      "<body><?start name='A'><input type='text' src='javascript:ignored' value='Original'><?end?><template for='A'><p>Good</p></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        flunk("inert metadata fetched #{inspect(request)}")
      end)

    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Good"
    refute text =~ "Original"
  end
end
