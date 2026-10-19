alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateSourceResourceContextTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.Validators.TemplateExecutionValidator
  Code.require_file(Paths.support("png_fixture.ex"))
  alias NativeElixirPdfUtilities.TestSupport.PngFixture
  @opts [system_font_discovery: false, compress_streams: false, forms: :static]

  test "discarded image-input reference under a hidden ancestor cannot evade original resource validation" do
    html =
      "<body><?start name='A'><div hidden><input type='image' src='missing-original.png'></div><?end?><template for='A'><p>Good</p></template></body>"

    opts =
      Keyword.put(@opts, :asset_resolver, fn request ->
        send(self(), {:original_asset, request.reference})
        :not_found
      end)

    assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == :asset and diagnostic.operation == :render
    assert diagnostic.source == "missing-original.png"
    assert diagnostic.message =~ "not supplied"
    assert_received {:original_asset, "missing-original.png"}

    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "discarded generated control text under a hidden ancestor cannot evade selected-glyph policy" do
    html =
      "<body><?start name='A'><div hidden><input type='button' value='&#x10FFFF;'></div><?end?><template for='A'><p>Good</p></template></body>"

    assert {:error, {:unsupported_glyph, diagnostic}} =
             HtmlToPdf.render(html, Keyword.put(@opts, :unsupported_glyphs, :error))

    assert diagnostic.stage == :font and diagnostic.operation == :resolve_fonts
    assert diagnostic.source == <<0x10FFFF::utf8>>
    assert diagnostic.message =~ "U+10FFFF"

    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "conditional control text is checked independently of its parent's fallback selection" do
    html =
      "<body><?start name='A'><object><input type='button' value='&#x10FFFF;'></object><?end?><template for='A'><p>Good</p></template></body>"

    assert {:error, {:unsupported_glyph, diagnostic}} =
             HtmlToPdf.render(html, Keyword.put(@opts, :unsupported_glyphs, :error))

    assert diagnostic.stage == :font and diagnostic.operation == :resolve_fonts
    assert diagnostic.source == <<0x10FFFF::utf8>>
  end

  test "an Hidden input has no generated glyph or image obligation from its inert value" do
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

  test "hidden original images, backgrounds, text and controls complete independently of replacement" do
    png = PngFixture.build(2, 2, 2, 8, 0, fn _, _ -> [20, 120, 200] end)

    for original <- [
          "<div hidden style='font-size:18px;font-family:DejaVu Sans;background-image:url(original.png)'><img src='original.png'><input type=button value=Original><span>Authored</span></div>",
          "<details><summary>Title</summary><p style='background-image:url(original.png)'>Closed original</p></details>",
          "<select><option selected>Selected</option><option><span style='background-image:url(original.png)'>Unselected</span></option></select>",
          "<div hidden=until-found><input value=Original><textarea>Original text</textarea></div>",
          "<noembed>Inactive original text</noembed>",
          "<div><template shadowrootmode=open><span>Shadow</span></template><span style='background-image:url(original.png)'>Unassigned original</span></div>"
        ] do
      html =
        "<body><?start name='A'>#{original}<?end?><template for='A'><p>Replacement</p></template></body>"

      opts =
        Keyword.merge(@opts,
          assets: %{"original.png" => {:bytes, png}},
          unsupported_glyphs: :error
        )

      assert {:ok, pdf} = HtmlToPdf.render(html, opts)
      assert {:ok, "Replacement"} = Text.extract(pdf)
      assert Process.get({TemplateExecutionValidator, :execution}) == nil
    end
  end

  test "selected local object and its inactive fallback each retain original resource obligations" do
    root = Paths.project("tmp/template-source-local-#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    on_exit(fn -> File.rm_rf!(root) end)
    png = PngFixture.build(2, 2, 2, 8, 0, fn _, _ -> [20, 120, 200] end)
    File.write!(Path.join(root, "accepted.png"), png)

    original =
      "<object data='accepted.png' type='image/png'><img src='fallback.png'><input type=button value=Original></object>"

    html =
      "<body><?start name='A'>#{original}<?end?><template for='A'><p>Replacement</p></template></body>"

    opts = Keyword.merge(@opts, base_url: root, assets: %{"fallback.png" => {:bytes, png}})
    assert {:ok, pdf} = HtmlToPdf.render(html, opts)
    assert {:ok, "Replacement"} = Text.extract(pdf)

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(html, Keyword.delete(opts, :assets))

    assert diagnostic.stage == :asset and diagnostic.source == "fallback.png"
  end

  test "original hidden text fails its own selected glyph policy and releases its source scope" do
    html =
      "<body><?start name='A'><div hidden><span>&#x10FFFF;</span></div><?end?><template for='A'><p>Replacement</p></template></body>"

    assert {:error, {:unsupported_glyph, diagnostic}} =
             HtmlToPdf.render(html, Keyword.put(@opts, :unsupported_glyphs, :error))

    assert diagnostic.reason == :unsupported_glyph and diagnostic.stage == :font
    assert diagnostic.source == <<0x10FFFF::utf8>>
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end

  test "dormant controls validate without creating fields or hidden name collisions" do
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    for original <- [
          "<input name=x value=Visible><div hidden><input name=x value=Hidden></div>",
          "<div hidden><input type=range><input type=color><input type=image src=original.png></div>"
        ] do
      html =
        "<body><?start name='A'>#{original}<?end?><template for='A'><p>Replacement</p></template></body>"

      assert {:ok, pdf} =
               HtmlToPdf.render(
                 html,
                 Keyword.merge(@opts,
                   forms: :interactive,
                   assets: %{"original.png" => {:bytes, png}}
                 )
               )

      assert {:ok, "Replacement"} = Text.extract(pdf)
      assert {:ok, []} = NativeElixirPdfUtilities.Forms.fields(pdf)
    end
  end

  test "original Math clone identities survive the separate dormant-source preparation" do
    original =
      "<div hidden>Hidden</div><select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option><math><mi>x</mi></math></option></select>"

    html =
      "<body><?start name='A'>#{original}<?end?><template for='A'><p>Replacement</p></template></body>"

    assert {:ok, pdf} = HtmlToPdf.render(html, @opts)
    assert {:ok, "Replacement"} = Text.extract(pdf)
  end

  test "dormant image occurrences still consume the shared image budget on reuse" do
    alias NativeElixirPdfUtilities.Limits
    png = PngFixture.build(1, 1, 2, 8, 0, fn _, _ -> [255, 0, 0] end)

    html =
      "<body><?start name='A'><div hidden><img src=original.png><img src=original.png></div><?end?><template for='A'><p>Replacement</p></template></body>"

    opts = Keyword.put(@opts, :assets, %{"original.png" => {:bytes, png}})
    assert {:ok, _} = HtmlToPdf.render(html, opts)
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    Limits.install(Map.put(limits, :max_image_count, 1))
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == :limits and diagnostic.message =~ "image count"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
  end
end
