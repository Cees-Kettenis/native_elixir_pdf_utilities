defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateSourceFontContextTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  @opts [system_font_discovery: false, compress_streams: false]

  test "discarded inactive font bytes cannot evade original resource and font validation" do
    html = source("missing-original.ttf")
    assert {:error, {_reason, diagnostic}} = HtmlToPdf.render(html, @opts)
    assert diagnostic.stage in [:asset, :font]
    assert diagnostic.source =~ "missing-original.ttf"
  end

  test "caller-provided inactive font bytes validate without becoming an active family" do
    html = source("provided-original.ttf")

    bytes =
      File.read!(
        Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSansMono.ttf")
      )

    caller = self()

    resolver = fn request ->
      send(caller, {:original_font, request.reference})
      {:ok, bytes}
    end

    assert {:ok, pdf} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
    assert_receive {:original_font, "provided-original.ttf"}
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Good" and text =~ "After"
    refute text =~ "Old"
  end

  test "invalid caller-provided inactive font bytes fail before replacement" do
    html = source("invalid-original.ttf")
    resolver = fn _ -> {:ok, "invalid font bytes"} end

    assert {:error, {_reason, diagnostic}} =
             HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))

    assert diagnostic.stage == :font
  end

  defp source(reference) do
    "<body><?start name='A'><style>@media screen{@font-face{font-family:OriginalInactive;src:url('#{reference}')}}</style><p>Old</p><?end?><template for='A'><p>Good</p></template><p>After</p></body>"
  end
end
