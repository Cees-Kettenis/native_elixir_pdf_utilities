defmodule NativeElixirPdfUtilities.HtmlToPdf.TemplateOriginalFontResourcesTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.Validators.{FontValidator, TemplateExecutionValidator}

  @opts [system_font_discovery: false, compress_streams: false]

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)

    bytes =
      File.read!(
        Path.join(:code.priv_dir(:native_elixir_pdf_utilities), "fonts/dejavu/DejaVuSansMono.ttf")
      )

    %{bytes: bytes}
  end

  test "invalid discarded font bytes report the exact original descriptor and position" do
    html = """
    <body><?start name='A'>
    <style>@media screen{@font-face{font-family:OriginalInactive;
    src:url('invalid-original.ttf')}}</style><p>Old</p><?end?>
    <template for='A'><p>Good</p></template><p>After</p></body>
    """

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(
               html,
               Keyword.put(@opts, :asset_resolver, fn _ -> {:ok, "invalid font bytes"} end)
             )

    assert diagnostic.stage == :font
    assert diagnostic.reason == :invalid_document
    assert diagnostic.operation == :original_font_sources
    assert diagnostic.source == "src:url('invalid-original.ttf')"
    assert diagnostic.line == 3 and diagnostic.column == 1
    assert diagnostic.message =~ "parsed or embedded"
    assert Process.get({TemplateExecutionValidator, :execution}) == nil
    assert Process.get({FontValidator, :budget}) == nil
  end

  test "inactive original registry does not supply fonts to surviving text", %{bytes: bytes} do
    html =
      "<body><?start name='A'><style>@media screen{@font-face{font-family:OriginalInactive;src:url('provided-original.ttf')}}</style><p>Old</p><?end?><template for='A'><p style='font-family:OriginalInactive'>Good</p></template><p>After</p></body>"

    caller = self()

    resolver = fn request ->
      send(
        caller,
        {:font_request, request.reference, Process.get({FontValidator, :budget}).budget_id}
      )

      {:ok, bytes}
    end

    assert {:ok, pdf} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
    assert_receive {:font_request, "provided-original.ttf", budget_id}
    assert is_reference(budget_id)
    refute pdf =~ "OriginalInactive"
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "Good" and text =~ "After"
    refute text =~ "Old"
  end

  test "superseded original src keeps its own ordered loading obligation", %{bytes: bytes} do
    html =
      "<body><?start name='A'><style>@media screen{@font-face{font-family:OriginalInactive;src:url('invalid-original.ttf');src:url('valid-original.ttf')}}</style><p>Old</p><?end?><template for='A'><p>Good</p></template></body>"

    resolver = fn request ->
      send(self(), {:source, request.reference})

      {:ok,
       if(request.reference == "invalid-original.ttf", do: "invalid font bytes", else: bytes)}
    end

    assert {:error, {:invalid_document, diagnostic}} =
             HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))

    assert diagnostic.stage == :font
    assert diagnostic.source == "src:url('invalid-original.ttf')"
    assert_receive {:source, "invalid-original.ttf"}
    refute_received {:source, "valid-original.ttf"}
  end

  test "one discarded src uses normal supported fallback candidates", %{bytes: bytes} do
    html =
      "<body><?start name='A'><style>@media screen{@font-face{font-family:OriginalInactive;src:url('invalid-original.ttf'),url('valid-original.ttf')}}</style><p>Old</p><?end?><template for='A'><p>Good</p></template></body>"

    resolver = fn request ->
      send(self(), {:candidate, request.reference})

      {:ok,
       if(request.reference == "invalid-original.ttf", do: "invalid font bytes", else: bytes)}
    end

    assert {:ok, pdf} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
    assert_receive {:candidate, "invalid-original.ttf"}
    assert_receive {:candidate, "valid-original.ttf"}
    refute pdf =~ "OriginalInactive"
  end

  test "inactive stylesheet media also preserves original font resources", %{bytes: bytes} do
    html =
      "<body><?start name='A'><style media='screen'>@font-face{font-family:OriginalInactive;src:url('media-original.ttf')}</style><p>Old</p><?end?><template for='A'><p>Good</p></template></body>"

    resolver = fn request ->
      send(self(), {:media_font, request.reference})
      {:ok, bytes}
    end

    assert {:ok, pdf} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
    assert_receive {:media_font, "media-original.ttf"}
    refute pdf =~ "OriginalInactive"
  end

  test "original font assets use the original document URL context", %{bytes: bytes} do
    html =
      "<head><base href='https://fonts.example/original/'></head><body><?start name='A'><style>@media screen{@font-face{font-family:OriginalInactive;src:url('provided-original.ttf')}}</style><p>Old</p><?end?><template for='A'><p>Good</p></template></body>"

    resolver = fn request ->
      send(self(), {:resolved_original, request.reference})
      {:ok, bytes}
    end

    assert {:ok, _} = HtmlToPdf.render(html, Keyword.put(@opts, :asset_resolver, resolver))
    assert_receive {:resolved_original, "https://fonts.example/original/provided-original.ttf"}
  end

  test "original font preparation and active preparation share the font candidate ceiling", %{
    bytes: bytes
  } do
    html =
      "<body><?start name='A'><style>@media screen{@font-face{font-family:OriginalInactive;src:url('provided-original.ttf')}}</style><p>Old</p><?end?><template for='A'><p>Good</p></template></body>"

    Limits.install(%{Limits.effective() | max_font_candidates: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render(
               html,
               Keyword.put(@opts, :assets, %{"provided-original.ttf" => {:bytes, bytes}})
             )

    assert diagnostic.stage == :font
    assert diagnostic.message =~ "max_font_candidates"
    assert diagnostic.source == "src:url('provided-original.ttf')"
    assert diagnostic.line == 1 and is_integer(diagnostic.column)
    assert Process.get({FontValidator, :budget}) == nil
  end

  test "equal configured CSS keeps each file's actual resolution base", %{bytes: bytes} do
    root =
      Path.join(System.tmp_dir!(), "original-font-context-#{System.unique_integer([:positive])}")

    on_exit(fn -> File.rm_rf!(root) end)
    css = "@media screen{@font-face{font-family:OriginalInactive;src:url('same.ttf')}}"

    for directory <- ["first", "second"] do
      File.mkdir_p!(Path.join(root, directory))
      File.write!(Path.join([root, directory, "fonts.css"]), css)

      File.write!(
        Path.join([root, directory, "same.ttf"]),
        if(directory == "first", do: bytes, else: "invalid font bytes")
      )
    end

    html = "<body><?marker name='A'><template for='A'><p>Good</p></template></body>"

    opts =
      Keyword.put(
        @opts,
        :stylesheets,
        Enum.map(["first", "second"], &{:file, Path.join([root, &1, "fonts.css"])})
      )

    assert {:error, {:invalid_document, diagnostic}} = HtmlToPdf.render(html, opts)
    assert diagnostic.stage == :font
    assert diagnostic.source == "src:url('same.ttf')"
  end
end
