defmodule NativeElixirPdfUtilities.HtmlToPdf.BrowserReferenceCacheTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.TestSupport.{
    BrowserReferenceCache,
    BrowserReferenceValidator,
    Paths
  }

  setup do
    previous = System.get_env() |> Map.take(["BROWSER_PARITY_CACHE", "BROWSER_PARITY_TOOLCHAIN"])

    directory =
      Paths.project("tmp/reference-cache-contract-#{System.unique_integer([:positive])}")

    BrowserReferenceCache.stop()
    System.put_env("BROWSER_PARITY_CACHE", directory)
    System.put_env("BROWSER_PARITY_TOOLCHAIN", "cache-contract-image")
    {:ok, cache} = BrowserReferenceCache.start()
    # on_exit owns teardown after this test process has exited.
    Process.unlink(cache)

    on_exit(fn ->
      BrowserReferenceCache.stop()
      for key <- ["BROWSER_PARITY_CACHE", "BROWSER_PARITY_TOOLCHAIN"], do: System.delete_env(key)
      System.put_env(previous)
      {:ok, cache} = BrowserReferenceCache.start()
      # The suite owns the restored coordinator after this cleanup worker exits.
      Process.unlink(cache)
      File.rm_rf!(directory)
    end)

    artifact_dir = Path.join(directory, "input")
    File.mkdir_p!(artifact_dir)
    pdf = Path.join(artifact_dir, "chromium.pdf")
    page = Path.join(artifact_dir, "chromium-1.ppm")
    File.write!(pdf, "%PDF-reference")
    File.write!(page, "P6\n1 1\n255\n" <> <<255, 0, 0>>)

    reference = %{
      artifacts: [pdf, page],
      pages: [page],
      vector_pages: [],
      embedded_fonts: "independent font inspection"
    }

    %{directory: directory, reference: reference}
  end

  test "round trips exact reference bytes and regenerates missing or corrupt entries", context do
    key = BrowserReferenceCache.key(%{html: "original", dpi: 72})
    output = Path.join(context.directory, "restored")
    File.mkdir_p!(output)
    assert BrowserReferenceCache.restore(key, output) == nil
    assert :ok = BrowserReferenceCache.store(key, context.reference)
    restored = BrowserReferenceCache.restore(key, output)
    assert restored.embedded_fonts == context.reference.embedded_fonts

    assert Enum.map(restored.pages, &File.read!/1) ==
             Enum.map(context.reference.pages, &File.read!/1)

    assert File.read!(Path.join(output, "chromium.pdf")) == "%PDF-reference"
    assert File.ls!(output) |> Enum.sort() == ["chromium-1.ppm", "chromium.pdf"]

    path = Path.join(context.directory, key <> ".cache")

    for corrupt <- [
          "truncated",
          <<0::256, "bad payload">>,
          <<:crypto.hash(:sha256, "invalid term")::binary, "invalid term">>
        ] do
      File.write!(path, corrupt)
      assert BrowserReferenceCache.restore(key, output) == nil
      refute File.exists?(path)
    end

    assert :ok = BrowserReferenceCache.store(key, context.reference)
    assert BrowserReferenceCache.restore(key, output)
  end

  test "keys invalidate browser inputs and toolchains while resource trees track all local bytes",
       context do
    identity = %{html: "original", dpi: 72, browser: "pinned", font: "face"}
    key = BrowserReferenceCache.key(identity)

    for field <- Map.keys(identity) do
      refute BrowserReferenceCache.key(Map.put(identity, field, "changed")) == key
    end

    BrowserReferenceCache.stop()
    System.put_env("BROWSER_PARITY_TOOLCHAIN", "another-image")
    {:ok, cache} = BrowserReferenceCache.start()
    Process.unlink(cache)
    refute BrowserReferenceCache.key(identity) == key

    tree = Path.join(context.directory, "dependencies")
    File.mkdir_p!(tree)
    path = Path.join(tree, "nested.css")
    before = BrowserReferenceValidator.tree_identity([tree])
    File.write!(path, "p{color:red}")
    added = BrowserReferenceValidator.tree_identity([tree])
    refute before == added
    File.write!(path, "p{color:blue}")
    refute BrowserReferenceValidator.tree_identity([tree]) == added
    File.rm!(path)
    assert BrowserReferenceValidator.tree_identity([tree]) == before
  end

  test "unsafe paths, missing pages and changed artifact bytes cannot be restored" do
    files = [
      {"chromium.pdf", "pdf", :crypto.hash(:sha256, "pdf")},
      {"chromium-1.ppm", "rgb", :crypto.hash(:sha256, "rgb")}
    ]

    payload = %{
      files: files,
      pages: ["chromium-1.ppm"],
      vector_pages: [],
      embedded_fonts: "fonts"
    }

    assert BrowserReferenceValidator.cache_payload?(payload)
    refute BrowserReferenceValidator.cache_payload?(%{payload | pages: ["missing.ppm"]})

    refute BrowserReferenceValidator.cache_payload?(%{
             payload
             | files: [{"../chromium.pdf", "pdf", :crypto.hash(:sha256, "pdf")}]
           })

    refute BrowserReferenceValidator.cache_payload?(%{
             payload
             | files: [{"chromium.pdf", "changed", :crypto.hash(:sha256, "pdf")}]
           })

    refute BrowserReferenceValidator.cache_payload?(nil)
  end

  test "startup removes interrupted writes and excess generations; writes and replacement stay bounded",
       context do
    BrowserReferenceCache.stop()

    for name <- ["old.cache", "recent.cache", "newest.cache", "interrupted.tmp"] do
      path = Path.join(context.directory, name)

      File.open!(path, [:write, :binary], fn file ->
        :file.position(file, 30 * 1024 * 1024 - 1)
        IO.binwrite(file, <<0>>)
      end)
    end

    File.touch!(Path.join(context.directory, "old.cache"), 1)
    {:ok, cache} = BrowserReferenceCache.start()
    Process.unlink(cache)
    refute File.exists?(Path.join(context.directory, "old.cache"))
    refute File.exists?(Path.join(context.directory, "interrupted.tmp"))

    for n <- 1..25 do
      :ok = BrowserReferenceCache.store(Integer.to_string(n), context.reference)
    end

    :ok = BrowserReferenceCache.store("25", context.reference)

    bytes =
      Path.wildcard(Path.join(context.directory, "*.cache"))
      |> Enum.reduce(0, &(File.stat!(&1).size + &2))

    assert bytes <= 64 * 1024 * 1024
    assert Path.wildcard(Path.join(context.directory, "*.tmp")) == []

    # Force a write across the remaining budget without allocating huge binaries.
    path = Path.join(context.directory, "recent.cache")

    File.open!(path, [:write, :binary], fn file ->
      :file.position(file, 64 * 1024 * 1024 - 1)
      IO.binwrite(file, <<0>>)
    end)

    File.touch!(path, 1)
    :ok = BrowserReferenceCache.store("next", context.reference)
    refute File.exists?(path)

    assert Path.wildcard(Path.join(context.directory, "*.cache"))
           |> Enum.reduce(0, &(File.stat!(&1).size + &2)) <= 64 * 1024 * 1024
  end

  test "dynamic inputs and explicit fresh-capture options bypass persistence" do
    fixture = Paths.fixture("html_to_pdf/browser_parity/absolute_positioning.html")
    assert BrowserReferenceCache.eligible?(fixture, [])
    refute BrowserReferenceCache.eligible?(fixture, reference_cache: false)
    refute BrowserReferenceCache.eligible?(fixture, chromium_bin: "alternate")
    refute BrowserReferenceCache.eligible?(Paths.project("tmp/live-input.html"), [])
  end

  test "transitive local resources are eligible while unknown, encoded and remote resources stay fresh",
       context do
    fixture =
      Paths.fixture(
        "html_to_pdf/browser_parity/.chromium-input-cache-contract-#{System.unique_integer([:positive])}.html"
      )

    on_exit(fn -> File.rm!(fixture) end)
    css = Path.join(context.directory, "style.css")
    File.write!(css, "p{color:red}")
    dependencies = BrowserReferenceValidator.tree_identity([context.directory])
    File.write!(fixture, "<link href='#{css}'><p>Local</p>")
    assert BrowserReferenceValidator.cacheable_fixture?(fixture, [], dependencies)

    for source <- [
          "<img src='https://example.test/live.png'>",
          "<img src='/unknown/file.png'>",
          "<iframe srcdoc='dynamic'></iframe>",
          "<style>@import 'other.css';</style>",
          "<img src='asset&#46;png'>",
          "<video poster='https://example.test/live.png'></video>",
          "<style>p{background-image:image-set(\"https://example.test/live.png\" 1x)}</style>",
          "<svg><animate attributeName='href' to='https://example.test/live.svg'/></svg>"
        ] do
      File.write!(fixture, source)
      refute BrowserReferenceValidator.cacheable_fixture?(fixture, [], dependencies)
    end

    File.write!(fixture, "<link href='#{css}'>")
    File.write!(css, "p{background:url(https://example.test/live.png)}")
    refute BrowserReferenceValidator.cacheable_fixture?(fixture, [], dependencies)
  end

  test "observed requests independently reject untracked or network resources", context do
    local = Path.join(context.directory, "known.css")
    inputs = [Path.join(context.directory, "input.html")]
    dependencies = %{local => :crypto.hash(:sha256, "css")}
    covered = ["file://" <> local, "file://" <> hd(inputs), "data:image/png;base64,AA=="]

    assert BrowserReferenceValidator.requests_covered?(
             covered,
             dependencies,
             inputs,
             context.directory
           )

    for url <- ["https://example.test/asset.png", "file:///outside/unknown.png", "blob:unknown"] do
      refute BrowserReferenceValidator.requests_covered?(
               covered ++ [url],
               dependencies,
               inputs,
               context.directory
             )
    end

    refute BrowserReferenceValidator.requests_covered?(
             [],
             dependencies,
             inputs,
             context.directory
           )
  end

  test "navigation links stay cacheable without admitting fetched remote resources", context do
    fixture =
      Paths.fixture(
        "html_to_pdf/browser_parity/.chromium-input-navigation-cache-#{System.unique_integer([:positive])}.html"
      )

    on_exit(fn -> File.rm!(fixture) end)
    dependencies = BrowserReferenceValidator.tree_identity([context.directory])

    File.write!(
      fixture,
      "<a href='https://example.test/page'>Link</a><map><area href='https://example.test/region'></map>"
    )

    assert BrowserReferenceValidator.cacheable_fixture?(fixture, [], dependencies)
    File.write!(fixture, "<svg><use href='https://example.test/shape.svg#part'/></svg>")
    refute BrowserReferenceValidator.cacheable_fixture?(fixture, [], dependencies)
  end

  test "SVG data images verify decoded and nested resource dependencies", context do
    fixture =
      Paths.fixture(
        "html_to_pdf/browser_parity/.chromium-input-svg-cache-#{System.unique_integer([:positive])}.html"
      )

    on_exit(fn -> File.rm!(fixture) end)
    image = Path.join(context.directory, "paint.png")
    File.write!(image, NativeElixirPdfUtilities.TestSupport.PngFixture.solid_rgb(1, 1))
    dependencies = BrowserReferenceValidator.tree_identity([context.directory])
    data_uri = fn svg -> "data:image/svg+xml;base64," <> Base.encode64(svg) end

    for svg <- [
          "<svg><rect width='4' height='4' fill='red'/></svg>",
          "<svg><image href='#{image}'/></svg>",
          "<svg><image href='#{data_uri.("<svg><rect fill='blue'/></svg>")}'/></svg>"
        ] do
      File.write!(fixture, "<img src='#{data_uri.(svg)}'>")
      assert BrowserReferenceValidator.cacheable_fixture?(fixture, [], dependencies)
    end

    for svg <- [
          <<255>>,
          "<!DOCTYPE svg SYSTEM 'https://example.test/live.dtd'><svg/>",
          "<?xml-stylesheet href='https://example.test/live.css'?><svg/>",
          "<svg><image href='https://example.test/live.png'/></svg>",
          "<svg><image href='/unknown/paint.png'/></svg>",
          "<svg><script>alert(1)</script></svg>",
          "<svg><animate attributeName='href'/></svg>",
          "<s:svg xmlns:s='http://www.w3.org/2000/svg'><s:animate attributeName='fill'/></s:svg>",
          "<s:svg xmlns:s='http://www.w3.org/2000/svg'><s:script>alert(1)</s:script></s:svg>",
          "<svg xml:base='https://example.test/'><image href='live.png'/></svg>",
          "<svg><image href='#{data_uri.("<svg><image href='https://example.test/live.png'/></svg>")}'/></svg>"
        ] do
      File.write!(fixture, "<img src='#{data_uri.(svg)}'>")
      refute BrowserReferenceValidator.cacheable_fixture?(fixture, [], dependencies)
    end

    File.write!(fixture, "<img src='data:image/svg+xml;base64,not-base64!'>")
    refute BrowserReferenceValidator.cacheable_fixture?(fixture, [], dependencies)
  end

  test "static transformed placeholders are candidates but final unknown dependencies still fail" do
    fixture = Paths.fixture("html_to_pdf/browser_parity/html_embedded_images.html")
    assert BrowserReferenceCache.candidate?(fixture, [])
    refute BrowserReferenceCache.eligible?(fixture, [])
    refute BrowserReferenceCache.candidate?(fixture, reference_cache: false)
  end

  test "only the helper's exact local Fontconfig declaration is eligible" do
    alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
    fixture = Paths.fixture("html_to_pdf/browser_parity/absolute_positioning.html")

    PdfVisualCompare.with_embedded_font_configuration(fixture, fn ->
      assert BrowserReferenceCache.eligible?(fixture, [])
      configuration = System.fetch_env!("FONTCONFIG_FILE")
      original = File.read!(configuration)

      try do
        File.write!(
          configuration,
          String.replace(original, "/etc/fonts/fonts.conf", "/unverified/fonts.conf")
        )

        refute BrowserReferenceCache.candidate?(fixture, [])
        File.write!(configuration, String.replace(original, "<dir>", "<dir>/unknown/"))
        refute BrowserReferenceCache.candidate?(fixture, [])
      after
        File.write!(configuration, original)
      end
    end)
  end

  test "installed font directories are fingerprinted for the helper's Fontconfig policy" do
    alias NativeElixirPdfUtilities.TestSupport.PdfVisualCompare
    fixture = Paths.fixture("html_to_pdf/browser_parity/fonts_and_print_media.html")

    PdfVisualCompare.with_embedded_font_configuration(fixture, fn ->
      configuration = File.read!(System.fetch_env!("FONTCONFIG_FILE"))
      assert configuration =~ ~r{<dir>/usr/(?:local/)?share/fonts/}
      assert BrowserReferenceCache.eligible?(fixture, [])
    end)
  end
end
