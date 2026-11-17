defmodule NativeElixirPdfUtilities.TestSupport.BrowserReferenceValidator do
  @moduledoc false
  import ExUnit.Assertions

  @doc "Rejects output directories that could overwrite or remove an immutable reference."
  @spec assert_isolated_output!(map(), String.t()) :: :ok
  def assert_isolated_output!(reference, directory) do
    source = reference.artifact_dir |> Path.expand() |> Path.split()
    destination = directory |> Path.expand() |> Path.split()

    refute Enum.take(source, length(destination)) == destination or
             Enum.take(destination, length(source)) == source,
           "Browser variant output directory overlaps its prepared reference"

    :ok
  end

  @doc "Checks that a prepared reference belongs to the exact browser input and environment."
  @spec assert_identity!(map(), map()) :: :ok
  def assert_identity!(reference, identity) do
    assert reference.identity == identity,
           "Prepared browser reference identity differs: " <>
             inspect(for {key, value} <- identity, reference.identity[key] != value, do: key)

    for {path, digest} <- reference.artifact_hashes do
      assert Base.encode16(:crypto.hash(:sha256, File.read!(path)), case: :lower) == digest,
             "Prepared browser reference artifact changed: #{path}"
    end

    :ok
  end

  @doc "Uses original captures when the browser, rasterizer, supplied faces or locale lack an equivalence proof."
  @spec page_profile_supported?(map() | nil, map()) :: boolean()
  def page_profile_supported?(proof, profile) do
    if is_map(proof) do
      fonts =
        Enum.map(profile.fonts, fn font ->
          [
            font.family,
            font.sha256,
            Map.get(font, :weight, 400),
            to_string(Map.get(font, :style, :normal))
          ]
        end)

      config = profile.environment["FONTCONFIG_FILE"]

      digest =
        if config && File.regular?(config),
          do: Base.encode16(:crypto.hash(:sha256, File.read!(config)), case: :lower),
          else: ""

      profile.browser == proof["browser"] and profile.rasterizer == proof["rasterizer"] and
        profile.adapter == "pipe" and fonts == proof["fonts"] and
        digest == proof["fontconfig_sha256"] and not is_nil(config) == proof["fontconfig_present"] and
        Map.drop(profile.environment, ["FONTCONFIG_FILE"]) == proof["environment"]
    else
      true
    end
  end

  @doc "Keeps original physical fonts, geometry, density and browser environment in a shared document."
  @spec assert_page_environment!(map(), map(), boolean()) :: :ok
  def assert_page_environment!(capture, source, same_geometry?) do
    for key <- [
          :base_url,
          :dpi,
          :reference_mode,
          :browser,
          :rasterizer,
          :environment,
          :fontconfig_sha256,
          :browser_adapter
        ] do
      assert capture[key] == source[key], "Combined document changed the original #{key} contract"
    end

    fonts = fn values ->
      Enum.map(values, fn font ->
        {font.family, font.sha256, Map.get(font, :weight, 400), Map.get(font, :style, :normal)}
      end)
    end

    assert fonts.(capture.fonts) == fonts.(source.fonts),
           "Combined document changed physical browser faces"

    if same_geometry? do
      alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry

      assert PageGeometry.normalize_page_size(capture.page_size) ==
               PageGeometry.normalize_page_size(source.page_size),
             "Combined document changed the original page size"

      assert PageGeometry.normalize_margins(capture.margin) ==
               PageGeometry.normalize_margins(source.margin),
             "Combined document changed the original margins"
    end

    assert source.dpi == 72 and source.reference_mode == nil,
           "SVG page groups need both density and vector proofs"

    :ok
  end

  @doc "Fingerprints the complete local resource trees, including missing versus newly added files."
  @spec tree_identity([String.t()]) :: map()
  def tree_identity(roots) do
    roots
    |> Enum.flat_map(&Path.wildcard(Path.join(&1, "**/*"), match_dot: true))
    |> Enum.filter(
      &(File.regular?(&1) and not String.starts_with?(Path.basename(&1), ".chromium-input-"))
    )
    |> Map.new(fn path -> {path, :crypto.hash(:sha256, File.read!(path))} end)
  end

  @doc "Admits repository captures for final-input checking after deterministic reference transformation."
  @spec cache_candidate?(String.t(), keyword(), map()) :: boolean()
  def cache_candidate?(fixture, opts, dependencies) do
    root = NativeElixirPdfUtilities.TestSupport.Paths.fixture("html_to_pdf/browser_parity") <> "/"

    String.starts_with?(Path.expand(fixture), root) and
      Keyword.get(opts, :reference_cache, true) and
      not Keyword.has_key?(opts, :chromium_bin) and not Keyword.has_key?(opts, :pdftoppm_bin) and
      System.get_env("BROWSER_PARITY_BROWSER", "pipe") == "pipe" and
      cache_fontconfig?(dependencies) and System.get_env("FONTCONFIG_PATH") == nil and
      System.get_env("CHROMIUM_BIN", "/usr/bin/chromium") == "/usr/bin/chromium" and
      System.get_env("PDFTOPPM_BIN", "/usr/bin/pdftoppm") == "/usr/bin/pdftoppm"
  end

  @doc "Allows only static local dependencies covered by the suite fingerprint. Ambiguous URLs use fresh captures."
  @spec cacheable_fixture?(String.t(), keyword(), map()) :: boolean()
  def cacheable_fixture?(fixture, opts, dependencies) do
    cache_candidate?(fixture, opts, dependencies) and
      static_resources?(Path.expand(fixture), dependencies, MapSet.new())
  end

  defp cache_fontconfig?(dependencies) do
    alias NativeElixirPdfUtilities.TestSupport.Paths

    case System.get_env("FONTCONFIG_FILE") do
      nil ->
        true

      file ->
        directory = Paths.project("tmp/embedded-document-fontconfig")

        with true <- file == Path.join(directory, "fonts.conf"),
             {:ok, bytes} <- File.read(file) do
          dirs = Regex.scan(~r/<dir>([^<]+)<\/dir>/, bytes) |> Enum.map(&Enum.at(&1, 1))
          declarations = Enum.map_join(dirs, &"<dir>#{&1}</dir>")

          expected = """
          <?xml version="1.0"?>
          <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
          <fontconfig><include>/etc/fonts/fonts.conf</include>#{declarations}<cachedir>#{directory}/cache</cachedir></fontconfig>
          """

          known =
            dependencies
            |> Map.keys()
            |> Enum.filter(&(Path.extname(&1) in [".ttf", ".otf"]))
            |> Enum.map(&Path.dirname/1)
            |> MapSet.new()

          bytes == expected and dirs != [] and Enum.all?(dirs, &MapSet.member?(known, &1))
        else
          _ -> false
        end
    end
  end

  defp static_resources?(path, dependencies, visited) do
    if MapSet.member?(visited, path) or Path.extname(path) not in [".html", ".svg", ".css"] do
      true
    else
      static_resource_bytes?(
        File.read!(path),
        Path.dirname(path),
        dependencies,
        MapSet.put(visited, path)
      )
    end
  end

  defp static_resource_bytes?(source, directory, dependencies, visited) do
    # Navigation links do not fetch resources while printing. SVG data images
    # need the same resource checks as external SVGs, including nested images.
    html =
      Regex.replace(~r/<(?:a|area)\b[^>]*>/is, source, fn tag ->
        Regex.replace(~r/\bhref\s*=\s*(?:"[^"]*"|'[^']*'|[^\s>]+)/i, tag, "")
      end)

    urls =
      Regex.scan(
        ~r/(?:src|href|srcset|data|poster|background)\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s>]+))|url\(\s*(?:"([^"]*)"|'([^']*)'|([^\s)]+))/i,
        html
      )

    not Regex.match?(
      ~r/<(?:[\w.-]+:)?(?:base|animate\w*|set|script)\b|srcdoc\s*=|xml:base\s*=|@import|image-set\s*\(|http-equiv\s*=|\\/i,
      html
    ) and
      Enum.all?(urls, fn [_match | captures] ->
        url = Enum.find(captures, "", &(&1 != ""))
        uri = URI.parse(url)

        cond do
          url == "" or String.starts_with?(url, "#") ->
            true

          Regex.match?(~r/^data:image\/(?:png|jpeg|gif|webp);base64,/i, url) ->
            true

          Regex.match?(~r/^data:image\/svg\+xml;base64,/i, url) ->
            with {:ok, svg} <- Base.decode64(List.last(String.split(url, ",", parts: 2))),
                 true <- String.valid?(svg),
                 false <- Regex.match?(~r/<!doctype|<\?xml-stylesheet/i, svg) do
              static_resource_bytes?(svg, directory, dependencies, visited)
            else
              _ -> false
            end

          uri.scheme not in [nil, "file"] or uri.host not in [nil, "", "localhost"] or
              String.contains?(url, ["&", " ", "\\"]) ->
            false

          true ->
            resource = URI.decode(uri.path || "") |> Path.expand(directory)

            Map.has_key?(dependencies, resource) and
              static_resources?(resource, dependencies, visited)
        end
      end)
  end

  @doc "Rejects persistence when Chromium requested any resource outside the fingerprinted trees or generated inputs."
  @spec requests_covered?([String.t()], map(), [String.t()], String.t()) :: boolean()
  def requests_covered?(requests, dependencies, inputs, directory) do
    requests != [] and
      Enum.all?(requests, fn url ->
        uri = URI.parse(url)

        case uri.scheme do
          "data" ->
            true

          "about" ->
            url == "about:blank"

          "file" ->
            path = URI.decode(uri.path || "") |> Path.expand()

            uri.host in [nil, "", "localhost"] and
              (Map.has_key?(dependencies, path) or path in inputs or
                 String.starts_with?(path, Path.expand(directory) <> "/"))

          _ ->
            false
        end
      end)
  end

  @doc "Checks the fixed cache payload vocabulary, safe basenames, page ownership and every artifact digest."
  @spec cache_payload?(term()) :: boolean()
  def cache_payload?(value) do
    case value do
      %{files: files, pages: pages, vector_pages: vectors, embedded_fonts: fonts}
      when is_list(files) and is_list(pages) and is_list(vectors) and is_binary(fonts) ->
        valid_files =
          Enum.all?(files, fn item ->
            case item do
              {name, bytes, digest}
              when is_binary(name) and is_binary(bytes) and is_binary(digest) ->
                Path.basename(name) == name and String.starts_with?(name, "chromium") and
                  :crypto.hash(:sha256, bytes) == digest

              _ ->
                false
            end
          end)

        names = for {name, _, _} <- files, do: name

        valid_files and length(Enum.uniq(names)) == length(names) and "chromium.pdf" in names and
          pages != [] and
          Enum.all?(pages ++ vectors, &(&1 in names and String.ends_with?(&1, ".ppm")))

      _ ->
        false
    end
  end
end
