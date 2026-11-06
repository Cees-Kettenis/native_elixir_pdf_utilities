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
end
