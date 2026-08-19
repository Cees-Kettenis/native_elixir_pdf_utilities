defmodule NativeElixirPdfUtilities.HtmlToPdf.AssetLoaderTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.AssetLoader
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "resolves explicit bytes trusted files and authorized local references" do
    directory = Path.join(System.tmp_dir!(), "native-elixir-pdf-asset-loader")
    path = Path.join(directory, "asset.bin")
    File.mkdir_p!(directory)
    File.write!(path, "file bytes")

    assert {:ok, "inline bytes"} =
             AssetLoader.resolve(
               "asset:inline",
               :image,
               [assets: %{"asset:inline" => {:bytes, "inline bytes"}}],
               HtmlValidator.new_image_budget()
             )

    assert {:ok, "file bytes"} =
             AssetLoader.resolve(
               "asset:file",
               :image,
               [assets: %{"asset:file" => {:file, path}}],
               HtmlValidator.new_image_budget()
             )

    assert {:ok, "file bytes"} =
             AssetLoader.resolve("asset.bin", :font, base_url: directory)

    file_uri = "file://#{path}"

    assert {:error, {:invalid_document, %{stage: :asset, source: ^file_uri}}} =
             AssetLoader.resolve(file_uri, :font, base_url: directory)
  after
    File.rm_rf(Path.join(System.tmp_dir!(), "native-elixir-pdf-asset-loader"))
  end

  test "returns actionable diagnostics for missing files resolver failures and unsupported schemes" do
    assert {:ok, "resolved bytes"} =
             AssetLoader.resolve("https://x.test/background.png", :background_image,
               asset_resolver: fn %{kind: :background_image} -> {:ok, "resolved bytes"} end
             )

    assert {:error, {:invalid_document, %{stage: :asset, source: "asset:missing"}}} =
             AssetLoader.resolve(
               "asset:missing",
               :background_image,
               assets: %{"asset:missing" => {:file, "/missing/file.png"}}
             )

    assert {:error, {:invalid_document, %{stage: :asset, source: "https://x.test/a.png"}}} =
             AssetLoader.resolve("https://x.test/a.png", :image,
               asset_resolver: fn _request -> :not_found end
             )

    assert {:error, {:invalid_document, %{stage: :asset}}} =
             AssetLoader.resolve("https://x.test/a.png", :image,
               asset_resolver: fn _request -> {:ok, 123} end
             )

    assert {:error, {:invalid_document, %{stage: :asset}}} =
             AssetLoader.resolve("https://x.test/font.ttf", :font,
               asset_resolver: fn _request -> {:error, :denied} end
             )

    assert {:error, {:invalid_document, %{stage: :asset}}} =
             AssetLoader.resolve("https://x.test/a.png", :image,
               asset_resolver: fn _request -> raise "failed" end
             )

    assert {:error, {:invalid_document, %{stage: :asset}}} =
             AssetLoader.resolve("https://x.test/a.png", :image,
               asset_resolver: fn _request -> throw(:failed) end
             )

    assert {:error, {:invalid_document, %{stage: :asset, source: "asset:unknown"}}} =
             AssetLoader.resolve("asset:unknown", :font, [])

    assert {:error, {:invalid_document, %{stage: :asset, source: "mailto:x@y.test"}}} =
             AssetLoader.resolve("mailto:x@y.test", :image, [])
  end

  test "propagates image budget failures before reading approved files" do
    path = Path.join(System.tmp_dir!(), "native-elixir-pdf-budgeted-asset.bin")
    File.write!(path, "bytes")

    assert {:error, {:invalid_document, %{stage: :style}}} =
             AssetLoader.resolve(
               "asset:file",
               :image,
               [assets: %{"asset:file" => {:file, path}}],
               make_ref()
             )
  after
    File.rm(Path.join(System.tmp_dir!(), "native-elixir-pdf-budgeted-asset.bin"))
  end
end
