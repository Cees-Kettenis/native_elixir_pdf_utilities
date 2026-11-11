defmodule NativeElixirPdfUtilities.HtmlToPdf.BrotliTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.BrotliValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "independent Google codec and synthetic format vectors decode exactly or explain rejection" do
    vectors =
      Enum.flat_map(["brotli-vectors.json", "brotli-edge-vectors.json"], fn path ->
        manifest = Paths.fixture("fonts/" <> path) |> File.read!() |> :json.decode()
        manifest["vectors"]
      end)

    for vector <- vectors do
      input = Base.decode64!(vector["compressed"])

      case vector["expected_zlib"] do
        :null ->
          assert {:error, {_reason, diagnostic}} =
                   BrotliValidator.decompress(input, Map.get(vector, "expected_size", 1_000_000)),
                 vector["name"]

          assert diagnostic.stage == :font
          assert is_binary(diagnostic.message)

        encoded ->
          expected = encoded |> Base.decode64!() |> :zlib.uncompress()

          assert {:ok, ^expected} = BrotliValidator.decompress(input, byte_size(expected)),
                 vector["name"]
      end
    end
  end

  test "length ownership, alignment, truncation and caller types have actionable diagnostics" do
    for {input, expected} <- [
          {nil, 0},
          {<<6>>, -1},
          {<<6>>, :size},
          {<<>>, 0},
          {<<6>>, 1},
          {<<6, 0>>, 0},
          {<<134>>, 0},
          {<<17>>, 0},
          {<<0x8B, 2, 128, "abcdef", 3>>, 5},
          {<<0x8B, 2, 128, "abc">>, 6}
        ] do
      assert {:error, {:invalid_font, diagnostic}} = BrotliValidator.decompress(input, expected)
      assert diagnostic.stage == :font
      assert diagnostic.reason == :invalid_font
      assert diagnostic.operation == :decode_brotli
      assert diagnostic.source =~ "Brotli bit"
      assert diagnostic.message != ""
    end
  end

  test "source, decoded-size and cumulative work allowances stop decoding" do
    for {limit, value, input, expected} <- [
          {:max_font_source_bytes, 1, <<6, 0>>, 0},
          {:max_font_decoded_bytes, 1, <<6>>, 2},
          {:max_font_brotli_work, 1, <<6>>, 0}
        ] do
      original = Limits.effective()
      Limits.install(%{original | limit => value})

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               BrotliValidator.decompress(input, expected)

      assert diagnostic.stage == :font
      assert diagnostic.message =~ Atom.to_string(limit)
      Limits.install(original)
    end
  end
end
