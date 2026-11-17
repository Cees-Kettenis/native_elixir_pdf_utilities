defmodule NativeElixirPdfUtilities.HtmlToPdf.BrotliLimitsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.BrotliValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "source, decoded-size and cumulative work allowances stop decoding" do
    for {limit, value, input, expected} <- [
          {:max_input_bytes, 1, <<6, 0>>, 0},
          {:max_decoded_bytes, 1, <<6>>, 2},
          {:max_work, 1, <<6>>, 0}
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

  test "bulk history and constant literals retain exact cumulative work and partial failure charges" do
    alias NativeElixirPdfUtilities.TestSupport.{Paths, ResourceBudget}
    vectors = :json.decode(File.read!(Paths.fixture("fonts/brotli-vectors.json")))["vectors"]

    for {name, work} <- [{"encoder-8-5-17", 67_119}, {"testZeroCostLiterals", 16_778_399}] do
      vector = Enum.find(vectors, &(&1["name"] == name))
      source = Base.decode64!(vector["compressed"])
      expected = vector["expected_zlib"] |> Base.decode64!() |> :zlib.uncompress()
      original = Limits.effective()

      assert {{:ok, ^expected}, ^work} =
               ResourceBudget.measure(:max_work, fn ->
                 BrotliValidator.decompress(source, byte_size(expected))
               end)

      Limits.install(%{original | max_work: work - 512})

      assert {{:error, {:resource_limit_exceeded, diagnostic}}, charged} =
               ResourceBudget.measure(:max_work, fn ->
                 BrotliValidator.decompress(source, byte_size(expected))
               end)

      assert charged == work - 512
      assert diagnostic.stage == :font and diagnostic.message =~ "max_work"
      Limits.install(original)
    end
  end
end
