defmodule NativeElixirPdfUtilities.HtmlToPdf.VariationStoreTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits

  alias NativeElixirPdfUtilities.Validators.{
    VariationStoreValidator,
    FontVariationValidator,
    GvarValidator,
    Cff2FontValidator
  }

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "region products, mixed-width rows, map formats and CFF2 blends agree with hand-computed values" do
    data =
      <<1::16, 12::32, 1::16, 22::32, 1::16, 1::16, 0::16, 8192::16, 16384::16, 1::16, 1::16,
        1::16, 0::16, 100::signed-16>>

    assert {:ok, store, 32} = VariationStoreValidator.prepare_with_extent(data, 0, [0.75])
    assert {:ok, 50.0} = VariationStoreValidator.lookup(store, 0, 0)
    assert {:ok, [12.0, 23.0]} = VariationStoreValidator.blend([10, 20, 4, 6, 2], store, 0)
    assert {:ok, [99, 12.0]} = VariationStoreValidator.blend([99, 10, 4, 1], store, 0)
    assert {:ok, []} = VariationStoreValidator.blend([0], store, 0)
    assert {:ok, nil} = VariationStoreValidator.prepare_map(data, 0, store, 2, false)
    assert {:ok, [50.0]} = VariationStoreValidator.prepare_map(data, 0, store, 1, true)

    for map <- [<<0, 0, 1::16, 0>>, <<1, 0, 1::32, 0>>] do
      assert {:ok, [50.0, 50.0, 50.0]} =
               VariationStoreValidator.prepare_map(data <> map, byte_size(data), store, 3, false)
    end

    assert {:ok, [0]} =
             VariationStoreValidator.prepare_map(
               data <> <<0, 63, 1::16, 0xFFFFFFFF::32>>,
               byte_size(data),
               store,
               1,
               false
             )

    long =
      <<1::16, 12::32, 1::16, 16::32, 0::16, 2::16, 1::16, 0x8001::16, 2::16, 0::16, 1::16,
        1_000_000::signed-32, -20::signed-16>>

    assert {:ok, store} = VariationStoreValidator.prepare(long, 0, [])
    assert {:ok, 999_980} = VariationStoreValidator.lookup(store, 0, 0)

    assert {:ok, [10, 20]} =
             VariationStoreValidator.blend([10, 20, 2], {%{scalars: [], values: {}}}, 0)

    for {region, value, expected} <- [
          {[{0, 0, 1}], 0.5, 1},
          {[{-1, 0.5, 1}], 0.75, 1},
          {[{0, 0.5, 1}], 0.5, 1},
          {[{0, 0.5, 1}], 0.25, 0.5},
          {[{0, 0.5, 1}], 1, 0},
          {[{0.5, 0.25, 1}], 0.2, 1}
        ] do
      assert VariationStoreValidator.scalar(region, [value]) == expected
    end
  end

  test "malformed store fields, delta maps and blend requests retain actionable diagnostics" do
    data =
      <<1::16, 12::32, 1::16, 22::32, 1::16, 1::16, 0::16, 8192::16, 16384::16, 1::16, 1::16,
        1::16, 0::16, 100::signed-16>>

    assert {:ok, store} = VariationStoreValidator.prepare(data, 0, [0.75])

    for {offset, replacement} <- [
          {0, <<0::16>>},
          {12, <<2::16>>},
          {16, <<-20000::signed-16>>},
          {18, <<20000::16>>},
          {24, <<2::16>>},
          {28, <<1::16>>}
        ] do
      changed =
        binary_part(data, 0, offset) <>
          replacement <>
          binary_part(
            data,
            offset + byte_size(replacement),
            byte_size(data) - offset - byte_size(replacement)
          )

      assert {:error, {:invalid_font, diagnostic}} =
               VariationStoreValidator.prepare(changed, 0, [0.75])

      assert diagnostic.stage == :font
      assert diagnostic.message != ""
    end

    for {bytes, offset, coordinates} <- [
          {nil, 0, []},
          {data, -1, []},
          {data, 0, [0 | :improper]},
          {data, 0, [2]},
          {<<>>, 0, []}
        ] do
      assert {:error, {:invalid_font, _}} =
               VariationStoreValidator.prepare(bytes, offset, coordinates)
    end

    for map <- [
          <<2, 0>>,
          <<0, 192>>,
          <<0, 15>>,
          <<0, 0, 0::16>>,
          <<0, 0, 1::16, 2>>,
          <<0, 0, 1::16, 1>>,
          <<0>>
        ] do
      assert {:error, {:invalid_font, diagnostic}} =
               VariationStoreValidator.prepare_map(data <> map, byte_size(data), store, 1, false)

      assert diagnostic.message != ""
    end

    assert {:error, {:invalid_font, _}} =
             VariationStoreValidator.prepare_map(nil, 0, store, 1, true)

    assert {:error, {:invalid_font, _}} =
             VariationStoreValidator.prepare_map(data, 0, store, 2, true)

    for {outer, inner} <- [{-1, 0}, {1, 0}, {0, -1}, {0, 1}] do
      assert {:error, {:invalid_font, _}} = VariationStoreValidator.lookup(store, outer, inner)
    end

    for {values, index} <- [{[], 0}, {[1, 0.5], 0}, {[1], 0}, {[1], 1}] do
      assert {:error, {:invalid_font, _}} = VariationStoreValidator.blend(values, store, index)
    end
  end

  test "all variation consumers reject improper coordinates and reserve work before allocating maps" do
    for coordinates <- [[0 | :improper], :invalid, [2], ["0"]] do
      assert {:error, {:invalid_font, _}} =
               FontVariationValidator.prepare_normalized_coordinates(coordinates)

      assert {:error, {:invalid_font, _}} = GvarValidator.prepare(nil, {}, [], coordinates)
      assert {:error, {:invalid_font, _}} = Cff2FontValidator.prepare(<<>>, 1, coordinates)
    end

    assert {:error, {:invalid_font, _}} =
             FontVariationValidator.prepare_request([{"wght", 1} | :improper], nil)

    Limits.install(Map.put(Limits.effective(), :max_work, 1))

    assert {:error, {:resource_limit_exceeded, _}} =
             FontVariationValidator.prepare_normalized_coordinates([0, 0])

    assert {:error, {:resource_limit_exceeded, _}} =
             FontVariationValidator.prepare_request([{"wght", 1}, {"wdth", 2}], nil)

    assert {:error, {:resource_limit_exceeded, _}} =
             VariationStoreValidator.prepare_map(<<>>, 0, {}, 2, true)
  end
end
