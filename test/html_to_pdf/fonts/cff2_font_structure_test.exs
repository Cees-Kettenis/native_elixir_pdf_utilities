Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("cff_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.Cff2FontStructureTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.TestSupport.CffFixture

  alias NativeElixirPdfUtilities.Validators.{
    Cff2FontValidator,
    CffFontValidator,
    CffCharstringValidator
  }

  test "static CFF2, implicit subroutine returns, FDSelect formats and large-store sentinel preserve their owners" do
    store =
      <<1::16, 12::32, 1::16, 22::32, 1::16, 1::16, 0::16, 16384::16, 16384::16, 0::16, 0::16,
        1::16, 0::16>>

    for options <- [
          [],
          [local_subrs: [<<139, 139, 21>>], charstrings: [<<32, 10>>, <<>>]],
          [global_subrs: [<<139, 139, 21>>], charstrings: [<<32, 29>>, <<>>]],
          [store: store],
          [store: store, store_length: 65535],
          [font_count: 2, fd_select: <<4, 1::32, 0::32, 1::16, 2::32>>],
          [fd_select: <<0, 0, 0>>]
        ] do
      data = CffFixture.face2_fixture(options)
      coordinates = if Keyword.has_key?(options, :store), do: [1], else: []
      assert {:ok, face} = Cff2FontValidator.prepare(data, 2, coordinates)
      assert tuple_size(face.fonts) == Keyword.get(options, :font_count, 1)
      assert {:ok, evaluated} = CffCharstringValidator.prepare(data, 2, coordinates)
      assert tuple_size(evaluated.glyphs) == 2
    end

    assert {:ok, {1, 1}, _} =
             CffFontValidator.prepare_fd_select(<<4, 1::32, 0::32, 1::16, 2::32>>, 0, 2, 2, 2)
  end

  test "CFF2 dictionaries, indexes, private data and selected store failures return actionable diagnostics" do
    store =
      <<1::16, 12::32, 1::16, 22::32, 1::16, 1::16, 0::16, 16384::16, 16384::16, 0::16, 0::16,
        1::16, 0::16>>

    cases = [
      {nil, 2, []},
      {<<>>, 2, []},
      {CffFixture.face2_fixture(), 0, []},
      {CffFixture.face2_fixture(top: %{24 => [-1]}), 2, []},
      {CffFixture.face2_fixture(top: %{25 => [514]}), 2, []},
      {CffFixture.face2_fixture(), 3, []},
      {CffFixture.face2_fixture(font_count: 0), 2, []},
      {CffFixture.face2_fixture(font_count: 2), 2, []},
      {CffFixture.face2_fixture(font_dictionary: %{18 => [1]}), 2, []},
      {CffFixture.face2_fixture(private: %{19 => [-1]}), 2, []},
      {CffFixture.face2_fixture(top: %{{12, 37} => [-1]}), 2, []},
      {CffFixture.face2_fixture(top: %{17 => [-1]}), 2, []},
      {CffFixture.face2_fixture(top: %{{12, 7} => [1]}), 2, []},
      {CffFixture.face2_fixture(top: %{{12, 7} => [0, 0, 0, 0, 0, 0]}), 2, []},
      {CffFixture.face2_fixture(fd_select: <<4, 0::32>>), 2, []},
      {CffFixture.face2_fixture(store: <<0::16>>), 2, []},
      {CffFixture.face2_fixture(
         store: <<2::16, binary_part(store, 2, byte_size(store) - 2)::binary>>
       ), 2, [1]},
      {CffFixture.face2_fixture(store: store), 2, []},
      {CffFixture.face2_fixture(store: store, private: %{22 => [10]}), 2, [1]},
      {CffFixture.face2_fixture(store: store <> <<0>>, store_length: 65535, top: %{17 => [0]}), 2,
       [1]}
    ]

    stored =
      <<1::16, 12::32, 1::16, 22::32, 1::16, 1::16, 0::16, 16384::16, 16384::16, 1::16, 1::16,
        1::16, 0::16, 4::16>>

    cases = cases ++ [{CffFixture.face2_fixture(store: stored), 2, [1]}]

    assert {:error, {:invalid_font, _}} =
             CffFontValidator.prepare_dictionary(<<140, 23, 6>>, %{
               version: 2,
               kind: :private,
               vsindex: 0,
               store: {%{scalars: [1], rows: {}, word_count: 0}}
             })

    for {data, count, coordinates} <- cases do
      result = Cff2FontValidator.prepare(data, count, coordinates)
      assert match?({:error, {_, _}}, result), inspect({count, coordinates, result})
      {:error, {_, diagnostic}} = result
      assert diagnostic.stage == :font
      assert diagnostic.message != ""
    end

    for bytes <- [
          <<2, 0, 4, 0, 0>>,
          <<2, 0, 5, 0, 6, 29, 0, 0, 0, 0, 17, 0, 0, 0, 0>>,
          <<2, 0, 5, 0, 0, 0, 0, 0, 1, 0>>
        ] do
      assert {:error, {reason, _}} = Cff2FontValidator.prepare(bytes, 2, [])
      assert reason in [:invalid_font, :resource_limit_exceeded]
    end

    assert {:error, {:invalid_font, _}} =
             CffFontValidator.prepare(
               CffFixture.face_fixture(private: %{{12, 19} => [2_147_483_648.0]}),
               3
             )
  end

  test "CFF2 private blends and single-use vsindex agree across DICT and charstring execution" do
    store = {%{scalars: [0.5], values: {}, word_count: 0}}
    context = %{version: 2, kind: :private, store: store, vsindex: 0}

    assert {:ok, %{22 => [0], 10 => [12.0]}} =
             CffFontValidator.prepare_dictionary(<<139, 22, 149, 143, 140, 23, 10>>, context)

    for data <- [
          <<139, 22, 139, 22>>,
          <<140, 22>>,
          <<149, 143, 140, 23, 10, 139, 22>>,
          <<139, 10, 139, 10>>,
          <<139, 12, 7, 139, 12, 7>>
        ] do
      assert {:error, {:invalid_font, _}} = CffFontValidator.prepare_dictionary(data, context)
    end

    rawstore =
      <<1::16, 12::32, 1::16, 22::32, 1::16, 1::16, 0::16, 16384::16, 16384::16, 0::16, 0::16,
        1::16, 0::16>>

    for program <- [
          <<139, 15, 139, 15>>,
          <<140, 15>>,
          <<139, 16, 139, 15>>,
          <<140, 16>>,
          <<11>>,
          <<14>>,
          <<139, 12, 23>>
        ] do
      data = CffFixture.face2_fixture(store: rawstore, charstrings: [program, <<>>])
      assert {:error, {:invalid_font, diagnostic}} = CffCharstringValidator.prepare(data, 2, [1])
      assert diagnostic.message != ""
    end

    data =
      CffFixture.face2_fixture(
        store: rawstore,
        charstrings: [<<139, 15, 139, 139, 21>>, <<>>],
        private: %{22 => [0]}
      )

    assert {:ok, _} = CffCharstringValidator.prepare(data, 2, [1])
  end
end
