defmodule NativeElixirPdfUtilities.HtmlToPdf.CffCharstringTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.TestSupport.{CffFixture, Paths}
  alias NativeElixirPdfUtilities.Validators.CffCharstringValidator

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "real font programs retain independently measured advances and cubic bounds" do
    data = File.read!(Paths.fixture("fonts/cff-static-sans.cff"))
    expected = Paths.fixture("fonts/cff-static-sans.json") |> File.read!() |> :json.decode()
    assert {:ok, face} = CffCharstringValidator.prepare(data, 5)

    for {name, index} <- Enum.with_index(expected["glyph_order"]) do
      actual = elem(face.glyphs, index)
      assert actual.width == expected["glyphs"][name]["advance"]

      for {actual, expected} <-
            Enum.zip(Tuple.to_list(actual.bbox), expected["glyphs"][name]["bounds"]) do
        assert_in_delta actual, expected, 1.0e-8
      end
    end
  end

  test "path operators, flex and owned subroutines agree with independent fontTools vectors" do
    corpus = Paths.fixture("fonts/cff-type2-vectors.json") |> File.read!() |> :json.decode()
    local = Enum.map(corpus["local_subrs"], &Base.decode16!(&1, case: :mixed))
    global = Enum.map(corpus["global_subrs"], &Base.decode16!(&1, case: :mixed))

    for vector <- corpus["vectors"] do
      data =
        CffFixture.face_fixture(
          glyph_count: 1,
          charstrings: [Base.decode16!(vector["program"], case: :mixed)],
          local_subrs: local,
          global_subrs: global
        )

      assert {:ok, face} = CffCharstringValidator.prepare(data, 1), vector["name"]
      actual = elem(face.glyphs, 0)
      assert actual.width == vector["width"], vector["name"]

      for {actual, expected} <- Enum.zip(Tuple.to_list(actual.bbox), vector["bounds"]) do
        assert_in_delta actual, expected, 1.0e-8, vector["name"]
      end

      commands =
        Enum.map(vector["commands"], fn [op, points] ->
          case {op, points} do
            {"moveTo", [[x, y]]} -> {:move, x, y}
            {"lineTo", [[x, y]]} -> {:line, x, y}
            {"curveTo", [[a, b], [c, d], [e, f]]} -> {:curve, a, b, c, d, e, f}
            {"closePath", []} -> :close
          end
        end)

      assert actual.commands == commands, vector["name"]
    end
  end

  test "arithmetic, storage, logic and stack operators feed the same outline state" do
    # Each expression leaves its result as the horizontal coordinate of a move.
    expressions = [
      {[2, 3, {:op, 10}], 5},
      {[2, 3, {:op, 11}], -1},
      {[8, 2, {:op, 12}], 4},
      {[2, 3, {:op, 24}], 6},
      {[-2, {:op, 9}], 2},
      {[2, {:op, 14}], -2},
      {[9, {:op, 26}], 3},
      {[0, {:op, 5}], 1},
      {[2, {:op, 5}], 0},
      {[1, 2, {:op, 3}], 1},
      {[0, 2, {:op, 3}], 0},
      {[0, 2, {:op, 4}], 1},
      {[0, 0, {:op, 4}], 0},
      {[2, 2, {:op, 15}], 1},
      {[2, 3, {:op, 15}], 0},
      {[42, 3, {:op, 20}, 3, {:op, 21}], 42},
      {[10, 20, 1, 2, {:op, 22}], 10},
      {[10, 20, 2, 1, {:op, 22}], 20},
      {[7, {:op, 27}, {:op, 10}], 14},
      {[1, 2, {:op, 28}, {:op, 11}], 1},
      {[2, 5, 1, {:op, 29}, {:op, 10}, {:op, 10}], 9},
      {[2, -1, {:op, 29}, {:op, 10}], 4},
      {[2, 5, 2, 1, {:op, 30}, {:op, 11}], 3},
      {[2, 5, 2, -1, {:op, 30}, {:op, 11}], 3},
      {[7, 0, 2, {:op, 30}], 7},
      {[7, 9, {:op, 18}], 7}
    ]

    for {expression, expected} <- expressions do
      program = numbers(expression ++ [0]) <> <<21>> <> numbers([1, 0]) <> <<5, 14>>

      assert {:ok, face} =
               CffCharstringValidator.prepare(
                 CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
                 1
               )

      assert [{:move, x, 0}, {:line, finish, 0}, :close] = elem(face.glyphs, 0).commands
      assert x == expected and finish == expected + 1
    end
  end

  test "widths are read only at the first stack-clearing operator and masks skip owned bytes" do
    for program <- [
          numbers([500, 10, 20]) <> <<1, 19, 0x80>> <> numbers([1, 2]) <> <<21, 14>>,
          numbers([500, 10]) <> <<22, 14>>,
          numbers([500, 10]) <> <<4, 14>>,
          numbers([500]) <> <<14>>
        ] do
      assert {:ok, face} =
               CffCharstringValidator.prepare(
                 CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
                 1
               )

      assert elem(face.glyphs, 0).width == 600
    end

    program = numbers([0, 0]) <> <<21>> <> numbers([10, 20]) <> <<18, 20, 0x80, 14>>

    assert {:ok, _} =
             CffCharstringValidator.prepare(
               CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
               1
             )

    # A move with no drawing does not create ink.
    assert {:ok, face} =
             CffCharstringValidator.prepare(
               CffFixture.face_fixture(
                 glyph_count: 1,
                 charstrings: [numbers([500, 500]) <> <<21, 14>>]
               ),
               1
             )

    assert elem(face.glyphs, 0).bbox == {0, 0, 0, 0}
  end

  test "invalid programs return actionable diagnostics without escaping the bounded interpreter" do
    programs = [
      <<>>,
      <<11>>,
      <<28>>,
      <<255>>,
      <<0>>,
      <<12>>,
      <<12, 1>>,
      <<139, 10, 14>>,
      <<12, 10, 14>>,
      numbers([3]) <> <<12, 21, 14>>,
      numbers([1, 0]) <> <<12, 12, 14>>,
      numbers([-1]) <> <<12, 26, 14>>,
      numbers([32]) <> <<12, 21, 14>>,
      numbers([0.5]) <> <<12, 21, 14>>,
      numbers([1, 2]) <> <<5, 14>>,
      numbers([1]) <> <<21, 14>>,
      numbers([1, 2]) <> <<1, 19>>,
      numbers([1, 2, 3, 4]) <> <<21, 14>>,
      numbers(List.duplicate(0, 49)) <> <<14>>,
      numbers([1]) <> <<12, 29, 14>>,
      numbers([1, 2]) <> <<12, 29, 14>>,
      numbers([1, 2, 3]) <> <<12, 30, 14>>,
      numbers([0, 0]) <> <<21>> <> numbers([1]) <> <<5, 14>>,
      numbers([0, 0]) <> <<21>> <> numbers([1]) <> <<8, 14>>
    ]

    for program <- programs do
      assert {:error, {:invalid_font, diagnostic}} =
               CffCharstringValidator.prepare(
                 CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
                 1
               )

      assert %{
               stage: :font,
               reason: :invalid_font,
               operation: :prepare_cff_outlines,
               source: source,
               message: message
             } = diagnostic

      assert is_binary(source) and is_binary(message)
    end

    assert {:ok, face} =
             CffCharstringValidator.prepare(
               CffFixture.face_fixture(glyph_count: 1, charstrings: [<<12, 23, 14>>]),
               1
             )

    assert elem(face.glyphs, 0).static?
    assert elem(face.glyphs, 0).width > 100
    assert elem(face.glyphs, 0).width <= 101
  end

  test "recursive subroutines, expanded work and coordinates stop at their owning limits" do
    data =
      CffFixture.face_fixture(
        glyph_count: 1,
        charstrings: [numbers([-107]) <> <<10, 14>>],
        local_subrs: [numbers([-107]) <> <<10, 11>>]
      )

    assert {:error, {:invalid_font, %{message: message}}} =
             CffCharstringValidator.prepare(data, 1)

    assert message =~ "ten-level"
    data = CffFixture.face_fixture(glyph_count: 1, charstrings: [numbers([100, 0]) <> <<21, 14>>])
    Limits.install(%{Limits.effective() | max_numeric_magnitude: 10})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             CffCharstringValidator.prepare(data, 1)

    assert message =~ "max_numeric_magnitude"
    Limits.install(%{Limits.defaults() | max_work: 1})
    assert {:error, {:resource_limit_exceeded, _}} = CffCharstringValidator.prepare(data, 1)
  end

  test "legacy seac resolves StandardEncoding components and rejects nested or missing dependencies" do
    # StandardEncoding A/B name SIDs are 34/35, irrespective of the font encoding.
    base = numbers([0, 0]) <> <<21>> <> numbers([10, 0, 0, 20]) <> <<5, 14>>
    accent = numbers([0, 0]) <> <<21>> <> numbers([5, 0, 0, 5]) <> <<5, 14>>
    composite = numbers([20, 30, 65, 66]) <> <<14>>

    data =
      CffFixture.face_fixture(
        glyph_count: 4,
        charset: <<0, 34::16, 35::16, 36::16>>,
        charstrings: [<<14>>, base, accent, composite]
      )

    assert {:ok, face} = CffCharstringValidator.prepare(data, 4)
    assert elem(face.glyphs, 3).bbox == {0, 0, 25, 35}
    assert elem(face.glyphs, 3).component_depth == 2
    assert {:move, 20, 30} in elem(face.glyphs, 3).commands
    Limits.install(%{Limits.effective() | max_depth: 1})
    assert {:error, {:resource_limit_exceeded, _}} = CffCharstringValidator.prepare(data, 4)
    Limits.install(Limits.defaults())

    for options <- [
          [glyph_count: 1, charstrings: [composite]],
          [
            glyph_count: 3,
            charset: <<0, 34::16, 35::16>>,
            charstrings: [<<14>>, composite, base]
          ],
          [glyph_count: 1, charstrings: [numbers([0, 0, 256, 65]) <> <<14>>]],
          [cid: true, glyph_count: 1, charstrings: [composite]]
        ] do
      assert {:error, {:invalid_font, %{stage: :font}}} =
               CffCharstringValidator.prepare(
                 CffFixture.face_fixture(options),
                 options[:glyph_count]
               )
    end
  end

  test "subroutine bias boundaries and fixed program sizes are enforced" do
    for {count, bias} <- [{1240, 1131}, {33900, 32768}] do
      program = numbers([-bias]) <> <<10, 14>>

      data =
        CffFixture.face_fixture(
          glyph_count: 1,
          charstrings: [program],
          local_subrs: List.duplicate(<<11>>, count)
        )

      assert {:ok, _} = CffCharstringValidator.prepare(data, 1)
    end

    for opts <- [
          [charstrings: [:binary.copy(<<139>>, 65536)]],
          [charstrings: [<<14>>], local_subrs: [:binary.copy(<<139>>, 65536)]]
        ] do
      assert {:error, {:invalid_font, %{message: message}}} =
               CffCharstringValidator.prepare(
                 CffFixture.face_fixture([glyph_count: 1] ++ opts),
                 1
               )

      assert message =~ "65,535-byte"
    end
  end

  test "remaining drawing operand contracts and deprecated dotsection are explicit" do
    for operator <- [24, 25, 26, 27, 30, 31] do
      program = numbers([0, 0]) <> <<21>> <> numbers([1]) <> <<operator, 14>>

      assert {:error, {:invalid_font, _}} =
               CffCharstringValidator.prepare(
                 CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
                 1
               )
    end

    for operator <- [34, 35, 36, 37] do
      program = numbers([0, 0]) <> <<21, 12, operator, 14>>

      assert {:error, {:invalid_font, _}} =
               CffCharstringValidator.prepare(
                 CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
                 1
               )
    end

    for program <- [<<12, 0, 14>>, numbers([0, 0]) <> <<21, 12, 0, 14>>] do
      assert {:ok, _} =
               CffCharstringValidator.prepare(
                 CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
                 1
               )
    end

    for program <- [
          numbers([0, 0]) <> <<21>> <> numbers([1]) <> <<1, 14>>,
          numbers([1, 2, 3]) <> <<14>>
        ] do
      assert {:error, {:invalid_font, _}} =
               CffCharstringValidator.prepare(
                 CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
                 1
               )
    end
  end

  test "numeric overflow and affine overflow remain recoverable even with enlarged configured allowances" do
    Limits.install(%{
      Limits.effective()
      | max_numeric_magnitude: round(1.0e308)
    })

    program =
      numbers([32767.0] ++ List.flatten(List.duplicate([{:op, 27}, {:op, 24}], 7))) <> <<14>>

    assert {:error, {:invalid_font, %{message: message}}} =
             CffCharstringValidator.prepare(
               CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
               1
             )

    assert message =~ "arithmetic"

    data =
      CffFixture.face_fixture(
        cid: true,
        glyph_count: 1,
        charstrings: [numbers([100, 0]) <> <<21>> <> numbers([1, 1]) <> <<5, 14>>],
        fd_matrix: [1.0e200, 0, 0, 1, 0, 0],
        top: %{{12, 7} => [1.0e200, 0, 0, 1, 0, 0]}
      )

    assert {:error, {:invalid_font, %{message: message}}} =
             CffCharstringValidator.prepare_metrics(data, 1, 1000)

    assert message =~ "FontMatrix"
  end

  test "seac resolves cubic components and budget charges accumulate across glyph programs" do
    curve = numbers([0, 0]) <> <<21>> <> numbers([10, 20, 30, 40, 50, 60]) <> <<8, 14>>
    composite = numbers([10, 10, 65, 66]) <> <<14>>

    data =
      CffFixture.face_fixture(
        glyph_count: 4,
        charset: <<0, 34::16, 35::16, 36::16>>,
        charstrings: [<<14>>, curve, curve, composite]
      )

    assert {:ok, face} = CffCharstringValidator.prepare(data, 4)
    assert {:curve, 20, 30, 50, 70, 100, 130} in elem(face.glyphs, 3).commands
    Limits.install(%{Limits.effective() | max_work: 500})
    program = :binary.copy(<<12, 0>>, 500) <> <<14>>

    assert {:error, {:resource_limit_exceeded, _}} =
             CffCharstringValidator.prepare(
               CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
               1
             )
  end

  test "hint declarations share the fixed 96-stem protocol ceiling" do
    stems = numbers(List.duplicate(1, 48)) <> <<1>>
    program = :binary.copy(stems, 4) <> <<19>> <> :binary.copy(<<255>>, 12) <> <<14>>

    assert {:ok, _} =
             CffCharstringValidator.prepare(
               CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
               1
             )

    program = :binary.copy(stems, 4) <> numbers([1, 1]) <> <<1, 14>>

    assert {:error, {:invalid_font, %{message: message}}} =
             CffCharstringValidator.prepare(
               CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
               1
             )

    assert message =~ "96-hint"
  end

  test "nearly quadratic cubic extrema retain the small interior root without cancellation" do
    # y(t) approaches 300*t*(1-t), whose maximum is 75 at t=1/2.
    # A nonzero final epsilon must not make the stable root disappear.
    epsilon = [
      1,
      1000,
      {:op, 12},
      1000,
      {:op, 12},
      1000,
      {:op, 12},
      1000,
      {:op, 12},
      100,
      {:op, 11}
    ]

    program = numbers([0, 0]) <> <<21>> <> numbers([10, 100, 10, 0, 10] ++ epsilon) <> <<8, 14>>

    assert {:ok, face} =
             CffCharstringValidator.prepare(
               CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
               1
             )

    assert {0, 0, 30, maximum} = elem(face.glyphs, 0).bbox
    assert_in_delta maximum, 75, 1.0e-10
    # A zero double root stays at the endpoint and must not divide by zero.
    program = numbers([0, 0]) <> <<21>> <> numbers([0, 0, 0, 0, 10, 10]) <> <<8, 14>>

    assert {:ok, face} =
             CffCharstringValidator.prepare(
               CffFixture.face_fixture(glyph_count: 1, charstrings: [program]),
               1
             )

    assert elem(face.glyphs, 0).bbox == {0, 0, 10, 10}
  end

  defp numbers(values) do
    Enum.map(values, fn value ->
      case value do
        {:op, byte} -> <<12, byte>>
        value when is_integer(value) and value in -32768..32767 -> <<28, value::signed-16>>
        value when is_float(value) -> <<255, round(value * 65536)::signed-32>>
      end
    end)
    |> IO.iodata_to_binary()
  end
end
