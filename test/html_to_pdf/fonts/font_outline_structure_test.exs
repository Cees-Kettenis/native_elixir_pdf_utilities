Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("font_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.FontOutlineStructureTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.TrueTypeGlyph
  alias NativeElixirPdfUtilities.TestSupport.{Paths, FontFixture}
  alias NativeElixirPdfUtilities.Validators.{FontOutlineValidator, TrueTypeGlyphValidator}

  test "CFF cubic paths and off-curve TrueType start points preserve original design geometry" do
    data = File.read!(Paths.fixture("fonts/cff-static-sans.otf"))
    assert {:ok, [cff]} = Font.load_configured_faces([%{family: "Cubic", data: data}])
    assert {:ok, path} = FontOutlineValidator.prepare_path(cff, cff.cmap[?0])
    assert path =~ "M" and path =~ "C" and path =~ "Z"
    assert {:ok, path} = FontOutlineValidator.prepare_path(cff, cff.cmap[?H])
    assert path =~ "L"
    assert {:error, {:invalid_font, _}} = FontOutlineValidator.prepare_path(cff, 65535)

    data = File.read!(Paths.fixture("fonts/selected-variable.ttf"))
    assert {:ok, [font]} = Font.load_configured_faces([%{family: "Quadratic", data: data}])

    for {points, expected} <- [
          {[{0, 0, 0}, {100, 100, 0}, {200, 0, 1}], "M200 0Q0 0 50 50Q100 100 200 0Z"},
          {[{0, 0, 0}, {100, 100, 1}, {200, 0, 0}], "M100 0Q0 0 100 100Q200 0 100 0Z"}
        ] do
      selected = outline_face(font, simple(points))
      assert {:ok, path} = FontOutlineValidator.prepare_path(selected, font.cmap[?H])
      assert String.replace(path, ~r/([0-9]+)\.0(?![0-9.])/, "\\1") == expected
    end

    bad = %{simple([]) | contours: -1, components: [component(2, true, {0, 0}, 2)]}
    bad = %{bad | components: [%{hd(bad.components) | flags: 3}]}
    selected = outline_face(font, bad)

    assert {:error, {:invalid_font, %{message: message}}} =
             FontOutlineValidator.prepare_path(selected, font.cmap[?H])

    assert message =~ "cycle"
  end

  test "composite point attachment, scaled translations and cached dependency depth share one resolver" do
    NativeElixirPdfUtilities.Validators.FontValidator.with_budget(fn ->
      child = simple([{0, 0, 1}, {100, 100, 1}])

      parent = %{
        simple([])
        | contours: -1,
          components: [component(0, true, {10, 20}, 2050), component(0, false, {1, 0}, 0)]
      }

      parent = %{
        parent
        | components: [
            %{hd(parent.components) | matrix: {0.5, 0, 0, 0.5}} | tl(parent.components)
          ]
      }

      {resolved, cache} = TrueTypeGlyphValidator.resolve(1, {child, parent}, %{}, MapSet.new(), 0)
      assert resolved.points == [{5, 10, 1}, {55, 60, 1}, {55, 60, 1}, {155, 160, 1}]
      assert resolved.ends == [1, 3]

      assert {resolved, cache} ==
               TrueTypeGlyphValidator.resolve(1, {child, parent}, cache, MapSet.new(), 0)

      invalid = %{parent | components: [component(0, false, {0, 0}, 0)]}

      assert {:invalid_glyph, {:error, {:invalid_font, _}}} =
               catch_throw(
                 TrueTypeGlyphValidator.resolve(1, {child, invalid}, %{}, MapSet.new(), 0)
               )

      large = simple(List.duplicate({0, 0, 1}, 40000))

      parent = %{
        parent
        | components: [component(0, true, {0, 0}, 2), component(0, true, {0, 0}, 2)]
      }

      assert {:invalid_glyph, {:error, {:invalid_font, _}}} =
               catch_throw(
                 TrueTypeGlyphValidator.resolve(1, {large, parent}, %{}, MapSet.new(), 0)
               )
    end)
  end

  test "glyph byte streams admit transform forms and reject invalid contours, repeat flags and accumulated coordinates" do
    head = <<1::signed-16, 0::64>>

    for bytes <- [
          nil,
          <<1>>,
          <<-2::signed-16, 0::64>>,
          <<1::signed-16, 10::signed-16, 0::signed-16, 0::32>>,
          <<2::signed-16, 0::64, 1::16, 0::16, 0::16>>,
          head <> <<0::16, 0::16, 128>>,
          head <> <<0::16, 0::16, 9, 1>>,
          <<1::signed-16, 0::64, 1::16, 0::16, 1, 1, 32767::signed-16, 1::signed-16, 0::32>>,
          <<0::signed-16, 0::64, 0::16, 1>>
        ] do
      assert {:error, {:invalid_font, _}} = TrueTypeGlyphValidator.prepare(bytes, 2)
    end

    assert {:error, {:invalid_font, _}} = TrueTypeGlyphValidator.prepare(<<>>, 0)

    assert {:error, {:invalid_font, _}} =
             TrueTypeGlyphValidator.prepare(<<-1::signed-16, 0::64, 0xE003::16, 0::16, 0::32>>, 2)

    for {flag, args, transform} <- [
          {0, <<0, 0>>, <<>>},
          {1, <<0::16, 0::16>>, <<>>},
          {2, <<0, 0>>, <<>>},
          {0x10B, <<0::16, 0::16>>, <<8192::signed-16>>},
          {0x43, <<0::16, 0::16>>, <<8192::signed-16, 16384::signed-16>>},
          {0x83, <<0::16, 0::16>>, <<16384::signed-16, 0::16, 0::16, 16384::signed-16>>}
        ] do
      instructions = if Bitwise.band(flag, 256) != 0, do: <<1::16, 0>>, else: <<>>

      assert {:ok, %{components: [_]}} =
               TrueTypeGlyphValidator.prepare(
                 <<-1::signed-16, 0::64, flag::16, 0::16, args::binary, transform::binary,
                   instructions::binary>>,
                 2
               )
    end
  end

  defp simple(points) do
    %{
      contours: if(points == [], do: 0, else: 1),
      points: points,
      ends: if(points == [], do: [], else: [length(points) - 1]),
      instructions: <<>>,
      components: [],
      bbox: {0, 0, 200, 200}
    }
  end

  defp component(id, xy, args, flags) do
    %{id: id, xy?: xy, args: args, flags: flags, matrix: {1, 0, 0, 1}, transform: <<>>}
  end

  defp outline_face(font, glyph) do
    bytes = TrueTypeGlyph.serialize(glyph)
    # Keep six glyph slots; only the authored H at index two owns these bytes.
    offsets = [0, 0, 0, byte_size(bytes), byte_size(bytes), byte_size(bytes), byte_size(bytes)]
    {_, tables} = FontFixture.tables(font.data)
    head = FontFixture.replace(tables["head"], 50, <<1::16>>)

    %{
      font
      | data:
          FontFixture.rebuild(font.data, %{
            "head" => head,
            "glyf" => bytes,
            "loca" => for(n <- offsets, into: <<>>, do: <<n::32>>)
          })
    }
  end
end
