defmodule NativeElixirPdfUtilities.HtmlToPdf.GvarStructureTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.GvarValidator

  test "private and shared point runs interpolate equal, reversed and interior contour coordinates" do
    glyph = %{contours: 1, points: [{0, 0, 1}, {50, 0, 1}, {100, 0, 1}, {50, 100, 1}], ends: [3]}
    # Points 0 and 2 receive different x deltas. Equal y reference coordinates
    # with different deltas leave unreferenced y coordinates unchanged.
    payload = <<2, 1, 0, 2, 1, 10, 30, 1, 20, 40>>

    data =
      table(<<1::16, 10::16, byte_size(payload)::16, 0xA000::16, 16384::16, payload::binary>>)

    assert {:ok, [%{glyph: result}]} =
             GvarValidator.prepare(data, {glyph}, [{0, 600, 800, -200}], [1])

    assert result.points == [{10.0, 20.0, 1}, {70.0, 0.0, 1}, {130.0, 40.0, 1}, {70.0, 100.0, 1}]

    shared = <<2, 0x81, 0::16, 2::16>>
    deltas = <<1, 10, 30, 1, 20, 20>>

    data =
      table(
        <<0x8001::16, 10::16, byte_size(deltas)::16, 0x8000::16, 16384::16, shared::binary,
          deltas::binary>>
      )

    assert {:ok, [%{glyph: result}]} =
             GvarValidator.prepare(data, {glyph}, [{0, 600, 800, -200}], [1])

    assert result.points == [{10.0, 20.0, 1}, {70.0, 20.0, 1}, {130.0, 20.0, 1}, {70.0, 120.0, 1}]

    # Both reference coordinates equal, with equal deltas, shift the full contour.
    vertical = %{glyph | points: [{0, 0, 1}, {0, 50, 1}, {0, 100, 1}, {0, 150, 1}]}

    assert {:ok, [%{glyph: result}]} =
             GvarValidator.prepare(data, {vertical}, [{0, 600, 800, -200}], [1])

    assert result.points == [{10.0, 20.0, 1}, {0.0, 70.0, 1}, {30.0, 120.0, 1}, {0.0, 170.0, 1}]
  end

  test "long point counts, word and expansion-spec long deltas retain exact signed values" do
    glyph = %{contours: 1, points: List.duplicate({0, 0, 1}, 130), ends: [129]}
    # 130 private indices, packed in 128 + 2 byte-index runs.
    points = <<0x80, 130, 127, 0>> <> :binary.copy(<<1>>, 127) <> <<1, 1, 1>>

    xs =
      <<0x7F>> <>
        :binary.copy(<<300::signed-16>>, 64) <>
        <<0xFF>> <>
        :binary.copy(<<-40000::signed-32>>, 64) <> <<0x41, -20::signed-16, 20::signed-16>>

    ys = <<0xBF, 0xBF, 0x81>>
    payload = points <> xs <> ys

    data =
      table(
        <<1::16, 14::16, byte_size(payload)::16, 0xE000::16, 16384::16, 0::16, 16384::16,
          payload::binary>>
      )

    assert {:ok, [%{glyph: result}]} =
             GvarValidator.prepare(data, {glyph}, [{0, 600, 800, -200}], [0.5])

    assert Enum.at(result.points, 0) == {150.0, 0.0, 1}
    assert Enum.at(result.points, 64) == {-20000.0, 0.0, 1}
    assert Enum.at(result.points, 129) == {10.0, 0.0, 1}
  end

  test "malformed table ownership, tuple headers, point runs and payloads return actionable diagnostics" do
    glyph = %{contours: 1, points: [{0, 0, 1}], ends: [0]}
    valid = <<1::16, 10::16, 2::16, 0x8000::16, 16384::16, 0x84, 0x84>>

    invalid = [
      <<>>,
      <<1::16, 0::16, 2::16, 0::16, 0::32, 1::16, 0::16, 0::32>>,
      table(valid, 1, <<20000::signed-16>>),
      table(<<0x1000::16, 4::16>>),
      table(<<1::16, 10::16, 2::16, 0x9000::16, 16384::16, 0x84, 0x84>>),
      table(<<1::16, 8::16, 2::16, 0::16, 0x84, 0x84>>),
      table(<<1::16, 10::16, 2::16, 0x8000::16, 20000::signed-16, 0x84, 0x84>>),
      table(<<1::16, 14::16, 2::16, 0xC000::16, 0::16, 16384::16, 0::16, 0x84, 0x84>>),
      table(<<1::16, 4::16, 2::16, 0x8000::16, 16384::16, 0x84, 0x84>>),
      table(valid <> <<1, 1>>),
      table(<<1::16, 10::16, 3::16, 0x8000::16, 16384::16, 0x84, 0x84, 0>>),
      table(<<1::16, 10::16, 2::16, 0x8000::16, 16384::16, 0x85, 0x84>>)
    ]

    for points <- [<<6>>, <<1, 1, 0, 1>>, <<2, 1, 0, 0>>, <<1, 0, 5>>] do
      payload = points <> <<0x80, 0x80>>

      data =
        table(<<1::16, 10::16, byte_size(payload)::16, 0xA000::16, 16384::16, payload::binary>>)

      assert {:error, {:invalid_font, %{stage: :font, message: message}}} =
               GvarValidator.prepare(data, {glyph}, [{0, 600, 800, -200}], [1])

      assert message != ""
    end

    for data <- invalid do
      assert {:error, {:invalid_font, %{stage: :font, operation: :instantiate_font}}} =
               GvarValidator.prepare(data, {glyph}, [{0, 600, 800, -200}], [1])
    end

    assert {:error, {:invalid_font, _}} = GvarValidator.prepare(nil, {glyph}, [], [1])

    assert {:ok, [%{glyph: ^glyph, phantoms: {0, 600, 800, -200}}]} =
             GvarValidator.prepare(nil, {glyph}, [{0, 600, 800, -200}], [1])

    # Short offsets are measured in two-byte units.
    short =
      <<1::16, 0::16, 1::16, 0::16, 0::32, 1::16, 0::16, 24::32, 0::16, 6::16, valid::binary>>

    assert {:ok, [_]} = GvarValidator.prepare(short, {glyph}, [{0, 600, 800, -200}], [1])

    assert {:error, {:invalid_font, _}} =
             GvarValidator.prepare(
               binary_part(short, 0, 20) <> <<1::16, 6::16>> <> valid,
               {glyph},
               [{0, 600, 800, -200}],
               [1]
             )
  end

  defp table(payload, shared_count \\ 0, shared \\ <<>>) do
    <<1::16, 0::16, 1::16, shared_count::16, 28::32, 1::16, 1::16, 28 + byte_size(shared)::32,
      0::32, byte_size(payload)::32, shared::binary, payload::binary>>
  end
end
