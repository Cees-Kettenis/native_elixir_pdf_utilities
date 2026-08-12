defmodule NativeElixirPdfUtilities.Validators.WriterValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.WriterValidator

  test "prepares valid pages and normalized metadata for serialization" do
    pages = [%{size: {100.0, 100.0}, boxes: []}]

    assert {:ok, %{pages: ^pages, metadata: %{keywords: "one, two", creation_date: "D:20260812"}}} =
             WriterValidator.prepare(pages,
               metadata: [keywords: ["one", "two"], creation_date: ~D[2026-08-12]]
             )
  end

  test "rejects invalid writer colors before serialization" do
    pages = [
      %{
        size: {100.0, 100.0},
        boxes: [
          %{
            type: :text,
            text: "Invalid",
            x: 10.0,
            y: 20.0,
            font: "Helvetica",
            font_size: 12.0,
            color: {1.1, 0, 0}
          }
        ]
      }
    ]

    assert {:error, {:invalid_pdf_input, %{stage: :pdf}}} =
             WriterValidator.prepare(pages, [])
  end
end
