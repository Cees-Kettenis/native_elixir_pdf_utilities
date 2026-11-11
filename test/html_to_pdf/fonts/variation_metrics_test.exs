defmodule NativeElixirPdfUtilities.HtmlToPdf.VariationMetricsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.TestSupport.Paths

  test "vertical advances, VORG origins and MVAR gasp ranges match independent variation-store results" do
    manifest =
      Paths.fixture("fonts/variation-metrics-fonts.json") |> File.read!() |> :json.decode()

    for {file, expected} <- manifest["files"] do
      data = Paths.fixture("fonts/" <> file) |> File.read!()

      assert {:ok, [font]} =
               Font.load_configured_faces([
                 %{family: "Metrics", data: data, variation_settings: %{"wght" => 900}}
               ])

      assert {:ok, _, directory} = FontValidator.prepare_table_directory(font.data)

      if Map.has_key?(expected, "gasp") do
        {offset, size} = directory["gasp"]
        <<1::16, count::16, records::binary>> = binary_part(font.data, offset, size)
        assert count == 2
        assert for(<<ppem::16, _flags::16 <- records>>, do: ppem) == expected["gasp"]
      else
        {offset, size} = directory["vmtx"]
        metrics = binary_part(font.data, offset, size)

        assert Enum.all?(
                 for(<<advance::16, bearing::signed-16 <- metrics>>, do: {advance, bearing}),
                 &(&1 == {expected["advance"], expected["bearing"]})
               )

        {offset, size} = directory["VORG"]

        <<1::16, 0::16, default::signed-16, count::16, records::binary>> =
          binary_part(font.data, offset, size)

        records = Map.new(for <<id::16, value::signed-16 <- records>>, do: {id, value})
        assert count == 1

        for {name, id} <- Enum.with_index(expected["glyph_order"]),
            do: assert(Map.get(records, id, default) == expected["origins"][name])
      end
    end
  end
end
