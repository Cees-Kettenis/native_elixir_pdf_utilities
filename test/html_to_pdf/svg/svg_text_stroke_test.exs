alias NativeElixirPdfUtilities.TestSupport.Paths

Code.require_file(Paths.support("svg_converted_image.ex"))
Code.require_file(Paths.support("svg_public_workflow.ex"))
Code.require_file(Paths.support("png_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgTextStrokeTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{data: File.read!("priv/fonts/dejavu/DejaVuSans.ttf"), limits: limits}
  end

  test("stroke font embedding flags remain governed by the shared source policy", %{data: data}) do
    <<_::binary-size(4), count::16, _::binary-size(6), records::binary>> = data

    table =
      for <<tag::binary-size(4), _checksum::32, offset::32,
            _length::32 <- binary_part(records, 0, count * 16)>>,
          into: %{} do
        {tag, offset}
      end

    offset = Map.fetch!(table, "OS/2") + 8

    source =
      "<svg width='150' height='60'><text y='35' font-family='NativeSvgFont0' fill='none' stroke='red'>Original Ω</text></svg>"

    for flags <- [2, 512, 256] do
      modified =
        binary_part(data, 0, offset) <>
          <<flags::16>> <> binary_part(data, offset + 2, byte_size(data) - offset - 2)

      result =
        NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.pdf(source, [], %{
          mode: :inline,
          fonts: [%{data: modified}]
        })

      if flags == 256 do
        assert {:ok, pdf} = result
        assert :ok = NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow.assert_image(pdf)

        assert {:error, {:no_extractable_text, _}} =
                 NativeElixirPdfUtilities.Text.extract(pdf, layout: false)
      else
        assert {:error, {:invalid_document, diagnostic}} = result
        assert diagnostic.stage == :font
        assert diagnostic.message =~ "does not permit outline embedding"
      end
    end
  end
end
