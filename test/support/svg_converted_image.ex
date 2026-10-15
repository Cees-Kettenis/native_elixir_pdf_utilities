defmodule NativeElixirPdfUtilities.TestSupport.SvgConvertedImage do
  @moduledoc false
  import ExUnit.Assertions
  alias NativeElixirPdfUtilities.{HtmlToPdf, Pdf.Reader}

  @doc false
  @spec render(String.t()) :: %{
          data: binary(),
          alpha: binary(),
          width: pos_integer(),
          height: pos_integer()
        }
  def render(html) do
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    assert pdf =~ "/Subtype /Image"
    refute pdf =~ "/Subtype /Type3"
    assert {:ok, document} = Reader.read(pdf)

    {reference, image} =
      Enum.find(document.objects, fn {_reference, object} ->
        match?(
          %{value: %{"Subtype" => {:name, "Image"}, "ColorSpace" => {:name, "DeviceRGB"}}},
          object
        )
      end)

    assert image.value["Filter"] in [nil, {:name, "FlateDecode"}]
    assert {:ok, data} = Reader.decoded_stream(document, {:ref, reference})
    assert byte_size(data) == image.value["Width"] * image.value["Height"] * 3

    alpha =
      case image.value["SMask"] do
        {:ref, _} = reference ->
          mask = document.objects[elem(reference, 1)]
          assert mask.value["Filter"] in [nil, {:name, "FlateDecode"}]
          assert {:ok, alpha} = Reader.decoded_stream(document, reference)
          assert byte_size(alpha) == image.value["Width"] * image.value["Height"]
          alpha

        _ ->
          :binary.copy(<<255>>, image.value["Width"] * image.value["Height"])
      end

    %{data: data, alpha: alpha, width: image.value["Width"], height: image.value["Height"]}
  end

  @doc false
  @spec coverage(%{alpha: binary()}) :: non_neg_integer()
  def coverage(image), do: Enum.count(:binary.bin_to_list(image.alpha), &(&1 > 0))
end
