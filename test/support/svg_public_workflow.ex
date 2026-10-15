defmodule NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.{ResourceContext, SvgRasterizer, SvgScene}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc false
  @spec pdf(binary(), keyword(), map()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def pdf(source, _former_vector_options \\ [], context \\ %{}) do
    options = Keyword.put(converter_options(context), :margin, 0)

    HtmlToPdf.render(source, options)
  end

  @doc false
  @spec png(binary(), keyword(), map()) :: {:ok, binary()} | {:error, {atom(), map()}}
  def png(source, dimensions \\ [], context \\ %{}) do
    options = converter_options(context)

    budget = HtmlValidator.new_image_budget()

    ResourceContext.run(options, budget, fn _scope ->
      with {:ok, scene} <- SvgScene.extract(source, budget, options) do
        SvgRasterizer.rasterize(
          scene.document_source,
          dimensions,
          budget,
          scene.selected_fonts,
          %{
            mode: Map.get(context, :mode, :inline),
            resources: scene.resources
          }
        )
      end
    end)
  end

  # The former backend accepted byte-only face descriptors. Derive missing
  # CSS family names from those actual bytes, preserving every supplied name.
  defp converter_options(context) do
    fonts =
      Enum.map(Map.get(context, :fonts, []), fn font ->
        case Map.fetch(font, :family) do
          {:ok, _} ->
            Map.take(font, [:data, :family, :weight, :style])

          :error ->
            {:ok, family} =
              NativeElixirPdfUtilities.Validators.FontValidator.prepare_font_family_name(
                font.data
              )

            font |> Map.take([:data, :weight, :style]) |> Map.put(:family, family)
        end
      end)

    assets =
      Map.new(Map.get(context, :resources, %{}), fn {name, resource} ->
        {name, {:bytes, resource.data}}
      end)

    options = [fonts: fonts, assets: assets, system_font_discovery: false]
    if fonts == [], do: options, else: Keyword.put(options, :default_font, hd(fonts).family)
  end

  @doc false
  @spec document(binary(), term(), binary(), atom(), list()) ::
          {:ok, map()} | {:error, {atom(), map()}}
  def document(source, budget, origin \\ "SVG", host \\ :object, fonts \\ []) do
    fonts =
      Enum.map(fonts, fn font ->
        case Map.fetch(font, :family) do
          {:ok, _family} ->
            font

          :error ->
            {:ok, family} =
              NativeElixirPdfUtilities.Validators.FontValidator.prepare_font_family_name(
                font.data
              )

            Map.put(font, :family, family)
        end
      end)

    NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentImage.prepare(
      source,
      budget,
      origin,
      host,
      fonts
    )
  end

  @doc false
  @spec compose_document(map(), number(), number()) ::
          {:ok, map() | :skipped} | {:error, {atom(), map()}}
  def compose_document(request, width, height) do
    NativeElixirPdfUtilities.HtmlToPdf.SvgDocumentImage.compose(request, width, height)
  end

  @doc false
  @spec image_pdf(map(), number(), number()) :: binary()
  def image_pdf(image, width, height) do
    {:ok, pdf} =
      NativeElixirPdfUtilities.HtmlToPdf.PdfWriter.render([
        %{
          size: {width, height},
          boxes: [%{type: :image, image: image, x: 0, y: 0, width: width, height: height}]
        }
      ])

    pdf
  end

  @doc false
  @spec assert_image(binary(), boolean()) :: :ok
  def assert_image(pdf, visible \\ true) do
    import ExUnit.Assertions
    assert {:ok, document} = NativeElixirPdfUtilities.Pdf.Reader.read(pdf)

    images =
      for {reference, %{value: value} = image} <- document.objects,
          value["Subtype"] == {:name, "Image"} and value["ColorSpace"] == {:name, "DeviceRGB"},
          do: {reference, image}

    assert images != []

    planes =
      Enum.map(images, fn {reference, image} ->
        assert image.value["Filter"] in [nil, {:name, "FlateDecode"}]

        assert {:ok, pixels} =
                 NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(document, {:ref, reference})

        assert byte_size(pixels) == image.value["Width"] * image.value["Height"] * 3

        alpha =
          case image.value["SMask"] do
            {:ref, reference} ->
              mask = document.objects[reference]
              assert mask.value["Filter"] in [nil, {:name, "FlateDecode"}]

              assert {:ok, alpha} =
                       NativeElixirPdfUtilities.Pdf.Reader.decoded_stream(
                         document,
                         {:ref, reference}
                       )

              assert byte_size(alpha) == image.value["Width"] * image.value["Height"]
              alpha

            nil ->
              :binary.copy(<<255>>, image.value["Width"] * image.value["Height"])
          end

        {pixels, alpha}
      end)

    if visible,
      do:
        assert(
          Enum.any?(planes, fn {_pixels, alpha} ->
            Enum.any?(:binary.bin_to_list(alpha), &(&1 > 0))
          end)
        )

    :ok
  end
end
