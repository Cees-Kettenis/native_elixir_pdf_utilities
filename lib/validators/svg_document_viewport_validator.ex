defmodule NativeElixirPdfUtilities.Validators.SvgDocumentViewportValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.SvgViewport

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssValueValidator,
    SvgStyleValidator,
    SvgValidator,
    SvgViewportValidator
  }

  @typedoc "Internal canonical plan retained in the validator-issued SVG document request."
  @type plan :: %{
          attributes: %{String.t() => String.t()},
          host: :object | :embed | :iframe | :frame,
          root_style: map(),
          width: number(),
          height: number()
        }

  @type raster :: %{
          width: pos_integer(),
          height: pos_integer(),
          root_size: {number(), number()}
        }

  @typedoc "Validated SVG root attributes and their optional prepared host CSS dimensions."
  @type source_viewport :: %{
          required(:source_root) => %{String.t() => String.t()},
          optional(:root_style) => map()
        }

  @doc """
  Prepares intrinsic metadata from the validated SVG root and host CSS dimensions.
  The validated host determines whether the root uses document sizing or the
  replaced child viewport. Selected documents enter through the PNG document adapter.
  """
  @spec prepare(source_viewport()) :: {:ok, plan()} | {:error, {atom(), map()}}
  @spec prepare(source_viewport(), term()) ::
          {:ok, plan()} | {:error, {atom(), map()}}
  def prepare(summary, host \\ :object) do
    attributes = summary.source_root

    css_context =
      CssValueValidator.policy_style()
      |> Map.merge(SvgStyleValidator.defaults(%{}))
      |> Map.merge(%{_svg_element: true, _svg_prepare: true, _svg_inherited: %{}})

    with true <- host in [:object, :embed, :iframe, :frame],
         {:ok, root_css} <-
           CssValueValidator.validate_declarations_in_context(
             Map.get(attributes, "style", ""),
             css_context,
             HtmlValidator.new_css_budget()
           ),
         root_style <-
           root_css._svg_specified
           |> Map.keys()
           |> Enum.filter(&(&1 in ~w(width height)))
           |> Enum.map(&String.to_existing_atom("svg_" <> &1))
           |> then(&Map.take(root_css, &1))
           |> Map.merge(Map.get(summary, :root_style, %{})),
         {:ok, natural} <-
           SvgViewport.measure(attributes, root_style, %{width: 225.0, height: 112.5}) do
      {:ok,
       %{
         attributes: attributes,
         root_style: root_style,
         host: host,
         width: natural.width,
         height: natural.height
       }}
    else
      {:error, {_, _}} = error ->
        error

      _ ->
        invalid("selected SVG document requires a supported host and bounded intrinsic geometry")
    end
  end

  @doc "Validates the actual flex formatting context retained for initial SVG document canvas layout."
  @spec prepare_layout_context(term()) :: {:ok, map() | nil} | {:error, {atom(), map()}}
  def prepare_layout_context(context) do
    valid? =
      case context do
        nil ->
          true

        %{main_axis: axis, alignment: alignment, containing_height: containing, height: height} =
            context
        when map_size(context) == 4 ->
          axis in [:row, :column] and
            alignment in [:stretch, :flex_start, :flex_end, :center, :baseline] and
            Enum.all?([containing, height], fn value ->
              is_nil(value) or
                (is_number(value) and value >= 0 and
                   value <= Limits.get(:max_css_numeric_magnitude) and
                   match?({:ok, _}, HtmlValidator.normalize_geometry_number(value)))
            end)

        _ ->
          false
      end

    if valid?,
      do: {:ok, context},
      else:
        Diagnostics.error(
          :layout,
          :invalid_document,
          "selected SVG canvas requires its actual flex axis, alignment and finite containing/used height",
          operation: :rasterize_document,
          module: __MODULE__,
          source: "SVG"
        )
  end

  @doc """
  Resolves the actual child viewport and initial used SVG root without allocating pixels.
  The plan must be the canonical result of prepare/2 retained in the issued
  request. The PNG document adapter resolves that request through validator
  authority before calling this helper; caller-authored maps are not plans.
  """
  @spec prepare_viewport(plan(), term(), term(), HtmlValidator.image_budget()) ::
          {:ok, map() | :skipped} | {:error, {atom(), map()}}
  @spec prepare_viewport(plan(), term(), term(), HtmlValidator.image_budget(), map() | nil) ::
          {:ok, map() | :skipped} | {:error, {atom(), map()}}
  def prepare_viewport(plan, width, height, budget, context \\ nil) do
    with {:ok, {width, height}} <- HtmlValidator.prepare_viewport_dimensions(width, height),
         :ok <- HtmlValidator.validate_image_budget(budget),
         :ok <-
           if(
             is_nil(context) or
               (is_map(context) and map_size(context) == 2 and context[:host] == plan.host),
             do: :ok,
             else: invalid("selected SVG raster context must retain its original issued host")
           ),
         {:ok, layout} <-
           prepare_layout_context(if(is_nil(context), do: nil, else: context[:layout])),
         # A stretched embed in an indefinite flex row initializes its document
         # canvas before the host cross size becomes definite.
         canvas_height <-
           (case {plan.host, layout} do
              {:embed,
               %{main_axis: :row, alignment: :stretch, containing_height: nil, height: nil}} ->
                elem(
                  NativeElixirPdfUtilities.Validators.EmbeddedViewportValidator.intrinsic_size(),
                  1
                )

              _ ->
                height
            end),
         {:ok, root} <-
           SvgViewportValidator.prepare(plan.attributes, plan.root_style, %{
             width: width,
             height: canvas_height
           }),
         used_style <-
           Map.merge(
             plan.root_style,
             if(plan.host in [:object, :embed],
               do: %{svg_width: {:points, width}, svg_height: {:points, canvas_height}},
               else: %{
                 svg_width: if(root.width == :auto, do: {:percent, 1.0}, else: root.width),
                 svg_height: if(root.height == :auto, do: {:percent, 1.0}, else: root.height)
               }
             )
           ),
         {:ok, target} <-
           SvgViewport.measure(plan.attributes, used_style, %{width: width, height: canvas_height}),
         true <- width > 0 and height > 0 and target.paint?,
         :ok <-
           if(plan.host in [:iframe, :frame] and (target.width > width or target.height > height),
             do:
               unsupported(
                 "selected SVG document overflow requires its initial scrollbar geometry"
               ),
             else: :ok
           ),
         root_size <- {target.width / 0.75, target.height / 0.75},
         :ok <-
           if(
             Enum.all?(
               Tuple.to_list(root_size),
               &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
             ),
             do: :ok,
             else: invalid("selected SVG raster transform exceeds the supported numeric range")
           ),
         # Positive native viewport dimensions are represented as f32 by usvg.
         # This is the pinned representation invariant, not another tunable limit.
         :ok <-
           if(
             Enum.all?(Tuple.to_list(root_size) ++ [width / 0.75, height / 0.75], fn value ->
               <<native::float-32>> = <<value::float-32>>
               native > 0
             end),
             do: :ok,
             else:
               invalid(
                 "selected SVG root dimensions cannot be represented by the native viewport"
               )
           ) do
      {:ok, %{width: width, height: height, root_size: root_size}}
    else
      false ->
        {:ok, :skipped}

      {:error, {_, _}} = error ->
        error

      _ ->
        invalid("selected SVG raster requires a positive child viewport and a bounded transform")
    end
  rescue
    ArithmeticError ->
      invalid("selected SVG viewport arithmetic exceeds the supported numeric range")
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error,
       {reason,
        Diagnostics.with_context(diagnostic,
          source: "SVG",
          module: __MODULE__,
          operation: :rasterize_document
        )}}
  end

  @doc "Charges actual raster allocation after shared document viewport resolution."
  @spec prepare_raster(plan(), term(), term(), HtmlValidator.image_budget()) ::
          {:ok, raster() | :skipped} | {:error, {atom(), map()}}
  @spec prepare_raster(plan(), term(), term(), HtmlValidator.image_budget(), map() | nil) ::
          {:ok, raster() | :skipped} | {:error, {atom(), map()}}
  def prepare_raster(plan, width, height, budget, context \\ nil) do
    with {:ok, viewport} <- prepare_viewport(plan, width, height, budget, context) do
      case viewport do
        :skipped ->
          {:ok, :skipped}

        viewport ->
          raster_width = max(ceil(viewport.width / 0.75 * Limits.get(:svg_raster_scale)), 1)
          raster_height = max(ceil(viewport.height / 0.75 * Limits.get(:svg_raster_scale)), 1)

          with :ok <- SvgValidator.validate_raster_dimensions(raster_width, raster_height, nil),
               :ok <-
                 HtmlValidator.reserve_render_resource(
                   :max_aggregate_svg_surface_pixels,
                   raster_width * raster_height * 2,
                   :layout
                 ) do
            {:ok,
             %{
               width: raster_width,
               height: raster_height,
               root_size: viewport.root_size
             }}
          end
      end
    end
  rescue
    ArithmeticError ->
      invalid("selected SVG viewport arithmetic exceeds the supported numeric range")
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      {:error,
       {reason,
        Diagnostics.with_context(diagnostic,
          source: "SVG",
          module: __MODULE__,
          operation: :rasterize_document
        )}}
  end

  defp unsupported(message) do
    Diagnostics.error(:resource, :not_implemented, message,
      module: __MODULE__,
      operation: :prepare_document,
      source: "SVG"
    )
  end

  defp invalid(message) do
    Diagnostics.error(:layout, :invalid_document, message,
      module: __MODULE__,
      operation: :prepare_document,
      source: "SVG"
    )
  end
end
