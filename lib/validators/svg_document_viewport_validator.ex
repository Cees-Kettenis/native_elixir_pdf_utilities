defmodule NativeElixirPdfUtilities.Validators.SvgDocumentViewportValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, SvgViewport}

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgSceneValidator,
    SvgStyleValidator,
    SvgTransformValidator,
    SvgValidator,
    SvgViewportValidator
  }

  @lengths ~w(x y x1 y1 x2 y2 cx cy r rx ry width height stroke-width stroke-dashoffset stroke-dasharray)

  @typedoc "Internal canonical plan retained in the validator-issued SVG document request."
  @type plan :: %{
          attributes: %{String.t() => String.t()},
          inverse: {number(), number(), number(), number(), number(), number()},
          width: number(),
          height: number(),
          surface_factor: non_neg_integer()
        }

  @type raster :: %{
          width: pos_integer(),
          height: pos_integer(),
          matrix: {number(), number(), number(), number(), number(), number()}
        }

  @doc """
  Internal preparation of intrinsic metadata and the pinned tree's original
  viewport transform without raster allocation. Accepts only the bounded XML
  summary issued by SvgValidator.validate_processing/2. Application callers
  enter through SvgBackend.prepare_document/3 and its validated source contract.
  """
  @spec prepare(SvgValidator.document_summary()) ::
          {:ok, plan()} | {:error, {atom(), map()}}
  def prepare(summary) do
    # The XML backend uses case-sensitive, unqualified SVG root attributes.
    # Keep the original names alongside the existing folded summary metadata.
    attributes = summary.source_root

    with {:ok, root} <-
           SvgViewportValidator.prepare(attributes, %{}, %{width: 225.0, height: 112.5}),
         :ok <- validate_source_geometry(summary.source_nodes, root, attributes),
         {:ok, natural} <- SvgViewport.measure(attributes, %{}, %{width: 225.0, height: 112.5}),
         true <- natural.width > 0 and natural.height > 0,
         {original_width, original_height} <-
           if(root.view_box,
             do: {elem(root.view_box, 2), elem(root.view_box, 3)},
             else: {100.0, 100.0}
           ),
         {:ok, original} <-
           SvgViewport.measure(attributes, %{svg_x: {:user, 0.0}, svg_y: {:user, 0.0}}, %{
             width: original_width,
             height: original_height,
             nested: true
           }),
         true <- original.paint?,
         {:ok, inverse} <- SvgTransformValidator.inverse(original.ctm) do
      {:ok,
       %{
         attributes: attributes,
         inverse: inverse,
         width: natural.width,
         height: natural.height,
         surface_factor: 75 * summary.expanded_work
       }}
    else
      {:error, {_, _}} = error ->
        error

      _ ->
        invalid(
          "selected SVG root requires positive intrinsic dimensions and an invertible viewBox transform"
        )
    end
  end

  # Percent geometry without a viewBox is resolved by usvg against its original
  # root canvas, rather than the eventual child viewport. Root CSS sizing changes
  # that original transform too. Do not claim either profile through a bitmap fit.
  defp validate_source_geometry(nodes, root, attributes) do
    Enum.reduce_while(nodes, :ok, fn node, :ok ->
      with {:ok, declarations} <-
             CssParser.parse_declarations_detailed(Map.get(node.attributes, "style", "")),
           :ok <-
             (cond do
                node.index == 1 and
                    ((root.width == :auto and Map.has_key?(attributes, "width")) or
                       (root.height == :auto and Map.has_key?(attributes, "height")) or
                       match?({unit, _} when unit in [:percent, :svg_em, :svg_rem], root.width) or
                       match?({unit, _} when unit in [:percent, :svg_em, :svg_rem], root.height) or
                       Enum.any?(
                         declarations,
                         &(elem(&1, 0) in ~w(width height x y transform transform-origin))
                       )) ->
                  unsupported(
                    "selected SVG root percentage/font-relative dimensions and CSS viewport sizing require proven used geometry"
                  )

                root.view_box == nil and
                    ((node.tag == "svg" and node.index != 1) or
                       (node.tag in ~w(linearGradient radialGradient) and
                          Map.get(node.attributes, "gradientunits") == "userSpaceOnUse") or
                       Enum.any?(node.attributes, fn {name, value} ->
                         (name in @lengths and String.contains?(value, "%")) or
                             (node.tag in ~w(linearGradient radialGradient) and
                                Map.get(node.attributes, "gradientunits") == "userSpaceOnUse" and
                                String.contains?(value, "%"))
                       end) or
                       Enum.any?(declarations, fn declaration ->
                         elem(declaration, 0) in @lengths and
                             String.contains?(elem(declaration, 1), "%")
                       end)) ->
                  unsupported(
                    "selected SVG geometry without viewBox cannot resolve viewport-relative percentages or nested viewports against the child yet"
                  )

                Enum.any?(node.attributes, fn {name, value} ->
                  name in @lengths and
                      match?(
                        {:ok, {unit, _}} when unit in [:svg_em, :svg_rem],
                        SvgStyleValidator.length(value, true)
                      )
                end) or
                    Enum.any?(declarations, fn declaration ->
                      elem(declaration, 0) in @lengths and
                          match?(
                            {:ok, {unit, _}} when unit in [:svg_em, :svg_rem],
                            SvgStyleValidator.length(elem(declaration, 1), true)
                          )
                    end) ->
                  unsupported(
                    "selected SVG font-relative geometry requires its actual document font context"
                  )

                SvgTransformValidator.parse(Map.get(node.attributes, "transform", "none")) ==
                    :error ->
                  invalid("selected SVG transform requires a complete bounded transform list")

                true ->
                  if node.tag in ~w(svg g defs symbol use path rect circle ellipse line polyline polygon title desc),
                    do: SvgSceneValidator.validate_node(node),
                    else: :ok
              end) do
        {:cont, :ok}
      else
        error -> {:halt, error}
      end
    end)
  end

  @doc """
  Internal validation of the actual child viewport and its canvas allocation.
  The plan must be the canonical result of prepare/1 retained in the issued
  request. SvgBackend.paint_document/3 resolves that request through validator
  authority before calling this helper; caller-authored maps are not plans.
  """
  @spec prepare_raster(plan(), term(), term(), HtmlValidator.image_budget()) ::
          {:ok, raster()} | {:error, {atom(), map()}}
  def prepare_raster(plan, width, height, budget) do
    with {:ok, {width, height}} <- HtmlValidator.prepare_viewport_dimensions(width, height),
         true <- width > 0 and height > 0,
         {:ok, target} <-
           SvgViewport.measure(
             plan.attributes,
             %{svg_width: {:points, width}, svg_height: {:points, height}},
             %{width: width, height: height}
           ),
         {a, b, c, d, e, f} <- SvgTransformValidator.multiply(target.ctm, plan.inverse),
         raster_width <- max(ceil(width / 0.75 * Limits.get(:svg_raster_scale)), 1),
         raster_height <- max(ceil(height / 0.75 * Limits.get(:svg_raster_scale)), 1),
         sx <- raster_width / width,
         sy <- raster_height / height,
         matrix <- {a * sx, b * sy, c * sx, d * sy, e * sx, f * sy},
         true <-
           Enum.all?(
             Tuple.to_list(matrix),
             &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
           ),
         :ok <- SvgValidator.validate_raster_dimensions(raster_width, raster_height, budget),
         :ok <-
           HtmlValidator.reserve_render_resource(
             :max_aggregate_svg_surface_pixels,
             raster_width * raster_height * plan.surface_factor,
             :layout
           ) do
      {:ok, %{width: raster_width, height: raster_height, matrix: matrix}}
    else
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
