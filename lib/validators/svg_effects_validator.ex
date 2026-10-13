defmodule NativeElixirPdfUtilities.Validators.SvgEffectsValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    SvgSceneValidator,
    SvgStyleValidator,
    SvgTransformValidator,
    SvgViewportValidator
  }

  @doc false
  @spec validate_summary(map()) :: :ok | {:error, {atom(), map()}}
  def validate_summary(summary) do
    if summary.context.mode == :inline,
      do: validate_tree(SvgSceneValidator.summary_tree(summary)),
      else: :ok
  end

  @doc false
  @spec validate_tree(map()) :: :ok | {:error, {atom(), map()}}
  def validate_tree(root) do
    nodes = SvgSceneValidator.tree_nodes(root)
    ids = Map.new(nodes, fn {node, _} -> {Map.get(node.attributes, "id"), node.tag} end)

    Enum.reduce_while(nodes, :ok, fn {node, _}, :ok ->
      result =
        with :ok <- validate_definition(node, ids) do
          Enum.reduce_while(~w(mask marker-start marker-mid marker-end), :ok, fn property, :ok ->
            reference =
              case Map.get(node, :style) do
                nil ->
                  SvgStyleValidator.parse(property, Map.get(node.attributes, property, "none"))

                style ->
                  {:ok,
                   Map.get(
                     style,
                     Map.fetch!(SvgStyleValidator.presentation_properties(), property),
                     :none
                   )}
              end

            case reference do
              {:ok, :none} ->
                {:cont, :ok}

              {:ok, {:fragment, id}} ->
                if Map.get(ids, id) == if(property == "mask", do: "mask", else: "marker"),
                  do: {:cont, :ok},
                  else:
                    {:halt,
                     invalid(
                       "SVG #{property} must identify a same-scene #{if(property == "mask", do: "mask", else: "marker")}",
                       node.tag <> " " <> property
                     )}

              _ ->
                {:halt,
                 invalid(
                   "SVG #{property} requires none or a local marker reference",
                   node.tag <> " " <> property
                 )}
            end
          end)
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp validate_definition(node, ids) do
    attrs = node.attributes
    href = Map.get(attrs, "href", Map.get(attrs, "xlink:href"))

    geometry =
      case node.tag do
        "pattern" -> ~w(x y width height)
        "mask" -> ~w(x y width height)
        "marker" -> ~w(refX refY markerWidth markerHeight)
        _ -> []
      end

    result =
      Enum.reduce_while(geometry, :ok, fn property, :ok ->
        case Map.get(attrs, property) do
          nil ->
            {:cont, :ok}

          value ->
            if match?({:ok, _}, SvgStyleValidator.length(value, property in ~w(x y refX refY))),
              do: {:cont, :ok},
              else:
                {:halt,
                 invalid(
                   "SVG #{property} requires a finite bounded geometry length",
                   node.tag <> " " <> property
                 )}
        end
      end)

    with :ok <- result do
      cond do
        node.tag == "pattern" and href != nil and
            not (String.starts_with?(href, "#") and
                     Map.get(ids, String.trim_leading(href, "#")) == "pattern") ->
          invalid("SVG pattern inheritance requires a same-scene pattern target", "pattern href")

        node.tag == "pattern" and
            (Map.get(attrs, "patternUnits", "objectBoundingBox") not in ~w(objectBoundingBox userSpaceOnUse) or
               Map.get(attrs, "patternContentUnits", "userSpaceOnUse") not in ~w(objectBoundingBox userSpaceOnUse)) ->
          invalid("SVG pattern units require userSpaceOnUse or objectBoundingBox", "pattern")

        node.tag == "pattern" and
            SvgTransformValidator.parse(Map.get(attrs, "patternTransform", "none")) == :error ->
          invalid(
            "SVG patternTransform requires a bounded affine transform",
            "pattern patternTransform"
          )

        node.tag == "mask" and
            (Map.get(attrs, "maskUnits", "objectBoundingBox") not in ~w(objectBoundingBox userSpaceOnUse) or
               Map.get(attrs, "maskContentUnits", "userSpaceOnUse") not in ~w(objectBoundingBox userSpaceOnUse)) ->
          invalid("SVG mask units require userSpaceOnUse or objectBoundingBox", "mask")

        node.tag == "mask" and Map.get(attrs, "mask-type", "luminance") not in ~w(luminance alpha) ->
          invalid("SVG mask-type requires luminance or alpha", "mask mask-type")

        node.tag == "marker" and
            Map.get(attrs, "markerUnits", "strokeWidth") not in ~w(strokeWidth userSpaceOnUse) ->
          invalid("SVG markerUnits requires strokeWidth or userSpaceOnUse", "marker markerUnits")

        node.tag == "marker" and not valid_orientation?(Map.get(attrs, "orient", "0")) ->
          invalid(
            "SVG orient requires a finite angle, auto or auto-start-reverse",
            "marker orient"
          )

        node.tag in ~w(pattern marker) ->
          with {:ok, _} <- SvgViewportValidator.view_box(Map.get(attrs, "viewBox")),
               {:ok, _} <-
                 SvgViewportValidator.aspect_ratio(Map.get(attrs, "preserveAspectRatio")) do
            :ok
          else
            _ ->
              invalid(
                "SVG #{node.tag} requires valid viewBox and preserveAspectRatio geometry",
                node.tag
              )
          end

        true ->
          :ok
      end
    end
  end

  defp valid_orientation?(value) do
    value in ~w(auto auto-start-reverse) or
      case HtmlValidator.parse_css_number(value) do
        {_, unit} -> unit in ["", "deg", "rad", "grad", "turn"]
        _ -> false
      end
  end

  defp invalid(message, source) do
    Diagnostics.error(:layout, :invalid_document, message,
      source: source,
      module: __MODULE__,
      operation: :prepare
    )
  end
end
