defmodule NativeElixirPdfUtilities.Validators.CssEffectsValidator do
  @moduledoc "Static HTML filter admission and bounded effect geometry."
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}

  alias NativeElixirPdfUtilities.Validators.{
    CssSyntaxValidator,
    CssLengthValidator,
    CssMathValidator,
    CssTextShadowValidator,
    HtmlValidator,
    WriterValidator
  }

  @amounts ~w(brightness contrast grayscale invert opacity saturate sepia)
  @doc "Parses ordered CSS filter functions using the shared lexical, length and color validators."
  @spec prepare(map(), binary(), binary()) :: {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(style, property, source) do
    key = if property == "filter", do: :css_filter, else: :css_backdrop_filter
    source = HtmlValidator.trim_css_whitespace(source)

    if String.downcase(source, :ascii) == "none" do
      {:ok, Map.put(style, key, [])}
    else
      components =
        source
        |> CssSyntaxValidator.components()
        |> Enum.reject(&(HtmlValidator.trim_css_whitespace(&1.text) == "" or &1.kind == :comment))

      HtmlValidator.check_render_resource(:max_entries, length(components), :style)

      Enum.reduce_while(components, {:ok, []}, fn component, {:ok, operations} ->
        result =
          case component do
            %{kind: :block, closed: true, opener: opener, children: children} ->
              with {:ok, name, "("} <- HtmlValidator.parse_css_identifier(opener) do
                argument =
                  Enum.map_join(children, & &1.text) |> HtmlValidator.trim_css_whitespace()

                operation(String.downcase(name, :ascii), argument, style)
              else
                _ -> :error
              end

            %{kind: :url, text: text} ->
              operation("url", text, style)

            _ ->
              :error
          end

        case result do
          {:ok, operation} -> {:cont, {:ok, [operation | operations]}}
          error -> {:halt, error}
        end
      end)
      |> case do
        {:ok, []} -> :error
        {:ok, operations} -> {:ok, Map.put(style, key, Enum.reverse(operations))}
        error -> error
      end
    end
  end

  defp operation(name, source, style) do
    case name do
      name when name in @amounts ->
        source = if source == "", do: "1", else: source

        with {:ok, n, type} when type == %{} and is_number(n) and n >= 0 <-
               CssMathValidator.evaluate(source, %{
                 percentage_basis: 1.0,
                 percentage_dimension: :number
               }) do
          {:ok, {name, if(name in ~w(grayscale invert opacity sepia), do: min(n, 1.0), else: n)}}
        else
          _ -> :error
        end

      "blur" ->
        with {:ok, length} <-
               CssLengthValidator.prepare(if(source == "", do: "0", else: source), :length),
             {:ok, n} when is_number(n) and n >= 0 <-
               CssLengthValidator.resolve(length, nil, style),
             do: {:ok, {name, n}}

      "hue-rotate" ->
        with {:ok, angle} <- CssMathValidator.angle(if(source == "", do: "0deg", else: source)),
             do: {:ok, {name, angle}}

      "drop-shadow" ->
        with {:ok, %{text_shadow: [shadow]}} <-
               CssTextShadowValidator.prepare(style, "text-shadow", source),
             do: {:ok, {name, shadow}},
             else: (_ -> :error)

      "url" ->
        value = if String.starts_with?(source, "url("), do: source, else: "url(" <> source <> ")"

        with {:ok, "#" <> id} when id != "" <-
               HtmlValidator.parse_background_image_reference(value),
             do: {:ok, {name, id}},
             else: (_ -> :error)

      _ ->
        :error
    end
  end

  @doc "Identifies effects that require an intermediate paint group."
  @spec active?(map()) :: boolean()
  def active?(style),
    do:
      Map.get(style, :css_filter, []) != [] or Map.get(style, :css_backdrop_filter, []) != [] or
        NativeElixirPdfUtilities.Validators.CssClipValidator.active?(style) or
        NativeElixirPdfUtilities.Validators.CssMaskValidator.active?(style)

  @doc "Validates computed filter values before layout."
  @spec valid_style?(map()) :: boolean()
  def valid_style?(style) do
    case style do
      style
      when is_map(style) and not is_map_key(style, :css_filter) and
             not is_map_key(style, :css_backdrop_filter) ->
        true

      _ ->
        Enum.all?([:css_filter, :css_backdrop_filter], fn key ->
          case Map.get(style, key, []) do
            operations when is_list(operations) ->
              length(operations) <= Limits.get(:max_entries) and
                Enum.all?(operations, fn operation ->
                  case operation do
                    {name, n} when name in @amounts or name in ["blur", "hue-rotate"] ->
                      is_number(n) and abs(n) <= Limits.get(:max_numeric_magnitude) and
                        (name == "hue-rotate" or n >= 0) and
                        (name not in ~w(grayscale invert opacity sepia) or n <= 1)

                    {"drop-shadow", %{x: x, y: y, blur: blur, color: color}} ->
                      Enum.all?(
                        [x, y, blur],
                        &(is_number(&1) and abs(&1) <= Limits.get(:max_numeric_magnitude))
                      ) and blur >= 0 and
                        (color in [:current_color, :transparent] or
                           WriterValidator.valid_color?(color))

                    {"url", id} ->
                      is_binary(id) and id != "" and String.valid?(id)

                    _ ->
                      false
                  end
                end)

            _ ->
              false
          end
        end)
    end
  end

  @doc "Binds same-document filter graph references to selected SVG sources in the same document and tree scope."
  @spec resolve_references(map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def resolve_references(tree) do
    if reference_tree?(tree) do
      nodes =
        NativeElixirPdfUtilities.Validators.SvgSceneValidator.tree_nodes(
          Map.put_new(tree, :tag, "document")
        )

      graphs =
        Enum.reduce(nodes, %{}, fn {node, _}, graphs ->
          case get_in(node, [:style, :svg_scene]) do
            %{original_tree: root, source: source} = scene ->
              root
              |> NativeElixirPdfUtilities.Validators.SvgSceneValidator.tree_nodes()
              |> Enum.reduce(graphs, fn {definition, _}, graphs ->
                if definition[:tag] == "filter" and
                     is_binary(get_in(definition, [:attributes, "id"])) do
                  key =
                    {Map.get(node, :document_id), Map.get(node, :tree_scope_id),
                     definition.attributes["id"]}

                  Map.put_new(graphs, key, {source, Map.get(scene, :resources, %{})})
                else
                  graphs
                end
              end)

            _ ->
              graphs
          end
        end)

      bind_references(tree, graphs)
    else
      {:ok, tree}
    end
  end

  defp reference_tree?(node) do
    referenced =
      case Map.get(node, :style, %{}) do
        style
        when not is_map_key(style, :css_filter) and not is_map_key(style, :css_backdrop_filter) ->
          false

        style ->
          Enum.any?(
            Map.get(style, :css_filter, []) ++ Map.get(style, :css_backdrop_filter, []),
            &match?({"url", _}, &1)
          )
      end

    referenced or
      case node do
        %{children: [_ | _] = children} -> Enum.any?(children, &reference_tree?/1)
        _ -> false
      end
  end

  defp bind_references(node, graphs) do
    with {:ok, children} <-
           Enum.reduce_while(Map.get(node, :children, []), {:ok, []}, fn child, {:ok, children} ->
             case bind_references(child, graphs) do
               {:ok, child} -> {:cont, {:ok, [child | children]}}
               error -> {:halt, error}
             end
           end) do
      node =
        if Map.has_key?(node, :children),
          do: %{node | children: Enum.reverse(children)},
          else: node

      style = Map.get(node, :style, %{})

      references =
        (Map.get(style, :css_filter, []) ++ Map.get(style, :css_backdrop_filter, []))
        |> Enum.flat_map(fn operation ->
          case operation do
            {"url", id} -> [id]
            _ -> []
          end
        end)

      Enum.reduce_while(references, {:ok, []}, fn id, {:ok, sources} ->
        case Map.fetch(graphs, {Map.get(node, :document_id), Map.get(node, :tree_scope_id), id}) do
          {:ok, source} ->
            {:cont, {:ok, [source | sources]}}

          :error ->
            {:halt,
             unsupported(
               "HTML filter reference ##{id} does not identify a selected SVG filter in this document/tree scope",
               "filter url(##{id})"
             )}
        end
      end)
      |> case do
        {:ok, []} ->
          {:ok, node}

        {:ok, sources} ->
          definitions =
            sources |> Enum.map(&elem(&1, 0)) |> Enum.uniq() |> Enum.reverse() |> Enum.join()

          resources =
            Enum.reduce(sources, %{}, fn {_, resources}, acc -> Map.merge(acc, resources) end)

          {:ok,
           %{
             node
             | style:
                 style
                 |> Map.put(:_css_effect_definitions, definitions)
                 |> Map.put(:_css_effect_resources, resources)
           }}

        error ->
          error
      end
    end
  end

  @doc "Validates a measured effect descriptor before native paint preparation."
  @spec validate_group(term()) :: :ok | {:error, {atom(), map()}}
  def validate_group(effect) do
    case effect do
      %{
        filters: filters,
        backdrop: backdrop,
        bounds: %{x: x, top: top, width: width, height: height},
        definitions: definitions,
        color: color
      } ->
        if valid_style?(%{css_filter: filters, css_backdrop_filter: backdrop}) and
             NativeElixirPdfUtilities.Validators.CssMaskValidator.valid_style?(
               Map.get(effect, :style, %{})
             ) and
             is_binary(definitions) and String.valid?(definitions) and
             WriterValidator.valid_color?(color) and is_number(width) and width >= 0 and
             is_number(height) and height >= 0 and
             Enum.all?(
               [x, top, width, height],
               &(is_number(&1) and abs(&1) <= Limits.get(:max_numeric_magnitude))
             ),
           do: :ok,
           else:
             unsupported(
               "CSS effects require validated filters and finite measured source bounds",
               "CSS effect group"
             )

      _ ->
        unsupported("CSS effects require a measured source descriptor", "CSS effect group")
    end
  end

  @doc "Checks paint combinations whose editable annotations cannot share a filtered raster."
  @spec validate_artwork([map()], keyword()) :: :ok | {:error, {atom(), map()}}
  def validate_artwork(boxes, opts) do
    Enum.reduce_while(boxes, :ok, fn box, :ok ->
      result =
        cond do
          (is_map(Map.get(box, :embedded_widgets)) or is_map(Map.get(box, :form_control))) and
              Keyword.get(opts, :forms, :interactive) != :static ->
            unsupported(
              "Filtered editable controls cannot share a raster appearance; use forms: :static for complete initial artwork",
              "filter controls"
            )

          box[:type] == :rect and
              Enum.any?(Map.get(box, :border_styles, %{}), fn {_, style} ->
                style not in [:solid, :none, :hidden]
              end) ->
            unsupported(
              "Rasterized HTML effects currently require solid source borders; use solid borders or apply the effect to an inner wrapper",
              "filter border styles"
            )

          is_list(Map.get(box, :boxes)) ->
            validate_artwork(box.boxes, opts)

          true ->
            :ok
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  @doc "Admits a finite raster region using the shared numeric and decoded-image budgets."
  @spec region(map(), [map()], number(), list()) :: {:ok, map()} | {:error, {atom(), map()}}
  def region(bounds, boxes, height, operations) do
    with {:ok, rectangles} <-
           NativeElixirPdfUtilities.HtmlToPdf.DrawingGeometry.placed_ink_rectangles(boxes) do
      {left, bottom, right, top} =
        Enum.reduce(
          rectangles,
          {bounds.x, bounds.top - bounds.height, bounds.x + bounds.width, bounds.top},
          fn {x1, y1, x2, y2}, {l, b, r, t} ->
            {min(l, x1), min(b, y1), max(r, x2), max(t, y2)}
          end
        )

      # Gaussian support is three standard deviations, a geometric expansion rather than a tunable limit.
      pad =
        Enum.reduce(operations, 0.75, fn operation, pad ->
          case operation do
            {"blur", n} -> pad + 3 * n
            {"drop-shadow", shadow} -> pad + 3 * shadow.blur + max(abs(shadow.x), abs(shadow.y))
            _ -> pad
          end
        end)

      left = min(left, bounds.x) - pad
      right = max(right, bounds.x + bounds.width) + pad
      bottom = min(bottom, bounds.top - bounds.height) - pad
      top = max(top, bounds.top) + pad
      values = [left, bottom, right, top, height, right - left, top - bottom]

      with :ok <- WriterValidator.validate_pdf_geometry(values ++ [0]) do
        {:ok,
         %{
           x: floor(left / 0.75) * 0.75,
           y: floor(bottom / 0.75) * 0.75,
           width: (ceil(right / 0.75) - floor(left / 0.75)) * 0.75,
           height: (ceil(top / 0.75) - floor(bottom / 0.75)) * 0.75
         }}
      end
    end
  end

  @doc "Returns a shared actionable failure for an effect combination requiring static source artwork."
  @spec unsupported(binary(), binary()) :: {:error, {atom(), map()}}
  def unsupported(message, source),
    do:
      Diagnostics.error(:layout, :unsupported_html, message,
        module: __MODULE__,
        operation: :prepare_css_effect,
        source: source
      )
end
