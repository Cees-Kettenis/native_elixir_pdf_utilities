defmodule NativeElixirPdfUtilities.Validators.SvgReferenceValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.CssParser

  alias NativeElixirPdfUtilities.Validators.{
    CssValueValidator,
    HtmlValidator,
    SvgSceneValidator,
    SvgStyleValidator,
    SvgViewportValidator
  }

  @targets ~w(symbol svg use g path rect circle ellipse line polyline polygon)

  @doc false
  @spec referenced_ids(map()) :: MapSet.t()
  def referenced_ids(root) do
    root
    |> SvgSceneValidator.tree_nodes()
    |> Enum.reduce(MapSet.new(), fn {node, _}, ids ->
      if node.tag == "use" do
        case Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", "")) do
          "#" <> id -> MapSet.put(ids, id)
          _ -> ids
        end
      else
        ids
      end
    end)
  end

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

    non_inherited =
      SvgStyleValidator.presentation_properties()
      |> Enum.reject(fn {_, key} -> SvgStyleValidator.inherited_property?(key) end)
      |> Enum.map(&elem(&1, 0))
      |> Enum.sort()

    with :ok <-
           validate_instance_inheritance(
             root,
             referenced_ids(root),
             non_inherited,
             false,
             HtmlValidator.new_css_budget()
           ) do
      Enum.reduce_while(nodes, :ok, fn {node, ancestors}, :ok ->
        href = Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", ""))

        artwork =
          Enum.filter(
            node.children,
            &(Map.get(&1, :type) == :element and &1.tag not in ~w(title desc))
          )

        result =
          cond do
            node.tag == "symbol" and List.first(ancestors) != "defs" ->
              invalid("SVG symbols currently require a direct defs parent", "symbol")

            node.tag == "symbol" ->
              with {:ok, _} <- SvgViewportValidator.view_box(Map.get(node.attributes, "viewBox")),
                   {:ok, _} <-
                     SvgViewportValidator.aspect_ratio(
                       Map.get(node.attributes, "preserveAspectRatio")
                     ) do
                :ok
              else
                _ ->
                  invalid(
                    "SVG symbol requires valid viewBox and preserveAspectRatio geometry",
                    "symbol"
                  )
              end

            node.tag == "use" and artwork != [] ->
              invalid(
                "SVG use accepts descriptive metadata, not authored instance artwork",
                "use"
              )

            node.tag == "use" ->
              case href do
                "#" <> id ->
                  if Map.get(ids, id) in @targets,
                    do: :ok,
                    else:
                      invalid(
                        "SVG use target ##{id} must identify same-scene static geometry or a symbol",
                        "use"
                      )

                _ ->
                  invalid("SVG use requires an explicit same-scene #id reference", "use")
              end

            true ->
              :ok
          end

        case result do
          :ok -> {:cont, :ok}
          error -> {:halt, error}
        end
      end)
    end
  end

  # Reused targets cannot resolve non-inherited effects against their authored
  # parent before expansion. Raw scenes also need issued effect/cost context.
  defp validate_instance_inheritance(node, references, properties, reference?, budget) do
    reference? = reference? or MapSet.member?(references, Map.get(node.attributes, "id"))
    specified = Map.get(Map.get(node, :style, %{}), :_svg_specified, %{})
    css = Map.get(node.attributes, "style", "")

    if not Map.has_key?(node, :style) and String.trim(css) != "" and
         node.tag not in ~w(linearGradient radialGradient stop) do
      invalid(
        "SVG inline CSS and stylesheets require host cascade preparation before validated effects and allocation accounting",
        node.tag <> " style"
      )
    else
      with {:ok, authored} <- inheritance_declarations(node, properties, css, budget) do
        pending =
          Enum.find(properties, fn property ->
            property in authored or
              (is_binary(Map.get(specified, property)) and
                 String.trim(specified[property]) == "inherit")
          end)

        if pending && (reference? or not Map.has_key?(node, :style)) do
          Diagnostics.error(
            :layout,
            :unsupported_html,
            "SVG #{pending}:inherit requires prepared instance effects and allocation accounting; use an explicit supported value until that reference scope is implemented",
            source: node.tag <> " " <> pending,
            module: __MODULE__,
            operation: :prepare
          )
        else
          Enum.reduce_while(node.children, :ok, fn child, :ok ->
            result =
              if Map.get(child, :type) == :element,
                do:
                  validate_instance_inheritance(
                    child,
                    references,
                    properties,
                    reference?,
                    budget
                  ),
                else: :ok

            case result do
              :ok -> {:cont, :ok}
              error -> {:halt, error}
            end
          end)
        end
      end
    end
  end

  # Source attributes and original inline CSS use the same declaration policy
  # as host preparation. Prepared metadata cannot hide an authored keyword.
  defp inheritance_declarations(node, properties, css, budget) do
    with {:ok, declarations} <- CssParser.parse_declarations_detailed(css) do
      attributes =
        Enum.flat_map(properties, fn property ->
          if SvgStyleValidator.presentation_property?(node, property) do
            case Map.fetch(node.attributes, property) do
              {:ok, value} -> [{property, value}]
              :error -> []
            end
          else
            []
          end
        end)

      declarations =
        Enum.filter(declarations, fn declaration -> elem(declaration, 0) in properties end)

      style = Map.get(node, :style, %{})

      context =
        CssValueValidator.policy_style()
        |> Map.merge(SvgStyleValidator.defaults(%{}))
        |> Map.merge(Map.take(style, [:_custom_properties, :_root_font_size, :font_size, :color]))
        |> Map.merge(%{_svg_prepare: true, _svg_element: true})

      Enum.reduce_while(attributes ++ declarations, {:ok, []}, fn declaration, {:ok, pending} ->
        case CssValueValidator.validate_declaration(context, declaration, budget) do
          {:ok, validated} ->
            property = elem(declaration, 0)
            value = validated._svg_specified[property]
            pending = if String.trim(value) == "inherit", do: [property | pending], else: pending
            {:cont, {:ok, pending}}

          error ->
            {:halt,
             if(elem(declaration, 0) == "transform" and node.tag in ~w(use symbol),
               do: invalid("SVG instances require a valid bounded affine transform", node.tag),
               else: HtmlValidator.source_style_error(node, declaration, error)
             )}
        end
      end)
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
