defmodule NativeElixirPdfUtilities.Validators.SvgTextValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font

  alias NativeElixirPdfUtilities.Validators.{
    CssValueValidator,
    HtmlValidator,
    SvgSceneValidator,
    SvgStyleValidator,
    SvgTransformValidator,
    SvgTextPositionValidator,
    SvgTextPathValidator,
    SvgValidator
  }

  @doc false
  @spec validate_summary(map()) :: :ok | {:error, {atom(), map()}}
  def validate_summary(summary) do
    if summary.context.mode == :inline,
      do: validate_tree(SvgSceneValidator.summary_tree(summary)),
      else: :ok
  end

  @doc false
  @spec validate_selected_text(map(), [{binary(), map()}]) :: :ok | {:error, {atom(), map()}}
  def validate_selected_text(summary, selected) do
    if summary.context.mode == :inline do
      fonts =
        selected
        |> Enum.reverse()
        |> Enum.with_index()
        |> Map.new(fn {{_data, parsed}, index} ->
          {"NativeSvgFont#{index}", Map.put(parsed, :type, :embedded)}
        end)

      Enum.reduce_while(summary.geometry, :ok, fn node, :ok ->
        if node.tag in ~w(text tspan textPath) do
          font = Map.get(fonts, Map.get(node.attributes, "font-family"))

          text =
            Map.get(summary.text_by_node, node.svg_index, "") |> String.replace(~r/[\t\r\n]/, " ")

          if font && Font.supports_text?(font, text),
            do: {:cont, :ok},
            else:
              {:halt,
               unsupported(
                 "inline SVG text requires its exact selected NativeSvgFontN alias and complete Unicode coverage; fallback is disabled",
                 node.tag
               )}
        else
          {:cont, :ok}
        end
      end)
    else
      :ok
    end
  end

  @doc false
  @spec validate_tree(map()) :: :ok | {:error, {atom(), map()}}
  def validate_tree(root) do
    with {:ok, _positions} <- prepare_tree_profile(root), do: :ok
  end

  defp prepare_tree_profile(root) do
    nodes = SvgSceneValidator.tree_nodes(root)
    text? = Enum.any?(nodes, fn {node, _} -> node.tag in ~w(text tspan textPath) end)

    with :ok <- SvgTextPathValidator.validate_tree(root),
         :ok <-
           if(text?,
             do: validate_original_fonts(root),
             else: :ok
           ) do
      Enum.reduce_while(nodes, {:ok, %{}}, fn {node, ancestors}, {:ok, positions} ->
        attributes = if text?, do: validate_text_attributes(node), else: :ok

        result =
          cond do
            attributes != :ok ->
              attributes

            node.tag in ~w(text tspan textPath) ->
              cond do
                node.tag == "tspan" and List.first(ancestors) not in ~w(text tspan textPath) ->
                  invalid("SVG tspan requires a text or tspan parent", "tspan")

                node.tag == "text" and Enum.any?(ancestors, &(&1 in ~w(text tspan textPath))) ->
                  invalid("SVG text roots cannot contain nested text roots", "text")

                Enum.any?(node.children, fn child ->
                  Map.get(child, :type) == :element and
                      child.tag not in ~w(tspan textPath title desc)
                end) ->
                  unsupported(
                    "SVG text children require supported tspan or descriptive metadata",
                    node.tag
                  )

                Enum.any?(node.attributes, fn {key, value} ->
                  (key in ~w(baseline-shift alignment-baseline dominant-baseline writing-mode direction unicode-bidi font-stretch font-feature-settings font-variation-settings font-optical-sizing text-decoration word-spacing) and
                     value not in ~w(normal auto none ltr lr-tb horizontal-tb baseline)) or
                      (key == "xml:space" and value != "default")
                end) ->
                  unsupported(
                    "SVG advanced text positioning, bidi, font features or preserved whitespace require a complete text adapter",
                    node.tag
                  )

                not Map.has_key?(node, :style) and Map.get(node.attributes, "style", "") != "" ->
                  unsupported("SVG text CSS requires host cascade preparation", node.tag)

                node.tag in ~w(tspan textPath) and
                    SvgTransformValidator.parse(Map.get(node.attributes, "transform", "none")) !=
                      {:ok, {1.0, 0.0, 0.0, 1.0, 0.0, 0.0}} ->
                  unsupported(
                    "SVG #{node.tag} transforms require a complete text positioning adapter",
                    node.tag
                  )

                Map.has_key?(node, :style) and
                    (Map.get(node.style, :svg_stroke, :none) != :none or
                       match?({:paint_server, _}, Map.get(node.style, :svg_fill))) ->
                  unsupported(
                    "SVG text currently requires solid RGB fill without stroke; gradient/stroke text paint remains pending",
                    node.tag
                  )

                Map.has_key?(node, :style) and
                    (Map.get(node.style, :text_transform, :none) != :none or
                       Map.get(node.style, :white_space, :normal) not in [:normal, :nowrap] or
                       Map.get(node.style, :_text_decorations, []) != []) ->
                  unsupported(
                    "SVG text transforms, preserved whitespace and decorations require matching visible and logical text",
                    node.tag
                  )

                Map.has_key?(node.attributes, "textLength") and
                    Enum.any?(node.children, &(Map.get(&1, :tag) == "tspan")) ->
                  unsupported(
                    "SVG textLength over nested spans requires complete ancestral length distribution",
                    node.tag
                  )

                true ->
                  with {:ok, plan} <- SvgTextPositionValidator.prepare(node) do
                    if Map.has_key?(plan.lengths, "textLength") and
                         Enum.any?(~w(x y), &(length(Map.get(plan.lengths, &1, [])) > 1)) do
                      unsupported(
                        "SVG textLength across absolute-position chunks requires complete chunk length distribution",
                        node.tag
                      )
                    else
                      {:ok, plan}
                    end
                  end
              end

            true ->
              :ok
          end

        case result do
          :ok -> {:cont, {:ok, positions}}
          {:ok, plan} -> {:cont, {:ok, Map.put(positions, plan.source_key, plan)}}
          error -> {:halt, error}
        end
      end)
    end
  end

  @doc false
  @spec validate_original_fonts(map()) :: :ok | {:error, {atom(), map()}}
  def validate_original_fonts(root) do
    validate_original_font_declarations(
      root,
      CssValueValidator.policy_style(),
      HtmlValidator.new_css_budget()
    )
  end

  # Original declarations are checked even on supplied styled trees. Prepared
  # parent styles provide descendant context, never proof of source validity.
  defp validate_original_font_declarations(node, inherited, budget) do
    case node do
      %{attributes: attributes, children: children}
      when is_map(attributes) and is_list(children) ->
        style =
          case Map.get(node, :style) do
            style when is_map(style) -> style
            _ -> %{}
          end

        context =
          Map.merge(inherited, Map.take(style, [:_root_font_size, :_custom_properties]))

        with {:ok, declarations} <- SvgStyleValidator.presentation_declarations(node),
             {:ok, context} <-
               CssValueValidator.validate_source_cascade(
                 context,
                 Enum.filter(declarations, fn {property, _} ->
                   property in ~w(font-size font-weight font-style letter-spacing)
                 end),
                 budget,
                 node
               ) do
          context =
            Map.merge(
              context,
              Map.take(style, [
                :font_size,
                :font_face,
                :font_weight,
                :font_style,
                :letter_spacing
              ])
            )

          Enum.reduce_while(children, :ok, fn child, :ok ->
            result =
              if is_map(child) and Map.get(child, :type) == :element,
                do: validate_original_font_declarations(child, context, budget),
                else: :ok

            case result do
              :ok -> {:cont, :ok}
              error -> {:halt, error}
            end
          end)
        end

      _ ->
        invalid("SVG font validation requires original attributes and children", "SVG fonts")
    end
  end

  defp validate_text_attributes(node) do
    Enum.reduce_while(node.attributes, :ok, fn {property, value}, :ok ->
      result =
        cond do
          property in ~w(text-anchor opacity fill-opacity visibility) ->
            if value in ~w(inherit initial unset) or
                 match?({:ok, _}, SvgStyleValidator.parse(property, value)),
               do: :ok,
               else:
                 invalid(
                   "SVG #{property} requires its supported text/presentation grammar",
                   node.tag <> " " <> property
                 )

          property in ~w(rotate textLength lengthAdjust) and
              node.tag not in ~w(text tspan textPath) ->
            unsupported(
              "SVG inherited advanced text positioning requires a complete text adapter",
              node.tag
            )

          property == "lengthAdjust" and value not in ~w(spacing spacingAndGlyphs) ->
            invalid(
              "SVG lengthAdjust requires spacing or spacingAndGlyphs",
              node.tag <> " lengthAdjust"
            )

          property in ~w(font-stretch font-variant font-feature-settings font-variation-settings font-optical-sizing font-kerning kerning word-spacing baseline-shift alignment-baseline dominant-baseline writing-mode direction unicode-bidi text-decoration) and
              value not in ~w(normal auto none ltr lr-tb horizontal-tb baseline inherit initial unset) ->
            unsupported(
              "SVG inherited advanced text shaping or paint requires a complete text adapter",
              node.tag
            )

          true ->
            :ok
        end

      case result do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  @doc false
  @spec prepare_tree(map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare_tree(root) do
    nodes = SvgSceneValidator.tree_nodes(root)
    ids = Map.new(nodes, fn {node, _} -> {Map.get(node.attributes, "id"), node} end)

    with {:ok, positions} <- prepare_tree_profile(root),
         {:ok, _summary} <-
           if(Enum.any?(nodes, fn {node, _} -> node.tag in ~w(text tspan textPath) end),
             do:
               SvgValidator.validate_processing(
                 NativeElixirPdfUtilities.HtmlToPdf.SvgScene.authored_source(root),
                 %{mode: :inline}
               ),
             else: {:ok, nil}
           ),
         :ok <- validate_instance_fonts(nodes, ids),
         {:ok, selected} <-
           Enum.reduce_while(nodes, {:ok, []}, fn {node, _}, {:ok, selected} ->
             if node.tag in ~w(text tspan textPath use) and
                  (node.tag != "use" or text_target?(node, ids)) do
               case node.style.font_face do
                 %{type: :embedded, data: data} = face when is_binary(data) ->
                   with :ok <-
                          HtmlValidator.validate_font_embedding(
                            face.family,
                            face.embedding_flags,
                            face.variable_font?
                          ),
                        true <-
                          Enum.all?(node.children, fn child ->
                            Map.get(child, :type) != :text or
                              Font.supports_text?(
                                face,
                                String.replace(child.text, ~r/[\t\r\n]/, " ")
                              )
                          end) do
                     selected =
                       if Enum.any?(selected, &(&1.data == data)),
                         do: selected,
                         else: selected ++ [face]

                     {:cont, {:ok, selected}}
                   else
                     false ->
                       {:halt,
                        unsupported(
                          "selected SVG font does not cover its original text; select an authorized face covering every Unicode scalar",
                          node.tag
                        )}

                     error ->
                       {:halt, error}
                   end

                 _ ->
                   {:halt,
                    unsupported(
                      "SVG text requires a selected byte-backed TrueType face; select an authorized font such as DejaVu Sans",
                      node.tag
                    )}
               end
             else
               {:cont, {:ok, selected}}
             end
           end) do
      if length(selected) > Limits.get(:max_font_count) do
        Diagnostics.error(:limits, :resource_limit_exceeded, "SVG fonts exceed max_font_count",
          source: "SVG text"
        )
      else
        aliases =
          selected
          |> Enum.with_index()
          |> Map.new(fn {face, index} -> {face.data, "NativeSvgFont#{index}"} end)

        {:ok,
         %{
           fonts: selected,
           aliases: aliases,
           positions: positions,
           logical_text: logical_nodes(root, ids)
         }}
      end
    end
  end

  defp validate_instance_fonts(nodes, ids) do
    Enum.reduce_while(nodes, :ok, fn {node, _}, :ok ->
      if node.tag == "use" and text_target?(node, ids) do
        target =
          Map.fetch!(
            ids,
            String.trim_leading(
              Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href")),
              "#"
            )
          )

        if target.style.font_face == node.style.font_face,
          do: {:cont, :ok},
          else:
            {:halt,
             unsupported(
               "reused SVG text currently requires the same selected face as its authored target; instance font changes need shaped instance preparation",
               "use text"
             )}
      else
        {:cont, :ok}
      end
    end)
  end

  defp text_target?(node, ids) do
    href = Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href", ""))
    target = Map.get(ids, String.trim_leading(href, "#"))

    target &&
      Enum.any?(SvgSceneValidator.tree_nodes(target), fn {child, _} ->
        child.tag in ~w(text tspan textPath) or (child.tag == "use" and text_target?(child, ids))
      end)
  end

  defp logical_nodes(node, ids, inherited_visibility \\ :visible, reference? \\ false) do
    visibility = reference_visibility(node, inherited_visibility, reference?)

    cond do
      Map.get(node.style, :display) == :none or node.tag in ~w(defs title desc style) ->
        []

      node.tag == "use" ->
        target =
          Map.fetch!(
            ids,
            String.trim_leading(
              Map.get(node.attributes, "href", Map.get(node.attributes, "xlink:href")),
              "#"
            )
          )

        logical_nodes(target, ids, visibility, true)

      node.tag == "text" ->
        text =
          Enum.join(text_children(node, visibility, reference?))
          |> String.replace(~r/[\t\r\n]/, " ")
          |> String.replace(~r/ +/, " ")
          |> String.trim(" ")

        if text == "", do: [], else: [text]

      true ->
        node.children
        |> Enum.filter(&(Map.get(&1, :type) == :element))
        |> Enum.flat_map(&logical_nodes(&1, ids, visibility, reference?))
    end
  end

  defp text_children(node, visibility, reference?) do
    Enum.flat_map(node.children, fn child ->
      case child do
        %{type: :text, text: text} ->
          if(visibility == :visible, do: [text], else: [])

        %{type: :element, tag: tag} when tag in ~w(tspan textPath) ->
          if Map.get(child.style, :display) == :none,
            do: [],
            else:
              text_children(
                child,
                reference_visibility(child, visibility, reference?),
                reference?
              )

        _ ->
          []
      end
    end)
  end

  # Reused descendants inherit the current instance's visibility when their
  # authored cascade did not locally specify it or explicitly selected inherit.
  defp reference_visibility(node, inherited, reference?) do
    specified = Map.get(node.style, :_svg_specified, %{})

    if reference? and Map.get(specified, "visibility") in [nil, "inherit", "unset"],
      do: inherited,
      else: Map.get(node.style, :svg_visibility, :visible)
  end

  defp invalid(message, source),
    do:
      Diagnostics.error(:layout, :invalid_document, message,
        source: source,
        module: __MODULE__,
        operation: :prepare
      )

  defp unsupported(message, source),
    do:
      Diagnostics.error(:layout, :unsupported_html, message,
        source: source,
        module: __MODULE__,
        operation: :prepare
      )
end
