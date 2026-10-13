defmodule NativeElixirPdfUtilities.Validators.SvgTextPositionValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.{Diagnostics, Limits}
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, SvgStyleValidator}

  @properties ~w(x y dx dy rotate textLength startOffset)

  @doc false
  @spec reserve_work(map(), atom()) :: {:ok, non_neg_integer()} | {:error, {atom(), map()}}
  def reserve_work(node, stage \\ :layout) do
    with %{tag: tag, attributes: attributes} when is_binary(tag) and is_map(attributes) <- node do
      result =
        Enum.reduce_while(@properties, {:ok, 0}, fn property, {:ok, count} ->
          case if(Map.has_key?(Map.get(node, :attribute_namespaces, %{}), property),
                 do: :error,
                 else: Map.fetch(attributes, property)
               ) do
            {:ok, value} when is_binary(value) ->
              case token_work(value, Limits.get(:max_svg_expansion_work) - count, 0, false) do
                {:ok, work} -> {:cont, {:ok, count + work}}
                :limit -> {:halt, limit(tag <> " " <> property)}
              end

            :error ->
              {:cont, {:ok, count}}

            _ ->
              {:halt, invalid("SVG text positioning requires original length-list strings", tag)}
          end
        end)

      with {:ok, count} <- result do
        # Reserve before allocating token or parsed-length lists. The SAX owner
        # separately charges reused occurrences when visiting the reference graph.
        HtmlValidator.reserve_render_resource(:max_svg_expansion_work, count, stage)
        {:ok, count}
      end
    else
      _ -> invalid("SVG text positioning requires original attributes and a tag", "SVG positions")
    end
  catch
    {:render_resource_limit, {:error, {_reason, diagnostic}}} ->
      Diagnostics.error(diagnostic.stage, diagnostic.reason, diagnostic.message,
        source: "SVG positions",
        module: __MODULE__,
        operation: :prepare
      )
  end

  @doc false
  @spec prepare(map()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(node) do
    with {:ok, work} <- reserve_work(node),
         :ok <-
           (case Map.get(node, :style) do
              nil ->
                :ok

              %{font_size: size, _root_font_size: root} ->
                with {:ok, size} <- HtmlValidator.normalize_geometry_number(size),
                     {:ok, root} <- HtmlValidator.normalize_geometry_number(root),
                     true <- size >= 0 and root >= 0 do
                  :ok
                else
                  _ ->
                    invalid(
                      "prepared SVG text positions require finite inherited font/root sizes",
                      node.tag
                    )
                end

              _ ->
                invalid(
                  "prepared SVG text positions require their actual inherited font/root context",
                  node.tag
                )
            end) do
      Enum.reduce_while(
        @properties,
        {:ok, %{lengths: %{}, work: work, source_key: plan_key(node)}},
        fn property, {:ok, plan} ->
          case if(Map.has_key?(Map.get(node, :attribute_namespaces, %{}), property),
                 do: :error,
                 else: Map.fetch(node.attributes, property)
               ) do
            :error ->
              {:cont, {:ok, plan}}

            {:ok, value} ->
              source = node.tag <> " " <> property
              value = HtmlValidator.trim_ascii_whitespace(value)

              if value == "" or Regex.match?(~r/^,|,[ \t\r\n]*,|,$/, value) do
                {:halt,
                 invalid(
                   "SVG text #{property} requires a nonempty comma/whitespace length list",
                   source
                 )}
              else
                result =
                  value
                  |> String.split(~r/[, \t\r\n]+/, trim: true)
                  |> Enum.reduce_while({:ok, []}, fn token, {:ok, lengths} ->
                    parsed =
                      if property == "rotate" do
                        case HtmlValidator.parse_css_number(token) do
                          {angle, ""} ->
                            # 360 is the mathematical degree period, not a work
                            # ceiling. Host plans normalize before native f32
                            # parsing; raw callers supply a canonical one-turn
                            # representation until their source adapter exists.
                            if Map.has_key?(node, :style) or abs(angle) <= 360 do
                              {:ok, {:user, angle - Float.floor(angle / 360) * 360}}
                            else
                              :raw_angle
                            end

                          _ ->
                            :error
                        end
                      else
                        SvgStyleValidator.length(token, true)
                      end

                    case parsed do
                      {:ok, length} when property == "textLength" and elem(length, 1) < 0 ->
                        {:halt,
                         invalid("SVG textLength requires a nonnegative supported length", source)}

                      {:ok, length} ->
                        if not Map.has_key?(node, :style) and
                             (match?({:svg_rem, _}, length) or String.ends_with?(token, "q")) do
                          {:halt,
                           unsupported(
                             "raw SVG text rem/q positions require the host's validated font/unit preparation",
                             source
                           )}
                        else
                          {:cont, {:ok, [length | lengths]}}
                        end

                      :raw_angle ->
                        {:halt,
                         unsupported(
                           "raw SVG rotate requires canonical one-turn degrees or validated host normalization before native float conversion",
                           source
                         )}

                      :error ->
                        {:halt,
                         invalid(
                           if(property == "rotate",
                             do: "SVG rotate requires finite unitless degrees",
                             else: "SVG text #{property} requires finite supported lengths"
                           ),
                           source
                         )}
                    end
                  end)

                case result do
                  {:ok, lengths}
                  when property in ~w(textLength startOffset) and length(lengths) != 1 ->
                    {:halt,
                     invalid("SVG #{property} requires exactly one supported length", source)}

                  {:ok, lengths} ->
                    {:cont,
                     {:ok,
                      %{plan | lengths: Map.put(plan.lengths, property, Enum.reverse(lengths))}}}

                  error ->
                    {:halt, error}
                end
              end
          end
        end
      )
    end
  end

  @doc false
  @spec plan_key(map()) :: tuple()
  def plan_key(node) do
    style =
      case Map.get(node, :style) do
        style when is_map(style) -> style
        _ -> %{}
      end

    {node.tag, node.attributes, Map.get(node, :attribute_namespaces, %{}),
     Map.take(style, [:font_size, :_root_font_size]),
     {Map.get(node, :node_id), Map.get(node, :document_id), Map.get(node, :tree_scope_id)}}
  end

  # Count ASCII-separated tokens directly over the binary, without allocating
  # a token list before the caller's complexity ceiling has been checked.
  defp token_work(value, remaining, count, inside?) do
    case value do
      <<byte, rest::binary>> ->
        separator? = byte in [32, 9, 13, 10, 44]
        count = count + if(separator? or inside?, do: 0, else: 1)

        if count > remaining,
          do: :limit,
          else: token_work(rest, remaining, count, not separator?)

      <<>> ->
        {:ok, count}
    end
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

  defp limit(source),
    do:
      Diagnostics.error(
        :limits,
        :resource_limit_exceeded,
        "SVG text positions exceed max_svg_expansion_work",
        source: source,
        module: __MODULE__,
        operation: :prepare
      )
end
