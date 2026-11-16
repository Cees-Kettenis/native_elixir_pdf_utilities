defmodule NativeElixirPdfUtilities.Validators.CssBorderImageValidator do
  @moduledoc "Admission and computed values for nine-part CSS border images."
  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssImageValueValidator,
    CssLengthValidator,
    CssMathValidator
  }

  @sides [:top, :right, :bottom, :left]
  @properties ~w(border-image border-image-source border-image-slice border-image-width border-image-outset border-image-repeat)

  @doc "Lists the border image shorthand and longhands."
  @spec properties() :: [String.t()]
  def properties, do: @properties

  @doc "Returns reset values without adding them to ordinary element styles."
  @spec defaults() :: map()
  def defaults do
    %{
      border_image_source: nil,
      border_image_slice: %{edges: Map.new(@sides, &{&1, {:percent, 1.0}}), fill: false},
      border_image_width: Map.new(@sides, &{&1, {:multiple, 1.0}}),
      border_image_outset: Map.new(@sides, &{&1, {:multiple, 0.0}}),
      border_image_repeat: {:stretch, :stretch}
    }
  end

  @doc "Prepares image, slices, destination widths, outsets and edge repetition."
  @spec prepare(map(), String.t(), String.t()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(style, property, source) do
    case property do
      "border-image-source" ->
        with {:ok, image} <- CssImageValueValidator.prepare(source, style),
             do: {:ok, Map.put(style, :border_image_source, image)}

      "border-image-repeat" ->
        tokens = source |> String.downcase(:ascii) |> HtmlValidator.split_css_tokens()

        if length(tokens) in [1, 2] and Enum.all?(tokens, &(&1 in ~w(stretch repeat round space))) do
          {:ok,
           Map.put(
             style,
             :border_image_repeat,
             {String.to_existing_atom(hd(tokens)), String.to_existing_atom(List.last(tokens))}
           )}
        else
          :error
        end

      "border-image-slice" ->
        tokens = source |> String.downcase(:ascii) |> HtmlValidator.split_css_tokens()

        with true <- Enum.count(tokens, &(&1 == "fill")) <= 1,
             {:ok, edges} <- edges(Enum.reject(tokens, &(&1 == "fill")), :slice, style) do
          {:ok, Map.put(style, :border_image_slice, %{edges: edges, fill: "fill" in tokens})}
        else
          _ -> :error
        end

      property when property in ["border-image-width", "border-image-outset"] ->
        kind = if property == "border-image-width", do: :width, else: :outset

        with {:ok, edges} <-
               edges(
                 source |> String.downcase(:ascii) |> HtmlValidator.split_css_tokens(),
                 kind,
                 style
               ),
             do:
               {:ok,
                Map.put(style, String.to_existing_atom(String.replace(property, "-", "_")), edges)}

      "border-image" ->
        tokens = CssMathValidator.component_terms(source, true)

        {repeats, tokens} =
          Enum.split_with(
            tokens,
            &(String.downcase(&1, :ascii) in ~w(stretch repeat round space))
          )

        # Slash tokens delimit optional width and outset, including an omitted width.
        chunks =
          Enum.reduce(tokens, [[]], fn token, [current | rest] ->
            if token == "/", do: [[], current | rest], else: [current ++ [token] | rest]
          end)
          |> Enum.reverse()

        with true <- length(chunks) in 1..3,
             [head | tail] = chunks,
             {sources, slices} =
               Enum.split_with(head, fn token ->
                 match?({:ok, _}, CssImageValueValidator.prepare(token, style))
               end),
             true <- length(sources) <= 1,
             true <- sources != [] or slices != [] or repeats != [],
             initial = Map.merge(style, defaults()),
             {:ok, prepared} <-
               if(sources == [],
                 do: {:ok, initial},
                 else: prepare(initial, "border-image-source", hd(sources))
               ),
             {:ok, prepared} <-
               if(slices == [],
                 do: {:ok, prepared},
                 else: prepare(prepared, "border-image-slice", Enum.join(slices, " "))
               ),
             {:ok, prepared} <-
               Enum.reduce_while(
                 Enum.zip(tail, ["border-image-width", "border-image-outset"]),
                 {:ok, prepared},
                 fn {values, name}, {:ok, acc} ->
                   case if(values == [] and name == "border-image-width" and length(tail) == 2,
                          do: {:ok, acc},
                          else: prepare(acc, name, Enum.join(values, " "))
                        ) do
                     {:ok, acc} -> {:cont, {:ok, acc}}
                     error -> {:halt, error}
                   end
                 end
               ),
             {:ok, prepared} <-
               if(repeats == [],
                 do: {:ok, prepared},
                 else: prepare(prepared, "border-image-repeat", Enum.join(repeats, " "))
               ) do
          {:ok, prepared}
        else
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp edges(tokens, kind, style) do
    if length(tokens) in 1..4 do
      Enum.reduce_while(tokens, {:ok, []}, fn token, {:ok, values} ->
        result =
          cond do
            kind == :width and token == "auto" ->
              {:ok, :auto}

            true ->
              case CssMathValidator.scalar_in_range(token, :number, 0, nil, style) do
                {:ok, n} ->
                  {:ok, if(kind == :slice, do: {:slice, n}, else: {:multiple, n})}

                _ ->
                  if kind == :slice do
                    with {:ok, n, %{percentage: 1}} <- CssMathValidator.evaluate(token, %{}),
                         true <- is_number(n) and n >= 0,
                         {:ok, n} <- HtmlValidator.normalize_geometry_number(n),
                         do: {:ok, {:percent, n / 100.0}},
                         else: (_ -> :error)
                  else
                    with {:ok, value} <-
                           CssLengthValidator.prepare(
                             token,
                             if(kind == :outset, do: :length, else: :padding)
                           ),
                         {:ok, value} <- CssLengthValidator.resolve(value, nil, style),
                         do: {:ok, value}
                  end
              end
          end

        case result do
          {:ok, value} -> {:cont, {:ok, values ++ [value]}}
          _ -> {:halt, :error}
        end
      end)
      |> case do
        {:ok, values} ->
          [top, right, bottom, left] =
            case values do
              [a] -> [a, a, a, a]
              [a, b] -> [a, b, a, b]
              [a, b, c] -> [a, b, c, b]
              four -> four
            end

          {:ok, %{top: top, right: right, bottom: bottom, left: left}}

        error ->
          error
      end
    else
      :error
    end
  end

  @doc "Validates prepared border image fields before layout."
  @spec validate_style(map(), String.t()) :: :ok | {:error, term()}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :border_image_source) and
             not is_map_key(style, :border_image_slice) and
             not is_map_key(style, :border_image_width) and
             not is_map_key(style, :border_image_outset) and
             not is_map_key(style, :border_image_repeat) ->
        :ok

      _ ->
        fields = Map.merge(defaults(), style)

        valid? =
          CssImageValueValidator.valid_loaded?(fields.border_image_source) and
            match?(%{edges: %{}, fill: fill} when is_boolean(fill), fields.border_image_slice) and
            Enum.all?(
              [
                {:slice, fields.border_image_slice[:edges]},
                {:width, fields.border_image_width},
                {:outset, fields.border_image_outset}
              ],
              fn {kind, edges} ->
                is_map(edges) and Enum.sort(Map.keys(edges)) == [:bottom, :left, :right, :top] and
                  Enum.all?(edges, fn {_side, value} ->
                    case value do
                      {:slice, n} ->
                        kind == :slice and is_number(n) and n >= 0 and
                          match?({:ok, _}, HtmlValidator.normalize_geometry_number(n))

                      {:multiple, n} ->
                        kind != :slice and is_number(n) and n >= 0 and
                          match?({:ok, _}, HtmlValidator.normalize_geometry_number(n))

                      :auto ->
                        kind == :width

                      {:percent, n} ->
                        kind != :outset and is_number(n) and n >= 0 and
                          match?({:ok, _}, HtmlValidator.normalize_geometry_number(n))

                      n ->
                        kind != :slice and
                          CssLengthValidator.valid_edge?(
                            n,
                            if(kind == :outset, do: :border_widths, else: :padding)
                          )
                    end
                  end)
              end
            ) and
            case fields.border_image_repeat do
              {x, y} ->
                x in [:stretch, :repeat, :round, :space] and
                  y in [:stretch, :repeat, :round, :space]

              _ ->
                false
            end

        if valid?,
          do: :ok,
          else:
            NativeElixirPdfUtilities.Diagnostics.error(
              :layout,
              :invalid_layout,
              "border images require a loaded image and bounded slices, widths, outsets and repetition",
              operation: :layout,
              module: __MODULE__,
              source: source
            )
    end
  end
end
