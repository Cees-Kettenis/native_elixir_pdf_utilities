defmodule NativeElixirPdfUtilities.Validators.CssGridValidator do
  @moduledoc "Owns grid track, placement and template-area grammars."
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssSyntaxValidator,
    CssMathValidator,
    CssLengthValidator
  }

  @doc "Parses a track list, retaining named lines, auto repetition and subgrid metadata."
  @spec tracks(String.t(), (String.t() -> term())) :: {:ok, list()} | :error | {:error, term()}
  def tracks(value, parser) do
    HtmlValidator.with_render_budget(fn -> parse_tracks(String.trim(value), parser, 0) end)
  end

  defp parse_tracks(value, parser, depth) do
    HtmlValidator.check_render_resource(:max_depth, depth, :style)
    tokens = tokens(value)

    case tokens do
      ["none"] ->
        {:ok, []}

      ["subgrid" | names] ->
        with {:ok, groups} <- subgrid_names(names),
             do: {:ok, [{:subgrid, groups}]}

      [] ->
        :error

      tokens ->
        Enum.reduce_while(tokens, {:ok, [], 0}, fn token, {:ok, parts, count} ->
          parsed =
            case token do
              "[" <> _ ->
                with {:ok, names} <- line_name_tokens([token]), do: {:ok, [{:line_names, names}]}

              _ ->
                case function(token) do
                  {:ok, "repeat", arguments} ->
                    case CssSyntaxValidator.split(arguments, ",") do
                      [repeat, body] ->
                        with false <-
                               Enum.any?(tokens(body), &match?({:ok, "repeat", _}, function(&1))),
                             {:ok, pattern} <- parse_tracks(String.trim(body), parser, depth + 1),
                             true <-
                               pattern != [] and
                                 not Enum.any?(
                                   pattern,
                                   &(match?({:repeat, _, _}, &1) or match?({:subgrid, _}, &1))
                                 ) do
                          case String.trim(repeat) do
                            name when name in ["auto-fit", "auto-fill"] ->
                              if Enum.all?(pattern, &fixed_repeat_track?/1),
                                do:
                                  {:ok,
                                   [
                                     {:repeat,
                                      if(name == "auto-fit", do: :auto_fit, else: :auto_fill),
                                      pattern}
                                   ]},
                                else: :error

                            number ->
                              with {n, ""} when n > 0 <- Integer.parse(number),
                                   :ok <-
                                     HtmlValidator.validate_layout_cardinality(
                                       :grid_tracks,
                                       n * track_count(pattern)
                                     ) do
                                HtmlValidator.reserve_render_resource(
                                  :max_work,
                                  n * length(pattern),
                                  :style
                                )

                                {:ok, List.duplicate(pattern, n) |> List.flatten()}
                              else
                                {:error, _} = error -> error
                                _ -> :error
                              end
                          end
                        else
                          {:error, _} = error -> error
                          _ -> :error
                        end

                      _ ->
                        :error
                    end

                  _ ->
                    with {:ok, track} <- track(token, parser), do: {:ok, [track]}
                end
            end

          with {:ok, parsed} <- parsed,
               :ok <-
                 if(count + track_count(parsed) == 0,
                   do: :ok,
                   else:
                     HtmlValidator.validate_layout_cardinality(
                       :grid_tracks,
                       count + track_count(parsed)
                     )
                 ) do
            {:cont, {:ok, [parsed | parts], count + track_count(parsed)}}
          else
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, parts, count} when count > 0 ->
            parts = parts |> Enum.reverse() |> List.flatten()
            repeats = Enum.count(parts, &match?({:repeat, _, _}, &1))

            if repeats <= 1 and (repeats == 0 or Enum.all?(parts, &fixed_repeat_track?/1)),
              do: {:ok, parts},
              else: :error

          {:error, _} = error ->
            error

          _ ->
            :error
        end
    end
  end

  defp track_count(tracks) do
    Enum.count(tracks, &(not match?({:line_names, _}, &1)))
  end

  defp subgrid_names(tokens) do
    Enum.reduce_while(tokens, {:ok, []}, fn token, {:ok, groups} ->
      result =
        case function(token) do
          {:ok, "repeat", arguments} ->
            with [count, body] <- CssSyntaxValidator.split(arguments, ","),
                 parts when parts != [] <- tokens(body),
                 parsed = Enum.map(parts, &line_name_tokens([&1])),
                 true <- Enum.all?(parsed, &match?({:ok, _}, &1)) do
              pattern = Enum.map(parsed, &elem(&1, 1))

              case String.trim(count) do
                "auto-fill" ->
                  {:ok, [{:auto_fill, pattern}]}

                count ->
                  with {count, ""} when count > 0 <- Integer.parse(count),
                       :ok <-
                         HtmlValidator.validate_layout_cardinality(
                           :grid_tracks,
                           count * length(pattern)
                         ) do
                    HtmlValidator.reserve_render_resource(
                      :max_work,
                      count * length(pattern),
                      :style
                    )

                    {:ok, List.duplicate(pattern, count) |> Enum.flat_map(& &1)}
                  else
                    {:error, _} = error -> error
                    _ -> :error
                  end
              end
            else
              _ -> :error
            end

          _ ->
            with {:ok, names} <- line_name_tokens([token]), do: {:ok, [names]}
        end

      case result do
        {:ok, names} -> {:cont, {:ok, Enum.reverse(names, groups)}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, groups} ->
        groups = Enum.reverse(groups)

        if groups != [],
          do: HtmlValidator.reserve_render_resource(:max_entries, length(groups), :style)

        if Enum.count(groups, &match?({:auto_fill, _}, &1)) <= 1, do: {:ok, groups}, else: :error

      error ->
        error
    end
  end

  defp fixed_repeat_track?(track) do
    case track do
      {:line_names, _} -> true
      {:length, _} -> true
      {:minmax, {:length, _}, _} -> true
      {:minmax, _, {:length, _}} -> true
      {:repeat, _, pattern} -> Enum.all?(pattern, &fixed_repeat_track?/1)
      _ -> false
    end
  end

  @doc "Parses a single track sizing function through the supplied shared length grammar."
  @spec track(String.t(), (String.t() -> term())) :: {:ok, term()} | :error | {:error, term()}
  def track(value, parser) do
    value = String.trim(value)

    case String.downcase(value, :ascii) do
      "auto" ->
        {:ok, :auto}

      "min-content" ->
        {:ok, :min_content}

      "max-content" ->
        {:ok, :max_content}

      _ ->
        case function(value) do
          {:ok, "minmax", arguments} ->
            case CssSyntaxValidator.split(arguments, ",") do
              [minimum, maximum] ->
                with {:ok, minimum} <- track_leaf(String.trim(minimum), parser),
                     false <- match?({:fr, _}, minimum),
                     {:ok, maximum} <- track_leaf(String.trim(maximum), parser),
                     do: {:ok, {:minmax, minimum, maximum}},
                     else: (
                       {:error, _} = error -> error
                       _ -> :error
                     )

              _ ->
                :error
            end

          {:ok, "fit-content", limit} ->
            with {:ok, {:length, length}} <- track_leaf(String.trim(limit), parser),
                 do: {:ok, {:fit_content, length}},
                 else: (
                   {:error, _} = error -> error
                   _ -> :error
                 )

          _ ->
            track_leaf(value, parser)
        end
    end
  end

  defp track_leaf(value, parser) do
    case String.downcase(value, :ascii) do
      "auto" ->
        {:ok, :auto}

      "min-content" ->
        {:ok, :min_content}

      "max-content" ->
        {:ok, :max_content}

      value ->
        case HtmlValidator.parse_css_number(value) do
          {fraction, "fr"} when fraction >= 0 ->
            {:ok, {:fr, fraction}}

          _ ->
            parsed =
              case parser.(value) do
                result when result in [:error, {:error, :invalid_document}] ->
                  NativeElixirPdfUtilities.Validators.CssSizingValidator.prepare_math(value)

                result ->
                  result
              end

            with {:ok, length} <- parsed, do: {:ok, {:length, length}}
        end
    end
  end

  defp function(value) do
    case HtmlValidator.parse_css_identifier(value) do
      {:ok, name, "(" <> arguments} ->
        if String.ends_with?(arguments, ")"),
          do:
            {:ok, String.downcase(name, :ascii),
             binary_part(arguments, 0, byte_size(arguments) - 1)},
          else: :error

      _ ->
        :error
    end
  end

  defp line_name_tokens(tokens) do
    Enum.reduce_while(tokens, {:ok, []}, fn token, {:ok, result} ->
      case token do
        "[" <> rest ->
          if String.ends_with?(rest, "]") do
            names =
              rest |> binary_part(0, byte_size(rest) - 1) |> HtmlValidator.split_css_tokens()

            parsed = Enum.map(names, &name/1)

            if Enum.all?(parsed, &match?({:ok, _}, &1)),
              do: {:cont, {:ok, result ++ Enum.map(parsed, &elem(&1, 1))}},
              else: {:halt, :error}
          else
            {:halt, :error}
          end

        _ ->
          {:halt, :error}
      end
    end)
  end

  defp name(value) do
    case HtmlValidator.parse_css_identifier(value) do
      {:ok, name, ""} ->
        if String.downcase(name, :ascii) in ~w(auto span default inherit initial unset revert revert-layer),
          do: :error,
          else: {:ok, name}

      _ ->
        :error
    end
  end

  # The shared syntax tokenizer keeps bracket blocks, strings and functions
  # opaque. Track separators can therefore be identified without another CSS lexer.
  defp tokens(source) do
    {parts, current} =
      source
      |> CssSyntaxValidator.components()
      |> Enum.reduce({[], ""}, fn token, {parts, current} ->
        if token.kind == :comment or String.trim(token.text) == "" do
          {if(current == "", do: parts, else: [current | parts]), ""}
        else
          if token.kind == :block and token.opener == "[" do
            {[token.text | if(current == "", do: parts, else: [current | parts])], ""}
          else
            {parts, current <> token.text}
          end
        end
      end)

    Enum.reverse(if(current == "", do: parts, else: [current | parts]))
  end

  @doc "Parses positive/negative grid lines, named occurrences and named or numeric spans."
  @spec line(String.t()) :: {:ok, term()} | :error | {:error, term()}
  def line(value) do
    tokens = HtmlValidator.split_css_tokens(String.trim(value))

    tokens =
      Enum.map(tokens, fn token ->
        lower = String.downcase(token, :ascii)
        if lower in ["auto", "span"], do: lower, else: token
      end)

    span? = "span" in tokens
    parts = if span?, do: List.delete(tokens, "span"), else: tokens
    {numbers, names} = Enum.split_with(parts, &match?({_, ""}, Integer.parse(&1)))

    case {numbers, names} do
      {[], ["auto"]} when not span? ->
        {:ok, :auto}

      {[], [identifier]} ->
        with {:ok, name} <- name(identifier),
             do: {:ok, if(span?, do: {:named_span, name, 1}, else: {:named_line, name, 1})}

      {[number], identifiers} when length(identifiers) <= 1 ->
        {n, ""} = Integer.parse(number)

        if n == 0 or (span? and n < 0) do
          :error
        else
          with :ok <- HtmlValidator.validate_layout_cardinality(:grid_placement, abs(n)) do
            case identifiers do
              [] ->
                {:ok, if(span?, do: {:span, n}, else: n)}

              [identifier] ->
                with {:ok, name} <- name(identifier),
                     do:
                       {:ok, if(span?, do: {:named_span, name, n}, else: {:named_line, name, n})}
            end
          end
        end

      _ ->
        :error
    end
  end

  @doc "Parses grid-template-areas into a rectangular named-cell matrix."
  @spec areas(String.t()) :: {:ok, list()} | :error
  def areas(value) do
    if String.trim(value) == "none" do
      {:ok, []}
    else
      rows =
        Enum.map(tokens(value), fn token ->
          case CssSyntaxValidator.components(token) do
            [%{kind: :string}] ->
              text = HtmlValidator.decode_css_string(token)

              if Regex.match?(~r/\A[\w\p{L}\p{N}.\s-]+\z/u, text),
                do: Regex.scan(~r/[\w\p{L}\p{N}-]+|\.+/u, text) |> List.flatten()

            _ ->
              nil
          end
        end)

      width = if is_list(List.first(rows)), do: length(hd(rows)), else: 0
      valid? = width > 0 and Enum.all?(rows, &(is_list(&1) and length(&1) == width))

      if valid? do
        cells =
          for {row, y} <- Enum.with_index(rows),
              {name, x} <- Enum.with_index(row),
              not String.starts_with?(name, "."),
              do: {name, x, y}

        rectangular? =
          cells
          |> Enum.group_by(&elem(&1, 0))
          |> Enum.all?(fn {_, cells} ->
            xs = Enum.map(cells, &elem(&1, 1))
            ys = Enum.map(cells, &elem(&1, 2))
            length(cells) == (Enum.max(xs) - Enum.min(xs) + 1) * (Enum.max(ys) - Enum.min(ys) + 1)
          end)

        if rectangular?, do: {:ok, rows}, else: :error
      else
        :error
      end
    end
  end

  @doc "Parses the grid auto-placement axis and optional dense backfilling."
  @spec flow(String.t()) :: {:ok, atom()} | :error
  def flow(value) do
    case value |> String.downcase(:ascii) |> HtmlValidator.split_css_tokens() |> Enum.sort() do
      ["row"] -> {:ok, :row}
      ["column"] -> {:ok, :column}
      ["dense"] -> {:ok, :row_dense}
      ["dense", "row"] -> {:ok, :row_dense}
      ["column", "dense"] -> {:ok, :column_dense}
      _ -> :error
    end
  end

  @doc "Parses one or more implicit track sizes as a repeating cycle."
  @spec auto_tracks(String.t(), function()) :: {:ok, term()} | :error | {:error, term()}
  def auto_tracks(value, parser) do
    with {:ok, tracks} <- tracks(value, parser),
         true <-
           tracks != [] and
             not Enum.any?(
               tracks,
               &(match?({:repeat, _, _}, &1) or match?({:line_names, _}, &1) or
                   match?({:subgrid, _}, &1))
             ) do
      case tracks do
        [track] -> {:ok, track}
        tracks -> {:ok, {:track_cycle, tracks}}
      end
    else
      {:error, _} = error -> error
      _ -> :error
    end
  end

  @doc "Prepares grid-template and grid shorthands, including named area row sizes."
  @spec template(String.t(), String.t(), function()) :: {:ok, map()} | :error | {:error, term()}
  def template(property, value, parser) do
    initial = %{grid_template_columns: [], grid_template_rows: [], grid_template_areas: []}

    initial =
      if property == "grid",
        do:
          Map.merge(initial, %{
            grid_auto_columns: :auto,
            grid_auto_rows: :auto,
            grid_auto_flow: :row
          }),
        else: initial

    case String.trim(value) do
      "none" ->
        {:ok, initial}

      value ->
        parts = CssSyntaxValidator.split(value, "/")

        parts =
          case parts do
            [rows] ->
              if Enum.any?(tokens(rows), &String.starts_with?(&1, ["\"", "'"])),
                do: [rows, "none"],
                else: parts

            _ ->
              parts
          end

        case parts do
          [rows, columns] ->
            row_tokens = tokens(rows)
            column_tokens = tokens(columns)

            cond do
              property == "grid" and "auto-flow" in row_tokens ->
                with {:ok, columns} <- tracks(columns, parser),
                     {:ok, size, dense?} <- auto_flow_track(row_tokens, parser),
                     do:
                       {:ok,
                        Map.merge(initial, %{
                          grid_template_columns: columns,
                          grid_auto_rows: size,
                          grid_auto_flow: if(dense?, do: :row_dense, else: :row)
                        })}

              property == "grid" and "auto-flow" in column_tokens ->
                with {:ok, rows} <- tracks(rows, parser),
                     {:ok, size, dense?} <- auto_flow_track(column_tokens, parser),
                     do:
                       {:ok,
                        Map.merge(initial, %{
                          grid_template_rows: rows,
                          grid_auto_columns: size,
                          grid_auto_flow: if(dense?, do: :column_dense, else: :column)
                        })}

              Enum.any?(
                row_tokens,
                &(String.starts_with?(&1, "\"") or String.starts_with?(&1, "'"))
              ) ->
                {area_rows, row_sizes} =
                  Enum.reduce(row_tokens, {[], []}, fn token, {areas, sizes} ->
                    if String.starts_with?(token, ["\"", "'"]),
                      do: {[token | areas], ["auto" | sizes]},
                      else:
                        {areas,
                         case sizes do
                           ["auto" | rest] -> [token | rest]
                           _ -> [token | sizes]
                         end}
                  end)

                with {:ok, areas} <- areas(area_rows |> Enum.reverse() |> Enum.join(" ")),
                     {:ok, rows} <- tracks(row_sizes |> Enum.reverse() |> Enum.join(" "), parser),
                     {:ok, columns} <- tracks(columns, parser),
                     do:
                       {:ok,
                        Map.merge(initial, %{
                          grid_template_columns: columns,
                          grid_template_rows: rows,
                          grid_template_areas: areas
                        })}

              true ->
                with {:ok, rows} <- tracks(rows, parser),
                     {:ok, columns} <- tracks(columns, parser),
                     do:
                       {:ok,
                        Map.merge(initial, %{
                          grid_template_columns: columns,
                          grid_template_rows: rows
                        })}
            end

          _ ->
            :error
        end
    end
  end

  defp auto_flow_track(tokens, parser) do
    dense? = "dense" in tokens
    sizes = Enum.reject(tokens, &(&1 in ["auto-flow", "dense"]))

    with true <-
           Enum.count(tokens, &(&1 == "auto-flow")) == 1 and
             Enum.count(tokens, &(&1 == "dense")) <= 1,
         {:ok, size} <-
           auto_tracks(if(sizes == [], do: "auto", else: Enum.join(sizes, " ")), parser),
         do: {:ok, size, dense?},
         else: (_ -> :error)
  end

  @doc "Freezes grid length units at the selected element font, retaining percentage bases."
  @spec compute_style(map()) :: {:ok, map()} | :error | {:error, term()}
  def compute_style(style) do
    case style do
      style
      when not is_map_key(style, :grid_template_columns) and
             not is_map_key(style, :grid_template_rows) and
             not is_map_key(style, :grid_auto_columns) and
             not is_map_key(style, :grid_auto_rows) ->
        {:ok, style}

      _ ->
        Enum.reduce_while(
          [:grid_template_columns, :grid_template_rows, :grid_auto_columns, :grid_auto_rows],
          {:ok, style},
          fn property, {:ok, result} ->
            case compute_track(Map.get(style, property), style) do
              {:ok, value} ->
                {:cont,
                 {:ok,
                  if(Map.has_key?(style, property),
                    do: Map.put(result, property, value),
                    else: result
                  )}}

              error ->
                {:halt, error}
            end
          end
        )
    end
  end

  defp compute_track(value, style) do
    case value do
      tracks when is_list(tracks) ->
        Enum.reduce_while(tracks, {:ok, []}, fn track, {:ok, result} ->
          case compute_track(track, style) do
            {:ok, track} -> {:cont, {:ok, [track | result]}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, values} -> {:ok, Enum.reverse(values)}
          error -> error
        end

      {kind, {:css_size_math, {source, percentage?}}}
      when kind in [:length, :fit_content] and is_binary(source) ->
        with {:ok, calculation} <- CssMathValidator.compute_length(source, style),
             do: {:ok, {kind, {:css_size_math, {calculation, percentage?}}}}

      {kind, length} when kind in [:length, :fit_content] ->
        with {:ok, length} <- CssLengthValidator.resolve(length, nil, style),
             do: {:ok, {kind, length}}

      {:minmax, minimum, maximum} ->
        with {:ok, minimum} <- compute_track(minimum, style),
             {:ok, maximum} <- compute_track(maximum, style),
             do: {:ok, {:minmax, minimum, maximum}}

      {:repeat, mode, pattern} ->
        with {:ok, pattern} <- compute_track(pattern, style), do: {:ok, {:repeat, mode, pattern}}

      {:track_cycle, tracks} ->
        with {:ok, tracks} <- compute_track(tracks, style), do: {:ok, {:track_cycle, tracks}}

      _ ->
        {:ok, value}
    end
  end

  @doc "Checks supplied computed grid grammars and their shared cardinality budget before layout."
  @spec validate_style(map(), String.t()) :: :ok | {:error, term()}
  def validate_style(style, source) do
    case style do
      style
      when not is_map_key(style, :grid_template_columns) and
             not is_map_key(style, :grid_template_rows) and
             not is_map_key(style, :grid_auto_columns) and
             not is_map_key(style, :grid_auto_rows) and
             not is_map_key(style, :grid_template_areas) and
             not is_map_key(style, :grid_auto_flow) and
             not is_map_key(style, :grid_column_start) and
             not is_map_key(style, :grid_column_end) and
             not is_map_key(style, :grid_row_start) and
             not is_map_key(style, :grid_row_end) ->
        :ok

      _ ->
        HtmlValidator.with_render_budget(fn ->
          tracks? =
            Enum.all?(
              [:grid_template_columns, :grid_template_rows, :grid_auto_columns, :grid_auto_rows],
              fn key -> valid_track?(Map.get(style, key), 0) end
            )

          lines? =
            Enum.all?(
              [:grid_column_start, :grid_column_end, :grid_row_start, :grid_row_end],
              fn key ->
                case Map.get(style, key) do
                  nil ->
                    true

                  :auto ->
                    true

                  n when is_integer(n) and n != 0 ->
                    valid_cardinality?(:grid_placement, abs(n))

                  {:span, n} when is_integer(n) and n > 0 ->
                    valid_cardinality?(:grid_placement, n)

                  {kind, identifier, n}
                  when kind in [:named_line, :named_span] and is_binary(identifier) and
                         is_integer(n) and n != 0 ->
                    name(identifier) == {:ok, identifier} and (kind == :named_line or n > 0) and
                      valid_cardinality?(:grid_placement, abs(n))

                  _ ->
                    false
                end
              end
            )

          areas? =
            case Map.get(style, :grid_template_areas, []) do
              [] ->
                true

              rows when is_list(rows) ->
                if Enum.all?(
                     rows,
                     &(is_list(&1) and &1 != [] and
                         Enum.all?(&1, fn name ->
                           is_binary(name) and
                             Regex.match?(~r/\A[\w\p{L}\p{N}-]+\z|\A\.+\z/u, name)
                         end))
                   ) do
                  source = Enum.map_join(rows, " ", &("\"" <> Enum.join(&1, " ") <> "\""))
                  areas(source) == {:ok, rows}
                else
                  false
                end

              _ ->
                false
            end

          if tracks? and lines? and areas? and
               Map.get(style, :grid_auto_flow, :row) in [:row, :column, :row_dense, :column_dense],
             do: :ok,
             else:
               Diagnostics.error(
                 :layout,
                 :invalid_layout,
                 "grid requires validated tracks, named lines and row/column auto-placement; recompute styles",
                 operation: :layout,
                 module: __MODULE__,
                 source: source
               )
        end)
    end
  end

  defp valid_cardinality?(kind, count) do
    case HtmlValidator.validate_layout_cardinality(kind, count) do
      :ok -> true
      error -> throw({:render_resource_limit, error})
    end
  end

  defp valid_track?(value, depth) do
    HtmlValidator.check_render_resource(:max_depth, depth, :layout)

    case value do
      value when value in [nil, :auto, :min_content, :max_content] ->
        true

      tracks when is_list(tracks) ->
        HtmlValidator.reserve_render_resource(:max_work, length(tracks), :layout)
        count = track_count(tracks)

        (tracks == [] or
           (count > 0 and valid_cardinality?(:grid_tracks, count))) and
          Enum.all?(tracks, &valid_track?(&1, depth + 1)) and
          not Enum.any?(tracks, &(&1 == nil or is_list(&1))) and
          (not Enum.any?(tracks, &match?({:subgrid, _}, &1)) or match?([{:subgrid, _}], tracks)) and
          Enum.count(tracks, &match?({:repeat, _, _}, &1)) <= 1

      {:length, value} ->
        not match?({:css_intrinsic_size, _}, value) and
          (HtmlValidator.validate_prepared_css_size(value, :size) == :ok or
             CssLengthValidator.valid_edge?(value, :padding))

      {:fit_content, value} ->
        valid_track?({:length, value}, depth + 1)

      {:fr, value} ->
        is_number(value) and value >= 0 and
          match?({:ok, _}, HtmlValidator.normalize_geometry_number(value))

      {:minmax, minimum, maximum} ->
        minimum != nil and maximum != nil and
          not Enum.any?(
            [minimum, maximum],
            &(is_list(&1) or match?({:minmax, _, _}, &1) or match?({:fit_content, _}, &1) or
                match?({:line_names, _}, &1) or match?({:subgrid, _}, &1) or
                match?({:repeat, _, _}, &1))
          ) and
          not match?({:fr, _}, minimum) and valid_track?(minimum, depth + 1) and
          valid_track?(maximum, depth + 1)

      {:line_names, names} when is_list(names) ->
        Enum.all?(names, &(is_binary(&1) and name(&1) == {:ok, &1}))

      {:subgrid, names} when is_list(names) ->
        Enum.count(names, &match?({:auto_fill, _}, &1)) <= 1 and
          Enum.all?(names, fn group ->
            groups =
              case group do
                {:auto_fill, pattern} when is_list(pattern) and pattern != [] -> pattern
                _ -> [group]
              end

            Enum.all?(groups, fn group ->
              is_list(group) and Enum.all?(group, &(is_binary(&1) and name(&1) == {:ok, &1}))
            end)
          end)

      {:repeat, mode, tracks}
      when mode in [:auto_fit, :auto_fill] and is_list(tracks) and tracks != [] ->
        Enum.all?(tracks, &fixed_repeat_track?/1) and
          not Enum.any?(tracks, &match?({:repeat, _, _}, &1)) and valid_track?(tracks, depth + 1)

      {:track_cycle, tracks} when is_list(tracks) and tracks != [] ->
        valid_track?(tracks, depth + 1)

      _ ->
        false
    end
  end
end
