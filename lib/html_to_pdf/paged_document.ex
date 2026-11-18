defmodule NativeElixirPdfUtilities.HtmlToPdf.PagedDocument do
  @moduledoc "Coordinates static CSS publishing with shared layout and pagination."
  alias NativeElixirPdfUtilities.HtmlToPdf.{
    CounterState,
    CounterStyle,
    CssColor,
    DrawingGeometry,
    Font,
    FontFallback,
    Layout,
    PageGeometry,
    Pagination,
    Style
  }

  alias NativeElixirPdfUtilities.Validators.{
    CssPageValidator,
    CssPublishingValidator,
    CssValueValidator,
    HtmlValidator
  }

  @doc "Lays out a document, resolving publishing dependencies only when present."
  @spec prepare(map(), keyword(), keyword(), function()) ::
          {:ok, map(), [map()]} | {:error, term()}
  def prepare(tree, opts, explicit, layout) do
    CssColor.with_profiles(Map.get(tree, :css_color_profiles, %{}), fn ->
      prepare_pages(tree, opts, explicit, layout)
    end)
  end

  defp prepare_pages(tree, opts, explicit, layout) do
    active =
      Enum.any?(Map.get(tree, :css_page_rules, []), fn rule ->
        rule.margin_boxes != %{} or Enum.any?(rule.selectors, &(&1.name != "" or &1.pseudo != [])) or
          Enum.any?(rule.declarations, fn d ->
            elem(d, 0) not in ~w(size margin margin-top margin-right margin-bottom margin-left) or
              match?({:ok, nil}, CssPageValidator.declaration_option(elem(d, 0), elem(d, 1)))
          end)
      end) or active?(tree)

    if active do
      base = first_style(tree)

      {tree, registry} =
        collect(tree, %{
          options: opts,
          page_layout: {base, explicit},
          nodes: %{},
          targets: %{},
          strings: [],
          running: [],
          notes: [],
          floats: [],
          counter_styles: Map.get(base, :_counter_styles, %{}),
          page_rules: Map.get(tree, :css_page_rules, [])
        })

      state = %{
        total: 0,
        page_values: %{},
        note_values: %{},
        float_queues: %{},
        locations: %{},
        starts: MapSet.new(),
        targets: %{},
        strings: %{},
        reserves: %{},
        extra: %{},
        minimum_pages: %{}
      }

      converge(tree, registry, opts, explicit, layout, state, %{}, 0)
    else
      with {:ok, result} <- Font.with_measurement_cache(fn -> layout.(tree, opts) end),
           {:ok, result} <- Layout.resolve_images(result),
           {:ok, pages} <- Pagination.paginate(result, opts),
           do: {:ok, result, pages}
    end
  end

  @doc "Recognizes column-reference floats without changing ordinary column layout."
  @spec column_floats?([map()]) :: boolean()
  def column_floats?(nodes) do
    Enum.any?(nodes, fn node ->
      case node do
        %{style: %{float_reference: "column", publishing_float: _}} -> true
        %{children: children} -> column_floats?(children)
        _ -> false
      end
    end)
  end

  @doc "Prepares column float sources through their existing layout owner."
  @spec column_source([map()], map(), number(), function()) ::
          {:ok, [map()], number(), map()} | {:error, term()}
  def column_source(nodes, style, width, render) do
    {tree, registry} =
      collect(%{type: :document, children: nodes}, %{
        options: [],
        nodes: %{},
        targets: %{},
        strings: [],
        running: [],
        notes: [],
        floats: [],
        counter_styles: Map.get(style, :_counter_styles, %{}),
        page_rules: []
      })

    with {:ok, boxes, height} <- render.(tree.children, style, 0.0, 0.0, width),
         {:ok, _paint, extra_height} <-
           render.(
             Enum.map(registry.floats, fn {_id, node} -> clean_capture(node) end),
             style,
             0.0,
             0.0,
             width
           ) do
      {:ok, boxes, height + extra_height, registry}
    end
  end

  @doc "Allocates column floats with the same finite queues and resource budgets as page floats."
  @spec column_fragments([map()], map(), number(), number()) :: {:ok, [map()]} | {:error, term()}
  def column_fragments(source, registry, width, height) do
    state = %{
      total: 0,
      page_values: %{},
      note_values: %{},
      float_queues: %{},
      locations: %{},
      starts: MapSet.new(),
      targets: %{},
      strings: %{},
      reserves: %{},
      extra: %{},
      minimum_pages: %{}
    }

    column_pass(source, registry, width, height, state, %{}, 0)
  end

  @spec column_pass([map()], map(), number(), number(), map(), map(), non_neg_integer()) ::
          {:ok, [map()]} | {:error, term()}
  defp column_pass(source, registry, width, height, state, seen, iteration) do
    signature = :crypto.hash(:sha256, :erlang.term_to_binary(Map.drop(state, [:extra])))

    with :ok <- CssPublishingValidator.convergence(iteration, signature, seen),
         {:ok, pages} <-
           Pagination.fragment_columns(source, width, height, %{
             reserves: state.reserves,
             page_offset: 0,
             direction: :ltr,
             minimum_pages: state.minimum_pages
           }),
         pages =
           Enum.map(
             pages,
             &Map.merge(&1, %{margins: CssValueValidator.edges(0.0), page_name: "", blank: false})
           ),
         next = %{state | total: length(pages), locations: locations(pages)},
         {:ok, next} <- allocate(registry, pages, [], next),
         pages = continuation_pages(pages, next.extra),
         next = %{next | total: length(pages)} do
      if Map.drop(next, [:extra]) == Map.drop(state, [:extra]) do
        {:ok,
         pages
         |> Enum.with_index(1)
         |> Enum.map(fn {page, n} ->
           %{page | boxes: page.boxes ++ Map.get(next.extra, n, [])}
         end)}
      else
        column_pass(
          source,
          registry,
          width,
          height,
          next,
          Map.put(seen, signature, true),
          iteration + 1
        )
      end
    end
  end

  defp active?(node) do
    case node do
      %{publishing_content: _} ->
        true

      %{style: style}
      when is_map_key(style, :page_name) or is_map_key(style, :string_set) or
             is_map_key(style, :running_name) or is_map_key(style, :publishing_float) or
             is_map_key(style, :bookmark_level) ->
        true

      %{children: children} ->
        Enum.any?(children, &active?/1)

      _ ->
        false
    end
  end

  defp collect(node, registry, inside_columns \\ false) do
    if node.type == :text do
      {Map.put(node, :publishing_text_id, make_ref()), registry}
    else
      id = Map.get(node, :node_id, make_ref())
      style = Map.get(node, :style)
      registry = %{registry | nodes: Map.put(registry.nodes, id, node)}
      target = get_in(node, [:attributes, "id"])

      registry =
        if target == nil,
          do: registry,
          else: %{registry | targets: Map.put_new(registry.targets, target, id)}

      registry =
        if style != nil and Map.get(style, :string_set, []) != [],
          do: %{registry | strings: registry.strings ++ [{id, style.string_set}]},
          else: registry

      column_children =
        inside_columns or
          (style != nil and NativeElixirPdfUtilities.HtmlToPdf.ColumnLayout.enabled?(style))

      {children, registry} =
        Enum.map_reduce(Map.get(node, :children, []), registry, fn child, acc ->
          collect(child, acc, column_children)
        end)

      node = Map.put(node, :children, children)

      node =
        if style == nil,
          do: node,
          else: Map.put(node, :style, Map.put(style, :publishing_anchor, id))

      cond do
        match?(%{display: :none}, style) ->
          {node, registry}

        style != nil and is_binary(Map.get(style, :running_name)) ->
          registry = %{registry | running: registry.running ++ [{id, style.running_name, node}]}
          {if(Map.get(style, :running_copy, false), do: node, else: marker(node)), registry}

        style != nil and Map.get(style, :publishing_float) == :footnote ->
          counter_state = Map.get(node, :publishing_counters, CounterState.new())
          number = CounterState.value(counter_state, "footnote")

          registry = %{registry | notes: registry.notes ++ [{id, number, node}]}

          call_style =
            Map.get(
              style,
              :footnote_call,
              Style.anonymous_box_style(style, :inline)
              |> Map.put(:font_size, style.font_size * 0.7)
            )
            |> Map.put(:publishing_anchor, id)

          counter =
            CounterState.apply(
              %{display: :block, counter_set: [{"footnote", number}]},
              CounterState.new(),
              CounterState.new(),
              id
            )

          call_children =
            publishing_result(
              content_nodes(
                Map.get(call_style, :content, [{:counter, "footnote"}]),
                call_style,
                registry,
                {Map.get(node, :attributes, %{}), counter, %{total: 0, locations: %{}}, 1},
                nil,
                id
              )
            )

          call = %{
            type: :element,
            tag: "span",
            style: call_style,
            children: call_children
          }

          {call, registry}

        style != nil and Map.has_key?(style, :publishing_float) ->
          if inside_columns and Map.get(style, :float_reference) == "column",
            do: {node, registry},
            else: {marker(node), %{registry | floats: registry.floats ++ [{id, node}]}}

        true ->
          {node, registry}
      end
    end
  end

  defp marker(node) do
    style =
      Style.anonymous_box_style(node.style, :block)
      |> Map.merge(%{
        height: 0.0,
        margin: CssValueValidator.edges(0.0),
        publishing_anchor: node.style.publishing_anchor
      })

    %{node | style: style, children: []}
  end

  @spec converge(
          map(),
          map(),
          keyword(),
          keyword(),
          function(),
          map(),
          map(),
          non_neg_integer()
        ) :: {:ok, map(), [map()]} | {:error, term()}
  defp converge(tree, registry, opts, explicit, layout, state, seen, iteration) do
    signature = :crypto.hash(:sha256, :erlang.term_to_binary(Map.drop(state, [:extra])))

    with :ok <- CssPublishingValidator.convergence(iteration, signature, seen),
         {:ok, resolved} <- resolve_tree(tree, registry, state, 1),
         {:ok, resolved} <-
           FontFallback.resolve(resolved, Keyword.get(opts, :unsupported_glyphs, :replace)),
         {:ok, result, pages} <- layout_segments(resolved, opts, explicit, layout, state),
         locations = locations(pages),
         next = %{
           state
           | total: length(pages),
             locations: locations,
             starts: page_starts(pages),
             page_values: page_counter_values(pages, tree),
             targets: target_values(registry, locations)
         },
         next = %{next | note_values: note_counter_values(registry, next, pages)},
         {:ok, next} <- allocate(registry, pages, opts, next),
         pages = continuation_pages(pages, next.extra),
         next = %{next | total: length(pages)} do
      if Map.drop(next, [:extra]) == Map.drop(state, [:extra]) do
        with {:ok, pages} <- add_margin_boxes(pages, tree, registry, next, opts, explicit),
             {:ok, pages} <- Pagination.finalize(pages, result, opts) do
          {:ok, result, pages}
        end
      else
        converge(
          tree,
          registry,
          opts,
          explicit,
          layout,
          next,
          Map.put(seen, signature, true),
          iteration + 1
        )
      end
    end
  end

  defp resolve_tree(node, registry, state, page) do
    case node do
      %{publishing_content: content} when is_list(content.parts) ->
        content =
          case Map.get(content, :note_id) do
            nil ->
              content

            id ->
              number =
                Map.get(state.note_values, id, CounterState.value(content.counters, "footnote"))

              %{
                content
                | counters:
                    CounterState.apply(
                      %{display: :block, counter_set: [{"footnote", number}]},
                      content.counters,
                      content.counters,
                      id
                    )
              }
          end

        registry =
          Map.put(
            registry,
            :quote_pairs,
            Map.get(content, :quote_pairs, [{"“", "”"}, {"‘", "’"}])
          )

        if Enum.any?(content.parts, &match?({:leader, _}, &1)) do
          children =
            Enum.reduce_while(Enum.with_index(content.parts), {:ok, []}, fn {part, index},
                                                                            {:ok, children} ->
              case content_text(
                     [part],
                     content.attributes,
                     content.counters,
                     registry,
                     state,
                     page
                   ) do
                {:ok, text} ->
                  style =
                    case part do
                      {:leader, pattern} -> Map.put(node.style, :_publishing_leader, pattern)
                      _ -> node.style
                    end

                  {:cont,
                   {:ok,
                    children ++
                      [
                        %{
                          type: :text,
                          text: publishing_text(text, style, Map.get(content, :language, [])),
                          style: style,
                          publishing_text_id: {node.publishing_text_id, index}
                        }
                      ]}}

                error ->
                  {:halt, error}
              end
            end)

          with {:ok, children} <- children do
            {:ok,
             %{
               type: :element,
               tag: "span",
               style: Style.anonymous_box_style(node.style, :inline),
               children: children
             }}
          end
        else
          case content_text(
                 content.parts,
                 content.attributes,
                 content.counters,
                 registry,
                 state,
                 page
               ) do
            {:ok, text} ->
              {:ok,
               %{node | text: publishing_text(text, node.style, Map.get(content, :language, []))}}

            error ->
              error
          end
        end

      %{children: children} ->
        children
        |> Enum.reduce_while({:ok, []}, fn child, {:ok, children} ->
          case resolve_tree(
                 child,
                 registry,
                 state,
                 Map.get(state.locations, get_in(child, [:style, :publishing_anchor]), page)
               ) do
            {:ok, child} -> {:cont, {:ok, [child | children]}}
            error -> {:halt, error}
          end
        end)
        |> case do
          {:ok, children} -> {:ok, %{node | children: Enum.reverse(children)}}
          error -> error
        end

      _ ->
        {:ok, node}
    end
  end

  defp publishing_text(text, style, language) do
    case Map.get(style, :text_transform, :none) do
      transform when transform in [:uppercase, :lowercase, :capitalize] ->
        NativeElixirPdfUtilities.HtmlToPdf.TextTransform.convert(text, transform, language)

      _ ->
        text
    end
  end

  defp content_text(parts, attributes, counters, registry, state, page) do
    {parts, _} =
      Enum.map_reduce(parts, counters, fn part, scope ->
        case part do
          {:quote, kind} ->
            {value, depth} =
              NativeElixirPdfUtilities.Validators.HtmlQuoteValidator.token(
                kind,
                Map.get(registry, :quote_pairs, [{"“", "”"}, {"‘", "’"}]),
                CounterState.value(scope, :_quote_depth)
              )

            {{:string, value}, CounterState.quote_depth(scope, depth)}

          _ ->
            {part, scope}
        end
      end)

    Enum.reduce_while(parts, {:ok, []}, fn part, {:ok, text} ->
      value =
        case part do
          {:string, value} ->
            {:ok, value}

          {:counter, "page"} ->
            {:ok,
             Integer.to_string(
               if(Map.get(state, :page_counter_scope, false),
                 do: CounterState.value(counters, "page"),
                 else: Map.get(Map.get(state, :page_values, %{}), page, page)
               )
             )}

          {:counter, "pages"} ->
            {:ok, Integer.to_string(state.total)}

          {:counter, name} ->
            {:ok, Integer.to_string(CounterState.value(counters, name))}

          {:counter, name, style} ->
            {:ok,
             CounterStyle.format(
               style,
               if(name == "page",
                 do:
                   if(Map.get(state, :page_counter_scope, false),
                     do: CounterState.value(counters, "page"),
                     else: Map.get(Map.get(state, :page_values, %{}), page, page)
                   ),
                 else:
                   if(name == "pages", do: state.total, else: CounterState.value(counters, name))
               ),
               registry.counter_styles
             )}

          {:counters, name, separator, style} ->
            {:ok,
             Enum.map_join(
               CounterState.values(counters, name),
               separator,
               &CounterStyle.format(style, &1, registry.counter_styles)
             )}

          {:leader, pattern} ->
            {:ok, pattern}

          {:target_counter, target, name, style} ->
            with {:ok, id} <- target_id(target, registry) do
              target_node = Map.fetch!(registry.nodes, id)

              value =
                if name == "page",
                  do:
                    Map.get(
                      Map.get(state, :page_values, %{}),
                      Map.get(state.locations, id, 0),
                      Map.get(state.locations, id, 0)
                    ),
                  else:
                    CounterState.value(
                      Map.get(target_node, :publishing_counters, CounterState.new()),
                      name
                    )

              {:ok, CounterStyle.format(style, value, registry.counter_styles)}
            end

          {:target_text, target, kind} ->
            with {:ok, id} <- target_id(target, registry) do
              target_node = Map.fetch!(registry.nodes, id)
              {:ok, selected_text(target_node, kind)}
            end

          {:string, name, selection} ->
            running_string(name, selection, page, registry, state)

          {:content, kind} ->
            {:ok, Map.get(attributes, kind, "")}
        end

      case value do
        {:ok, value} ->
          HtmlValidator.reserve_render_resource(:max_output_bytes, byte_size(value), :pagination)
          {:cont, {:ok, [value | text]}}

        error ->
          {:halt, error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, values |> Enum.reverse() |> IO.iodata_to_binary()}
      error -> error
    end
  end

  defp target_id(target, registry) do
    target =
      target
      |> String.trim()
      |> String.replace(~r/^url\((.*)\)$/u, "\\1")
      |> HtmlValidator.decode_css_string()
      |> String.trim_leading("#")

    case Map.fetch(registry.targets, URI.decode(target)) do
      {:ok, id} ->
        {:ok, id}

      :error ->
        CssPublishingValidator.failure(
          "generated reference has no document target; add the referenced id",
          target
        )
    end
  end

  defp selected_text(node, kind) do
    children = Map.get(node, :children, [])

    selected =
      case kind do
        "before" -> Enum.filter(children, &(Map.get(&1, :tag) == "::before"))
        "after" -> Enum.filter(children, &(Map.get(&1, :tag) == "::after"))
        _ -> Enum.reject(children, &(Map.get(&1, :tag) in ["::before", "::after"]))
      end

    text = text(selected)
    if kind == "first-letter", do: String.first(String.trim_leading(text)) || "", else: text
  end

  defp text(nodes) do
    Enum.map_join(nodes, fn node ->
      case node do
        %{text: text} -> text
        _ -> text(Map.get(node, :children, []))
      end
    end)
  end

  defp target_values(registry, locations) do
    Map.new(registry.targets, fn {name, id} -> {name, Map.get(locations, id, 0)} end)
  end

  defp locations(pages) do
    pages
    |> Enum.with_index(1)
    |> Enum.reduce(%{}, fn {page, index}, acc -> events(page.boxes, index, acc) end)
  end

  defp events(boxes, page, acc) do
    Enum.reduce(boxes, acc, fn box, acc ->
      acc = Enum.reduce(Map.get(box, :publishing_targets, []), acc, &Map.put_new(&2, &1, page))
      events(Map.get(box, :boxes, []), page, acc)
    end)
  end

  defp layout_segments(tree, opts, explicit, layout, state) do
    rules = Map.get(tree, :css_page_rules, [])
    group_names = CssPageValidator.group_names(rules)

    segments =
      case split_nodes(tree.children, "", [], group_names) do
        [] -> [[]]
        nodes -> chunk_page_nodes(nodes)
      end

    Enum.reduce_while(segments, {:ok, nil, [], %{}}, fn segment,
                                                        {:ok, result, pages, group_counts} ->
      {name, groups} =
        case segment do
          [] -> {"", []}
          [{key, _} | _] -> key
        end

      number = length(pages) + 1

      positioned =
        Enum.group_by(groups, &elem(&1, 0), fn {_, id} -> Map.get(group_counts, id, 0) + 1 end)

      selected =
        CssPageValidator.select(
          Map.get(tree, :css_page_rules, []),
          name,
          number,
          false,
          Map.get(first_style(tree), :direction, :ltr),
          positioned
        )

      part = %{tree | children: Enum.map(segment, &elem(&1, 1))}

      with {:ok, options, page_margins} <-
             page_options(selected, opts, explicit, first_style(tree)),
           {:ok, measured} <- Font.with_measurement_cache(fn -> layout.(part, options) end),
           {:ok, measured} <- Layout.resolve_images(measured),
           measured = Map.put(measured, :publishing_defer_finalization, true),
           measured =
             Map.put(measured, :publishing_context, %{
               reserves: state.reserves,
               page_offset: length(pages),
               direction: Map.get(first_style(tree), :direction, :ltr),
               minimum_pages: state.minimum_pages
             }),
           {:ok, placed} <-
             if(variable_geometry?(tree, name, number, opts, explicit),
               do:
                 varying_pages(part, opts, explicit, layout, state, name, number, [], positioned),
               else: Pagination.paginate(measured, options)
             ) do
        placed =
          Enum.with_index(placed, fn page, index ->
            page
            |> Map.put(:page_name, name)
            |> Map.put(:page_groups, group_positions(positioned, index))
            |> Map.put_new(:margins, measured.margins)
            |> Map.put_new(:page_box_margins, page_margins)
          end)

        group_counts =
          Enum.reduce(groups, group_counts, fn {_, id}, counts ->
            Map.update(counts, id, length(placed), &(&1 + length(placed)))
          end)

        {:cont,
         {:ok, result || measured,
          pages ++
            Enum.map(placed, fn page ->
              Map.put(
                page,
                :blank,
                Enum.any?(page.boxes, &(Map.get(&1, :role) == :publishing_blank))
              )
            end), group_counts}}
      else
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, measured, pages, _groups} -> {:ok, measured, pages}
      error -> error
    end
  end

  defp group_positions(positions, delta) do
    Map.new(positions, fn {name, indices} -> {name, Enum.map(indices, &(&1 + delta))} end)
  end

  defp variable_geometry?(tree, name, number, opts, explicit) do
    rules = Map.get(tree, :css_page_rules, [])

    options =
      for n <- number..(number + 2),
          do:
            page_options(
              CssPageValidator.select(
                rules,
                name,
                n,
                false,
                Map.get(first_style(tree), :direction, :ltr)
              ),
              opts,
              explicit,
              first_style(tree)
            )

    CssPageValidator.positional_geometry?(rules) or length(Enum.uniq(options)) > 1
  end

  defp varying_pages(tree, opts, explicit, layout, state, name, number, pages, positions) do
    HtmlValidator.check_render_resource(:max_pages, number, :pagination)

    selected =
      CssPageValidator.select(
        Map.get(tree, :css_page_rules, []),
        name,
        number,
        false,
        Map.get(first_style(tree), :direction, :ltr),
        group_positions(positions, length(pages))
      )

    with {:ok, options, page_margins} <- page_options(selected, opts, explicit, first_style(tree)),
         {:ok, measured} <- Font.with_measurement_cache(fn -> layout.(tree, options) end),
         {:ok, measured} <- Layout.resolve_images(measured),
         measured =
           measured
           |> Map.put(:publishing_defer_finalization, true)
           |> Map.put(:publishing_context, %{
             reserves: state.reserves,
             page_offset: number - 1,
             minimum_pages: state.minimum_pages,
             direction: Map.get(first_style(tree), :direction, :ltr)
           }),
         {:ok, fragments} <- Pagination.paginate(measured, options) do
      [first | rest] = fragments

      first =
        Map.merge(first, %{
          page_name: name,
          margins: measured.margins,
          page_box_margins: page_margins,
          blank: Enum.any?(first.boxes, &(Map.get(&1, :role) == :publishing_blank))
        })

      cond do
        rest == [] ->
          {:ok, pages ++ [first]}

        first.blank ->
          varying_pages(
            tree,
            opts,
            explicit,
            layout,
            state,
            name,
            number + 1,
            pages ++ [first],
            positions
          )

        true ->
          {ranges, anchors} = consumption(first.boxes, %{}, MapSet.new())
          {remaining, changed} = consume_nodes(tree.children, ranges, anchors)

          if changed do
            varying_pages(
              %{tree | children: remaining},
              opts,
              explicit,
              layout,
              state,
              name,
              number + 1,
              pages ++ [first],
              positions
            )
          else
            CssPublishingValidator.failure(
              "mixed page geometry could not advance through the remaining source",
              name
            )
          end
      end
    end
  end

  defp consumption(boxes, ranges, anchors) do
    Enum.reduce(boxes, {ranges, anchors}, fn box, {ranges, anchors} ->
      ranges =
        Map.merge(ranges, Map.get(box, :publishing_text_ranges, %{}), fn _id, a, b ->
          max(a, b)
        end)

      anchors = Enum.reduce(Map.get(box, :publishing_targets, []), anchors, &MapSet.put(&2, &1))
      consumption(Map.get(box, :boxes, []), ranges, anchors)
    end)
  end

  defp consume_nodes(nodes, ranges, anchors) do
    {nodes, changed} =
      Enum.map_reduce(nodes, false, fn node, changed ->
        case node do
          %{type: :text} ->
            id = Map.get(node, :publishing_text_id)

            count =
              Map.get(ranges, id, Map.get(node, :publishing_offset, 0)) -
                Map.get(node, :publishing_offset, 0)

            remaining =
              node.text |> String.codepoints() |> Enum.drop(max(count, 0)) |> Enum.join()

            if count > 0,
              do:
                {if(remaining == "",
                   do: nil,
                   else:
                     %{node | text: remaining}
                     |> Map.put(:publishing_offset, Map.get(node, :publishing_offset, 0) + count)
                 ), true},
              else: {node, changed}

          %{children: children} ->
            {remaining, consumed} = consume_nodes(children, ranges, anchors)
            style = Map.get(node, :style, %{})
            started = MapSet.member?(anchors, Map.get(style, :publishing_anchor))

            if started and remaining == [] and Map.get(node, :tag) not in ["html", "body"] do
              {nil, true}
            else
              style =
                if started do
                  style =
                    style
                    |> Map.drop([:publishing_anchor, :html_target])
                    |> Map.put(:break_before, :auto)

                  if Map.get(style, :box_decoration_break, "slice") == "clone",
                    do: style,
                    else:
                      style
                      |> Map.update(
                        :margin,
                        CssValueValidator.edges(0.0),
                        &Map.put(&1, :top, 0.0)
                      )
                      |> Map.update(
                        :padding,
                        CssValueValidator.edges(0.0),
                        &Map.put(&1, :top, 0.0)
                      )
                      |> Map.update(
                        :border_widths,
                        CssValueValidator.edges(0.0),
                        &Map.put(&1, :top, 0.0)
                      )
                else
                  style
                end

              node = %{node | children: remaining}

              {if(Map.has_key?(node, :style), do: %{node | style: style}, else: node),
               changed or consumed}
            end
        end
      end)

    {Enum.reject(nodes, &is_nil/1), changed}
  end

  defp split_nodes(nodes, inherited, parent_groups, tracked) do
    Enum.flat_map(nodes, fn node ->
      style = Map.get(node, :style, %{})
      authored = Map.get(style, :page_name)
      name = authored || inherited

      groups =
        if MapSet.member?(tracked, authored) and
             (Map.get(style, :break_before) in [:page, :left, :right, :recto, :verso] or
                Map.get(style, :break_after) in [:page, :left, :right, :recto, :verso]),
           do: [{authored, make_ref()} | parent_groups],
           else: parent_groups

      case node do
        %{type: :element, children: [_ | _] = children} ->
          split_nodes(children, name, groups, tracked)
          |> chunk_page_nodes()
          |> Enum.map(fn group ->
            {elem(hd(group), 0), %{node | children: Enum.map(group, &elem(&1, 1))}}
          end)

        _ ->
          [{{name, groups}, node}]
      end
    end)
  end

  # A named page boundary can isolate indentation that would collapse within
  # ordinary flow. Do not lay out that indentation as an entire page segment.
  defp chunk_page_nodes(nodes) do
    groups = Enum.chunk_by(nodes, &elem(&1, 0))

    if match?([_, _ | _], groups) do
      Enum.reject(groups, fn group ->
        Enum.all?(group, fn
          {_, %{type: :text, text: text, style: style}} ->
            collapse = Map.get(style, :white_space_collapse)
            whitespace = Map.get(style, :white_space, :normal)

            (collapse in [:collapse, :discard] or
               (collapse == nil and whitespace in [:normal, :nowrap])) and
              HtmlValidator.trim_ascii_whitespace(text) == ""

          _ ->
            false
        end)
      end)
    else
      groups
    end
  end

  defp page_options(selected, opts, explicit, base) do
    box_declarations =
      Enum.filter(selected.declarations, fn {p, _} ->
        String.starts_with?(p, ["font", "border", "padding"]) or p == "line-height"
      end)

    with {:ok, style} <-
           apply_declarations(Style.anonymous_box_style(base, :block), box_declarations),
         {:ok, options} <- CssPageValidator.resolve_geometry(selected, opts, explicit, style),
         {:ok, {width, _height}} <- PageGeometry.normalize_page_size(options[:page_size]),
         {:ok, margins} <- PageGeometry.normalize_margins(options[:margin]),
         {:ok, style} <-
           NativeElixirPdfUtilities.Validators.CssLengthValidator.resolve_edges(style, width) do
      content =
        Map.new(margins, fn {side, value} ->
          {side, value + Map.fetch!(style.padding, side) + Map.fetch!(style.border_widths, side)}
        end)

      {:ok, Keyword.put(options, :margin, content), margins}
    end
  end

  defp running_string(name, selection, page, registry, state) do
    entries =
      for {id, strings} <- registry.strings,
          {^name, parts} <- strings,
          at = Map.get(state.locations, id, 0),
          at > 0 and at <= page,
          do: {id, at, parts}

    same = Enum.filter(entries, &(elem(&1, 1) == page))
    previous = Enum.filter(entries, &(elem(&1, 1) < page))

    chosen =
      choose_running(same, previous, selection, fn {id, _, _} ->
        MapSet.member?(state.starts, id)
      end)

    case chosen do
      nil ->
        {:ok, ""}

      {id, at, parts} ->
        node = Map.fetch!(registry.nodes, id)

        attributes =
          Map.new(~w(text before after first-letter), fn kind ->
            {kind, selected_text(node, if(kind == "text", do: "content", else: kind))}
          end)

        content_text(
          parts,
          Map.merge(Map.get(node, :attributes, %{}), attributes),
          Map.get(node, :publishing_counters, CounterState.new()),
          %{registry | strings: []},
          state,
          at
        )
    end
  end

  defp choose_running(same, previous, selection, at_start) do
    case selection do
      "first-except" -> if same == [], do: List.last(previous)
      "start" -> Enum.find(same, at_start) || List.last(previous)
      "last" -> List.last(same) || List.last(previous)
      _ -> List.first(same) || List.last(previous)
    end
  end

  defp page_starts(pages) do
    Enum.reduce(pages, MapSet.new(), fn page, starts ->
      boxes = source_boxes(page.boxes)

      top =
        boxes
        |> Enum.map(&elem(PageGeometry.box_vertical_bounds(&1), 0))
        |> Enum.max(fn -> 0.0 end)

      Enum.reduce(boxes, starts, fn box, starts ->
        if abs(elem(PageGeometry.box_vertical_bounds(box), 0) - top) <= 1.0,
          do: Enum.reduce(Map.get(box, :publishing_targets, []), starts, &MapSet.put(&2, &1)),
          else: starts
      end)
    end)
  end

  defp source_boxes(boxes) do
    Enum.flat_map(boxes, fn box ->
      cond do
        Map.has_key?(box, :boxes) -> source_boxes(box.boxes)
        box.type in [:text, :flow_marker] -> [box]
        true -> []
      end
    end)
  end

  defp allocate(registry, pages, _opts, state) do
    initial = %{state | reserves: %{}, extra: %{}, float_queues: %{}}

    items =
      Enum.map(registry.floats, fn {id, node} -> {id, :float, nil, node} end) ++
        Enum.map(inline_notes(registry.notes, state), fn {id, number, node} ->
          {id, :footnote, number, node}
        end)

    Enum.reduce_while(items, {:ok, initial}, fn {id, kind, number, node}, {:ok, next} ->
      source_page = Map.get(state.locations, id, 1)
      minimum = max(Map.get(next.minimum_pages, id, 1), source_page)
      next = %{next | minimum_pages: Map.put(next.minimum_pages, id, minimum)}
      defer = Map.get(node.style, :float_defer, 0)

      target =
        cond do
          kind == :footnote -> minimum
          defer == :last -> max(length(pages), minimum)
          defer < 0 -> max(length(pages) + 1 + defer, minimum)
          true -> minimum + defer
        end

      side =
        if kind == :footnote or
             Map.get(node.style, :publishing_float) in [
               "bottom",
               "page-bottom",
               "column-bottom",
               "block-end"
             ],
           do: :bottom,
           else: :top

      queue = {side, Map.get(node.style, :float_reference, "page")}

      target =
        if kind == :float, do: max(target, Map.get(next.float_queues, queue, 1)), else: target

      page = Enum.at(pages, min(target, length(pages)) - 1)
      width = elem(page.size, 0) - page.margins.left - page.margins.right
      full = elem(page.size, 1) - page.margins.top - page.margins.bottom

      existing = Map.get(next.reserves, target, %{top: 0.0, bottom: 0.0})
      call_top = event_top(page.boxes, id) || elem(page.size, 1) - page.margins.top
      line = Map.get(node.style, :line_height, node.style.font_size * 1.2)

      available =
        if kind == :footnote,
          do: max(call_top - page.margins.bottom - line - existing.bottom - 6.0, line),
          else: full - existing.top - existing.bottom

      selected =
        CssPageValidator.select(
          registry.page_rules,
          page.page_name,
          target,
          Map.get(page, :blank, false),
          Map.get(node.style, :direction, :ltr),
          Map.get(page, :page_groups, %{})
        )

      area_declarations =
        if kind == :footnote, do: Map.get(selected.margin_boxes, "footnote", []), else: []

      area_style =
        if area_declarations == [] do
          Style.anonymous_box_style(node.style, :block)
        else
          publishing_result(
            with {:ok, style} <-
                   apply_declarations(
                     Style.anonymous_box_style(node.style, :block),
                     area_declarations
                   ) do
              Style.prepare_publishing_paint(
                style,
                registry.options,
                get_in(selected.margin_contexts, ["footnote", :background])
              )
            end
          )
        end

      capacity = max(min(available, full - line - 6.0), line)

      capacity =
        case CssPublishingValidator.footnote_capacity(area_style, capacity, full, line) do
          {:ok, value} -> value
          error -> throw({:render_resource_limit, error})
        end

      capture = clean_capture(node)

      capture = %{
        capture
        | style:
            Map.merge(capture.style, %{display: :block, margin: CssValueValidator.edges(0.0)})
      }

      capture =
        if kind == :footnote do
          components = Map.get(node.style, :note_components)

          if components == nil do
            note_capture(capture, node, id, number, registry, state, source_page)
          else
            children =
              Enum.flat_map(components, fn {note_id, note_number, original} ->
                captured =
                  note_capture(
                    clean_capture(original),
                    original,
                    note_id,
                    note_number,
                    registry,
                    state,
                    source_page
                  )

                captured = %{
                  captured
                  | style:
                      Map.merge(captured.style, %{
                        display: :inline,
                        margin: CssValueValidator.edges(0.0)
                      })
                }

                [
                  captured,
                  %{
                    type: :text,
                    text: " ",
                    style: Style.anonymous_box_style(original.style, :inline)
                  }
                ]
              end)

            %{capture | children: children}
          end
        else
          capture
        end

      capture =
        if area_declarations == [],
          do: capture,
          else: %{
            type: :element,
            tag: "div",
            style: Map.drop(area_style, [:max_height, :height]),
            children: [capture]
          }

      with {:ok, capture} <- resolve_tree(capture, registry, state, source_page),
           {:ok, boxes} <- surface([capture], width, capacity),
           {top, bottom} = bounds(boxes, capacity),
           {target, fragment_capacity, next} =
             note_policy(
               kind,
               node,
               id,
               target,
               capacity,
               top - bottom,
               full,
               line,
               registry,
               next
             ),
           target =
             if(
               kind == :float and defer in [:last, -1] and target <= length(pages) and
                 top - bottom > body_free_space(page),
               do: target + 1,
               else: target
             ),
           {:ok, fragments} <-
             Pagination.fragment_columns(
               DrawingGeometry.translate_boxes(boxes, 0.0, -capacity),
               width,
               fragment_capacity
             ),
           {:ok, next, last} <-
             place_allocations(
               fragments,
               target,
               side,
               page,
               next,
               kind,
               line,
               Map.get(node.style, :float_offset, 0.0),
               {pages, registry}
             ) do
        next =
          if kind == :float,
            do: %{next | float_queues: Map.put(next.float_queues, queue, last)},
            else: next

        {:cont, {:ok, next}}
      else
        error -> {:halt, error}
      end
    end)
  end

  defp inline_notes(notes, state) do
    notes
    |> Enum.chunk_by(fn {id, _number, node} ->
      if Map.get(node.style, :footnote_display, "block") == "block",
        do: {:block, id},
        else: {:inline, Map.get(state.locations, id, 1)}
    end)
    |> Enum.map(fn group ->
      {id, number, node} = hd(group)

      if length(group) == 1,
        do: {id, number, node},
        else: {id, number, %{node | style: Map.put(node.style, :note_components, group)}}
    end)
  end

  defp note_capture(capture, node, id, number, registry, state, source_page) do
    number = Map.get(state.note_values, id, number)

    marker_style =
      Map.get(node.style, :footnote_marker, Style.anonymous_box_style(capture.style, :inline))

    counter =
      CounterState.apply(
        %{display: :block, counter_set: [{"footnote", number}]},
        CounterState.new(),
        CounterState.new(),
        id
      )

    marker_nodes =
      publishing_result(
        content_nodes(
          Map.get(marker_style, :content, [{:counter, "footnote"}, {:string, ". "}]),
          marker_style,
          registry,
          {Map.get(node, :attributes, %{}), counter, state, source_page},
          nil
        )
      )

    %{capture | children: marker_nodes ++ capture.children}
  end

  defp note_policy(kind, node, id, target, capacity, height, full, line, registry, state) do
    policy = Map.get(node.style, :footnote_policy, "auto")

    if kind == :footnote and policy != "auto" and height > capacity and
         height <= full - line - 6.0 do
      owner = if policy == "block", do: note_block_owner(node, id, registry), else: id
      minimum = target + 1

      {minimum, full - line - 6.0,
       %{state | minimum_pages: Map.put(state.minimum_pages, owner, minimum)}}
    else
      {target, capacity, state}
    end
  end

  defp note_block_owner(node, fallback, registry) do
    case Map.get(registry.nodes, Map.get(node, :parent_id)) do
      parent when parent == nil or :erlang.map_get(:tag, parent) in ["body", "html"] ->
        fallback

      parent ->
        if get_in(parent, [:style, :display]) in [:block, :list_item, :table_cell],
          do: parent.node_id,
          else: note_block_owner(parent, fallback, registry)
    end
  end

  defp continuation_pages(pages, extra) do
    needed = extra |> Map.keys() |> Enum.max(fn -> length(pages) end)

    if needed <= length(pages) do
      pages
    else
      last = List.last(pages)

      pages ++
        Enum.map((length(pages) + 1)..needed, fn _ -> %{last | boxes: [], blank: false} end)
    end
  end

  defp event_top(boxes, id) do
    Enum.find_value(boxes, fn box ->
      if id in Map.get(box, :publishing_targets, []),
        do: elem(PageGeometry.box_vertical_bounds(box), 0),
        else: event_top(Map.get(box, :boxes, []), id)
    end)
  end

  defp body_free_space(page) do
    boxes =
      Enum.reject(
        page.boxes,
        &(Map.get(&1, :_stacking_root_background, false) or Map.get(&1, :out_of_flow, false))
      )

    {_top, bottom} = bounds(boxes, elem(page.size, 1) - page.margins.top)
    max(bottom - page.margins.bottom, 0.0)
  end

  defp float_destination(number, required, full, line, reserves) do
    HtmlValidator.check_render_resource(:max_pages, number, :pagination)
    reserve = Map.get(reserves, number, %{top: 0.0, bottom: 0.0})

    if required + line <= full and reserve.top + reserve.bottom + required + line > full,
      do: float_destination(number + 1, required, full, line, reserves),
      else: number
  end

  defp place_allocations(fragments, start, side, template, state, kind, line, offset, placement) do
    Enum.reduce_while(fragments, {:ok, state, start - 1}, fn fragment, {:ok, state, last} ->
      number = last + 1
      HtmlValidator.check_render_resource(:max_pages, number, :pagination)
      {top, bottom} = bounds(fragment.boxes, elem(fragment.size, 1))
      height = max(top - bottom, 0.0)
      full = elem(template.size, 1) - template.margins.top - template.margins.bottom

      offset =
        case offset do
          {:percent, ratio} -> (full - height) * ratio
          value -> value
        end

      gap = if(kind == :footnote, do: 6.0, else: 0.0) + offset
      required = height + gap

      number =
        if kind == :float,
          do: float_destination(number, required, full, line, state.reserves),
          else: number

      reserve = Map.get(state.reserves, number, %{top: 0.0, bottom: 0.0})

      with :ok <- check_allocation_geometry(number, template, placement),
           true <- reserve.top + reserve.bottom + required + line <= full do
        target =
          if side == :bottom,
            do: template.margins.bottom + offset,
            else: elem(template.size, 1) - template.margins.top - reserve.top - height - offset

        paint =
          DrawingGeometry.translate_boxes(fragment.boxes, template.margins.left, target - bottom)

        separator =
          if kind == :footnote and reserve.bottom == 0.0,
            do: [
              %{
                type: :rect,
                x: template.margins.left,
                y: target + height + gap / 2,
                width:
                  (elem(template.size, 0) - template.margins.left - template.margins.right) / 3,
                height: 0.5,
                fill_color: {0, 0, 0},
                stroke_color: nil,
                stroke_width: 0.0,
                border_radius: 0.0
              }
            ],
            else: []

        existing_paint = Map.get(state.extra, number, [])

        existing_paint =
          if side == :bottom,
            do:
              Enum.map(existing_paint, fn box ->
                if Map.get(box, :publishing_side) == :bottom,
                  do: DrawingGeometry.translate_box(box, 0.0, required),
                  else: box
              end),
            else: existing_paint

        added = Enum.map(separator ++ paint, &Map.put(&1, :publishing_side, side))

        {:cont,
         {:ok,
          %{
            state
            | reserves:
                Map.put(state.reserves, number, Map.update!(reserve, side, &(&1 + required))),
              extra: Map.put(state.extra, number, existing_paint ++ added)
          }, number}}
      else
        false ->
          {:halt,
           CssPublishingValidator.failure(
             "footnotes and page floats leave no room for their source calls; reduce kept content or note size",
             number
           )}

        error ->
          {:halt, error}
      end
    end)
  end

  defp check_allocation_geometry(number, template, {pages, registry}) do
    case registry do
      %{page_layout: {base, explicit}} ->
        count = length(pages)
        sheet = Enum.at(pages, min(number, count) - 1)

        selection =
          CssPageValidator.select(
            registry.page_rules,
            sheet.page_name,
            number,
            number <= count and sheet.blank,
            Map.get(base, :direction, :ltr),
            group_positions(Map.get(sheet, :page_groups, %{}), max(number - count, 0))
          )

        with {:ok, destination} <-
               blank_page_geometry(sheet, selection, registry.options, explicit, base) do
          if destination.size == template.size and
               PageGeometry.snap_css_pixel_margins(destination.margins) == template.margins do
            :ok
          else
            CssPublishingValidator.failure(
              "a footnote or page float cannot continue across different page geometry; use the same page size and margins for its destinations",
              %{
                page: number,
                from: {template.size, template.margins},
                to: {destination.size, destination.margins}
              }
            )
          end
        end

      _ ->
        :ok
    end
  end

  defp note_counter_values(registry, state, sheets) do
    {values, _} =
      Enum.map_reduce(registry.notes, %{}, fn {id, number, node}, pages ->
        page = Map.get(state.locations, id, 1)
        sheet = Enum.at(sheets, page - 1)

        selected =
          CssPageValidator.select(
            registry.page_rules,
            sheet.page_name,
            page,
            Map.get(sheet, :blank, false),
            Map.get(node.style, :direction, :ltr),
            Map.get(sheet, :page_groups, %{})
          )

        style =
          publishing_result(
            apply_declarations(
              Style.anonymous_box_style(node.style, :block),
              Enum.filter(selected.declarations, fn {p, _} ->
                p in ~w(counter-reset counter-set)
              end)
            )
          )

        reset =
          List.keyfind(Map.get(style, :counter_reset, []), "footnote", 0) ||
            List.keyfind(Map.get(style, :counter_set, []), "footnote", 0)

        case reset do
          {"footnote", start} when is_integer(start) ->
            increment =
              case List.keyfind(Map.get(node.style, :counter_increment, []), "footnote", 0) do
                nil -> 1
                {_, n} -> n
              end

            number = Map.get(pages, page, start) + increment
            {{id, number}, Map.put(pages, page, number)}

          _ ->
            {{id, number}, pages}
        end
      end)

    Map.new(values)
  end

  defp page_counter_values(pages, tree) do
    base = first_style(tree)

    {values, _} =
      pages
      |> Enum.with_index(1)
      |> Enum.map_reduce(CounterState.new(), fn {page, n}, counters ->
        selected =
          CssPageValidator.select(
            tree.css_page_rules,
            page.page_name,
            n,
            Map.get(page, :blank, false),
            Map.get(base, :direction, :ltr),
            Map.get(page, :page_groups, %{})
          )

        declarations =
          Enum.filter(selected.declarations, fn {p, _} ->
            p in ~w(counter-reset counter-increment counter-set)
          end)

        style =
          publishing_result(
            apply_declarations(Style.anonymous_box_style(base, :block), declarations)
          )

        counters = advance_page_counters(style, counters, length(pages))
        {{n, CounterState.value(counters, "page")}, counters}
      end)

    Map.new(values)
  end

  defp advance_page_counters(style, counters, total) do
    increments = Map.get(style, :counter_increment, [])

    increments =
      if List.keymember?(increments, "page", 0), do: increments, else: increments ++ [{"page", 1}]

    counters =
      CounterState.apply(
        Map.put(style, :counter_increment, increments),
        counters,
        counters,
        :page
      )

    CounterState.apply(
      %{display: :block, counter_set: [{"pages", total}]},
      counters,
      counters,
      :page
    )
  end

  defp add_margin_boxes(pages, tree, registry, state, opts, explicit) do
    base = first_style(tree)

    pages
    |> Enum.with_index(1)
    |> Enum.reduce_while({:ok, [], CounterState.new()}, fn {page, number},
                                                           {:ok, pages, page_counters} ->
      selected =
        CssPageValidator.select(
          Map.get(tree, :css_page_rules, []),
          page.page_name,
          number,
          Map.get(page, :blank, false),
          Map.get(base, :direction, :ltr),
          Map.get(page, :page_groups, %{})
        )

      typography =
        Enum.filter(selected.declarations, fn {property, _} ->
          property not in ~w(size margin margin-top margin-right margin-bottom margin-left page-orientation marks bleed counter-reset counter-increment)
        end)

      counter_declarations =
        Enum.filter(selected.declarations, fn {property, _} ->
          property in ~w(counter-reset counter-increment counter-set)
        end)

      with {:ok, page} <-
             if(page.blank,
               do: blank_page_geometry(page, selected, opts, explicit, base),
               else: {:ok, page}
             ),
           {:ok, page_style} <-
             apply_declarations(Style.anonymous_box_style(base, :block), typography),
           {:ok, page_style} <-
             NativeElixirPdfUtilities.Validators.CssLengthValidator.resolve_edges(
               page_style,
               elem(page.size, 0)
             ),
           {:ok, page_style} <-
             Style.prepare_publishing_paint(
               page_style,
               registry.options,
               selected.resource_context
             ),
           {:ok, page_style} <- apply_declarations(page_style, counter_declarations),
           page_counters = advance_page_counters(page_style, page_counters, state.total),
           page_style = Map.put(page_style, :page_counters, page_counters),
           {:ok, margin_paints} <-
             margin_paints(
               page,
               selected.margin_boxes,
               page_style,
               Map.put(registry, :margin_contexts, selected.margin_contexts),
               Map.put(state, :page_counter_scope, true),
               number
             ),
           {:ok, decoration} <- page_decoration(page, selected, page_style),
           page = %{
             page
             | boxes:
                 decoration ++ page.boxes ++ Map.get(state.extra, number, []) ++ margin_paints
           },
           {:ok, page} <- page_metadata(page, selected, page_style, registry, state, number) do
        {:cont, {:ok, pages ++ [page], page_counters}}
      else
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, pages, _counters} -> {:ok, pages}
      error -> error
    end
  end

  defp first_style(node) do
    Map.get(node, :style) || Enum.find_value(Map.get(node, :children, []), &first_style/1) ||
      Map.get(node, :publishing_base_style)
  end

  defp blank_page_geometry(page, selected, opts, explicit, base) do
    with {:ok, options, page_margins} <- page_options(selected, opts, explicit, base),
         {:ok, size} <- PageGeometry.normalize_page_size(Keyword.fetch!(options, :page_size)),
         {:ok, margins} <- PageGeometry.normalize_margins(Keyword.fetch!(options, :margin)),
         :ok <- HtmlValidator.validate_printable_area(size, margins) do
      {:ok, %{page | size: size, margins: margins, page_box_margins: page_margins}}
    end
  end

  defp apply_declarations(style, declarations) do
    budget = HtmlValidator.new_css_budget()

    prepared =
      Enum.reduce_while(declarations, {:ok, style}, fn declaration, {:ok, style} ->
        case CssValueValidator.validate_declaration(style, declaration, budget) do
          {:ok, style} ->
            {:cont, {:ok, style}}

          {:error, _} ->
            {:halt,
             NativeElixirPdfUtilities.Diagnostics.error(
               :css,
               :invalid_css,
               "page or margin-box declaration is invalid",
               source: inspect(declaration)
             )}
        end
      end)

    with {:ok, style} <- prepared,
         {:ok, families, face} <-
           Font.resolve(
             style.font_families,
             style.font_weight,
             style.font_style,
             style._font_registry
           ) do
      {:ok,
       Map.merge(style, %{font_families: families, font_face: face, font_family: face.family})}
    end
  end

  defp margin_paints(page, definitions, base, registry, state, number) do
    prepared =
      Enum.reduce_while(CssPageValidator.margin_boxes(), {:ok, %{}}, fn name, {:ok, prepared} ->
        case Map.fetch(definitions, name) do
          :error ->
            {:cont, {:ok, prepared}}

          {:ok, declarations} ->
            align =
              cond do
                String.starts_with?(name, ["left-", "right-"]) or String.contains?(name, "center") ->
                  :center

                String.ends_with?(name, "right") ->
                  :right

                true ->
                  :left
              end

            vertical =
              cond do
                String.ends_with?(name, "top") -> :top
                String.ends_with?(name, "bottom") -> :bottom
                true -> :middle
              end

            initial =
              Style.anonymous_box_style(base, :block)
              |> Map.merge(%{
                text_align: align,
                vertical_align: vertical,
                page_counters: base.page_counters,
                _page_margin_box: true
              })

            with {:ok, style} <- apply_declarations(initial, declarations),
                 content when content != :none <- Map.get(style, :content, :none),
                 {:ok, style} <-
                   Style.prepare_publishing_paint(
                     style,
                     registry.options,
                     get_in(registry.margin_contexts, [name, :background])
                   ),
                 {:ok, nodes} <-
                   margin_nodes(
                     content,
                     style,
                     registry,
                     state,
                     number,
                     get_in(registry.margin_contexts, [name, :content])
                   ) do
              {:cont, {:ok, Map.put(prepared, name, %{style: style, nodes: nodes})}}
            else
              :none -> {:cont, {:ok, prepared}}
              error -> {:halt, error}
            end
        end
      end)

    with {:ok, prepared} <- prepared do
      rectangles = margin_rectangles(page, prepared)

      Enum.reduce_while(CssPageValidator.margin_boxes(), {:ok, []}, fn name, {:ok, paints} ->
        case Map.fetch(prepared, name) do
          :error ->
            {:cont, {:ok, paints}}

          {:ok, %{style: style, nodes: nodes}} ->
            {x, y, width, height} = Map.fetch!(rectangles, name)

            with {:ok, boxes} <- surface(nodes, width, height),
                 boxes = align_surface(boxes, style, x, y, width, height) do
              {:cont, {:ok, paints ++ boxes}}
            else
              error -> {:halt, error}
            end
        end
      end)
    end
  end

  defp image_extents(nodes) do
    Enum.reduce(nodes, {0.0, 0.0}, fn node, {width, height} ->
      {w, h} =
        case node do
          %{style: %{display: :image, image: image}} -> {image.width, image.height}
          %{children: children} -> image_extents(children)
          _ -> {0.0, 0.0}
        end

      {width + w, max(height, h)}
    end)
  end

  defp margin_rectangles(page, prepared) do
    initial = Map.new(CssPageValidator.margin_boxes(), &{&1, margin_rectangle(&1, page)})

    Enum.reduce(
      [
        {"top", ~w(left center right), :width},
        {"bottom", ~w(left center right), :width},
        {"left", ~w(top middle bottom), :height},
        {"right", ~w(top middle bottom), :height}
      ],
      initial,
      fn {side, slots, axis}, rectangles ->
        names = Enum.map(slots, &(side <> "-" <> &1))
        {x, y, width, height} = Map.fetch!(rectangles, hd(names))
        available = if axis == :width, do: width, else: height

        metrics =
          Enum.map(names, fn name ->
            case Map.fetch(prepared, name) do
              :error ->
                %{generated: false, min: 0.0, max: 0.0, fixed: 0.0}

              {:ok, %{style: style, nodes: nodes}} ->
                content = text(nodes)

                metric = fn text ->
                  Font.css_text_width(
                    text,
                    style.font_face,
                    style.font_size,
                    Map.get(style, :letter_spacing, 0.0)
                  )
                end

                {image_width, image_height} = image_extents(nodes)

                maximum =
                  if axis == :width,
                    do: metric.(content) + image_width,
                    else: max(style.line_height, image_height)

                minimum =
                  if axis == :width,
                    do:
                      max(
                        content |> String.split() |> Enum.map(metric) |> Enum.max(fn -> 0.0 end),
                        image_width
                      ),
                    else: max(style.line_height, image_height)

                fixed =
                  case Map.get(style, axis) do
                    n when is_number(n) -> n
                    _ -> nil
                  end

                minimum_constraint =
                  case Map.get(style, if(axis == :width, do: :min_width, else: :min_height)) do
                    n when is_number(n) -> n
                    _ -> 0.0
                  end

                maximum_constraint =
                  case Map.get(style, if(axis == :width, do: :max_width, else: :max_height)) do
                    n when is_number(n) -> n
                    _ -> :infinity
                  end

                %{
                  generated: Map.get(style, :content, :none) != :none,
                  min: minimum,
                  max: maximum,
                  fixed: fixed,
                  minimum: minimum_constraint,
                  maximum: maximum_constraint
                }
            end
          end)

        sizes = PageGeometry.margin_axis(metrics, available)

        names
        |> Enum.zip(sizes)
        |> Enum.with_index()
        |> Enum.reduce(rectangles, fn {{name, size}, index}, rectangles ->
          offset =
            case index do
              0 -> 0.0
              1 -> (available - size) / 2
              2 -> available - size
            end

          rectangle =
            if axis == :width,
              do: {x + offset, y, size, height},
              else: {x, y + available - offset - size, width, size}

          Map.put(rectangles, name, rectangle)
        end)
      end
    )
  end

  defp margin_nodes(parts, style, registry, state, page, resource) do
    case Enum.find(parts, &(elem(&1, 0) == :element)) do
      {:element, name, selection} ->
        entries =
          Enum.filter(registry.running, fn {id, n, _} ->
            n == name and Map.get(state.locations, id, 0) in 1..page
          end)

        same = Enum.filter(entries, fn {id, _, _} -> Map.get(state.locations, id) == page end)
        previous = Enum.reject(entries, fn {id, _, _} -> Map.get(state.locations, id) == page end)

        choice =
          choose_running(same, previous, selection, fn {id, _, _} ->
            MapSet.member?(state.starts, id)
          end)

        if choice == nil do
          {:ok, []}
        else
          with {:ok, node} <-
                 choice |> elem(2) |> clean_capture() |> resolve_tree(registry, state, page),
               do: {:ok, [%{type: :element, tag: "div", style: style, children: [node]}]}
        end

      _ ->
        counters =
          CounterState.apply(
            style,
            Map.get(style, :page_counters, CounterState.new()),
            Map.get(style, :page_counters, CounterState.new()),
            {:margin, make_ref()}
          )

        with {:ok, children} <-
               content_nodes(
                 parts,
                 style,
                 registry,
                 {%{}, counters, state, page},
                 resource
               ),
             do: {:ok, [%{type: :element, tag: "div", style: style, children: children}]}
    end
  end

  defp content_nodes(
         parts,
         style,
         registry,
         {attrs, counters, state, page},
         resource,
         note \\ nil
       ) do
    parts = if parts == :none, do: [], else: parts
    {alternatives, visual} = Enum.split_with(parts, &match?({:alternative, _}, &1))

    # Each image is a separate owner. Adjacent text still shares one shaped run.
    groups = Enum.chunk_by(visual, &if(match?({:image, _}, &1), do: make_ref(), else: :text))

    result =
      Enum.reduce_while(groups, {:ok, []}, fn group, {:ok, nodes} ->
        child =
          case group do
            [{:image, source}] ->
              Style.prepare_generated_image(source, style, registry.options, resource)

            chunks when note != nil ->
              {:ok,
               %{
                 type: :text,
                 text: "",
                 style: style,
                 publishing_text_id: make_ref(),
                 publishing_content: %{
                   parts: chunks,
                   attributes: attrs,
                   counters: counters,
                   note_id: note
                 }
               }}

            chunks ->
              with {:ok, value} <- content_text(chunks, attrs, counters, registry, state, page),
                   do:
                     {:ok, %{type: :text, text: publishing_text(value, style, []), style: style}}
          end

        case child do
          {:ok, child} -> {:cont, {:ok, [child | nodes]}}
          error -> {:halt, error}
        end
      end)

    with {:ok, nodes} <- result do
      nodes = Enum.reverse(nodes)

      case alternatives do
        [] ->
          {:ok, nodes}

        [{:alternative, parts}] ->
          with {:ok, text} <- content_text(parts, attrs, counters, registry, state, page),
               do: {:ok, Style.mark_generated_alternative(nodes, text)}
      end
    end
  end

  defp clean_capture(node) do
    node
    |> Map.replace_lazy(
      :style,
      &Map.drop(&1, [
        :running_name,
        :running_copy,
        :publishing_anchor,
        :publishing_float,
        :html_target
      ])
    )
    |> Map.replace_lazy(:children, &Enum.map(&1, fn child -> clean_capture(child) end))
  end

  defp surface(nodes, width, height) do
    if nodes == [] do
      {:ok, []}
    else
      wrapper = %{
        type: :element,
        tag: "div",
        style: Style.anonymous_box_style(first_style(hd(nodes)), :block),
        children: nodes
      }

      document = %{type: :document, children: [wrapper]}

      with {:ok, document} <- FontFallback.resolve(document),
           {:ok, measured} <-
             Font.with_measurement_cache(fn ->
               Layout.layout_viewport(document, width, height)
             end),
           {:ok, measured} <- Layout.resolve_images(measured) do
        {:ok, measured.boxes}
      end
    end
  end

  defp align_surface(boxes, style, x, y, _width, height) do
    {_top, bottom} = bounds(boxes, height)

    offset =
      case Map.get(style, :vertical_align, :middle) do
        :top -> 0.0
        :bottom -> bottom
        _ -> bottom / 2
      end

    DrawingGeometry.translate_boxes(boxes, x, y - offset)
    |> Enum.map(&Map.drop(&1, [:outline_anchor, :publishing_targets]))
  end

  defp bounds(boxes, height) do
    if boxes == [],
      do: {height, height},
      else:
        (
          values = Enum.map(boxes, &PageGeometry.box_vertical_bounds/1)

          {values |> Enum.map(&elem(&1, 0)) |> Enum.max(),
           values |> Enum.map(&elem(&1, 1)) |> Enum.min()}
        )
  end

  defp margin_rectangle(name, page) do
    {width, height} = page.size
    m = Map.get(page, :page_box_margins, page.margins)
    content_width = width - m.left - m.right
    content_height = height - m.top - m.bottom

    cond do
      name == "top-left-corner" ->
        {0.0, height - m.top, m.left, m.top}

      name == "top-right-corner" ->
        {width - m.right, height - m.top, m.right, m.top}

      name == "bottom-left-corner" ->
        {0.0, 0.0, m.left, m.bottom}

      name == "bottom-right-corner" ->
        {width - m.right, 0.0, m.right, m.bottom}

      String.starts_with?(name, ["top-", "bottom-"]) ->
        y = if String.starts_with?(name, "top-"), do: height - m.top, else: 0.0
        h = if String.starts_with?(name, "top-"), do: m.top, else: m.bottom
        {m.left, y, content_width, h}

      true ->
        x = if String.starts_with?(name, "left-"), do: 0.0, else: width - m.right
        w = if x == 0.0, do: m.left, else: m.right
        {x, m.bottom, w, content_height}
    end
  end

  defp page_decoration(page, _selected, style) do
    {width, height} = page.size
    background = Map.get(style, :background_color)

    background_paint =
      if background == nil,
        do: [],
        else: [
          %{
            type: :rect,
            x: 0.0,
            y: 0.0,
            width: width,
            height: height,
            fill_color: background,
            stroke_color: nil,
            stroke_width: 0.0,
            border_radius: 0.0
          }
        ]

    margins = Map.get(page, :page_box_margins, page.margins)
    w = width - margins.left - margins.right
    h = height - margins.top - margins.bottom

    border_style =
      style
      |> Map.drop([:background_image, :background_images, :background_image_source])
      |> Map.put(:width, w)
      |> Map.put(:height, h)
      |> Map.put(:box_sizing, :border_box)
      |> Map.put(:background_color, nil)

    node = %{type: :element, tag: "div", style: border_style, children: []}

    image_style =
      style
      |> Map.merge(%{
        width: width,
        height: height,
        box_sizing: :border_box,
        margin: CssValueValidator.edges(0.0),
        padding: CssValueValidator.edges(0.0),
        border_widths: CssValueValidator.edges(0.0),
        background_color: nil
      })

    with {:ok, background_images} <- surface([%{node | style: image_style}], width, height),
         {:ok, paint} <- surface([node], w, h) do
      {:ok,
       background_paint ++
         background_images ++ DrawingGeometry.translate_boxes(paint, margins.left, margins.bottom)}
    end
  end

  defp bookmark_boxes(boxes, registry, state, number) do
    Enum.map(boxes, fn box ->
      anchors =
        Enum.flat_map(Map.get(box, :publishing_targets, []), fn id ->
          node = Map.get(registry.nodes, id)
          style = if node == nil, do: %{}, else: Map.get(node, :style, %{})

          case Map.get(style, :bookmark_level) do
            nil ->
              []

            level ->
              parts = Map.get(style, :bookmark_label, [{:content, "text"}])

              title =
                publishing_result(
                  content_text(
                    parts,
                    Map.put(
                      Map.get(node, :attributes, %{}),
                      "text",
                      selected_text(node, "content")
                    ),
                    Map.get(node, :publishing_counters, CounterState.new()),
                    registry,
                    state,
                    number
                  )
                )

              [
                %{
                  title: title,
                  level: level,
                  open: Map.get(style, :bookmark_state, "open") == "open",
                  id: id
                }
              ]
          end
        end)

      box =
        if Map.has_key?(box, :boxes),
          do: Map.update!(box, :boxes, &bookmark_boxes(&1, registry, state, number)),
          else: box

      if anchors == [], do: box, else: Map.put(box, :css_bookmark_anchors, anchors)
    end)
  end

  defp publishing_result(result) do
    case result do
      {:ok, value} -> value
      error -> throw({:render_resource_limit, error})
    end
  end

  defp page_metadata(page, selected, style, registry, state, number) do
    page = Map.put(page, :boxes, bookmark_boxes(page.boxes, registry, state, number))

    descriptors = Map.new(selected.declarations)

    marks =
      descriptors
      |> Map.get("marks", "none")
      |> String.downcase()
      |> String.split()
      |> Enum.filter(&(&1 in ["crop", "cross"]))

    bleed =
      publishing_result(
        CssPageValidator.resolve_bleed(Map.get(descriptors, "bleed", "auto"), marks, style)
      )

    extent = max(bleed, if(marks == [], do: 0.0, else: 18.0))
    {width, height} = page.size

    page =
      if extent == 0 do
        page
      else
        paint = DrawingGeometry.translate_boxes(page.boxes, extent, extent)
        cuts = if "crop" in marks, do: crop_marks(width, height, extent), else: []
        crosses = if "cross" in marks, do: cross_marks(width, height, extent), else: []

        page
        |> Map.put(:size, {width + extent * 2, height + extent * 2})
        |> Map.put(:trim_box, {extent, extent, width + extent, height + extent})
        |> Map.put(
          :bleed_box,
          {extent - bleed, extent - bleed, width + extent + bleed, height + extent + bleed}
        )
        |> Map.put(:boxes, paint ++ cuts ++ crosses)
      end

    orientation =
      descriptors |> Map.get("page-orientation", "upright") |> String.trim() |> String.downcase()

    case orientation do
      value when value not in ["rotate-left", "rotate-right"] ->
        {:ok, page}

      orientation ->
        {width, height} = page.size

        matrix =
          if orientation == "rotate-left",
            do: {0.0, 1.0, -1.0, 0.0, height, 0.0},
            else: {0.0, -1.0, 1.0, 0.0, 0.0, width}

        group = %{
          type: :drawing_group,
          x: 0.0,
          y: 0.0,
          width: width,
          height: height,
          transform: matrix,
          boxes: page.boxes
        }

        page = %{page | size: {height, width}, boxes: [group]}

        page =
          Enum.reduce([:trim_box, :bleed_box], page, fn key, page ->
            case Map.fetch(page, key) do
              :error ->
                page

              {:ok, {l, b, r, t}} ->
                rectangle =
                  if orientation == "rotate-left",
                    do: {height - t, l, height - b, r},
                    else: {b, width - r, t, width - l}

                Map.put(page, key, rectangle)
            end
          end)

        m = page.margins

        margins =
          if orientation == "rotate-left",
            do: %{top: m.right, right: m.bottom, bottom: m.left, left: m.top},
            else: %{top: m.left, right: m.top, bottom: m.right, left: m.bottom}

        {:ok, %{page | margins: margins}}
    end
  end

  defp crop_marks(width, height, e) do
    for {x, y, w, h} <- [
          {e - 12, e, 6.0, 0.25},
          {e, e - 12, 0.25, 6.0},
          {e + width + 6, e, 6.0, 0.25},
          {e + width, e - 12, 0.25, 6.0},
          {e - 12, e + height, 6.0, 0.25},
          {e, e + height + 6, 0.25, 6.0},
          {e + width + 6, e + height, 6.0, 0.25},
          {e + width, e + height + 6, 0.25, 6.0}
        ],
        do: %{
          type: :rect,
          x: x,
          y: y,
          width: w,
          height: h,
          fill_color: {0, 0, 0},
          stroke_color: nil,
          stroke_width: 0.0,
          border_radius: 0.0
        }
  end

  defp cross_marks(width, height, e) do
    for {x, y} <- [
          {e / 2, e + height / 2},
          {e + width + e / 2, e + height / 2},
          {e + width / 2, e / 2},
          {e + width / 2, e + height + e / 2}
        ],
        {dx, dy, w, h} <- [{-3.0, 0.0, 6.0, 0.25}, {0.0, -3.0, 0.25, 6.0}],
        do: %{
          type: :rect,
          x: x + dx,
          y: y + dy,
          width: w,
          height: h,
          fill_color: {0, 0, 0},
          stroke_color: nil,
          stroke_width: 0.0,
          border_radius: 0.0
        }
  end
end
