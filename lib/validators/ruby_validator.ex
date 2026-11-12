defmodule NativeElixirPdfUtilities.Validators.RubyValidator do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator, HtmlLineBreakValidator}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, ResourceContext}

  @doc "Lists the ruby formatting declarations."
  @spec properties() :: [String.t()]
  def properties, do: ~w(ruby-align ruby-position ruby-merge ruby-overhang)

  @doc "Returns the selected CSS Ruby 1 initial values."
  @spec defaults() :: map()
  def defaults,
    do: %{
      ruby_align: :space_around,
      ruby_position: [:over],
      ruby_merge: :separate,
      ruby_overhang: :auto
    }

  @doc "Admits ruby placement, distribution, merging and overhang identifiers."
  @spec prepare_property(map(), String.t(), String.t()) :: {:ok, map()} | :error
  def prepare_property(style, property, value) do
    tokens = HtmlValidator.split_css_tokens(String.downcase(String.trim(value), :ascii))

    parsed =
      case {property, tokens} do
        {"ruby-align", [value]} when value in ~w(start center space-between space-around) ->
          {:ruby_align, value |> String.replace("-", "_") |> String.to_existing_atom()}

        {"ruby-position", [value]} when value in ~w(over under inter-character) ->
          {:ruby_position, [value |> String.replace("-", "_") |> String.to_existing_atom()]}

        {"ruby-position", ["alternate"]} ->
          {:ruby_position, [:alternate, :over]}

        {"ruby-position", ["alternate", value]} when value in ~w(over under) ->
          {:ruby_position, [:alternate, String.to_existing_atom(value)]}

        {"ruby-position", [value, "alternate"]} when value in ~w(over under) ->
          {:ruby_position, [:alternate, String.to_existing_atom(value)]}

        {"ruby-merge", ["collapse"]} ->
          # CSS Ruby 2014 spelling; the 2022 grammar calls this merge.
          {:ruby_merge, :merge}

        {"ruby-merge", [value]} when value in ~w(separate merge auto) ->
          {:ruby_merge, String.to_existing_atom(value)}

        {"ruby-overhang", [value]} when value in ~w(auto none) ->
          {:ruby_overhang, String.to_existing_atom(value)}

        _ ->
          nil
      end

    if parsed == nil, do: :error, else: {:ok, Map.put(style, elem(parsed, 0), elem(parsed, 1))}
  end

  @doc "Checks computed ruby controls through the same finite grammar as authored declarations."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    if :maps.get(:ruby_align, style, :space_around) in [
         :start,
         :center,
         :space_between,
         :space_around
       ] and
         Map.get(style, :ruby_position, [:over]) in [
           [:over],
           [:under],
           [:inter_character],
           [:alternate, :over],
           [:alternate, :under]
         ] and
         :maps.get(:ruby_merge, style, :separate) in [:separate, :merge, :auto] and
         :maps.get(:ruby_overhang, style, :auto) in [:auto, :none],
       do: :ok,
       else:
         Diagnostics.error(:layout, :invalid_layout, "recompute ruby formatting controls",
           module: __MODULE__,
           operation: :layout,
           source: source
         )
  end

  @doc "Forces inter-character annotation writing axes before descendant styles and font selection."
  @spec compute_style(map()) :: map()
  def compute_style(style) do
    ruby? =
      :maps.get(:_ruby_container, style, false) or :maps.get(:_ruby_base, style, false) or
        :maps.get(:_ruby_annotation, style, false) or
        :maps.get(:_display_role, style, nil) in [
          :ruby,
          :ruby_base,
          :ruby_text,
          :ruby_base_container,
          :ruby_text_container
        ]

    style =
      if ruby? do
        case :maps.get(:unicode_bidi, style, :normal) do
          value when value in [:normal, :embed] -> Map.put(style, :unicode_bidi, :isolate)
          :bidi_override -> Map.put(style, :unicode_bidi, :isolate_override)
          _ -> style
        end
      else
        style
      end

    parent = :maps.get(:_cascade_inherited, style, %{})

    if :maps.get(:_ruby_annotation, style, false) and
         Map.get(style, :ruby_position, [:over]) == [:inter_character] and
         :maps.get(:writing_mode, parent, :horizontal_tb) == :horizontal_tb do
      style |> Map.put(:writing_mode, :vertical_rl) |> Map.put(:_ruby_inter_character, true)
    else
      Map.delete(style, :_ruby_inter_character)
    end
  end

  @doc "Rejects caller-supplied ruby measurements, which are created only inside inline layout."
  @spec validate_layout_metadata(term()) :: :ok | {:error, {atom(), Diagnostics.diagnostic()}}
  def validate_layout_metadata(node) do
    case node do
      %{style: style} when is_map_key(style, :_ruby_box) or is_map_key(style, :_ruby_clearance) ->
        failure(
          "ruby measurements and clearance are internal inline state; pass ordinary styled ruby children"
        )

      %{tag: "rt", style: %{_ruby_annotation: true}} ->
        :ok

      %{style: style} when is_map_key(style, :_ruby_annotation) ->
        failure("ruby annotation role belongs only to styled rt elements")

      _ ->
        :ok
    end
  end

  @doc "Prepares horizontal ruby base groups and annotation rows from a styled ruby element."
  @spec prepare(term()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec prepare(term(), term()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def prepare(node, context \\ nil) do
    case node do
      %{type: :element, tag: "ruby", style: style, children: children}
      when is_map(style) and is_list(children) ->
        case context do
          nil ->
            ResourceContext.run([], HtmlValidator.new_image_budget(), &prepare(node, &1))

          _ ->
            with {:ok, _request} <- HtmlValidator.prepare_measured_child(node, context, %{}) do
              # The canonical measured-child validator already admitted the styled subtree.
              {groups, pending} =
                Enum.reduce(expand_container_columns(children), {[], []}, fn child,
                                                                             {groups, pending} ->
                  HtmlValidator.reserve_render_resource(
                    :max_work,
                    length(groups) + length(pending) + 1,
                    :layout
                  )

                  case child do
                    %{style: %{display: :none}} ->
                      {groups, pending}

                    %{tag: "rt", style: %{_ruby_annotation: true}} ->
                      cond do
                        :maps.get(:_ruby_same_base, child, false) and groups != [] ->
                          {List.update_at(
                             groups,
                             -1,
                             &%{
                               &1
                               | annotations:
                                   &1.annotations ++ [Map.delete(child, :_ruby_same_base)]
                             }
                           ), pending}

                        pending != [] ->
                          {groups ++ [%{base: pending, annotations: [child]}], []}

                        true ->
                          {groups ++ [%{base: [], annotations: [child]}], []}
                      end

                    _ ->
                      {groups, pending ++ [child]}
                  end
                end)

              groups =
                if pending == [],
                  do: groups,
                  else: groups ++ [%{base: pending, annotations: []}]

              groups =
                Enum.reduce(groups, [], fn group, merged ->
                  case merged do
                    [previous | rest] ->
                      collapse? =
                        length(previous.annotations) == length(group.annotations) and
                          group.annotations != [] and
                          Enum.all?(
                            previous.annotations ++ group.annotations,
                            &(Map.get(&1.style, :ruby_merge, :separate) == :merge and
                                Map.get(&1.style, :ruby_position, [:over]) != [:inter_character])
                          )

                      if collapse? do
                        annotations =
                          Enum.zip(previous.annotations, group.annotations)
                          |> Enum.map(fn {before, following} ->
                            %{
                              before
                              | children:
                                  isolate_nodes(before.children, before.style) ++
                                    isolate_nodes(following.children, following.style)
                            }
                          end)

                        [
                          %{
                            previous
                            | base:
                                isolate_nodes(previous.base, style) ++
                                  isolate_nodes(group.base, style),
                              annotations: annotations
                          }
                          | rest
                        ]
                      else
                        [group | merged]
                      end

                    [] ->
                      [group]
                  end
                end)
                |> Enum.reverse()

              text = Enum.map_join(groups, &base_text(&1.base))

              with {:ok, breaks} <- HtmlLineBreakValidator.opportunities(text) do
                offsets = MapSet.new(breaks, &elem(&1, 0))

                {groups, _} =
                  Enum.map_reduce(groups, 0, fn group, start ->
                    base = base_text(group.base)
                    anywhere? = :maps.get(:line_break, style, nil) == :anywhere

                    nowrap? =
                      :maps.get(:text_wrap_mode, style, :wrap) == :nowrap or
                        :maps.get(:white_space, style, :normal) in [:nowrap, :pre]

                    clusters =
                      NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator.cluster_boundaries(
                        %{text: base, style: style}
                      )
                      |> MapSet.new()

                    {counts, _} =
                      base
                      |> UnicodeGrapheme.graphemes()
                      |> Enum.with_index(1)
                      |> Enum.map_reduce(0, fn {grapheme, count}, offset ->
                        offset = offset + byte_size(grapheme)
                        {{count, offset}, offset}
                      end)

                    counts =
                      for {count, offset} <- counts,
                          not nowrap?,
                          offset < byte_size(base),
                          MapSet.member?(clusters, offset),
                          anywhere? or MapSet.member?(offsets, start + offset),
                          do: count

                    prepared =
                      group
                      |> Map.put(
                        :break_before,
                        start == 0 or
                          (not nowrap? and (anywhere? or MapSet.member?(offsets, start)))
                      )
                      |> Map.put(:break_counts, counts)

                    {prepared, start + byte_size(base)}
                  end)

                {:ok, groups}
              end
            end
        end

      _ ->
        failure("ruby measurement requires a styled ruby element with children")
    end
  catch
    {:render_resource_limit, error} -> error
  end

  defp isolate_nodes(nodes, style, context \\ nil) do
    context =
      context || %{id: make_ref(), mode: :isolate, direction: :maps.get(:direction, style, :ltr)}

    inherited = length(:maps.get(:_bidi_contexts, style, []))

    Enum.map(nodes, fn node ->
      HtmlValidator.reserve_render_resource(:max_work, 1, :layout)

      if :maps.get(:_ruby_isolated_unit, node, false) do
        node
      else
        current = Map.get(node, :style, style)
        {outer, inner} = Enum.split(:maps.get(:_bidi_contexts, current, []), inherited)
        current = Map.put(current, :_bidi_contexts, outer ++ [context] ++ inner)
        HtmlValidator.check_render_resource(:max_depth, length(current._bidi_contexts), :layout)
        node = node |> Map.put(:style, current) |> Map.put(:_ruby_isolated_unit, true)

        if Map.has_key?(node, :children),
          do: Map.update!(node, :children, &isolate_nodes(&1, style, context)),
          else: node
      end
    end)
  end

  # CSS Ruby 1, 31 December 2022, sections 2.1 and 2.3. Container
  # roles pair columns before the existing horizontal group measurement.
  # Legacy HTML rtc keeps its established fallback unless CSS assigns a role.
  defp expand_container_columns(children) do
    case children do
      [] ->
        []

      [%{style: %{_display_role: :ruby_base_container}} = base | rest] ->
        {rows, rest} =
          Enum.split_while(rest, &match?(%{style: %{_display_role: :ruby_text_container}}, &1))

        bases = base.children
        annotations = Enum.map(rows, & &1.children)
        count = Enum.max([length(bases) | Enum.map(annotations, &length/1)])

        HtmlValidator.reserve_render_resource(
          :max_work,
          count * max(length(annotations), 1),
          :layout
        )

        columns =
          if count == 0,
            do: [],
            else:
              Enum.flat_map(0..(count - 1), fn index ->
                base = Enum.at(bases, index)

                readings =
                  Enum.with_index(annotations)
                  |> Enum.flat_map(fn {row, level} ->
                    case Enum.at(row, index) do
                      nil ->
                        []

                      annotation ->
                        [
                          %{
                            annotation
                            | tag: "rt",
                              style: Map.put(annotation.style, :_ruby_annotation, true)
                          }
                          |> Map.put(:_ruby_same_base, level > 0)
                        ]
                    end
                  end)

                if(base, do: [base], else: []) ++ readings
              end)

        columns ++ expand_container_columns(rest)

      [%{style: %{_display_role: :ruby_text_container}} = row | rest] ->
        [
          %{row | tag: "rt", style: Map.put(row.style, :_ruby_annotation, true)}
          | expand_container_columns(rest)
        ]

      [child | rest] ->
        [child | expand_container_columns(rest)]
    end
  end

  @doc "Computes the selected font's normalized typographic em box for horizontal ruby placement."
  @spec em_metrics(map()) ::
          {:ok, %{ascent: number(), descent: number()}}
          | {:error, {atom(), Diagnostics.diagnostic()}}
  def em_metrics(style) do
    face = style.font_face

    with {:ok, metrics} <- FontValidator.prepare_text_metrics(face) do
      height = metrics.typographic_height || %{ascent: face.ascent, descent: -face.descent}
      pixels = style.font_size / 0.75
      total = height.ascent + height.descent

      # Blink normalizes the typographic ratio to one em in fixed CSS 1/64-pixel units,
      # then rounds each em edge upward and clamps it to the primary font height.
      # The 1/64 unit is a fixed layout representation, not a resource limit.
      ascent = if total > 0, do: round(height.ascent * pixels / total * 64) / 64, else: 0.0
      descent = round(pixels * 64) / 64 - ascent
      primary_ascent = round(face.ascent / face.units_per_em * pixels)
      primary_descent = round(-face.descent / face.units_per_em * pixels)

      {:ok,
       %{
         ascent: min(Float.ceil(ascent), primary_ascent) * 0.75,
         descent: min(Float.ceil(descent), primary_descent) * 0.75
       }}
    else
      :error -> failure("ruby requires the selected font's readable typographic metrics")
    end
  end

  @doc "Counts the graphemes and indivisible atomic children of a validated ruby base."
  @spec group_length(map()) :: non_neg_integer()
  def group_length(group) do
    node_length(group.base)
  end

  @doc "Determines whether a validated ruby column can be fragmented by the pinned horizontal policy."
  @spec splittable_group?(map(), number()) :: boolean()
  def splittable_group?(group, base_width) do
    # Blink's four/eight glyph boundary is a fixed composition rule, not a resource ceiling.
    base_width > 0 and :maps.get(:break_counts, group, []) != [] and
      Enum.all?(group.annotations, fn annotation ->
        not :maps.get(:_ruby_inter_character, annotation.style, false) and
          :maps.get(:white_space, annotation.style, :normal) not in [:nowrap, :pre]
      end) and
      (node_length(group.base, true) > 4 or
         Enum.any?(group.annotations, &(node_length(&1.children, true) > 8)))
  end

  @doc "Partitions a validated ruby group at measured base and annotation grapheme boundaries."
  @spec partition_group(map(), pos_integer(), [non_neg_integer()]) :: {map(), map()}
  def partition_group(group, count, reading_counts) do
    {before, after_nodes, _remaining} = partition_nodes(group.base, count)

    {annotations_before, annotations_after} =
      Enum.zip(group.annotations, reading_counts)
      |> Enum.map(fn {annotation, reading_count} ->
        {left, right, _remaining} = partition_nodes(annotation.children, reading_count)
        {%{annotation | children: left}, %{annotation | children: right}}
      end)
      |> Enum.unzip()

    {%{group | base: before, annotations: annotations_before},
     %{group | base: after_nodes, annotations: annotations_after}}
  end

  defp node_length(nodes, shaped \\ false) do
    Enum.reduce(nodes, 0, fn node, count ->
      HtmlValidator.reserve_render_resource(:max_work, 1, :layout)

      count +
        case node do
          %{style: %{display: :none}} ->
            0

          %{tag: "ruby"} ->
            if shaped, do: 0, else: 1

          %{style: %{display: display}}
          when display in [:inline_block, :inline_flex, :inline_grid, :image] ->
            if shaped, do: 0, else: 1

          %{type: :text, text: text} ->
            HtmlValidator.reserve_render_resource(:max_work, byte_size(text), :layout)

            text =
              if shaped,
                do:
                  Font.shape_ligatures(
                    text,
                    node.style.font_face,
                    :maps.get(:letter_spacing, node.style, 0)
                  ),
                else: text

            String.length(text)

          %{children: []} ->
            1

          %{children: children} ->
            node_length(children, shaped)
        end
    end)
  end

  defp partition_nodes(nodes, remaining) do
    Enum.reduce(nodes, {[], [], remaining}, fn node, {before, after_nodes, remaining} ->
      HtmlValidator.reserve_render_resource(:max_work, 1, :layout)

      cond do
        remaining == 0 ->
          {before, after_nodes ++ [node], 0}

        node_length([node]) <= remaining ->
          {before ++ [node], after_nodes, remaining - node_length([node])}

        node.type == :text ->
          graphemes = UnicodeGrapheme.graphemes(node.text)
          {left, right} = Enum.split(graphemes, remaining)

          left_text = Enum.join(left)
          right_text = Enum.join(right)

          {left_style, right_style} =
            case :maps.get(:_source_glyph_characters, node.style, nil) do
              characters when is_list(characters) ->
                {left_chars, right_chars} =
                  Enum.split(characters, length(String.codepoints(left_text)))

                {Map.put(node.style, :_source_glyph_characters, left_chars),
                 Map.put(node.style, :_source_glyph_characters, right_chars)}

              _ ->
                {node.style, node.style}
            end

          {before ++ [%{node | text: left_text, style: left_style}],
           after_nodes ++ [%{node | text: right_text, style: right_style}], 0}

        true ->
          {left, right, remaining} = partition_nodes(node.children, remaining)

          {before ++ [%{node | children: left}], after_nodes ++ [%{node | children: right}],
           remaining}
      end
    end)
  end

  defp base_text(children) do
    Enum.map_join(children, fn child ->
      HtmlValidator.reserve_render_resource(:max_work, 1, :layout)

      case child do
        %{style: %{display: :none}} ->
          ""

        %{tag: "ruby"} ->
          "\uFFFC"

        %{style: %{display: display}}
        when display in [:inline_block, :inline_flex, :inline_grid, :image] ->
          "\uFFFC"

        %{type: :text, text: text} ->
          text

        %{children: []} ->
          "\uFFFC"

        %{children: children} ->
          base_text(children)
      end
    end)
  end

  defp failure(message) do
    Diagnostics.error(:layout, :invalid_layout, message,
      operation: :measure_ruby,
      module: __MODULE__,
      source: "ruby"
    )
  end
end
