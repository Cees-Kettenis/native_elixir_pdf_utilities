defmodule NativeElixirPdfUtilities.Validators.RubyValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{FontValidator, HtmlValidator, HtmlLineBreakValidator}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, ResourceContext}

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
                Enum.reduce(children, {[], []}, fn child, {groups, pending} ->
                  HtmlValidator.reserve_render_resource(
                    :max_html_tree_work,
                    length(groups) + length(pending) + 1,
                    :layout
                  )

                  case child do
                    %{style: %{display: :none}} ->
                      {groups, pending}

                    %{tag: "rt", style: %{_ruby_annotation: true}} ->
                      cond do
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

              text = Enum.map_join(groups, &base_text(&1.base))

              with {:ok, breaks} <- HtmlLineBreakValidator.opportunities(text) do
                offsets = MapSet.new(breaks, &elem(&1, 0))

                {groups, _} =
                  Enum.map_reduce(groups, 0, fn group, start ->
                    base = base_text(group.base)
                    anywhere? = Map.get(style, :line_break) == :anywhere

                    {counts, _} =
                      base
                      |> String.graphemes()
                      |> Enum.with_index(1)
                      |> Enum.map_reduce(0, fn {grapheme, count}, offset ->
                        offset = offset + byte_size(grapheme)
                        {{count, offset}, offset}
                      end)

                    counts =
                      for {count, offset} <- counts,
                          offset < byte_size(base),
                          anywhere? or MapSet.member?(offsets, start + offset),
                          do: count

                    prepared =
                      group
                      |> Map.put(
                        :break_before,
                        start == 0 or anywhere? or MapSet.member?(offsets, start)
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
    base_width > 0 and Map.get(group, :break_counts, []) != [] and
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
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :layout)

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
            HtmlValidator.reserve_render_resource(:max_layout_text_work, byte_size(text), :layout)

            text =
              if shaped,
                do:
                  Font.shape_ligatures(
                    text,
                    node.style.font_face,
                    Map.get(node.style, :letter_spacing, 0)
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
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :layout)

      cond do
        remaining == 0 ->
          {before, after_nodes ++ [node], 0}

        node_length([node]) <= remaining ->
          {before ++ [node], after_nodes, remaining - node_length([node])}

        node.type == :text ->
          graphemes = String.graphemes(node.text)
          {left, right} = Enum.split(graphemes, remaining)

          {before ++ [%{node | text: Enum.join(left)}],
           after_nodes ++ [%{node | text: Enum.join(right)}], 0}

        true ->
          {left, right, remaining} = partition_nodes(node.children, remaining)

          {before ++ [%{node | children: left}], after_nodes ++ [%{node | children: right}],
           remaining}
      end
    end)
  end

  defp base_text(children) do
    Enum.map_join(children, fn child ->
      HtmlValidator.reserve_render_resource(:max_html_tree_work, 1, :layout)

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
