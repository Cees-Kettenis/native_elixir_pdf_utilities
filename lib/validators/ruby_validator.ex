defmodule NativeElixirPdfUtilities.Validators.RubyValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
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
              if Enum.all?(children, fn child ->
                   match?(
                     %{type: :text, text: text, style: child_style}
                     when is_binary(text) and is_map(child_style),
                     child
                   ) or
                     match?(
                       %{type: :element, style: child_style, children: nested}
                       when is_map(child_style) and is_list(nested),
                       child
                     )
                 end) do
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

                {groups, _} =
                  Enum.map_reduce(groups, "", fn group, previous ->
                    text = base_text(group.base)

                    break_before =
                      previous == "" or Regex.match?(~r/[ \t\n\r\f]$/u, previous) or
                        Regex.match?(~r/^[ \t\n\r\f]|^[\p{Han}\p{Hiragana}\p{Katakana}]/u, text) or
                        Map.get(style, :line_break) == :anywhere

                    {Map.put(group, :break_before, break_before), text}
                  end)

                {:ok, groups}
              else
                failure("ruby requires styled text or element children")
              end
            end
        end

      _ ->
        failure("ruby measurement requires a styled ruby element with children")
    end
  catch
    {:render_resource_limit, error} -> error
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
    base_width > 0 and
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
        %{type: :text, text: text} -> text
        %{children: children} -> base_text(children)
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
