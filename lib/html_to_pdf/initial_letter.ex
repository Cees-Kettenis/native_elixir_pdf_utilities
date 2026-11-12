defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialLetter do
  @moduledoc false
  alias NativeElixirPdfUtilities.HtmlToPdf.{ClipGeometry, FontFallback}

  alias NativeElixirPdfUtilities.Validators.{
    CssInitialLetterValidator,
    CssTextSpacingValidator,
    CssWhiteSpaceValidator,
    HtmlValidator,
    NativeGlyphSourceValidator
  }

  @doc "Retains a non-inherited initial-letter declaration across its inline descendants."
  @spec mark_runs([map()], map()) :: [map()]
  def mark_runs(runs, style) do
    if Map.get(style, :initial_letter, :normal) == :normal do
      runs
    else
      owner = make_ref()
      Enum.map(runs, &Map.put(&1, :_initial_letter_owner, {owner, style}))
    end
  end

  @doc "Separates the first typographic unit after source ownership has been prepared."
  @spec prepare_runs([map()], map(), (map(), atom(), number() | nil, number() -> number())) ::
          {:ok, [map()]} | {:error, {atom(), map()}}
  def prepare_runs(runs, parent_style, resolve_size) do
    active? =
      Map.has_key?(parent_style, :_first_letter_style) or
        Enum.any?(runs, &Map.has_key?(&1, :_initial_letter_owner))

    if active? do
      runs =
        if Map.get(parent_style, :white_space, :normal) in [:normal, :nowrap] do
          runs
          |> CssWhiteSpaceValidator.trim_line_start()
          |> Enum.reject(
            &(Map.get(&1, :type) in [nil, :text] and &1.text == "" and
                not Map.get(&1, :hard_break, false))
          )
        else
          runs
        end

      {prefix, remaining} =
        Enum.split_while(
          runs,
          &(&1.text == "" and Map.get(&1, :type) == :inline_positioning_marker)
        )

      case remaining do
        [first | _] ->
          pseudo = Map.get(parent_style, :_first_letter_style)
          owner = Map.get(first, :_initial_letter_owner)

          cond do
            Map.get(first, :type) not in [nil, :text] or (pseudo == nil and owner == nil) ->
              {:ok, runs}

            true ->
              {selected, tail, style} =
                if owner != nil do
                  {_, style} = owner

                  {selected, tail} =
                    Enum.split_while(remaining, &(Map.get(&1, :_initial_letter_owner) == owner))

                  {selected, tail, style}
                else
                  text =
                    remaining
                    |> Enum.take_while(
                      &(Map.get(&1, :type) in [nil, :text] and not Map.get(&1, :hard_break, false))
                    )
                    |> Enum.map_join(& &1.text)

                  HtmlValidator.reserve_render_resource(:max_work, byte_size(text), :layout)
                  # CSS Pseudo 4 includes punctuation immediately around the first
                  # typographic unit. Slicing the admitted runs retains DOM owners.
                  selected_text = CssInitialLetterValidator.first_letter(text)

                  {selected, tail} = split_runs(remaining, byte_size(selected_text))
                  {selected, tail, pseudo}
                end

              selected =
                Enum.map(selected, fn run ->
                  selected_style =
                    if owner == nil,
                      do: Map.merge(run.style, Map.drop(style, [:link_url, :link_destination])),
                      else: Map.put_new(run.style, :_font_registry, style._font_registry)

                  run
                  |> Map.put(:type, :text)
                  |> Map.put(:style, selected_style)
                end)

              with {:ok, %{children: selected}} <-
                     FontFallback.resolve(%{type: :document, children: selected}) do
                result =
                  case Map.get(style, :initial_letter, :normal) do
                    :normal ->
                      {:ok, selected}

                    {size, sink} ->
                      following_space? =
                        case Enum.find(tail, &(&1.text != "")) do
                          nil -> false
                          run -> Regex.match?(~r/\A\p{Zs}/u, run.text)
                        end

                      with {:ok, run} <-
                             measure(
                               selected,
                               style,
                               parent_style,
                               size,
                               sink,
                               following_space?,
                               resolve_size
                             ),
                           do: {:ok, [run]}
                  end

                with {:ok, selected} <- result,
                     do:
                       {:ok,
                        Enum.map(
                          prefix ++ selected ++ tail,
                          &Map.put(&1, :_initial_letter_paragraph, true)
                        )}
              end
          end

        [] ->
          {:ok, runs}
      end
    else
      {:ok, runs}
    end
  end

  # A selection may span several owners or styles, including punctuation nodes.
  defp split_runs(runs, count) do
    {selected, tail, _} =
      Enum.reduce(runs, {[], [], count}, fn run, {selected, tail, remaining} ->
        amount = min(byte_size(run.text), remaining)

        selected =
          if amount == 0,
            do: selected,
            else: [
              NativeGlyphSourceValidator.slice_run(run, 0, binary_part(run.text, 0, amount))
              | selected
            ]

        tail =
          if remaining > 0 and amount == byte_size(run.text),
            do: tail,
            else: [
              NativeGlyphSourceValidator.slice_run(
                run,
                amount,
                binary_part(run.text, amount, byte_size(run.text) - amount)
              )
              | tail
            ]

        {selected, tail, remaining - amount}
      end)

    {Enum.reverse(selected), Enum.reverse(tail)}
  end

  defp measure(pieces, style, parent, size, sink, following_space?, resolve_size) do
    {mode, border?} = Map.get(style, :initial_letter_align, {:alphabetic, false})
    font = hd(pieces).style.font_face

    scripts =
      pieces
      |> Enum.map_join(& &1.text)
      |> String.to_charlist()
      |> Enum.map(&elem(NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData.properties(&1), 0))

    script = Enum.find(scripts, &(&1 not in ~w(Zyyy Zinh Zzzz))) || "Latn"

    initial_mode =
      cond do
        Enum.any?(scripts, &(&1 in ~w(Hani Hang Hira Kana Yiii))) -> :ideographic
        Enum.any?(scripts, &(&1 in ~w(Deva Beng Guru Tibt))) -> :hanging
        true -> :alphabetic
      end

    margin = Map.get(style, :margin, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})
    padding = Map.get(style, :padding, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})
    border = Map.get(style, :border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})
    vertical_edges = padding.top + padding.bottom + border.top + border.bottom

    with {:ok, {parent_over, parent_under}} <-
           CssInitialLetterValidator.alignment_metrics(
             parent.font_face,
             mode,
             parent.font_size,
             script
           ),
         {:ok, {over, under}} <-
           CssInitialLetterValidator.alignment_metrics(
             font,
             initial_mode,
             style.font_size,
             script
           ) do
      depth = HtmlValidator.text_baseline_depth(parent)
      parent_over = if mode == :leading, do: depth, else: parent_over * parent.font_size

      parent_under =
        if mode == :leading, do: depth - parent.line_height, else: parent_under * parent.font_size

      target = (size - 1) * parent.line_height + parent_over - parent_under
      target = max(target - if(border?, do: vertical_edges, else: 0.0), 0.0)
      font_size = Float.floor(target / (over - under) / 0.75 * 64) / 64 * 0.75
      HtmlValidator.check_render_resource(:max_numeric_magnitude, font_size, :layout)
      pieces = Enum.map(pieces, &put_in(&1.style.font_size, font_size))

      wrap = Map.get(style, :initial_letter_wrap, :none)

      geometry =
        case NativeElixirPdfUtilities.HtmlToPdf.TextInk.measure(pieces, wrap in [:first, :all]) do
          {:ok, geometry} -> geometry
          error -> throw({:render_resource_limit, error})
        end

      basis = if is_number(Map.get(parent, :width)), do: parent.width, else: nil
      width = resolve_size.(style, :width, basis, geometry.advance)
      box_height = resolve_size.(style, :height, nil, (over - under) * font_size) + vertical_edges
      ink_bottom = geometry.points |> Enum.map(&elem(&1, 1)) |> Enum.min(fn -> 0.0 end)

      outer_width =
        ceil(
          (width + margin.left + margin.right + padding.left + padding.right + border.left +
             border.right) / 0.75
        ) * 0.75

      baseline_shift =
        if size >= sink,
          do: -(sink - 1) * parent.line_height + parent_under - under * font_size,
          else: parent_over - over * font_size

      baseline_shift = baseline_shift + if(border?, do: padding.bottom + border.bottom, else: 0.0)

      raise =
        max(
          (size - min(size, sink)) * parent.line_height + margin.top + padding.top + border.top,
          0.0
        )

      below =
        depth - baseline_shift - min(under * font_size, ink_bottom) + margin.bottom +
          padding.bottom +
          border.bottom

      height = raise + max(parent.line_height, below)

      count = if parent.line_height > 0, do: max(ceil(below / parent.line_height), 1), else: 1
      HtmlValidator.reserve_render_resource(:max_entries, count, :layout)

      HtmlValidator.reserve_render_resource(
        :max_work,
        count * (length(geometry.edges) + 1),
        :layout
      )

      inset = margin.left + padding.left + border.left

      content_alignment =
        case CssTextSpacingValidator.alignment(style, true) do
          :center -> (width - geometry.advance) / 2
          :right -> width - geometry.advance
          _ -> 0.0
        end

      end_space = margin.right + padding.right + border.right

      widths =
        Enum.map(0..(count - 1), fn index ->
          cond do
            wrap == :grid ->
              # One ic is a fixed valid length under the admitted positive
              # bounds. Resource exhaustion still propagates from its validator.
              {:ok, length} =
                NativeElixirPdfUtilities.Validators.CssLengthValidator.prepare("1ic", :margin)

              {:ok, grid} =
                NativeElixirPdfUtilities.Validators.CssLengthValidator.resolve(
                  length,
                  0.0,
                  parent
                )

              grid = grid + Map.get(parent, :letter_spacing, 0.0)
              if grid > 0, do: ceil(outer_width / grid) * grid, else: outer_width

            wrap == :all or (wrap == :first and index == 0 and not following_space?) ->
              limit = NativeElixirPdfUtilities.Limits.get(:max_numeric_magnitude)

              clipped =
                ClipGeometry.clip_edges(
                  geometry.edges,
                  {-limit, depth - (index + 1) * parent.line_height - baseline_shift, limit,
                   depth - index * parent.line_height - baseline_shift}
                )

              right =
                clipped
                |> Enum.flat_map(fn {{x, _}, {a, _}} -> [x, a] end)
                |> Enum.max(fn -> width end)

              min(outer_width, max(0.0, inset + right + end_space))

            index == 0 and wrap not in [:none, :first, :all] ->
              adjustment =
                case NativeElixirPdfUtilities.Validators.CssLengthValidator.resolve(
                       wrap,
                       width,
                       style
                     ) do
                  {:ok, {:percent, ratio}} -> width * ratio
                  {:ok, adjustment} -> adjustment
                  error -> throw({:render_resource_limit, error})
                end

              outer_width + adjustment

            true ->
              outer_width
          end
        end)

      run_style =
        parent
        |> Map.put(:css_transform, :none)
        |> Map.put(:display, :inline)
        |> Map.put(:margin, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})
        |> Map.put(:padding, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})
        |> Map.put(:border_widths, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0})

      {:ok,
       %{
         type: :inline_block,
         text: "",
         style: run_style,
         children: [],
         _inline_content_width: hd(widths),
         _inline_content_height: 0.0,
         _inline_baseline_depth: 0.0,
         _initial_letter_box: %{
           pieces: pieces,
           baseline_shift: baseline_shift,
           raise: raise,
           height: height,
           lines: count,
           width: width,
           exclusion_width: outer_width,
           inset: inset + content_alignment,
           style: style,
           border_box: %{
             x: margin.left,
             y: baseline_shift + under * font_size - padding.bottom - border.bottom,
             width: width + padding.left + padding.right + border.left + border.right,
             height: box_height
           }
         },
         _initial_exclusion: %{
           remaining: count - 1,
           widths: tl(widths)
         }
       }}
    end
  end

  @doc "Forwards an ancestor first-letter style to its first formatted block line."
  @spec forward_pseudo([map()], map()) :: [map()]
  def forward_pseudo(groups, style) do
    case Map.get(style, :_first_letter_style) do
      nil ->
        groups

      pseudo ->
        HtmlValidator.reserve_render_resource(:max_work, length(groups), :layout)

        {groups, _} =
          Enum.map_reduce(groups, pseudo, fn child, pending ->
            cond do
              pending == nil or get_in(child, [:style, :display]) == :none ->
                {child, pending}

              child.type == :inline_flow ->
                whitespace? =
                  Map.get(style, :white_space, :normal) in [:normal, :nowrap] and
                    Enum.all?(
                      child.children,
                      &(Map.get(&1, :type) == :text and
                          HtmlValidator.trim_ascii_whitespace(&1.text) == "")
                    )

                {child, if(whitespace?, do: pending)}

              NativeElixirPdfUtilities.Validators.CssDisplayValidator.independent?(child.style) ->
                {child, nil}

              true ->
                {%{child | style: Map.put_new(child.style, :_first_letter_style, pending)}, nil}
            end
          end)

        groups
    end
  end

  @doc "Carries a short paragraph's initial through the surrounding block flow."
  @spec following_exclusion([map()], number(), map() | nil) :: map() | nil
  def following_exclusion(boxes, bottom, previous) do
    region =
      Enum.reduce(boxes, previous, fn box, region ->
        case box do
          %{role: :initial_letter, _initial_exclusion_width: width} ->
            %{left: box.x, right: box.x + width, bottom: box.y}

          _ ->
            region
        end
      end)

    if region != nil and region.bottom < bottom, do: region
  end

  @doc "Keeps independent formatting contexts below an existing initial's exclusion."
  @spec flow_context(map(), map() | nil, number()) :: {map(), number()}
  def flow_context(style, region, top) do
    if region != nil and region.bottom < top do
      if NativeElixirPdfUtilities.Validators.CssDisplayValidator.independent?(style) do
        {Map.delete(style, :_initial_exclusion_region), region.bottom}
      else
        {Map.put(style, :_initial_exclusion_region, region), top}
      end
    else
      {Map.delete(style, :_initial_exclusion_region), top}
    end
  end

  @doc "Reserves an inherited initial at the actual width and line height of a subsequent paragraph."
  @spec inherit_exclusion([map()], map(), number(), number(), number()) :: [map()]
  def inherit_exclusion(runs, style, x, top, width) do
    case Map.get(style, :_initial_exclusion_region) do
      %{bottom: bottom} = region when bottom < top and style.line_height > 0 ->
        count = ceil((top - bottom) / style.line_height)
        HtmlValidator.reserve_render_resource(:max_entries, count, :layout)

        extent =
          if Map.get(style, :direction, :ltr) == :rtl,
            do: x + width - region.left,
            else: region.right - x

        extent = max(extent, 0.0)

        marker = %{
          type: :inline_positioning_marker,
          text: "",
          style: style,
          _inline_padding_width: extent,
          _initial_exclusion: %{remaining: count - 1, widths: List.duplicate(extent, count - 1)},
          inline_positioning_contexts: []
        }

        marker =
          Map.merge(
            marker,
            Map.take(List.first(runs) || %{}, [:_paragraph_wrap_style, :_paragraph_indent_basis])
          )

        [marker | runs]

      _ ->
        runs
    end
  end

  @doc "Carries the initial's exclusion into the next fitted line."
  @spec next_line([map()]) :: [map()]
  def next_line(line) do
    case Enum.find(line, &Map.has_key?(&1, :_initial_exclusion)) do
      %{_initial_exclusion: %{remaining: count} = exclusion} = run when count > 0 ->
        [
          %{
            type: :inline_positioning_marker,
            text: "",
            style: run.style,
            _inline_padding_width: hd(exclusion.widths),
            _initial_exclusion: %{exclusion | remaining: count - 1, widths: tl(exclusion.widths)},
            inline_positioning_contexts: []
          }
        ]

      _ ->
        []
    end
  end
end
