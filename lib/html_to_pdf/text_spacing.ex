defmodule NativeElixirPdfUtilities.HtmlToPdf.TextSpacing do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{
    CssLengthValidator,
    CssTextSpacingValidator,
    HtmlValidator,
    NativeGlyphSourceValidator
  }

  @doc "Retains the paragraph's edge policy through inline splitting and source-owned slices."
  @spec prepare_runs([map()], map(), (map() -> map())) :: [map()]
  def prepare_runs(runs, style, project_style) do
    wrap_style = :maps.get(:text_wrap_style, style, :auto)

    runs =
      case runs do
        [] ->
          []

        _ when wrap_style != :auto ->
          List.update_at(
            runs,
            0,
            &Map.put(&1, :_paragraph_wrap_style, wrap_style)
          )

        _ ->
          runs
      end

    hanging = :maps.get(:hanging_punctuation, style, [])
    indent = get_in(style, [:text_indent, :length])

    if hanging != [] or indent not in [nil, 0, 0.0] do
      style = project_style.(style)
      runs = Enum.map(runs, &Map.put(&1, :_paragraph_indent_style, style))

      if :last in hanging and runs != [] do
        List.update_at(runs, -1, fn run ->
          if :maps.get(:type, run, nil) in [nil, :text] do
            ending = Regex.replace(~r/[ \t\r\n\f]+\z/u, run.text, "")
            Map.merge(run, %{_paragraph_end_length: byte_size(ending), _paragraph_offset: 0})
          else
            run
          end
        end)
      else
        runs
      end
    else
      runs
    end
  end

  @doc "Balances static paragraphs by reusing the admitted line fitter and its shared work budget."
  @spec fit_lines([map()], number() | nil, ([map()], number() | nil -> [[map()]])) :: [[map()]]
  def fit_lines(runs, width, fit) do
    lines = fit.(runs, width)

    mode =
      case runs do
        [first | _] -> :maps.get(:_paragraph_wrap_style, first, :auto)
        [] -> :auto
      end

    pretty? =
      mode == :pretty and length(lines) > 1 and
        List.last(lines) |> Enum.map_join(& &1.text) |> String.split() |> length() < 2

    if is_number(width) and width > 0 and length(lines) > 1 and
         (mode == :balance or pretty?) do
      # The line fitter uses CSS's existing 1/64-pixel layout units. Binary
      # search terminates at that same fixed precision; no new resource knob.
      high = ceil(width / 0.75 * 64)
      count = length(lines)
      bytes = Enum.reduce(runs, 0, &(&2 + byte_size(&1.text)))
      runs = List.update_at(runs, 0, &Map.put(&1, :_paragraph_indent_basis, width))

      search = fn search, low, high, best ->
        if low >= high do
          best
        else
          middle = div(low + high, 2)
          HtmlValidator.reserve_render_resource(:max_work, bytes + 1, :layout)
          candidate = fit.(runs, middle / 64 * 0.75)

          if length(candidate) <= count,
            do: search.(search, low, middle, candidate),
            else: search.(search, middle + 1, high, best)
        end
      end

      search.(search, 1, high, lines)
    else
      lines
    end
  end

  @doc "Reserves an inherited paragraph indent as an unpainted logical start margin."
  @spec start_line([map()], number() | nil) :: [map()]
  def start_line(runs, width) do
    case runs do
      [first | _] ->
        style = Map.get(first, :_paragraph_indent_style, first.style)
        policy = Map.get(style, :text_indent, %{length: 0.0, hanging: false, each_line: false})

        basis = Map.get(first, :_paragraph_indent_basis, width) || 0.0

        case CssLengthValidator.resolve(policy.length, basis, style) do
          {:ok, length} ->
            length =
              case length do
                {:percent, ratio} -> basis * ratio
                value -> value
              end

            HtmlValidator.check_render_resource(:max_numeric_magnitude, abs(length), :layout)

            if length == 0 and :maps.get(:hanging_punctuation, style, []) == [],
              do: [],
              else: [indent_marker(style, %{policy | length: length}, true, true)]

          error ->
            throw({:render_resource_limit, error})
        end

      [] ->
        []
    end
  end

  @doc "Starts the next fitted or forced line with the same paragraph indentation policy."
  @spec next_line([[map()]], boolean()) :: [[map()]]
  def next_line(lines, forced? \\ false) do
    marker = Enum.find(List.last(lines) || [], &Map.has_key?(&1, :_text_indent_policy))

    following =
      if marker == nil,
        do: [],
        else: [
          indent_marker(
            marker.style,
            marker._text_indent_policy,
            forced? and marker._text_indent_policy.each_line,
            false
          )
        ]

    exclusion = NativeElixirPdfUtilities.HtmlToPdf.InitialLetter.next_line(List.last(lines) || [])
    lines ++ [following ++ exclusion]
  end

  @doc "Identifies a line containing only unpainted positioning margins."
  @spec empty_line?([map()]) :: boolean()
  def empty_line?(line), do: Enum.all?(line, &(Map.get(&1, :type) == :inline_positioning_marker))

  defp indent_marker(style, policy, selected?, first?) do
    selected? = if policy.hanging, do: not selected?, else: selected?

    %{
      type: :inline_positioning_marker,
      text: "",
      style: style,
      _text_indent_policy: policy,
      _paragraph_first_line: first?,
      _inline_padding_width: if(selected?, do: policy.length, else: 0.0),
      inline_positioning_contexts: []
    }
  end

  @doc "Measures a line after excluding punctuation allowed to hang outside its logical edges."
  @spec measured_width([map()], number() | nil, (map() -> number())) :: number()
  def measured_width(runs, width, advance) do
    natural = Enum.reduce(runs, 0.0, &(&2 + advance.(&1)))
    {first, last} = hanging_edges(runs, width, advance)
    natural - first - last
  end

  @doc "Returns the unmeasured start and end punctuation advances under CSS Text 3."
  @spec hanging_edges([map()], number() | nil, (map() -> number())) :: {number(), number()}
  def hanging_edges(runs, width, advance) do
    marker = List.first(runs)

    style =
      if marker == nil, do: %{}, else: Map.get(marker, :_paragraph_indent_style, marker.style)

    controls = :maps.get(:hanging_punctuation, style, [])

    if controls == [] do
      {0.0, 0.0}
    else
      visible =
        Enum.reject(
          runs,
          &(Map.get(&1, :type) in [:inline_positioning_marker, :soft_break] or &1.text == "")
        )

      first = List.first(visible)
      last = List.last(visible)

      blocked_start? =
        runs
        |> Enum.take_while(&(&1 != first))
        |> Enum.any?(fn run ->
          not Map.has_key?(run, :_text_indent_policy) and
            :maps.get(:_inline_padding_width, run, 0) != 0
        end)

      blocked_end? =
        runs
        |> Enum.reverse()
        |> Enum.take_while(&(&1 != last))
        |> Enum.any?(fn run ->
          not Map.has_key?(run, :_text_indent_policy) and
            :maps.get(:_inline_padding_width, run, 0) != 0
        end)

      opening =
        if first != nil and :maps.get(:type, first, nil) in [nil, :text],
          do: String.first(first.text)

      closing =
        if last != nil and :maps.get(:type, last, nil) in [nil, :text], do: String.last(last.text)

      # Inline border/padding margins are represented by intervening markers;
      # atomic content likewise prevents a punctuation glyph from reaching an edge.
      start =
        if :first in controls and not blocked_start? and
             Map.get(marker || %{}, :_paragraph_first_line, false) and
             opening != nil and Regex.match?(~r/\A[\p{Ps}\p{Pf}\p{Pi}'"\x{3000}]\z/u, opening),
           do: advance.(NativeGlyphSourceValidator.slice_run(first, 0, opening)),
           else: 0.0

      final? =
        last != nil and Map.has_key?(last, :_paragraph_end_length) and
          :maps.get(:_paragraph_offset, last, 0) + byte_size(last.text) ==
            last._paragraph_end_length

      quote? = closing != nil and Regex.match?(~r/\A[\p{Pe}\p{Pf}\p{Pi}'"]\z/u, closing)
      stop? = closing in [",", ".", "،", "۔", "、", "。", "，", "．", "﹐", "﹑", "﹒", "｡", "､"]

      end_width =
        if closing != nil and not blocked_end? and
             ((:last in controls and final? and quote?) or
                (stop? and Enum.any?(controls, &(&1 in [:force_end, :allow_end])))),
           do:
             advance.(
               NativeGlyphSourceValidator.slice_run(
                 last,
                 byte_size(last.text) - byte_size(closing),
                 closing
               )
             ),
           else: 0.0

      finish =
        if stop? and :allow_end in controls and not (:last in controls and final? and quote?) do
          natural = Enum.reduce(runs, 0.0, &(&2 + advance.(&1))) - start
          if width == nil, do: 0.0, else: min(end_width, max(natural - width, 0.0))
        else
          end_width
        end

      {start, finish}
    end
  end

  @doc "Returns cluster-safe expansion slots shared by line justification and ruby distribution."
  @spec justification_plan([map()], atom(), String.t()) :: {[map()], non_neg_integer()}
  def justification_plan(runs, mode, language) do
    plans =
      Enum.map(runs, fn run ->
        if mode != :none and :maps.get(:type, run, nil) in [nil, :text] and run.text != "" do
          boundaries = NativeGlyphSourceValidator.cluster_boundaries(run)

          units =
            boundaries
            |> Enum.chunk_every(2, 1, :discard)
            |> Enum.map(fn [a, b] ->
              %{text: binary_part(run.text, a, b - a), first: a, last: b}
            end)

          selected =
            units
            |> Enum.chunk_every(2, 1, :discard)
            |> Enum.flat_map(fn [left, right] ->
              if CssTextSpacingValidator.justification_boundary?(
                   left.text,
                   right.text,
                   mode,
                   language
                 ), do: [right.first], else: []
            end)

          words =
            if mode == :inter_character,
              do: 0,
              else: CssTextSpacingValidator.word_count(run.text)

          %{
            run: run,
            boundaries: selected,
            words: words,
            first: List.first(units),
            last: List.last(units)
          }
        else
          %{run: run, boundaries: [], words: 0, first: nil, last: nil}
        end
      end)

    plans =
      Enum.zip(plans, Enum.drop(plans, 1) ++ [nil])
      |> Enum.map(fn {plan, following} ->
        after? =
          following != nil and plan.last != nil and following.first != nil and
            CssTextSpacingValidator.justification_boundary?(
              plan.last.text,
              following.first.text,
              mode,
              language
            )

        Map.put(plan, :after?, after?)
      end)

    opportunities =
      Enum.reduce(plans, 0, fn plan, n ->
        n + plan.words + length(plan.boundaries) + if(plan.after?, do: 1, else: 0)
      end)

    {plans, opportunities}
  end

  @doc "Distributes remaining width across admitted word and script-specific cluster opportunities."
  @spec justify([map()], map(), number(), boolean(), (map() -> number())) :: [map()]
  def justify(runs, style, width, last?, advance) do
    last? = last? or Enum.any?(runs, &Map.get(&1, :_forced_line_end, false))
    mode = :maps.get(:text_justify, style, :auto)

    if CssTextSpacingValidator.alignment(style, last?) == :justify do
      {plans, opportunities} =
        justification_plan(runs, mode, :maps.get(:_text_language, style, "und"))

      remainder = width - measured_width(runs, width, advance)

      if opportunities > 0 and remainder > 0 do
        spacing = remainder / opportunities

        Enum.map(plans, fn plan ->
          if plan.words == 0 and plan.boundaries == [] and not plan.after? do
            plan.run
          else
            plan.run
            |> Map.update!(:style, fn run_style ->
              run_style
              |> Map.update(:word_spacing, if(plan.words > 0, do: spacing, else: 0.0), fn value ->
                value + if(plan.words > 0, do: spacing, else: 0.0)
              end)
              |> Map.put(
                :_justification_spacing,
                if(plan.boundaries == [], do: 0.0, else: spacing)
              )
            end)
            |> Map.put(:_justification_boundaries, plan.boundaries)
            |> Map.put(:_justification_after, if(plan.after?, do: spacing, else: 0.0))
          end
        end)
      else
        # CSS Text 3 section 6.4.3 specifies last-line alignment for text
        # without opportunities, with center as the fallback for justify.
        alignment = CssTextSpacingValidator.alignment(style, true)
        alignment = if alignment == :justify, do: :center, else: alignment

        alignment =
          if remainder < 0,
            do:
              CssTextSpacingValidator.alignment(
                %{text_align: :start, direction: :maps.get(:direction, style, :ltr)},
                false
              ),
            else: alignment

        if runs == [],
          do: runs,
          else: List.update_at(runs, 0, &Map.put(&1, :_line_alignment, alignment))
      end
    else
      runs
    end
  end
end
