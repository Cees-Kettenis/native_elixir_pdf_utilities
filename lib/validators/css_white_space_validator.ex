defmodule NativeElixirPdfUtilities.Validators.CssWhiteSpaceValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, CssMathValidator, HtmlValidator}

  @collapse %{
    "collapse" => :collapse,
    "discard" => :discard,
    "preserve" => :preserve,
    "preserve-breaks" => :preserve_breaks,
    "preserve-spaces" => :preserve_spaces,
    "break-spaces" => :break_spaces
  }
  @wrap %{"wrap" => :wrap, "nowrap" => :nowrap}
  @trim %{
    "discard-before" => :discard_before,
    "discard-after" => :discard_after,
    "discard-inner" => :discard_inner
  }
  @legacy %{
    "normal" => {:normal, :collapse, :wrap},
    "nowrap" => {:nowrap, :collapse, :nowrap},
    "pre" => {:pre, :preserve, :nowrap},
    "pre-wrap" => {:pre_wrap, :preserve, :wrap},
    "pre-line" => {:pre_line, :preserve_breaks, :wrap},
    "break-spaces" => {:break_spaces, :break_spaces, :wrap}
  }
  @legacy_modes Map.new(@legacy, fn {_name, {mode, _, _} = values} -> {mode, values} end)
  @wrap_styles %{"auto" => :auto, "balance" => :balance, "stable" => :stable, "pretty" => :pretty}
  @trim_values Map.values(@trim)
  @collapse_values Map.values(@collapse)
  @wrap_style_values Map.values(@wrap_styles)
  @properties ~w(white-space white-space-collapse white-space-trim text-wrap-mode text-wrap-style text-wrap tab-size)
  @cjk_segment_break Regex.compile!(
                       "(?<=[\\p{Han}\\p{Hiragana}\\p{Katakana}\\x{3000}-\\x{303F}\\x{FF01}-\\x{FF60}])" <>
                         "[ \\t]*[\\r\\n]+[ \\t]*" <>
                         "(?=[\\p{Han}\\p{Hiragana}\\p{Katakana}\\x{3000}-\\x{303F}\\x{FF01}-\\x{FF60}])",
                       "u"
                     )

  @doc "Returns the whitespace declarations owned by this validator."
  @spec properties() :: [String.t()]
  def properties, do: @properties

  @doc "Returns initial longhand values and their legacy fitting projection."
  @spec defaults() :: map()
  def defaults,
    do: %{
      white_space: :normal,
      white_space_collapse: :collapse,
      white_space_trim: [],
      text_wrap_mode: :wrap,
      text_wrap_style: :auto,
      tab_size: {:number, 8.0}
    }

  @doc "Expands an existing legacy whitespace default without changing unrelated style fields."
  @spec expand_legacy(map()) :: map()
  def expand_legacy(style) do
    case Map.fetch(style, :white_space) do
      {:ok, mode} ->
        {_, collapse, wrap} = Map.fetch!(@legacy_modes, mode)

        Map.merge(style, %{
          white_space_collapse: collapse,
          text_wrap_mode: wrap,
          white_space_trim: []
        })

      :error ->
        style
    end
  end

  @doc "Computes the shared fitting mode from the independently cascaded whitespace longhands."
  @spec compute_style(map()) :: map()
  def compute_style(style) do
    if Map.has_key?(style, :white_space_collapse) or Map.has_key?(style, :text_wrap_mode) do
      {collapse, wrap} =
        case style do
          %{white_space_collapse: collapse, text_wrap_mode: wrap} ->
            {collapse, wrap}

          _ ->
            baseline = expand_legacy(Map.take(style, [:white_space]))

            {Map.get(
               style,
               :white_space_collapse,
               :maps.get(:white_space_collapse, baseline, :collapse)
             ), Map.get(style, :text_wrap_mode, :maps.get(:text_wrap_mode, baseline, :wrap))}
        end

      mode =
        case {collapse, wrap} do
          {mode, :nowrap} when mode in [:preserve, :preserve_spaces, :break_spaces] -> :pre
          {_, :nowrap} -> :nowrap
          {:preserve, _} -> :pre_wrap
          {:preserve_spaces, _} -> :pre_wrap
          {:preserve_breaks, _} -> :pre_line
          {:break_spaces, _} -> :break_spaces
          _ -> :normal
        end

      Map.put(style, :white_space, mode)
    else
      style
    end
  end

  @doc "Admits whitespace shorthands, independent collapse/wrap longhands and nonnegative tab sizes."
  @spec prepare(map(), String.t(), String.t()) ::
          {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(style, property, value) do
    value = value |> String.trim() |> String.downcase(:ascii)

    case property do
      "white-space" ->
        case Map.fetch(@legacy, value) do
          {:ok, {mode, collapse, wrap}} ->
            {:ok,
             Map.merge(style, %{
               white_space: mode,
               white_space_collapse: collapse,
               text_wrap_mode: wrap,
               white_space_trim: []
             })}

          :error ->
            tokens = HtmlValidator.split_css_tokens(value)

            if length(tokens) in 1..5 and length(Enum.uniq(tokens)) == length(tokens) and
                 Enum.count(tokens, &Map.has_key?(@collapse, &1)) <= 1 and
                 Enum.count(tokens, &Map.has_key?(@wrap, &1)) <= 1 and
                 not ("none" in tokens and Enum.any?(tokens, &Map.has_key?(@trim, &1))) and
                 Enum.all?(
                   tokens,
                   &(Map.has_key?(@collapse, &1) or Map.has_key?(@wrap, &1) or
                       Map.has_key?(@trim, &1) or &1 == "none")
                 ) do
              collapse = Enum.find_value(tokens, :collapse, &Map.get(@collapse, &1))
              wrap = Enum.find_value(tokens, :wrap, &Map.get(@wrap, &1))

              trim =
                Enum.flat_map(tokens, fn token ->
                  case Map.fetch(@trim, token) do
                    {:ok, value} -> [value]
                    :error -> []
                  end
                end)

              {:ok,
               compute_style(
                 Map.merge(style, %{
                   white_space_collapse: collapse,
                   text_wrap_mode: wrap,
                   white_space_trim: trim
                 })
               )}
            else
              :error
            end
        end

      "white-space-collapse" ->
        with {:ok, collapse} <- Map.fetch(@collapse, value),
             do: {:ok, Map.put(style, :white_space_collapse, collapse)}

      "text-wrap-mode" ->
        with {:ok, wrap} <- Map.fetch(@wrap, value),
             do: {:ok, Map.put(style, :text_wrap_mode, wrap)}

      "text-wrap-style" ->
        with {:ok, mode} <- Map.fetch(@wrap_styles, value),
             do: {:ok, Map.put(style, :text_wrap_style, mode)}

      "text-wrap" ->
        tokens = HtmlValidator.split_css_tokens(value)

        if length(tokens) in 1..2 and length(Enum.uniq(tokens)) == length(tokens) and
             Enum.count(tokens, &Map.has_key?(@wrap, &1)) <= 1 and
             Enum.count(tokens, &Map.has_key?(@wrap_styles, &1)) <= 1 and
             Enum.all?(tokens, &(Map.has_key?(@wrap, &1) or Map.has_key?(@wrap_styles, &1))) do
          {:ok,
           Map.merge(style, %{
             text_wrap_mode: Enum.find_value(tokens, :wrap, &Map.get(@wrap, &1)),
             text_wrap_style: Enum.find_value(tokens, :auto, &Map.get(@wrap_styles, &1))
           })}
        else
          :error
        end

      "white-space-trim" ->
        tokens = HtmlValidator.split_css_tokens(value)

        cond do
          tokens == ["none"] ->
            {:ok, Map.put(style, :white_space_trim, [])}

          length(tokens) in 1..3 and length(Enum.uniq(tokens)) == length(tokens) and
              Enum.all?(tokens, &Map.has_key?(@trim, &1)) ->
            {:ok, Map.put(style, :white_space_trim, Enum.map(tokens, &Map.fetch!(@trim, &1)))}

          true ->
            :error
        end

      "tab-size" ->
        case CssMathValidator.prepare_scalar(value, :number, 0) do
          {:ok, number} ->
            with {:ok, number} <- CssMathValidator.resolve_scalar(number, style),
                 do: {:ok, Map.put(style, :tab_size, {:number, number})}

          _ ->
            with {:ok, length} <- CssLengthValidator.prepare(value, :length),
                 do: {:ok, Map.put(style, :tab_size, {:length, length})}
        end

      _ ->
        :error
    end
  end

  @doc "Validates styled-tree whitespace values through the same admitted control model."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    tab_valid? =
      is_map(style) and
        case Map.get(style, :tab_size, {:number, 8.0}) do
          {:number, number} ->
            is_number(number) and number >= 0 and
              match?({:ok, _}, HtmlValidator.normalize_geometry_number(number))

          {:length, length} ->
            CssLengthValidator.valid_edge?(length, :border_widths)

          _ ->
            false
        end

    trim = if is_map(style), do: :maps.get(:white_space_trim, style, [])

    if tab_valid? and is_list(trim) and Enum.all?(trim, &(&1 in @trim_values)) and
         length(Enum.uniq(trim)) == length(trim) and
         :maps.get(:white_space, style, :normal) in [
           :normal,
           :nowrap,
           :pre,
           :pre_wrap,
           :pre_line,
           :break_spaces
         ] and
         :maps.get(:white_space_collapse, style, :collapse) in @collapse_values and
         :maps.get(:text_wrap_mode, style, :wrap) in [:wrap, :nowrap] and
         :maps.get(:text_wrap_style, style, :auto) in @wrap_style_values do
      :ok
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "styled text requires valid whitespace, wrapping and tab-size controls",
        operation: :layout,
        module: __MODULE__,
        source: source
      )
    end
  end

  @doc "Returns the shared ordered scalar-preserving whitespace normalization operations."
  @spec normalizations(map()) :: [{Regex.t(), String.t()}]
  def normalizations(style) do
    baseline =
      if Map.has_key?(style, :white_space_collapse), do: style, else: expand_legacy(style)

    case :maps.get(:white_space_collapse, baseline, :collapse) do
      mode when mode in [:preserve, :break_spaces] ->
        [{~r/\r\n?|\n/u, "\n"}]

      :preserve_breaks ->
        [{~r/\r\n?|\n/u, "\n"}, {~r/[ \t\f]+/u, " "}]

      :preserve_spaces ->
        [{~r/\r\n?|[\r\n\t\f]/u, " "}]

      :discard ->
        [{~r/[ \t\r\n\f]+/u, ""}]

      :collapse ->
        # Korean words retain spaces, including Hanja. Chinese and Japanese
        # segment breaks between full-width text do not introduce a word gap.
        language = style |> Map.get(:_text_language, "und") |> String.split("-") |> hd()
        segment_breaks = if language == "ko", do: [], else: [{@cjk_segment_break, ""}]
        segment_breaks ++ [{~r/[ \t\r\n\f]+/u, " "}]
    end
  end

  @doc "Resolves a tab to the next stop, keeping the CSS minimum half-space advance."
  @spec tab_advance(map(), number(), number()) :: number()
  def tab_advance(style, position, space_width) do
    size =
      case Map.get(style, :tab_size, {:number, 8.0}) do
        {:number, count} ->
          count * space_width

        {:length, value} ->
          case CssLengthValidator.resolve(value, nil, style) do
            {:ok, size} -> size
            error -> throw({:render_resource_limit, error})
          end
      end

    if size == 0 do
      0.0
    else
      advance = (Float.floor(position / size) + 1) * size - position
      if advance < space_width / 2, do: advance + size, else: advance
    end
  end

  @doc "Applies admitted inline edge scopes and block/inline inner trimming before paragraph itemization."
  @spec trim_runs([map()], map()) :: [map()]
  def trim_runs(runs, style) do
    if :maps.get(:white_space_trim, style, []) == [] and
         not Enum.any?(runs, &(Map.get(&1, :type) == :inline_whitespace_boundary)) do
      runs
    else
      {runs, _} =
        Enum.reduce(runs, {[], false}, fn run, {result, discard?} ->
          case run do
            %{type: :inline_whitespace_boundary, edge: :before, controls: controls} ->
              if :discard_before in controls,
                do: {trim_adjacent(result, :trailing), discard?},
                else: {result, discard?}

            %{type: :inline_whitespace_boundary, edge: :after, controls: controls} ->
              {result, discard? or :discard_after in controls}

            _ ->
              if discard? do
                [selected] = trim_adjacent([run], :leading)

                {[selected | result],
                 selected.text == "" and
                   :maps.get(:type, selected, nil) not in [:inline_block, :inline_image]}
              else
                {[run | result], false}
              end
          end
        end)

      runs = Enum.reverse(runs)

      if :discard_inner in :maps.get(:white_space_trim, style, []) do
        pieces =
          Enum.map(runs, fn run ->
            if :maps.get(:type, run, nil) in [:inline_block, :inline_image] or
                 :maps.get(:hard_break, run, false), do: "*", else: run.text
          end)

        text = Enum.join(pieces)
        HtmlValidator.reserve_render_resource(:max_work, byte_size(text), :layout)
        [leading] = Regex.run(~r/\A[ \t\r\n\f]*/u, text)
        [trailing] = Regex.run(~r/[ \t\r\n\f]*\z/u, text)
        inline? = :maps.get(:display, style, nil) == :inline

        first =
          if inline? do
            byte_size(leading)
          else
            case List.last(:binary.matches(leading, "\n")) do
              nil -> 0
              {offset, 1} -> offset + 1
            end
          end

        last =
          if inline? do
            byte_size(text) - byte_size(trailing)
          else
            case :binary.match(trailing, "\n") do
              :nomatch -> byte_size(text)
              {offset, 1} -> byte_size(text) - byte_size(trailing) + offset
            end
          end

        last = max(first, last)

        Enum.zip(runs, pieces)
        |> Enum.map_reduce(0, fn {run, piece}, offset ->
          finish = offset + byte_size(piece)

          selected =
            if piece == run.text do
              start = min(max(first - offset, 0), byte_size(piece))
              count = max(min(last, finish) - max(first, offset), 0)

              NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator.slice_run(
                run,
                start,
                binary_part(piece, start, count)
              )
            else
              run
            end

          {selected, finish}
        end)
        |> elem(0)
      else
        runs
      end
      |> Enum.reject(fn run -> run.text == "" and :maps.get(:type, run, nil) in [nil, :text] end)
    end
  end

  @doc "Trims a line's collapsible leading text while retaining admitted source owners."
  @spec trim_line_start([map()]) :: [map()]
  def trim_line_start(runs), do: trim_adjacent(runs, :leading)

  @doc "Measures alignment with hanging preserved spaces while retaining their source and paint advances."
  @spec alignment_width([map()], number(), boolean(), ([map()] -> number())) :: number()
  def alignment_width(runs, available, last_line?, measure) do
    total = measure.(runs)

    if Enum.any?(runs, &(Map.get(&1.style, :white_space) == :pre_wrap)) do
      conditional? = last_line? or Enum.any?(runs, &Map.get(&1, :_forced_line_end, false))

      if not conditional? or total > available,
        do:
          runs
          |> Enum.reverse()
          |> trim_adjacent(:trailing, [:pre_wrap])
          |> Enum.reverse()
          |> measure.(),
        else: total
    else
      total
    end
  end

  # Both edges consume the same source records. Once visible content or
  # preserved whitespace is reached, the rest of the run list stays shared.
  defp trim_adjacent(runs, edge, modes \\ [:normal, :nowrap, :pre_line]) do
    case runs do
      [] ->
        []

      [run | rest] ->
        HtmlValidator.reserve_render_resource(
          :max_work,
          byte_size(run.text) + 1,
          :layout
        )

        cond do
          :maps.get(:type, run, nil) in [:inline_block, :inline_image] or
              :maps.get(:hard_break, run, false) ->
            runs

          modes == [:pre_wrap] and :maps.get(:_inline_padding_width, run, 0.0) != 0.0 ->
            runs

          :maps.get(:type, run, nil) in [:inline_positioning_marker, :soft_break] ->
            [run | trim_adjacent(rest, edge, modes)]

          :maps.get(:white_space, run.style, :normal) in modes ->
            pattern = if edge == :leading, do: ~r/\A[ \t\f\r]+/u, else: ~r/[ \t\f\r]+\z/u
            text = String.replace(run.text, pattern, "")

            selected =
              NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator.trim_run(run, text)

            if text == "",
              do: [selected | trim_adjacent(rest, edge, modes)],
              else: [selected | rest]

          true ->
            runs
        end
    end
  end
end
