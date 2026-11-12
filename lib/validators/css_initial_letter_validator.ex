defmodule NativeElixirPdfUtilities.Validators.CssInitialLetterValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, CssMathValidator, HtmlValidator}

  @doc "Selects the first Unicode 17 typographic unit with adjacent CSS first-letter punctuation."
  @spec first_letter(String.t()) :: String.t()
  def first_letter(text) do
    {leading, tail} = punctuation_prefix(text, 0)

    case NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme.next(tail) do
      nil ->
        leading

      {letter, tail} ->
        {trailing, _tail} = punctuation_prefix(tail, 0)
        leading <> letter <> trailing
    end
  end

  defp punctuation_prefix(original, consumed) do
    tail = binary_part(original, consumed, byte_size(original) - consumed)

    case NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme.next(tail) do
      {cluster, _remaining} ->
        <<codepoint::utf8, _::binary>> = cluster

        category =
          elem(NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData.properties(codepoint), 2)

        if category in [:Ps, :Pe, :Pi, :Pf, :Po, :Pc, :Pd] do
          punctuation_prefix(original, consumed + byte_size(cluster))
        else
          {binary_part(original, 0, consumed), tail}
        end

      nil ->
        {original, ""}
    end
  end

  @doc "Returns the initial-letter declarations."
  @spec properties() :: [String.t()]
  def properties, do: ~w(initial-letter initial-letter-align initial-letter-wrap)

  @doc "Returns CSS Inline 3 initial values."
  @spec defaults() :: map()
  def defaults,
    do: %{
      initial_letter: :normal,
      initial_letter_align: {:alphabetic, false},
      initial_letter_wrap: :none
    }

  @doc "Returns the dated CSS Inline 3 language defaults before author declarations."
  @spec language_defaults(map()) :: map()
  def language_defaults(node) do
    alias NativeElixirPdfUtilities.Validators.LanguageValidator

    case LanguageValidator.language_attribute(node) do
      {:ok, value} ->
        parts = LanguageValidator.canonicalize(value)

        mode =
          cond do
            Enum.any?(parts, &(&1 in ~w(latn cyrl))) -> :alphabetic
            Enum.any?(parts, &(&1 in ~w(hani hant hans))) -> :ideographic
            List.first(parts) in ~w(zh ja ko ii) -> :ideographic
            List.first(parts) in ~w(hi mr ne pi kok brx mai sd sa) -> :hanging
            true -> nil
          end

        if mode == nil, do: %{}, else: %{initial_letter_align: {mode, false}}

      :inherit ->
        %{}
    end
  end

  @doc "Admits initial sizing, sinking, alignment and wrapping values."
  @spec prepare(map(), String.t(), String.t()) ::
          {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(style, property, value) do
    tokens = HtmlValidator.split_css_tokens(String.downcase(String.trim(value), :ascii))

    case {property, tokens} do
      {"initial-letter", ["normal"]} ->
        {:ok, Map.put(style, :initial_letter, :normal)}

      {"initial-letter", tokens} ->
        {flags, numbers} = Enum.split_with(tokens, &(&1 in ["drop", "raise"]))

        with true <- length(flags) <= 1,
             true <- length(numbers) in 1..2,
             true <- flags == [] or length(numbers) == 1,
             {:ok, size} <- CssMathValidator.prepare_scalar(hd(numbers), :number, 1),
             {:ok, size} <- CssMathValidator.resolve_scalar(size, style),
             {:ok, sink} <- sink(numbers, flags, size, style) do
          {:ok, Map.put(style, :initial_letter, {size, sink})}
        else
          {:error, _} = error -> error
          _ -> :error
        end

      {"initial-letter-align", tokens} ->
        border? = List.first(tokens) == "border-box"
        remaining = if border?, do: tl(tokens), else: tokens

        modes = %{
          "alphabetic" => :alphabetic,
          "ideographic" => :ideographic,
          "hanging" => :hanging,
          "leading" => :leading
        }

        case remaining do
          [] when border? ->
            {:ok, Map.put(style, :initial_letter_align, {:alphabetic, true})}

          [mode] ->
            with {:ok, mode} <- Map.fetch(modes, mode),
                 do: {:ok, Map.put(style, :initial_letter_align, {mode, border?})}

          _ ->
            :error
        end

      {"initial-letter-wrap", [value]} when value in ~w(none first all grid) ->
        {:ok, Map.put(style, :initial_letter_wrap, String.to_existing_atom(value))}

      {"initial-letter-wrap", [_]} ->
        with {:ok, value} <- CssLengthValidator.prepare(value, :margin),
             true <- value != :auto,
             {:ok, value} <- CssLengthValidator.resolve(value, nil, style),
             do: {:ok, Map.put(style, :initial_letter_wrap, value)}

      _ ->
        :error
    end
  end

  # Both explicit and implicit sinking use this rule, including fractional sizes.
  defp sink(numbers, flags, size, style) do
    case numbers do
      [_, token] ->
        with {:ok, value} <- CssMathValidator.prepare_scalar(token, :integer, 1),
             do: CssMathValidator.resolve_scalar(value, style)

      [_] ->
        {:ok, if(flags == ["raise"], do: 1, else: floor(size))}
    end
  end

  @doc "Checks computed values before line layout consumes them."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    bound = Limits.get(:max_numeric_magnitude)

    size? =
      case Map.get(style, :initial_letter, :normal) do
        :normal ->
          true

        {size, sink} ->
          is_number(size) and size >= 1 and size <= bound and
            is_integer(sink) and sink >= 1 and sink <= bound

        _ ->
          false
      end

    align? =
      case Map.get(style, :initial_letter_align, {:alphabetic, false}) do
        {mode, border?} ->
          mode in [:alphabetic, :ideographic, :hanging, :leading] and is_boolean(border?)

        _ ->
          false
      end

    wrap? =
      case Map.get(style, :initial_letter_wrap, :none) do
        mode when mode in [:none, :first, :all, :grid] -> true
        value when is_number(value) -> abs(value) <= bound
        {:percent, value} -> is_number(value) and abs(value) <= bound
        value -> CssLengthValidator.valid_edge?(value, :margin)
      end

    pseudo? =
      case Map.fetch(style, :_first_letter_style) do
        :error ->
          true

        {:ok, %{font_size: size, line_height: height} = pseudo} ->
          not Map.has_key?(pseudo, :_first_letter_style) and
            is_number(size) and size >= 0 and size <= bound and
            is_number(height) and height >= 0 and height <= bound and
            match?(
              :ok,
              HtmlValidator.validate_font_fallback_input(
                %{type: :document, children: [%{type: :text, text: "", style: pseudo}]},
                :error
              )
            ) and
            match?(:ok, validate_style(pseudo, source))

        _ ->
          false
      end

    if size? and align? and wrap? and pseudo? do
      :ok
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "recompute initial-letter sizing and alignment",
        module: __MODULE__,
        operation: :layout,
        source: source
      )
    end
  end

  @doc "Returns normalized over and under alignment points from selected font metrics."
  @spec alignment_metrics(map(), atom(), number()) ::
          {:ok, {number(), number()}} | {:error, {atom(), map()}}
  @spec alignment_metrics(map(), atom(), number(), String.t() | nil) ::
          {:ok, {number(), number()}} | {:error, {atom(), map()}}
  def alignment_metrics(font, mode, size, script \\ nil) do
    alias NativeElixirPdfUtilities.HtmlToPdf.{Font, TextShaper}
    alias NativeElixirPdfUtilities.Validators.FontBaselineValidator
    script = script || if(mode == :ideographic, do: "Hani", else: "Deva")
    axis = if Map.has_key?(font, :_vertical_text), do: :vertical, else: :horizontal
    ppem = Font.used_size(font, size) / 0.75
    ppem = if ppem > 0, do: ppem

    selected =
      if mode in [:ideographic, :hanging],
        do:
          FontBaselineValidator.coordinates(
            font,
            axis,
            TextShaper.script_tags(script) ++ ["DFLT"],
            ppem
          ),
        else: {:ok, nil}

    with {:ok, baselines} <- selected do
      values = if baselines == nil, do: %{}, else: baselines.values

      alignment =
        case mode do
          :hanging ->
            if Map.has_key?(values, "hang"), do: {values["hang"], Map.get(values, "romn", 0)}

          :ideographic ->
            under = Map.get(values, "icfb", Map.get(values, "ideo"))

            over =
              Map.get(
                values,
                "icft",
                if(under != nil, do: under + Map.get(font, :units_per_em, 1000))
              )

            if under != nil and over != nil, do: {over, under}

          _ ->
            nil
        end

      case alignment do
        {over, under} when over > under ->
          ratio = Font.used_size(font, 1.0) / font.units_per_em
          {:ok, {over * ratio, under * ratio}}

        _ ->
          synthesized_alignment_metrics(font, mode, size)
      end
    end
  end

  defp synthesized_alignment_metrics(font, mode, size) do
    alias NativeElixirPdfUtilities.HtmlToPdf.Font
    alias NativeElixirPdfUtilities.Validators.{FontMetricValidator, FontValidator}

    case mode do
      :ideographic ->
        metrics = Font.line_metrics(font)
        ascent = Map.get(metrics, :ascent, 880)
        descent = Map.get(metrics, :descent, -120)
        extent = ascent - descent
        {:ok, {ascent / extent, descent / extent}}

      :hanging ->
        {:ok, {0.6, 0.0}}

      :leading ->
        metrics = Font.line_metrics(font)
        units = Map.get(metrics, :units_per_em, 1000)
        {:ok, {Map.get(metrics, :ascent, 800) / units, Map.get(metrics, :descent, -200) / units}}

      _ ->
        case font do
          %{type: :embedded} ->
            with {:ok, aspect} <- FontMetricValidator.aspect(font, "cap-height") do
              aspect = (aspect || 0.66) * Font.used_size(font, 1.0)
              # Without sCapHeight, synthesize from the H outline at the
              # selected CSS pixel size, as in FreeType's hinted glyph metric.
              aspect =
                case FontValidator.prepare_text_metrics(font) do
                  {:ok, %{cap_height: nil}} when size > 0 ->
                    max(round(aspect * size / 0.75), 1) * 0.75 / size

                  _ ->
                    aspect
                end

              {:ok, {aspect, 0.0}}
            end

          _ ->
            {:ok, {0.66, 0.0}}
        end
    end
  end
end
