defmodule NativeElixirPdfUtilities.Validators.CssTextSpacingValidator do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, HtmlValidator}

  @properties ~w(word-spacing text-indent text-align-last text-justify hanging-punctuation)
  @separators [0x20, 0xA0, 0x1361, 0x10100, 0x10101, 0x1039F, 0x1091F]

  @doc "Returns the spacing and line alignment declarations owned by this validator."
  @spec properties() :: [String.t()]
  def properties, do: @properties

  @doc "Returns initial values for inherited spacing and line alignment."
  @spec defaults() :: map()
  def defaults,
    do: %{
      word_spacing: 0.0,
      text_indent: %{length: 0.0, hanging: false, each_line: false},
      text_align_last: :auto,
      text_justify: :auto,
      hanging_punctuation: []
    }

  @doc "Admits CSS Text 3 spacing, indentation and justification values."
  @spec prepare(map(), String.t(), String.t()) ::
          {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(style, property, value) do
    value = value |> String.trim() |> String.downcase(:ascii)

    case property do
      "word-spacing" ->
        if value == "normal" do
          {:ok, Map.put(style, :word_spacing, 0.0)}
        else
          with {:ok, length} <- CssLengthValidator.prepare(value, :signed_length),
               {:ok, length} <- CssLengthValidator.resolve(length, nil, style),
               do: {:ok, Map.put(style, :word_spacing, length)}
        end

      "text-indent" ->
        tokens = HtmlValidator.split_css_tokens(value)
        flags = Enum.filter(tokens, &(&1 in ["hanging", "each-line"]))
        lengths = Enum.reject(tokens, &(&1 in flags))

        with true <- length(Enum.uniq(flags)) == length(flags),
             [source] <- lengths,
             {:ok, length} <- CssLengthValidator.prepare(source, :margin),
             true <- length != :auto,
             {:ok, length} <- CssLengthValidator.resolve(length, nil, style) do
          {:ok,
           Map.put(style, :text_indent, %{
             length: length,
             hanging: "hanging" in flags,
             each_line: "each-line" in flags
           })}
        else
          {:error, _} = error -> error
          _ -> :error
        end

      "text-align-last" ->
        values = %{
          "auto" => :auto,
          "start" => :start,
          "end" => :end,
          "left" => :left,
          "right" => :right,
          "center" => :center,
          "justify" => :justify
        }

        if value == "match-parent" do
          {:ok, Map.put(style, :text_align_last, parent_alignment(style, :text_align_last))}
        else
          with {:ok, alignment} <- Map.fetch(values, value),
               do: {:ok, Map.put(style, :text_align_last, alignment)}
        end

      "text-justify" ->
        values = %{
          "auto" => :auto,
          "none" => :none,
          "inter-word" => :inter_word,
          "inter-character" => :inter_character,
          "distribute" => :inter_character
        }

        with {:ok, mode} <- Map.fetch(values, value),
             do: {:ok, Map.put(style, :text_justify, mode)}

      "hanging-punctuation" ->
        tokens = HtmlValidator.split_css_tokens(value)

        values = %{
          "first" => :first,
          "last" => :last,
          "allow-end" => :allow_end,
          "force-end" => :force_end
        }

        cond do
          tokens == ["none"] ->
            {:ok, Map.put(style, :hanging_punctuation, [])}

          tokens != [] and length(Enum.uniq(tokens)) == length(tokens) and
            Enum.all?(tokens, &Map.has_key?(values, &1)) and
              not ("allow-end" in tokens and "force-end" in tokens) ->
            {:ok, Map.put(style, :hanging_punctuation, Enum.map(tokens, &Map.fetch!(values, &1)))}

          true ->
            :error
        end
    end
  end

  @doc "Identifies nonzero-width word separators, excluding fixed-width and zero-width spaces."
  @spec word_separator?(non_neg_integer()) :: boolean()
  def word_separator?(codepoint), do: codepoint in @separators

  @doc "Counts word spacing opportunities after white-space processing."
  @spec word_count(String.t()) :: non_neg_integer()
  def word_count(text) do
    HtmlValidator.reserve_render_resource(:max_work, byte_size(text), :layout)
    Enum.count(String.to_charlist(text), &word_separator?/1)
  end

  @doc "Identifies script-appropriate opportunities between complete shaping clusters."
  @spec justification_boundary?(String.t(), String.t(), atom(), String.t()) :: boolean()
  def justification_boundary?(left, right, mode, language) do
    <<a::utf8, _::binary>> = left
    <<b::utf8, _::binary>> = right
    data = NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData
    sa = elem(data.properties(a), 0)
    sb = elem(data.properties(b), 0)
    cursive = ~w(Arab Syrc Thaa Mong Nkoo Mand Mani Phag Adlm Rohg Sogd Sogo Ougr Chrs Yezi)

    clustered =
      ~w(Beng Deva Gujr Guru Knda Khmr Laoo Mlym Mymr Orya Sinh Taml Telu Thai Bali Java Tale Talu Lana Cham Tavt)

    block = ~w(Hani Hira Kana Hang Bopo Yiii)

    unspaced? =
      sa in (block ++ clustered) or sb in (block ++ clustered) or
        a in 0x3000..0x303F or b in 0x3000..0x303F or a in 0xFF01..0xFF60 or b in 0xFF01..0xFF60

    cond do
      mode in [:none, :inter_word] -> false
      sa in cursive and sb in cursive -> false
      (left <> right) in ["——", "――", "……", "‥‥"] -> false
      length(UnicodeGrapheme.graphemes(left <> right)) < 2 -> false
      mode == :inter_character -> true
      word_separator?(a) or word_separator?(b) -> false
      language == "ko" or String.starts_with?(language, "ko-") -> false
      true -> unspaced?
    end
  end

  @doc "Computes match-parent by resolving the inherited alignment in its parent's direction."
  @spec parent_alignment(map(), :text_align | :text_align_last) :: atom()
  def parent_alignment(style, key) do
    parent = :maps.get(:_cascade_inherited, style, %{})
    value = Map.get(parent, key, if(key == :text_align, do: :start, else: :auto))

    value =
      if key == :text_align and value == :left and :maps.get(:_text_align_initial, parent, false),
        do: :start,
        else: value

    case value do
      :start -> if :maps.get(:direction, parent, :ltr) == :rtl, do: :right, else: :left
      :end -> if :maps.get(:direction, parent, :ltr) == :rtl, do: :left, else: :right
      value -> value
    end
  end

  @doc "Admits bounded paint spacing and ordered UTF-8 cluster offsets before PDF emission."
  @spec valid_paint?(map()) :: boolean()
  def valid_paint?(box) do
    if not is_map_key(box, :word_spacing) and not is_map_key(box, :_justification_spacing) and
         not is_map_key(box, :_justification_boundaries) do
      true
    else
      numbers? =
        Enum.all?([:word_spacing, :_justification_spacing], fn key ->
          match?({:ok, _}, HtmlValidator.normalize_geometry_number(Map.get(box, key, 0.0)))
        end)

      offsets = :maps.get(:_justification_boundaries, box, [])

      numbers? and
        (not Map.has_key?(box, :glyph_ids) or box.font_size > 0 or
           :maps.get(:word_spacing, box, 0) == 0) and
        is_list(offsets) and
        Enum.reduce_while(offsets, 0, fn offset, previous ->
          if is_integer(offset) and offset > previous and offset < byte_size(box.text) and
               :binary.at(box.text, offset) not in 0x80..0xBF,
             do: {:cont, offset},
             else: {:halt, false}
        end) != false
    end
  end

  @doc "Checks computed spacing values before layout consumes a caller-supplied styled tree."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    case style do
      %{
        word_spacing: spacing,
        text_indent: %{length: indent, hanging: false, each_line: false},
        text_align_last: :auto,
        text_justify: :auto,
        hanging_punctuation: []
      }
      when spacing == 0 and indent == 0 ->
        :ok

      _ ->
        indent = Map.get(style, :text_indent, %{length: 0.0, hanging: false, each_line: false})

        valid? =
          is_number(:maps.get(:word_spacing, style, 0.0)) and
            match?(
              %{length: _, hanging: h, each_line: e} when is_boolean(h) and is_boolean(e),
              indent
            ) and
            case indent.length do
              {:percent, ratio} ->
                is_number(ratio) and
                  match?({:ok, _}, HtmlValidator.normalize_geometry_number(ratio))

              value ->
                CssLengthValidator.valid_edge?(value, :margin)
            end and
            :maps.get(:text_align_last, style, :auto) in [
              :auto,
              :start,
              :end,
              :left,
              :right,
              :center,
              :justify
            ] and
            :maps.get(:text_justify, style, :auto) in [
              :auto,
              :none,
              :inter_word,
              :inter_character
            ] and
            is_list(:maps.get(:hanging_punctuation, style, [])) and
            Enum.all?(
              :maps.get(:hanging_punctuation, style, []),
              &(&1 in [:first, :last, :allow_end, :force_end])
            )

        if valid? do
          HtmlValidator.check_render_resource(
            :max_numeric_magnitude,
            abs(:maps.get(:word_spacing, style, 0.0)),
            :layout
          )

          :ok
        else
          NativeElixirPdfUtilities.Diagnostics.error(
            :layout,
            :invalid_layout,
            "Provide valid computed word spacing, indentation, last-line alignment and justification controls",
            module: __MODULE__,
            operation: :layout,
            source: source
          )
        end
    end
  end

  @doc "Resolves logical and last-line alignment, including lines ended by forced breaks."
  @spec alignment(map(), boolean()) :: :left | :right | :center | :justify
  def alignment(style, last?) do
    direction = :maps.get(:direction, style, :ltr)

    alignment =
      if :maps.get(:_text_align_initial, style, false) and
           :maps.get(:text_align, style, :left) == :left,
         do: :start,
         else: :maps.get(:text_align, style, :left)

    alignment =
      if last? do
        case :maps.get(:text_align_last, style, :auto) do
          :auto -> if alignment == :justify, do: :start, else: alignment
          value -> value
        end
      else
        alignment
      end

    case alignment do
      :start -> if direction == :rtl, do: :right, else: :left
      :end -> if direction == :rtl, do: :left, else: :right
      value -> value
    end
  end
end
