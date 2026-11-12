defmodule NativeElixirPdfUtilities.Validators.CssTextEmphasisValidator do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, WriterValidator}

  @doc "Lists emphasis longhands and shorthand."
  @spec properties() :: [String.t()]
  def properties,
    do: ~w(text-emphasis text-emphasis-style text-emphasis-color text-emphasis-position)

  @doc "Returns inherited initial emphasis values."
  @spec defaults() :: map()
  def defaults,
    do: %{
      text_emphasis_style: :none,
      text_emphasis_color: :current_color,
      text_emphasis_position: {:over, :right}
    }

  @doc "Admits emphasis marks, color, physical side and shorthand resets."
  @spec prepare(map(), String.t(), String.t()) ::
          {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(style, property, value) do
    tokens = HtmlValidator.split_css_tokens(value)

    case property do
      "text-emphasis-position" ->
        tokens = Enum.map(tokens, &String.downcase(&1, :ascii))
        block = Enum.filter(tokens, &(&1 in ["over", "under"]))
        side = Enum.filter(tokens, &(&1 in ["left", "right"]))

        if length(block) == 1 and length(side) <= 1 and
             length(tokens) == length(block) + length(side) do
          {:ok,
           Map.put(
             style,
             :text_emphasis_position,
             {if(hd(block) == "over", do: :over, else: :under),
              if(side == ["left"], do: :left, else: :right)}
           )}
        else
          :error
        end

      "text-emphasis-color" ->
        if String.downcase(String.trim(value), :ascii) == "currentcolor" do
          {:ok, Map.put(style, :text_emphasis_color, :current_color)}
        else
          with {:ok, color} <- HtmlValidator.parse_css_color(value, nil),
               true <- color != nil,
               do: {:ok, Map.put(style, :text_emphasis_color, color)}
        end

      "text-emphasis-style" ->
        with {:ok, mark} <- mark(tokens), do: {:ok, Map.put(style, :text_emphasis_style, mark)}

      "text-emphasis" ->
        {colors, marks} =
          Enum.split_with(tokens, fn token ->
            String.downcase(token, :ascii) == "currentcolor" or
              match?(
                {:ok, color} when not is_nil(color),
                HtmlValidator.parse_css_color(token, nil)
              )
          end)

        with true <- length(colors) <= 1,
             {:ok, updated} <-
               prepare(style, "text-emphasis-color", List.first(colors, "currentcolor")),
             {:ok, mark} <- if(marks == [], do: {:ok, :none}, else: mark(marks)),
             do: {:ok, Map.put(updated, :text_emphasis_style, mark)}

      _ ->
        :error
    end
  end

  defp mark(tokens) do
    case tokens do
      [token] ->
        if Regex.match?(Regex.compile!("\\A#{HtmlValidator.css_string_pattern()}\\z", "u"), token) do
          {:ok,
           {:custom,
            token
            |> HtmlValidator.decode_css_string()
            |> UnicodeGrapheme.graphemes()
            |> List.first("")}}
        else
          keyword_mark(tokens)
        end

      _ ->
        keyword_mark(tokens)
    end
  end

  defp keyword_mark(tokens) do
    tokens = Enum.map(tokens, &String.downcase(&1, :ascii))
    fills = Enum.filter(tokens, &(&1 in ["filled", "open"]))

    shapes =
      Enum.filter(tokens, &(&1 in ["dot", "circle", "double-circle", "triangle", "sesame"]))

    cond do
      tokens == ["none"] ->
        {:ok, :none}

      tokens != [] and length(fills) <= 1 and length(shapes) <= 1 and
          length(tokens) == length(fills) + length(shapes) ->
        shape =
          case List.first(shapes) do
            nil -> :auto
            "dot" -> :dot
            "circle" -> :circle
            "double-circle" -> :double_circle
            "triangle" -> :triangle
            "sesame" -> :sesame
          end

        {:ok, {if(fills == ["open"], do: :open, else: :filled), shape}}

      true ->
        :error
    end
  end

  @doc "Checks emphasis style data before shaping or layout."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    case style do
      %{
        text_emphasis_style: :none,
        text_emphasis_color: :current_color,
        text_emphasis_position: {:over, :right}
      } ->
        :ok

      _ ->
        mark =
          case Map.get(style, :text_emphasis_style, :none) do
            :none ->
              true

            {:custom, text} when is_binary(text) ->
              String.valid?(text) and length(UnicodeGrapheme.graphemes(text)) <= 1

            {fill, shape} ->
              fill in [:filled, :open] and
                shape in [:auto, :dot, :circle, :double_circle, :triangle, :sesame]

            _ ->
              false
          end

        color = Map.get(style, :text_emphasis_color, :current_color)

        {block, side} =
          case Map.get(style, :text_emphasis_position, {:over, :right}) do
            {block, side} -> {block, side}
            _ -> {nil, nil}
          end

        if mark and block in [:over, :under] and side in [:left, :right] and
             (color in [:current_color, :transparent] or WriterValidator.valid_color?(color)) do
          if get_in(style, [:font_face, :type]) == :built_in and Map.get(style, :font_size, 0) > 0 and
               Map.get(style, :text_emphasis_style, :none) not in [:none, {:custom, ""}] do
            Diagnostics.error(
              :font,
              :invalid_document,
              "text emphasis requires selected font metrics and glyph outlines; choose an embedded font instead of a built-in PDF face",
              module: __MODULE__,
              operation: :layout,
              source: source
            )
          else
            :ok
          end
        else
          Diagnostics.error(
            :layout,
            :invalid_layout,
            "recompute text-emphasis style, color and position",
            module: __MODULE__,
            operation: :layout,
            source: source
          )
        end
    end
  end

  @doc "Applies CSS emphasis exclusions using the pinned Unicode general categories."
  @spec receives?(non_neg_integer()) :: boolean()
  def receives?(codepoint) do
    category = elem(UnicodeTextData.properties(codepoint), 2)

    (category not in [:Zs, :Zl, :Zp, :Cn, :Cc, :Cf, :Pd, :Ps, :Pe, :Pc, :Po, :Pi, :Pf] and
       codepoint not in [0x1361, 0x10100, 0x10101, 0x1039F, 0x0F0B, 0x0F0C]) or
      codepoint in [
        0x23,
        0x25,
        0x26,
        0x40,
        0xA7,
        0xB6,
        0x609,
        0x60A,
        0x66A,
        0x2030,
        0x2031,
        0x204A,
        0x204B,
        0x2053,
        0x303D,
        0xFE5F,
        0xFE60,
        0xFE6A,
        0xFE6B,
        0xFF03,
        0xFF05,
        0xFF06,
        0xFF20
      ]
  end
end
