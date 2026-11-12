defmodule NativeElixirPdfUtilities.Validators.CssTextShadowValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, HtmlValidator, WriterValidator}

  @doc "Lists inherited text shadow properties."
  @spec properties() :: [String.t()]
  def properties, do: ["text-shadow"]

  @doc "Returns the initial shadow list."
  @spec defaults() :: map()
  def defaults, do: %{text_shadow: []}

  @doc "Admits ordered shadow colors and two signed lengths with an optional nonnegative blur."
  @spec prepare(map(), String.t(), String.t()) ::
          {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(style, property, value) do
    if property == "text-shadow" do
      if String.downcase(String.trim(value), :ascii) == "none" do
        {:ok, Map.put(style, :text_shadow, [])}
      else
        entries = HtmlValidator.split_css_comma(value, true)
        HtmlValidator.check_render_resource(:max_entries, length(entries), :style)

        Enum.reduce_while(entries, {:ok, []}, fn entry, {:ok, shadows} ->
          tokens = HtmlValidator.split_css_tokens(entry)

          parsed =
            Enum.reduce_while(tokens, {:ok, [], nil}, fn token, {:ok, lengths, color} ->
              paint =
                if String.downcase(token, :ascii) == "currentcolor",
                  do: {:ok, :current_color},
                  else: HtmlValidator.parse_css_color(token, nil)

              case paint do
                {:ok, paint} when not is_nil(paint) ->
                  if color == nil, do: {:cont, {:ok, lengths, paint}}, else: {:halt, :error}

                _ ->
                  policy = if length(lengths) < 2, do: :signed_length, else: :length

                  with {:ok, length} <- CssLengthValidator.prepare(token, policy),
                       {:ok, length} when is_number(length) <-
                         CssLengthValidator.resolve(length, nil, style) do
                    {:cont, {:ok, lengths ++ [length], color}}
                  else
                    {:error, _} = error -> {:halt, error}
                    _ -> {:halt, :error}
                  end
              end
            end)

          case parsed do
            {:ok, [x, y | rest], color} when length(rest) <= 1 ->
              shadow = %{x: x, y: y, blur: List.first(rest, 0.0), color: color || :current_color}
              {:cont, {:ok, [shadow | shadows]}}

            {:error, _} = error ->
              {:halt, error}

            _ ->
              {:halt, :error}
          end
        end)
        |> case do
          {:ok, shadows} -> {:ok, Map.put(style, :text_shadow, Enum.reverse(shadows))}
          error -> error
        end
      end
    else
      :error
    end
  end

  @doc "Validates prepared shadow lists before layout or paint."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    case style do
      %{text_shadow: []} ->
        :ok

      _ ->
        shadows = Map.get(style, :text_shadow, [])

        valid? =
          is_list(shadows) and
            Enum.all?(shadows, fn shadow ->
              case shadow do
                %{x: x, y: y, blur: blur, color: color} ->
                  Enum.all?(
                    [x, y, blur],
                    &match?({:ok, _}, HtmlValidator.normalize_geometry_number(&1))
                  ) and
                    blur >= 0 and
                    (color in [:current_color, :transparent] or
                       WriterValidator.valid_color?(color))

                _ ->
                  false
              end
            end)

        if valid? do
          HtmlValidator.check_render_resource(:max_entries, length(shadows), :layout)

          if get_in(style, [:font_face, :type]) == :built_in and Map.get(style, :font_size, 0) > 0 and
               Enum.any?(shadows, &(&1.blur > 0 and &1.color != :transparent)) do
            Diagnostics.error(
              :font,
              :invalid_document,
              "blurred text shadows require selected glyph outlines; choose an embedded font instead of a built-in PDF face",
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
            "recompute text-shadow as two bounded offsets, an optional nonnegative blur and a color",
            module: __MODULE__,
            operation: :layout,
            source: source
          )
        end
    end
  end
end
