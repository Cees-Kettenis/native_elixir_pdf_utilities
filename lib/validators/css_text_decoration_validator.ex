defmodule NativeElixirPdfUtilities.Validators.CssTextDecorationValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.{CssLengthValidator, HtmlValidator}

  @doc "Lists inherited underline placement controls."
  @spec properties() :: [String.t()]
  def properties, do: ~w(text-underline-offset text-underline-position text-decoration-skip-ink)

  @doc "Returns initial underline placement values."
  @spec defaults() :: map()
  def defaults,
    do: %{
      text_underline_offset: :auto,
      text_underline_position: [:auto],
      text_decoration_skip_ink: :auto
    }

  @doc "Admits CSS Text Decoration underline offsets and side/alignment controls."
  @spec prepare(map(), String.t(), String.t()) ::
          {:ok, map()} | :error | {:error, {atom(), map()}}
  def prepare(style, property, value) do
    value = String.downcase(String.trim(value), :ascii)

    case property do
      "text-underline-offset" ->
        if value == "auto" do
          {:ok, Map.put(style, :text_underline_offset, :auto)}
        else
          with {:ok, value} <- CssLengthValidator.prepare(value, :margin),
               true <- value != :auto,
               {:ok, value} <- CssLengthValidator.resolve(value, nil, style),
               do: {:ok, Map.put(style, :text_underline_offset, value)}
        end

      "text-decoration-skip-ink" ->
        case value do
          "auto" -> {:ok, Map.put(style, :text_decoration_skip_ink, :auto)}
          "none" -> {:ok, Map.put(style, :text_decoration_skip_ink, :none)}
          "all" -> {:ok, Map.put(style, :text_decoration_skip_ink, :all)}
          _ -> :error
        end

      "text-underline-position" ->
        tokens = HtmlValidator.split_css_tokens(value)

        valid? =
          tokens in [
            ["auto"],
            ["from-font"],
            ["under"],
            ["left"],
            ["right"],
            ["under", "left"],
            ["under", "right"],
            ["left", "under"],
            ["right", "under"]
          ]

        if valid? do
          values =
            Enum.map(tokens, fn value ->
              case value do
                "from-font" -> :from_font
                "auto" -> :auto
                "under" -> :under
                "left" -> :left
                "right" -> :right
              end
            end)

          {:ok, Map.put(style, :text_underline_position, Enum.sort(values))}
        else
          :error
        end

      _ ->
        :error
    end
  end

  @doc "Checks prepared underline controls before any layout consumer uses them."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    offset = Map.get(style, :text_underline_offset, :auto)
    position = Map.get(style, :text_underline_position, [:auto])

    if Map.get(style, :text_decoration_skip_ink, :auto) in [:auto, :none, :all] and
         (offset == :auto or CssLengthValidator.valid_edge?(offset, :margin)) and
         position in [
           [:auto],
           [:from_font],
           [:under],
           [:left],
           [:right],
           [:left, :under],
           [:right, :under]
         ] do
      :ok
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "recompute underline offset and position styles",
        module: __MODULE__,
        operation: :layout,
        source: source
      )
    end
  end

  @doc "Resolves an admitted underline's near edge in its originating font and writing plane."
  @spec underline(map(), map(), number(), number()) :: {:ok, number()} | {:error, {atom(), map()}}
  def underline(style, metrics, scale, thickness) do
    position = Map.get(style, :text_underline_position, [:auto])
    gap = max(1, Float.ceil(thickness / 0.75 / 2)) * 0.75

    near =
      cond do
        :from_font in position and metrics.underline != nil -> metrics.underline.position * scale
        :under in position -> Map.get(style.font_face, :descent, 0) * scale - gap
        true -> -gap
      end

    offset =
      case Map.get(style, :text_underline_offset, :auto) do
        :auto ->
          {:ok, near}

        value ->
          with {:ok, length} <- CssLengthValidator.resolve(value, style.font_size, style),
               do: {:ok, if(:under in position, do: near - length, else: -length)}
      end

    with {:ok, near} <- offset do
      vertical = Map.get(style.font_face, :_vertical_text)
      right? = :right in position
      left? = :left in position

      reflect? =
        vertical != nil and
          ((vertical.mode in [:vertical_rl, :sideways_rl] and right?) or
             (vertical.mode in [:vertical_lr, :sideways_lr] and left?))

      {:ok, if(reflect?, do: -near + thickness, else: near)}
    end
  end
end
