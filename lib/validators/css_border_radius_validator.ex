defmodule NativeElixirPdfUtilities.Validators.CssBorderRadiusValidator do
  @moduledoc "Physical elliptical corner radii, computed units and overlap reduction."
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, CssLengthValidator, CssMathValidator}

  @corners %{
    "border-top-left-radius" => :top_left,
    "border-top-right-radius" => :top_right,
    "border-bottom-right-radius" => :bottom_right,
    "border-bottom-left-radius" => :bottom_left
  }

  @doc "Lists physical corner longhands and their radius shorthand."
  @spec properties() :: [String.t()]
  def properties, do: ["border-radius" | Map.keys(@corners)]

  @doc "Prepares independent horizontal and vertical corner radii through shared length validators."
  @spec prepare(map(), String.t(), String.t()) :: {:ok, map()} | :error | {:error, term()}
  def prepare(style, property, source) do
    if property == "border-radius" do
      tokens = CssMathValidator.component_terms(source, true)

      {horizontal, vertical} =
        case Enum.split_while(tokens, &(&1 != "/")) do
          {one, []} -> {one, one}
          {one, ["/" | two]} -> {one, two}
        end

      with {:ok, x} <-
             HtmlValidator.parse_css_box_lengths(
               Enum.join(horizontal, " "),
               {:computed, :padding}
             ),
           {:ok, y} <-
             HtmlValidator.parse_css_box_lengths(
               Enum.join(vertical, " "),
               {:computed, :padding}
             ) do
        radii = %{
          top_left: {x.top, y.top},
          top_right: {x.right, y.right},
          bottom_right: {x.bottom, y.bottom},
          bottom_left: {x.left, y.left}
        }

        {:ok, Map.put(style, :border_radii, radii)}
      end
    else
      with {:ok, corner} <- Map.fetch(@corners, property),
           tokens = HtmlValidator.split_css_tokens(source),
           true <- length(tokens) in [1, 2],
           {:ok, x} <- CssLengthValidator.prepare(hd(tokens), :padding),
           {:ok, y} <- CssLengthValidator.prepare(List.last(tokens), :padding) do
        base =
          Map.get_lazy(style, :border_radii, fn ->
            Map.new(
              Map.values(@corners),
              &{&1, {Map.get(style, :border_radius, 0.0), Map.get(style, :border_radius, 0.0)}}
            )
          end)

        {:ok, Map.put(style, :border_radii, Map.put(base, corner, {x, y}))}
      else
        _ -> :error
      end
    end
  end

  @doc "Freezes corner lengths at the declaring font and retains the legacy uniform-circle value."
  @spec compute_style(map()) :: {:ok, map()} | :error | {:error, term()}
  def compute_style(style) do
    case Map.fetch(style, :border_radii) do
      :error ->
        {:ok, style}

      {:ok, radii} ->
        with {:ok, radii} <- resolve(radii, nil, nil, style) do
          radius =
            case Enum.uniq(Map.values(radii)) do
              [{x, y}] when is_number(x) and x == y -> x
              _ -> 0.0
            end

          {:ok, style |> Map.put(:border_radii, radii) |> Map.put(:border_radius, radius)}
        end
    end
  end

  @doc "Checks externally supplied corner calculations at the styled-tree boundary."
  @spec validate_style(map(), String.t()) :: :ok | {:error, term()}
  def validate_style(style, source) do
    valid? =
      case Map.fetch(style, :border_radii) do
        :error ->
          true

        {:ok, radii} when is_map(radii) ->
          Enum.sort(Map.keys(radii)) == [:bottom_left, :bottom_right, :top_left, :top_right] and
            Enum.all?(radii, fn {_corner, pair} ->
              case pair do
                {x, y} ->
                  Enum.all?([x, y], fn n ->
                    n != :auto and CssLengthValidator.valid_edge?(n, :padding)
                  end)

                _ ->
                  false
              end
            end)

        _ ->
          false
      end

    if valid?,
      do: :ok,
      else:
        NativeElixirPdfUtilities.Diagnostics.error(
          :layout,
          :invalid_layout,
          "border radii require all four nonnegative length or percentage pairs",
          operation: :layout,
          module: __MODULE__,
          source: source
        )
  end

  @doc "Resolves percentages against their own border-box axes and reduces overlapping corners by one common factor."
  @spec used_radii(map(), number(), number()) :: {:ok, map() | nil} | :error | {:error, term()}
  def used_radii(style, width, height) do
    case Map.fetch(style, :border_radii) do
      :error ->
        {:ok, nil}

      {:ok, radii} ->
        case Enum.uniq(Map.values(radii)) do
          [{x, y}] when is_number(x) and x == y ->
            {:ok, nil}

          _ ->
            with {:ok, radii} <- resolve(radii, width, height, style) do
              {tlx, tly} = radii.top_left
              {trx, try_} = radii.top_right
              {blx, bly} = radii.bottom_left
              {brx, bry} = radii.bottom_right

              factor =
                Enum.reduce(
                  [
                    {width, tlx + trx},
                    {width, blx + brx},
                    {height, tly + bly},
                    {height, try_ + bry}
                  ],
                  1.0,
                  fn {extent, total}, factor ->
                    if total > 0, do: min(factor, extent / total), else: factor
                  end
                )

              {:ok, Map.new(radii, fn {corner, {x, y}} -> {corner, {x * factor, y * factor}} end)}
            end
        end
    end
  end

  defp resolve(radii, width, height, style) do
    Enum.reduce_while(radii, {:ok, %{}}, fn {corner, {x, y}}, {:ok, out} ->
      with {:ok, x} <- CssLengthValidator.resolve(x, width, style),
           {:ok, y} <- CssLengthValidator.resolve(y, height, style),
           do: {:cont, {:ok, Map.put(out, corner, {x, y})}},
           else: (error -> {:halt, error})
    end)
  end
end
