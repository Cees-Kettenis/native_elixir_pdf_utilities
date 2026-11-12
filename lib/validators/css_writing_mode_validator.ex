defmodule NativeElixirPdfUtilities.Validators.CssWritingModeValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @doc "Lists the inline orientation and combination declarations."
  @spec properties() :: [String.t()]
  def properties, do: ~w(text-orientation text-combine-upright)

  @doc "Returns initial inline orientation and combination values."
  @spec defaults() :: map()
  def defaults, do: %{text_orientation: :mixed, text_combine_upright: :none}

  @doc "Admits orientation identifiers and the fixed CSS two-to-four digit combination grammar."
  @spec prepare(map(), String.t(), String.t()) :: {:ok, map()} | :error
  def prepare(style, property, value) do
    tokens = HtmlValidator.split_css_tokens(String.downcase(String.trim(value), :ascii))

    result =
      case {property, tokens} do
        {"text-orientation", [value]} when value in ~w(mixed upright sideways) ->
          {:text_orientation, String.to_existing_atom(value)}

        {"text-combine-upright", ["none"]} ->
          {:text_combine_upright, :none}

        {"text-combine-upright", ["all"]} ->
          {:text_combine_upright, :all}

        {"text-combine-upright", ["digits"]} ->
          {:text_combine_upright, {:digits, 2}}

        {"text-combine-upright", ["digits", count]} when count in ~w(2 3 4) ->
          {:text_combine_upright, {:digits, String.to_integer(count)}}

        _ ->
          nil
      end

    if result == nil, do: :error, else: {:ok, Map.put(style, elem(result, 0), elem(result, 1))}
  end

  @doc "Checks computed orientation metadata before layout consumes it."
  @spec validate_style(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_style(style, source) do
    if Map.get(style, :text_orientation, :mixed) in [:mixed, :upright, :sideways] and
         Map.get(style, :text_combine_upright, :none) in [
           :none,
           :all,
           {:digits, 2},
           {:digits, 3},
           {:digits, 4}
         ] do
      :ok
    else
      Diagnostics.error(
        :layout,
        :invalid_layout,
        "recompute text orientation and combination styles",
        module: __MODULE__,
        operation: :layout,
        source: source
      )
    end
  end

  @doc "Checks the glyph's admitted two-dimensional paint basis."
  @spec valid_paint?(map()) :: boolean()
  def valid_paint?(box) do
    case Map.fetch(box, :glyph_basis) do
      :error ->
        true

      {:ok, {a, b, c, d}} ->
        Enum.all?(
          [a, b, c, d],
          &(is_number(&1) and
              abs(&1) <= NativeElixirPdfUtilities.Limits.get(:max_numeric_magnitude))
        ) and a * d - b * c != 0

      _ ->
        false
    end
  end
end
