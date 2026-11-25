defmodule NativeElixirPdfUtilities.Validators.CssTextBoxValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @trims %{"none" => :none, "trim-start" => :start, "trim-end" => :end, "trim-both" => :both}
  @over %{"text" => :text, "cap" => :cap, "ex" => :ex}
  @under %{"text" => :text, "alphabetic" => :alphabetic}

  @doc "Parses text-box, line-fit-edge and text-fit declarations into computed style fields."
  @spec prepare(String.t(), String.t()) :: {:ok, map()} | :error
  def prepare(property, value) do
    tokens = value |> String.downcase(:ascii) |> String.split()

    case {property, tokens} do
      {"text-box", ["normal"]} ->
        {:ok, %{text_box_trim: :none, text_box_edge: :auto}}

      {"text-box", tokens} ->
        {trims, edges} = Enum.split_with(tokens, &Map.has_key?(@trims, &1))

        with {:ok, trim} <- trim(trims, :both),
             {:ok, edge} <- edges(edges, :auto) do
          {:ok, %{text_box_trim: trim, text_box_edge: edge}}
        end

      {"text-box-trim", tokens} ->
        with {:ok, trim} <- trim(tokens, nil), do: {:ok, %{text_box_trim: trim}}

      {"text-box-edge", tokens} ->
        with {:ok, edge} <- edges(tokens, nil), do: {:ok, %{text_box_edge: edge}}

      {"line-fit-edge", ["leading"]} ->
        {:ok, %{line_fit_edge: :leading}}

      {"line-fit-edge", tokens} ->
        case edges(tokens, nil) do
          {:ok, {_, _} = edge} -> {:ok, %{line_fit_edge: edge}}
          _ -> :error
        end

      {"text-fit", [mode | tokens]} when mode in ~w(none grow shrink) ->
        with {:ok, method, cap} <- fit_options(tokens) do
          {:ok,
           %{
             text_fit:
               {%{"none" => :none, "grow" => :grow, "shrink" => :shrink}[mode], method, cap}
           }}
        end

      _ ->
        :error
    end
  end

  defp trim(tokens, default) do
    case tokens do
      [] when default != nil -> {:ok, default}
      [token] -> Map.fetch(@trims, token)
      _ -> :error
    end
  end

  defp edges(tokens, default) do
    case tokens do
      [] when default != nil ->
        {:ok, default}

      ["auto"] ->
        {:ok, :auto}

      [above] ->
        edges([above, "text"], default)

      [above, below] ->
        with {:ok, above} <- Map.fetch(@over, above),
             {:ok, below} <- Map.fetch(@under, below),
             do: {:ok, {above, below}}

      _ ->
        :error
    end
  end

  defp fit_options(tokens) do
    case tokens do
      ["consistent" | rest] -> fit_limit(rest, :consistent)
      ["per-line" | rest] -> fit_limit(rest, :per_line)
      ["per-line-all" | rest] -> fit_limit(rest, :per_line_all)
      rest -> fit_limit(rest, :consistent)
    end
  end

  defp fit_limit(tokens, method) do
    case tokens do
      [] ->
        {:ok, method, nil}

      [limit] ->
        case HtmlValidator.parse_css_number(limit) do
          {value, "%"} -> {:ok, method, value / 100}
          _ -> :error
        end

      _ ->
        :error
    end
  end
end
