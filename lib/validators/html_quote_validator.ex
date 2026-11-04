defmodule NativeElixirPdfUtilities.Validators.HtmlQuoteValidator do
  @moduledoc "Validates CSS quotation pairs and resolves authored quote generation."

  alias NativeElixirPdfUtilities.HtmlToPdf.QuotationData
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, LanguageValidator}

  @string_sequence Regex.compile!("\\s*(" <> HtmlValidator.css_string_pattern() <> ")", "u")

  @doc "Parses inherited quote keywords or complete opening/closing CSS string pairs."
  @spec parse(String.t(), map()) :: {:ok, term()} | :error
  def parse(value, style) do
    source = HtmlValidator.trim_css_whitespace(value)

    case String.downcase(source, :ascii) do
      "none" ->
        {:ok, []}

      keyword when keyword in ["auto", "initial"] ->
        {:ok, :auto}

      keyword when keyword in ["inherit", "unset", "revert", "revert-layer"] ->
        {:ok, Map.get(style, :_inherited_quotes, :auto)}

      _ ->
        tokens = Regex.scan(@string_sequence, source)
        reconstructed = tokens |> Enum.map_join("", &hd/1) |> HtmlValidator.trim_css_whitespace()

        if tokens != [] and rem(length(tokens), 2) == 0 and reconstructed == source do
          {:ok,
           tokens
           |> Enum.map(fn [_, token] -> HtmlValidator.decode_css_string(token) end)
           |> Enum.chunk_every(2)
           |> Enum.map(&List.to_tuple/1)}
        else
          :error
        end
    end
  end

  @doc "Resolves automatic quote pairs using the element's authored inherited language."
  @spec pairs(map(), map(), [map()]) :: [{String.t(), String.t()}]
  def pairs(style, node, ancestors) do
    case Map.get(style, :quotes, :auto) do
      :auto -> QuotationData.pairs(LanguageValidator.resolved_language(node, ancestors))
      pairs -> pairs
    end
  end

  @doc "Produces one quote token and the next document-wide nesting depth."
  @spec token(atom(), [{String.t(), String.t()}], non_neg_integer()) ::
          {String.t(), non_neg_integer()}
  def token(kind, pairs, depth) do
    next =
      case kind do
        kind when kind in [:open_quote, :no_open_quote] -> depth + 1
        _ -> max(depth - 1, 0)
      end

    index = if kind == :open_quote, do: depth, else: next

    text =
      if pairs == [] or kind in [:no_open_quote, :no_close_quote] or
           (kind == :close_quote and depth == 0) do
        ""
      else
        pair = Enum.at(pairs, min(index, length(pairs) - 1))
        elem(pair, if(kind == :open_quote, do: 0, else: 1))
      end

    {text, next}
  end
end
