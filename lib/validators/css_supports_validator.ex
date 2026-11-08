defmodule NativeElixirPdfUtilities.Validators.CssSupportsValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.{
    HtmlValidator,
    CssSyntaxValidator,
    CssSelectorValidator,
    CssValueValidator
  }

  @doc "Evaluates static supports conditions through the owning declaration and selector validators."
  @spec matches?(String.t()) :: boolean()
  def matches?(source) do
    values =
      source
      |> CssSyntaxValidator.components()
      |> Enum.reject(&(&1.kind == :comment or Regex.match?(~r/\A[ \t\n\r\f]+\z/, &1.text)))

    condition(values, 0)
  end

  defp condition(values, depth) do
    HtmlValidator.check_render_resource(:max_css_syntax_depth, depth, :css)

    case values do
      [%{kind: :token, text: keyword}, term] ->
        keyword?(keyword, "not") and not term_matches?(term, depth + 1)

      [term] ->
        term_matches?(term, depth + 1)

      [term, %{kind: :token, text: keyword}, next | tail] ->
        operator =
          cond do
            keyword?(keyword, "and") -> :and
            keyword?(keyword, "or") -> :or
            true -> nil
          end

        if operator do
          {terms, valid} =
            Enum.chunk_every([next | tail], 2)
            |> Enum.reduce({[term], rem(length(values), 2) == 1}, fn chunk, {terms, valid} ->
              case chunk do
                [term, %{kind: :token, text: keyword}] ->
                  {[term | terms], valid and keyword?(keyword, Atom.to_string(operator))}

                [term] ->
                  {[term | terms], valid}

                _ ->
                  {terms, false}
              end
            end)

          valid and
            if(operator == :and,
              do: Enum.all?(terms, &term_matches?(&1, depth + 1)),
              else: Enum.any?(terms, &term_matches?(&1, depth + 1))
            )
        else
          false
        end

      _ ->
        false
    end
  end

  defp term_matches?(term, depth) do
    case term do
      %{kind: :block, opener: "(", closed: true} ->
        body = binary_part(term.text, 1, term.length - 2) |> HtmlValidator.trim_css_whitespace()
        declarations = CssSyntaxValidator.declarations(body)

        case declarations do
          [%{declaration: declaration, offset: 0, length: length}]
          when length == byte_size(body) ->
            property = elem(declaration, 0)

            if String.starts_with?(property, "--") do
              true
            else
              record = %{declaration: declaration, source: %{text: body, line: nil, column: nil}}
              budget = HtmlValidator.new_css_budget()

              result =
                if NativeElixirPdfUtilities.Validators.CssCascadeValidator.supported_property?(
                     property
                   ) do
                  case CssValueValidator.validate_source_literal(record, budget) do
                    :ok ->
                      {:ok, :literal}

                    :requires_element_context ->
                      CssValueValidator.prepare_unapplied_declaration(record, budget)

                    error ->
                      error
                  end
                else
                  :error
                end

              case result do
                {:ok, _} ->
                  true

                :requires_element_context ->
                  false

                {:error, {:resource_limit_exceeded, _}} = error ->
                  throw({:render_resource_limit, error})

                _ ->
                  false
              end
            end

          _ ->
            values =
              body
              |> CssSyntaxValidator.components()
              |> Enum.reject(
                &(&1.kind == :comment or Regex.match?(~r/\A[ \t\n\r\f]+\z/, &1.text))
              )

            condition(values, depth)
        end

      %{kind: :block, opener: opener, closed: true} ->
        case HtmlValidator.parse_css_identifier(opener) do
          {:ok, name, "("} ->
            if String.downcase(name, :ascii) == "selector" do
              selector =
                binary_part(term.text, byte_size(opener), term.length - byte_size(opener) - 1)

              match?({:ok, _}, CssSelectorValidator.parse(selector))
            else
              false
            end

          _ ->
            false
        end

      _ ->
        false
    end
  end

  defp keyword?(source, expected) do
    case HtmlValidator.parse_css_identifier(source) do
      {:ok, keyword, ""} -> String.downcase(keyword, :ascii) == expected
      _ -> false
    end
  end
end
