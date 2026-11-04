defmodule NativeElixirPdfUtilities.Validators.CssReferenceValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    CssValueValidator,
    HtmlCssSourceValidator,
    HtmlValidator
  }

  @doc false
  @spec scan(String.t()) :: {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  @spec scan(String.t(), (non_neg_integer(), non_neg_integer() -> map()) | nil) ::
          {:ok, [map()]} | {:error, {atom(), Diagnostics.diagnostic()}}
  def scan(source, mapper \\ nil) do
    HtmlValidator.with_render_budget(fn ->
      with {:ok, source} <- HtmlValidator.validate_css_source(source, :declarations),
           :ok <-
             if(is_nil(mapper) or is_function(mapper, 2),
               do: :ok,
               else:
                 Diagnostics.error(
                   :css,
                   :invalid_options,
                   "source reference mapper must accept byte offset and length",
                   module: __MODULE__,
                   source: "source_mapper"
                 )
             ) do
        HtmlValidator.reserve_render_resource(:max_css_work, 4 * byte_size(source), :css)

        {units, _finish} =
          source
          |> HtmlValidator.css_syntax_units()
          |> Enum.map_reduce(0, fn unit, offset ->
            {{unit, offset}, offset + byte_size(unit)}
          end)

        mapper =
          mapper ||
            fn offset, length ->
              HtmlCssSourceValidator.map_span(
                HtmlCssSourceValidator.local(source),
                offset,
                length
              )
            end

        scan_units(units, source, nil, [], mapper)
      end
    end)
  end

  # Strings are opaque. Identifier decoding is shared with selector and property
  # parsing, so escaped URL function names cannot evade static source policy.
  defp scan_units(units, source, quote, references, mapper) do
    case units do
      [] ->
        {:ok, Enum.reverse(references)}

      [{unit, offset} | rest] ->
        cond do
          unit == quote ->
            scan_units(rest, source, nil, references, mapper)

          not is_nil(quote) ->
            scan_units(rest, source, quote, references, mapper)

          unit in ["'", "\""] ->
            scan_units(rest, source, unit, references, mapper)

          true ->
            remaining = binary_part(source, offset, byte_size(source) - offset)

            case HtmlValidator.parse_css_identifier(remaining) do
              {:ok, name, tail} ->
                name_length = byte_size(remaining) - byte_size(tail)
                finish = offset + name_length
                following = Enum.drop_while(units, fn {_unit, at} -> at < finish end)

                after_space =
                  Enum.drop_while(following, fn {unit, _} ->
                    unit in [" ", "\t", "\n", "\r", "\f"]
                  end)

                case {String.downcase(name, :ascii), after_space} do
                  {"url", [{"(", _opening} | content]} ->
                    case function_end(content, 1, nil) do
                      {:ok, end_offset, rest} ->
                        length = end_offset - offset
                        original = binary_part(source, offset, length)
                        suffix = binary_part(source, finish, end_offset - finish)

                        case CssValueValidator.validate_source_reference("url" <> suffix) do
                          {:ok, decoded} ->
                            reference = %{
                              offset: offset,
                              length: length,
                              decoded: decoded,
                              policy: :static_url_syntax_only,
                              authorization: :unresolved,
                              contents: :unresolved
                            }

                            scan_units(rest, source, nil, [reference | references], mapper)

                          failure ->
                            reference_error(original, offset, failure, mapper)
                        end

                      :error ->
                        reference_error(
                          binary_part(source, offset, byte_size(source) - offset),
                          offset,
                          :invalid_syntax,
                          mapper
                        )
                    end

                  _ ->
                    scan_units(following, source, nil, references, mapper)
                end

              :error ->
                scan_units(rest, source, nil, references, mapper)
            end
        end
    end
  end

  defp function_end(units, depth, quote) do
    case units do
      [] ->
        :error

      [{unit, offset} | rest] ->
        cond do
          unit == quote -> function_end(rest, depth, nil)
          not is_nil(quote) -> function_end(rest, depth, quote)
          unit in ["'", "\""] -> function_end(rest, depth, unit)
          unit == "(" -> function_end(rest, depth + 1, nil)
          unit == ")" and depth == 1 -> {:ok, offset + 1, rest}
          unit == ")" -> function_end(rest, depth - 1, nil)
          true -> function_end(rest, depth, nil)
        end
    end
  end

  defp reference_error(source, offset, failure, mapper) do
    {reason, message} =
      case failure do
        {:error, {reason, diagnostic}} -> {reason, diagnostic.message}
        _ -> {:invalid_css, "authored CSS url() reference is malformed or unsupported"}
      end

    original = mapper.(offset, byte_size(source))

    Diagnostics.error(:css, reason, message,
      module: __MODULE__,
      operation: :source_references,
      source: original.text,
      line: original.line,
      column: original.column
    )
  end
end
