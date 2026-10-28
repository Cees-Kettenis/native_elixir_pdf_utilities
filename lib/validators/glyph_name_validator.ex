defmodule NativeElixirPdfUtilities.Validators.GlyphNameValidator do
  @moduledoc false

  # Digit widths and scalar ranges are fixed by Adobe's glyph-name mapping rules.
  @unicode_hex ~r/\A[0-9A-F]{4,6}\z/

  @doc "Resolves known glyph names and validates Adobe Unicode-name forms for strict extraction."
  @spec prepare(term(), %{optional(binary()) => String.t()}) :: {:ok, String.t()} | :error
  def prepare(name, glyphs) do
    case name do
      name when is_binary(name) ->
        if String.valid?(name) do
          base = name |> String.split(".", parts: 2) |> hd()

          case Map.get(glyphs, base) do
            text when is_binary(text) -> {:ok, text}
            nil -> derived_glyph(base, glyphs)
          end
        else
          :error
        end

      _ ->
        :error
    end
  end

  defp derived_glyph(name, glyphs) do
    parts = String.split(name, "_")

    if length(parts) > 1 do
      parts
      |> Enum.reduce_while({:ok, []}, fn part, {:ok, characters} ->
        case prepare(part, glyphs) do
          {:ok, character} -> {:cont, {:ok, [character | characters]}}
          :error -> {:halt, :error}
        end
      end)
      |> case do
        {:ok, characters} -> {:ok, characters |> Enum.reverse() |> Enum.join()}
        :error -> :error
      end
    else
      case name do
        <<"uni", hex::binary>> -> unicode_chunks(hex)
        <<"u", hex::binary>> -> unicode_scalar(hex)
        _ -> :error
      end
    end
  end

  defp unicode_chunks(hex) do
    if byte_size(hex) > 0 and rem(byte_size(hex), 4) == 0 do
      for(<<chunk::binary-size(4) <- hex>>, do: chunk)
      |> Enum.reduce_while({:ok, []}, fn chunk, {:ok, characters} ->
        case unicode_scalar(chunk) do
          {:ok, character} -> {:cont, {:ok, [character | characters]}}
          :error -> {:halt, :error}
        end
      end)
      |> case do
        {:ok, characters} -> {:ok, characters |> Enum.reverse() |> Enum.join()}
        :error -> :error
      end
    else
      :error
    end
  end

  defp unicode_scalar(hex) do
    if Regex.match?(@unicode_hex, hex) do
      value = String.to_integer(hex, 16)

      if value <= 0x10FFFF and value not in 0xD800..0xDFFF do
        {:ok, <<value::utf8>>}
      else
        :error
      end
    else
      :error
    end
  end
end
