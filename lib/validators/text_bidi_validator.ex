defmodule NativeElixirPdfUtilities.Validators.TextBidiValidator do
  @moduledoc false
  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.Validators.TextItemizationValidator

  @css_values Map.new(
                ~w(normal embed isolate bidi-override isolate-override plaintext),
                &{&1, String.to_atom(String.replace(&1, "-", "_"))}
              )

  @doc "Admits the fixed unicode-bidi keyword vocabulary through the shared CSS identifier parser."
  @spec prepare_css(binary()) :: {:ok, atom()} | :error
  def prepare_css(value) do
    with {:ok, identifier, ""} <-
           NativeElixirPdfUtilities.Validators.HtmlValidator.parse_css_identifier(value),
         {:ok, value} <- Map.fetch(@css_values, String.downcase(identifier, :ascii)),
         do: {:ok, value},
         else: (_ -> :error)
  end

  @doc "Carries inline bidi scopes and resolved text language through inherited text styles."
  @spec prepare_context(map(), map(), map(), [map()]) :: map()
  def prepare_context(style, parent, node, ancestors) do
    contexts =
      if :maps.get(:display, style, nil) in [
           :inline,
           :contents,
           :inline_block,
           :inline_flex,
           :inline_grid,
           :inline_table
         ], do: :maps.get(:_bidi_contexts, parent, []), else: []

    mode = :maps.get(:unicode_bidi, style, :normal)

    contexts =
      if mode != :normal and :maps.get(:display, style, nil) == :inline,
        do:
          contexts ++
            [%{id: node._selector_id, mode: mode, direction: :maps.get(:direction, style, :ltr)}],
        else: contexts

    style =
      if contexts == [],
        do: Map.delete(style, :_bidi_contexts),
        else: Map.put(style, :_bidi_contexts, contexts)

    language =
      NativeElixirPdfUtilities.Validators.LanguageValidator.resolved_language(node, ancestors)

    Map.put(style, :_text_language, Enum.join(language, "-"))
  end

  @doc "Detects inline paragraphs that require Unicode level resolution."
  @spec required?([map()], map()) :: boolean()
  def required?(runs, style) do
    :maps.get(:direction, style, :ltr) == :rtl or
      :maps.get(:unicode_bidi, style, :normal) != :normal or
      Enum.any?(runs, fn run ->
        :maps.get(:_bidi_contexts, run.style, []) != [] or directional_text?(run.text)
      end)
  end

  @doc "Finds HTML auto direction from strong text outside Unicode isolate contents."
  @spec first_strong_direction(binary()) :: binary() | nil
  def first_strong_direction(text), do: first_strong(text, 0)

  @doc "Checks optional writer direction and per-scalar resolved levels against Unicode's fixed level bound."
  @spec valid_paint?(map()) :: boolean()
  def valid_paint?(box) do
    :maps.get(:bidi_direction, box, :auto) in [:auto, :ltr, :rtl] and
      case Map.fetch(box, :bidi_levels) do
        :error ->
          true

        {:ok, levels} when is_list(levels) ->
          length(levels) == length(String.codepoints(box.text)) and
            Enum.all?(levels, &(is_integer(&1) and &1 in 0..126))

        _ ->
          false
      end
  end

  defp first_strong(text, depth) do
    case text do
      "" ->
        nil

      <<codepoint::utf8, remaining::binary>> ->
        type = elem(NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData.properties(codepoint), 1)

        cond do
          type in [:LRI, :RLI, :FSI] -> first_strong(remaining, depth + 1)
          type == :PDI -> first_strong(remaining, max(0, depth - 1))
          type == :B -> first_strong(remaining, 0)
          depth != 0 -> first_strong(remaining, depth)
          type == :L -> "ltr"
          type in [:R, :AL] -> "rtl"
          true -> first_strong(remaining, depth)
        end
    end
  end

  defp directional_text?(text) do
    case text do
      "" ->
        false

      <<bytes::64, remaining::binary>> when :erlang.band(bytes, 0x8080808080808080) == 0 ->
        directional_text?(remaining)

      <<byte, remaining::binary>> when byte < 128 ->
        directional_text?(remaining)

      <<codepoint::utf8, remaining::binary>> ->
        type = elem(NativeElixirPdfUtilities.HtmlToPdf.UnicodeTextData.properties(codepoint), 1)

        type in [:R, :AL, :RLE, :LRE, :RLO, :LRO, :PDF, :LRI, :RLI, :FSI, :PDI] or
          directional_text?(remaining)
    end
  end

  @doc "Admits paragraph direction and shares scalar/source validation with itemization."
  @spec prepare(term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def prepare(text, direction) do
    if direction in [:auto, :ltr, :rtl] do
      TextItemizationValidator.prepare(text)
    else
      Diagnostics.error(:font, :invalid_text, "paragraph direction must be :auto, :ltr or :rtl",
        operation: :resolve_bidi,
        module: __MODULE__
      )
    end
  end

  @doc "Admits an inclusive/exclusive scalar range within a resolved paragraph."
  @spec validate_line(term(), term(), non_neg_integer()) :: :ok | {:error, {atom(), map()}}
  def validate_line(first, last, count) do
    if is_integer(first) and is_integer(last) and first >= 0 and first <= last and last <= count do
      :ok
    else
      Diagnostics.error(
        :font,
        :invalid_text,
        "bidi line boundaries must be ordered scalar offsets within the paragraph",
        operation: :reorder_bidi_line,
        module: __MODULE__
      )
    end
  end
end
