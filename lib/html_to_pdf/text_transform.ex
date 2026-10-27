defmodule NativeElixirPdfUtilities.HtmlToPdf.TextTransform do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeCharacterProperty

  @word ~r/^[\p{L}\p{N}_]$/u
  @mark ~r/^\p{M}$/u
  @word_ignorable ~r/^[\p{M}\p{Cf}'’]$/u
  @cased Regex.compile!(UnicodeCharacterProperty.expression("Cased", false), "u")
  @ignorable Regex.compile!(UnicodeCharacterProperty.expression("Case_Ignorable", false), "u")

  @doc "Scopes deferred case conversion to one styled document, including nested renders."
  @spec with_document((-> result)) :: result when result: term()
  def with_document(fun) do
    key = {__MODULE__, :pending}
    previous = Process.get(key)
    Process.put(key, false)

    try do
      case fun.() do
        {:ok, tree} ->
          {:ok, if(Process.get(key), do: apply_tree(tree), else: tree)}

        error ->
          error
      end
    after
      if previous == nil, do: Process.delete(key), else: Process.put(key, previous)
    end
  end

  @doc "Retains a text node's ownership and language until inline context is available."
  @spec defer(map(), [String.t()]) :: map()
  def defer(node, language) do
    Process.put({__MODULE__, :pending}, true)
    Map.put(node, :_case_language, List.first(language))
  end

  @doc "Resolves an independent text context before its owner's intrinsic measurement."
  @spec resolve_children([map()]) :: [map()]
  def resolve_children(children) do
    if Process.get({__MODULE__, :pending}) do
      %{children: children} = apply_tree(%{type: :document, children: children})
      children
    else
      children
    end
  end

  @doc "Converts one independent string using the known language's case mapping."
  @spec convert(String.t(), atom(), [String.t()]) :: String.t()
  def convert(text, transform, language) do
    mode = if List.first(language) in ["tr", "az"], do: :turkic, else: :default

    case transform do
      :uppercase ->
        String.upcase(text, mode)

      :lowercase ->
        String.downcase(text, if(mode == :turkic, do: mode, else: :greek))

      :capitalize ->
        Regex.replace(~r/\b\p{L}/u, text, fn char ->
          if mode == :turkic, do: String.upcase(char, mode), else: String.capitalize(char)
        end)

      _ ->
        text
    end
  end

  defp apply_tree(tree) do
    {tree, {_id, events}} = collect(tree, {0, []}, true)

    # Right-to-left context makes final sigma and decomposed Turkic dotted I
    # independent of authored inline boundaries. The forward pass also carries
    # word context through nodes whose own transform is none.
    {tokens, _following} =
      Enum.reduce(events, {[], {false, false}}, fn event, acc ->
        case event do
          :boundary ->
            {tokens, _following} = acc
            {[:boundary | tokens], {false, false}}

          {id, text, transform, language} ->
            HtmlValidator.reserve_render_resource(:max_css_work, byte_size(text), :style)

            text
            |> String.codepoints()
            |> Enum.reverse()
            |> Enum.reduce(acc, fn char, {tokens, {next_cased, next_dot}} ->
              token = {id, char, transform, language, next_cased, next_dot}

              cased =
                Regex.match?(@cased, char) or
                  (Regex.match?(@ignorable, char) and next_cased)

              dot = if Regex.match?(@mark, char), do: char == "̇" or next_dot, else: false
              {[token | tokens], {cased, dot}}
            end)
        end
      end)

    {replacements, _context} =
      Enum.reduce(tokens, {%{}, {false, false, nil}}, fn token, {texts, context} ->
        case token do
          :boundary ->
            {texts, {false, false, nil}}

          {id, char, transform, language, next_cased, next_dot} ->
            {word, before_cased, base} = context
            turkic = language in ["tr", "az"]

            mapped =
              cond do
                transform == :capitalize and not word and Regex.match?(@word, char) ->
                  if(turkic, do: String.upcase(char, :turkic), else: String.capitalize(char))

                transform == :lowercase and turkic and char == "I" ->
                  if next_dot, do: "i", else: "ı"

                transform == :lowercase and turkic and char == "̇" and base == "I" ->
                  ""

                transform == :lowercase and char == "Σ" and before_cased and not next_cased ->
                  "ς"

                transform == :lowercase ->
                  String.downcase(char, if(turkic, do: :turkic, else: :default))

                true ->
                  char
              end

            HtmlValidator.reserve_render_resource(
              :max_rendered_text_bytes,
              max(byte_size(mapped) - byte_size(char), 0),
              :style
            )

            mark = Regex.match?(@mark, char)
            word = Regex.match?(@word, char) or (Regex.match?(@word_ignorable, char) and word)

            cased =
              Regex.match?(@cased, char) or
                (Regex.match?(@ignorable, char) and before_cased)

            base = if mark, do: base, else: char
            {Map.update(texts, id, [mapped], &[mapped | &1]), {word, cased, base}}
        end
      end)

    replace(tree, replacements)
  end

  defp collect(node, {id, events}, root) do
    case node do
      %{type: :text, text: text} ->
        pending = Map.has_key?(node, :_case_language)
        transform = if pending, do: node.style.text_transform, else: :none
        event = {id, text, transform, Map.get(node, :_case_language)}
        {Map.put(node, :_case_id, id), {id + 1, [event | events]}}

      %{children: children} ->
        display = get_in(node, [:style, :display])

        cond do
          display == :none ->
            {node, {id, events}}

          not root and get_in(node, [:style, :position]) == :absolute ->
            {Map.put(apply_tree(node), :_case_island, true), {id, events}}

          Map.get(node, :namespace) == :svg or
              Map.has_key?(Map.get(node, :style, %{}), :_math_box) ->
            {Map.put(node, :_case_island, true), {id, [:boundary | events]}}

          true ->
            boundary = display not in [nil, :inline, :contents, :soft_break]
            events = if boundary, do: [:boundary | events], else: events

            {children, {id, events}} =
              Enum.map_reduce(children, {id, events}, &collect(&1, &2, false))

            events = if boundary, do: [:boundary | events], else: events
            {%{node | children: children}, {id, events}}
        end
    end
  end

  defp replace(node, texts) do
    case node do
      %{_case_island: true} ->
        Map.delete(node, :_case_island)

      %{_case_id: id} ->
        text = texts |> Map.get(id, []) |> Enum.reverse() |> IO.iodata_to_binary()
        node |> Map.drop([:_case_id, :_case_language]) |> Map.put(:text, text)

      %{children: children} ->
        %{node | children: Enum.map(children, &replace(&1, texts))}
    end
  end
end
