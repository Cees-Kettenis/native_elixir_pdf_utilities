defmodule NativeElixirPdfUtilities.HtmlToPdf.FontFallback do
  @moduledoc """
  Resolves styled text graphemes to fonts before layout and PDF writing.

  The selected CSS face is tried first, followed by the remaining requested
  families, configured font faces, and the bundled fallback faces. Adjacent
  graphemes using the same face remain one text node so layout can measure and
  wrap the final font runs.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type styled_tree :: NativeElixirPdfUtilities.HtmlToPdf.Style.styled_tree()
  @type unsupported_glyphs :: :replace | :error
  @type error_reason ::
          :invalid_document | :invalid_encoding | :invalid_options | :unsupported_glyph
  @replacement_character "\uFFFD"

  @doc """
  Resolves every styled text node to font faces that contain its graphemes.

  Unsupported graphemes are replaced with U+FFFD by default. Pass `:error` as
  the second argument to return an `:unsupported_glyph` diagnostic instead.
  """
  @spec resolve(styled_tree(), unsupported_glyphs()) ::
          {:ok, styled_tree()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def resolve(styled_tree, unsupported_glyphs \\ :replace) do
    with :ok <- HtmlValidator.validate_font_fallback_input(styled_tree, unsupported_glyphs),
         prepared <- prepare_candidates(styled_tree),
         :ok <-
           HtmlValidator.validate_font_coverage(
             prepared,
             unsupported_glyphs,
             @replacement_character
           ) do
      {:ok,
       %{
         prepared
         | children: resolve_prepared_nodes(prepared.children, unsupported_glyphs)
       }}
    else
      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic, operation: :resolve_fonts, module: __MODULE__)}}
    end
  end

  defp prepare_candidates(%{type: :document, children: children} = document) do
    {children, _cache} = Enum.map_reduce(children, {nil, %{}}, &prepare_candidate_node/2)
    %{document | children: children}
  end

  defp prepare_candidate_node(node, {cached_registry, cached_candidates} = cache) do
    case node do
      %{type: :text, style: style} ->
        registry = Map.fetch!(style, :_font_registry)
        selected = Map.fetch!(style, :font_face)
        families = Map.fetch!(style, :font_families)
        weight = Map.fetch!(style, :font_weight)
        font_style = Map.fetch!(style, :font_style)

        # Keep the registry out of map keys: hashing its font payloads per node
        # would cost more than the candidate lookup being cached.
        cached_candidates = if registry == cached_registry, do: cached_candidates, else: %{}
        key = {families, weight, font_style}

        {candidates, cached_candidates} =
          case Map.fetch(cached_candidates, key) do
            {:ok, {previous_selected, candidates}} when previous_selected == selected ->
              {candidates, cached_candidates}

            _ ->
              requested = Font.requested_faces(families, weight, font_style, registry)

              candidates =
                [selected | requested ++ Font.fallback_faces(registry, weight, font_style)]
                |> Enum.uniq_by(&Font.pdf_name/1)

              {candidates, Map.put(cached_candidates, key, {selected, candidates})}
          end

        graphemes =
          node.text
          |> String.replace("\r\n", "\n")
          |> String.replace("\r", "\n")
          |> String.graphemes()
          |> Enum.map(fn grapheme ->
            layout_whitespace? =
              grapheme
              |> String.to_charlist()
              |> Enum.all?(&(&1 in [9, 10, 13]))

            %{text: grapheme, layout_whitespace?: layout_whitespace?}
          end)

        prepared =
          node
          |> Map.put(:_font_candidates, candidates)
          |> Map.put(:_font_graphemes, graphemes)

        {prepared, {registry, cached_candidates}}

      %{type: :element, children: children} = element ->
        {children, cache} = Enum.map_reduce(children, cache, &prepare_candidate_node/2)
        {%{element | children: children}, cache}
    end
  end

  defp resolve_prepared_nodes(nodes, unsupported_glyphs) do
    Enum.flat_map(nodes, &resolve_prepared_node(&1, unsupported_glyphs))
  end

  defp resolve_prepared_node(node, unsupported_glyphs) do
    case node do
      %{
        type: :text,
        style: style,
        _font_candidates: candidates,
        _font_graphemes: graphemes
      } ->
        node = Map.drop(node, [:_font_candidates, :_font_graphemes])

        resolved =
          Enum.reduce(graphemes, [], fn grapheme, runs ->
            {font_face, text} =
              case grapheme.layout_whitespace? do
                true ->
                  {List.first(candidates), grapheme.text}

                false ->
                  case Enum.find(candidates, &Font.supports_text?(&1, grapheme.text)) do
                    nil when unsupported_glyphs == :replace ->
                      replacement_face =
                        Enum.find(candidates, &Font.supports_text?(&1, @replacement_character))

                      {replacement_face, @replacement_character}

                    font_face ->
                      {font_face, grapheme.text}
                  end
              end

            append_run(runs, node, style, font_face, text)
          end)

        Enum.reverse(resolved)

      %{type: :element, children: children} = element when is_list(children) ->
        [%{element | children: resolve_prepared_nodes(children, unsupported_glyphs)}]
    end
  end

  defp append_run(runs, node, style, font_face, grapheme) do
    case runs do
      [%{style: %{font_face: previous_face}} = previous | remaining]
      when previous_face == font_face ->
        [%{previous | text: previous.text <> grapheme} | remaining]

      _ ->
        resolved_style =
          style
          |> Map.put(:font_face, font_face)
          |> Map.put(:font_family, font_face.family)

        [%{node | text: grapheme, style: resolved_style} | runs]
    end
  end
end
