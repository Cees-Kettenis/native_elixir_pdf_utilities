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
    %{document | children: Enum.map(children, &prepare_candidate_node/1)}
  end

  defp prepare_candidate_node(node) do
    case node do
      %{type: :text, style: style} ->
        registry = Map.fetch!(style, :_font_registry)
        selected = Map.fetch!(style, :font_face)
        families = Map.fetch!(style, :font_families)
        weight = Map.fetch!(style, :font_weight)
        font_style = Map.fetch!(style, :font_style)

        requested =
          Enum.flat_map(families, fn family ->
            case Font.resolve([family], weight, font_style, registry) do
              {:ok, _families, font_face} -> [font_face]
              :error -> []
            end
          end)

        candidates =
          [selected | requested ++ Font.fallback_faces(registry, weight, font_style)]
          |> Enum.uniq_by(&Font.pdf_name/1)

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

        node
        |> Map.put(:_font_candidates, candidates)
        |> Map.put(:_font_graphemes, graphemes)

      %{type: :element, children: children} = element ->
        %{element | children: Enum.map(children, &prepare_candidate_node/1)}
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

        resolved

      %{type: :element, children: children} = element when is_list(children) ->
        [%{element | children: resolve_prepared_nodes(children, unsupported_glyphs)}]
    end
  end

  defp append_run(runs, node, style, font_face, grapheme) do
    case List.last(runs) do
      %{style: %{font_face: previous_face}} when previous_face == font_face ->
        List.update_at(runs, -1, &%{&1 | text: &1.text <> grapheme})

      _ ->
        resolved_style =
          style
          |> Map.put(:font_face, font_face)
          |> Map.put(:font_family, font_face.family)

        runs ++ [%{node | text: grapheme, style: resolved_style}]
    end
  end
end
