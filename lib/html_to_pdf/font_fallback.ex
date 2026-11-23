defmodule NativeElixirPdfUtilities.HtmlToPdf.FontFallback do
  alias NativeElixirPdfUtilities.HtmlToPdf.UnicodeGrapheme

  @moduledoc """
  Resolves styled text graphemes to fonts before layout and PDF writing.

  The selected CSS face is tried first, followed by the remaining requested
  families, configured font faces, and the bundled fallback faces. Adjacent
  graphemes using the same face remain one text node so layout can measure and
  wrap the final font runs.
  """

  alias NativeElixirPdfUtilities.Diagnostics
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.Validators.FontRenderingValidator
  alias NativeElixirPdfUtilities.Validators.FontValidator
  alias NativeElixirPdfUtilities.Validators.HtmlLineBreakValidator
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @type styled_tree :: NativeElixirPdfUtilities.HtmlToPdf.Style.styled_tree()
  @type unsupported_glyphs :: :replace | :error
  @type error_reason ::
          :invalid_document
          | :invalid_encoding
          | :invalid_options
          | :unsupported_font
          | :unsupported_glyph
          | :resource_limit_exceeded
  @replacement_character "\uFFFD"

  @doc """
  Resolves every styled text node to font faces that contain its graphemes.

  Unsupported graphemes are replaced with U+FFFD by default. Pass `:error` as
  the second argument to return an `:unsupported_glyph` diagnostic instead.
  """
  @spec resolve(styled_tree(), unsupported_glyphs()) ::
          {:ok, styled_tree()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def resolve(styled_tree, unsupported_glyphs \\ :replace) do
    FontValidator.with_budget(fn ->
      with :ok <- HtmlValidator.validate_font_fallback_input(styled_tree, unsupported_glyphs),
           prepared <- prepare_candidates(styled_tree),
           {:ok, prepared} <-
             HtmlValidator.prepare_font_coverage(
               prepared,
               unsupported_glyphs,
               @replacement_character
             ) do
        {:ok,
         %{
           prepared
           | children: resolve_prepared_nodes(prepared.children)
         }}
      else
        {:error, {reason, diagnostic}} ->
          {:error,
           {reason,
            Diagnostics.with_context(diagnostic, operation: :resolve_fonts, module: __MODULE__)}}
      end
    end)
  catch
    {:font_selection_error, error} -> error
  end

  defp prepare_candidates(%{type: :document, children: children} = document) do
    {children, _cache} = Enum.map_reduce(children, {nil, %{}}, &prepare_candidate_node/2)
    %{document | children: children}
  end

  defp prepare_candidate_node(node, {cached_context, cached_candidates} = cache) do
    case node do
      %{type: :text, style: style} ->
        registry = Map.fetch!(style, :_font_registry)
        selected = Map.fetch!(style, :font_face)
        families = Map.fetch!(style, :font_families)
        weight = Map.fetch!(style, :font_weight)
        font_style = Map.fetch!(style, :font_style)

        # Compare shared font data and scoped definitions outside the hashed key.
        context = {registry, Map.get(style, :_font_definitions)}
        cached_candidates = if context == cached_context, do: cached_candidates, else: %{}

        key =
          {families, FontRenderingValidator.rendering_request(style),
           Map.get(style, :_text_language, "und"), Map.get(style, :writing_mode, :horizontal_tb),
           Map.get(style, :text_orientation, :mixed),
           Map.get(style, :text_combine_upright, :none)}

        {candidates, cached_candidates} =
          case Map.fetch(cached_candidates, key) do
            {:ok, {previous_selected, candidates}} when previous_selected == selected ->
              {candidates, cached_candidates}

            _ ->
              requested = Font.requested_faces(families, weight, font_style, registry)

              candidates =
                [selected | requested ++ Font.css_fallback_faces(registry, weight, font_style)]
                |> then(fn fonts ->
                  case FontRenderingValidator.prepare_candidates(
                         fonts,
                         style
                       ) do
                    {:ok, fonts} -> fonts
                    error -> throw({:font_selection_error, error})
                  end
                end)
                |> Enum.uniq_by(&{Font.pdf_name(&1), Map.get(&1, :unicode_range)})

              {candidates, Map.put(cached_candidates, key, {selected, candidates})}
          end

        text = HtmlValidator.preprocess_html(node.text)

        # A fully covered run needs no font-boundary segmentation. Source
        # provenance still follows individual graphemes through normalization.
        graphemes =
          if text != "" and
               not Map.has_key?(selected, :synthetic_variants) and
               not NativeElixirPdfUtilities.Validators.EmojiPresentationValidator.matching_required?(
                 text,
                 selected
               ) and
               Font.supports_text?(selected, text) and
               (not NativeElixirPdfUtilities.Validators.TextShapingValidator.required?(
                  selected,
                  text
                ) or
                  FontRenderingValidator.supports_text?(
                    selected,
                    text
                  )) do
            [%{text: text, layout_whitespace?: false}]
          else
            text
            |> UnicodeGrapheme.graphemes()
            |> Enum.map(fn grapheme ->
              %{
                text: grapheme,
                layout_whitespace?: HtmlLineBreakValidator.layout_whitespace?(grapheme, style)
              }
            end)
          end

        graphemes =
          case Map.get(style, :_source_glyph_characters) do
            characters when is_list(characters) ->
              characters =
                NativeElixirPdfUtilities.Validators.NativeGlyphSourceValidator.normalize_characters(
                  characters,
                  %{white_space: :pre}
                )

              {graphemes, []} =
                Enum.map_reduce(graphemes, characters, fn grapheme, remaining ->
                  {source, remaining} =
                    Enum.split(remaining, length(String.codepoints(grapheme.text)))

                  {Map.put(grapheme, :source_characters, source), remaining}
                end)

              graphemes

            _ ->
              graphemes
          end

        graphemes =
          if Map.has_key?(node, :publishing_text_id) do
            {graphemes, _} =
              Enum.map_reduce(graphemes, Map.get(node, :publishing_offset, 0), fn grapheme,
                                                                                  offset ->
                {Map.put(grapheme, :publishing_offset, offset),
                 offset + length(String.codepoints(grapheme.text))}
              end)

            graphemes
          else
            graphemes
          end

        prepared =
          node
          |> Map.put(:_font_candidates, candidates)
          |> Map.put(:_font_graphemes, graphemes)

        {prepared, {context, cached_candidates}}

      %{type: :element, children: children} = element ->
        {children, cache} = Enum.map_reduce(children, cache, &prepare_candidate_node/2)

        case element do
          %{style: %{_list_marker: markers} = style} ->
            {markers, cache} = Enum.map_reduce(markers, cache, &prepare_candidate_node/2)

            {%{element | style: Map.put(style, :_list_marker, markers), children: children},
             cache}

          _ ->
            {%{element | children: children}, cache}
        end
    end
  end

  defp resolve_prepared_nodes(nodes) do
    Enum.flat_map(nodes, &resolve_prepared_node/1)
  end

  defp resolve_prepared_node(node) do
    case node do
      %{
        type: :text,
        style: style,
        _font_graphemes: graphemes
      } ->
        node = Map.delete(node, :_font_graphemes)

        resolved =
          Enum.reduce(graphemes, [], fn grapheme, runs ->
            text = grapheme.displayed
            source = Map.get(grapheme, :source_characters)

            source =
              if source && text != grapheme.text,
                do: [%{displayed: text, fragments: Enum.flat_map(source, & &1.fragments)}],
                else: source

            append_run(
              runs,
              node,
              style,
              grapheme.font_face,
              text,
              source,
              Map.get(grapheme, :publishing_offset)
            )
          end)

        if resolved == [] and Map.has_key?(node, :publishing_content),
          do: [node],
          else: Enum.reverse(resolved)

      %{type: :element, children: children} = element when is_list(children) ->
        element =
          case element do
            %{style: %{_list_marker: markers} = style} ->
              markers = resolve_prepared_nodes(markers)
              %{element | style: Map.put(style, :_list_marker, markers)}

            _ ->
              element
          end

        [%{element | children: resolve_prepared_nodes(children)}]
    end
  end

  defp append_run(runs, node, style, font_face, grapheme, source, publishing_offset) do
    case runs do
      [%{style: %{font_face: previous_face}} = previous | remaining]
      when previous_face == font_face ->
        combined_style =
          if source,
            do: Map.update!(previous.style, :_source_glyph_characters, &(&1 ++ source)),
            else: previous.style

        [%{previous | text: previous.text <> grapheme, style: combined_style} | remaining]

      _ ->
        resolved_style =
          style
          |> Map.put(:font_face, font_face)
          |> Map.put(:font_family, font_face.family)

        resolved_style =
          if source,
            do: Map.put(resolved_style, :_source_glyph_characters, source),
            else: resolved_style

        resolved_style =
          if runs != [] and Map.has_key?(resolved_style, :_generated_alternative),
            do: Map.put(resolved_style, :_generated_alternative, ""),
            else: resolved_style

        node =
          if publishing_offset == nil,
            do: node,
            else: Map.put(node, :publishing_offset, publishing_offset)

        [%{node | text: grapheme, style: resolved_style} | runs]
    end
  end
end
