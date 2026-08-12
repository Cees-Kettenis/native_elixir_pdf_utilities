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
  @type error_reason :: :invalid_document | :invalid_encoding | :unsupported_glyph

  @doc """
  Resolves every styled text node to font faces that contain its graphemes.
  """
  @spec resolve(styled_tree()) ::
          {:ok, styled_tree()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def resolve(styled_tree) do
    case HtmlValidator.prepare_font_fallback(styled_tree) do
      {:ok, %{children: children} = document} ->
        {:ok, %{document | children: resolve_prepared_nodes(children)}}

      {:error, {reason, diagnostic}} ->
        {:error,
         {reason,
          Diagnostics.with_context(diagnostic, operation: :resolve_fonts, module: __MODULE__)}}
    end
  end

  defp resolve_prepared_nodes(nodes) do
    Enum.flat_map(nodes, &resolve_prepared_node/1)
  end

  defp resolve_prepared_node(node) do
    case node do
      %{type: :text, text: text, style: style, _font_candidates: candidates} ->
        resolved =
          text
          |> String.replace("\r\n", "\n")
          |> String.replace("\r", "\n")
          |> String.graphemes()
          |> Enum.reduce([], fn grapheme, runs ->
            layout_whitespace? =
              grapheme
              |> String.to_charlist()
              |> Enum.all?(&(&1 in [9, 10, 13]))

            font_face =
              case layout_whitespace? do
                true -> List.first(candidates)
                false -> Enum.find(candidates, &Font.supports_text?(&1, grapheme))
              end

            append_run(runs, Map.delete(node, :_font_candidates), style, font_face, grapheme)
          end)

        resolved

      %{type: :element, children: children} = element when is_list(children) ->
        [%{element | children: resolve_prepared_nodes(children)}]
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
