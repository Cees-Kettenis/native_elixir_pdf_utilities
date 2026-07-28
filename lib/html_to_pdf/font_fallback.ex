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

  @type styled_tree :: NativeElixirPdfUtilities.HtmlToPdf.Style.styled_tree()
  @type error_reason :: :invalid_document | :invalid_encoding | :unsupported_glyph

  @doc """
  Resolves every styled text node to font faces that contain its graphemes.
  """
  @spec resolve(styled_tree()) ::
          {:ok, styled_tree()} | {:error, {error_reason(), Diagnostics.diagnostic()}}
  def resolve(styled_tree) do
    case styled_tree do
      %{type: :document, children: children} = document when is_list(children) ->
        case resolve_nodes(children) do
          {:ok, resolved_children} -> {:ok, %{document | children: resolved_children}}
          {:error, {_reason, _diagnostic}} = error -> error
        end

      _ ->
        Diagnostics.error(
          :font,
          :invalid_document,
          "font fallback requires a styled document tree",
          operation: :resolve_fonts,
          module: __MODULE__
        )
    end
  end

  defp resolve_nodes(nodes) do
    Enum.reduce_while(nodes, {:ok, []}, fn node, {:ok, resolved} ->
      case resolve_node(node) do
        {:ok, resolved_nodes} -> {:cont, {:ok, resolved ++ resolved_nodes}}
        {:error, {_reason, _diagnostic}} = error -> {:halt, error}
      end
    end)
  end

  defp resolve_node(node) do
    case node do
      %{type: :text, text: text, style: style} when is_binary(text) and is_map(style) ->
        resolve_text(node, text, style)

      %{type: :element, children: children} = element when is_list(children) ->
        case resolve_nodes(children) do
          {:ok, resolved_children} -> {:ok, [%{element | children: resolved_children}]}
          {:error, {_reason, _diagnostic}} = error -> error
        end

      _ ->
        Diagnostics.error(
          :font,
          :invalid_document,
          "font fallback encountered an invalid styled node",
          operation: :resolve_fonts,
          module: __MODULE__
        )
    end
  end

  defp resolve_text(node, text, style) do
    case String.valid?(text) do
      true ->
        with {:ok, candidates} <- font_candidates(style) do
          text
          |> String.replace("\r\n", "\n")
          |> String.replace("\r", "\n")
          |> String.graphemes()
          |> Enum.reduce_while({:ok, []}, fn grapheme, {:ok, runs} ->
            layout_whitespace? =
              grapheme
              |> String.to_charlist()
              |> Enum.all?(&(&1 in [9, 10, 13]))

            font_face =
              case layout_whitespace? do
                true -> List.first(candidates)
                false -> Enum.find(candidates, &Font.supports_text?(&1, grapheme))
              end

            case font_face do
              nil ->
                {:halt, unsupported_glyph(grapheme)}

              font_face ->
                {:cont, {:ok, append_run(runs, node, style, font_face, grapheme)}}
            end
          end)
        end

      false ->
        Diagnostics.error(
          :font,
          :invalid_encoding,
          "styled text must be valid UTF-8",
          operation: :resolve_fonts,
          module: __MODULE__
        )
    end
  end

  defp font_candidates(style) do
    case style do
      %{
        _font_registry: registry,
        font_face: selected,
        font_families: families,
        font_weight: weight,
        font_style: font_style
      }
      when is_map(registry) and is_map(selected) and is_list(families) and is_number(weight) and
             font_style in [:normal, :italic] ->
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

        {:ok, candidates}

      _ ->
        Diagnostics.error(
          :font,
          :invalid_document,
          "styled text is missing its resolved font registry",
          operation: :resolve_fonts,
          module: __MODULE__
        )
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

  defp unsupported_glyph(grapheme) do
    codepoints =
      grapheme
      |> String.to_charlist()
      |> Enum.map_join(
        " ",
        &("U+" <> (&1 |> Integer.to_string(16) |> String.upcase() |> String.pad_leading(4, "0")))
      )

    Diagnostics.error(
      :font,
      :unsupported_glyph,
      "no requested, configured, or bundled font contains every glyph in #{inspect(grapheme)} (#{codepoints})",
      operation: :resolve_fonts,
      module: __MODULE__,
      source: grapheme
    )
  end
end
