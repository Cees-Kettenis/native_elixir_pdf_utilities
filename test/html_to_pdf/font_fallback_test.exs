defmodule NativeElixirPdfUtilities.HtmlToPdf.FontFallbackTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.FontFallback

  test "keeps supported ASCII in the selected face and falls back per grapheme" do
    style = text_style()

    tree = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "p",
          style: %{},
          children: [%{type: :text, text: "ASCII\r\ncafé © α €", style: style}]
        }
      ]
    }

    assert {:ok, resolved} = FontFallback.resolve(tree)
    [paragraph] = resolved.children
    runs = paragraph.children

    assert Enum.map_join(runs, & &1.text) == "ASCII\ncafé © α €"
    assert hd(runs).text == "ASCII\ncaf"
    assert hd(runs).style.font_family == "Helvetica"

    fallback_runs = Enum.filter(runs, &(&1.style.font_family == "DejaVu Sans"))
    assert Enum.map(fallback_runs, & &1.text) == ["é", "©", "α", "€"]
    assert Enum.all?(fallback_runs, &(&1.style.font_face.type == :embedded))
  end

  test "keeps combining sequences together and matches bold italic fallback faces" do
    style = text_style(700, :italic)
    tree = %{type: :document, children: [%{type: :text, text: "e\u0301", style: style}]}

    assert {:ok, %{children: [run]}} = FontFallback.resolve(tree)
    assert run.text == "e\u0301"
    assert run.style.font_face.family == "DejaVu Sans"
    assert run.style.font_face.weight == 700
    assert run.style.font_face.style == :italic
  end

  test "uses configured fonts before the bundled fallback" do
    assert {:ok, registry} =
             Font.load_registry(fonts: [%{family: "Provided Sans", path: bundled_font_path()}])

    style = text_style_with_registry(registry)
    tree = %{type: :document, children: [%{type: :text, text: "α", style: style}]}

    assert {:ok, %{children: [run]}} = FontFallback.resolve(tree)
    assert run.style.font_face.family == "Provided Sans"
  end

  test "returns actionable diagnostics for invalid text and missing font state" do
    invalid_encoding = %{
      type: :document,
      children: [%{type: :text, text: <<255>>, style: text_style()}]
    }

    assert {:error,
            {:invalid_encoding,
             %{
               stage: :font,
               reason: :invalid_encoding,
               operation: :resolve_fonts,
               module: FontFallback,
               message: "styled text must be valid UTF-8"
             }}} = FontFallback.resolve(invalid_encoding)

    missing_font_state = %{
      type: :document,
      children: [%{type: :text, text: "text", style: %{}}]
    }

    assert {:error,
            {:invalid_document,
             %{
               stage: :font,
               reason: :invalid_document,
               message: "styled text is missing its resolved font registry"
             }}} = FontFallback.resolve(missing_font_state)
  end

  test "replaces unsupported graphemes by default and preserves strict diagnostics" do
    unsupported = %{
      type: :document,
      children: [%{type: :text, text: "can\u0092t 漢", style: text_style()}]
    }

    assert {:ok, resolved} = FontFallback.resolve(unsupported)
    assert Enum.map_join(resolved.children, & &1.text) == "can\uFFFDt \uFFFD"

    replacement_runs = Enum.filter(resolved.children, &String.contains?(&1.text, "\uFFFD"))
    assert Enum.all?(replacement_runs, &(&1.style.font_face.family == "DejaVu Sans"))

    assert {:error,
            {:unsupported_glyph,
             %{
               stage: :font,
               reason: :unsupported_glyph,
               source: "漢",
               message:
                 "no requested, configured, or bundled font contains every glyph in \"漢\" (U+6F22)"
             }}} =
             FontFallback.resolve(
               %{unsupported | children: [%{type: :text, text: "漢", style: text_style()}]},
               :error
             )
  end

  test "rejects malformed styled trees without raising" do
    valid_tree = %{type: :document, children: [%{type: :text, text: "text", style: text_style()}]}

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               message: "unsupported_glyphs must be :replace or :error"
             }}} = FontFallback.resolve(valid_tree, :ignore)

    assert {:error,
            {:invalid_document,
             %{
               stage: :font,
               reason: :invalid_document,
               message: "font fallback requires a styled document tree"
             }}} = FontFallback.resolve(:not_a_tree)

    assert {:error,
            {:invalid_document,
             %{
               stage: :font,
               reason: :invalid_document,
               message: "font fallback encountered an invalid styled node"
             }}} = FontFallback.resolve(%{type: :document, children: [:bad_node]})

    assert {:ok, %{children: []}} =
             FontFallback.resolve(%{
               type: :document,
               children: [%{type: :text, text: "", style: text_style()}]
             })

    no_replacement_style =
      text_style()
      |> Map.put(:_font_registry, %{embedded: [], fallback: []})

    assert {:error, {:unsupported_glyph, %{source: "漢"}}} =
             FontFallback.resolve(%{
               type: :document,
               children: [%{type: :text, text: "漢", style: no_replacement_style}]
             })
  end

  defp text_style(weight \\ 400, font_style \\ :normal) do
    {:ok, registry} = Font.load_registry([])
    text_style_with_registry(registry, weight, font_style)
  end

  defp text_style_with_registry(registry, weight \\ 400, font_style \\ :normal) do
    font_face = %{type: :built_in, family: "Helvetica", pdf_name: "Helvetica"}

    %{
      _font_registry: registry,
      font_face: font_face,
      font_families: ["Helvetica"],
      font_family: font_face.family,
      font_weight: weight,
      font_style: font_style
    }
  end

  defp bundled_font_path do
    Application.app_dir(
      :native_elixir_pdf_utilities,
      "priv/fonts/dejavu/DejaVuSans.ttf"
    )
  end
end
