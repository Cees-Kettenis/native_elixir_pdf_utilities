defmodule NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheets do
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.{CssParser, PageGeometry}

  @doc "Builds a comparison sheet from unchanged fixture sources and explicit original canvas positions."
  @spec build!(String.t(), map()) :: String.t()
  def build!(directory, sheet) do
    [width, height] = sheet["size"]

    {styles, sections, _ids} =
      sheet["regions"]
      |> Enum.with_index()
      |> Enum.reduce({[], [], MapSet.new()}, fn {[name, [x, y, w, h]], index},
                                                {styles, sections, ids} ->
        source = File.read!(Path.join(directory, name))
        [_, css] = Regex.run(~r/<style>(.*?)<\/style>/s, source)
        [_, body] = Regex.run(~r/<body>(.*?)<\/body>/s, source)
        {:ok, parsed} = CssParser.prepare_source_stylesheet(css)

        # Keep authored SVG IDs and URL references intact. Sharing an ID across
        # samples could make Chromium resolve artwork from another fixture.
        sample_ids =
          Regex.scan(~r/\bid=["']([^"']+)/, body, capture: :all_but_first)
          |> List.flatten()
          |> MapSet.new()

        true = MapSet.disjoint?(ids, sample_ids)
        scope = "#sample_#{index}"

        scoped_styles =
          Enum.map(parsed.authored_rules, fn rule ->
            selectors =
              rule.source.text
              |> String.split(",")
              |> Enum.map_join(",", fn selector ->
                selector = String.trim(selector)

                if Regex.match?(~r/^(html|body)\b/, selector) do
                  Regex.replace(~r/^(?:html\s+)?body\b|^html\b/, selector, scope)
                else
                  scope <> " " <> selector
                end
              end)

            declarations = Enum.map_join(rule.source_declarations, ";", & &1.source.text)
            selectors <> "{" <> declarations <> "}"
          end)

        {:ok, %{top: margin, bottom: margin, left: margin, right: margin}} =
          PageGeometry.normalize_margins(Keyword.fetch!(parsed.page_options, :margin))

        digest = Base.encode16(:crypto.hash(:sha256, source), case: :lower)

        section = """
        <!-- #{name}; SHA256 #{digest}; original #{w} x #{h} pt canvas -->
        <section id="sample_#{index}" style="position:absolute;left:#{x + margin}pt;top:#{y + margin}pt;width:#{w - 2 * margin}pt;height:#{h - 2 * margin}pt;box-sizing:border-box">#{body}</section>
        """

        {styles ++ scoped_styles, sections ++ [section], MapSet.union(ids, sample_ids)}
      end)

    """
    <!doctype html>
    <html><head><meta charset="utf-8"><style>
    @page{size:#{width}pt #{height}pt;margin:0}
    html,body{margin:0;padding:0;font-size:#{sheet["font_size"]}}
    #{Enum.join(styles, "\n")}
    </style></head><body>
    #{Enum.join(sections, "\n")}
    </body></html>
    """
  end
end
