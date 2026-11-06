defmodule NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheetValidator do
  @moduledoc false
  import ExUnit.Assertions

  @doc "Prepares explicitly reviewed fixed-canvas samples without using the production CSS parser to rewrite their reference."
  @spec prepare_sources!(String.t(), map()) :: map()
  def prepare_sources!(directory, sheet) do
    [sheet_width, sheet_height] = sheet["size"]
    regions = sheet["regions"]
    names = Enum.map(regions, &hd/1)
    preserved = sheet["preserved_sources"] || %{}
    assert length(names) == length(Enum.uniq(names)), "Duplicate sheet source owner"
    assert sheet["composition"] in ["literal_blocks", "literal_additions"]

    assert Enum.sort(names) == Enum.sort(Map.keys(sheet["sources"]) ++ Map.keys(preserved)),
           "Every sheet region needs exactly one source owner"

    if sheet["composition"] == "literal_blocks" do
      assert preserved == %{}
    else
      assert map_size(preserved) > 0
      assert Enum.sort(sheet["assertion_engines"]) == ["chromium", "native"]
      assert Enum.sort(Map.keys(sheet["preserved_fields"])) == ["chromium", "native"]
    end

    for {name, digest} <- preserved do
      assert Base.encode16(:crypto.hash(:sha256, File.read!(Path.join(directory, name))),
               case: :lower
             ) == digest,
             "Preserved sheet source changed; recheck extension equivalence: #{name}"
    end

    for {[left, [x, y, width, height]], index} <- Enum.with_index(regions) do
      assert x >= 0 and y >= 0 and width > 0 and height > 0
      assert x + width <= sheet_width and y + height <= sheet_height

      for [right, [rx, ry, rw, rh]] <- Enum.drop(regions, index + 1) do
        assert x + width <= rx or rx + rw <= x or y + height <= ry or ry + rh <= y,
               "Overlapping sheet regions: #{left}, #{right}"
      end
    end

    regions
    |> Enum.filter(fn [name, _bounds] -> Map.has_key?(sheet["sources"], name) end)
    |> Map.new(fn [name, [_x, _y, width, height]] ->
      metadata = Map.fetch!(sheet["sources"], name)
      source = File.read!(Path.join(directory, name))

      assert Base.encode16(:crypto.hash(:sha256, source), case: :lower) == metadata["sha256"],
             "Sheet source changed; recheck independent equivalence: #{name}"

      assert metadata["canvas"] == [width, height]
      assert is_binary(metadata["owner"]) and metadata["owner"] != ""
      assert metadata["assertions"]["page_count"] == 1
      assert is_list(metadata["assertions"]["text"])
      assert metadata["max_changed_ratio"] > 0 and metadata["max_changed_ratio"] <= 0.01
      assert metadata["max_average_delta"] > 0 and metadata["max_average_delta"] <= 0.01

      [[_, css]] = Regex.scan(~r/<style>(.*?)<\/style>/s, source)
      [[_, body]] = Regex.scan(~r/<body>(.*?)<\/body>/s, source)
      [[_, page]] = Regex.scan(~r/@page\s*\{([^{}]*)\}/s, css)
      margin = metadata["margin"]
      assert margin >= 0 and width > 2 * margin and height > 2 * margin
      effective_margin = metadata["effective_margin"] || margin

      # Print margins round to physical CSS pixels, fixed at 96 pixels/inch.
      assert effective_margin == round(margin / 0.75) * 0.75

      assert Regex.replace(~r/\s+/, page, "") in [
               "size:#{width}pt#{height}pt;margin:#{margin}pt",
               "size:#{width}pt#{height}pt;margin:#{margin}pt;"
             ],
             "Literal sheet page geometry differs from its scenario metadata"

      css = Regex.replace(~r/@page\s*\{[^{}]*\}/s, css, "")
      blocks = Regex.scan(~r/([^{}]+)\{([^{}]*)\}/s, css)
      assert String.trim(Regex.replace(~r/([^{}]+)\{([^{}]*)\}/s, css, "")) == ""

      rules =
        Enum.map(blocks, fn [_, selectors, declarations] ->
          # This route deliberately excludes escapes, selector functions, namespaces,
          # nested rules and viewport/root-relative values. New syntax needs a new proof.
          assert Regex.match?(
                   ~r/\A\s*(?:html|body|[a-z][a-z0-9-]*|[.#][a-zA-Z_][a-zA-Z0-9_-]*)(?:\s*,\s*(?:html|body|[a-z][a-z0-9-]*|[.#][a-zA-Z_][a-zA-Z0-9_-]*))*\s*\z/,
                   selectors
                 ),
                 "Unsupported literal sheet selector: #{selectors}"

          refute Regex.match?(
                   ~r/@|\b(?:var|counter|counters)\s*\(|[\d.](?:rem|vw|vh|vmin|vmax)/i,
                   declarations
                 ),
                 "Document-dependent literal sheet declaration"

          if String.contains?(declarations, "%") do
            # Only a reviewed column width with an explicit fixed table basis
            # can use percentages. This does not enable general percentage layout.
            basis = metadata["percentage_columns"]
            assert is_map(basis), "Percentage columns need a fixed table width"
            assert is_number(basis["table_width"]) and basis["table_width"] > 0
            assert is_number(basis["column_width"]) and basis["column_width"] > 0
            assert String.trim(selectors) == "col", "Percentages are limited to table columns"

            assert declarations |> String.replace(~r/\s+/, "") |> String.trim_trailing(";") ==
                     "width:#{basis["column_width"]}%"

            assert Enum.any?(blocks, fn [_, selector, block] ->
                     String.trim(selector) == "table" and
                       "width:#{basis["table_width"]}pt" in String.split(
                         String.replace(block, ~r/\s+/, ""),
                         ";"
                       )
                   end),
                   "Percentage columns need the declared fixed table width"
          end

          %{source: %{text: selectors}, source_declarations: [%{source: %{text: declarations}}]}
        end)

      {name, %{source: source, body: body, margin: effective_margin, rules: rules}}
    end)
  end

  @doc "Keeps authored IDs local to one sample so another sample cannot supply its SVG, label or link target."
  @spec include_source_ids!(MapSet.t(String.t()), String.t()) :: MapSet.t(String.t())
  def include_source_ids!(previous, body) do
    ids = source_identifiers(body).ids

    assert MapSet.disjoint?(previous, ids), "Sheet samples share an authored ID"
    MapSet.union(previous, ids)
  end

  @doc "Reads fixture identifiers independently of the renderer while keeping quoted child HTML and inert template content out of the parent scope."
  @spec source_identifiers(String.t()) :: %{
          ids: MapSet.t(),
          names: MapSet.t(),
          references: MapSet.t()
        }
  def source_identifiers(html) do
    tags =
      Regex.scan(
        ~r/<!--.*?-->(*SKIP)(*F)|<\/?[a-zA-Z][a-zA-Z0-9:-]*(?:\s+(?:"[^"]*"|'[^']*'|[^'">])*)?\s*\/?\s*>/s,
        html
      )

    {result, _skip} =
      Enum.reduce(
        tags,
        {%{ids: MapSet.new(), names: MapSet.new(), references: MapSet.new()}, []},
        fn [tag], {result, skip} ->
          [_, closing, name, attributes] =
            Regex.run(~r/<(\/?)([a-zA-Z][a-zA-Z0-9:-]*)(.*)>/s, tag)

          name = String.downcase(name)

          cond do
            closing == "/" and skip != [] and hd(skip) == name ->
              {result, tl(skip)}

            closing == "/" ->
              {result, skip}

            skip != [] ->
              {result,
               if(name == "template" and hd(skip) == "template", do: [name | skip], else: skip)}

            true ->
              values =
                Regex.scan(
                  ~r/\s+([^\s=\/>]+)(?:\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s>]+)))?/s,
                  attributes,
                  capture: :all_but_first
                )

              result =
                Enum.reduce(values, result, fn [attribute | captures], result ->
                  value =
                    captures
                    |> Enum.find("", &(&1 != ""))
                    |> NativeElixirPdfUtilities.HtmlToPdf.HtmlEntities.decode(:attribute)

                  case String.downcase(attribute) do
                    "id" ->
                      Map.update!(result, :ids, &MapSet.put(&1, value))

                    "name" ->
                      Map.update!(result, :names, &MapSet.put(&1, value))

                    key when key in ["for", "form", "list", "headers"] ->
                      Map.update!(
                        result,
                        :references,
                        &MapSet.union(&1, MapSet.new(String.split(value)))
                      )

                    key when key in ["href", "xlink:href"] ->
                      if String.starts_with?(value, "#"),
                        do:
                          Map.update!(
                            result,
                            :references,
                            &MapSet.put(&1, String.trim_leading(value, "#"))
                          ),
                        else: result

                    _ ->
                      result
                  end
                end)

              skip =
                if name in [
                     "template",
                     "textarea",
                     "iframe",
                     "xmp",
                     "script",
                     "style",
                     "title",
                     "noembed",
                     "noframes"
                   ], do: [name], else: []

              {result, skip}
          end
        end
      )

    urls =
      Regex.scan(~r/url\(\s*["']?#([^\s"')]+)["']?\s*\)/i, html, capture: :all_but_first)
      |> List.flatten()
      |> MapSet.new()

    %{result | references: MapSet.union(result.references, urls)}
  end

  @doc "Checks that reconstructing the preserved sources reproduces the reviewed original sheet bytes."
  @spec assert_preserved_sheet!(map(), String.t()) :: :ok
  def assert_preserved_sheet!(sheet, html) do
    assert Base.encode16(:crypto.hash(:sha256, html), case: :lower) ==
             sheet["preserved_sheet_sha256"],
           "Preserved sheet composition changed; record a new extension baseline"

    :ok
  end

  @doc "Checks each sample's text inside its canvas and preserves the existing sheet's public form fields."
  @spec assert_contents!(map(), binary(), String.t()) :: :ok
  def assert_contents!(sheet, pdf, engine \\ "native") do
    assert {:ok, %{pages: [page]}} = NativeElixirPdfUtilities.Text.extract_spans(pdf)

    if expected = get_in(sheet, ["preserved_fields", engine]) do
      assert {:ok, fields} = NativeElixirPdfUtilities.Forms.fields(pdf)

      assert fields |> :json.encode() |> IO.iodata_to_binary() |> :json.decode() == expected,
             "Sheet extension changed preserved #{engine} form fields"
    end

    normalize = fn text ->
      if sheet["text_whitespace"] == "collapse",
        do: String.replace(text, ~r/\s+/u, " "),
        else: String.replace(text, ~r/\s/u, "")
    end

    for [name, [x, y, width, height]] <- sheet["regions"],
        metadata = sheet["sources"][name],
        metadata != nil do
      text =
        page.spans
        |> Enum.filter(&(&1.x >= x and &1.x < x + width and &1.y >= y and &1.y < y + height))
        |> Enum.map_join(& &1.text)
        |> normalize.()

      for expected <- metadata["assertions"]["text"] do
        assert text =~ normalize.(expected),
               "Missing text in sheet region #{name}: #{inspect(expected)}"
      end
    end

    :ok
  end

  @doc "Preserves the registry's shared comparison assertions for fixed-canvas HTML sheets."
  @spec assert_comparison!(map()) :: :ok
  def assert_comparison!(stats) do
    assert %{
             page_count: 1,
             pages: [_],
             dpi: 72,
             pixel_channel_tolerance: 12,
             scripting: :disabled
           } =
             stats

    assert String.starts_with?(stats.chromium_version, "Chromium ")
    assert String.starts_with?(stats.rasterizer_version, "pdftoppm version ")
    assert length(stats.font_sources) in [4, 5, 6, 8]
    assert Enum.all?(stats.font_sources, &String.match?(&1.sha256, ~r/\A[0-9a-f]{64}\z/))
    assert File.dir?(stats.artifact_dir)
    :ok
  end
end
