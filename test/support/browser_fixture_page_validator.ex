defmodule NativeElixirPdfUtilities.TestSupport.BrowserFixturePageValidator do
  @moduledoc false
  import ExUnit.Assertions
  alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}

  @doc "Prepares unchanged source bodies and independently scoped literal CSS for reviewed document pages."
  @spec prepare!(String.t(), map()) :: [map()]
  def prepare!(directory, group) do
    names = Enum.map(group["sources"], & &1["name"])
    assert length(names) == length(Enum.uniq(names)), "Duplicate document page owner"

    {sources, _} =
      Enum.map_reduce(
        Enum.with_index(group["sources"]),
        MapSet.new(
          for index <- 0..(length(names) - 1),
              prefix <- ["body_", "page_"],
              do: prefix <> Integer.to_string(index)
        ),
        fn {metadata, index}, ids ->
          source = File.read!(Path.join(directory, metadata["name"]))
          digest = Base.encode16(:crypto.hash(:sha256, source), case: :lower)

          assert digest == metadata["sha256"],
                 "Document page source changed; recheck both engines: #{metadata["name"]}"

          [[_, css]] = Regex.scan(~r/<style>(.*?)<\/style>/s, source)
          [[_, body_attributes, body]] = Regex.scan(~r/<body\b([^>]*)>(.*?)<\/body>/s, source)
          [[_, root_attributes]] = Regex.scan(~r/<html\b([^>]*)>/s, source)

          attributes =
            ~r/\A\s*(?:(?:lang|xml:lang|dir|translate)\s*=\s*(?:"[^"]*"|'[^']*'|[^\s>]+)\s*)*\z/i

          assert Regex.match?(attributes, root_attributes) and
                   Regex.match?(attributes, body_attributes),
                 "Document root hints require their original document"

          refute Regex.match?(~r/<(?:frameset|script)\b/i, body)

          css = Regex.replace(~r/@page\s*\{[^{}]*\}/s, css, "")
          refute Regex.match?(~r/@|\b(?:var|counter|counters)\s*\(|:root/, css)

          refute Regex.match?(~r/position\s*:\s*fixed|\bpopover\b/i, source),
                 "Fixed-position and popover contexts remain independent"

          assert String.trim(Regex.replace(~r/([^{}]+)\{([^{}]*)\}/s, css, "")) == ""

          ids =
            NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheetValidator.include_source_ids!(
              ids,
              body
            )

          marker = "parity_#{index}_#{digest}"
          scope = "#body_#{index}"
          root = "#page_#{index}"

          root_selector =
            if group["page_mode"] == "named", do: "section:where(#{root})", else: root

          body_selector = if group["page_mode"] == "named", do: "div:where(#{scope})", else: scope
          descendant_scope = if group["page_mode"] == "named", do: ":where(#{scope})", else: scope

          rules =
            Regex.replace(~r/([^{}]+)\{([^{}]*)\}/s, css, fn _, selectors, declarations ->
              selectors =
                selectors
                |> split_selectors()
                |> Enum.map_join(",", fn selector ->
                  selector = String.trim(selector)
                  refute Regex.match?(~r/\bhtml\s*[>+~]/, selector)

                  cond do
                    Regex.match?(~r/^html\s+body\b/, selector) ->
                      Regex.replace(
                        ~r/^html\s+body\b/,
                        selector,
                        root_selector <> " " <> body_selector
                      )

                    Regex.match?(~r/^html\b/, selector) ->
                      Regex.replace(~r/^html\b/, selector, root_selector)

                    Regex.match?(~r/^body\b/, selector) ->
                      Regex.replace(~r/^body\b/, selector, body_selector)

                    true ->
                      descendant_scope <> " " <> selector
                  end
                end)

              selectors <> "{" <> declarations <> "}"
            end)

          {%{
             metadata: metadata,
             body: body,
             css: rules,
             marker: marker,
             scope: scope,
             root_attributes: root_attributes,
             body_attributes: body_attributes
           }, ids}
        end
      )

    scopes =
      Enum.map(
        sources,
        &NativeElixirPdfUtilities.TestSupport.BrowserFixtureSheetValidator.source_identifiers(
          &1.body <> &1.css
        )
      )

    all_ids = Enum.reduce(scopes, MapSet.new(), &MapSet.union(&2, MapSet.union(&1.ids, &1.names)))

    Enum.reduce(scopes, MapSet.new(), fn scope, names ->
      owned = MapSet.union(scope.ids, scope.names)

      assert MapSet.disjoint?(names, owned),
             "Document scenarios share an identifier or name/radio owner"

      missing = MapSet.difference(scope.references, MapSet.union(scope.ids, scope.names))

      assert MapSet.disjoint?(missing, all_ids),
             "Another scenario supplies a missing local target"

      MapSet.union(names, owned)
    end)

    sources
  end

  @doc "Freezes the independently verified composition so changing the compiler requires a new equivalence proof."
  @spec assert_compilation!(map(), String.t()) :: :ok
  def assert_compilation!(group, html) do
    if expected = group["html_sha256"] do
      assert Base.encode16(:crypto.hash(:sha256, html), case: :lower) == expected,
             "Document composition changed; recheck original page equivalence"
    end

    :ok
  end

  @doc "Keeps new browser-option variants on the ordinary original-document capture path."
  @spec same_browser_options?(keyword(), keyword()) :: boolean()
  def same_browser_options?(request, reviewed) do
    keys = [
      :chromium_transform,
      :chromium_page_size,
      :chromium_page_furniture,
      :chromium_page_margin,
      :chromium_page_margin_css,
      :svg_reference,
      :dpi
    ]

    render_keys = [:fonts, :page_size, :margin, :base_url]

    Enum.all?(keys, &(Keyword.get(request, &1) == Keyword.get(reviewed, &1))) and
      Enum.all?(
        render_keys,
        &(Keyword.get(Keyword.get(request, :render_opts, []), &1) ==
            Keyword.get(Keyword.get(reviewed, :render_opts, []), &1))
      )
  end

  @doc "Checks the generated document against the retained source compilation."
  @spec assert_generated!(String.t(), String.t()) :: :ok
  def assert_generated!(path, expected) do
    assert File.read!(path) == expected, "Regenerate reviewed document pages: #{path}"
    :ok
  end

  @doc "Rejects multiple capture owners for the same original scenario."
  @spec assert_owners!([{String.t(), map()}]) :: :ok
  def assert_owners!(members) do
    names = Enum.map(members, &elem(&1, 0))
    assert length(names) == length(Enum.uniq(names)), "Duplicate document page capture owner"
    :ok
  end

  @doc "Identifies the non-painting boundary annotation used by both PDF engines."
  @spec marker_uri(String.t()) :: String.t()
  def marker_uri(marker), do: "https://parity.invalid/fixture-start/" <> marker

  @doc "Validates every source's start page and removes only boundary annotations before extracting its PDF."
  @spec clean_boundaries!(binary(), [map()]) :: {binary(), [Range.t()]}
  def clean_boundaries!(pdf, sources) do
    assert {:ok, context} = Reader.read_validated(pdf)

    {ranges, next} =
      Enum.map_reduce(sources, 1, fn source, first ->
        count = source.metadata["page_count"]
        assert is_integer(count) and count > 0
        {first..(first + count - 1), first + count}
      end)

    assert length(context.pages) == next - 1, "Combined fixture page count changed"

    for {source, range} <- Enum.zip(sources, ranges), expected = source.metadata["media_boxes"] do
      actual =
        context.pages
        |> Enum.slice(range.first - 1, Enum.count(range))
        |> Enum.map(& &1.media_box)

      assert actual == expected,
             "Original browser page bounds changed: #{source.metadata["name"]}"
    end

    expected =
      Map.new(Enum.zip(sources, ranges), fn {source, range} ->
        {marker_uri(source.marker), range.first}
      end)

    {updates, found} =
      context.pages
      |> Enum.with_index(1)
      |> Enum.map_reduce(%{}, fn {page, number}, found ->
        {:ok, dictionary} = Reader.dictionary(context.document, {:ref, page.ref})
        {:ok, annotations} = Reader.resolve(context.document, Map.get(dictionary, "Annots", []))

        {kept, found} =
          Enum.reduce(annotations, {[], found}, fn annotation, {kept, found} ->
            {:ok, value} = Reader.dictionary(context.document, annotation)
            {:ok, action} = Reader.resolve(context.document, Map.get(value, "A", %{}))
            uri = action["URI"]

            case uri do
              {:string, uri} when is_map_key(expected, uri) ->
                refute Map.has_key?(found, uri), "Duplicate fixture page boundary"
                assert number == expected[uri], "Fixture page boundary moved: #{uri}"
                {kept, Map.put(found, uri, number)}

              _ ->
                {kept ++ [annotation], found}
            end
          end)

        {object, generation} = page.ref

        update =
          if length(kept) != length(annotations),
            do: {object, generation, {:value, Map.put(dictionary, "Annots", kept)}}

        {update, found}
      end)

    assert found == expected,
           "Missing fixture page boundary: #{inspect(Map.keys(expected) -- Map.keys(found))}"

    assert {:ok, clean} = IncrementalWriter.write(context, Enum.reject(updates, &is_nil/1))
    {clean, ranges}
  end

  defp split_selectors(value) do
    opaque = ~r/"(?:\\.|[^"\\])*"|'(?:\\.|[^'\\])*'|\/\*.*?\*\/|\\.|[\[\](),]/s

    {parts, start, depth} =
      Enum.reduce(Regex.scan(opaque, value, return: :index), {[], 0, 0}, fn [{index, size}],
                                                                            {parts, start, depth} ->
        token = binary_part(value, index, size)

        cond do
          token in ["(", "["] ->
            {parts, start, depth + 1}

          token in [")", "]"] ->
            {parts, start, depth - 1}

          token == "," and depth == 0 ->
            {parts ++ [binary_part(value, start, index - start)], index + 1, depth}

          true ->
            {parts, start, depth}
        end
      end)

    assert depth == 0, "Unbalanced literal selector"
    parts ++ [binary_part(value, start, byte_size(value) - start)]
  end
end
