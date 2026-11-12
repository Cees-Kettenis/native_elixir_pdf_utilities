# Run with MIX_ENV=test, Chromium and Poppler available. Each supported runtime
# must prove the same retained originals before changing pages.json.
Code.require_file("../../test/support/paths.ex", __DIR__)
Code.require_file("../../test/support/pdf_visual_compare.ex", __DIR__)

alias NativeElixirPdfUtilities.TestSupport.{Paths, BrowserFixturePages, PdfVisualCompare}
alias NativeElixirPdfUtilities.Text
import ExUnit.Assertions
ExUnit.start(autorun: false)

Code.require_file(Paths.support("font_corpus.ex"))
NativeElixirPdfUtilities.TestSupport.FontCorpus.prepare!()

groups = :json.decode(File.read!(Paths.fixture("html_to_pdf/browser_parity/pages.json")))

for group <- groups do
  directory = Paths.fixture(group["directory"])
  entries = BrowserFixturePages.entries(group)
  options = hd(entries) |> elem(1)

  input =
    Path.join(
      directory,
      ".chromium-input-#{group["name"]}-#{System.unique_integer([:positive])}.html"
    )

  File.write!(input, BrowserFixturePages.build!(directory, group))

  verify = fn ->
    references =
      PdfVisualCompare.prepare_fixture_page_references!(
        input,
        Map.delete(group, "proof"),
        entries,
        options
      )

    for {name, opts} <- entries do
      original = PdfVisualCompare.prepare_browser_reference!(Path.join(directory, name), opts)
      actual = Map.fetch!(references, name)

      assert length(original.pages) == length(actual.pages),
             "Original page count changed: #{name}"

      for {before, after_path} <- Enum.zip(original.pages, actual.pages) do
        assert File.read!(before) == File.read!(after_path),
               "Original Chromium pixels changed: #{name}"
      end

      case {Text.extract_file_spans(original.chromium_pdf),
            Text.extract_file_spans(actual.chromium_pdf)} do
        {{:ok, before}, {:ok, after_spans}} ->
          assert before.page_count == after_spans.page_count

          for {left, right} <- Enum.zip(before.pages, after_spans.pages) do
            assert left.media_box == right.media_box, "Original page bounds changed: #{name}"

            assert length(left.spans) == length(right.spans),
                   "Original text count changed: #{name}"

            for {a, b} <- Enum.zip(left.spans, right.spans) do
              for key <- [
                    :text,
                    :render_mode,
                    :adds_to_clip_path?,
                    :joins_previous?,
                    :paints_text?
                  ],
                  do: assert(a[key] == b[key], "Original text semantics changed: #{name}")

              for key <- [:x, :y, :end_x, :end_y, :font_size],
                  do:
                    assert_in_delta(
                      a[key],
                      b[key],
                      0.0001,
                      "Original positioned text changed: #{name}"
                    )
            end
          end

        {{:error, {reason, _}}, {:error, {same_reason, _}}} ->
          assert reason == same_reason

        other ->
          flunk("Original text extraction changed for #{name}: #{inspect(other)}")
      end

      IO.puts("PASS #{group["name"]}: #{name}")
    end
  end

  try do
    if group["proof"]["fontconfig_present"],
      do: PdfVisualCompare.with_embedded_font_configuration(input, verify),
      else: verify.()
  after
    File.rm!(input)
  end
end
