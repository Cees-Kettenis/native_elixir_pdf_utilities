defmodule NativeElixirPdfUtilities.HtmlToPdf.OutlineCacheTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, FontCache}
  alias NativeElixirPdfUtilities.TestSupport.ResourceBudget
  alias NativeElixirPdfUtilities.Validators.FontOutlineValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    {:ok, registry} = Font.load_registry(system_font_discovery: false)
    {:ok, _, font} = Font.resolve("DejaVu Sans", 400, :normal, registry)
    %{font: font, glyph: font.cmap[?é]}
  end

  test "cold and warm outline admission returns identical paths and charges identical work", %{
    font: font,
    glyph: glyph
  } do
    :sys.replace_state(FontCache, fn state ->
      :ets.delete(state.table, {:outlines, font.data})
      state
    end)

    {cold, cold_work} =
      ResourceBudget.measure(:max_work, fn ->
        FontOutlineValidator.prepare_commands(font, glyph)
      end)

    {warm, warm_work} =
      ResourceBudget.measure(:max_work, fn ->
        FontOutlineValidator.prepare_commands(font, glyph)
      end)

    assert {:ok, [_ | _]} = cold
    assert warm == cold
    assert cold_work > 0 and warm_work == cold_work

    [{_, _, _, {:ok, {_, decode_work}}, _}] = :ets.lookup(FontCache, {:outlines, font.data})
    Limits.install(%{Limits.effective() | max_work: decode_work - 1})

    assert {:error, {:resource_limit_exceeded, warm_diagnostic}} =
             FontOutlineValidator.prepare_commands(font, glyph)

    :sys.replace_state(FontCache, fn state ->
      :ets.delete(state.table, {:outlines, font.data})
      state
    end)

    assert {:error, {:resource_limit_exceeded, cold_diagnostic}} =
             FontOutlineValidator.prepare_commands(font, glyph)

    assert cold_diagnostic == warm_diagnostic
    assert cold_diagnostic.stage == :font and cold_diagnostic.message =~ "max_work"
    assert :ets.lookup(FontCache, {:outlines, font.data}) == []
  end

  test "changed bytes cannot reuse a font ID's cached outline and cache eviction preserves output",
       %{font: font, glyph: glyph} do
    assert {:ok, commands} = FontOutlineValidator.prepare_commands(font, glyph)
    damaged = %{font | data: <<0>>}

    assert {:error, {:invalid_document, diagnostic}} =
             FontOutlineValidator.prepare_commands(damaged, glyph)

    assert diagnostic.stage == :font and diagnostic.message != ""
    assert :ets.lookup(FontCache, {:outlines, damaged.data}) == []

    Limits.install(%{Limits.effective() | max_cache_bytes: 1})

    :sys.replace_state(FontCache, fn state ->
      :ets.delete(state.table, {:outlines, font.data})
      state
    end)

    assert {:ok, ^commands} = FontOutlineValidator.prepare_commands(font, glyph)
    assert :ets.lookup(FontCache, {:outlines, font.data}) == []
    assert {:ok, ^commands} = FontOutlineValidator.prepare_commands(font, glyph)
  end

  test "stored size metadata stays exact across insertion sequence encodings", %{
    font: font,
    glyph: glyph
  } do
    assert {:ok, _} = FontOutlineValidator.prepare_commands(font, glyph)
    for n <- 1..260, do: FontCache.identity("outline-cache-size-#{n}", <<>>)
    state = :sys.get_state(FontCache)
    entries = :ets.tab2list(FontCache)

    assert Enum.all?(entries, fn entry ->
             state.sizes[elem(entry, 0)] ==
               NativeElixirPdfUtilities.Validators.FontValidator.cache_bytes(entry)
           end)

    assert Enum.sum(Map.values(state.sizes)) <= Limits.get(:max_cache_bytes)
    assert map_size(state.sizes) == length(entries)
  end
end
