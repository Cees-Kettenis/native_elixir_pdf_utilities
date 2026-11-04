defmodule NativeElixirPdfUtilities.HtmlToPdf.StyleStateScalingTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{CounterState, HtmlParser, QuotationData, Style}

  test "expired child scopes and replaced peer instances release their indexed values" do
    empty = CounterState.new()
    seed = CounterState.apply(%{counter_reset: [{"x", 1}, {"y", 2}]}, empty, empty, :seed)
    replaced = CounterState.apply(%{counter_reset: [{"x", 3}]}, seed, empty, :peer)
    assert map_size(replaced.values) == 2
    assert replaced.created[:seed] == %{"y" => :seed}

    state =
      Enum.reduce(1..500, replaced, fn creator, state ->
        CounterState.apply(
          %{counter_reset: [{"x", creator}, {"y", creator}]},
          state,
          empty,
          creator
        )
      end)

    assert map_size(state.values) == 2 and map_size(state.created) == 1
    assert CounterState.value(state, "x") == 500

    nested = CounterState.apply(%{counter_reset: [{"x", 9}, {"child", 7}]}, seed, seed, :child)
    restored = CounterState.inherit(seed, nil, nested)
    assert restored.scope == seed.scope and restored.values == seed.values
    assert restored.created == seed.created
    assert CounterState.value(restored, "child") == 0
  end

  test "long language suffixes do not rebuild every locale prefix" do
    suffix = List.duplicate("private", 20_000)
    assert QuotationData.pairs(["fr", "ch"]) == [{"«", "»"}, {"‹", "›"}]
    assert QuotationData.pairs(["de", "ch"]) == [{"„", "“"}, {"‚", "‘"}]
    assert QuotationData.pairs([]) == QuotationData.pairs(["unknown"])
    assert QuotationData.pairs(["de", "ch", "x" | suffix]) == QuotationData.pairs(["de", "ch"])
    {:reductions, before} = Process.info(self(), :reductions)
    assert QuotationData.pairs(["x" | suffix]) == [{"“", "”"}, {"‘", "’"}]
    {:reductions, after_work} = Process.info(self(), :reductions)
    assert after_work - before < 10_000

    lang = "fr-ch-x-" <> Enum.join(Enum.take(suffix, 2_000), "-")
    assert {:ok, dom} = HtmlParser.parse_detailed("<q lang='#{lang}'>Text</q>")
    assert {:ok, %{children: [quote]}} = Style.compute_detailed(dom)
    assert hd(hd(quote.children).children).text == "«"
    assert hd(List.last(quote.children).children).text == "»"
  end

  test "hundreds of named counters stay efficient across ordinary descendants" do
    declarations = Enum.map_join(1..400, " ", &"item#{&1} #{&1}")
    descendants = String.duplicate("<span>x</span>", 200)

    html =
      "<style>.read::before{content:counter(item400);counter-increment:item400}</style><div style='counter-reset:#{declarations}'>#{descendants}<span class=read></span><div style='counter-reset:item400 7'><span class=read></span></div><span class=read></span></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    # Warm font/cache setup before measuring the styling work being guarded.
    assert {:ok, warmup} = HtmlParser.parse_detailed("<p>Warm</p>")
    assert {:ok, _} = Style.compute_detailed(warmup)
    {:reductions, before} = Process.info(self(), :reductions)
    assert {:ok, %{children: [container]}} = Style.compute_detailed(dom)
    {:reductions, after_work} = Process.info(self(), :reductions)
    assert after_work - before < 3_000_000
    [first, inner, last] = Enum.drop(container.children, 200)
    assert hd(hd(first.children).children).text == "401"
    assert hd(hd(hd(inner.children).children).children).text == "8"
    assert hd(hd(last.children).children).text == "402"
  end
end
