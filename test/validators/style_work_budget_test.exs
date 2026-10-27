defmodule NativeElixirPdfUtilities.Validators.StyleWorkBudgetTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.CounterState
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
  end

  test "counter operations and scope merges use the cumulative CSS work allowance" do
    empty = CounterState.new()
    operations = Enum.map(1..8, &{"item#{&1}", &1})
    parent = CounterState.apply(%{counter_reset: operations}, empty, empty, :parent)
    sibling = CounterState.apply(%{counter_reset: [{"extra", 3}]}, parent, empty, :sibling)
    Limits.install(Map.put(Limits.effective(), :max_css_work, 8))

    for fun <- [
          fn ->
            CounterState.apply(%{counter_reset: operations}, empty, empty, :element)
            CounterState.quote_depth(empty, 1)
          end,
          fn -> CounterState.inherit(parent, sibling, sibling) end,
          fn ->
            CounterState.include_created(empty, parent, :parent)
            CounterState.quote_depth(empty, 1)
          end
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlValidator.with_render_budget(fun)

      assert diagnostic.stage == :style and diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.module == HtmlValidator
      assert diagnostic.message =~ "max_css_work"
    end

    assert CounterState.value(CounterState.inherit(empty, nil, empty), "missing") == 0
  end

  test "public casing failures retain actionable rendered-text diagnostics" do
    Limits.install(Map.put(Limits.effective(), :max_rendered_text_bytes, 2))

    for html <- [
          "<p lang=tr style='text-transform:uppercase'>ii</p>",
          "<p style='text-transform:capitalize'>ŉ</p>"
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render(html, system_font_discovery: false)

      assert diagnostic.stage == :style and diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.module == HtmlValidator
      assert diagnostic.message =~ "max_rendered_text_bytes"
    end
  end

  test "deferred context work is admitted before allocating case tokens" do
    Limits.install(Map.put(Limits.effective(), :max_css_work, 4))
    alias NativeElixirPdfUtilities.HtmlToPdf.TextTransform

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               TextTransform.with_document(fn ->
                 {:ok,
                  TextTransform.defer(
                    %{type: :text, text: "abcde", style: %{text_transform: :capitalize}},
                    []
                  )}
               end)
             end)

    assert diagnostic.stage == :style and diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "max_css_work"
    assert Process.get({TextTransform, :pending}) == nil
  end
end
