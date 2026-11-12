defmodule NativeElixirPdfUtilities.HtmlRenderBudgetTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  setup do
    limits = Limits.effective()
    Limits.install(Map.put(limits, :max_work, 7))
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "absent and explicit empty scope values preserve standalone limit checks" do
    key = {HtmlValidator, :render_budget}

    for initial <- [:missing, nil, :undefined] do
      if initial == :missing, do: Process.delete(key), else: Process.put(key, initial)
      before = :erlang.get(key)
      assert :ok = HtmlValidator.reserve_render_resource(:max_work, 7, :html)
      assert :erlang.get(key) == before

      assert {:render_resource_limit, {:error, {:resource_limit_exceeded, diagnostic}}} =
               catch_throw(HtmlValidator.reserve_render_resource(:max_work, 8, :html))

      assert diagnostic.stage == :html
      assert diagnostic.message =~ "max_work"
      assert :erlang.get(key) == before
    end
  end

  test "nested scopes share accumulated reservations and clean up after success" do
    key = {HtmlValidator, :render_budget}

    assert :ok =
             HtmlValidator.with_render_budget(fn ->
               assert :ok = HtmlValidator.reserve_render_resource(:max_work, 3, :layout)

               assert :ok =
                        HtmlValidator.with_render_budget(fn ->
                          HtmlValidator.reserve_render_resource(:max_work, 4, :layout)
                        end)

               assert NativeElixirPdfUtilities.Validators.ResourceBudgetValidator.snapshot() == %{
                        max_work: 7
                      }

               assert :ok = HtmlValidator.reserve_render_resource(:max_work, 0, :layout)

               assert NativeElixirPdfUtilities.Validators.ResourceBudgetValidator.snapshot() == %{
                        max_work: 7
                      }

               :ok
             end)

    refute Process.get(key)
  end

  test "an excess accumulated reservation keeps the shared diagnostic and cleans up" do
    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(:max_work, 4, :layout)
               HtmlValidator.reserve_render_resource(:max_work, 4, :layout)
             end)

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "max_work"
    assert diagnostic.module == HtmlValidator
    refute Process.get({HtmlValidator, :render_budget})
  end

  test "a zero reservation still enforces a newly lowered limit" do
    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               HtmlValidator.reserve_render_resource(:max_work, 4, :layout)
               Limits.install(Map.put(Limits.effective(), :max_work, 3))
               HtmlValidator.reserve_render_resource(:max_work, 0, :layout)
             end)

    assert diagnostic.stage == :layout
    assert diagnostic.message =~ "configured limit 3"
    refute Process.get({HtmlValidator, :render_budget})
  end
end
