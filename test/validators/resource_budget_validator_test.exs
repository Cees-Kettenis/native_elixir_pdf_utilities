defmodule NativeElixirPdfUtilities.Validators.ResourceBudgetValidatorTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Limits, Operation}

  alias NativeElixirPdfUtilities.Validators.{
    FontValidator,
    HtmlValidator,
    PdfValidator,
    ResourceBudgetValidator
  }

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "an image read cannot reuse source bytes consumed by another stage" do
    Limits.install(Map.put(Limits.effective(), :max_input_bytes, 5))

    ResourceBudgetValidator.with_budget(fn ->
      assert :ok = ResourceBudgetValidator.reserve(:max_input_bytes, 5, :font)

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlValidator.image_source_read_limit(HtmlValidator.new_image_budget())

      assert diagnostic.stage == :limits
      assert diagnostic.message =~ "remaining max_input_bytes"
    end)
  end

  test "shared reservations expose cumulative snapshots and release counters after exceptions" do
    Limits.install(Map.put(Limits.effective(), :max_work, 3))

    for _ <- 1..2 do
      assert_raise RuntimeError, "stop operation", fn ->
        ResourceBudgetValidator.with_budget(fn ->
          assert ResourceBudgetValidator.snapshot() == %{}
          assert :ok = ResourceBudgetValidator.reserve(:max_work, 0, :layout)
          assert ResourceBudgetValidator.snapshot() == %{}
          assert :ok = ResourceBudgetValidator.reserve(:max_work, 2, :layout)

          ResourceBudgetValidator.with_budget(fn ->
            assert ResourceBudgetValidator.snapshot() == %{max_work: 2}
            assert :ok = ResourceBudgetValidator.reserve(:max_work, 1, :font)
            assert ResourceBudgetValidator.snapshot() == %{max_work: 3}
          end)

          raise "stop operation"
        end)
      end

      assert ResourceBudgetValidator.snapshot() == nil
      assert ResourceBudgetValidator.remaining(:max_work) == 3
    end
  end

  test "font, render and reader stages consume one allowance without resetting nested loops" do
    original = Limits.effective()

    for limit <- [:max_work, :max_entries, :max_input_bytes, :max_decoded_bytes] do
      Limits.install(Map.put(original, limit, 3))

      ResourceBudgetValidator.with_budget(fn ->
        assert :ok = FontValidator.with_budget(fn -> FontValidator.reserve(limit, 1) end)

        assert :ok =
                 HtmlValidator.with_render_budget(fn ->
                   HtmlValidator.reserve_render_resource(limit, 1, :layout)
                 end)

        assert ResourceBudgetValidator.remaining(limit) == 1

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 PdfValidator.with_reader_budget(fn ->
                   PdfValidator.charge_reader_budget(limit, 2)
                 end)

        assert diagnostic.stage == :limits and diagnostic.operation == :read
        assert diagnostic.message =~ Atom.to_string(limit)
        assert ResourceBudgetValidator.remaining(limit) == 1

        assert :ok = FontValidator.with_budget(fn -> FontValidator.reserve(limit, 1) end)
        assert ResourceBudgetValidator.remaining(limit) == 0

        assert {:error, {:resource_limit_exceeded, %{stage: :layout}}} =
                 HtmlValidator.with_render_budget(fn ->
                   HtmlValidator.reserve_render_resource(limit, 1, :layout)
                 end)
      end)

      assert ResourceBudgetValidator.remaining(limit) == 3
    end
  end

  test "read allowances reflect resources consumed by an enclosing rendering stage" do
    Limits.install(Map.put(Limits.effective(), :max_input_bytes, 5))

    HtmlValidator.with_render_budget(fn ->
      HtmlValidator.reserve_render_resource(:max_input_bytes, 3, :html)
      assert FontValidator.with_budget(fn -> FontValidator.read_limit() end) == 2
    end)
  end

  test "image sources and decoded pixels join the allowance already consumed by font work" do
    limits = Limits.effective()

    for {limit, reserve_image} <- [
          {:max_input_bytes, fn budget -> HtmlValidator.reserve_image_source(budget, 3) end},
          {:max_decoded_bytes,
           fn budget -> HtmlValidator.reserve_decoded_image(budget, 1, 1, 3) end}
        ] do
      Limits.install(Map.put(limits, limit, 5))

      FontValidator.with_budget(fn ->
        FontValidator.reserve(limit, 3)

        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 reserve_image.(HtmlValidator.new_image_budget())

        assert diagnostic.message =~ Atom.to_string(limit)
      end)
    end
  end

  test "filled fields reuse the same admitted font bytes across their appearance loops" do
    alias NativeElixirPdfUtilities.{Forms, HtmlToPdf}
    alias NativeElixirPdfUtilities.TestSupport.ResourceBudget
    original = Limits.effective()
    html = Enum.map_join(1..6, &"<input name='field#{&1}' value='Initial'>")
    assert {:ok, pdf} = HtmlToPdf.render(html, system_font_discovery: false)
    one = %{"field1" => "Updated"}
    many = Map.new(1..6, &{"field#{&1}", "Updated"})

    assert {{:ok, _}, bytes} =
             ResourceBudget.measure_operation(:max_input_bytes, fn -> Forms.fill(pdf, one) end)

    assert {{:ok, _}, ^bytes} =
             ResourceBudget.measure_operation(:max_input_bytes, fn -> Forms.fill(pdf, many) end)

    Limits.install(%{original | max_input_bytes: bytes})
    assert {:ok, _} = Forms.fill(pdf, many)
    Limits.install(%{original | max_input_bytes: bytes - 1})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Forms.fill(pdf, many)
    assert diagnostic.operation == :fill and diagnostic.module == Forms
    assert diagnostic.message =~ "max_input_bytes"
  end

  test "sequential form workers check live memory rather than adding released working sets" do
    alias NativeElixirPdfUtilities.Validators.HtmlFormValueValidator
    Limits.install(%{Limits.effective() | max_operation_memory_bytes: 3})

    HtmlValidator.with_render_budget(fn ->
      for _ <- 1..2 do
        assert {:ok, :matched} =
                 HtmlFormValueValidator.result(
                   {:ok, :matched, %{compile: 0, matches: 0, memory: 3}}
                 )
      end

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlFormValueValidator.result(
                 {:ok, :matched, %{compile: 0, matches: 0, memory: 4}}
               )

      assert diagnostic.operation == :initial_form_value
      assert diagnostic.message =~ "max_operation_memory_bytes"
    end)
  end

  test "nested public operations share their worker's resource scope and completed operations release it" do
    Limits.install(Map.put(Limits.effective(), :max_work, 3))

    for _ <- 1..2 do
      assert {:error, {:resource_limit_exceeded, %{operation: :nested}}} =
               Operation.run(__MODULE__, :outer, fn ->
                 assert :ok = ResourceBudgetValidator.reserve(:max_work, 2, :layout)

                 Operation.run(__MODULE__, :nested, fn ->
                   ResourceBudgetValidator.reserve(:max_work, 2, :layout, operation: :nested)
                 end)
               end)
    end

    assert ResourceBudgetValidator.remaining(:max_work) == 3
    assert :ok = ResourceBudgetValidator.reserve(:max_work, 3, :layout)
    assert ResourceBudgetValidator.remaining(:max_work) == 3

    assert {:error, {:resource_limit_exceeded, _}} =
             ResourceBudgetValidator.reserve(:max_work, 4, :layout)
  end
end
