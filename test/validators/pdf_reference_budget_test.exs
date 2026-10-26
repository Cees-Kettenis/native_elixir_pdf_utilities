defmodule NativeElixirPdfUtilities.PdfReferenceBudgetTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "alias cache memory does not multiply the terminal payload" do
    memory = fn value ->
      aliases = Map.new(1..32, &{{&1, 0}, %{value: {:ref, {&1 + 1, 0}}}})
      document = %{objects: Map.put(aliases, {33, 0}, %{value: value})}

      PdfValidator.with_reference_resolution([], fn opts ->
        for id <- 1..32 do
          assert {:ok, ^value} = PdfValidator.resolve(document, {:ref, {id, 0}}, opts)
        end

        :ets.all()
        |> Enum.filter(&(:ets.info(&1, :owner) == self()))
        |> Enum.map(&:ets.info(&1, :memory))
        |> Enum.sum()
      end)
    end

    small = memory.([1])
    large = memory.(Enum.to_list(1..2_048))
    assert large <= small * 2
  end

  test "fresh resolution caches share a reader's reference allowance", %{limits: limits} do
    Limits.install(%{limits | max_pdf_reference_resolution_work: 2})
    first = %{objects: %{{1, 0} => %{value: 1}}}
    second = %{objects: %{{1, 0} => %{value: 2}}}

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfValidator.with_reader_budget(fn ->
               assert {:ok, 1} = PdfValidator.resolve(first, {:ref, {1, 0}})
               assert {:ok, 2} = PdfValidator.resolve(second, {:ref, {1, 0}})
               PdfValidator.resolve(first, {:ref, {1, 0}})
             end)

    assert diagnostic.stage == :limits
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.message =~ "reference resolution work"
    assert {:ok, 1} = PdfValidator.resolve(first, {:ref, {1, 0}})
  end

  test "scope cleanup preserves generations, cycle errors and cached depth", %{limits: limits} do
    document = %{
      objects: %{
        {1, 2} => %{value: {:ref, {2, 3}}},
        {2, 3} => %{value: 42},
        {3, 0} => %{value: {:ref, {1, 2}}},
        {4, 0} => %{value: {:ref, {4, 0}}}
      }
    }

    owned = fn -> Enum.filter(:ets.all(), &(:ets.info(&1, :owner) == self())) end
    before = owned.()
    Limits.install(%{limits | max_pdf_reference_chain_depth: 2})

    PdfValidator.with_reference_resolution([], fn opts ->
      assert {:ok, 42} = PdfValidator.resolve(document, {:ref, {1, 2}}, opts)
      assert {:ok, 42} = PdfValidator.resolve(document, {:ref, {1, 2}}, opts)

      assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
               PdfValidator.resolve(document, {:ref, {3, 0}}, opts)

      assert {:error, {:invalid_pdf_input, %{stage: :resolution}}} =
               PdfValidator.resolve(document, {:ref, {4, 0}}, opts)
    end)

    assert owned.() == before

    assert_raise RuntimeError, fn ->
      PdfValidator.with_reference_resolution([], fn _ -> raise "scope failed" end)
    end

    assert owned.() == before
  end
end
