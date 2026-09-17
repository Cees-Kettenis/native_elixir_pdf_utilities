defmodule NativeElixirPdfUtilities.HtmlToPdf.CssVariableLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "unused doubling chains fail with actionable render diagnostics" do
    declarations =
      "--v0:x;" <>
        Enum.map_join(1..30, fn index ->
          "--v#{index}:var(--v#{index - 1})var(--v#{index - 1});"
        end)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<div style='#{declarations}'>Hello</div>")

    assert diagnostic.stage == :limits
    assert diagnostic.reason == :resource_limit_exceeded
    assert diagnostic.module == HtmlToPdf
    assert diagnostic.operation == :render
    assert diagnostic.message =~ "max_css_variable_bytes"
  end

  test "byte checks account for multibyte values, prefixes, suffixes and repeated references" do
    Limits.install(Map.put(Limits.effective(), :max_css_variable_bytes, 20))

    assert {:ok, "éééééééééé"} =
             HtmlValidator.resolve_css_variables("var(--x)var(--x)", %{"--x" => "ééééé"}, %{})

    for value <- ["xvar(--x)var(--x)", "var(--x)var(--x)x", String.duplicate("x", 21)] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlValidator.resolve_css_variables(value, %{"--x" => "ééééé"}, %{})

      assert diagnostic.message =~ "max_css_variable_bytes"
    end
  end

  test "ordinary declaration substitution propagates resource diagnostics" do
    Limits.install(Map.put(Limits.effective(), :max_css_variable_bytes, 20))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<p style='--x:1234567890;width:var(--x)var(--x)x'>A</p>")

    assert diagnostic.stage == :limits
    assert diagnostic.message =~ "max_css_variable_bytes"
  end

  test "aggregate budget spans separate properties and elements in one render" do
    Limits.install(Map.put(Limits.effective(), :max_css_variable_total_bytes, 6))
    budget = HtmlValidator.new_css_budget()

    assert {:ok, %{"--a" => "abc", "--b" => "def"}} =
             HtmlValidator.compute_custom_properties(%{"--a" => "abc", "--b" => "def"}, budget)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.compute_custom_properties(%{"--c" => "x"}, budget)

    assert diagnostic.message =~ "max_css_variable_total_bytes"

    Limits.install(Map.put(Limits.effective(), :max_css_variable_total_bytes, 200))
    html = String.duplicate("<p style='--unused:#{String.duplicate("x", 50)}'>A</p>", 10)
    assert {:error, {:resource_limit_exceeded, diagnostic}} = HtmlToPdf.render(html)
    assert diagnostic.message =~ "max_css_variable_total_bytes"
  end

  test "dependency depth is bounded even when a dependency is memoized" do
    Limits.install(Map.put(Limits.effective(), :max_css_variable_depth, 2))

    assert {:ok, %{"--a" => "x", "--b" => "x"}} =
             HtmlValidator.compute_custom_properties(%{"--a" => "x", "--b" => "var(--a)"})

    for properties <- [
          %{"--a" => "x", "--b" => "var(--a)", "--c" => "var(--b)"},
          %{"--a" => "var(--b)", "--b" => "var(--c)", "--c" => "x"}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlValidator.compute_custom_properties(properties)

      assert diagnostic.message =~ "max_css_variable_depth"
    end
  end

  test "work counts repeated references and dependencies are computed once" do
    Limits.install(Map.put(Limits.effective(), :max_css_variable_work, 4))
    properties = %{"--a" => "x", "--b" => "var(--a)var(--a)"}

    assert {:ok, %{"--a" => "x", "--b" => "xx"}} =
             HtmlValidator.compute_custom_properties(properties)

    Limits.install(Map.put(Limits.effective(), :max_css_variable_work, 3))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.compute_custom_properties(properties)

    assert diagnostic.message =~ "max_css_variable_work"
  end

  test "missing, cyclic and inherited invalid properties remain invalid" do
    assert {:ok, properties} =
             HtmlValidator.compute_custom_properties(%{
               "--a" => "var(--b)",
               "--b" => "var(--a)",
               "--c" => nil,
               "--d" => "var(--missing)",
               "--e" => "var(--d)"
             })

    assert Enum.all?(properties, fn {_name, value} -> is_nil(value) end)
    assert :error = HtmlValidator.resolve_css_variables("var(--a)", properties, %{})
  end

  test "shared dependencies consume their computed byte budget only once" do
    Limits.install(Map.put(Limits.effective(), :max_css_variable_total_bytes, 7))

    properties = %{
      "--a" => "x",
      "--b" => "var(--a)var(--a)",
      "--c" => "var(--b)var(--b)"
    }

    assert {:ok, %{"--a" => "x", "--b" => "xx", "--c" => "xxxx"}} =
             HtmlValidator.compute_custom_properties(properties)

    Limits.install(Map.put(Limits.effective(), :max_css_variable_total_bytes, 6))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.compute_custom_properties(properties)

    assert diagnostic.message =~ "max_css_variable_total_bytes"
  end

  test "work budgets span calls but separate renders receive fresh budgets" do
    Limits.install(Map.put(Limits.effective(), :max_css_variable_work, 1))
    budget = HtmlValidator.new_css_budget()

    assert {:ok, "x"} =
             HtmlValidator.resolve_css_variables("var(--a)", %{"--a" => "x"}, %{}, budget)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.resolve_css_variables("var(--a)", %{"--a" => "x"}, %{}, budget)

    assert diagnostic.stage == :limits
    assert diagnostic.message =~ "max_css_variable_work"

    for _ <- 1..2 do
      assert {:ok, _pdf} = HtmlToPdf.render("<p style='--unused:x'>A</p>")
    end
  end
end
