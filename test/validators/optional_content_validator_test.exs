defmodule NativeElixirPdfUtilities.Validators.OptionalContentValidatorTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.OptionalContentValidator, as: Layers

  test "standalone link normalization still accepts an empty object list" do
    assert {:ok, []} =
             NativeElixirPdfUtilities.Validators.LinkActionValidator.normalize(
               %{objects: %{}},
               %{},
               []
             )
  end

  test "indirect presets, application rules and layer panel controls remain scoped to their groups" do
    g = {:ref, {4, 2}}
    other = {:ref, {15, 0}}
    usage = %{"Event" => {:name, "Print"}, "Category" => [{:name, "Print"}], "OCGs" => [g]}
    panel = [[{:string, "Plans"}, g]]
    default = %{"AS" => [usage], "Order" => panel, "Locked" => [g], "RBGroups" => [[g]]}
    keep = %{"Name" => {:string, "Keep"}, "BaseState" => {:name, "Unchanged"}, "OFF" => [g]}
    hide = %{"Name" => {:string, "Hide"}, "BaseState" => {:name, "OFF"}}

    properties = %{
      "OCGs" => {:ref, {1, 0}},
      "D" => {:ref, {2, 0}},
      "Configs" => [{:ref, {3, 0}}, hide]
    }

    context = %{
      catalog: %{"OCProperties" => properties},
      document: %{
        objects: %{
          {4, 2} => %{value: %{"Type" => {:name, "OCG"}}},
          {1, 0} => %{value: [g]},
          {2, 0} => %{value: default},
          {3, 0} => %{value: keep}
        }
      }
    }

    assert {:ok, prepared} = Layers.prepare(context)

    assert {:ok, combined} =
             Layers.combine([
               prepared,
               %{"OCGs" => [other], "D" => %{"Order" => [other]}, "Configs" => []}
             ])

    assert %{"OCGs" => [^g, ^other], "D" => initial, "Configs" => [keep, hide]} = combined
    assert initial["Order"] == panel ++ [other]
    assert initial["Locked"] == [g] and initial["AS"] == [usage]
    assert initial["RBGroups"] == [[g]] and initial["OFF"] == []

    for preset <- [keep, hide] do
      assert preset["BaseState"] == {:name, "Unchanged"}
      assert preset["ON"] == [other] and preset["OFF"] == [g]
      assert preset["Order"] == initial["Order"] and preset["RBGroups"] == [[g]]
      assert preset["Locked"] == [] and preset["AS"] == []
    end
  end

  test "each layer preset combines its own panel mode with the other inputs' modes" do
    visible = {:name, "VisiblePages"}
    all = {:name, "AllPages"}

    first = %{
      "OCGs" => [],
      "D" => %{"ListMode" => visible},
      "Configs" => [%{"ListMode" => all}, %{}]
    }

    second = %{"OCGs" => [], "D" => %{"ListMode" => visible}, "Configs" => []}

    for inputs <- [[first, second], [second, first]] do
      assert {:ok, %{"D" => %{"ListMode" => ^visible}, "Configs" => alternates}} =
               Layers.combine(inputs)

      assert Enum.map(alternates, & &1["ListMode"]) == [all, all]
    end
  end

  test "malformed or unresolved configurations return diagnostics" do
    g = {:ref, {2, 0}}
    doc = %{objects: %{{2, 0} => %{value: %{"Type" => {:name, "OCG"}}}}}

    for entry <- [
          1,
          %{"OCGs" => 42},
          %{"OCGs" => [], "D" => %{"ListMode" => {:name, "Unknown"}}},
          %{"OCGs" => [1]},
          %{"OCGs" => [], "D" => 9},
          %{"OCGs" => [], "D" => %{"Locked" => [g]}},
          %{"OCGs" => [g], "D" => %{"BaseState" => {:name, "Unknown"}}},
          %{"OCGs" => [g], "D" => %{"BaseState" => {:name, "Unchanged"}}}
        ] do
      assert {:error, {:invalid_pdf_input, %{stage: :optional_content}}} =
               Layers.prepare(%{document: doc, catalog: %{"OCProperties" => entry}})
    end

    assert {:error, {:invalid_pdf_input, %{stage: :optional_content}}} =
             Layers.prepare(%{
               document: doc,
               catalog: %{"OCProperties" => %{"OCGs" => [g], "D" => %{"ON" => [g], "OFF" => [g]}}}
             })

    for {intent, expected} <- [{[{:name, "View"}], {:name, "View"}}, {[], []}] do
      assert {:ok, %{"D" => %{"Intent" => ^expected}}} =
               Layers.prepare(%{
                 document: doc,
                 catalog: %{"OCProperties" => %{"OCGs" => [g], "D" => %{"Intent" => intent}}}
               })
    end

    assert {:error, {:invalid_pdf_input, %{stage: :resolution}}} =
             Layers.prepare(%{document: doc, catalog: %{"OCProperties" => {:ref, {99, 0}}}})
  end

  test "merged layer arrays and expanded presets obey the ordinary container and output limits" do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    first = %{"OCGs" => [{:ref, {1, 0}}], "D" => %{}, "Configs" => []}
    second = %{"OCGs" => [{:ref, {2, 0}}], "D" => %{}, "Configs" => [%{}, %{}]}
    assert {:ok, _} = Layers.combine([first, second])
    Limits.install(%{original | max_entries: 1})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             Layers.combine([first, second])

    assert {:error, {:resource_limit_exceeded, _}} =
             Layers.prepare(%{
               document: %{},
               catalog: %{"OCProperties" => %{"OCGs" => [], "Configs" => [%{}, %{}]}}
             })

    Limits.install(%{original | max_output_bytes: 128})
    assert {:error, {:resource_limit_exceeded, _}} = Layers.combine([first, second])
  end
end
