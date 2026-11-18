defmodule NativeElixirPdfUtilities.Validators.CssContainerQueryValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.CssContainerQueryValidator, as: Queries

  @container %{
    display: :block,
    container_type: :size,
    container_name: ["Chapter"],
    font_size: 12.0,
    _custom_properties: %{"--density" => "compact", "--arrangement" => "a /*keep*/ b"}
  }
  @size %{width: 120.0, height: 40.0}
  @viewport %{width: 400.0, height: 600.0}

  test "container declarations preserve case and reject unsupported names and types" do
    for {property, value, result} <- [
          {"container-type", "SIZE", %{container_type: :size}},
          {"container-name", "none", %{container_name: []}},
          {"container-name", "Chapter Chapter Footnote",
           %{container_name: ["Chapter", "Footnote"]}},
          {"container", "Chapter", %{container_name: ["Chapter"], container_type: :normal}}
        ] do
      assert {:ok, ^result} = Queries.declaration(property, value)
    end

    for {property, value} <- [
          {"container-type", "scroll-state"},
          {"container-type", "10"},
          {"container-name", ""},
          {"container-name", "123"},
          {"container-name", "none footnote"},
          {"container", "name/size/extra"}
        ] do
      assert :error = Queries.declaration(property, value)
    end
  end

  test "prepared queries evaluate nested size and style alternatives in their selected context" do
    for {source, expected} <- [
          {"Chapter", true},
          {"(inline-size) and (block-size) and (height)", true},
          {"Chapter ((block-size >= 30pt) and (aspect-ratio > 2))", true},
          {"(orientation:portrait) or (width:120pt) or (height > 50pt)", true},
          {"not (width < 1px)", true},
          {"(bogus-feature)", :unknown},
          {"(bogus-feature) and (width < 1px)", false},
          {"(aspect-ratio)", true},
          {"(bogus-feature) or (width > 1px)", true},
          {"style(--density)", true},
          {"style(--missing)", false},
          {"style((--density: compact))", true},
          {"style(not (--density: large))", true},
          {"style((--density:compact) and (--arrangement:a b))", true},
          {"style((--density: full) xor (--missing))", :unknown},
          {"style(unknown)", :unknown}
        ] do
      assert {:ok, [query]} = Queries.prepare(source)
      assert Queries.eligible?(query, @container)
      assert Queries.evaluate(query.expression, @container, @size, @viewport) == expected, source
    end

    assert {:ok, [first, second]} =
             Queries.prepare("chapter (width > 1pt), Chapter (height > 1pt)")

    refute Queries.eligible?(first, @container)
    assert Queries.eligible?(second, @container)
    refute Queries.eligible?(second, %{@container | container_type: :inline_size})

    refute Queries.eligible?(first, %{
             @container
             | display: :contents,
               container_name: ["chapter"]
           })
  end

  test "malformed conditions are discarded without activating dependent style" do
    for source <- [
          "",
          "none (width)",
          "not width",
          "(width) xor (height)",
          "(width) and (height) or (orientation:portrait)",
          "(width) and (height) and",
          "(width) and none",
          "(width) (height)",
          "Chapter [width]",
          "Chapter (width"
        ] do
      assert :error = Queries.prepare(source), source
    end
  end
end
