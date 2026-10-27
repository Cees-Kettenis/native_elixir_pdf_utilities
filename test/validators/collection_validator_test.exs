defmodule NativeElixirPdfUtilities.Validators.CollectionValidatorTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.CollectionValidator

  test "proper lists are admitted through their exact configured boundary" do
    assert {:ok, 0} = CollectionValidator.bounded_length([], 0)
    assert {:ok, 2} = CollectionValidator.bounded_length([:a, :b], 2)
    assert :limit_exceeded = CollectionValidator.bounded_length([:a, :b], 1)
    assert :limit_exceeded = CollectionValidator.bounded_length([:a | :unvisited], 0)
  end

  test "non-list values and improper tails fail before enumeration" do
    for value <- [:bad, nil, %{}, [:head | :bad], [:a, :b | :bad]] do
      assert :invalid = CollectionValidator.bounded_length(value, 3)
    end
  end
end
