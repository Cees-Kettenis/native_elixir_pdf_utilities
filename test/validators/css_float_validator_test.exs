defmodule NativeElixirPdfUtilities.Validators.CssFloatValidatorTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.Validators.CssFloatValidator

  test "float blockification and item suppression preserve the owning formatting context" do
    for {inline, block} <- [
          {:inline, :block},
          {:inline_block, :block},
          {:inline_flex, :flex},
          {:inline_grid, :grid},
          {:inline_table, :table},
          {:block, :block}
        ] do
      assert {:ok, %{display: ^block}} =
               CssFloatValidator.compute_style(%{display: inline, float: :left})
    end

    assert {:ok, %{display: :inline}} =
             CssFloatValidator.compute_style(%{
               display: :inline,
               float: :left,
               position: :absolute
             })

    assert %{float: :none, clear: :none} =
             CssFloatValidator.prepare_item(%{float: :left, clear: :both})

    assert CssFloatValidator.prepare_item(%{font_size: 10}) == %{font_size: 10, _flow_root: true}

    for {source, expected} <- [
          {"inline-start", :inline_start},
          {"inline-end", :inline_end},
          {"right", :right}
        ] do
      assert {:ok, %{float: ^expected}} = CssFloatValidator.prepare("float", source)
    end

    assert {:ok, %{clear: :both}} = CssFloatValidator.prepare("clear", "BOTH")
    assert :error = CssFloatValidator.prepare("float", "both")
    assert :error = CssFloatValidator.prepare("unknown", "left")
    assert :ok = CssFloatValidator.validate_style(%{}, "empty")

    for style <- [
          %{float: :both},
          %{clear: :top},
          %{_float_placed: false},
          %{_float_preceding_width: 0},
          %{_float_line_height: 10}
        ] do
      assert {:error, {:invalid_layout, %{stage: :layout, source: "float", message: message}}} =
               CssFloatValidator.validate_style(style, "float")

      assert message =~ "float"
    end
  end
end
