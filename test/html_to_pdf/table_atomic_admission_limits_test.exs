defmodule NativeElixirPdfUtilities.HtmlToPdf.TableAtomicAdmissionLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlTableValidator

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    :ok
  end

  test "marker geometry validation does not count the same paint box twice" do
    marker = %{
      type: :flow_marker,
      role: :atomic_table_line,
      x: 0.0,
      y: 0.0,
      width: 0.0,
      height: 100.0,
      flow_id: :line,
      fragment_lines: true,
      fragment_id: {:line, :line, 0}
    }

    Limits.install(%{Limits.effective() | max_layout_boxes: 1})
    assert :ok = HtmlTableValidator.validate_atomic_pagination([marker], [marker], 100.0)
  end
end
