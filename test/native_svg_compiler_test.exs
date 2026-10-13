defmodule NativeElixirPdfUtilities.NativeSvgCompilerTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.SvgBackend

  test "loaded native artwork remains usable after the compiler republishes its library" do
    source =
      "<svg xmlns='http://www.w3.org/2000/svg' width='2' height='2'><rect width='2' height='2' fill='red'/></svg>"

    assert {:ok, before} = SvgBackend.rasterize(source, [], %{mode: :image})

    for _ <- 1..2 do
      assert {:ok, []} = Mix.Tasks.Compile.SvgBackend.run([])
      assert {:ok, ^before} = SvgBackend.rasterize(source, [], %{mode: :image})
    end
  end
end
