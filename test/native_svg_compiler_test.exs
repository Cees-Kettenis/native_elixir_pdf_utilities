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

  test "changed private binding fingerprints reload without module or NIF upgrade warnings" do
    import ExUnit.CaptureIO
    import ExUnit.CaptureLog
    source = "<svg width='2' height='2'><rect width='2' height='2' fill='red'/></svg>"
    assert {:ok, before} = SvgBackend.rasterize(source, [], %{mode: :image})
    manifest = Path.join([Mix.Project.build_path(), "svg_backend", "bindings.sha256"])
    fingerprint = File.read!(manifest)
    File.write!(manifest, "prior binding revision")
    on_exit(fn -> File.write!(manifest, fingerprint) end)

    caller_options = Code.compiler_options()
    Code.compiler_options(debug_info: false)

    try do
      stderr =
        capture_io(:stderr, fn ->
          log = capture_log(fn -> assert {:ok, []} = Mix.Tasks.Compile.SvgBackend.run([]) end)
          refute log =~ "warning"
        end)

      refute stderr =~ "warning"
      assert File.read!(manifest) == fingerprint
      assert {:ok, ^before} = SvgBackend.rasterize(source, [], %{mode: :image})

      assert {:ok, legacy_work} =
               apply(
                 NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes,
                 :validate_shape_work,
                 [source, 100]
               )

      assert is_integer(legacy_work) and legacy_work > 0

      beam =
        Path.join([
          Mix.Project.app_path(),
          "priv/native",
          "Elixir.NativeElixirPdfUtilities.HtmlToPdf.SvgBackend.NativeBytes.beam"
        ])

      assert {:ok, {module, [debug_info: {:debug_info_v1, backend, debug}]}} =
               :beam_lib.chunks(String.to_charlist(beam), [:debug_info])

      refute debug == :none
      assert {:ok, _core} = apply(backend, :debug_info, [:core_v1, module, debug, []])
      assert Code.compiler_options().debug_info == false
    after
      Code.compiler_options(caller_options)
    end

    assert Code.compiler_options() == caller_options
  end
end
