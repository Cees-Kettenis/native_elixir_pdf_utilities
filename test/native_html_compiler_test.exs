defmodule NativeElixirPdfUtilities.NativeHtmlCompilerTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.NativeBackend
  alias NativeElixirPdfUtilities.HtmlToPdf.NativeBackend.NativeBytes

  test "loaded native utilities remain usable after the compiler republishes its library" do
    assert {:module, NativeBytes} = NativeBackend.load_native()
    source = "red"
    assert {:ok, before} = NativeBytes.parse_css_color(source, false, [])

    for _ <- 1..2 do
      assert {:ok, []} = Mix.Tasks.Compile.HtmlBackend.run([])
      assert {:ok, ^before} = NativeBytes.parse_css_color(source, false, [])
    end
  end

  test "changed private binding fingerprints reload without module or NIF upgrade warnings" do
    import ExUnit.CaptureIO
    import ExUnit.CaptureLog
    assert {:module, NativeBytes} = NativeBackend.load_native()
    source = "red"
    assert {:ok, before} = NativeBytes.parse_css_color(source, false, [])
    manifest = Path.join([Mix.Project.build_path(), "html_backend", "bindings.sha256"])
    fingerprint = File.read!(manifest)
    File.write!(manifest, "prior binding revision")
    on_exit(fn -> File.write!(manifest, fingerprint) end)

    caller_options = Code.compiler_options()
    Code.compiler_options(debug_info: false)

    try do
      stderr =
        capture_io(:stderr, fn ->
          log = capture_log(fn -> assert {:ok, []} = Mix.Tasks.Compile.HtmlBackend.run([]) end)
          refute log =~ "warning"
        end)

      refute stderr =~ "warning"
      assert File.read!(manifest) == fingerprint
      assert {:ok, ^before} = NativeBytes.parse_css_color(source, false, [])

      assert [{3, false}, {7, true}, {8, true}] = NativeBytes.unicode_line_breaks("甲乙\nB")

      beam =
        Path.join([
          Mix.Project.app_path(),
          "priv/native",
          "Elixir.NativeElixirPdfUtilities.HtmlToPdf.NativeBackend.NativeBytes.beam"
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
