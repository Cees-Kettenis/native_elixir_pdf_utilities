defmodule NativeElixirPdfUtilities.HtmlToPdf.CssColorPackagingTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Validators.{
    CssColorValidator,
    HtmlLineBreakValidator,
    NativeBackendValidator
  }

  test "a package missing its native binding returns canonical utility diagnostics for color and line breaking" do
    directory =
      Path.join(System.tmp_dir!(), "css-color-package-#{System.unique_integer([:positive])}")

    ebin = Path.join(directory, "native_elixir_pdf_utilities/ebin")
    File.mkdir_p!(ebin)
    on_exit(fn -> File.rm_rf!(directory) end)

    for file <- File.ls!(Application.app_dir(:native_elixir_pdf_utilities, "ebin")) do
      File.cp!(
        Application.app_dir(:native_elixir_pdf_utilities, "ebin/#{file}"),
        Path.join(ebin, file)
      )
    end

    result_path = Path.join(directory, "result.etf")
    coverage_path = Path.join(directory, "native-failure.coverdata")

    # A fresh VM loads the actual packaged Elixir modules with no native priv
    # artifacts. It cannot unload or replace a binding used by another test.
    script = """
    Application.load(:native_elixir_pdf_utilities)
    {:ok, _} = :cover.start()
    {:ok, #{inspect(CssColorValidator)}} = :cover.compile_beam(#{inspect(String.to_charlist(Path.join(ebin, "Elixir.NativeElixirPdfUtilities.Validators.CssColorValidator.beam")))})
    result =
      try do
        #{inspect(CssColorValidator)}.parse_function_color("rgb(1 2 3)", nil)
      catch
        value -> value
      end
    line_break = #{inspect(HtmlLineBreakValidator)}.opportunities("東京 A")
    File.write!(#{inspect(result_path)}, :erlang.term_to_binary({result, line_break}))
    :ok = :cover.export(#{inspect(String.to_charlist(coverage_path))})
    """

    {output, status} =
      System.cmd(System.find_executable("elixir"), ["-pa", ebin, "-e", script],
        stderr_to_stdout: true
      )

    assert status == 0, output

    assert {{:render_resource_limit, {:error, {:native_backend_unavailable, diagnostic}}},
            {:error, {:native_backend_unavailable, line_break_diagnostic}}} =
             result_path |> File.read!() |> :erlang.binary_to_term()

    assert line_break_diagnostic == diagnostic

    assert diagnostic.stage == :layout
    assert diagnostic.module == NativeBackendValidator
    assert diagnostic.source == "HTML utilities"
    assert diagnostic.operation == :load_native
    assert diagnostic.message =~ "native/html_backend"

    # Standard cover import combines actual subprocess execution with the
    # enclosing coverage run. Ordinary focused test runs need no cover server.
    if Process.whereis(:cover_server),
      do: assert(:ok == apply(:cover, :import, [String.to_charlist(coverage_path)]))
  end
end
