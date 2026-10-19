defmodule NativeElixirPdfUtilities.HtmlToPdf.CssColorPackagingTest do
  use ExUnit.Case

  test "packaged BEAM modules handle colors, line breaking and forms without native artifacts" do
    directory =
      Path.join(System.tmp_dir!(), "elixir-runtime-package-#{System.unique_integer([:positive])}")

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

    script = """
    Application.load(:native_elixir_pdf_utilities)
    alias NativeElixirPdfUtilities.Validators.{CssColorValidator,HtmlLineBreakValidator,HtmlPatternValidator,HtmlUrlValidator}
    results = {
      CssColorValidator.parse_function_color("rgb(10 20 30)",nil),
      HtmlLineBreakValidator.opportunities("甲乙\\nB"),
      HtmlPatternValidator.initial_pattern("[[a-z]&&[^aeiou]]+",["bcdf"]),
      HtmlUrlValidator.valid_initial_input_url?("https://example.test")
    }
    File.write!(#{inspect(result_path)},:erlang.term_to_binary(results))
    """

    {output, status} =
      System.cmd(System.find_executable("elixir"), ["-pa", ebin, "-e", script],
        stderr_to_stdout: true
      )

    assert status == 0, output

    assert {{:ok, {r, g, b}}, {:ok, [{3, false}, {7, true}, {8, true}]},
            {:ok, %{compiled?: true, mismatch: false}}, {:ok, true}} =
             result_path |> File.read!() |> :erlang.binary_to_term()

    assert_in_delta r, 10 / 255, 1.0e-9
    assert_in_delta g, 20 / 255, 1.0e-9
    assert_in_delta b, 30 / 255, 1.0e-9
  end
end
