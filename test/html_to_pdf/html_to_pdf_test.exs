defmodule NativeElixirPdfUtilities.HtmlToPdfTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf

  test "render exposes the planned facade while behavior is pending" do
    assert HtmlToPdf.render("<p>Hello</p>") == {:error, :not_implemented}
  end

  test "render_file reads input before delegating to the renderer" do
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.html")
    output_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.pdf")

    File.write!(input_path, "<p>Hello</p>")

    assert HtmlToPdf.render_file(input_path, output_path) == {:error, :not_implemented}
    refute File.exists?(output_path)
    assert HtmlToPdf.render_file(input_path <> ".missing", output_path) == {:error, :enoent}
    assert HtmlToPdf.render_file(:bad_input, output_path) == {:error, :invalid_path}
  after
    input_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.html")
    output_path = Path.join(System.tmp_dir!(), "native-elixir-pdf-html-to-pdf-test.pdf")

    File.rm(input_path)
    File.rm(output_path)
  end
end
