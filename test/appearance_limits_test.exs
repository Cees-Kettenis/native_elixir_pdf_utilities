defmodule NativeElixirPdfUtilities.AppearanceLimitsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Stamp, Info}
  alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    %{limits: original}
  end

  test "charges repeated stamp text before generating appearances", %{limits: limits} do
    {:ok, pdf} = HtmlToPdf.render("<p>one</p><p style='break-before:page'>two</p>")
    Limits.install(%{limits | max_appearance_text_bytes: 10})
    assert {:ok, _} = Stamp.text(pdf, "12345")

    for function <- [&Stamp.text/2, &Stamp.watermark/2] do
      assert {:error, {:resource_limit_exceeded, %{stage: :limits, message: message}}} =
               function.(pdf, "123456")

      assert message =~ "aggregate"
    end

    Limits.install(%{limits | max_appearance_widgets: 1})
    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} = Stamp.page_numbers(pdf)
  end

  test "bounds form widget and text expansion across selected fields", %{limits: limits} do
    {:ok, pdf} =
      HtmlToPdf.render(
        "<div><input name='a'><input name='b'><select name='c'><option value='x'>Long label</option></select></div>"
      )

    Limits.install(%{limits | max_appearance_text_bytes: 10})
    assert {:ok, _} = Forms.fill(pdf, %{"a" => "12345", "b" => "12345"})

    assert {:error, {:resource_limit_exceeded, %{source: source}}} =
             Forms.fill(pdf, %{"a" => "123456", "b" => "12345"})

    assert source in ["a", "b"]
    assert {:error, {:resource_limit_exceeded, _}} = Forms.fill(pdf, %{"c" => "x"})

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlToPdf.render("<div><input value='12345678901'></div>")

    Limits.install(%{limits | max_appearance_widgets: 1})
    assert {:error, {:resource_limit_exceeded, _}} = Forms.flatten(pdf)

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlToPdf.render("<div><input name='a' value='Old'></div>")
  end

  test "incremental operations cannot return unreadable oversized output", %{limits: limits} do
    {:ok, pdf} = HtmlToPdf.render("<div><input name='a' value='Old'></div>")
    Limits.install(%{limits | max_pdf_input_bytes: byte_size(pdf) + 100})

    assert {:error, {:resource_limit_exceeded, %{stage: :incremental_write}}} =
             Forms.fill(pdf, %{"a" => "New"})

    assert {:error, {:resource_limit_exceeded, %{stage: :incremental_write}}} =
             Stamp.text(pdf, "New")

    Limits.install(%{limits | max_rendered_pdf_bytes: byte_size(pdf) + 100})

    assert {:error, {:resource_limit_exceeded, %{stage: :incremental_write}}} =
             Info.put(pdf, title: String.duplicate("x", 200))

    Limits.install(limits)
    {:ok, context} = Reader.read_validated(pdf)
    {:ok, output} = IncrementalWriter.write(context, [])
    Limits.install(%{limits | max_rendered_pdf_bytes: byte_size(output) - 1})
    assert {:error, {:resource_limit_exceeded, _}} = IncrementalWriter.write(context, [])
    Limits.install(%{limits | max_rendered_pdf_bytes: byte_size(output)})
    assert {:ok, ^output} = IncrementalWriter.write(context, [])
  end
end
