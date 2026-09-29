defmodule NativeElixirPdfUtilities.AppearanceLimitsTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{Forms, HtmlToPdf, Limits, Stamp, Info}
  alias NativeElixirPdfUtilities.Pdf.{Reader, IncrementalWriter}

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    %{limits: original}
  end

  test "incremental writes reserve xref revisions before returning output", %{limits: limits} do
    assert {:ok, pdf} = HtmlToPdf.render("<p>Original</p>")
    assert {:ok, context} = Reader.read_validated(pdf)
    assert context.document.xref_revisions == 1
    Limits.install(%{limits | max_pdf_xref_revisions: 1})
    assert {:ok, ^pdf} = Info.put(pdf, [])

    for {module, operation, result} <- [
          {Info, :put_info, Info.put(pdf, title: "New")},
          {Stamp, :stamp_text, Stamp.text(pdf, "New")}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = result
      assert diagnostic.stage == :incremental_write
      assert diagnostic.reason == :resource_limit_exceeded
      assert diagnostic.module == module
      assert diagnostic.operation == operation
      assert diagnostic.message =~ "max_pdf_xref_revisions"
    end

    Limits.install(%{limits | max_pdf_xref_revisions: 2})
    assert {:ok, updated} = Info.put(pdf, title: "New")
    assert {:ok, %{title: "New"}} = Info.get(updated)
    assert {:ok, updated_context} = Reader.read_validated(updated)
    assert updated_context.document.xref_revisions == 2
    assert {:error, {:resource_limit_exceeded, _}} = Info.put(updated, title: "Again")
    malformed = update_in(context.document, &Map.delete(&1, :xref_revisions))

    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
             IncrementalWriter.write(malformed, [])
  end

  test "fill and flatten reserves both revisions while empty updates remain no-ops", %{
    limits: limits
  } do
    assert {:ok, pdf} = HtmlToPdf.render("<div><input name='a' value='Old'></div>")
    assert {:ok, context} = Reader.read_validated(pdf)
    count = context.document.xref_revisions
    Limits.install(%{limits | max_pdf_xref_revisions: count})
    assert {:ok, ^pdf} = Forms.fill(pdf, %{}, flatten: true)
    assert {:ok, ^pdf} = Forms.flatten(pdf, fields: [])
    Limits.install(%{limits | max_pdf_xref_revisions: count + 1})
    assert {:ok, updated} = Forms.fill(pdf, %{"a" => "New"})
    assert {:ok, _} = Reader.read(updated)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Forms.fill(pdf, %{"a" => "New"}, flatten: true)

    assert diagnostic.operation == :fill
    assert diagnostic.module == Forms
    assert diagnostic.message =~ "xref revisions"
    Limits.install(%{limits | max_pdf_xref_revisions: count + 2})
    assert {:ok, flattened} = Forms.fill(pdf, %{"a" => "New"}, flatten: true)
    assert {:ok, []} = Forms.fields(flattened)
    assert {:ok, final} = Reader.read_validated(flattened)
    assert final.document.xref_revisions == count + 2
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
