defmodule NativeElixirPdfUtilities.Pdf.WriterReadbackLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Outlines, Stamp}
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, InfoCodec, OutlineWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.OutlineValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: []}])
    %{limits: limits, pdf: pdf}
  end

  test "generated and incremental writers include object zero at exact readback boundaries",
       ctx do
    for operation <- [
          fn -> HtmlToPdf.render("<p>Hello</p>", system_font_discovery: false) end,
          fn -> Stamp.text(ctx.pdf, "Mark") end,
          fn -> Outlines.put(ctx.pdf, [{"Page", 1}]) end
        ] do
      Limits.install(ctx.limits)
      assert {:ok, output} = operation.()
      assert {:ok, prepared} = Reader.read_validated(output)
      count = map_size(prepared.document.xref)

      for allowance <- [count - 1, count, count + 1] do
        Limits.install(%{ctx.limits | max_pdf_objects: allowance})

        case allowance < count do
          true ->
            assert {:error, {:resource_limit_exceeded, diagnostic}} = operation.()
            assert diagnostic.reason == :resource_limit_exceeded
            assert diagnostic.message =~ "object"

          false ->
            assert {:ok, output} = operation.()
            assert {:ok, _} = Reader.read_validated(output)
        end
      end
    end
  end

  test "sparse xrefs retain separate entry and allocated-ID bounds", ctx do
    assert {:ok, source} = Reader.read_validated(ctx.pdf)
    assert {:ok, sparse} = IncrementalWriter.write(source, [{8, 0, {:value, %{}}}])
    assert {:ok, prepared} = Reader.read_validated(sparse)
    assert prepared.document.trailer["Size"] == 9
    assert map_size(prepared.document.xref) == 6
    Limits.install(%{ctx.limits | max_pdf_objects: 10})
    assert {:ok, updated} = Outlines.put(sparse, [{"Page", 1}])
    assert {:ok, prepared} = Reader.read_validated(updated)
    assert map_size(prepared.document.xref) == 8
    assert prepared.document.trailer["Size"] == 11

    Limits.install(%{ctx.limits | max_pdf_objects: 9})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = Outlines.put(sparse, [{"Page", 1}])
    assert diagnostic.message =~ "object IDs"
    Limits.install(%{ctx.limits | max_pdf_objects: 8})
    assert {:ok, removed} = Outlines.put(sparse, [])
    assert {:ok, _} = Reader.read_validated(removed)
  end

  test "the shared incremental serializer rejects unsupported copied trailer entries", ctx do
    assert {:ok, context} = Reader.read_validated(ctx.pdf)
    context = put_in(context.document.trailer["Info"], :unsupported)
    assert {:error, {:invalid_pdf_input, diagnostic}} = IncrementalWriter.write(context, [])
    assert diagnostic.stage == :incremental_write
    assert diagnostic.message =~ "trailer cannot be serialized"
  end

  test "outline additions and removals reserve an incremental revision before writing", ctx do
    Limits.install(%{ctx.limits | max_pdf_xref_revisions: 1})

    for items <- [[], [{"Page", 1}]] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = Outlines.put(ctx.pdf, items)
      assert diagnostic.stage == :incremental_write
      assert diagnostic.operation == :put_outlines and diagnostic.module == Outlines
      assert diagnostic.message =~ "max_pdf_xref_revisions"
    end

    assert {:ok, context} = Reader.read_validated(ctx.pdf)
    assert {:error, {:resource_limit_exceeded, _}} = OutlineWriter.write(context, [])
    Limits.install(%{ctx.limits | max_pdf_xref_revisions: 2})
    assert {:ok, updated} = Outlines.put(ctx.pdf, [{"Page", 1}])
    assert {:ok, _} = Reader.read_validated(updated)
  end

  test "outline destination numbers follow magnitude and serialized token limits", ctx do
    huge = Integer.pow(10, 1_500)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Outlines.put(ctx.pdf, [%{title: "Huge", page: 1, view: {:fit_h, huge}}])

    assert diagnostic.operation == :put_outlines
    assert diagnostic.source == "outline destination"
    assert diagnostic.message =~ "max_pdf_numeric_magnitude"

    tiny = -1.0e-100
    assert {:ok, encoded} = InfoCodec.serialize_value(tiny)
    bytes = IO.iodata_length(encoded)

    for kind <- [:fit_h, :fit_v, :fit_bh, :fit_bv] do
      items = [%{title: "Tiny", page: 1, view: {kind, tiny}}]
      Limits.install(%{ctx.limits | max_pdf_numeric_token_bytes: bytes - 1})
      assert {:error, {:resource_limit_exceeded, diagnostic}} = Outlines.put(ctx.pdf, items)
      assert diagnostic.message =~ "max_pdf_numeric_token_bytes"
      assert diagnostic.source == "outline destination"
      Limits.install(%{ctx.limits | max_pdf_numeric_token_bytes: bytes})
      assert {:ok, updated} = Outlines.put(ctx.pdf, items)
      assert {:ok, [%{view: {^kind, actual}}]} = Outlines.get(updated)
      assert actual == tiny
    end

    Limits.install(ctx.limits)

    for view <- [{:fit_r, 0, 0, huge, 100}, {:xyz, nil, huge, nil}] do
      assert {:error, {:resource_limit_exceeded, _}} =
               OutlineValidator.normalize([%{title: "Huge", page: 1, view: view}], 1)
    end
  end
end
