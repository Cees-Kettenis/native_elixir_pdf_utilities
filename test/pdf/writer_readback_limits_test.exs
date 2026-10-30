defmodule NativeElixirPdfUtilities.Pdf.WriterReadbackLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Merge, Outlines, Stamp}
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.TestSupport.Paths

  alias NativeElixirPdfUtilities.Pdf.{
    AssemblyWriter,
    IncrementalWriter,
    InfoCodec,
    OutlineWriter,
    Reader
  }

  alias NativeElixirPdfUtilities.Validators.MergeValidator
  alias NativeElixirPdfUtilities.Validators.OutlineValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator
  alias NativeElixirPdfUtilities.Validators.WriterValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 100}, boxes: []}])
    %{limits: limits, pdf: pdf}
  end

  test "arrays and dictionaries share inclusive serialization and readback boundaries", ctx do
    Limits.install(%{ctx.limits | max_pdf_container_entries: 4})

    for value <- [
          [],
          %{},
          [1, 2, 3, 4],
          %{"A" => 1, "B" => 2, "C" => 3, "D" => 4},
          [[1, 2, 3, 4]],
          %{"A" => [1, 2, 3, 4]}
        ] do
      assert {:ok, encoded} = InfoCodec.serialize_value(value)
      assert :ok = PdfValidator.validate_output_containers(encoded)
    end

    for value <- [
          List.duplicate(1, 5),
          Map.new(1..5, &{to_string(&1), 1}),
          [List.duplicate(1, 5)],
          %{"Nested" => List.duplicate(1, 5)}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = InfoCodec.serialize_value(value)
      assert diagnostic.stage == :limits
      assert diagnostic.message =~ "max_pdf_container_entries (4)"
    end

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             PdfValidator.validate_container([1 | :tail])

    assert diagnostic.message =~ "proper list"
  end

  test "generated object checks count references and repeated keys, leaving stream bytes opaque",
       ctx do
    Limits.install(%{ctx.limits | max_pdf_container_entries: 2})
    assert :ok = PdfValidator.validate_output_containers("[1 0 R 2 0 R]")
    assert :ok = PdfValidator.validate_output_containers("<< /A 1 /A 2 >>")

    assert :ok =
             PdfValidator.validate_output_containers(
               "<< /Length 9 >>\nstream\n[[[[[[[[[\nendstream"
             )

    assert :ok =
             PdfValidator.validate_output_containers(
               "<< /Length 7 0 R >>\nstream\nembedded endstream @@@ [[[\nendstream"
             )

    for body <- ["[1 0 R 2 0 R 3 0 R]", "<< /A 1 /A 2 /A 3 >>"] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               PdfValidator.validate_output_containers(body,
                 operation: :render,
                 source: "object 7 0"
               )

      assert diagnostic.operation == :render
      assert diagnostic.source == "object 7 0"
    end

    for body <- ["[   ", "@@@"] do
      assert {:error, {_reason, diagnostic}} = PdfValidator.validate_output_containers(body)
      assert is_binary(diagnostic.message)
    end

    assert :ok = WriterValidator.reserve_output_containers("[]")

    assert {:render_resource_limit, {:error, {:resource_limit_exceeded, _}}} =
             catch_throw(WriterValidator.reserve_output_containers("[1 2 3]"))
  end

  test "merge and render reject a generated Kids array above the shared cap", ctx do
    Limits.install(%{ctx.limits | max_pdf_container_entries: 8})

    for count <- [7, 8, 9] do
      for operation <- [
            fn -> Merge.merge(List.duplicate(ctx.pdf, count)) end,
            fn -> PdfWriter.render(List.duplicate(%{size: {100, 100}, boxes: []}, count)) end
          ] do
        if count <= 8 do
          assert {:ok, output} = operation.()
          assert {:ok, _} = Reader.read(output)
        else
          assert {:error, {:resource_limit_exceeded, diagnostic}} = operation.()
          assert diagnostic.stage == :limits
          assert diagnostic.message =~ "max_pdf_container_entries"
        end
      end
    end
  end

  test "inherited page entries and incremental stream Length use the same cap", ctx do
    assert {:ok, context} = Reader.read_validated(ctx.pdf)
    assert {:ok, input} = MergeValidator.prepare(context)
    assert {:ok, [input]} = MergeValidator.prepare_remapping([input], 3)

    # The input dictionary fits, but materializing inherited Rotate adds an entry.
    [page] = input.pages
    input = put_in(input.inherited[page].rotate, [{:int, 0}])
    Limits.install(%{ctx.limits | max_pdf_container_entries: 5})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = AssemblyWriter.write([input])
    assert diagnostic.message =~ "max_pdf_container_entries"
    assert is_binary(diagnostic.source)

    object = context.document.trailer["Size"]
    dictionary = Map.new(1..5, &{to_string(&1), 0})

    assert {:error, {:resource_limit_exceeded, _}} =
             IncrementalWriter.write(context, [{object, 0, {:stream, dictionary, ""}}])
  end

  test "generated annotation arrays use the inclusive container boundary", ctx do
    Limits.install(%{ctx.limits | max_pdf_container_entries: 12})

    for count <- [12, 13] do
      boxes =
        for index <- 1..count do
          %{
            type: :text,
            text: "A",
            font: "Helvetica",
            font_size: 8,
            x: index * 3,
            y: 20,
            width: 3,
            color: {0, 0, 0},
            link_url: "https://example.com/"
          }
        end

      result = PdfWriter.render([%{size: {100, 100}, boxes: boxes}])

      if count == 12 do
        assert {:ok, output} = result
        assert {:ok, _} = Reader.read(output)
      else
        assert {:error, {:resource_limit_exceeded, diagnostic}} = result
        assert diagnostic.message =~ "max_pdf_container_entries"
      end
    end
  end

  test "embedded font width and text-adjustment arrays cannot exceed the shared cap", ctx do
    assert {:ok, registry} =
             Font.load_registry(
               fonts: [{"Fixture Sans", Paths.project("priv/fonts/dejavu/DejaVuSans.ttf")}]
             )

    assert {:ok, _, font} = Font.resolve("Fixture Sans", 400, :normal, registry)
    glyph = font.cmap[?A]
    font = Map.put(font, :kerning, %{{glyph, glyph} => -100})
    Limits.install(%{ctx.limits | max_pdf_container_entries: 12})

    for kind <- [:kerning, :explicit, :widths], count <- [6, 7] do
      text =
        if kind == :widths,
          do: binary_part("ABCDEFG", 0, count),
          else: String.duplicate("A", count)

      box = %{
        type: :text,
        text: text,
        font: Font.pdf_name(font),
        font_face: font,
        font_size: 10.1,
        x: 0,
        y: 20,
        color: {0, 0, 0}
      }

      box =
        if kind == :explicit,
          do:
            Map.merge(box, %{
              glyph_ids: List.duplicate(glyph, count),
              snap_to_css_pixel_grid: true
            }),
          else: box

      result = PdfWriter.render([%{size: {100, 100}, boxes: [box]}])

      if count == 6 do
        assert {:ok, output} = result
        assert {:ok, _} = Reader.read(output)
      else
        assert {:error, {:resource_limit_exceeded, diagnostic}} = result
        assert diagnostic.message =~ "max_pdf_container_entries"
      end
    end
  end

  test "generated fixed dictionaries and outline serialization propagate container diagnostics",
       ctx do
    page = %{size: {100, 100}, boxes: []}
    Limits.install(%{ctx.limits | max_pdf_container_entries: 4})
    assert {:error, {:resource_limit_exceeded, _}} = PdfWriter.render([page])

    Limits.install(%{ctx.limits | max_pdf_container_entries: 5})
    assert {:ok, output} = PdfWriter.render([page])
    assert {:ok, _} = Reader.read(output)

    outline = {"Parent", 1, [{"Child", 1}]}

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfWriter.render([page], outlines: [outline])

    assert diagnostic.message =~ "max_pdf_container_entries"
  end

  test "serialization counts scalar and empty-container boundaries like the reader", ctx do
    for maximum <- [1, 2, 100, 200], leaf <- [0, [], %{}], kind <- [:array, :dictionary] do
      Limits.install(%{ctx.limits | max_pdf_value_depth: maximum})
      leaf_depth = if is_list(leaf) or is_map(leaf), do: 1, else: 0

      value =
        Enum.reduce(List.duplicate(:container, maximum - leaf_depth), leaf, fn _, child ->
          if kind == :array, do: [child], else: %{"Child" => child}
        end)

      assert {:ok, encoded} = InfoCodec.serialize_value(value)
      assert is_binary(IO.iodata_to_binary(encoded))

      for excessive <- [[value], %{"Child" => value}] do
        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 InfoCodec.serialize_value(excessive)

        assert diagnostic.stage == :limits
        assert diagnostic.reason == :resource_limit_exceeded
        assert diagnostic.message == "PDF value nesting depth exceeds the #{maximum}-level limit"
      end
    end
  end

  test "incremental values, streams and copied trailers propagate depth diagnostics", ctx do
    assert {:ok, context} = Reader.read_validated(ctx.pdf)
    object = context.document.trailer["Size"]
    Limits.install(%{ctx.limits | max_pdf_value_depth: 2})

    for body <- [
          {:value, %{"Custom" => [[]]}},
          {:stream, %{"Custom" => %{"Nested" => %{}}}, ""}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               IncrementalWriter.write(context, [{object, 0, body}])

      assert diagnostic.stage == :limits
      assert diagnostic.message == "PDF value nesting depth exceeds the 2-level limit"
    end

    context = put_in(context.document.trailer["Info"], %{"Custom" => []})

    assert {:error, {:resource_limit_exceeded, diagnostic}} = IncrementalWriter.write(context, [])
    assert diagnostic.stage == :limits
    assert diagnostic.message == "PDF value nesting depth exceeds the 2-level limit"
  end

  test "assembled outline objects propagate the shared depth diagnostic", ctx do
    assert {:ok, outlined} = Outlines.put(ctx.pdf, [{"Page", 1}])
    assert {:ok, context} = Reader.read_validated(outlined)
    assert {:ok, input} = MergeValidator.prepare(context)
    assert {:ok, inputs} = MergeValidator.prepare_remapping([input], 3)
    Limits.install(%{ctx.limits | max_pdf_value_depth: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} = AssemblyWriter.write(inputs)
    assert diagnostic.stage == :limits
    assert diagnostic.message == "PDF value nesting depth exceeds the 1-level limit"
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
