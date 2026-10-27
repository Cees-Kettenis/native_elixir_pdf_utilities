defmodule NativeElixirPdfUtilities.Validators.PdfFormImportBudgetTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Operation
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.Validators.{PdfFormValidator, PdfValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    # Use the genuine writer/reader/incremental interfaces to retain valid xrefs.
    assert {:ok, pdf} = PdfWriter.render([%{size: {100, 60}, boxes: []}], compress_streams: false)
    assert {:ok, context} = Reader.read_validated(pdf)
    {:ok, context: context}
  end

  test "combined stream bytes and separators share one admission allowance", %{context: context} do
    source = source(context, [{%{}, "q\n"}, {%{}, "Q\n"}], [0, 1])
    Limits.install(Map.put(Limits.effective(), :max_pdf_reader_decoded_bytes, 3))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfFormValidator.prepare_source(source)

    assert diagnostic.stage == :limits
    assert diagnostic.message =~ "max_pdf_reader_decoded_bytes"

    Limits.install(Map.put(Limits.effective(), :max_pdf_reader_decoded_bytes, 5))
    assert {:ok, prepared} = PdfFormValidator.prepare_source(source)
    assert prepared.stream == "q\n\nQ\n"
  end

  test "standalone imports share parsing and object-stream decoding with later Contents decoding" do
    assert {:ok, context} =
             Reader.read_validated(File.read!(Paths.fixture("pdf_reader/object-stream.pdf")))

    [page] = context.pages
    {id, generation} = page.ref
    contents = page.dictionary["Contents"]

    assert {:ok, source} =
             IncrementalWriter.write(context, [
               {id, generation, {:value, Map.put(page.dictionary, "Contents", [contents])}}
             ])

    assert {:ok, content} = Reader.decoded_stream(context.document, contents)

    parse_bytes =
      Operation.run(__MODULE__, :measure_source, fn ->
        PdfValidator.with_reader_budget(fn ->
          before = PdfValidator.remaining_reader_decoded_bytes()
          assert {:ok, _} = Reader.read(source)
          before - PdfValidator.remaining_reader_decoded_bytes()
        end)
      end)

    assert parse_bytes > 0

    Limits.install(
      Map.put(
        Limits.effective(),
        :max_pdf_reader_decoded_bytes,
        parse_bytes + byte_size(content) - 1
      )
    )

    assert {:ok, _} = Reader.read(source)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfFormValidator.prepare_source(source)

    assert diagnostic.message =~ "max_pdf_reader_decoded_bytes"

    Limits.install(
      Map.put(Limits.effective(), :max_pdf_reader_decoded_bytes, parse_bytes + byte_size(content))
    )

    assert {:ok, %{stream: ^content}} = PdfFormValidator.prepare_source(source)
  end

  test "repeated raw and compressed occurrences count before a later malformed stream", %{
    context: context
  } do
    for {dictionary, bytes} <- [
          {%{}, "q Q"},
          {%{"Filter" => {:name, "FlateDecode"}}, :zlib.compress("q Q")}
        ] do
      source =
        source(
          context,
          [{dictionary, bytes}, {%{"Filter" => {:name, "FlateDecode"}}, "invalid"}],
          [0, 0, 1]
        )

      Limits.install(Map.put(Limits.effective(), :max_pdf_reader_decoded_bytes, 6))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               PdfFormValidator.prepare_source(source)

      assert diagnostic.stage == :limits

      assert diagnostic.message =~
               if(dictionary == %{},
                 do: "max_pdf_reader_decoded_bytes",
                 else: "decoded stream exceeds its safety limit"
               )
    end
  end

  test "empty occurrences still charge separators and public drawing preserves the failure", %{
    context: context
  } do
    source = source(context, [{%{}, ""}], [0, 0, 0])
    Limits.install(Map.put(Limits.effective(), :max_pdf_reader_decoded_bytes, 1))

    image = %{
      format: :pdf_form,
      data: source,
      width_px: 100,
      height_px: 60,
      prepared: %{stream: "forged"}
    }

    box = %{type: :image, x: 0, y: 0, width: 100, height: 60, image: image}

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfWriter.render([%{size: {100, 60}, boxes: [box]}])

    assert diagnostic.stage == :limits
    assert diagnostic.message =~ "max_pdf_reader_decoded_bytes"
    assert diagnostic.operation == :write_pdf

    Limits.install(Map.put(Limits.effective(), :max_pdf_reader_decoded_bytes, 2))
    assert {:ok, prepared} = PdfFormValidator.prepare_source(source)
    assert prepared.stream == "\n\n"
    assert {:ok, _pdf} = PdfWriter.render([%{size: {100, 60}, boxes: [box]}])
  end

  test "both writer entry points parse a repeated source once and ignore supplied preparation", %{
    context: context
  } do
    source = source(context, [{%{}, "q"}, {%{}, "Q"}], [0, 1])

    image = %{
      format: :pdf_form,
      data: source,
      width_px: 100,
      height_px: 60,
      prepared: %{stream: "forged"}
    }

    box = %{type: :image, x: 0, y: 0, width: 100, height: 60, image: image}
    page = %{size: {100, 60}, boxes: [box, box]}
    assert :erlang.trace_pattern({Reader, :read, 1}, true, [:local]) == 1

    try do
      for kind <- [:pages, :appearances] do
        task =
          Task.async(fn ->
            receive do
              :render -> :ok
            end

            case kind do
              :pages ->
                assert {:ok, pdf} = PdfWriter.render([page, page])
                refute pdf =~ "forged"

              :appearances ->
                assert {:ok, objects, [_ref], _next} = PdfWriter.appearance_objects([page], 1)
                assert objects != []
            end

            :ok
          end)

        :erlang.trace(task.pid, true, [:call, :set_on_spawn])
        send(task.pid, :render)
        assert :ok = Task.await(task)
        assert_receive {:trace, _, :call, {Reader, :read, [^source]}}
        refute_receive {:trace, _, :call, {Reader, :read, _}}
      end
    after
      :erlang.trace_pattern({Reader, :read, 1}, false, [:local])
    end
  end

  test "nested source caches restore their caller and do not survive changed allowances", %{
    context: context
  } do
    source = source(context, [{%{}, "q"}], [0, 0])

    {{:ok, outer}, sources} =
      PdfFormValidator.with_prepared_sources(fn ->
        assert {:ok, first} = PdfFormValidator.prepare_source(source)

        assert {{:ok, ^first}, %{^source => ^first}} =
                 PdfFormValidator.with_prepared_sources(fn ->
                   PdfFormValidator.prepare_source(source)
                 end)

        PdfFormValidator.prepare_source(source)
      end)

    assert sources[source] == outer and outer.stream == "q\nq"
    Limits.install(Map.put(Limits.effective(), :max_pdf_reader_decoded_bytes, 2))
    assert {:error, {:resource_limit_exceeded, _}} = PdfFormValidator.prepare_source(source)
    assert Process.get({PdfFormValidator, :prepared_sources}) == nil
  end

  test "an active reader allowance also covers source import and failures release their scope", %{
    context: context
  } do
    source = source(context, [{%{}, "q"}], [0, 0])
    Limits.install(Map.put(Limits.effective(), :max_pdf_reader_decoded_bytes, 3))

    assert {:error, {:resource_limit_exceeded, _}} =
             Operation.run(__MODULE__, :import_with_reader, fn ->
               PdfValidator.with_reader_budget(fn ->
                 PdfValidator.charge_reader_budget(:max_pdf_reader_decoded_bytes, 1)
                 PdfFormValidator.prepare_source(source)
               end)
             end)

    assert {:ok, %{stream: "q\nq"}} = PdfFormValidator.prepare_source(source)
  end

  defp source(context, streams, order) do
    [page] = context.pages
    first = context.document.trailer["Size"]
    contents = Enum.map(order, &{:ref, {first + &1, 0}})

    objects =
      streams
      |> Enum.with_index(first)
      |> Enum.map(fn {{dictionary, bytes}, id} -> {id, 0, {:stream, dictionary, bytes}} end)

    {id, generation} = page.ref

    assert {:ok, pdf} =
             IncrementalWriter.write(
               context,
               objects ++
                 [{id, generation, {:value, Map.put(page.dictionary, "Contents", contents)}}]
             )

    pdf
  end
end
