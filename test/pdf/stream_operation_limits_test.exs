defmodule NativeElixirPdfUtilities.Pdf.StreamOperationLimitsTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{Limits, Operation}
  alias NativeElixirPdfUtilities.Pdf.{IncrementalWriter, Reader}
  alias NativeElixirPdfUtilities.TestSupport.Paths
  alias NativeElixirPdfUtilities.Validators.PdfValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "standalone stream decoding admits the worker before resolving or decoding", %{
    limits: limits
  } do
    for {filter, encoded} <- [
          {nil, "abc"},
          {"Fl", :zlib.compress("abc")},
          {"AHx", "616263>"},
          {"A85", "@:E^~>"},
          {"RL", <<2, "abc", 128>>}
        ] do
      Limits.install(limits)
      {document, reference, alias_reference} = stream_document(encoded, filter)
      assert {:ok, "abc"} = Reader.decoded_stream(document, reference)

      Limits.install(%{limits | max_operation_memory_bytes: 1})

      for value <- [reference, alias_reference] do
        assert {:error, {:resource_limit_exceeded, diagnostic}} =
                 Reader.decoded_stream(document, value)

        assert diagnostic.stage == :limits
        assert diagnostic.reason == :resource_limit_exceeded
        assert diagnostic.module == Reader
        assert diagnostic.operation == :read
        assert diagnostic.message =~ "max_operation_memory_bytes"
      end

      Limits.install(limits)
      assert {:ok, "abc"} = Reader.decoded_stream(document, alias_reference)
    end
  end

  test "a small valid stream cannot expand beyond the operation memory allowance", %{
    limits: limits
  } do
    encoded = :binary.copy(<<129, ?A>>, 100_000) <> <<128>>
    {document, reference, alias_reference} = stream_document(encoded, "RunLengthDecode")
    Limits.install(%{limits | max_operation_memory_bytes: 8_000_000})
    assert {:ok, _} = Reader.read(document.binary)

    for value <- [reference, alias_reference] do
      assert {:error, {reason, diagnostic}} = Reader.decoded_stream(document, value)
      assert reason in [:resource_limit_exceeded, :operation_worker_failed]
      assert diagnostic.stage == :limits
      assert diagnostic.reason == reason
      assert diagnostic.module == Reader and diagnostic.operation == :read
      assert diagnostic.message =~ "max_operation_memory_bytes"
      if reason == :operation_worker_failed, do: assert(diagnostic.message =~ ":killed")
    end

    Limits.install(%{limits | max_operation_memory_bytes: 67_108_864})
    assert {:ok, decoded} = Reader.decoded_stream(document, alias_reference)
    assert byte_size(decoded) == 12_800_000
    assert decoded == :binary.copy("A", 12_800_000)
  end

  test "nested decoding retains the owning operation allowance and caller state", %{
    limits: limits
  } do
    {document, reference, _alias_reference} = stream_document("abc", nil)
    caller = self()
    flags = Process.info(caller, :trap_exit)
    monitors = Process.info(caller, :monitors)
    send(caller, :unrelated_stream_message)

    assert {:ok, worker} =
             Operation.run(__MODULE__, :outer_decode, fn ->
               request = Process.get({Operation, :request})
               Limits.install(%{limits | max_operation_memory_bytes: 1})
               assert {:ok, "abc"} = Reader.decoded_stream(document, reference)
               assert Process.get({Operation, :request}) == request
               {:ok, self()}
             end)

    assert worker != caller
    refute Process.alive?(worker)
    assert Process.get({Operation, :request}) == nil
    assert Process.info(caller, :trap_exit) == flags
    assert Process.info(caller, :monitors) == monitors
    assert_received :unrelated_stream_message
    refute_received {:DOWN, _, :process, _, _}
  end

  test "nested decodes retain aggregate reader charges and standalone failures leave no budget",
       %{limits: limits} do
    {document, reference, alias_reference} = stream_document("abc", nil)
    Limits.install(%{limits | max_pdf_reader_decoded_bytes: 5})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             Operation.run(__MODULE__, :aggregate_decode, fn ->
               PdfValidator.with_reader_budget(fn ->
                 assert {:ok, "abc"} = Reader.decoded_stream(document, reference)
                 Reader.decoded_stream(document, alias_reference)
               end)
             end)

    assert diagnostic.stage == :limits
    assert diagnostic.message =~ "max_pdf_reader_decoded_bytes"
    assert {:ok, "abc"} = Reader.decoded_stream(document, reference)
    assert {:ok, "abc"} = Reader.decoded_stream(document, alias_reference)

    Limits.install(%{limits | max_pdf_reader_decoded_bytes: 2})
    assert {:error, {:resource_limit_exceeded, _}} = Reader.decoded_stream(document, reference)
    Limits.install(limits)
    assert {:ok, "abc"} = Reader.decoded_stream(document, reference)
  end

  defp stream_document(encoded, filter) do
    assert {:ok, context} =
             Paths.fixture("pdf_reader/classic-xref.pdf")
             |> File.read!()
             |> Reader.read_validated()

    next = context.document.trailer["Size"]
    dictionary = if filter, do: %{"Filter" => {:name, filter}}, else: %{}

    assert {:ok, pdf} =
             IncrementalWriter.write(context, [
               {next, 0, {:stream, dictionary, encoded}},
               {next + 1, 0, {:value, {:ref, {next, 0}}}}
             ])

    assert {:ok, document} = Reader.read(pdf)
    {document, {:ref, {next, 0}}, {:ref, {next + 1, 0}}}
  end
end
