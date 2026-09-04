defmodule NativeElixirPdfUtilities.InfoTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.Info
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Pdf.InfoWriter
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Validators.InfoValidator

  @fixture_directory Path.expand("fixtures/pdf_reader", __DIR__)

  setup do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    :ok
  end

  test "reads common information fields and normalizes PDF dates" do
    pdf =
      pdf(
        base_objects() ++
          [
            {4,
             "<< /Title (August statement) /Author <FEFF00460069006E0061006E00630065> /Subject (Monthly) /Keywords (statement, monthly) /Producer (Fixture) /CreationDate (D:2026) /ModDate (D:20260825143000+08'00') >>"}
          ],
        "/Info 4 0 R"
      )

    assert {:ok,
            %{
              title: "August statement",
              author: "Finance",
              subject: "Monthly",
              keywords: "statement, monthly",
              producer: "Fixture",
              creation_date: ~N[2026-01-01 00:00:00],
              modification_date: ~N[2026-08-25 14:30:00]
            }} = Info.get(pdf)

    assert {:ok,
            %{
              title: nil,
              author: nil,
              subject: nil,
              keywords: nil,
              producer: nil,
              creation_date: nil,
              modification_date: nil
            }} = Info.get(pdf(base_objects()))
  end

  test "returns page count and effective rotated MediaBox sizes" do
    pdf =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2,
         "<< /Type /Pages /Kids [3 0 R 4 0 R] /Count 2 /MediaBox [10 20 110 220] /Rotate 90 >>"},
        {3, "<< /Type /Page /Parent 2 0 R >>"},
        {4, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 300 100] /Rotate -180 >>"}
      ])

    assert {:ok, 2} = Info.page_count(pdf)

    assert {:ok, sizes} = Info.page_sizes(pdf)

    assert sizes == [
             %{
               page_number: 1,
               width: 200.0,
               height: 100.0,
               unit: :point,
               rotation: 90,
               media_box: %{left: 10.0, bottom: 20.0, right: 110.0, top: 220.0}
             },
             %{
               page_number: 2,
               width: 300.0,
               height: 100.0,
               unit: :point,
               rotation: 180,
               media_box: %{left: 0.0, bottom: 0.0, right: 300.0, top: 100.0}
             }
           ]
  end

  test "applies direct and indirect page UserUnit values to physical dimensions" do
    pdf =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2,
         "<< /Type /Pages /Kids [3 0 R 4 0 R 5 0 R] /Count 3 /MediaBox [10 20 110 220] /Rotate 90 /UserUnit 4 >>"},
        {3, "<< /Type /Page /Parent 2 0 R /UserUnit 2 >>"},
        {4,
         "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 300 100] /Rotate 180 /UserUnit 6 0 R >>"},
        {5, "<< /Type /Page /Parent 2 0 R >>"},
        {6, "2.5"}
      ])

    assert {:ok, sizes} = Info.page_sizes(pdf)

    assert [direct, indirect, default] = sizes

    assert %{width: 400.0, height: 200.0, rotation: 90} = direct

    assert direct.media_box == %{
             left: 10.0,
             bottom: 20.0,
             right: 110.0,
             top: 220.0
           }

    assert %{width: 750.0, height: 250.0, rotation: 180} = indirect
    assert %{width: 200.0, height: 100.0, rotation: 90} = default
  end

  test "rejects malformed direct and indirect UserUnit values" do
    for {entry, extra_objects} <- [
          {"0", []},
          {"-1", []},
          {"null", []},
          {"/invalid", []},
          {"4 0 R", [{4, "(invalid)"}]},
          {"4 0 R", []}
        ] do
      malformed =
        pdf(
          [
            {1, "<< /Type /Catalog /Pages 2 0 R >>"},
            {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
            {3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 200] /UserUnit #{entry} >>"}
          ] ++ extra_objects
        )

      assert {:error,
              {:invalid_pdf_input,
               %{
                 stage: :page_tree,
                 operation: :page_sizes,
                 module: Info,
                 source: "page 1",
                 message: "page 1 has a UserUnit value that is not a positive number"
               }}} = Info.page_sizes(malformed)
    end
  end

  test "detects encryption without loading encrypted objects" do
    encrypted =
      pdf(
        base_objects() ++ [{4, "<< /Filter /Standard >>"}],
        "/Encrypt 4 0 R"
      )

    assert {:ok, true} = Info.encrypted?(encrypted)
    assert {:ok, false} = Info.encrypted?(pdf(base_objects(), "/Encrypt null"))

    assert {:error,
            {:encrypted_pdf,
             %{
               stage: :encryption,
               reason: :encrypted_pdf,
               operation: :get_info,
               module: Info
             }}} = Info.get(encrypted)

    assert {:error, {:encrypted_pdf, %{operation: :page_count}}} = Info.page_count(encrypted)
    assert {:error, {:encrypted_pdf, %{operation: :page_sizes}}} = Info.page_sizes(encrypted)

    assert {:error, {:encrypted_pdf, %{operation: :put_info}}} =
             Info.put(encrypted, title: "Cannot update")
  end

  test "patches information through an incremental revision and preserves unknown fields" do
    original =
      pdf(
        base_objects() ++
          [
            {4,
             "<< /Title (Old title) /Author (Old author) /Creator (Fixture creator) /CreationDate (D:20260102) >>"}
          ],
        "/Info 4 0 R"
      )

    assert {:ok, updated} =
             Info.put(original,
               title: "Penyata bulanan",
               author: nil,
               keywords: ["statement", "monthly"],
               modification_date: ~N[2026-08-25 14:30:00]
             )

    assert String.starts_with?(updated, original)
    assert updated != original

    assert {:ok,
            %{
              title: "Penyata bulanan",
              author: nil,
              keywords: "statement, monthly",
              creation_date: ~N[2026-01-02 00:00:00],
              modification_date: ~N[2026-08-25 14:30:00]
            }} = Info.get(updated)

    assert {:ok, document} = Reader.read(updated)
    assert {:ok, info_dictionary} = Reader.dictionary(document, document.trailer["Info"])
    assert info_dictionary["Creator"] == {:string, "Fixture creator"}

    assert {:ok, ^updated} = Info.put(updated, %{})
  end

  test "accepts calendar, ISO 8601, and canonical PDF date inputs" do
    original = pdf(base_objects())

    assert {:ok, with_date} =
             Info.put(original,
               creation_date: ~D[2026-08-25],
               modification_date: "2026-08-25T14:30:00+08:00"
             )

    assert {:ok,
            %{
              creation_date: ~N[2026-08-25 00:00:00],
              modification_date: ~N[2026-08-25 14:30:00]
            }} = Info.get(with_date)

    assert {:ok, with_pdf_date} = Info.put(with_date, modification_date: "D:202612")
    assert {:ok, %{modification_date: ~N[2026-12-01 00:00:00]}} = Info.get(with_pdf_date)
  end

  test "appends information updates to supported xref and object-stream inputs" do
    for fixture <- [
          "classic-xref.pdf",
          "xref-stream.pdf",
          "object-stream.pdf",
          "hybrid-xref.pdf",
          "incremental-update.pdf"
        ] do
      original = File.read!(Path.join(@fixture_directory, fixture))
      assert {:ok, updated} = Info.put(original, title: "Updated #{fixture}")
      assert String.starts_with?(updated, original)
      assert {:ok, %{title: "Updated " <> ^fixture}} = Info.get(updated)
      assert {:ok, 1} = Info.page_count(updated)
    end
  end

  test "preserves the permanent trailer identifier and updates its revision identifier" do
    first = "00112233445566778899AABBCCDDEEFF"
    second = "FFEEDDCCBBAA99887766554433221100"

    original = pdf(base_objects(), "/ID [<#{first}> <#{second}>]")
    assert {:ok, updated} = Info.put(original, title: "Identified")
    assert {:ok, document} = Reader.read(updated)

    assert [{:hex, decoded_first}, {:hex, decoded_second}] = document.trailer["ID"]
    assert Base.encode16(decoded_first) == first
    refute Base.encode16(decoded_second) == second
    assert byte_size(decoded_second) == 16
  end

  test "enforces information value and aggregate byte limits" do
    original_limits = Limits.effective()

    Limits.install(%{
      original_limits
      | max_pdf_info_value_bytes: 4,
        max_pdf_info_total_bytes: 6
    })

    pdf = pdf(base_objects())

    assert {:error,
            {:resource_limit_exceeded, %{stage: :limits, operation: :put_info, module: Info}}} =
             Info.put(pdf, title: "12345")

    assert {:error,
            {:resource_limit_exceeded, %{stage: :limits, operation: :put_info, module: Info}}} =
             Info.put(pdf, title: "1234", author: "1234")
  end

  test "rejects an information update that would exceed the readable object count" do
    source =
      pdf(
        base_objects() ++
          [
            {4, "<< /Custom (four) >>"},
            {5, "<< /Custom (five) >>"}
          ]
      )

    original_limits = Limits.effective()
    Limits.install(%{original_limits | max_pdf_objects: 7})

    assert {:ok, updated} = Info.put(source, title: "Within the boundary")
    assert {:ok, _document} = Reader.read(updated)

    Limits.install(%{original_limits | max_pdf_objects: 6})

    assert {:ok, _document} = Reader.read(source)

    assert {:error,
            {:resource_limit_exceeded,
             %{
               stage: :limits,
               operation: :put_info,
               module: Info,
               message: "PDF object count cannot accommodate an information update"
             }}} = Info.put(source, title: "At the boundary")
  end

  test "internal validation and incremental writing reject malformed prepared contexts" do
    assert {:error, {:invalid_pdf_input, %{stage: :validation}}} =
             InfoValidator.prepare_page_sizes(:malformed)

    assert {:error, {:invalid_pdf_input, %{stage: :validation}}} =
             InfoValidator.prepare_info(:malformed)

    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
             InfoWriter.write(:malformed, %{})

    source = pdf(base_objects() ++ [{4, "<< /Custom (value) >>"}], "/Info 4 0 R")
    assert {:ok, context} = Reader.read_validated(source)

    assert {:error, {:invalid_pdf_input, %{stage: :input}}} =
             InfoValidator.prepare_write(context, :malformed)

    malformed_reference = put_in(context.document.trailer["Info"], {:ref, {99, 0}})

    assert {:error, {:invalid_pdf_input, %{stage: :info}}} =
             InfoValidator.prepare_info(malformed_reference)

    assert {:error, {:invalid_pdf_input, %{stage: :info}}} =
             InfoValidator.prepare_write(malformed_reference, %{})

    unresolved_title =
      put_in(context.document.objects[{4, 0}].value["Title"], {:ref, {99, 0}})

    assert {:error, {:invalid_pdf_input, %{stage: :info}}} =
             InfoValidator.prepare_info(unresolved_title)

    malformed_value = put_in(context.document.objects[{4, 0}].value["Custom"], self())

    assert {:error, {:invalid_pdf_input, %{stage: :info}}} =
             InfoValidator.prepare_write(malformed_value, %{})

    malformed_page = %{context | pages: [:malformed]}

    assert {:error, {:invalid_pdf_input, %{stage: :page_tree}}} =
             InfoValidator.prepare_page_sizes(malformed_page)

    assert {:error, {:invalid_pdf_input, %{stage: :info}}} =
             InfoValidator.normalize_new_metadata(title: nil)
  end

  test "internal incremental writer validates size, root, identifiers, and separators" do
    source = pdf(base_objects())
    assert {:ok, context} = Reader.read_validated(source)

    malformed_size = put_in(context.document.trailer["Size"], :invalid)

    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
             InfoWriter.write(malformed_size, %{})

    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
             InfoValidator.validate_incremental_object_capacity(%{}, :malformed)

    malformed_root = put_in(context.document.trailer["Root"], :invalid)

    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
             InfoWriter.write(malformed_root, %{})

    for identifier <- [[:malformed], [{:string, "one"}, {:name, "two"}]] do
      malformed_id = put_in(context.document.trailer["ID"], identifier)

      assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
               InfoWriter.write(malformed_id, %{})
    end

    malformed_dictionary = %{1 => self()}

    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
             InfoWriter.write(context, malformed_dictionary)

    original_limits = Limits.effective()
    Limits.install(%{original_limits | max_pdf_objects: 2})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             InfoWriter.write(context, %{})

    Limits.install(original_limits)

    no_trailing_newline = String.trim_trailing(source)
    assert {:ok, context_without_newline} = Reader.read_validated(no_trailing_newline)
    assert {:ok, updated} = InfoWriter.write(context_without_newline, %{})
    assert String.starts_with?(updated, no_trailing_newline <> "\n")

    empty_context = %{
      document: %{
        binary: "",
        trailer: %{"Size" => 1, "Root" => {:ref, {1, 0}}},
        xref: %{},
        xref_offset: 0
      }
    }

    assert {:ok, "%PDF" <> _rest} =
             InfoWriter.write(put_in(empty_context.document.binary, "%PDF"), %{})

    assert {:ok, "\n" <> _rest} = InfoWriter.write(empty_context, %{})
  end

  test "returns focused diagnostics for invalid inputs, information, dates, and page sizes" do
    assert {:error, {:invalid_pdf_input, %{stage: :input, operation: :put_info, module: Info}}} =
             Info.put(:not_a_pdf, unknown: "value")

    assert {:error, {:invalid_pdf_input, %{stage: :input}}} =
             Info.put(pdf(base_objects()), title: "one", title: "two")

    assert {:error, {:invalid_pdf_input, %{stage: :input}}} =
             Info.put(pdf(base_objects()), :invalid)

    assert {:error, {:invalid_pdf_input, %{stage: :info, operation: :put_info, module: Info}}} =
             Info.put(pdf(base_objects()), creation_date: "2026-99-99")

    malformed_info =
      pdf(base_objects() ++ [{4, "<< /CreationDate (not a date) >>"}], "/Info 4 0 R")

    assert {:error, {:invalid_pdf_input, %{stage: :info, operation: :get_info, module: Info}}} =
             Info.get(malformed_info)

    unsupported_info = pdf(base_objects() ++ [{4, "<< /Title 12 >>"}], "/Info 4 0 R")
    assert {:error, {:invalid_pdf_input, %{stage: :info}}} = Info.get(unsupported_info)

    invalid_text_info = pdf(base_objects() ++ [{4, "<< /Title <00> >>"}], "/Info 4 0 R")
    assert {:error, {:invalid_pdf_input, %{stage: :info}}} = Info.get(invalid_text_info)

    malformed_page =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 0 100] >>"}
      ])

    assert {:error,
            {:invalid_pdf_input,
             %{stage: :page_tree, operation: :page_sizes, module: Info, source: "page 1"}}} =
             Info.page_sizes(malformed_page)

    malformed_rotation =
      pdf([
        {1, "<< /Type /Catalog /Pages 2 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
        {3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 100] /Rotate 45 >>"}
      ])

    assert {:error, {:invalid_pdf_input, %{stage: :page_tree}}} =
             Info.page_sizes(malformed_rotation)

    assert {:error, {:invalid_pdf_input, %{stage: :input, operation: :encryption_status}}} =
             Info.encrypted?(:invalid)
  end

  defp base_objects do
    [
      {1, "<< /Type /Catalog /Pages 2 0 R >>"},
      {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
      {3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 200] >>"}
    ]
  end

  defp pdf(objects, trailer_extra \\ "") do
    header = "%PDF-1.7\n"

    {body, offsets, position} =
      Enum.reduce(objects, {[], %{}, byte_size(header)}, fn {id, value},
                                                            {body, offsets, position} ->
        object = "#{id} 0 obj\n#{value}\nendobj\n"

        {
          [object | body],
          Map.put(offsets, id, position),
          position + byte_size(object)
        }
      end)

    size = objects |> Enum.map(&elem(&1, 0)) |> Enum.max() |> Kernel.+(1)

    entries =
      Enum.map_join(1..(size - 1), fn id ->
        case Map.get(offsets, id) do
          nil -> "0000000000 00000 f \n"
          offset -> "#{offset |> Integer.to_string() |> String.pad_leading(10, "0")} 00000 n \n"
        end
      end)

    IO.iodata_to_binary([
      header,
      Enum.reverse(body),
      "xref\n0 #{size}\n",
      "0000000000 65535 f \n",
      entries,
      "trailer\n<< /Size #{size} /Root 1 0 R #{trailer_extra} >>\n",
      "startxref\n#{position}\n%%EOF\n"
    ])
  end
end
