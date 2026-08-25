defmodule NativeElixirPdfUtilities.ValidatorsTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.Validators.MergeValidator
  alias NativeElixirPdfUtilities.Validators.PdfValidator
  alias NativeElixirPdfUtilities.Validators.TextResourceValidator
  alias NativeElixirPdfUtilities.Validators.TextValidator

  @fixture_directory Path.expand("fixtures/pdf_reader", __DIR__)

  test "binary validation is a reusable façade over reader parsing" do
    pdf = File.read!(Path.join(@fixture_directory, "classic-xref.pdf"))
    assert {:ok, %{document: %{pages: [_page]}}} = PdfValidator.validate_pdf(pdf)

    assert {:error, {:invalid_pdf_input, diagnostic}} = PdfValidator.validate_pdf(:invalid)
    assert diagnostic.operation == :validate_pdf
    assert diagnostic.module == PdfValidator
  end

  test "public PDF and merge input boundaries are owned by validators" do
    assert :ok = PdfValidator.validate_input("%PDF-1.7\n")
    assert {:error, {:invalid_pdf_input, %{stage: :input}}} = PdfValidator.validate_input(nil)

    assert {:error, {:invalid_pdf_input, %{stage: :header}}} =
             PdfValidator.validate_input("%PDF-9.0\n")

    assert {:ok, ["one", "two"]} = MergeValidator.validate_inputs(["one", "two"])
    assert {:error, {:empty_pdf_list, %{stage: :merge}}} = MergeValidator.validate_inputs([])

    assert {:error, {:invalid_pdf_input, %{stage: :merge}}} =
             MergeValidator.validate_inputs(["one", :invalid])
  end

  test "shared xref validation requires a valid object-zero free-list head" do
    pdf = "%PDF-1.7\n"
    trailer = %{"Size" => 2, "Root" => {:ref, {1, 0}}}
    valid_entries = %{0 => {:free, 0, 65_535}, 1 => {:uncompressed, 1, 0}}

    assert :ok = PdfValidator.validate_xref(valid_entries, trailer, pdf)
    assert :ok = PdfValidator.validate_xref_structure(valid_entries, trailer, pdf)
    assert :ok = PdfValidator.validate_unencrypted(trailer)

    assert {:error, {:invalid_pdf_input, %{stage: :trailer}}} =
             PdfValidator.validate_unencrypted(:malformed)

    assert {:error, {:encrypted_pdf, %{stage: :encryption}}} =
             PdfValidator.validate_xref(
               valid_entries,
               Map.put(trailer, "Encrypt", {:ref, {2, 0}}),
               pdf
             )

    assert :ok =
             PdfValidator.validate_xref(
               valid_entries,
               Map.put(trailer, "Encrypt", nil),
               pdf
             )

    invalid_entries = [
      Map.delete(valid_entries, 0),
      Map.put(valid_entries, 0, {:uncompressed, 1, 0}),
      Map.put(valid_entries, 0, {:free, 0, 0}),
      Map.put(valid_entries, 0, {:free, 2, 65_535})
    ]

    for entries <- invalid_entries do
      assert {:error,
              {:invalid_pdf_input,
               %{
                 stage: :xref,
                 reason: :invalid_pdf_input,
                 message: "xref object 0 must be a free entry with generation 65535"
               }}} = PdfValidator.validate_xref(entries, trailer, pdf)
    end

    assert {:error,
            {:invalid_pdf_input, %{stage: :xref, message: "parsed xref context is malformed"}}} =
             PdfValidator.validate_xref(:not_entries, trailer, pdf)

    assert {:error,
            {:invalid_pdf_input,
             %{stage: :xref, message: "xref entry is outside its declared bounds"}}} =
             PdfValidator.validate_xref(
               Map.put(valid_entries, 1, :malformed),
               trailer,
               pdf
             )
  end

  test "object-stream metadata is bounded before header parsing" do
    assert {:ok, %{count: 10_000, first: 20}} =
             PdfValidator.validate_object_stream_header(
               %{"N" => 10_000, "First" => 20},
               20,
               {6, 0}
             )

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfValidator.validate_object_stream_header(
               %{"N" => 10_001, "First" => 20},
               20,
               {6, 0}
             )

    assert diagnostic.stage == :limits
    assert diagnostic.reason == :resource_limit_exceeded

    assert diagnostic.message ==
             "PDF object stream entry count exceeds the limit; object 6 0"

    assert {:error, {:invalid_pdf_input, %{stage: :object_stream}}} =
             PdfValidator.validate_object_stream_header(
               %{"N" => 1, "First" => 21},
               20,
               {6, 0}
             )

    for arguments <- [
          {%{"N" => -1, "First" => 0}, 20, {6, 0}},
          {%{"N" => 1, "First" => -1}, 20, {6, 0}},
          {:malformed, 20, {6, 0}},
          {%{"N" => 1, "First" => 0}, :malformed, {6, 0}},
          {%{"N" => 1, "First" => 0}, 20, :malformed}
        ] do
      assert {:error, {:invalid_pdf_input, %{stage: :object_stream}}} =
               arguments
               |> Tuple.to_list()
               |> then(&apply(PdfValidator, :validate_object_stream_header, &1))
    end
  end

  test "generic PDF value depth is bounded by the shared validator" do
    assert :ok = PdfValidator.validate_value_depth(100)

    assert {:error,
            {:resource_limit_exceeded,
             %{
               stage: :limits,
               reason: :resource_limit_exceeded,
               message: "PDF value nesting depth exceeds the 100-level limit"
             }}} = PdfValidator.validate_value_depth(101)

    assert {:error,
            {:invalid_pdf_input, %{stage: :object, reason: :invalid_pdf_input, message: message}}} =
             PdfValidator.validate_value_depth(:invalid)

    assert message == "PDF value nesting depth is invalid"
  end

  test "shared validation prepares reusable page identity and inherited values" do
    document = shared_document()

    assert {:ok, context} = PdfValidator.validate(document)
    assert context.catalog_ref == {1, 0}
    assert context.page_tree_ref == {2, 0}

    assert [page] = context.pages
    assert page.ref == {3, 0}
    assert page.media_box == {:ref, {6, 0}}
    assert page.inherited["MediaBox"].source_ref == {2, 0}

    assert context.document.pages == [
             %{
               ref: {3, 0},
               resources: nil,
               rotate: nil,
               media_box: {:ref, {6, 0}}
             }
           ]

    assert PdfValidator.resolve(document, {:ref, {5, 0}}) == {:ok, 1}

    assert PdfValidator.dictionary(document, {:ref, {1, 0}}) ==
             {:ok, %{"Pages" => {:ref, {2, 0}}, "Type" => {:name, "Catalog"}}}

    assert PdfValidator.fetch(document, {:ref, {1, 0}}, "Pages") ==
             {:ok, {:ref, {2, 0}}}

    assert PdfValidator.number_array(document, {:ref, {6, 0}}, 4) ==
             {:ok, [0, 0, 100, 200]}
  end

  test "generated page-tree depths retain one semantic page without re-traversal" do
    for depth <- 1..20 do
      assert {:ok, context} = depth |> nested_page_document() |> PdfValidator.validate()
      assert [%{ref: {page_object, 0}, media_box: [0, 0, 100, 100]}] = context.pages
      assert page_object == depth + 2

      assert context.document.pages == [
               %{ref: {page_object, 0}, resources: nil, rotate: nil, media_box: [0, 0, 100, 100]}
             ]
    end
  end

  test "limits page-tree depth before recursive traversal becomes unsafe" do
    assert {:ok, %{pages: [_page]}} =
             999 |> nested_page_document() |> PdfValidator.validate()

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             1_000 |> nested_page_document() |> PdfValidator.validate()

    assert diagnostic.stage == :limits
    assert diagnostic.message == "PDF page tree depth exceeds the limit; object 1002 0"
  end

  test "caches shared page Parent chains during validation" do
    assert {:ok, context} =
             500
             |> shared_parent_chain_document(500)
             |> PdfValidator.validate()

    assert length(context.pages) == 500
  end

  test "limits indirect reference chain depth and aggregate validation work" do
    assert {:ok, 42} =
             1_000
             |> reference_chain_document()
             |> PdfValidator.resolve({:ref, {1, 0}})

    assert {:error, {:resource_limit_exceeded, depth_diagnostic}} =
             1_001
             |> reference_chain_document()
             |> PdfValidator.resolve({:ref, {1, 0}})

    assert depth_diagnostic.stage == :limits

    assert depth_diagnostic.message ==
             "indirect reference chain depth exceeds the limit; object 1001 0"

    assert {:error, {:resource_limit_exceeded, work_diagnostic}} =
             50
             |> unique_parent_chains_document(500)
             |> PdfValidator.validate()

    assert work_diagnostic.stage == :limits
    assert work_diagnostic.message =~ "indirect reference resolution work exceeds the limit"

    assert {:error, {:resource_limit_exceeded, split_work_diagnostic}} =
             50
             |> unique_page_chains_document(500)
             |> PdfValidator.validate()

    assert split_work_diagnostic.stage == :limits

    assert split_work_diagnostic.message =~
             "indirect reference resolution work exceeds the limit"
  end

  test "page-tree Parent validation includes indirect-object generations" do
    document =
      shared_document()
      |> put_in([:objects, {3, 0}, :value, "Parent"], {:ref, {2, 1}})

    assert {:error, {:invalid_pdf_input, diagnostic}} = PdfValidator.validate(document)
    assert diagnostic.stage == :page_tree

    assert diagnostic.message ==
             "page tree node Parent does not match its containing Pages node; object 3 0"
  end

  test "shared validator utilities diagnose malformed contexts and nested values" do
    assert {:error, {:invalid_pdf_input, %{stage: :validation}}} = PdfValidator.validate(%{})

    document = shared_document()

    assert {:error, {:invalid_pdf_input, %{stage: :resolution}}} =
             PdfValidator.dictionary(document, 42)

    assert {:error, {:invalid_pdf_input, %{stage: :resolution}}} =
             PdfValidator.fetch(document, %{}, 42)

    document = put_in(document.objects[{7, 0}], object([{:ref, {99, 0}}, 2]))

    assert {:error, {:invalid_pdf_input, %{stage: :resolution}}} =
             PdfValidator.number_array(document, {:ref, {7, 0}}, 2)

    missing_kids =
      shared_document()
      |> put_in([:objects, {2, 0}, :value, "Kids"], {:ref, {99, 0}})

    assert {:error, {:invalid_pdf_input, %{stage: :resolution}}} =
             PdfValidator.validate(missing_kids)
  end

  test "public validator APIs diagnose malformed document object records" do
    malformed_object = %{objects: %{{1, 0} => %{unexpected: true}}}

    for result <- [
          PdfValidator.resolve(malformed_object, {:ref, {1, 0}}),
          PdfValidator.dictionary(malformed_object, {:ref, {1, 0}}),
          PdfValidator.fetch(malformed_object, {:ref, {1, 0}}, "Value")
        ] do
      assert {:error, {:invalid_pdf_input, diagnostic}} = result
      assert diagnostic.stage == :resolution
      assert diagnostic.message == "indirect object record is malformed; object 1 0"
    end

    validation_document =
      Map.put(malformed_object, :trailer, %{"Root" => {:ref, {1, 0}}})

    assert {:error, {:invalid_pdf_input, validation_diagnostic}} =
             PdfValidator.validate(validation_document)

    assert validation_diagnostic.stage == :resolution
    assert validation_diagnostic.message == "indirect object record is malformed; object 1 0"

    assert {:error, {:invalid_pdf_input, stream_diagnostic}} =
             PdfValidator.validate_stream(
               %{objects: %{{1, 0} => %{value: 42}}},
               {:ref, {1, 0}}
             )

    assert stream_diagnostic.stage == :stream
    assert stream_diagnostic.message == "stream object record is malformed; object 1 0"

    assert {:error, {:invalid_pdf_input, table_diagnostic}} =
             PdfValidator.resolve(%{objects: :malformed}, {:ref, {1, 0}})

    assert table_diagnostic.stage == :resolution
    assert table_diagnostic.message == "parsed PDF document object table is malformed"

    assert {:error, {:invalid_pdf_input, reference_diagnostic}} =
             PdfValidator.resolve(malformed_object, {:ref, :malformed})

    assert reference_diagnostic.stage == :resolution
    assert reference_diagnostic.message == "indirect reference is malformed"

    assert {:error, {:invalid_pdf_input, stream_reference_diagnostic}} =
             PdfValidator.validate_stream(malformed_object, {:ref, :malformed})

    assert stream_reference_diagnostic.stage == :resolution
    assert stream_reference_diagnostic.message == "indirect stream reference is malformed"

    assert {:error, {:invalid_pdf_input, stream_table_diagnostic}} =
             PdfValidator.validate_stream(%{objects: :malformed}, {:ref, {1, 0}})

    assert stream_table_diagnostic.stage == :resolution
    assert stream_table_diagnostic.message == "parsed PDF document object table is malformed"
  end

  test "shared stream validation returns prepared structure without decoding filters" do
    valid = %{
      objects: %{
        {1, 0} => object(%{"Length" => 2}, "ok")
      }
    }

    assert {:ok, %{ref: {1, 0}, dictionary: %{"Length" => 2}, stream: "ok"}} =
             PdfValidator.validate_stream(valid, {:ref, {1, 0}})

    scalar = %{objects: %{{1, 0} => object(42, "ok")}}

    assert {:error, {:invalid_pdf_input, %{stage: :stream}}} =
             PdfValidator.validate_stream(scalar, {:ref, {1, 0}})

    incomplete = %{objects: %{{1, 0} => %{stream: "ok"}}}

    assert {:error, {:invalid_pdf_input, %{stage: :stream}}} =
             PdfValidator.validate_stream(incomplete, {:ref, {1, 0}})
  end

  test "stream reference aliases enforce the shared reference depth limit" do
    assert {:ok, %{ref: {1_000, 0}, stream: "ok"}} =
             1_000
             |> stream_reference_chain_document()
             |> PdfValidator.validate_stream({:ref, {1, 0}})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             1_001
             |> stream_reference_chain_document()
             |> PdfValidator.validate_stream({:ref, {1, 0}})

    assert diagnostic.stage == :limits

    assert diagnostic.message ==
             "indirect reference chain depth exceeds the limit; object 1001 0"
  end

  test "cached reference suffixes cannot bypass the reference depth limit" do
    document = cached_suffix_number_array_document(600, 402)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             PdfValidator.number_array(
               document,
               [{:ref, {1, 0}}, {:ref, {601, 0}}],
               2
             )

    assert diagnostic.stage == :limits
    assert diagnostic.message =~ "indirect reference chain depth exceeds the limit"
  end

  test "decoded stream validation prepares canonical filters and parameters" do
    document = %{
      objects: %{
        {1, 0} =>
          object(
            %{
              "Length" => 2,
              "Filter" => {:name, "Fl"},
              "DecodeParms" => %{
                "Predictor" => 12,
                "Colors" => 3,
                "BitsPerComponent" => 8,
                "Columns" => 10
              }
            },
            "ok"
          )
      }
    }

    assert {:ok,
            %{
              filters: [
                %{
                  name: "FlateDecode",
                  parameters: %{
                    "Predictor" => 12,
                    "Colors" => 3,
                    "BitsPerComponent" => 8,
                    "Columns" => 10,
                    "EarlyChange" => 1
                  }
                }
              ]
            }} = PdfValidator.prepare_decoded_stream(document, {:ref, {1, 0}})

    invalid = put_in(document.objects[{1, 0}].value["DecodeParms"]["Columns"], 0)

    assert {:error, {:invalid_pdf_input, %{stage: :filter}}} =
             PdfValidator.prepare_decoded_stream(invalid, {:ref, {1, 0}})
  end

  test "merge validation materializes inherited tokens and exact-generation remapping" do
    assert {:ok, pdf_context} = PdfValidator.validate(shared_document())
    assert {:ok, input} = MergeValidator.prepare(pdf_context)

    assert input.pages == [{3, 0}]

    assert input.inherited[3].mediabox == [
             :lbracket,
             {:int, 0},
             {:int, 0},
             {:int, 100},
             {:int, 200},
             :rbracket
           ]

    generation_input = %{
      objects: [%{obj: 1, gen: 2, tokens: [], value: 42}],
      pages: [],
      inherited: %{}
    }

    assert {:ok, [%{map: %{{1, 2} => 3}}]} =
             MergeValidator.prepare_remapping([generation_input], 3)

    for generation <- 0..10 do
      generated =
        put_in(generation_input.objects, [%{obj: 1, gen: generation, tokens: [], value: 42}])

      assert {:ok, [%{map: map}]} = MergeValidator.prepare_remapping([generated], 3)
      assert map == %{{1, generation} => 3}
    end
  end

  test "merge validation rejects malformed contexts, token graphs, and incomplete remapping" do
    assert {:error, {:invalid_pdf_input, %{stage: :merge_validation}}} =
             MergeValidator.prepare(%{})

    assert {:error, {:invalid_pdf_input, %{stage: :reference_remapping}}} =
             MergeValidator.prepare_remapping(:invalid, 0)

    assert {:error, {:invalid_pdf_input, %{stage: :reference_remapping}}} =
             MergeValidator.prepare_remapping([%{objects: :invalid, pages: []}], 3)

    assert {:error, {:invalid_pdf_input, %{stage: :reference_remapping}}} =
             MergeValidator.prepare_remapping([%{objects: [:invalid], pages: []}], 3)

    assert {:error, {:resource_limit_exceeded, page_diagnostic}} =
             MergeValidator.prepare_remapping(
               [%{objects: [], pages: List.duplicate({1, 0}, 10_001)}],
               3
             )

    assert page_diagnostic.stage == :limits
    assert page_diagnostic.message == "merged page count exceeds the limit"

    object = %{obj: 1, gen: 0, tokens: [], value: 42}

    assert {:error, {:resource_limit_exceeded, object_diagnostic}} =
             MergeValidator.prepare_remapping(
               [%{objects: List.duplicate(object, 99_999), pages: []}],
               3
             )

    assert object_diagnostic.stage == :limits
    assert object_diagnostic.message == "merged object count exceeds the limit"

    unserializable = %{
      document: %{objects: %{{1, 0} => object(42, nil, [{:op, "invalid"}])}},
      pages: []
    }

    assert {:error, {:invalid_pdf_input, %{stage: :serialization}}} =
             MergeValidator.prepare(unserializable)

    missing_reference = %{
      objects: [
        %{obj: 1, gen: 0, tokens: [{:int, 9}, {:int, 2}, :R], value: {:ref, {9, 2}}}
      ],
      pages: [],
      inherited: %{}
    }

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             MergeValidator.prepare_remapping([missing_reference], 3)

    assert diagnostic.stage == :reference_remapping
    assert diagnostic.message =~ "9 2"

    missing_page_object = %{
      document: %{objects: %{}},
      pages: [merge_page(%{})]
    }

    assert {:error, {:invalid_pdf_input, %{stage: :serialization}}} =
             MergeValidator.prepare(missing_page_object)

    non_dictionary_page = %{
      document: %{
        objects: %{{3, 0} => object({:ref, {4, 0}}, nil, [{:int, 4}, {:int, 0}, :R])}
      },
      pages: [merge_page(%{})]
    }

    assert {:error, {:invalid_pdf_input, rewrite_diagnostic}} =
             MergeValidator.prepare(non_dictionary_page)

    assert rewrite_diagnostic.stage == :serialization

    assert rewrite_diagnostic.message ==
             "validated page object is not a dictionary ready for rewriting"

    missing_inherited_tokens = %{
      document: %{
        objects: %{
          {2, 0} => object(%{"Type" => {:name, "Pages"}}, nil, [:dict_start, :dict_end]),
          {3, 0} => object(%{"Type" => {:name, "Page"}}, nil, [:dict_start, :dict_end])
        }
      },
      pages: [merge_page(%{"Resources" => %{value: %{}, source_ref: {2, 0}}})]
    }

    assert {:error, {:invalid_pdf_input, %{stage: :serialization}}} =
             MergeValidator.prepare(missing_inherited_tokens)

    incomplete_inherited_tokens =
      put_in(
        missing_inherited_tokens.document.objects[{2, 0}].tokens,
        [:dict_start, {:name, "Resources"}]
      )

    assert {:error, {:invalid_pdf_input, %{stage: :serialization}}} =
             MergeValidator.prepare(incomplete_inherited_tokens)
  end

  test "merge token utilities handle nested values and incomplete input deterministically" do
    assert MergeValidator.take_value_tokens([]) == :error
    assert MergeValidator.take_value_tokens([:dict_start]) == :error

    assert MergeValidator.take_value_tokens([
             :lbracket,
             {:int, 1},
             :dict_start,
             {:name, "Nested"},
             true,
             :dict_end,
             :rbracket,
             false
           ]) ==
             {:ok,
              [
                :lbracket,
                {:int, 1},
                :dict_start,
                {:name, "Nested"},
                true,
                :dict_end,
                :rbracket
              ], [false]}

    assert MergeValidator.split_dictionary_value([{:name, "Missing"}], "Value") == :error
    assert MergeValidator.split_dictionary_value([{:name, "Value"}], "Value") == :error

    assert MergeValidator.split_dictionary_value(
             [{:name, "Before"}, true, {:name, "Value"}, {:int, 4}, false],
             "Value"
           ) == {:ok, [{:name, "Before"}, true], [{:int, 4}], [false]}

    nested_value = [
      {:name, "Before"},
      :dict_start,
      {:name, "Value"},
      true,
      :dict_end,
      {:name, "Value"},
      :lbracket,
      {:name, "Value"},
      :rbracket,
      {:name, "After"},
      false
    ]

    assert MergeValidator.split_dictionary_value(nested_value, "Value") ==
             {:ok, [{:name, "Before"}, :dict_start, {:name, "Value"}, true, :dict_end],
              [:lbracket, {:name, "Value"}, :rbracket], [{:name, "After"}, false]}
  end

  test "text validation prepares page instructions and reusable numeric values" do
    content = "BT /F1 12 Tf [(A) -20 (B)] TJ ET"

    assert {:ok, instructions} = TextValidator.instructions(content, 1)
    assert Enum.map(instructions, & &1.operator) == ["BT", "Tf", "TJ", "ET"]

    assert {:error, {:invalid_pdf_input, %{message: message}}} =
             TextValidator.instructions("BT /F1 /Bad Tf ET", 1)

    assert message =~ "Tf has invalid operands"

    assert TextValidator.number({:int, 2}) == {:ok, 2}
    assert TextValidator.number({:real, 2.5}) == {:ok, 2.5}
    assert TextValidator.number(:invalid) == :error
    assert TextValidator.numbers([{:int, 1}, {:real, 2.5}], 2) == {:ok, [1.0, 2.5]}
    assert TextValidator.numbers([{:int, 1}], 2) == :error

    assert {:error, {:invalid_pdf_input, %{stage: :text_validation}}} =
             TextValidator.prepare(%{})

    assert {:error, {:invalid_pdf_input, %{stage: :content}}} =
             TextValidator.instructions(:invalid, 0)

    malformed_page = %{
      document: %{objects: %{}},
      pages: [
        %{
          ref: {3, 0},
          dictionary: %{},
          resources: nil,
          media_box: [0, 0, 0, 100],
          crop_box: nil,
          rotate: nil,
          inherited: %{}
        }
      ]
    }

    assert {:error, {:invalid_pdf_input, %{stage: :page_tree}}} =
             TextValidator.prepare(malformed_page)
  end

  test "text preparation canonicalizes stream aliases and preserves the resource wrapper" do
    document = %{
      objects: %{
        {1, 0} => %{value: {:ref, {2, 0}}, stream: nil},
        {2, 0} => %{value: %{"Length" => 0}, stream: ""}
      }
    }

    preparation_context = TextValidator.new_preparation_context()

    assert {:ok, [], preparation_context} =
             TextValidator.prepare_content_stream(
               document,
               {:ref, {2, 0}},
               1,
               preparation_context
             )

    assert {:ok, [], preparation_context} =
             TextValidator.prepare_content_stream(
               document,
               {:ref, {1, 0}},
               1,
               preparation_context
             )

    assert map_size(preparation_context.decoded_streams) == 1
    assert map_size(preparation_context.instructions) == 1
    assert preparation_context.stream_uses == 2

    assert TextResourceValidator.prepare_contents(document, nil, [], 1) == {:ok, []}

    assert {:error, {:invalid_pdf_input, %{stage: :resolution}}} =
             TextResourceValidator.prepare_contents(
               document,
               nil,
               [[%{operator: "Tf", operands: [{:name, "F1"}, 12]}]],
               1
             )
  end

  defp shared_document do
    %{
      objects: %{
        {1, 0} =>
          object(
            %{"Type" => {:name, "Catalog"}, "Pages" => {:ref, {2, 0}}},
            nil,
            [
              :dict_start,
              {:name, "Type"},
              {:name, "Catalog"},
              {:name, "Pages"},
              {:int, 2},
              {:int, 0},
              :R,
              :dict_end
            ]
          ),
        {2, 0} =>
          object(
            %{
              "Type" => {:name, "Pages"},
              "Kids" => {:ref, {4, 0}},
              "Count" => {:ref, {5, 0}},
              "MediaBox" => {:ref, {6, 0}}
            },
            nil,
            [
              :dict_start,
              {:name, "Type"},
              {:name, "Pages"},
              {:name, "Kids"},
              {:int, 4},
              {:int, 0},
              :R,
              {:name, "Count"},
              {:int, 5},
              {:int, 0},
              :R,
              {:name, "MediaBox"},
              {:int, 6},
              {:int, 0},
              :R,
              :dict_end
            ]
          ),
        {3, 0} =>
          object(
            %{"Type" => {:name, "Page"}, "Parent" => {:ref, {2, 0}}},
            nil,
            [
              :dict_start,
              {:name, "Type"},
              {:name, "Page"},
              {:name, "Parent"},
              {:int, 2},
              {:int, 0},
              :R,
              :dict_end
            ]
          ),
        {4, 0} => object([{:ref, {3, 0}}], nil, [:lbracket, {:int, 3}, {:int, 0}, :R, :rbracket]),
        {5, 0} => object(1, nil, [{:int, 1}]),
        {6, 0} =>
          object([0, 0, 100, 200], nil, [
            :lbracket,
            {:int, 0},
            {:int, 0},
            {:int, 100},
            {:int, 200},
            :rbracket
          ])
      },
      trailer: %{"Root" => {:ref, {1, 0}}}
    }
  end

  defp merge_page(inherited) do
    %{
      ref: {3, 0},
      dictionary: %{"Type" => {:name, "Page"}},
      resources: nil,
      media_box: [0, 0, 100, 100],
      crop_box: nil,
      rotate: nil,
      inherited: inherited
    }
  end

  defp nested_page_document(depth) do
    page_object = depth + 2

    page_nodes =
      Enum.reduce(0..(depth - 1), %{}, fn level, objects ->
        object_number = level + 2
        child_number = object_number + 1

        dictionary = %{
          "Type" => {:name, "Pages"},
          "Kids" => [{:ref, {child_number, 0}}],
          "Count" => 1,
          "MediaBox" => [0, 0, 100, 100]
        }

        dictionary =
          case level do
            0 -> dictionary
            _ -> Map.put(dictionary, "Parent", {:ref, {object_number - 1, 0}})
          end

        Map.put(objects, {object_number, 0}, object(dictionary))
      end)

    objects =
      page_nodes
      |> Map.put(
        {1, 0},
        object(%{"Type" => {:name, "Catalog"}, "Pages" => {:ref, {2, 0}}})
      )
      |> Map.put(
        {page_object, 0},
        object(%{"Type" => {:name, "Page"}, "Parent" => {:ref, {page_object - 1, 0}}})
      )

    %{objects: objects, trailer: %{"Root" => {:ref, {1, 0}}}}
  end

  defp shared_parent_chain_document(page_count, chain_length) do
    alias_start = 3
    page_start = alias_start + chain_length

    aliases =
      Enum.reduce(0..(chain_length - 1), %{}, fn offset, objects ->
        object_number = alias_start + offset

        next_ref =
          case offset == chain_length - 1 do
            true -> {2, 0}
            false -> {object_number + 1, 0}
          end

        Map.put(objects, {object_number, 0}, object({:ref, next_ref}))
      end)

    {objects, kids} =
      Enum.reduce(0..(page_count - 1), {aliases, []}, fn offset, {objects, kids} ->
        page_ref = {page_start + offset, 0}

        page =
          object(%{"Type" => {:name, "Page"}, "Parent" => {:ref, {alias_start, 0}}})

        {Map.put(objects, page_ref, page), [{:ref, page_ref} | kids]}
      end)

    objects =
      objects
      |> Map.put(
        {1, 0},
        object(%{"Type" => {:name, "Catalog"}, "Pages" => {:ref, {2, 0}}})
      )
      |> Map.put(
        {2, 0},
        object(%{
          "Type" => {:name, "Pages"},
          "Kids" => Enum.reverse(kids),
          "Count" => page_count,
          "MediaBox" => [0, 0, 100, 100]
        })
      )

    %{objects: objects, trailer: %{"Root" => {:ref, {1, 0}}}}
  end

  defp unique_parent_chains_document(page_count, chain_length) do
    {objects, kids, _next_object} =
      Enum.reduce(1..page_count, {%{}, [], 3}, fn _page, {objects, kids, next_object} ->
        parent_ref = {next_object, 0}

        objects =
          Enum.reduce(0..(chain_length - 1), objects, fn offset, objects ->
            object_number = next_object + offset

            next_ref =
              case offset == chain_length - 1 do
                true -> {2, 0}
                false -> {object_number + 1, 0}
              end

            Map.put(objects, {object_number, 0}, object({:ref, next_ref}))
          end)

        page_ref = {next_object + chain_length, 0}
        page = object(%{"Type" => {:name, "Page"}, "Parent" => {:ref, parent_ref}})

        {Map.put(objects, page_ref, page), [{:ref, page_ref} | kids], elem(page_ref, 0) + 1}
      end)

    objects =
      objects
      |> Map.put(
        {1, 0},
        object(%{"Type" => {:name, "Catalog"}, "Pages" => {:ref, {2, 0}}})
      )
      |> Map.put(
        {2, 0},
        object(%{
          "Type" => {:name, "Pages"},
          "Kids" => Enum.reverse(kids),
          "Count" => page_count,
          "MediaBox" => [0, 0, 100, 100]
        })
      )

    %{objects: objects, trailer: %{"Root" => {:ref, {1, 0}}}}
  end

  defp unique_page_chains_document(page_count, chain_length) do
    {objects, kids, _next_object} =
      Enum.reduce(1..page_count, {%{}, [], 3}, fn _page, {objects, kids, next_object} ->
        page_ref = {next_object + chain_length, 0}

        objects =
          Enum.reduce(0..(chain_length - 1), objects, fn offset, objects ->
            object_number = next_object + offset

            next_ref =
              case offset == chain_length - 1 do
                true -> page_ref
                false -> {object_number + 1, 0}
              end

            Map.put(objects, {object_number, 0}, object({:ref, next_ref}))
          end)

        page = object(%{"Type" => {:name, "Page"}, "Parent" => {:ref, {2, 0}}})
        objects = Map.put(objects, page_ref, page)

        {objects, [{:ref, {next_object, 0}} | kids], elem(page_ref, 0) + 1}
      end)

    objects =
      objects
      |> Map.put(
        {1, 0},
        object(%{"Type" => {:name, "Catalog"}, "Pages" => {:ref, {2, 0}}})
      )
      |> Map.put(
        {2, 0},
        object(%{
          "Type" => {:name, "Pages"},
          "Kids" => Enum.reverse(kids),
          "Count" => page_count,
          "MediaBox" => [0, 0, 100, 100]
        })
      )

    %{objects: objects, trailer: %{"Root" => {:ref, {1, 0}}}}
  end

  defp reference_chain_document(length) do
    objects =
      Enum.reduce(1..length, %{}, fn object_number, objects ->
        value =
          case object_number == length do
            true -> 42
            false -> {:ref, {object_number + 1, 0}}
          end

        Map.put(objects, {object_number, 0}, object(value))
      end)

    %{objects: objects}
  end

  defp stream_reference_chain_document(length) do
    objects =
      Enum.reduce(1..length, %{}, fn object_number, objects ->
        object =
          case object_number == length do
            true -> object(%{"Length" => 2}, "ok")
            false -> object({:ref, {object_number + 1, 0}})
          end

        Map.put(objects, {object_number, 0}, object)
      end)

    %{objects: objects}
  end

  defp cached_suffix_number_array_document(suffix_length, prefix_length) do
    suffix =
      Enum.reduce(1..suffix_length, %{}, fn object_number, objects ->
        value =
          case object_number == suffix_length do
            true -> 42
            false -> {:ref, {object_number + 1, 0}}
          end

        Map.put(objects, {object_number, 0}, object(value))
      end)

    objects =
      Enum.reduce(0..(prefix_length - 1), suffix, fn offset, objects ->
        object_number = suffix_length + offset + 1

        next_ref =
          case offset == prefix_length - 1 do
            true -> {1, 0}
            false -> {object_number + 1, 0}
          end

        Map.put(objects, {object_number, 0}, object({:ref, next_ref}))
      end)

    %{objects: objects}
  end

  defp object(value, stream \\ nil, tokens \\ []) do
    %{value: value, stream: stream, offset: nil, tokens: tokens}
  end
end
