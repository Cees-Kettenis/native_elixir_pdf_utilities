defmodule NativeElixirPdfUtilities.Validators.TextValidatorTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Validators.TextValidator
  alias NativeElixirPdfUtilities.Limits

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "content arrays and dictionaries enforce one shared cap before collecting excess values",
       %{limits: limits} do
    Limits.install(%{limits | max_pdf_container_entries: 2})

    for content <- [
          "[1 2] ignored",
          "/Span << /A 1 /B 2 >> BDC EMC",
          "/Span << /A [1 2] /B << /C 1 /D 2 >> >> BDC EMC",
          "/Span << /A 1 /A 2 >> BDC EMC"
        ] do
      assert {:ok, _} = TextValidator.instructions(content, 3)
    end

    for content <- [
          "[1 2 3 @",
          "[1 [2 3 4 @",
          "/Span << /A 1 /B 2 /C @",
          "/Span << /A 1 /A 2 /A @"
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               TextValidator.instructions(content, 3)

      assert diagnostic.stage == :limits
      assert diagnostic.message =~ "max_pdf_container_entries"
      assert diagnostic.message =~ "page 3"
    end

    assert {:error, {:resource_limit_exceeded, _}} =
             TextValidator.instructions(
               ["[1 2", "3] ignored"],
               1,
               TextValidator.new_preparation_context()
             )
  end

  test "instruction admission stops before later invalid syntax", %{limits: limits} do
    Limits.install(%{limits | max_text_parsed_instructions: 1})

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             TextValidator.instructions("q q @", 1)

    assert diagnostic.stage == :limits and diagnostic.operation == :extract
    assert diagnostic.message =~ "parsed content instruction"
  end

  test "shallow sibling arrays require linear preparation work" do
    assert {:ok, _} = TextValidator.instructions("[] ignored", 1)

    measure = fn count ->
      Task.async(fn ->
        content = String.duplicate("[] ", count) <> "ignored"
        :erlang.garbage_collect()
        {:reductions, before} = Process.info(self(), :reductions)
        assert {:ok, [_]} = TextValidator.instructions(content, 1)
        {:reductions, after_work} = Process.info(self(), :reductions)
        after_work - before
      end)
      |> Task.await()
    end

    small = measure.(256)
    large = measure.(1_024)
    assert large < small * 6
  end

  test "token and value quotas admit boundaries and stop before later lexical failures", %{
    limits: limits
  } do
    Limits.install(%{limits | max_text_parsed_tokens: 3})
    assert {:ok, [%{operator: "ignored"}]} = TextValidator.instructions("[] ignored", 1)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             TextValidator.instructions("[] ignored q @", 2)

    assert diagnostic.stage == :limits and diagnostic.operation == :extract
    assert diagnostic.message =~ "token count"
    assert diagnostic.message =~ "page 2"

    Limits.install(%{limits | max_text_parsed_values: 2})
    assert {:ok, [_]} = TextValidator.instructions("[1] ignored", 1)

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             TextValidator.instructions("[1 2 @", 3)

    assert diagnostic.message =~ "value count"
    assert diagnostic.message =~ "page 3"
  end

  test "admission counters span content streams and unique page preparations", %{limits: limits} do
    Limits.install(%{
      limits
      | max_text_parsed_tokens: 4,
        max_text_parsed_values: 2,
        max_text_parsed_instructions: 2
    })

    assert {:ok, [_]} = TextValidator.instructions(["[", "1] ignored"], 1)
    context = TextValidator.new_preparation_context()
    assert {:ok, [_], context} = TextValidator.instructions("1 ignored", 1, context)
    assert {:ok, [_], context} = TextValidator.instructions("2 ignored", 2, context)

    assert context.parsed_tokens == 4 and context.parsed_values == 2 and
             context.parsed_instructions == 2

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             TextValidator.instructions("q @", 3, context)

    assert diagnostic.message =~ "token count"
  end

  test "invalid preparation context and content stream input return canonical failures" do
    assert {:error, {:invalid_pdf_input, diagnostic}} = TextValidator.prepare(nil)
    assert diagnostic.stage == :text_validation
    assert diagnostic.operation == :extract
    assert diagnostic.module == TextValidator
    assert diagnostic.message =~ "shared PDF validation context"

    assert {:error, {:invalid_pdf_input, diagnostic}} = TextValidator.instructions(42, 1)
    assert diagnostic.stage == :content
    assert diagnostic.operation == :extract
    assert diagnostic.message =~ "content stream input"
  end

  test "matrix composition validates representability without reapplying the input limit" do
    identity = [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]
    large = [1.0e300, 0.0, 0.0, 1.0e300, 0.0, 0.0]
    assert {:ok, ^large} = TextValidator.compose_matrix(identity, large, 2, "cm")

    assert {:error, {:invalid_pdf_input, diagnostic}} =
             TextValidator.compose_matrix(large, large, 2, "cm")

    assert diagnostic.stage == :content
    assert diagnostic.reason == :invalid_pdf_input
    assert diagnostic.module == TextValidator
    assert diagnostic.operation == :extract
    assert diagnostic.message =~ "cm"
    assert diagnostic.message =~ "supported numeric range"
    assert diagnostic.message =~ "page 2"
  end

  test "numeric tokens must fit the shared PDF magnitude limit" do
    assert {:ok, 1_000_000_000} = TextValidator.number({:int, 1_000_000_000})

    for value <- [{:int, Integer.pow(10, 400)}, {:real, 1.0e308}, {:int, :bad}] do
      assert :error = TextValidator.number(value)
      assert :error = TextValidator.numbers([value], 1)
    end
  end

  test "public text requests are normalized by the validator" do
    assert {:ok, %{pdf: "%PDF", options: %{layout: false}}} =
             TextValidator.validate_request("%PDF", [layout: false], :extract)

    assert {:ok, %{options: %{order: :visual}}} =
             TextValidator.validate_request("%PDF", [order: :visual], :extract_spans)

    assert {:ok, "/tmp/input.pdf"} = TextValidator.validate_path("/tmp/input.pdf", :extract_file)

    assert {:error, {:invalid_options, %{stage: :options}}} =
             TextValidator.validate_request("%PDF", [unknown: true], :extract)

    assert {:error, {:invalid_pdf_input, %{stage: :input}}} =
             TextValidator.validate_request("%PDF", [], :unknown)

    assert {:error, {:invalid_path, %{stage: :file}}} =
             TextValidator.validate_path("/tmp/input.pdf", 123)
  end

  test "validates reconstructed layout whitespace projections" do
    assert :ok = TextValidator.validate_layout_whitespace([{1, 0}, {2, 10}])

    for projection <- [:not_a_list, [{0, 1}], [{1, -1}], [{1, :not_a_byte_count}]] do
      assert {:error,
              {:invalid_pdf_input,
               %{
                 stage: :text_validation,
                 reason: :invalid_pdf_input,
                 message: "layout whitespace projection is malformed"
               }}} = TextValidator.validate_layout_whitespace(projection)
    end
  end

  test "validate_scopes rejects text showing outside a text object" do
    instructions = [[%{operator: "Tj", operands: [{:string, "outside"}]}]]

    assert {:error,
            {:invalid_pdf_input,
             %{stage: :content, message: "Tj appears outside a text object; page 1"}}} =
             TextValidator.validate_scopes(instructions, 1)
  end

  test "validate_scopes rejects malformed prepared instruction input" do
    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :content,
               reason: :invalid_pdf_input,
               operation: :extract,
               module: TextValidator,
               message: "content scope input is malformed"
             }}} = TextValidator.validate_scopes(:not_instructions, 1)

    assert {:error,
            {:invalid_pdf_input,
             %{
               stage: :content,
               reason: :invalid_pdf_input,
               operation: :extract,
               module: TextValidator,
               message: "content scope instruction is malformed; page 1"
             }}} = TextValidator.validate_scopes([[%{}]], 1)
  end
end
