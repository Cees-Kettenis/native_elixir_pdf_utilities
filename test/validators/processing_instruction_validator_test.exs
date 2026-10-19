defmodule NativeElixirPdfUtilities.Validators.ProcessingInstructionValidatorTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.Validators.ProcessingInstructionValidator, as: PI

  test "XML pseudo-attributes preserve case, names, literal whitespace and one reference decode" do
    data = ~S(name='A&amp;B' Name="&lt;&gt;&quot;&apos;" :ns.name="&amp;lt;&#38;lt;" empty='')
    node = %{type: :processing_instruction, target: "marker", data: data}

    assert {:ok, %{"name" => "A&B", "Name" => "<>\"'", ":ns.name" => "&lt;&lt;", "empty" => ""}} =
             PI.attributes(node)

    assert {:ok, "A&B"} = PI.attribute(node, "name")
    assert {:ok, "<>\"'"} = PI.attribute(node, "Name")
    assert {:ok, ""} = PI.attribute(node, "empty")

    for name <- ["NAME", "absent", "", "invalid name"],
        do: assert(PI.attribute(node, name) == {:ok, nil})

    assert node.data == data

    for data <- ["", " \t\r\n"] do
      assert PI.attributes(%{node | data: data}) == {:ok, %{}}
    end

    assert {:ok, %{"a" => " \t\r\n\f '", "b" => "\""}} =
             PI.attributes(%{node | data: " \t\ra \n= \t\" \t\r\n\f '\"\n b='\"' \t"})

    assert {:ok, %{"name" => "A"}} = PI.attributes("<?marker name='A'?>")
  end

  test "XML Name ranges and continuation characters follow Fifth Edition productions" do
    starts = [
      ?:,
      ?A,
      ?Z,
      ?_,
      ?a,
      ?z,
      0xC0,
      0xD6,
      0xD8,
      0xF6,
      0xF8,
      0x2FF,
      0x370,
      0x37D,
      0x37F,
      0x1FFF,
      0x200C,
      0x200D,
      0x2070,
      0x218F,
      0x2C00,
      0x2FEF,
      0x3001,
      0xD7FF,
      0xF900,
      0xFDCF,
      0xFDF0,
      0xFFFD,
      0x10000,
      0xEFFFF
    ]

    for char <- starts do
      name = <<char::utf8>> <> "-.0·\u0300\u036F\u203F\u2040"

      assert PI.attributes(%{
               type: :processing_instruction,
               target: "start",
               data: name <> "='value'"
             }) == {:ok, %{name => "value"}}
    end

    for char <- [
          ?-,
          ?.,
          ?0,
          0xB7,
          0xBF,
          0xD7,
          0xF7,
          0x300,
          0x36F,
          0x37E,
          0x200B,
          0x200E,
          0x206F,
          0x2190,
          0x2BFF,
          0x2FF0,
          0x3000,
          0xE000,
          0xF8FF,
          0xFDD0,
          0xFDEF,
          0xFFFE,
          0xF0000
        ] do
      data = <<char::utf8>> <> "='bad'"

      assert PI.attributes(%{type: :processing_instruction, target: "start", data: data}) ==
               {:ok, %{}}
    end

    for char <- [0x37E, 0x200B, 0x200E, 0xF0000] do
      data = "a" <> <<char::utf8>> <> "='bad'"

      assert PI.attributes(%{type: :processing_instruction, target: "marker", data: data}) ==
               {:ok, %{}}
    end
  end

  test "any malformed syntax or duplicate clears the entire map" do
    for data <- [
          "a='good' junk",
          "a='good'b='bad'",
          "a='good' a='again'",
          "a='good' b=bare",
          "a='good' b",
          "a='good' b==''",
          "a='good' b='unclosed",
          "a='good' b=\"mismatch'",
          "a='good' b='<bad'",
          "a='good' b='&'",
          "a='good' b='&nbsp;'",
          "a='good' b='&AMP;'",
          "a='good' b='&amp'",
          "a='good' b='&#;'",
          "a='good' b='&#x;'",
          "a='good' b='&#X41;'",
          "a='good' b='&#-1;'",
          "a='good' b='&#+65;'",
          "a='good' b='&#xG;'",
          "a='good'\fb='bad'",
          "a='good' b='bad'",
          "\fa='bad'",
          "a\f='bad'",
          "a=\f'bad'",
          "a='good'\f",
          "a='good'/",
          "a='good' b='&#0;'",
          "a='good' b='&#xD800;'",
          "a='good' b='&#xFFFE;'",
          "a='good' b='&#x110000;'",
          "a='good' b='&#99999999;'",
          "a='good' b='&#xFFFFFFF;'",
          "a='good' b='&#00000000;'",
          "a='good' b='&#x000000;'"
        ] do
      node = %{type: :processing_instruction, target: "marker", data: data}
      assert PI.attributes(node) == {:ok, %{}}
      assert PI.attribute(node, "a") == {:ok, nil}
    end
  end

  test "numeric references enforce XML Char without HTML replacement or integer expansion" do
    for codepoint <- [9, 10, 13, 0x20, 0x7F, 0x80, 0xD7FF, 0xE000, 0xFFFD, 0x10000, 0x10FFFF] do
      for reference <- [
            "&##{codepoint};",
            "&#x#{Integer.to_string(codepoint, 16)};",
            "&#000#{codepoint};"
          ] do
        assert {:ok, %{"a" => <<^codepoint::utf8>>}} =
                 PI.attributes(%{
                   type: :processing_instruction,
                   target: "marker",
                   data: "a='#{reference}'"
                 })
      end
    end

    for codepoint <- [1, 8, 11, 12, 14, 31, 0xD800, 0xDFFF, 0xFFFE, 0xFFFF, 0x110000] do
      assert PI.attributes(%{
               type: :processing_instruction,
               target: "marker",
               data: "a='&#x#{Integer.to_string(codepoint, 16)};'"
             }) == {:ok, %{}}
    end

    for digits <- [String.duplicate("9", 4096), "x" <> String.duplicate("F", 4096)] do
      assert PI.attributes(%{
               type: :processing_instruction,
               target: "marker",
               data: "a='&##{digits};'"
             }) == {:ok, %{}}
    end

    assert {:ok, %{"a" => "A"}} =
             PI.attributes(%{
               type: :processing_instruction,
               target: "marker",
               data: "a='&#" <> String.duplicate("0", 4096) <> "65;'"
             })
  end

  test "invalid transport and forged state still use shared PI diagnostics" do
    marker = %{type: :processing_instruction, target: "marker", data: "name='A'"}

    for node <- [
          nil,
          %{},
          %{marker | data: <<255>>},
          %{marker | data: "bad>"},
          Map.put(marker, :attributes, %{"name" => "forged"}),
          Map.put(marker, :patch_state, %{parent_id: make_ref()}),
          Map.put(marker, :insertion_target, make_ref()),
          Map.put(marker, :source, %{text: "<?marker name='B'?>", line: 1, column: 1})
        ] do
      assert {:error, {:invalid_document, diagnostic}} = PI.attributes(node)
      assert diagnostic.stage == :style and diagnostic.reason == :invalid_document
      assert diagnostic.module == HtmlValidator and diagnostic.source == "processing_instruction"
      assert diagnostic.message =~ "processing instructions require"
    end

    assert {:error, {:invalid_document, _}} = PI.attribute(nil, "name")

    for name <- [nil, :name, 1, %{}, [], <<255>>] do
      assert {:error, {:invalid_document, diagnostic}} = PI.attribute(marker, name)
      assert diagnostic.stage == :style and diagnostic.source == "processing_instruction"
      assert diagnostic.module == PI and diagnostic.message =~ "names must be valid UTF-8 strings"
    end
  end

  test "large valid maps and reference values do not acquire regex or integer ceilings" do
    attributes = Map.new(1..6000, fn i -> {"n#{i}", "value"} end)
    data = Enum.map_join(attributes, " ", fn {name, value} -> name <> "='" <> value <> "'" end)
    node = %{type: :processing_instruction, target: "marker", data: data}
    assert PI.attributes(node) == {:ok, attributes}

    data = "name='" <> String.duplicate("&amp;", 6000) <> "'"
    assert PI.attributes(%{node | data: data}) == {:ok, %{"name" => String.duplicate("&", 6000)}}
  end

  test "malformed data, name lookup and transport scans consume shared work before parsing" do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    node = %{type: :processing_instruction, target: "marker", data: "name='A'"}
    work = 3 * byte_size(node.target) + 13 * byte_size(node.data)
    Limits.install(%{limits | max_html_tree_work: work})
    assert PI.attributes(node) == {:ok, %{"name" => "A"}}

    assert {:error, {:resource_limit_exceeded, diagnostic}} = PI.attribute(node, "name")
    assert diagnostic.stage == :html and diagnostic.module == HtmlValidator
    assert diagnostic.message =~ "max_html_tree_work"

    Limits.install(%{limits | max_html_tree_work: work - 1})
    assert {:error, {:resource_limit_exceeded, _}} = PI.attributes(node)

    Limits.install(%{limits | max_html_tree_work: 10})

    for data <- [String.duplicate("broken ", 50), <<255>> <> String.duplicate("x", 50)] do
      assert {:error, {:resource_limit_exceeded, _}} = PI.attributes(%{node | data: data})
    end

    Limits.install(%{limits | max_html_tree_work: work})

    assert {:error, {:resource_limit_exceeded, _}} =
             HtmlValidator.with_render_budget(fn ->
               assert PI.attributes(node) == {:ok, %{"name" => "A"}}
               PI.attributes(node)
             end)

    Limits.install(%{limits | max_html_source_bytes: 3})
    assert {:error, {:resource_limit_exceeded, diagnostic}} = PI.attributes(node)
    assert diagnostic.message =~ "max_html_source_bytes"
    assert {:error, {:resource_limit_exceeded, _}} = PI.attribute(node, "name")
  end
end
