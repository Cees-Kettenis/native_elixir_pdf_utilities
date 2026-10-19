alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.RubyFragmentContractTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, ResourceContext, RubyLayout, Style}
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, RubyValidator}

  @font Paths.fixture("html_to_pdf/browser_parity/assets/cjk-ruby-fixture.ttf")
  @options [fonts: [%{family: "CJK Ruby Fixture", path: @font}], system_font_discovery: false]

  test "nested styled fragments partition without losing source children or graphemes" do
    ruby =
      styled_ruby(
        "<ruby><span><b>ÁB</b><i style='display:none'>hidden</i>CD</span><rt>reading text</rt></ruby>"
      )

    assert {:ok, [group]} = RubyValidator.prepare(ruby)
    assert RubyValidator.group_length(group) == 4
    {first, rest} = RubyValidator.partition_group(group, 1, [3])
    assert RubyValidator.group_length(first) == 1
    assert RubyValidator.group_length(rest) == 3
    assert hd(first.base).node_id == hd(group.base).node_id
    assert hd(rest.base).node_id == hd(group.base).node_id
    assert hd(first.annotations).node_id == hd(rest.annotations).node_id
    assert hd(first.annotations).node_id == hd(group.annotations).node_id

    assert [%{children: [%{children: [%{text: "Á"}]}]}] = first.base
    assert [%{children: [%{children: [%{text: "B"}]}, hidden, %{text: "CD"}]}] = rest.base
    assert hidden.style.display == :none
    assert hd(first.annotations).children |> Enum.map_join(& &1.text) == "rea"
    assert hd(rest.annotations).children |> Enum.map_join(& &1.text) == "ding text"
  end

  test "atomic and empty base children keep indivisible slots in fragment boundaries" do
    ruby =
      styled_ruby(
        "<ruby><span>A <i style='display:none'>hidden</i><span style='display:inline-block'>BOX</span><span></span><ruby>N<rt>n</rt></ruby> B</span><rt>long reading text</rt></ruby>"
      )

    assert {:ok, [group]} = RubyValidator.prepare(ruby)
    assert RubyValidator.group_length(group) == 7
    assert RubyValidator.splittable_group?(group, 20)
    {first, rest} = RubyValidator.partition_group(group, 3, [0])
    assert RubyValidator.group_length(first) == 3
    assert RubyValidator.group_length(rest) == 4
    [%{children: first_children}] = first.base
    assert Enum.any?(first_children, &(Map.get(&1.style, :display) == :inline_block))
    [%{children: rest_children}] = rest.base
    assert Enum.any?(rest_children, &(Map.get(&1, :tag) == "ruby"))
    assert Enum.any?(rest_children, &(Map.get(&1, :children) == []))
    assert hd(first.annotations).children == []
  end

  test "adjacent Latin annotation columns stay one unbreakable word" do
    ruby = styled_ruby("<ruby>A<rt>first</rt>B<rt>second</rt></ruby>")
    assert {:ok, groups} = RubyValidator.prepare(ruby)
    assert length(groups) == 2
    assert {:ok, [measured]} = RubyLayout.measure(ruby, nil)
    assert Enum.filter(measured.boxes, &(Map.get(&1, :text) in ["A", "B"])) |> length() == 2

    assert Enum.filter(measured.boxes, &(Map.get(&1, :text) in ["first", "second"])) |> length() ==
             2

    assert measured.width > measured.base_width
    assert {:ok, nil} = RubyLayout.split(measured, measured.width / 2)
  end

  test "a long reading permits proportional fragmentation of a short breakable base" do
    ruby = styled_ruby("<ruby>A B<rt>longreading</rt></ruby>")
    assert {:ok, [measured]} = RubyLayout.measure(ruby, nil)
    assert RubyValidator.group_length(measured.source_group) == 3
    assert RubyValidator.splittable_group?(measured.source_group, measured.base_width)

    assert {:ok, {first, last}} =
             ResourceContext.run([], HtmlValidator.new_image_budget(), fn _context ->
               RubyLayout.split(measured, measured.width * 0.8)
             end)

    assert first.width <= measured.width * 0.8 + 0.75 / 64
    assert first.width > 0 and last.width > 0
    assert Enum.map_join(first.source_group.base ++ last.source_group.base, & &1.text) == "A B"
  end

  test "annotation roles cannot be assigned to ordinary styled elements" do
    ruby = styled_ruby("<ruby>Base<rt>Reading</rt></ruby>")
    ordinary = %{ruby | style: Map.put(ruby.style, :_ruby_annotation, true)}

    assert {:error, {:invalid_layout, diagnostic}} =
             RubyValidator.validate_layout_metadata(ordinary)

    assert diagnostic.stage == :layout
    assert diagnostic.operation == :measure_ruby
    assert diagnostic.message =~ "only to styled rt"
  end

  test "ruby preparation rejects nonpainting caller nodes and exhausted tree work" do
    ruby = styled_ruby("<ruby>Base<rt>Reading</rt></ruby>")
    malformed = %{ruby | children: [%{type: :processing_instruction, target: "unpainted"}]}
    assert {:error, {:invalid_layout, diagnostic}} = RubyValidator.prepare(malformed)
    assert diagnostic.message =~ "validated styled node"

    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(Map.put(previous, :max_html_tree_work, 1))
    assert {:error, {:resource_limit_exceeded, diagnostic}} = RubyValidator.prepare(ruby)
    assert diagnostic.message =~ "max_html_tree_work"
  end

  test "empty readings reject unreadable caller font metadata before emitting paint" do
    ruby =
      styled_ruby(
        "<ruby style='font-family:CJK Ruby Fixture'>東京<rt style='font-family:DejaVu Sans'></rt></ruby>"
      )

    [base, annotation] = ruby.children
    font = annotation.style.font_face
    broken = %{font | data: binary_part(font.data, 0, 5)}
    annotation = %{annotation | style: Map.put(annotation.style, :font_face, broken)}

    assert {:error, {:invalid_layout, diagnostic}} =
             RubyLayout.measure(%{ruby | children: [base, annotation]}, nil)

    assert diagnostic.stage == :layout
    assert diagnostic.operation == :measure_ruby
    assert diagnostic.message =~ "readable typographic metrics"
  end

  test "proportional fitting retains font-source failures for readings and bases" do
    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)

    for source <- [
          "<ruby style='font-family:CJK Ruby Fixture'>東京京都大阪<rt style='font-family:DejaVu Sans'>long reading text</rt></ruby>",
          "<ruby>A B C D E<rt style='font-family:CJK Ruby Fixture'>東京京都</rt></ruby>"
        ] do
      Limits.install(previous)
      ruby = styled_ruby(source)
      assert {:ok, [measured]} = RubyLayout.measure(ruby, nil)
      Limits.install(Map.put(previous, :max_font_source_bytes, File.stat!(@font).size))

      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               RubyLayout.split(measured, measured.width / 2)

      assert diagnostic.message =~ "max_font_source_bytes"
    end
  end

  test "annotation measurement retains selected font source-budget failures" do
    ruby =
      styled_ruby(
        "<ruby style='font-family:CJK Ruby Fixture'>東京<rt style='font-family:DejaVu Sans'>abcdefghijklmnopqrstuvwxyz</rt></ruby>"
      )

    previous = Limits.effective()
    on_exit(fn -> Limits.install(previous) end)
    Limits.install(Map.put(previous, :max_font_source_bytes, File.stat!(@font).size))

    assert {:error, {:resource_limit_exceeded, diagnostic}} = RubyLayout.measure(ruby, nil)
    assert diagnostic.message =~ "max_font_source_bytes"
  end

  defp styled_ruby(source) do
    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, %{children: [ruby]}} = Style.compute_detailed(dom, @options)
    ruby
  end
end
