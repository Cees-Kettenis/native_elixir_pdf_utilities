Code.require_file(NativeElixirPdfUtilities.TestSupport.Paths.support("open_type_fixture.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.EmojiPresentationTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, PdfWriter, UnicodeTextData}
  alias NativeElixirPdfUtilities.TestSupport.{FontCorpus, FontFixture, OpenTypeFixture}

  alias NativeElixirPdfUtilities.Validators.{
    ColorFontValidator,
    EmojiPresentationValidator,
    FontRenderingValidator,
    WriterValidator
  }

  test "text presentation clears a selected bitmap strike without reusing its measurement identity" do
    alias NativeElixirPdfUtilities.Validators.BitmapFontValidator
    data = FontCorpus.path("selected-cbdt.ttf") |> File.read!()

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "Bitmap presentation", data: data}])

    font = %{font | cmap: Map.put(font.cmap, 0x2764, font.cmap[?H])}
    assert {:ok, bitmap} = BitmapFontValidator.prepare_metrics(font, 20)
    assert is_tuple(bitmap.bitmap_widths)
    text = EmojiPresentationValidator.prepare(bitmap, "❤︎")
    assert text.emoji_presentation == :text
    refute Map.has_key?(text, :bitmap_widths)
    assert text.id == bitmap.id <> "-text-presentation"
    assert text.data == bitmap.data and text.widths == bitmap.widths
  end

  test "standardized variation selectors override CSS presentation without affecting ordinary characters" do
    assert UnicodeTextData.emoji_variant?(?0)
    assert UnicodeTextData.emoji_variant?(0x2764)
    refute UnicodeTextData.emoji_variant?(?A)

    for {mode, text, expected} <- [
          {:normal, "❤", :normal},
          {:text, "❤", :text},
          {:emoji, "❤", :emoji},
          {:unicode, "❤", :text},
          {:unicode, "⌚", :emoji},
          {:text, "❤️", :emoji},
          {:emoji, "❤︎", :text},
          {:emoji, "A", :normal},
          {:unicode, "", :normal}
        ] do
      font = %{feature_plan: %{emoji: mode}}
      assert EmojiPresentationValidator.preference(text, font) == expected
    end

    assert EmojiPresentationValidator.matching_required?("a👩‍💻b", %{})
    assert EmojiPresentationValidator.matching_required?("🇳🇱", %{})
    assert EmojiPresentationValidator.matching_required?("0", %{feature_plan: %{emoji: :text}})
    refute EmojiPresentationValidator.matching_required?("ordinary text", %{})
    assert EmojiPresentationValidator.sequence?("👩‍💻")
    refute EmojiPresentationValidator.sequence?("Á")
  end

  test "fallback prefers a font that shapes a complete registered sequence over individual members" do
    set =
      OpenTypeFixture.join(
        fn [offset] -> OpenTypeFixture.words([1, offset]) end,
        [OpenTypeFixture.words([9, 3, 3, 2])]
      )

    ligature =
      OpenTypeFixture.join(
        fn [covered, set] -> OpenTypeFixture.words([1, covered, 1, set]) end,
        [OpenTypeFixture.coverage([1]), set]
      )

    cmap = %{0x1F469 => 1, 0x1F4BB => 2, 0x200D => 3}
    separate = OpenTypeFixture.font() |> Map.put(:cmap, cmap) |> Map.put(:id, "separate")

    joined =
      OpenTypeFixture.font([%{type: 4, subtables: [ligature]}], [], features: [{"ccmp", [0]}])
      |> Map.put(:cmap, cmap)
      |> Map.put(:id, "joined")

    assert FontRenderingValidator.supports_text?(separate, "👩‍💻")
    assert FontRenderingValidator.select_face([separate, joined], "👩‍💻").id == "joined"
    assert FontRenderingValidator.select_face([joined, separate], "👩‍💻").id == "joined"
    missing = %{separate | cmap: %{0x1F469 => 1}}
    refute FontRenderingValidator.supports_text?(missing, "👩‍💻")
    assert FontRenderingValidator.select_face([missing], "👩‍💻") == nil
    assert FontRenderingValidator.select_face([separate], "👩‍💻").id == "separate"
  end

  test "CSS emoji presentation selects an ASCII digit's cmap variation in measurement and painting" do
    data = OpenTypeFixture.outline_font([], [], cmap: %{?0 => 1, ?A => 2}, widths: %{9 => 900})
    {_, tables} = FontFixture.tables(data)
    original = binary_part(tables["cmap"], 12, byte_size(tables["cmap"]) - 12)
    # OpenType cmap format 14: one non-default VS16 mapping, 0 -> glyph 9.
    variation = <<14::16, 30::32, 1::32, 0xFE0F::24, 0::32, 21::32, 1::32, ?0::24, 9::16>>

    cmap =
      <<0::16, 2::16, 3::16, 10::16, 20::32, 0::16, 5::16, 20 + byte_size(original)::32,
        original::binary, variation::binary>>

    data = FontFixture.rebuild(data, %{"cmap" => cmap})

    assert {:ok, [font]} =
             Font.load_configured_faces([%{family: "ASCII presentation", data: data}])

    assert Font.text_width("0", font, 12) == 6

    assert {:ok, emoji} =
             FontRenderingValidator.prepare(font, %{
               font_controls: %{:"variant-emoji" => ["emoji"]}
             })

    assert Font.text_width("0", emoji, 12) == 10.8

    assert {:ok, text} =
             FontRenderingValidator.prepare(font, %{
               font_controls: %{:"variant-emoji" => ["text"]}
             })

    assert Font.text_width("0", text, 12) == 6

    assert {:ok, pdf} =
             HtmlToPdf.render("<p style='font-family:Authored;font-variant-emoji:emoji'>0</p>",
               system_font_discovery: false,
               fonts: [%{family: "Authored", data: data}]
             )

    assert {:ok, "0"} = Text.extract(pdf, layout: false)
  end

  test "available color faces and monochrome outlines honor CSS and selectors while preserving extraction" do
    data = FontCorpus.path("selected-colr.ttf") |> File.read!()
    assert {:ok, [color]} = Font.load_configured_faces([%{family: "Color", data: data}])
    # Reuse the existing H outline and color artwork for an authored emoji scalar.
    # This changes only the admitted map in memory, without adding a font asset.
    color = %{color | cmap: Map.put(color.cmap, 0x2764, color.cmap[?H])}
    mono = Map.delete(color, :color_tables) |> Map.put(:id, "monochrome")

    for {mode, expected_id} <- [{:emoji, color.id}, {:text, mono.id}, {:unicode, mono.id}] do
      candidates =
        for font <- [mono, color] do
          {:ok, selected} =
            FontRenderingValidator.prepare(font, %{
              font_controls: %{:"variant-emoji" => [Atom.to_string(mode)]}
            })

          selected
        end

      selected = FontRenderingValidator.select_face(candidates, "❤")
      assert String.starts_with?(selected.id, expected_id)
    end

    for {mode, text, images} <- [
          {:emoji, "❤", 1},
          {:text, "❤", 0},
          {:unicode, "❤", 0},
          {:emoji, "❤︎", 0},
          {:text, "❤️", 1},
          {:text, "H0", 1}
        ] do
      {:ok, font} =
        FontRenderingValidator.prepare(color, %{
          font_controls: %{:"variant-emoji" => [Atom.to_string(mode)]}
        })

      box = %{
        type: :text,
        text: text,
        x: 10,
        y: 10,
        font: Font.pdf_name(font),
        font_face: font,
        font_size: 15,
        color: {0, 0, 0}
      }

      page = %{size: {150, 100}, boxes: [box]}
      assert {:ok, context} = WriterValidator.prepare([page], [])
      assert {:ok, expanded} = ColorFontValidator.prepare(context, [])
      assert Enum.count(hd(expanded.pages).boxes, &(&1.type == :image)) == images
      assert {:ok, pdf} = PdfWriter.render([page], compress_streams: false)
      assert {:ok, ^text} = Text.extract(pdf, layout: false)
    end
  end

  test "HTML presentation matching selects configured faces per cluster within one line" do
    color = FontCorpus.path("selected-colr.ttf") |> File.read!()
    {:ok, [font]} = Font.load_configured_faces([%{family: "Color", data: color}])
    # The corpus's H has color artwork. Point the emoji-participating zero at
    # that same authored glyph in the in-memory font's actual cmap table.
    cmap =
      <<0::16, 1::16, 3::16, 10::16, 12::32, 12::16, 0::16, 28::32, 0::32, 1::32, ?0::32, ?0::32,
        font.cmap[?H]::32>>

    color = FontFixture.rebuild(color, %{"cmap" => cmap})
    outline = FontFixture.rebuild(color, %{"COLR" => nil, "CPAL" => nil})
    fonts = [%{family: "Outline", data: outline}, %{family: "Color", data: color}]

    for {mode, images} <- [{"emoji", 1}, {"text", 0}, {"unicode", 0}] do
      html = "<p style='font-family:Outline,Color;font-variant-emoji:#{mode}'>0</p>"

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 fonts: fonts,
                 system_font_discovery: false,
                 compress_streams: false
               )

      assert length(Regex.scan(~r/\/Subtype \/Image\b/, pdf)) == images * 2
      assert {:ok, "0"} = Text.extract(pdf, layout: false)
    end
  end
end
