defmodule NativeElixirPdfUtilities.HtmlToPdf.CffEmbeddingTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.Font.{Cff, Sfnt}
  alias NativeElixirPdfUtilities.TestSupport.{CffFixture, Paths}

  alias NativeElixirPdfUtilities.Validators.{
    CffCharstringValidator,
    CffFontValidator,
    CffSubsetValidator,
    FontValidator
  }

  setup do
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    data = File.read!(Paths.fixture("fonts/cff-static-sans.otf"))
    {:ok, [font]} = Font.load_configured_faces([%{family: "Cff", data: data}])
    %{data: data, font: font}
  end

  test "PDF font dictionaries, custom encoding, aliases and both independent extractors agree", %{
    data: data,
    font: font
  } do
    directory =
      Path.join(System.tmp_dir!(), "cff-embedding-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)

    for data <- [data, File.read!(Paths.fixture("fonts/cff-static-cid.otf"))],
        subset <- [true, false] do
      assert {:ok, pdf} =
               HtmlToPdf.render("<p style='font-family:Cff;font-size:24pt'>Hℋx0</p>",
                 fonts: [%{family: "Cff", data: data}],
                 system_font_discovery: false,
                 subset_fonts: subset,
                 compress_streams: false
               )

      assert pdf =~ "/Subtype /CIDFontType0 "
      assert pdf =~ "/Subtype /CIDFontType0C"
      assert pdf =~ "/FontFile3 "
      refute pdf =~ "/CIDToGIDMap "
      assert pdf =~ "begincidchar"
      assert {:ok, "Hℋx0"} = Text.extract(pdf, layout: false)
      assert {:ok, %{pages: [%{spans: [span]}]}} = Text.extract_spans(pdf)
      assert_in_delta span.end_x - span.x, Font.text_width("Hℋx0", font, 24), 0.001
      path = Path.join(directory, "#{subset}.pdf")
      File.write!(path, pdf)
      assert {text, 0} = System.cmd("pdftotext", ["-enc", "UTF-8", path, "-"])
      assert String.trim(text) == "Hℋx0"
      assert {inspection, 0} = System.cmd("pdffonts", [path])
      assert inspection =~ "CID Type 0C"
      assert inspection =~ "Custom"
    end
  end

  test "subsets preserve raw programs and private/subroutine owners while sparse CIDs stay original glyph IDs",
       %{font: font} do
    for subset <- [true, false] do
      assert {:ok, output, ^subset} = Cff.subset(font, [font.cmap[?H]], subset)
      retained = if subset, do: [0, font.cmap[?H]], else: Enum.to_list(0..4)
      assert {:ok, parsed} = CffCharstringValidator.prepare(output, length(retained))
      assert parsed.cid? and parsed.charset == tl(retained)
      assert elem(parsed.fonts, 0).private == elem(font.cff.fonts, 0).private
      assert parsed.global_subrs == font.cff.global_subrs

      for {original, index} <- Enum.with_index(retained) do
        assert elem(parsed.charstrings, index) == elem(font.cff.charstrings, original)
        assert elem(parsed.glyphs, index).bbox == elem(font.cff.glyphs, original).bbox
      end
    end
  end

  test "font licensing controls full embedding and prohibits restricted or bitmap-only sources",
       %{data: data} do
    for {flags, permitted} <- [{0x0100, true}, {0x0002, false}, {0x0200, false}] do
      changed =
        rebuild(data, "OS/2", fn bytes ->
          <<before::binary-size(8), _::16, rest::binary>> = bytes
          <<before::binary, flags::16, rest::binary>>
        end)

      if permitted do
        assert {:ok, [font]} = Font.load_configured_faces([%{family: "Cff", data: changed}])
        assert {:ok, embedded, false} = Cff.subset(font, [font.cmap[?H]], true)
        assert {:ok, face} = CffFontValidator.prepare(embedded, 5)
        assert tuple_size(face.charstrings) == 5

        assert {:ok, pdf} =
                 HtmlToPdf.render("<p style='font-family:Cff'>H</p>",
                   fonts: [%{family: "Cff", data: changed}],
                   system_font_discovery: false,
                   compress_streams: false
                 )

        refute pdf =~ ~r"/BaseFont /[A-Z]{6}\+"
      else
        assert {:error, {:invalid_document, %{stage: :font, message: message}}} =
                 HtmlToPdf.render("<p>H</p>", fonts: [%{family: "Cff", data: changed}])

        assert message =~ "does not permit outline embedding"

        assert {:error, {:invalid_document, %{stage: :font}}} =
                 Cff.subset(%{data: changed, family: "Cff"}, [3], true)
      end
    end
  end

  test "CFF metrics include top and per-FD affine matrices and exact cubic extrema", %{font: font} do
    assert {:ok, bounds} = FontValidator.prepare_glyph_bounds(font)
    assert elem(bounds, font.cmap[?x]) == {59.0, 0.0, 1145.0, 1120.0}
    assert {:ok, xheight} = FontValidator.prepare_x_height(font)
    assert_in_delta xheight, 1120 / 2048, 1.0e-10
    program = <<139, 139, 21, 239, 239, 239, 39, 239, 139, 8, 14>>

    data =
      CffFixture.face_fixture(
        cid: true,
        glyph_count: 1,
        charstrings: [program],
        fd_matrix: [2, 0, 0, 3, 10, 20],
        top: %{{12, 7} => [1, 0, 0, 1, 0, 0]}
      )

    assert {:ok, face} = CffCharstringValidator.prepare_metrics(data, 1, 1000)
    assert {10000, 20000, 610_000, maximum} = elem(face.glyph_bounds, 0)
    assert_in_delta maximum, 153_333.33333333334, 0.0001

    for units <- [0, :bad, 16385] do
      assert {:error, {:invalid_font, %{source: "CFF metrics"}}} =
               CffCharstringValidator.prepare_metrics(data, 1, units)
    end
  end

  test "malformed font directories, programs and subset requests retain diagnostics", %{
    data: data,
    font: font
  } do
    for {tag, bytes} <- [
          {"CFF ", <<>>},
          {"maxp", <<0x00010000::32, 5::16>>},
          {"CFF ", <<2, 0, 5, 4, 0>>}
        ] do
      assert {:error, {:invalid_font, %{stage: :font, source: source, message: message}}} =
               FontValidator.prepare_ttf(rebuild(data, tag, fn _ -> bytes end))

      assert is_binary(source) and is_binary(message)
    end

    assert {:error, {:invalid_font, %{stage: :font, operation: :glyph_bounds, source: "Cff"}}} =
             Font.glyph_bounds(%{font | data: <<"OTTO", 0>>})

    broken = rebuild(data, "CFF ", fn _ -> <<>> end)

    assert {:error, {:invalid_font, %{stage: :font, operation: :glyph_bounds}}} =
             Font.glyph_bounds(%{font | data: broken})

    for {input, glyphs, subset} <- [
          {nil, [], true},
          {font, [-1], true},
          {font, [5], true},
          {font, [], nil},
          {font, :bad, true},
          {font, [1 | :improper], true}
        ] do
      assert {:error, {:invalid_font, %{operation: :write_pdf}}} =
               CffSubsetValidator.prepare(input, glyphs, subset)
    end

    Limits.install(%{Limits.effective() | max_font_subset_work: 1})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             Cff.subset(font, [1, 2], true)

    assert message =~ "max_font_subset_work"
    Limits.install(Limits.defaults())
    Limits.install(%{Limits.effective() | max_font_subset_bytes: 1})

    assert {:error, {:resource_limit_exceeded, %{message: message}}} =
             Cff.subset(font, [font.cmap[?H]], true)

    assert message =~ "max_font_subset_bytes"
  end

  test "CID-keyed source subroutines and FD dictionaries survive conversion", %{
    data: data,
    font: font
  } do
    raw =
      CffFixture.face_fixture(
        cid: true,
        glyph_count: 5,
        font_count: 2,
        fd_select: <<0, 0, 0, 1, 1, 0>>,
        charstrings: Tuple.to_list(font.cff.charstrings)
      )

    input = rebuild(data, "CFF ", fn _ -> raw end)
    assert {:ok, [source]} = Font.load_configured_faces([%{family: "CID CFF", data: input}])
    assert {:ok, output, true} = Cff.subset(source, [2, 3], true)
    assert {:ok, face} = CffCharstringValidator.prepare(output, 3)
    assert face.fd_select == {0, 1, 1}
    assert face.global_subrs == source.cff.global_subrs

    for fd <- [0, 1] do
      assert elem(face.fonts, fd).local_subrs == elem(source.cff.fonts, fd).local_subrs
      assert elem(face.fonts, fd).private[20] == elem(source.cff.fonts, fd).private[20]
    end

    assert {:ok, pdf} =
             HtmlToPdf.render("<p style='font-family:Cid;font-size:24pt'>Hx0</p>",
               fonts: [%{family: "Cid", data: input}],
               system_font_discovery: false
             )

    assert {:ok, "Hx0"} = Text.extract(pdf, layout: false)
    # OpenType hmtx advances govern layout even when a shared CFF's PostScript
    # advances differ. Microsoft OpenType hmtx, published 2023-01-10:
    # https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx
    assert elem(source.cff.glyphs, source.cmap[?H]).width ==
             Enum.at(source.widths, source.cmap[?H]) + 100

    assert {:ok, %{pages: [%{spans: [span]}]}} = Text.extract_spans(pdf)
    assert_in_delta span.end_x - span.x, Font.text_width("Hx0", source, 24), 0.001
  end

  test "legacy composition is flattened before CID embedding and registry limits retain diagnostics",
       %{data: data} do
    # A CID-keyed program cannot retain seac. Emit the resolved component paths.
    seac = <<139, 139, 204, 205, 14>>

    raw =
      CffFixture.face_fixture(
        glyph_count: 5,
        charset: <<0, 34::16, 35::16, 36::16, 37::16>>,
        charstrings: [<<14>>, <<14>>, <<14>>, <<14>>, seac]
      )

    input = rebuild(data, "CFF ", fn _ -> raw end)
    assert {:ok, [source]} = Font.load_configured_faces([%{family: "Legacy", data: input}])
    assert {:ok, output, true} = Cff.subset(source, [4], true)
    assert {:ok, face} = CffCharstringValidator.prepare(output, 2)
    assert elem(face.glyphs, 1).seac == nil
    raw = CffFixture.face_fixture(glyph_count: 5, strings: List.duplicate("", 64608))
    input = rebuild(data, "CFF ", fn _ -> raw end)
    assert {:ok, [source]} = Font.load_configured_faces([%{family: "Full strings", data: input}])
    assert {:error, {:invalid_font, %{message: message}}} = Cff.subset(source, [1], true)
    assert message =~ "registry strings"

    raw =
      CffFixture.face_fixture(
        glyph_count: 5,
        strings: ["Adobe", "Identity" | List.duplicate("", 64607)]
      )

    input = rebuild(data, "CFF ", fn _ -> raw end)

    assert {:ok, [source]} =
             Font.load_configured_faces([%{family: "Reused registry", data: input}])

    assert {:ok, output, true} = Cff.subset(source, [1], true)
    assert {:ok, face} = CffFontValidator.prepare(output, 2)
    assert tuple_size(face.strings) == 64609
    assert face.top[{12, 30}] == [391, 392, 0]
  end

  test "dictionary number encoding retains signed reals and positive exponents" do
    Limits.install(%{Limits.effective() | max_font_numeric_magnitude: round(1.0e21)})

    assert {:ok, decoded} =
             CffFontValidator.prepare_dictionary(
               Cff.dictionary(%{5 => [-2.25, 1.0e20, 0.001, 0.0]})
             )

    assert decoded[5] == [-2.25, 1.0e20, 0.001, 0.0]
  end

  test "the public writer uses actual CFF bytes and returns malformed-source diagnostics even without subsetting",
       %{font: font} do
    alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter

    for face <- [
          Map.delete(font, :outline_format),
          Map.put(font, :outline_format, :caller_annotation)
        ] do
      box = %{
        type: :text,
        text: "H",
        x: 10,
        y: 20,
        font: Font.pdf_name(font),
        font_face: face,
        font_size: 20,
        color: {0, 0, 0}
      }

      assert {:ok, pdf} =
               PdfWriter.render([%{size: {100, 100}, boxes: [box]}], subset_fonts: false)

      assert pdf =~ "/Subtype /CIDFontType0C"
      assert {:ok, "H"} = Text.extract(pdf, layout: false)
      box = %{box | font_face: %{face | data: <<"OTTO", 0>>}}

      assert {:error, {:invalid_font, %{stage: :font, operation: :write_pdf}}} =
               PdfWriter.render([%{size: {100, 100}, boxes: [box]}], subset_fonts: false)
    end
  end

  defp rebuild(data, tag, change) do
    {:ok, scaler, directory} = FontValidator.prepare_table_directory(data)

    tables =
      Enum.map(directory, fn {name, {start, length}} ->
        bytes = binary_part(data, start, length)
        {name, if(name == tag, do: change.(bytes), else: bytes)}
      end)

    Sfnt.rebuild(scaler, tables)
  end
end
