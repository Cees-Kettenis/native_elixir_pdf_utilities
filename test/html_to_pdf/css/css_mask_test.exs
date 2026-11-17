defmodule NativeElixirPdfUtilities.HtmlToPdf.CssMaskTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.{HtmlToPdf, Text}
  alias NativeElixirPdfUtilities.HtmlToPdf.CssMask
  alias NativeElixirPdfUtilities.Validators.CssMaskValidator

  test "Porter-Duff mask operators match independent alpha vectors and reject malformed planes" do
    for {operator, expected} <- [
          {"add", 192},
          {"subtract", 64},
          {"intersect", 64},
          {"exclude", 127}
        ] do
      assert {:ok, <<^expected>>} = CssMask.composite(<<128>>, <<128>>, operator)
    end

    assert {:ok, <<>>} = CssMask.composite(<<>>, <<>>, "add")
    assert {:error, {:invalid_layout, diagnostic}} = CssMask.composite(<<0>>, <<>>, "add")
    assert diagnostic.source == "mask"
    assert {:error, {:invalid_layout, _}} = CssMask.composite(<<0>>, <<0>>, "unknown")
  end

  test "layered alpha and luminance masks retain source geometry and logical text" do
    for mode <- ["alpha", "luminance"] do
      html =
        "<style>html,body{margin:0;font:12pt/18pt DejaVu Sans}div{width:80pt;height:45pt;background:red;mask-image:linear-gradient(90deg,#0008,transparent),linear-gradient(white,white);mask-mode:#{mode};mask-composite:intersect}</style><div>Masked ink</div>"

      assert {:ok, pdf} =
               HtmlToPdf.render(html,
                 page_size: {180, 120},
                 margin: 0,
                 system_font_discovery: false
               )

      assert pdf =~ "/SMask"
      assert {:ok, text} = Text.extract(pdf)
      assert text =~ "Masked ink"
    end
  end

  test "mask grammar reuses image and border geometry and rejects unknown compositing modes" do
    assert {:ok, style} =
             CssMaskValidator.prepare(
               %{},
               "mask",
               "linear-gradient(white,black) center / 80% 60% no-repeat alpha subtract"
             )

    assert style.css_mask.modes == ["alpha"]
    assert style.css_mask.composites == ["subtract"]
    assert :error = CssMaskValidator.prepare(%{}, "mask-mode", "unknown")
    assert :error = CssMaskValidator.prepare(%{}, "mask-composite", "add,")

    assert {:ok, style} =
             CssMaskValidator.prepare(
               %{},
               "mask-border",
               "linear-gradient(white,black) 20% / 3pt round"
             )

    assert style.css_mask.border.border_image_repeat == {:round, :round}
  end

  test "mask longhands retain independent cascade winners and CSS-wide resets" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

    source =
      "<style>.mask{mask:linear-gradient(white,white) left top / 50% 80% no-repeat luminance exclude;mask-size:initial;mask-mode:alpha!important}div{mask-mode:luminance}</style><div class='mask'><p style='mask-repeat:inherit'>Plain</p></div>"

    assert {:ok, dom} = HtmlParser.parse_detailed(source)
    assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, system_font_discovery: false)
    assert node.style.css_mask.modes == ["alpha"]
    assert node.style.css_mask.composites == ["exclude"]
    assert node.style.css_mask.background.background_size == {:auto, :auto}

    assert Map.get(hd(node.children).style.css_mask.background, :background_repeats, [
             hd(node.children).style.css_mask.background.background_repeat
           ]) == [:no_repeat]
  end

  test "nine-part mask borders retain the unfilled center and static fragmentation" do
    html =
      "<style>html,body{margin:0;font:10pt/15pt DejaVu Sans}div{width:80pt;border:5pt solid red;mask-border:linear-gradient(white,transparent) 20% / 5pt}p{margin:0}</style><div><p>First mask fragment</p><p style='break-before:page'>Second mask fragment</p></div>"

    assert {:ok, pdf} = HtmlToPdf.render(html, page_size: {120, 90}, margin: 0, forms: :static)
    assert {:ok, text} = Text.extract(pdf)
    assert text =~ "First mask"
    assert text =~ "Second mask"
    assert {:ok, context} = NativeElixirPdfUtilities.Pdf.Reader.read_validated(pdf)
    assert length(context.document.pages) == 2
  end
end
