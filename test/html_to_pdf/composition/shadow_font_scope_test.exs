alias NativeElixirPdfUtilities.TestSupport.Paths

defmodule NativeElixirPdfUtilities.HtmlToPdf.ShadowFontScopeTest do
  use ExUnit.Case, async: true

  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, FontFallback, HtmlParser, Style}

  test "duplicate shadow family names select independent bytes and metrics with inherited declaration scope" do
    mono = File.read!(Paths.project("priv/fonts/dejavu/DejaVuSansMono.ttf"))
    serif = File.read!(Paths.project("priv/fonts/dejavu/DejaVuSans-Bold.ttf"))
    sans = File.read!(Paths.project("priv/fonts/dejavu/DejaVuSans.ttf"))

    html = """
    <style>@font-face{font-family:Shared;src:url(sans.ttf)}body{font-family:Shared}</style>
    <div><template shadowrootmode=open><style>@font-face{font-family:Shared;src:url(mono.ttf)}.local{font-family:Shared}</style><span>Inherited</span><span class=local>Mono</span><section><template shadowrootmode=open><span style='font-family:Shared'>Nested</span></template></section></template></div>
    <div><template shadowrootmode=closed><style>@font-face{font-family:Shared;src:url(serif.ttf)}span{font-family:Shared}</style><span>Serif</span></template></div><span>Outside</span>
    """

    opts = [
      system_font_discovery: false,
      assets: %{
        "mono.ttf" => {:bytes, mono},
        "serif.ttf" => {:bytes, serif},
        "sans.ttf" => {:bytes, sans}
      }
    ]

    nodes = text_faces(html, opts)

    assert nodes["Mono"].data == mono
    assert nodes["Serif"].data == serif
    assert nodes["Inherited"].data == sans
    assert nodes["Outside"].data == sans
    assert nodes["Nested"].data == mono
    refute nodes["Mono"].id == nodes["Serif"].id

    refute Font.text_width("iiii", nodes["Mono"], 12) ==
             Font.text_width("iiii", nodes["Serif"], 12)
  end

  test "document and shadow font families retain their distinct width faces" do
    sans = File.read!(Paths.fixture("fonts/selected-container-sans.ttf"))
    mono = File.read!(Paths.fixture("fonts/selected-container-mono.ttf"))

    opts = [
      system_font_discovery: false,
      assets: %{"sans.ttf" => {:bytes, sans}, "mono.ttf" => {:bytes, mono}}
    ]

    document = """
    <style>
    @font-face {font-family:Demo;src:url(sans.ttf);font-stretch:75%}
    @font-face {font-family:Demo;src:url(mono.ttf);font-stretch:125%}
    p {font-family:Demo;font-stretch:125%}
    </style><p>Hx</p>
    """

    empty = "<div><template shadowrootmode=open></template></div>"

    local = """
    <div><template shadowrootmode=open><style>
    @font-face {font-family:Demo;src:url(mono.ttf);font-stretch:75%}
    @font-face {font-family:Demo;src:url(sans.ttf);font-stretch:125%}
    p {font-family:Demo;font-stretch:125%}
    </style><p>xH</p></template></div>
    """

    outer = text_faces(document, opts)["Hx"]
    assert outer.data == mono
    assert text_faces(document <> empty, opts)["Hx"].id == outer.id
    faces = text_faces(document <> local, opts)
    assert faces["Hx"].id == outer.id
    assert faces["xH"].data == sans
  end

  test "cloned shadow roots keep same-named faces in their fresh destination scopes" do
    mono = File.read!(Paths.project("priv/fonts/dejavu/DejaVuSansMono.ttf"))
    bold = File.read!(Paths.project("priv/fonts/dejavu/DejaVuSans-Bold.ttf"))

    html = """
    <style>select::picker-icon{display:none}</style>
    <select style='appearance:base-select'><button><selectedcontent></selectedcontent></button><option selected>
    <span><template shadowrootmode=open shadowrootclonable><style>@font-face{font-family:Shared;src:url(mono.ttf)}span{font-family:Shared}</style><span>Mono clone</span></template></span>
    <span><template shadowrootmode=open shadowrootclonable><style>@font-face{font-family:Shared;src:url(bold.ttf)}span{font-family:Shared}</style><span>Bold clone</span></template></span>
    </option></select>
    """

    opts = [
      system_font_discovery: false,
      assets: %{"mono.ttf" => {:bytes, mono}, "bold.ttf" => {:bytes, bold}}
    ]

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, opts)
    assert {:ok, resolved} = FontFallback.resolve(styled, :error)

    clones =
      Stream.unfold(resolved.children, fn
        [] -> nil
        [node | rest] -> {node, Map.get(node, :children, []) ++ rest}
      end)
      |> Enum.filter(
        &(Map.get(&1, :type) == :text and Map.has_key?(&1, :cloned_from_node_id) and
            String.trim(&1.text) != "")
      )

    assert Enum.sort(Enum.map(clones, & &1.text)) == ["Bold clone", "Mono clone"]

    for clone <- clones do
      assert clone.node_id != clone.cloned_from_node_id
      assert clone.style.font_face.data == if(clone.text == "Mono clone", do: mono, else: bold)
    end
  end

  defp text_faces(html, opts) do
    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, styled} = Style.compute_detailed(dom, opts)
    assert {:ok, resolved} = FontFallback.resolve(styled, :error)

    Stream.unfold(resolved.children, fn
      [] -> nil
      [node | rest] -> {node, Map.get(node, :children, []) ++ rest}
    end)
    |> Enum.filter(&(Map.get(&1, :type) == :text and String.trim(&1.text) != ""))
    |> Map.new(&{&1.text, &1.style.font_face})
  end
end
