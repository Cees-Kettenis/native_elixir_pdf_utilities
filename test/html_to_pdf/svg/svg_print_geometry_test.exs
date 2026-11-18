alias NativeElixirPdfUtilities.TestSupport.Paths
Code.require_file(Paths.support("svg_public_workflow.ex"))

defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgPrintGeometryTest do
  use ExUnit.Case, async: true
  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.PngDecoder
  alias NativeElixirPdfUtilities.TestSupport.SvgPublicWorkflow
  alias NativeElixirPdfUtilities.Validators.SvgPrintGeometryValidator

  @marker """
  <defs><marker id="Dot" viewBox="0 0 6 6" markerWidth="6" markerHeight="6"
    markerUnits="userSpaceOnUse" refX="3" refY="3" orient="auto">
    <rect x="1" y="1" width="4" height="4" fill="#008000"/>
  </marker></defs>
  """

  test "segment and repeating markers match independently positioned artwork through a transform" do
    actual =
      @marker <>
        """
        <g transform="translate(5 2) scale(2 1)">
          <polyline points="5,12 15,12 30,12" stroke="black" fill="none" marker-segment="url(#Dot)"/>
          <line x1="5" y1="26" x2="30" y2="26" stroke="black" marker-pattern="20% url(#Dot) 20%"/>
        </g>
        """

    expected = """
    <g transform="translate(5 2) scale(2 1)">
      <path d="M5 12H30M5 26H30" stroke="black" fill="none"/>
      <g fill="#008000">
        <rect x="8" y="10" width="4" height="4"/>
        <rect x="20.5" y="10" width="4" height="4"/>
        <rect x="8" y="24" width="4" height="4"/>
        <rect x="18" y="24" width="4" height="4"/>
        <rect x="28" y="24" width="4" height="4"/>
      </g>
    </g>
    """

    assert pixels(actual) == pixels(expected)
  end

  test "inside and outside square strokes match independent filled outlines" do
    actual = """
    <rect x="13" y="5" width="18" height="16" fill="blue" stroke="red" stroke-width="3" stroke-alignment="inside"/>
    <rect x="42" y="8" width="20" height="14" fill="blue" stroke="red" stroke-width="4" style="stroke-alignment:outside"/>
    """

    expected = """
    <rect x="13" y="5" width="18" height="16" fill="red"/>
    <rect x="16" y="8" width="12" height="10" fill="blue"/>
    <rect x="38" y="4" width="28" height="22" fill="red"/>
    <rect x="42" y="8" width="20" height="14" fill="blue"/>
    """

    assert pixels(actual) == pixels(expected)
  end

  test "dash corners and adjusted gaps preserve the hand-calculated segment intervals" do
    actual = """
    <line x1="4" y1="8" x2="28" y2="8" stroke="red" stroke-width="2"
      stroke-dasharray="3 4" stroke-dashcorner="2" stroke-dashadjust="stretch gaps"/>
    <line x1="6" y1="16" x2="25" y2="16" stroke="black" stroke-width="2"
      stroke-dasharray="5 5" stroke-dashadjust="compress"/>
    """

    # The red interior is 20 units. Each period stretches to 10, retaining
    # the 3-unit dash. The black stroke has two 9.5-unit periods.
    expected = """
    <path d="M4 8H9 M16 8H19 M26 8H28" stroke="red" stroke-width="2"/>
    <path d="M6 16H10.75 M15.5 16H20.25" stroke="black" stroke-width="2"/>
    """

    assert pixels(actual) == pixels(expected)
  end

  test "endpoint marker knockouts preserve the marker and trim only the covered stroke" do
    actual = """
    <defs><marker id="End" markerUnits="userSpaceOnUse" markerWidth="8" markerHeight="8"
      refX="4" refY="4" marker-knockout-left="-4" marker-knockout-right="4">
      <rect x="1" y="1" width="6" height="6" fill="none" stroke="green"/>
    </marker></defs>
    <line x1="10" y1="14" x2="30" y2="14" stroke="red" stroke-width="2"
      marker-start="url(#End)" marker-end="url(#End)"/>
    """

    expected = """
    <line x1="14" y1="14" x2="26" y2="14" stroke="red" stroke-width="2"/>
    <rect x="7" y="11" width="6" height="6" fill="none" stroke="green"/>
    <rect x="27" y="11" width="6" height="6" fill="none" stroke="green"/>
    """

    assert pixels(actual) == pixels(expected)
  end

  test "unsupported geometry and external advanced marker references keep diagnostics" do
    for body <- [
          ~s|<circle r="5" stroke="red" stroke-alignment="inside"/>|,
          ~s|<line x2="10" marker-segment="url(https://example.com/marker.svg#dot)"/>|,
          ~s|<line x2="10" marker-pattern="url(#Dot)"/>|,
          ~s|<rect x="2" width="10" height="10" rx="3" stroke="red" stroke-alignment="outside"/>|,
          ~s|<line x2="10" marker-pattern="0.0000001 url(#Dot)"/>|
        ] do
      assert {:error, {reason, _}} =
               HtmlToPdf.render(svg(@marker <> body), system_font_discovery: false)

      assert reason in [:invalid_document, :invalid_css, :resource_limit_exceeded]
    end

    for {property, value} <- [
          {"marker-pattern", "3"},
          {"marker-knockout-left", "circle"},
          {"stroke-dashadjust", "stretch other"},
          {"stroke-alignment", "left"},
          {"stroke-dashcorner", "-2"}
        ] do
      assert :error = SvgPrintGeometryValidator.prepare(property, value)
    end
  end

  test "repeating marker offsets traverse polyline vertices and preserve descriptive nodes" do
    actual =
      @marker <>
        """
        <polyline points="5,15 5,15 15,15 15,25 25,25" stroke="black" fill="none"
          marker-pattern="5 url(#Dot) 5 none">
          <desc>Authored graph</desc>
        </polyline>
        """

    expected = """
    <path d="M5 15H15V25H25" stroke="black" fill="none"/>
    <g fill="#008000">
      <rect x="8" y="13" width="4" height="4"/>
      <rect x="13" y="18" width="4" height="4"/>
      <rect x="18" y="23" width="4" height="4"/>
    </g>
    """

    assert pixels(actual) == pixels(expected)
  end

  test "square interior strokes saturate and a solid single segment keeps its corner coverage" do
    actual = """
    <rect x="4" y="4" width="8" height="8" fill="blue" stroke="red" stroke-width="6" stroke-alignment="inside"/>
    <line x1="15" y1="20" x2="21" y2="20" stroke="black" stroke-width="2" stroke-dashcorner="4"/>
    <line x1="30" y1="20" x2="50" y2="20" stroke="black" stroke-width="2"
      stroke-dasharray="4 5" stroke-dashadjust="stretch dashes"/>
    """

    expected = """
    <rect x="4" y="4" width="8" height="8" fill="red"/>
    <path d="M15 20H21 M30 20H35 M40 20H45" stroke="black" stroke-width="2"/>
    """

    assert pixels(actual) == pixels(expected)
  end

  test "invalid retained SVG print geometry receives a source diagnostic" do
    alias NativeElixirPdfUtilities.HtmlToPdf.SvgRasterizer

    marker = """
    <defs><marker id="Clear" markerUnits="userSpaceOnUse" marker-knockout-right="3"/>
      <marker id="Scaled" marker-knockout-right="3"/>
    </defs>
    """

    for body <- [
          ~s|<style>line{stroke-dashadjust:stretch}</style><line x2="10"/>|,
          ~s|<line x2="10" style="stroke-dashadjust:stretch"/>|,
          ~s|<rect width="10" height="10" stroke-alignment="left"/>|,
          ~s|<rect width="10" height="10" marker-pattern="3 url(#Clear)"/>|,
          ~s|<line x2="10" marker-pattern="3 unknown"/>|,
          ~s|<line x2="10" marker-segment="red"/>|,
          ~s|<line x2="10" stroke-dasharray="1%" stroke-dashadjust="stretch"/>|,
          ~s|<polyline points="1,1 5,5 9,2" stroke-dashcorner="2"/>|,
          ~s|<line x2="10" marker-end="url(#Clear)" stroke-dasharray="2 3"/>|,
          ~s|<line x2="10" marker-end="url(#Scaled)"/>|
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               SvgRasterizer.rasterize(svg(marker <> body), [], nil)

      assert diagnostic.message =~ "SVG"
    end

    assert :error = SvgPrintGeometryValidator.prepare("marker-pattern", "2 invalid")
  end

  test "a knockout at one endpoint leaves the other endpoint untouched" do
    actual = """
    <defs><marker id="Start" markerUnits="userSpaceOnUse" marker-knockout-right="4"/></defs>
    <line x1="4" y1="12" x2="30" y2="12" stroke="red" marker-start="url(#Start)"/>
    """

    assert pixels(actual) == pixels(~s|<line x1="8" y1="12" x2="30" y2="12" stroke="red"/>|)
  end

  defp pixels(body) do
    assert {:ok, png} = SvgPublicWorkflow.png(svg(body))
    assert {:ok, image} = PngDecoder.decode(png, nil, false)
    {image.data, image.alpha_data}
  end

  defp svg(body),
    do: ~s|<svg xmlns="http://www.w3.org/2000/svg" width="80" height="40">| <> body <> "</svg>"
end
