defmodule NativeElixirPdfUtilities.HtmlToPdf.CssTextBreakModesTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.HtmlToPdf.{Font, HtmlParser, Layout, ResourceContext, Style}
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "mixed text intrinsics preserve atomic widths at following text breaks" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      for mode <- [
            "",
            "overflow-wrap:anywhere",
            "overflow-wrap:break-word",
            "word-break:break-word",
            "line-break:anywhere"
          ],
          content <- [
            "<span style='display:inline-block;width:72pt'>Box</span>Z",
            "A <span style='display:inline-block;width:72pt'>Box</span>Z"
          ] do
        html = "<span style='font-family:DejaVu Sans;font-size:12pt;#{mode}'>#{content}</span>"
        assert {:ok, dom} = HtmlParser.parse_detailed(html)
        assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, [], context.image_budget)
        assert {:ok, measured} = Layout.measure_child(node, context)
        assert_in_delta measured.min_content_width, 72.0, 1.0e-9
        assert measured.max_content_width > measured.min_content_width
      end
    end)
  end

  test "pre min-content uses the widest forced line" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      for {mode, content, minimum} <- [{"pre", "ABC\nDE", "ABC"}] do
        assert {:ok, dom} =
                 HtmlParser.parse_detailed(
                   "<span style='font-size:12pt;white-space:#{mode}'>#{content}</span>"
                 )

        assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, [], context.image_budget)
        assert {:ok, measured} = Layout.measure_child(node, context)

        assert_in_delta measured.min_content_width,
                        Font.css_text_width(minimum, node.style.font_face, 12),
                        1.0e-9
      end
    end)
  end

  test "accepted emergency modes keep their distinct min-content contract and nowrap override" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      for {mode, emergency_min?} <- [
            {"", false},
            {"overflow-wrap:normal", false},
            {"overflow-wrap:break-word", false},
            {"overflow-wrap:anywhere", true},
            {"word-break:break-word", true},
            {"line-break:anywhere", true}
          ],
          space <- ["normal", "nowrap", "pre"] do
        assert {:ok, dom} =
                 HtmlParser.parse_detailed(
                   "<span style='font-size:12pt;#{mode};white-space:#{space}'>ABCDE</span>"
                 )

        assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, [], context.image_budget)
        assert {:ok, measured} = Layout.measure_child(node, context)
        full = Font.css_text_width("ABCDE", node.style.font_face, 12)

        minimum =
          if emergency_min? and space == "normal",
            do:
              Enum.max(
                Enum.map(
                  String.graphemes("ABCDE"),
                  &Font.css_text_width(&1, node.style.font_face, 12)
                )
              ),
            else: full

        assert_in_delta measured.min_content_width, minimum, 1.0e-9
        assert_in_delta measured.max_content_width, full, 1.0e-9
      end
    end)
  end

  test "break-spaces counts the selected-font advances of preserved spaces" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      for content <- ["A B", "A<span> </span>B"] do
        assert {:ok, dom} =
                 HtmlParser.parse_detailed(
                   "<span style='font-size:12pt;white-space:break-spaces'>#{content}</span>"
                 )

        assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, [], context.image_budget)
        assert {:ok, measured} = Layout.measure_child(node, context)

        assert_in_delta measured.min_content_width,
                        Font.css_text_width("A ", node.style.font_face, 12),
                        1.0e-9
      end
    end)
  end

  test "atomic intrinsic boundaries retain outer edges, image sizing, and nowrap neighbors" do
    image =
      "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAICAIAAAB/FOjAAAAAGElEQVR4nGM4oSGHFYkELMCKGEY10EIDAJ7zhIFJAvtOAAAAAElFTkSuQmCC"

    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      for {content, expected} <- [
            {"<span style='display:inline-block;width:72pt;padding:2pt;border:1pt solid;margin:3pt'>Box</span>Z",
             83.5},
            {"<span style='display:inline-block;width:72pt;box-sizing:border-box;padding:2pt;border:1pt solid;margin:3pt'>Box</span>Z",
             78.0},
            {"<img src='#{image}' style='width:72pt;height:12pt'>Z", 72.0},
            {"<span style='display:inline-block;width:72pt'>A</span><span style='display:inline-block;width:84pt'>B</span>Z",
             84.0}
          ] do
        assert {:ok, dom} =
                 HtmlParser.parse_detailed("<span style='font-size:12pt'>#{content}</span>")

        assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, [], context.image_budget)
        assert {:ok, measured} = Layout.measure_child(node, context)
        assert_in_delta measured.min_content_width, expected, 1.0e-9
      end

      assert {:ok, dom} =
               HtmlParser.parse_detailed(
                 "<span style='font-size:12pt;white-space:nowrap'><span style='display:inline-block;width:72pt'>Box</span>Z</span>"
               )

      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, [], context.image_budget)
      assert {:ok, measured} = Layout.measure_child(node, context)

      assert_in_delta measured.min_content_width,
                      72 + Font.css_text_width("Z", node.style.font_face, 12),
                      1.0e-9

      assert_in_delta measured.max_content_width, measured.min_content_width, 1.0e-9
    end)
  end

  test "numeric border-box dimensions retain their requested outer size before edge subtraction" do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      for constraint <- ["", "max-width:60pt", "min-width:84pt"] do
        assert {:ok, dom} =
                 HtmlParser.parse_detailed(
                   "<span style='display:inline-block;width:72pt;height:24pt;box-sizing:border-box;padding:2pt;border:1pt solid;margin:3pt;#{constraint}'>Box</span>"
                 )

        assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, [], context.image_budget)
        assert {:ok, measured} = Layout.measure_child(node, context)

        outer_width =
          case constraint do
            "max-width:60pt" -> 66.0
            "min-width:84pt" -> 90.0
            _ -> 78.0
          end

        assert_in_delta measured.width, outer_width, 1.0e-9
        assert_in_delta measured.ascent + measured.descent, 30.0, 1.0e-9
      end
    end)
  end
end
