defmodule NativeElixirPdfUtilities.HtmlToPdf.FramesetBoundaryTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.{HtmlToPdf, Limits}
  alias NativeElixirPdfUtilities.HtmlToPdf.{Frameset, ResourceContext}
  alias NativeElixirPdfUtilities.Validators.{FramesetValidator, HtmlValidator}

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    %{limits: limits}
  end

  test "prepared track and cell cardinality is rejected before malformed track traversal", %{
    limits: limits
  } do
    Limits.install(Map.put(limits, :max_frameset_tracks, 2))
    plan = %{border: false, rows: [:malformed, :malformed, :malformed], columns: [relative: 1.0]}

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FramesetValidator.prepare_layout(plan, 150, 90)

    assert diagnostic.stage == :limits and diagnostic.source == "frameset"
    assert diagnostic.message =~ "max_frameset_tracks"
    Limits.install(Map.put(limits, :max_frameset_cells, 3))

    plan = %{
      border: false,
      rows: [:malformed, :malformed],
      columns: [relative: 1.0, relative: 1.0]
    }

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FramesetValidator.prepare_layout(plan, 150, 90)

    assert diagnostic.message =~ "max_frameset_cells"

    Limits.install(limits |> Map.put(:max_frameset_tracks, 2) |> Map.put(:max_frameset_cells, 4))

    valid = %{
      border: false,
      rows: [relative: 1.0, relative: 1.0],
      columns: [relative: 1.0, relative: 1.0]
    }

    assert {:ok, %{columns: [75.0, 75.0], rows: [45.0, 45.0]}} =
             FramesetValidator.prepare_layout(valid, 150, 90)

    assert {:error, {:invalid_document, malformed}} =
             FramesetValidator.prepare_layout(plan, 150, 90)

    assert malformed.message =~ "canonical nonnegative tracks"
  end

  test "malformed public prepared plans retain canonical diagnostics within allowed cardinality" do
    for plan <- [
          nil,
          %{border: false, rows: [], columns: [relative: 1.0]},
          %{border: true, rows: [relative: 1.0], columns: [relative: 1.0]},
          %{border: false, rows: [:malformed], columns: [relative: 1.0]},
          %{border: false, rows: [absolute: -1.0], columns: [relative: 1.0]},
          %{border: false, rows: [relative: 0.0], columns: [relative: 1.0]}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               FramesetValidator.prepare_layout(plan, 150, 90)

      assert diagnostic.stage == :layout and diagnostic.source == "frameset"
      assert diagnostic.module == FramesetValidator and diagnostic.message != ""
    end

    assert {:ok, plan} = FramesetValidator.prepare(%{"border" => "0"})

    assert {:error, {:invalid_layout, diagnostic}} =
             FramesetValidator.prepare_layout(plan, -1, 90)

    assert diagnostic.operation == :layout_viewport
  end

  test "attribute maps, numeric dimensions and inherited border values use shared validation", %{
    limits: limits
  } do
    assert {:error, {:invalid_document, diagnostic}} = FramesetValidator.prepare(nil)
    assert diagnostic.message =~ "attribute map"
    assert {:error, {:invalid_document, diagnostic}} = FramesetValidator.prepare_frame([], nil)
    assert diagnostic.message =~ "attribute map"

    for attributes <- [%{"frameborder" => "unknown"}, %{"border" => "malformed"}] do
      assert {:ok, recovered} =
               FramesetValidator.prepare(attributes, %{border_enabled: true, border_width: 0.0})

      assert recovered.border_enabled and recovered.border_width == 0.0
    end

    Limits.install(Map.put(limits, :max_css_numeric_magnitude, 1))
    assert {:error, {:invalid_document, diagnostic}} = FramesetValidator.tracks("2")
    assert diagnostic.message =~ "dimension exceeds"

    assert {:error, {:invalid_document, diagnostic}} =
             FramesetValidator.prepare(%{}, %{border_enabled: true, border_width: 2})

    assert diagnostic.message =~ "inherited frameset border"

    assert {:error, {:invalid_document, diagnostic}} =
             FramesetValidator.prepare(%{"border" => "3"})

    assert diagnostic.message =~ "border exceeds"

    assert {:error, {:invalid_document, diagnostic}} =
             FramesetValidator.prepare_frame(%{"marginwidth" => "2"}, %{
               border_enabled: true,
               border_width: 0.0
             })

    assert diagnostic.message =~ "margin exceeds"
  end

  test "public frameset and frame numeric failures preserve their resource diagnostic", %{
    limits: limits
  } do
    Limits.install(Map.put(limits, :max_html_numeric_token_bytes, 1))

    for result <- [
          FramesetValidator.prepare(%{"border" => "00"}),
          FramesetValidator.prepare_frame(%{"marginwidth" => "10"}, %{
            border_enabled: true,
            border_width: 0.0
          })
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = result
      assert diagnostic.stage == :html and diagnostic.reason == :resource_limit_exceeded

      assert diagnostic.message =~ "max_html_numeric_token_bytes" and
               diagnostic.source in ["00", "10"]
    end

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlToPdf.render("<frameset border='00'><frame></frameset>")

    assert diagnostic.operation == :render and diagnostic.source == "00"
  end

  test "legacy malformed margins recover absent hints and canonical body margins reject incomplete records" do
    assert {:ok, frame} =
             FramesetValidator.prepare_frame(%{"marginwidth" => "bad", "marginheight" => "-1"}, %{
               border_enabled: true,
               border_width: 0.0
             })

    assert frame.margins == %{horizontal: nil, vertical: nil}
    assert {:ok, margins} = FramesetValidator.prepare_body_margins(frame.margins)
    assert margins == %{top: 6.0, right: 6.0, bottom: 6.0, left: 6.0}

    for margins <- [%{}, %{horizontal: 2.0}, %{horizontal: nil, vertical: nil, unexpected: 0}] do
      assert {:error, {:invalid_document, diagnostic}} =
               FramesetValidator.prepare_body_margins(margins)

      assert diagnostic.message =~ "horizontal and vertical hints" and diagnostic.stage == :layout
    end
  end

  test "inactive templates and processing instructions retain navigation isolation" do
    dom = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "template",
          children: [
            %{type: :element, tag: "form", attributes: %{"target" => "named"}, children: []}
          ]
        },
        %{type: :processing_instruction, target: "retained", data: "source"},
        %{type: :text, text: "Visible"},
        %{type: :element, tag: "a", attributes: %{"target" => "_self"}, children: []}
      ]
    }

    assert :ok = FramesetValidator.validate_document_navigation(dom)

    assert {:error, {:invalid_document, diagnostic}} =
             FramesetValidator.validate_document_navigation(%{
               type: :document,
               children: [%{type: :unsupported}]
             })

    assert diagnostic.message =~ "canonical document nodes"

    assert {:error, {:not_implemented, diagnostic}} =
             FramesetValidator.validate_document_navigation(%{
               type: :document,
               children: [
                 %{type: :element, tag: "form", attributes: %{"target" => :invalid}, children: []}
               ]
             })

    assert diagnostic.message =~ "target navigation" and diagnostic.source == "frameset"
  end

  test "scene nesting and child limits run before malformed descendant validation", %{
    limits: limits
  } do
    assert {:ok, plan} = FramesetValidator.prepare(%{"border" => "0"})

    node = %{
      type: :element,
      tag: "frameset",
      style: %{_frameset: plan, display: :block},
      children: []
    }

    Limits.install(Map.put(limits, :max_html_depth, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FramesetValidator.prepare_scene(node, 150, 90, 2)

    assert diagnostic.message =~ "max_html_depth" and diagnostic.stage == :limits
    Limits.install(Map.put(limits, :max_html_nodes, 1))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             FramesetValidator.prepare_scene(
               %{node | children: [:malformed, :malformed]},
               150,
               90,
               1
             )

    assert diagnostic.message =~ "max_html_nodes"
    Limits.install(limits)
    frame = %{type: :element, tag: "frame", style: %{display: :block, embedded_document: %{}}}

    assert {:error, {:invalid_layout, diagnostic}} =
             FramesetValidator.prepare_scene(%{node | children: [frame]}, 150, 90, 1)

    assert diagnostic.operation == :layout_embedded_viewport
    assert diagnostic.message =~ "issued document"
  end

  test "Frameset layout catches final group budget exhaustion inside the existing scope and recovers",
       %{limits: limits} do
    ResourceContext.run([], HtmlValidator.new_image_budget(), fn context ->
      assert {:ok, plan} = FramesetValidator.prepare(%{"border" => "0"})

      node = %{
        type: :element,
        tag: "frameset",
        style: %{_frameset: plan, display: :block},
        children: []
      }

      Limits.install(Map.put(limits, :max_layout_boxes, 0))
      assert {:error, {:resource_limit_exceeded, diagnostic}} = Frameset.layout(node, 150, 90)
      assert diagnostic.stage == :layout and diagnostic.message =~ "max_layout_boxes"
      assert ResourceContext.current() == context
      Limits.install(limits)
      assert {:ok, group} = Frameset.layout(node, 150, 90)
      assert group.type == :drawing_group and group.boxes == []
      assert group.clip == %{x: 0.0, y: 0.0, width: 150, height: 90}
      assert ResourceContext.current() == context
      :continued_after_recovery
    end)
    |> then(&assert(&1 == :continued_after_recovery))
  end
end
