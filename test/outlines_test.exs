defmodule NativeElixirPdfUtilities.OutlinesTest do
  use ExUnit.Case, async: false

  alias NativeElixirPdfUtilities.HtmlToPdf
  alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
  alias NativeElixirPdfUtilities.HtmlToPdf.Layout
  alias NativeElixirPdfUtilities.HtmlToPdf.PdfWriter
  alias NativeElixirPdfUtilities.HtmlToPdf.Style
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Merge
  alias NativeElixirPdfUtilities.Outlines
  alias NativeElixirPdfUtilities.Pdf.OutlineBuilder
  alias NativeElixirPdfUtilities.Pdf.OutlineDetector
  alias NativeElixirPdfUtilities.Pdf.OutlineWriter
  alias NativeElixirPdfUtilities.Pdf.AssemblyWriter
  alias NativeElixirPdfUtilities.Pdf.Reader
  alias NativeElixirPdfUtilities.Split
  alias NativeElixirPdfUtilities.Transform
  alias NativeElixirPdfUtilities.Validators.MergeValidator
  alias NativeElixirPdfUtilities.Validators.OutlineValidator
  alias NativeElixirPdfUtilities.Validators.WriterValidator

  setup do
    original_limits = Limits.effective()
    on_exit(fn -> Limits.install(original_limits) end)
    :ok
  end

  test "puts, gets, replaces, and removes nested outlines" do
    pdf = two_page_pdf()

    items = [
      %{
        title: "Résumé",
        page: 1,
        view: {:xyz, nil, 90, nil},
        open: false,
        children: [
          {"Fit", 1},
          %{title: "Horizontal", page: 2, view: {:fit_h, nil}},
          %{title: "Vertical", page: 2, view: {:fit_v, 12}},
          %{title: "Bounded H", page: 1, view: {:fit_bh, 80}},
          %{title: "Bounded V", page: 1, view: {:fit_bv, nil}},
          %{title: "Rectangle", page: 2, view: {:fit_r, 0, 0, 50, 80}},
          %{title: "Bounding box", page: 2, view: :fit_b}
        ]
      },
      %{title: "Group only", page: nil, children: []}
    ]

    assert {:ok, updated} = Outlines.put(pdf, items)
    assert {:ok, [root, group]} = Outlines.get(updated)
    assert root.title == "Résumé"
    assert root.open == false

    assert Enum.map(root.children, & &1.view) == [
             :fit,
             {:fit_h, nil},
             {:fit_v, 12},
             {:fit_bh, 80},
             {:fit_bv, nil},
             {:fit_r, 0, 0, 50, 80},
             :fit_b
           ]

    assert group.page == nil
    assert updated =~ "/Outlines"

    assert {:ok, replaced} = Outlines.put(updated, [{"Only", 2}])
    assert {:ok, [%{title: "Only", page: 2}]} = Outlines.get(replaced)

    assert {:ok, removed} = Outlines.put(replaced, [])
    assert {:ok, []} = Outlines.get(removed)
  end

  test "creates semantic outlines from HTML headings" do
    html = """
    <h1>Quarterly Report</h1>
    <h2>Revenue</h2>
    <h3>Malaysia</h3>
    <h2 style="display: none">Hidden</h2>
    <h2>Expenses</h2>
    <p>Ordinary body copy.</p>
    """

    assert {:ok, pdf} = HtmlToPdf.render(html, outlines: :headings)
    assert {:ok, [report]} = Outlines.get(pdf)
    assert report.title == "Quarterly Report"
    assert Enum.map(report.children, & &1.title) == ["Revenue", "Expenses"]
    assert hd(report.children).children |> hd() |> Map.fetch!(:title) == "Malaysia"
    assert match?({:fit_h, _top}, report.view)

    assert {:ok, explicit} = HtmlToPdf.render("<p>Body</p>", outlines: [{"Start", 1}])
    assert {:ok, [%{title: "Start"}]} = Outlines.get(explicit)

    assert {:ok, none} = HtmlToPdf.render("<h1>Ignored</h1>", outlines: false)
    assert {:ok, []} = Outlines.get(none)
  end

  test "keeps semantic outlines for headings with forced page breaks" do
    for break_style <- ["break-before: page", "page-break-before: always"] do
      html = """
      <h1>First</h1>
      <p>Body</p>
      <h1 style="#{break_style}">Second</h1>
      <p>Tail</p>
      """

      assert {:ok, pdf} = HtmlToPdf.render(html, outlines: :headings)
      assert {:ok, outlines} = Outlines.get(pdf)
      assert Enum.map(outlines, &{&1.title, &1.page}) == [{"First", 1}, {"Second", 2}]
    end
  end

  test "detects visual headings and automatic writes the proposal" do
    assert {:ok, pdf} =
             HtmlToPdf.render("""
             <h1>Detected Report</h1>
             <p>Enough ordinary body text to establish the common body size.</p>
             <h2>Detected Section</h2>
             <p>More ordinary body text follows this section heading.</p>
             """)

    assert {:ok, [detected]} = Outlines.detect(pdf)
    assert detected.title == "Detected Report"
    assert hd(detected.children).title == "Detected Section"

    assert {:ok, automatic} = Outlines.automatic(pdf)
    assert Outlines.get(automatic) == {:ok, [detected]}

    assert {:ok, existing} = Outlines.put(pdf, [{"Existing", 1}])
    assert {:ok, [%{title: "Existing"}]} = Outlines.detect(existing)

    assert {:ok, plain} = HtmlToPdf.render("<p>Only ordinary text exists here.</p>")

    assert {:error,
            {:no_outline_source,
             %{stage: :outline_detection, operation: :detect_outlines, module: Outlines}}} =
             Outlines.detect(plain)
  end

  test "preserves and remaps outlines through merge, transform, and split" do
    assert {:ok, first} = HtmlToPdf.render("<h1>First</h1>", outlines: :headings)
    assert {:ok, second} = HtmlToPdf.render("<h1>Second</h1>", outlines: :headings)
    assert {:ok, merged} = Merge.merge([first, second])

    assert {:ok, merged_items} = Outlines.get(merged)
    assert Enum.map(merged_items, &{&1.title, &1.page}) == [{"First", 1}, {"Second", 2}]

    assert {:ok, picked} = Transform.pick_pages(merged, [2])
    assert {:ok, [%{title: "Second", page: 1}]} = Outlines.get(picked)

    assert {:ok, [first_page, second_page]} = Split.by_page(merged)
    assert {:ok, [%{title: "First", page: 1}]} = Outlines.get(first_page)
    assert {:ok, [%{title: "Second", page: 1}]} = Outlines.get(second_page)
  end

  test "resolves legacy and name-tree destinations" do
    pdf =
      pdf([
        {1,
         "<< /Type /Catalog /Pages 2 0 R /Outlines 5 0 R /Dests << /legacy [3 0 R /Fit] >> /Names 10 0 R >>"},
        {2, "<< /Type /Pages /Kids [3 0 R 4 0 R] /Count 2 >>"},
        {3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 100] >>"},
        {4, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 100] >>"},
        {5, "<< /Type /Outlines /First 6 0 R /Last 7 0 R /Count 2 >>"},
        {6, "<< /Title (Legacy) /Parent 5 0 R /Next 7 0 R /Dest /legacy >>"},
        {7, "<< /Title (Modern) /Parent 5 0 R /Prev 6 0 R /Dest (modern) >>"},
        {8, "null"},
        {9, "null"},
        {10, "<< /Dests 11 0 R >>"},
        {11, "<< /Names [(modern) [4 0 R /FitH 90]] >>"}
      ])

    assert {:ok, [legacy, modern]} = Outlines.get(pdf)
    assert {legacy.page, legacy.view} == {1, :fit}
    assert {modern.page, modern.view} == {2, {:fit_h, 90}}
  end

  test "validates exact input, source structures, and limits" do
    pdf = two_page_pdf()

    for invalid <- [
          :invalid,
          [:invalid],
          [%{title: "", page: 1}],
          [%{title: "Valid", page: 3}],
          [%{title: "Valid", page: 1, view: :unknown}],
          [%{title: "Valid", page: 1, open: :yes}],
          [%{title: "Valid", page: 1, children: :invalid}],
          [%{title: "Valid", page: 1, unknown: true}]
        ] do
      assert {:error, {:invalid_outlines, %{stage: :outlines}}} = Outlines.put(pdf, invalid)
    end

    assert {:error, {:invalid_pdf_input, %{operation: :get_outlines}}} = Outlines.get("bad")
    assert {:error, {:invalid_pdf_input, %{operation: :put_outlines}}} = Outlines.put("bad", [])

    assert {:error, {:invalid_pdf_input, %{operation: :automatic_outlines}}} =
             Outlines.automatic("bad")

    original = Limits.effective()
    Limits.install(%{original | max_pdf_outline_items: 1})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             Outlines.put(pdf, [{"One", 1}, {"Two", 2}])

    Limits.install(%{original | max_pdf_outline_title_bytes: 3})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             Outlines.put(pdf, [{"Long", 1}])

    Limits.install(%{
      original
      | max_pdf_outline_title_bytes: 10,
        max_pdf_outline_total_title_bytes: 3
    })

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             Outlines.put(pdf, [{"ab", 1, [{"cd", 1}]}])

    Limits.install(%{original | max_pdf_outline_depth: 1})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             Outlines.put(pdf, [{"Parent", 1, [{"Child", 1}]}])

    Limits.install(original)

    assert {:ok, [%{children: [%{title: "Child"}]}]} =
             OutlineValidator.normalize([{"Parent", 1, [{"Child", 1}]}], 1)
  end

  test "rejects malformed outline roots, links, titles, and destinations" do
    invalid_sources = [
      %{catalog: "<< /Type /Catalog /Pages 2 0 R /Outlines 42 >>"},
      %{root: "<< /Type /Bad /First 6 0 R /Last 6 0 R >>"},
      %{root: "<< /Type /Outlines /First 6 0 R >>"},
      %{root: "<< /Type /Outlines /First 42 /Last 42 >>"},
      %{root: "<< /Type /Outlines /First 6 0 R /Last 7 0 R >>"},
      %{item: "<< /Title (One) /Parent 5 0 R /Next 6 0 R /Dest [3 0 R /Fit] >>"},
      %{item: "<< /Title (One) /Parent 2 0 R /Dest [3 0 R /Fit] >>"},
      %{item: "<< /Title (One) /Parent 5 0 R /Prev 4 0 R /Dest [3 0 R /Fit] >>"},
      %{item: "<< /Title (One) /Parent 5 0 R /Count (bad) /Dest [3 0 R /Fit] >>"},
      %{item: "<< /Title (One) /Parent 5 0 R /First 7 0 R /Dest [3 0 R /Fit] >>"},
      %{item: "<< /Title (One) /Parent 5 0 R /Next 42 /Dest [3 0 R /Fit] >>"},
      %{item: "<< /Title 42 /Parent 5 0 R /Dest [3 0 R /Fit] >>"},
      %{item: "<< /Title (One) /Parent 5 0 R /Dest [3 0 R /Bad] >>"},
      %{item: "<< /Title (One) /Parent 5 0 R /Dest [3 0 R /FitH /bad] >>"},
      %{item: "<< /Title (One) /Parent 5 0 R /Dest [99 0 R /Fit] >>"},
      %{item: "<< /Title (One) /Parent 5 0 R /Dest 42 >>"},
      %{item: "<< /Title (One) /Parent 5 0 R /A 42 >>"},
      %{catalog: "<< /Type /Catalog /Pages 2 0 R /Outlines 5 0 R /Dests 42 >>"}
    ]

    for options <- invalid_sources do
      assert {:error, {:invalid_pdf_input, %{stage: :outlines}}} =
               options |> outlined_pdf() |> Outlines.get()
    end

    assert {:error, {:invalid_pdf_input, %{stage: :outlines}}} = OutlineValidator.extract(%{})
  end

  test "reads supported actions and destination name-tree shapes" do
    action_pdf =
      outlined_pdf(%{
        root: "<< /Type /Outlines /First 6 0 R /Last 8 0 R /Count 3 >>",
        item: "<< /Title (GoTo) /Parent 5 0 R /Next 7 0 R /A << /S /GoTo /D [3 0 R /Fit] >> >>",
        extras: [
          {7,
           "<< /Title (External) /Parent 5 0 R /Prev 6 0 R /Next 8 0 R /A << /S /URI /URI (https://example.com) >> >>"},
          {8, "<< /Title (Missing) /Parent 5 0 R /Prev 7 0 R /Dest /unknown >>"}
        ]
      })

    assert {:ok, [goto, external, missing]} = Outlines.get(action_pdf)
    assert {goto.page, goto.view} == {1, :fit}
    assert external.page == nil
    assert missing.page == nil

    direct_tree =
      outlined_pdf(%{
        catalog:
          "<< /Type /Catalog /Pages 2 0 R /Outlines 5 0 R /Names << /Dests << /Names [(named) << /D [3 0 R /FitB] >>] >> >> >>",
        item: "<< /Title (Named) /Parent 5 0 R /Dest (named) >>"
      })

    assert {:ok, [%{page: 1, view: :fit_b}]} = Outlines.get(direct_tree)

    indirect_tree =
      outlined_pdf(%{
        catalog: "<< /Type /Catalog /Pages 2 0 R /Outlines 5 0 R /Names << /Dests 10 0 R >> >>",
        item: "<< /Title (Named) /Parent 5 0 R /Dest (named) >>",
        extras: [
          {10, "<< /Kids [11 0 R] >>"},
          {11, "<< /Names [(named) [3 0 R /Fit]] >>"}
        ]
      })

    assert {:ok, [%{page: 1, view: :fit}]} = Outlines.get(indirect_tree)

    empty_tree =
      outlined_pdf(%{
        catalog: "<< /Type /Catalog /Pages 2 0 R /Outlines 5 0 R /Names << >> >>"
      })

    assert {:ok, [%{title: "One"}]} = Outlines.get(empty_tree)
  end

  test "rejects malformed destination name trees" do
    invalid_trees = [
      %{names: "42"},
      %{names: "<< /Dests 10 0 R >>", extra: {10, "<< /Kids [10 0 R] >>"}},
      %{names: "<< /Dests 10 0 R >>", extra: {10, "<< /Names [(odd)] >>"}},
      %{names: "<< /Dests 10 0 R >>", extra: {10, "<< /Names [42 [3 0 R /Fit]] >>"}},
      %{names: "<< /Dests 10 0 R >>", extra: {10, "<< /Kids 42 >>"}},
      %{names: "<< /Dests 10 0 R >>", extra: {10, "<< /Kids [42] >>"}},
      %{
        names: "<< /Dests 10 0 R >>",
        extra: {10, "<< /Names [(named) 99 0 R] >>"},
        destination: "(named)"
      }
    ]

    for %{names: names} = tree <- invalid_trees do
      destination = Map.get(tree, :destination, "[3 0 R /Fit]")

      pdf =
        outlined_pdf(%{
          catalog: "<< /Type /Catalog /Pages 2 0 R /Outlines 5 0 R /Names #{names} >>",
          item: "<< /Title (One) /Parent 5 0 R /Dest #{destination} >>",
          extras: List.wrap(Map.get(tree, :extra))
        })

      assert {:error, {reason, %{stage: stage}}} = Outlines.get(pdf)
      assert reason in [:invalid_pdf_input, :resource_limit_exceeded]
      assert stage in [:outlines, :limits, :resolution]
    end

    original = Limits.effective()

    node_limited =
      outlined_pdf(%{
        catalog: "<< /Type /Catalog /Pages 2 0 R /Outlines 5 0 R /Names << /Dests 10 0 R >> >>",
        extras: [{10, "<< /Kids [11 0 R] >>"}, {11, "<< /Names [] >>"}]
      })

    Limits.install(%{original | max_pdf_name_tree_nodes: 1})
    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} = Outlines.get(node_limited)
  end

  test "enforces source outline and incremental object limits" do
    original = Limits.effective()

    nested =
      outlined_pdf(%{
        item:
          "<< /Title (Parent) /Parent 5 0 R /First 7 0 R /Last 7 0 R /Count 1 /Dest [3 0 R /Fit] >>",
        extras: [{7, "<< /Title (Child) /Parent 6 0 R /Dest [3 0 R /Fit] >>"}]
      })

    Limits.install(%{original | max_pdf_outline_depth: 1})
    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} = Outlines.get(nested)

    siblings =
      outlined_pdf(%{
        root: "<< /Type /Outlines /First 6 0 R /Last 7 0 R /Count 2 >>",
        item: "<< /Title (One) /Parent 5 0 R /Next 7 0 R /Dest [3 0 R /Fit] >>",
        extras: [{7, "<< /Title (Two) /Parent 5 0 R /Prev 6 0 R /Dest [3 0 R /Fit] >>"}]
      })

    Limits.install(%{original | max_pdf_outline_items: 1})
    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} = Outlines.get(siblings)

    Limits.install(original)
    assert {:ok, context} = Reader.read_validated(two_page_pdf())
    size = context.document.trailer["Size"]
    Limits.install(%{original | max_pdf_objects: size - 1})

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             OutlineValidator.validate_incremental_capacity(context, [
               %{title: "ignored", page: 1, view: :fit, open: true, children: []}
             ])

    assert {:error, {:invalid_pdf_input, %{stage: :outlines}}} =
             OutlineValidator.validate_incremental_capacity(%{}, [])
  end

  test "covers detector, HTML option, writer, and assembly edge cases" do
    page = %{
      size: {200.0, 100.0},
      boxes: [
        text_box("A long body line establishes the ordinary body size", 10, 10, 10),
        text_box("Big", 10, 70, 20),
        text_box("Title", 70, 70, 20)
      ]
    }

    assert {:ok, visual_pdf} = PdfWriter.render([page])
    assert {:ok, [%{title: "Big Title"}]} = OutlineDetector.detect(visual_pdf)

    assert {:ok, empty_pdf} = PdfWriter.render([%{size: {100.0, 100.0}, boxes: []}])
    assert {:error, {:no_outline_source, _diagnostic}} = OutlineDetector.detect(empty_pdf)

    headings =
      OutlineDetector.from_paginated_headings([
        %{
          size: {100.0, 100.0},
          boxes: [
            %{y: 10.0, height: 5.0, outline_anchor: %{title: "Height", level: 1}},
            %{y: 20.0, line_height: 6.0, outline_anchor: %{title: "Line", level: 1}},
            %{y: 30.0, outline_anchor: %{title: "Y", level: 1}},
            %{outline_anchor: %{title: "Default", level: 1}}
          ]
        }
      ])

    assert Enum.map(headings, & &1.view) == [
             {:fit_h, 15.0},
             {:fit_h, 26.0},
             {:fit_h, 30.0},
             {:fit_h, 0.0}
           ]

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlToPdf.render("<p>Body</p>", outlines: :invalid)

    assert {:error, {:invalid_outlines, _diagnostic}} =
             WriterValidator.prepare([%{size: {10, 10}, boxes: []}], outlines: :invalid)

    assert {:ok, %{outlines: []}} =
             WriterValidator.prepare([%{size: {10, 10}, boxes: []}], outlines: false)

    assert {:ok, empty_assembly} = AssemblyWriter.write([])
    assert String.starts_with?(empty_assembly, "%PDF-")

    assert {:ok, dom} = HtmlParser.parse("<h1>Title</h1>")
    assert {:ok, styled} = Style.compute(dom)
    [heading] = styled.children

    hidden_heading = put_in(heading, [:style, :display], :none)
    assert {:ok, %{boxes: []}} = Layout.layout(%{styled | children: [hidden_heading]})

    ignored_child = %{type: :element, style: %{display: :none}}
    heading = Map.update!(heading, :children, &(&1 ++ [ignored_child]))
    assert {:ok, %{boxes: [_first | _rest]}} = Layout.layout(%{styled | children: [heading]})
  end

  test "outline writer reports invalid prepared object and trailer values" do
    base = %{
      catalog: %{},
      catalog_ref: {1, 0},
      pages: [],
      document: %{
        binary: "%PDF-1.7",
        trailer: %{"Size" => 2, "Root" => {:ref, {1, 0}}, "Info" => {:ref, {2, 0}}},
        xref_offset: 0
      }
    }

    assert {:ok, increment} = OutlineWriter.write(base, [])
    assert increment =~ "/Info 2 0 R"

    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
             OutlineWriter.write(%{base | catalog: %{"Invalid" => :atom}}, [])

    invalid_trailer = put_in(base, [:document, :trailer, "Root"], :atom)

    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
             OutlineWriter.write(invalid_trailer, [])
  end

  test "merge remapping reserves capacity for generated outline objects" do
    original = Limits.effective()
    Limits.install(%{original | max_pdf_objects: 4, max_merged_objects: 4})

    input = %{
      objects: [],
      pages: [],
      inherited: %{},
      outlines: [%{title: "One", page: nil, view: :fit, open: true, children: []}]
    }

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             MergeValidator.prepare_remapping([input], 3)
  end

  test "internal outline components reject malformed prepared data" do
    assert {:error, {:invalid_pdf_input, %{stage: :incremental_write}}} =
             OutlineWriter.write(%{}, [])

    assert OutlineBuilder.build([], fn _page -> {1, 0} end, 4) == %{
             root_ref: nil,
             objects: [],
             next_id: 4
           }

    assert OutlineDetector.from_paginated_headings([
             %{size: {100.0, 100.0}, boxes: [%{y: 20.0}]}
           ]) == []

    assert {:error, {:invalid_outlines, _diagnostic}} = OutlineValidator.normalize([], :bad)
  end

  defp two_page_pdf do
    {:ok, pdf} =
      HtmlToPdf.render("""
      <h1>First page</h1>
      <div style="break-before: page"><h1>Second page</h1></div>
      """)

    pdf
  end

  defp outlined_pdf(options) do
    defaults = %{
      catalog: "<< /Type /Catalog /Pages 2 0 R /Outlines 5 0 R >>",
      root: "<< /Type /Outlines /First 6 0 R /Last 6 0 R /Count 1 >>",
      item: "<< /Title (One) /Parent 5 0 R /Dest [3 0 R /Fit] >>",
      extras: []
    }

    options = Map.merge(defaults, options)

    ([
       {1, options.catalog},
       {2, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"},
       {3, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 100] >>"},
       {4, "null"},
       {5, options.root},
       {6, options.item}
     ] ++ options.extras)
    |> Enum.sort_by(&elem(&1, 0))
    |> pdf()
  end

  defp text_box(text, x, y, font_size) do
    %{
      type: :text,
      text: text,
      x: x,
      y: y,
      font: "Helvetica",
      font_face: %{type: :built_in, family: "Helvetica", pdf_name: "Helvetica"},
      font_size: font_size,
      color: {0, 0, 0}
    }
  end

  defp pdf(objects) do
    header = "%PDF-1.7\n"

    {body, offsets} =
      Enum.reduce(objects, {header, %{}}, fn {id, source}, {body, offsets} ->
        rendered = "#{id} 0 obj\n#{source}\nendobj\n"
        {body <> rendered, Map.put(offsets, id, byte_size(body))}
      end)

    maximum = objects |> Enum.map(&elem(&1, 0)) |> Enum.max()
    xref_offset = byte_size(body)

    entries =
      Enum.map(0..maximum, fn object ->
        case Map.get(offsets, object) do
          nil -> "0000000000 " <> if(object == 0, do: "65535 f \n", else: "00000 f \n")
          offset -> String.pad_leading(Integer.to_string(offset), 10, "0") <> " 00000 n \n"
        end
      end)

    body <>
      "xref\n0 #{maximum + 1}\n" <>
      Enum.join(entries) <>
      "trailer\n<< /Size #{maximum + 1} /Root 1 0 R >>\n" <>
      "startxref\n#{xref_offset}\n%%EOF\n"
  end
end
