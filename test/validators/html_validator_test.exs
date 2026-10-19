defmodule NativeElixirPdfUtilities.Validators.HtmlValidatorTest do
  use ExUnit.Case, async: true

  import NativeElixirPdfUtilities.HtmlTreeHelpers

  alias NativeElixirPdfUtilities.HtmlToPdf.Font
  alias NativeElixirPdfUtilities.HtmlToPdf.PageFurniture
  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  test "processing instruction syntax and direct tree transport share one validator" do
    marker = %{type: :processing_instruction, target: "marker", data: "name='A&amp;B'"}

    assert {:ok, ^marker} =
             HtmlValidator.validate_html_processing_instruction("<?marker \tname='A&amp;B'?>")

    assert {:ok, ^marker} = HtmlValidator.validate_html_processing_instruction(marker)
    assert {:ok, %{data: ""}} = HtmlValidator.validate_html_processing_instruction("<?_1-?>")

    assert {:ok, %{data: ""}} =
             HtmlValidator.validate_html_processing_instruction("<?marker \t\n\f>")

    assert HtmlValidator.html_content_nodes([marker, %{type: :text, text: "A"}]) == [
             %{type: :text, text: "A"}
           ]

    for invalid <- [
          nil,
          %{},
          %{marker | target: "xml"},
          %{marker | target: "bad.name"},
          %{marker | target: <<255>>},
          %{marker | data: <<255>>},
          %{marker | data: "x>y"},
          %{marker | data: "x?"},
          %{marker | data: <<0>>},
          Map.put(marker, :children, [
            %{type: :element, tag: "script", attributes: %{}, children: []}
          ]),
          Map.put(marker, :attributes, %{"onclick" => "run()"}),
          Map.put(marker, :node_id, "forged"),
          Map.put(marker, :source, nil),
          Map.put(marker, :source, %{text: "<?end>", line: 1, column: 1}),
          Map.put(marker, :source, %{text: "<?bad.name>", line: 1, column: 1}),
          Map.put(marker, :source, %{text: "<?marker>", line: 0, column: 1}),
          Map.put(marker, :source, %{text: "<?marker>", line: 1, column: 1, arbitrary: true})
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               HtmlValidator.validate_html_processing_instruction(invalid)

      assert diagnostic.stage == :style and diagnostic.source == "processing_instruction"
      assert diagnostic.message =~ "processing instructions require"
    end

    node =
      Map.merge(marker, %{
        node_id: make_ref(),
        source: %{text: "<?marker\r\nname='A&amp;B'?>", line: 3, column: 4}
      })

    assert {:ok, ^node} = HtmlValidator.validate_html_processing_instruction(node)

    for token <- ["<?>", "<?1bad>", "<?xml?>", "<?marker", "<?marker x??>"] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlValidator.validate_html_processing_instruction(token)

      assert diagnostic.stage == :html and diagnostic.source == token
    end
  end

  test "legacy text modes and boundaries belong to the HTML validator" do
    for tag <- ~w(xmp noembed plaintext) do
      assert HtmlValidator.supported_html_tag?(tag)
      assert {:ok, ^tag} = HtmlValidator.validate_html_fragment_context(String.upcase(tag))

      assert HtmlValidator.html_text_mode(tag) ==
               if(tag == "plaintext", do: :plaintext, else: :raw_text)

      assert HtmlValidator.html_text_mode({:foreign, :svg, tag, %{}}) == :data

      assert HtmlValidator.valid_html_element?(tag, %{}, [
               %{type: :text, text: "<script>Literal</script>"}
             ])

      refute HtmlValidator.valid_html_element?(tag, %{}, [
               %{type: :element, tag: "script", attributes: %{}, children: []}
             ])
    end

    assert HtmlValidator.html_text_mode("textarea") == :rcdata
    assert HtmlValidator.html_text_mode("noscript") == :data

    assert HtmlValidator.noembed_render_style(%{
             display: :block,
             color: {1, 0, 0},
             background_image_source: "unused.png"
           }) == %{display: :none, color: {1, 0, 0}}

    assert HtmlValidator.html_text_end_match("</plaintext>", "plaintext") == nil
    assert HtmlValidator.html_text_token_action("plaintext", "</plaintext>") == :text

    for tag <- ~w(style title textarea xmp noembed) do
      assert HtmlValidator.html_text_end_match("é</#{tag}suffix></#{String.upcase(tag)}>", tag) ==
               {byte_size("é</#{tag}suffix>"), byte_size("</#{tag}")}

      assert HtmlValidator.html_text_token_action(tag, "</#{String.upcase(tag)} \t\n\f>") == :end
      assert HtmlValidator.html_text_token_action(tag, "</#{tag}suffix>") == :text
      assert HtmlValidator.html_text_token_action(tag, "</#{tag}/>") == :invalid
      assert HtmlValidator.html_text_token_action(tag, "</#{tag} data-name=x>") == :invalid
    end

    assert HtmlValidator.html_text_end_match("é</script>", "script") == {2, 0}
    assert HtmlValidator.html_text_end_match("<!--<script></script>", "script") == nil
    assert HtmlValidator.html_text_token_action("script", "</script>") == :end
    assert HtmlValidator.html_text_token_action("script", "</script data-name=x>") == :invalid
    assert HtmlValidator.html_eof_end_tag_allowed?("plaintext", "plaintext", [], [])

    refute HtmlValidator.html_eof_end_tag_allowed?(
             {:foreign, :svg, "plaintext", %{}},
             "plaintext",
             [],
             []
           )

    refute HtmlValidator.html_eof_end_tag_allowed?(
             "div",
             "div",
             [%{type: :text, text: "Text"}],
             []
           )
  end

  test "direct legacy trees validate literal contents, context, attributes and executable policy" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    text = %{type: :text, text: "<img src=denied.png><script>Literal</script>"}

    element = fn tag, attributes, children ->
      %{type: :element, tag: tag, attributes: attributes, children: children}
    end

    for tag <- ~w(xmp plaintext noembed) do
      node = element.(tag, %{}, [text])
      dom = %{type: :document, children: [node]}
      assert {:ok, _} = Style.compute_detailed(dom)

      for child <- [element.("span", %{}, [text]), element.("script", %{}, [])] do
        assert {:error, {:invalid_document, diagnostic}} =
                 Style.compute_detailed(%{dom | children: [%{node | children: [child]}]})

        assert diagnostic.stage == :style and diagnostic.operation == :compute
        assert diagnostic.module == HtmlValidator and diagnostic.source == tag
        assert diagnostic.message =~ "literal text-only"
      end

      assert {:error, {:unsupported_html, diagnostic}} =
               Style.compute_detailed(%{
                 dom
                 | children: [%{node | attributes: %{"onclick" => "bad"}}]
               })

      assert diagnostic.stage == :html and diagnostic.operation == :compute
      assert diagnostic.source == tag

      assert {:error, {:invalid_document, diagnostic}} =
               Style.compute_detailed(%{dom | children: [element.("head", %{}, [node])]})

      assert diagnostic.stage == :style and diagnostic.source == tag
    end

    for tag <- ~w(xmp plaintext) do
      refute HtmlValidator.valid_html_child?("button", tag)
      assert HtmlValidator.omitted_html_end_tag?("p", {:start, tag}, ["div"])
      assert HtmlValidator.valid_html_child?("a", tag, ["div"])
      dom = %{type: :document, children: [element.("p", %{}, [element.(tag, %{}, [text])])]}

      assert {:error, {:invalid_document, %{stage: :style, source: ^tag}}} =
               Style.compute_detailed(dom)
    end

    assert HtmlValidator.valid_html_child?("p", "noembed")
    assert HtmlValidator.valid_html_child?("button", "noembed")
  end

  test "prepared gauge contracts reject forged types points classification and position" do
    alias NativeElixirPdfUtilities.Validators.HtmlGaugeValidator
    progress = HtmlGaugeValidator.prepare("progress", %{"value" => ".5"})
    meter = HtmlGaugeValidator.prepare("meter", %{"value" => ".5"})
    assert HtmlGaugeValidator.valid_prepared?(progress)
    assert HtmlGaugeValidator.valid_prepared?(HtmlGaugeValidator.prepare("progress", %{}))
    assert HtmlGaugeValidator.valid_prepared?(meter)

    for forged <- [
          nil,
          %{},
          Map.put(progress, :position, 0.7),
          Map.put(progress, :min, -1.0),
          Map.put(progress, :determinate, nil),
          Map.put(meter, :region, :even_less_good),
          Map.put(meter, :value, "0.5"),
          Map.delete(meter, :high),
          Map.put(meter, :extra, true)
        ] do
      refute HtmlGaugeValidator.valid_prepared?(forged)
    end

    assert HtmlValidator.parse_css_accent_color(" AUTO ", {0, 0, 0}) == {:ok, :auto}
    assert HtmlValidator.parse_css_accent_color("red", {0, 0, 0}) == {:ok, {1, 0, 0}}
    assert HtmlValidator.parse_css_accent_color("bad", {0, 0, 0}) == :error
  end

  test "HTML float UA parsing differs from author conformance without CSS magnitude ceilings" do
    for {source, expected, conforming} <- [
          {"0", 0.0, true},
          {"-0", 0.0, true},
          {".25", 0.25, true},
          {"-.25", -0.25, true},
          {"1e+2", 100.0, true},
          {"-1E-2", -0.01, true},
          {"+2", 2.0, false},
          {" \t\n\r\f2.5tail", 2.5, false},
          {"1.", 1.0, false},
          {"1.e3", 1000.0, false},
          {"1e-", 1.0, false},
          {"2px", 2.0, false},
          {"1e-999", 0.0, true},
          {"0e999", 0.0, true},
          {"1e100", 1.0e100, true}
        ] do
      assert HtmlValidator.parse_html_float(source) == {:ok, expected}
      assert HtmlValidator.valid_html_float?(source) == conforming
    end

    for value <- [nil, 1, "", "+", ".", " .e2", "- 2", "NaN", "Infinity", "1e999", "\u00a02", "٢"] do
      assert HtmlValidator.parse_html_float(value) == :error
      refute HtmlValidator.valid_html_float?(value)
    end
  end

  test "progress recovers maximum and present values while distinguishing missing value" do
    alias NativeElixirPdfUtilities.Validators.HtmlGaugeValidator
    absent = HtmlGaugeValidator.prepare("progress", %{"max" => "2"})
    assert absent.position == -1 and absent.value == 0 and absent.max == 2
    refute absent.determinate

    for {maximum, value, expected_max, expected_value} <- [
          {nil, nil, 1.0, 0.0},
          {"bad", "bad", 1.0, 0.0},
          {"0", "-1", 1.0, 0.0},
          {"-2", "2", 1.0, 1.0},
          {"2", "9", 2.0, 2.0},
          {"+4x", " .5tail", 4.0, 0.5},
          {"1e100", "5e99", 1.0e100, 5.0e99}
        ] do
      attrs = %{"value" => value || ""}
      attrs = if maximum, do: Map.put(attrs, "max", maximum), else: attrs
      state = HtmlGaugeValidator.prepare("progress", attrs)
      assert state.determinate
      assert state.max == expected_max and state.value == expected_value
      assert state.position == expected_value / expected_max
    end
  end

  test "meter resolves six points in order and classifies all optimum regions and boundaries" do
    alias NativeElixirPdfUtilities.Validators.HtmlGaugeValidator
    default = HtmlGaugeValidator.prepare("meter", %{})

    assert Map.take(default, [:min, :max, :low, :high, :optimum, :value, :position, :region]) ==
             %{
               min: 0.0,
               max: 1.0,
               low: 0.0,
               high: 1.0,
               optimum: 0.5,
               value: 0.0,
               position: 0.0,
               region: :optimum
             }

    assert HtmlGaugeValidator.prepare(
             "meter",
             Map.new(~w(min max value low high optimum), &{&1, "bad"})
           ) == default

    collapsed =
      HtmlGaugeValidator.prepare("meter", %{
        "min" => "3",
        "max" => "2",
        "value" => "5",
        "low" => "-3",
        "high" => "9",
        "optimum" => "1"
      })

    assert Enum.all?(Map.take(collapsed, [:min, :max, :value, :low, :high, :optimum]), fn {_key,
                                                                                           number} ->
             number == 3
           end)

    assert collapsed.position == 0

    clamped =
      HtmlGaugeValidator.prepare("meter", %{
        "min" => "-10",
        "max" => "10",
        "value" => "-20",
        "low" => "20",
        "high" => "-20",
        "optimum" => "20"
      })

    assert clamped.value == -10 and clamped.low == 10 and clamped.high == 10 and
             clamped.optimum == 10

    assert HtmlGaugeValidator.prepare("meter", %{"min" => "1e308", "max" => "1.5e308"}).optimum ==
             1.25e308

    assert_in_delta HtmlGaugeValidator.prepare("meter", %{
                      "min" => "-1e308",
                      "max" => "1e308",
                      "value" => "0"
                    }).position,
                    0.5,
                    1.0e-12

    for {optimum, regions} <- [
          {"1", [:optimum, :optimum, :suboptimal, :suboptimal, :even_less_good]},
          {"9", [:even_less_good, :suboptimal, :suboptimal, :optimum, :optimum]},
          {"5", [:suboptimal, :optimum, :optimum, :optimum, :suboptimal]},
          {"3", [:suboptimal, :optimum, :optimum, :optimum, :suboptimal]},
          {"7", [:suboptimal, :optimum, :optimum, :optimum, :suboptimal]}
        ],
        {value, expected} <- Enum.zip(["2", "3", "5", "7", "9"], regions) do
      state =
        HtmlGaugeValidator.prepare("meter", %{
          "max" => "10",
          "low" => "3",
          "high" => "7",
          "optimum" => optimum,
          "value" => value
        })

      assert state.region == expected
    end
  end

  test "gauges are labelable but not form-associated and stale state cannot change canonical values" do
    alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
    alias NativeElixirPdfUtilities.Validators.HtmlFormValidator

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<label for=p>Progress</label><form><progress id=p value=.5></progress><label><meter value=.2></meter>Meter</label></form>"
             )

    [label, form] = HtmlFormValidator.prepare_initial_nodes(dom.children)
    [progress, implicit] = form.children
    meter = hd(implicit.children)
    assert label._initial_label_control == progress._selector_id
    assert implicit._initial_label_control == meter._selector_id
    refute Map.has_key?(progress, :_initial_form_state)
    refute Map.has_key?(meter, :_initial_form_state)
    assert progress.attributes["value"] == ".5"
    assert progress._initial_gauge_state.position == 0.5

    [label, _] =
      HtmlFormValidator.prepare_initial_nodes([put_in(label.attributes["for"], "missing"), form])

    assert label._initial_label_control == nil
  end

  test "gauge appearance and vertical alignment delegate CSS grammar to validators" do
    for value <- ["auto", "AUTO", "progress-bar", "meter"],
        do: assert(HtmlValidator.parse_gauge_appearance(value) == {:ok, :auto})

    assert HtmlValidator.parse_gauge_appearance(" none ") == {:ok, :none}
    assert HtmlValidator.parse_gauge_appearance("button") == :error

    for value <- ~w(baseline top middle bottom),
        do: assert(HtmlValidator.parse_vertical_align(value, 10) == {:ok, String.to_atom(value)})

    assert HtmlValidator.parse_vertical_align("-0.2em", 10) == {:ok, -2.0}
    assert HtmlValidator.parse_vertical_align("invalid", 10) == :error
  end

  test "contenteditable keywords are ASCII enumerations with inherited invalid defaults" do
    for value <- ["", "true", "TRUE", "TrUe", "plaintext-only", "PLAINTEXT-ONLY"] do
      assert HtmlValidator.html_contenteditable_state(%{"contenteditable" => value}) == :editable
    end

    for value <- ["false", "FALSE", "FaLsE"] do
      assert HtmlValidator.html_contenteditable_state(%{"contenteditable" => value}) ==
               :not_editable
    end

    for value <- [
          nil,
          false,
          1,
          "inherit",
          " true ",
          " false",
          "true\n",
          "plaintext-only ",
          "truſ"
        ] do
      assert HtmlValidator.html_contenteditable_state(%{"contenteditable" => value}) == :inherit
    end

    assert HtmlValidator.html_contenteditable_state(%{}) == :inherit
  end

  test "editing regions distinguish hosts descendants controls and foreign namespaces" do
    host = %{tag: "div", attributes: %{"contenteditable" => ""}}
    locked = %{tag: "div", attributes: %{"contenteditable" => "false"}}
    paragraph = %{tag: "p", attributes: %{}}
    assert HtmlValidator.editing_state_matches?(:read_write, host, [])
    assert HtmlValidator.editing_state_matches?(:read_write, paragraph, [host])
    refute HtmlValidator.editing_state_matches?(:read_write, paragraph, [])
    refute HtmlValidator.editing_state_matches?(:read_write, paragraph, [locked, host])
    assert HtmlValidator.editing_state_matches?(:read_write, host, [locked])

    for value <- ["inherit", " true ", " false ", "truſ"] do
      node = put_in(paragraph.attributes, %{"contenteditable" => value})
      assert HtmlValidator.editing_state_matches?(:read_write, node, [host])
      assert HtmlValidator.editing_state_matches?(:read_only, node, [])
    end

    qualified =
      Map.put(host, :attribute_namespaces, %{"contenteditable" => %{namespace: "urn:test"}})

    assert HtmlValidator.editing_state_matches?(:read_only, qualified, [])
    assert HtmlValidator.editing_state_matches?(:read_write, qualified, [host])

    for {namespace, tag} <- [{:svg, "svg"}, {:mathml, "math"}] do
      foreign = %{tag: tag, namespace: namespace, attributes: %{"contenteditable" => "true"}}
      assert HtmlValidator.editing_state_matches?(:read_only, foreign, [])
      assert HtmlValidator.editing_state_matches?(:read_write, foreign, [host])
      refute HtmlValidator.editing_state_matches?(:read_write, foreign, [locked, host])
    end

    for {namespace, tag} <- [{:svg, "g"}, {:svg, "input"}, {:mathml, "mi"}, {:mathml, "textarea"}] do
      foreign = %{tag: tag, namespace: namespace, attributes: %{"contenteditable" => "true"}}
      assert HtmlValidator.editing_state_matches?(:read_only, foreign, [host])
      refute HtmlValidator.editing_state_matches?(:read_write, paragraph, [foreign, host])
    end

    for tag <- ["input", "textarea"],
        attrs <- [%{"readonly" => "false"}, %{"disabled" => "false"}] do
      node = %{tag: tag, attributes: Map.put(attrs, "contenteditable", "true")}
      assert HtmlValidator.editing_state_matches?(:read_only, node, [host])
    end

    for type <- ~w(text search tel url email password unknown TEXT),
        attrs <- [%{}, %{"contenteditable" => "false"}] do
      node = %{tag: "input", attributes: Map.put(attrs, "type", type)}
      assert HtmlValidator.editing_state_matches?(:read_write, node, [locked])
    end

    for type <- ~w(hidden checkbox radio submit reset button) do
      node = %{tag: "input", attributes: %{"type" => type, "contenteditable" => "true"}}
      assert HtmlValidator.editing_state_matches?(:read_only, node, [host])
    end

    assert HtmlValidator.editing_state_matches?(
             :read_write,
             %{tag: "textarea", attributes: %{}},
             [locked]
           )

    assert HtmlValidator.editing_state_matches?(
             :read_write,
             %{tag: "button", attributes: %{"disabled" => ""}},
             [host]
           )
  end

  test "control editability consumes canonical disabled state and keeps standalone matching" do
    host = %{tag: "div", attributes: %{"contenteditable" => "true"}}

    for tag <- ["input", "textarea"] do
      node = %{tag: tag, attributes: %{}, _initial_form_state: %{disabled: true}}
      assert HtmlValidator.editing_state_matches?(:read_only, node, [host])
      refute HtmlValidator.editing_state_matches?(:read_write, node, [host])
      enabled = %{node | _initial_form_state: %{disabled: false}}
      assert HtmlValidator.editing_state_matches?(:read_write, enabled, [])
      readonly = %{enabled | attributes: %{"readonly" => "false"}}
      assert HtmlValidator.editing_state_matches?(:read_only, readonly, [host])

      # Earlier prepared standalone controls may have ownership/selection but no disabled state.
      standalone = %{
        node
        | _initial_form_state: %{owner: nil},
          attributes: %{"disabled" => "false"}
      }

      assert HtmlValidator.editing_state_matches?(:read_only, standalone, [host])

      assert HtmlValidator.editing_state_matches?(
               :read_write,
               %{standalone | attributes: %{}},
               []
             )
    end

    qualified = %{
      tag: "input",
      attributes: %{"disabled" => "", "readonly" => "", "type" => "hidden"},
      attribute_namespaces: %{
        "disabled" => %{namespace: "urn:test"},
        "readonly" => %{namespace: "urn:test"},
        "type" => %{namespace: "urn:test"}
      }
    }

    assert HtmlValidator.editing_state_matches?(:read_write, qualified, [])
  end

  test "anchors and buttons share static descendant rules across parsed and direct trees" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

    node = fn tag, attributes, children ->
      %{type: :element, tag: tag, attributes: attributes, children: children}
    end

    text = %{type: :text, text: "Caption"}

    phrasing =
      Enum.map(
        ~w(code mark abbr ins del br img),
        &node.(&1, %{}, if(&1 in ~w(br img), do: [], else: [text]))
      )

    assert HtmlValidator.valid_form_element?("button", %{}, phrasing)
    assert HtmlValidator.valid_html_child?("div", "a")
    assert HtmlValidator.valid_html_child?("a", "div", ["div"])
    refute HtmlValidator.valid_html_child?("a", "div", ["p"])
    refute HtmlValidator.valid_html_child?("button", "div")
    assert HtmlValidator.valid_html_child?("div", "a", ["template", "a", "div"])
    refute HtmlValidator.valid_html_child?("span", "a", ["a", "template", "div"])
    assert HtmlValidator.valid_html_child_element?("a", node.("input", %{"type" => "hidden"}, []))
    assert HtmlValidator.valid_html_child_element?("button", node.("a", %{}, [text]))

    for child <- [
          node.("button", %{}, [text]),
          node.("input", %{}, []),
          node.("a", %{"href" => "https://example.com"}, [text]),
          node.("label", %{}, [text]),
          node.("details", %{}, []),
          node.("span", %{"tabindex" => "-1"}, [text]),
          node.("img", %{"usemap" => "#map"}, [])
        ] do
      wrapped = node.("span", %{}, [child])

      for host <- ["a", "button"] do
        refute HtmlValidator.valid_html_child_element?(host, child)
        refute HtmlValidator.valid_html_element?(host, %{}, [wrapped])
        dom = %{type: :document, children: [node.(host, %{}, [wrapped])]}
        assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(dom)
        assert diagnostic.stage == :style and diagnostic.operation == :compute
        assert diagnostic.source == host and diagnostic.message =~ "interactive"
      end
    end

    assert {:ok, dom} =
             HtmlParser.parse_detailed(
               "<div><a><p>Block</p></a><button><mark>Static</mark><br><code>Code</code></button></div>"
             )

    assert {:ok, _} = Style.compute_detailed(dom)
    [div] = dom.children
    [anchor, _] = div.children
    p = node.("p", %{}, [anchor])

    assert {:error, {:invalid_document, diagnostic}} =
             Style.compute_detailed(tree_content(%{dom | children: [p]}))

    assert diagnostic.stage == :style and diagnostic.source == "p"

    assert {:ok, inert} =
             HtmlParser.parse_detailed(
               "<button><template><input><button>Inert</button></template>Static</button>"
             )

    assert {:ok, _} = Style.compute_detailed(inert)
  end

  test "responsive srcset tokenizer follows URL commas and descriptor states" do
    alias NativeElixirPdfUtilities.Validators.ResponsiveImageValidator, as: Images

    candidates =
      Images.parse_srcset(
        " , first.png, data:image/png;base64,AAAA 2x, wide.png 400w 200h, third.png 3e0x"
      )

    assert Enum.map(candidates, &{&1.url, &1.width, &1.density}) == [
             {"first.png", nil, nil},
             {"data:image/png;base64,AAAA", nil, 2.0},
             {"wide.png", 400.0, nil},
             {"third.png", nil, 3.0}
           ]

    assert Images.parse_srcset(
             "a 1x 2x, b 0w, c -1x, d 2h, e 2w 3x, f 2x 3h, g junk(foo, bar), h 2w 3w, i 2h 3h, j 1.x, k +1x"
           ) == []

    assert [%{url: "last", width: 2.0}] = Images.parse_srcset("last 3h 2w")
    assert [%{url: "plain", density: nil}] = Images.parse_srcset("plain,,")
    assert [%{density: +0.0}] = Images.parse_srcset("zero 0x")
    assert [%{density: 0.5}] = Images.parse_srcset("half .5x")
    assert Images.parse_srcset("") == []
    assert [%{url: "a"}] = Images.parse_srcset("a ,")
    assert Images.parse_srcset("a unknown(unfinished") == []
  end

  test "responsive sizes and reusable print predicates use the validated environment" do
    alias NativeElixirPdfUtilities.Validators.{PrintMediaValidator, ResponsiveImageValidator}

    assert {:ok, context} =
             PrintMediaValidator.context(print_viewport: {800, 600}, image_selection_density: 2)

    for query <- [
          "",
          "all",
          "PRINT",
          "only print",
          "print /*note*/ and (width:800px)",
          "(400px < width <= 800px)",
          "(1000px >= width > 400px)",
          "(800px = width)",
          "screen, print",
          "print and (min-width: 600px)",
          "not screen and (width: 900px)",
          "(width >= 800px)",
          "(height < 700px)",
          "(height <= 600px)",
          "(width > 700px)",
          "(width = 800px)",
          "(orientation: landscape)",
          "(aspect-ratio: 4/3)",
          "(min-resolution: 192dpi)",
          "(resolution: 2dppx)",
          "(resolution: 2x)",
          "(max-resolution: 76dpcm)",
          "((min-width: 1000px) or (height: 600px))",
          "not (width: 900px)",
          "(width)",
          "(color)"
        ] do
      assert PrintMediaValidator.matches?(query, context), query
    end

    for query <- [
          "screen",
          "speech",
          "unknown-medium",
          "not print nonsense",
          "only (width:800px)",
          "(400px < width > 800px)",
          "(bad < width < 800px)",
          "(bad = width)",
          "print and (min-width: 900px)",
          "(unknown: yes)",
          "(unknown)",
          "(orientation: portrait)",
          "(resolution: rubbish)",
          "(width: rubbish)",
          "(aspect-ratio: 1/0)",
          "(nonsense > 4px)",
          "not print and (width: 800px)"
        ] do
      refute PrintMediaValidator.matches?(query, context), query
    end

    assert {:ok, portrait} = PrintMediaValidator.context(print_viewport: {600, 800})
    assert PrintMediaValidator.matches?("(orientation: portrait)", portrait)
    assert {:ok, page} = PrintMediaValidator.context(page_size: {300, 150}, margin: 15)
    assert page.width == 360 and page.height == 160 and page.density == 1
    assert {:ok, default} = PrintMediaValidator.context([])
    assert default.width > 790 and default.height > 1100

    for opts <- [
          [print_viewport: :bad],
          [print_viewport: {0, 1}],
          [print_viewport: {"a", 1}],
          [image_selection_density: 0],
          [image_selection_density: :bad],
          [image_selection_density: -1]
        ] do
      assert {:error, {:invalid_options, diagnostic}} = PrintMediaValidator.context(opts)
      assert diagnostic.stage == :options and diagnostic.message =~ "positive"
    end

    for opts <- [[page_size: :bad], [page_size: {100, 100}, margin: 60], [margin: -1]] do
      assert {:error, {:invalid_options, diagnostic}} = PrintMediaValidator.context(opts)
      assert diagnostic.message =~ "page area"
    end

    for {sizes, expected} <- [
          {"", 800.0},
          {"auto, 30px", 30.0},
          {"/*hint*/ (width:800px) /*gap*/ 30px", 30.0},
          {"auto", 800.0},
          {"(max-width: 700px) 30px, (min-width: 800px) 50vw, 100px", 400.0},
          {"20%, -1px, bad, 40px", 40.0},
          {"0", 0.0},
          {"calc(100vw - 20px - 10px)", 770.0},
          {"min(50vw, 200px)", 200.0},
          {"max(20px, 40px)", 40.0},
          {"clamp(20px, 50vw, 200px)", 200.0},
          {"calc((100vw - 20px) / 2)", 390.0},
          {"calc(2*20px + 10px*3)", 70.0}
        ] do
      assert ResponsiveImageValidator.source_size(sizes, context) == expected, sizes
    end

    for {length, expected} <- [
          {"1in", 96},
          {"1pc", 16},
          {"3pt", 4},
          {"2em", 32},
          {"2rem", 32},
          {"2.54cm", 96},
          {"25.4mm", 96},
          {"101.6q", 96},
          {"50vh", 300},
          {"50dvw", 400},
          {"50vi", 400},
          {"50svh", 300},
          {"50vb", 300},
          {"50lvmin", 300},
          {"50dvmax", 400},
          {"50vmin", 300},
          {"50vmax", 400},
          {"0px", 0}
        ] do
      assert {:ok, value} = PrintMediaValidator.source_length(length, context)
      assert_in_delta value, expected, 0.000001
    end

    for length <- [
          "2",
          "-1px",
          "20%",
          "calc(1px + 2)",
          "calc(1px*2px)",
          "calc(1px,2px)",
          "clamp(1px,2px)",
          "min()",
          "min(bad,2px)",
          "calc(+ 1px)",
          "1px + 2px",
          "calc(1px +)",
          "url(file)"
        ] do
      assert PrintMediaValidator.source_length(length, context) == :error, length
    end
  end

  test "infinite source-size arithmetic diagnoses rather than silently choosing 100vw" do
    alias NativeElixirPdfUtilities.HtmlToPdf

    for sizes <- ["calc(1px/1e-320)", "calc(1000000000px*1000000000)"] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               HtmlToPdf.render("<img srcset='a 20w' sizes='#{sizes}'>")

      assert diagnostic.stage == :style and diagnostic.message != ""
    end
  end

  test "responsive selection keeps first densities, source order and fallback semantics" do
    alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
    alias NativeElixirPdfUtilities.Validators.ResponsiveImageValidator, as: Images

    for {attrs, density, expected, correction} <- [
          {"src=fallback srcset='large 2x'", 1, "fallback", 1},
          {"src=fallback srcset='large 2x'", 2, "large", 2},
          {"src=fallback srcset='first 1x, duplicate 1x, large 2x'", 1, "first", 1},
          {"src=fallback srcset='small 100w, large 400w' sizes=200px", 1, "large", 2},
          {"src=fallback srcset='small 100w, large 400w' sizes=200px", 3, "large", 2},
          {"src=fallback srcset='bad -2x'", 1, "fallback", 1},
          {"srcset='first, other 2x'", 1, "first", 1},
          {"srcset='first 10w, other 20w' sizes=0", 1, "first", :infinity},
          {"srcset='first 10w' sizes=1e-320px", 1, "first", :infinity},
          {"srcset='first 10w' sizes='(bad-size)'", 1, "first", 10.0 / 800.0}
        ] do
      assert {:ok, %{children: [img]}} = HtmlParser.parse_detailed("<img #{attrs}>")

      assert {:ok, selected} =
               Images.select(img, [],
                 print_viewport: {800, 600},
                 image_selection_density: density
               )

      assert selected.url == expected and selected.density == correction
    end

    html =
      "<picture><source type=image/webp srcset=unsupported><source media=screen srcset=screen><source srcset=''><source type='IMAGE/PNG; charset=binary' media=print srcset='chosen 2x' width=80><img src=fallback height=100></picture>"

    assert {:ok, %{children: [picture]}} = HtmlParser.parse_detailed(html)
    img = List.last(picture.children)
    assert {:ok, selected} = Images.select(img, [picture], [])
    assert selected.url == "chosen" and selected.source == "source"
    assert selected.dimensions == %{width: 60.0}

    for srcset <- ["", "zero 0x"] do
      assert {:ok, %{children: [img]}} = HtmlParser.parse_detailed("<img srcset='#{srcset}'>")
      assert {:error, {reason, diagnostic}} = Images.select(img, [], [])
      assert reason in [:invalid_document, :resource_limit_exceeded]
      assert diagnostic.stage == :style and diagnostic.message != ""
    end

    img = %{
      type: :element,
      tag: "img",
      attributes: %{"srcset" => "javascript:bad 1x"},
      children: []
    }

    assert {:error, {:unsupported_html, diagnostic}} = Images.select(img, [], [])
    assert diagnostic.stage == :html
    assert {:error, {:invalid_options, _}} = Images.select(img, [], image_selection_density: 0)
    image = %{width: 60.0, height: 30.0}

    assert {:ok, %{width: 30.0, height: 15.0}} =
             Images.correct_natural_dimensions(image, %{url: "chosen", density: 2})

    assert {:ok, %{width: +0.0, height: +0.0}} =
             Images.correct_natural_dimensions(image, %{url: "chosen", density: :infinity})

    for density <- [1.0e-20, 1.0e-320] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               Images.correct_natural_dimensions(image, %{url: "chosen", density: density})

      assert diagnostic.source == "chosen" and diagnostic.message =~ "density correction"
    end
  end

  test "layout image display intent is validated centrally" do
    tree = %{
      type: :document,
      children: [
        %{
          type: :element,
          tag: "img",
          children: [],
          style: %{display: :image, image_display: :invalid}
        }
      ]
    }

    assert {:error, {:invalid_layout, diagnostic}} =
             HtmlValidator.validate_layout_input(
               tree,
               [],
               {:ok, {100.0, 100.0}},
               {:ok, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}}
             )

    assert %{stage: :layout, reason: :invalid_layout, operation: :layout, source: "img"} =
             diagnostic

    assert diagnostic.message =~ "inline or block"
  end

  test "microdata syntax accepts opaque absolute vocabularies and relative identifiers without fetching" do
    alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator

    for type <- [
          "https://schema.org/Person",
          "urn:example:Type",
          "mailto:type@example.org",
          "file:///type",
          "foo://host/type",
          "https://例え.テスト/名前",
          "https://[::1]:65535/type",
          "https://127.0.0.1/type",
          "javascript:opaque"
        ],
        id <- [
          "",
          "#fragment",
          "./person",
          "/person",
          "//example.org/person",
          "urn:example:42",
          " \t../person\r\n",
          "https://例え.テスト/人"
        ] do
      node = %{
        tag: "div",
        attributes: %{"itemscope" => "false", "itemtype" => type, "itemid" => id}
      }

      assert :ok = HtmlMetadataValidator.validate_attributes(node)
    end

    for attributes <- [
          %{},
          %{"itemscope" => ""},
          %{"itemscope" => "", "itemref" => ""},
          %{"itemscope" => "", "itemtype" => "urn:example:A\t urn:example:a\nurn:example:B"},
          %{"itemprop" => "Name\tname\fhttps://example.org/name"},
          %{"itemprop" => "non\u00A0breaking"},
          %{"itemscope" => "", "itemref" => "A a missing-reference"}
        ] do
      assert :ok =
               HtmlMetadataValidator.validate_attributes(%{tag: "x-box", attributes: attributes})
    end
  end

  test "microdata syntax diagnoses invalid applicability tokens and URLs in parsed and supplied trees" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
    alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator

    for {attributes, message} <-
          [
            {%{"itemtype" => "urn:example:A"}, "itemscope"},
            {%{"itemref" => "a"}, "itemscope"},
            {%{"itemid" => "#id"}, "both itemscope and itemtype"},
            {%{"itemscope" => "", "itemid" => "#id"}, "both itemscope and itemtype"},
            {%{"itemscope" => "", "itemtype" => " \t\n"}, "at least one"},
            {%{"itemprop" => ""}, "at least one"},
            {%{"itemprop" => "name name"}, "unique"},
            {%{"itemscope" => "", "itemref" => "a\ra"}, "unique"},
            {%{"itemscope" => "", "itemtype" => "urn:A urn:A"}, "unique"},
            {%{"itemscope" => "", "itemtype" => "relative"}, "absolute URL"},
            {%{"itemprop" => "bad.name"}, "full stop"},
            {%{"itemprop" => "1bad:name"}, "full stop"}
          ] ++
            Enum.map(
              [
                "https://",
                "https://[bad",
                "https://[1::2::3]/",
                "https://host:65536/type",
                "https://host:bad/type",
                "https://bad%host/type",
                "https://999.1.1.1/type",
                "https://host/%zz",
                "https://host/<bad>",
                "https:host/type",
                "https://host/\u0080",
                "https://host/\uFDD0",
                "https://host/\u{1FFFF}",
                "https://bad\u00A0host/type",
                "urn:example:[type]",
                "file://host:80/type",
                "file://user@host/type"
              ],
              fn url ->
                {%{"itemscope" => "", "itemtype" => url}, "absolute URL"}
              end
            ) ++
            Enum.map(["bad URL", "%zz", "https://", "a\\b"], fn url ->
              {%{"itemscope" => "", "itemtype" => "urn:example:A", "itemid" => url}, "valid URL"}
            end) do
      node = %{type: :element, tag: "div", attributes: attributes, children: []}

      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlMetadataValidator.validate_attributes(node)

      assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
      assert diagnostic.module == HtmlMetadataValidator and diagnostic.source == "div"
      assert diagnostic.message =~ message

      assert {:error, {:unsupported_html, diagnostic}} =
               Style.compute_detailed(%{type: :document, children: [node]})

      assert diagnostic.message =~ message

      markup = Enum.map_join(attributes, " ", fn {name, value} -> "#{name}='#{value}'" end)

      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlParser.parse_detailed("\n<div #{markup}></div>")

      assert diagnostic.line == 2 and diagnostic.column == 1
      assert diagnostic.source =~ "<div" and diagnostic.message =~ message
    end
  end

  test "namespaced metadata never acquires HTML semantics and foreign translation inherits" do
    alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator

    for namespace <- [:svg, :mathml] do
      node = %{
        namespace: namespace,
        tag: "host",
        attributes: %{"itemtype" => "relative", "translate" => "yes"}
      }

      assert :ok = HtmlMetadataValidator.validate_attributes(node)
      refute HtmlMetadataValidator.translation_mode(node, false)
      assert HtmlMetadataValidator.translation_mode(node, true)
    end

    for identity <- [
          "urn:example",
          %{namespace: "urn:example", local_name: "itemtype", prefix: "x"}
        ] do
      node = %{
        tag: "div",
        attributes: %{"itemtype" => "relative", "translate" => "yes"},
        attribute_namespaces: %{"itemtype" => identity, "translate" => identity}
      }

      assert :ok = HtmlMetadataValidator.validate_attributes(node)
      refute HtmlMetadataValidator.translation_mode(node, false)
    end

    for {value, expected} <- [
          {"", true},
          {"YES", true},
          {"nO", false},
          {" yes ", false},
          {" no ", false},
          {"invalid", false},
          {nil, false}
        ] do
      node = %{attributes: if(is_nil(value), do: %{}, else: %{"translate" => value})}
      assert HtmlMetadataValidator.translation_mode(node, false) == expected
    end
  end

  test "itemref validates case-sensitive IDs without crossing inert tree or namespace boundaries" do
    alias NativeElixirPdfUtilities.Validators.HtmlMetadataValidator

    node = %{
      type: :element,
      tag: "div",
      attributes: %{"itemscope" => "", "itemref" => "target"},
      children: [],
      line: 7,
      column: 4
    }

    target = %{
      type: :element,
      tag: "g",
      namespace: :svg,
      attributes: %{"id" => "target"},
      children: []
    }

    assert :ok =
             HtmlMetadataValidator.validate_item_references(node, [
               %{type: :text, text: "Before"},
               target
             ])

    assert :ok =
             HtmlMetadataValidator.validate_item_references(
               %{node | attributes: %{"itemref" => ""}},
               []
             )

    assert :ok =
             HtmlMetadataValidator.validate_item_references(Map.put(node, :namespace, :svg), [])

    assert :ok =
             HtmlMetadataValidator.validate_item_references(
               Map.put(node, :attribute_namespaces, %{"itemref" => "urn:example"}),
               []
             )

    for candidate <- [
          %{target | attributes: %{"id" => "Target"}},
          Map.put(target, :attribute_namespaces, %{"id" => "urn:example"}),
          Map.put(target, :attribute_namespaces, nil),
          %{target | children: :invalid},
          %{type: :text, text: "No target"}
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlMetadataValidator.validate_item_references(node, [candidate])

      assert diagnostic.stage == :html and diagnostic.reason == :unsupported_html
      assert diagnostic.operation == :compute and diagnostic.source == "div"
      assert diagnostic.line == 7 and diagnostic.column == 4
      assert diagnostic.message =~ "same authored tree"
    end
  end

  test "composition URL contexts retain the child document URL" do
    alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator
    child = "file:///allowed/reports/child.html"
    dom = %{type: :document, children: [%{tag: "base", attributes: %{"href" => "../assets/"}}]}
    assert {:ok, context} = HtmlUrlValidator.document_context_from_url(dom, child)
    assert context.fallback_base == child
    assert context.resolution_base == "file:///allowed/assets/"
    assert context.authored_base?

    assert {:ok, {:uri, "file:///allowed/assets/chapter.html"}} =
             HtmlUrlValidator.prepare_anchor("chapter.html", context)

    assert {:ok, plain} = HtmlUrlValidator.document_context_from_url(%{children: []}, child)
    assert plain.resolution_base == child and not plain.authored_base?
    assert {:ok, {:fragment, "target"}} = HtmlUrlValidator.prepare_anchor("#target", plain)

    assert {:ok, {:uri, "file:///allowed/reports/sibling.html"}} =
             HtmlUrlValidator.prepare_anchor("sibling.html", plain)

    assert {:error, {:invalid_document, %{source: "#bad%escape"}}} =
             HtmlUrlValidator.prepare_anchor("#bad%escape", plain)

    assert {:error, {:invalid_document, %{source: "<<255>>"}}} =
             HtmlUrlValidator.prepare_anchor(<<255>>, plain)

    refute HtmlValidator.valid_link_url?(<<255>>)
  end

  test "direct DOM bases diagnose invalid metadata placement and children" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style
    base = %{type: :element, tag: "base", attributes: %{"href" => "/"}, children: []}
    text = %{type: :text, text: "Unexpected"}
    container = %{type: :element, tag: "div", attributes: %{}, children: [base]}

    for node <- [%{base | children: [text]}, container] do
      assert {:error, {:invalid_document, diagnostic}} =
               Style.compute_detailed(%{type: :document, children: [node]})

      assert diagnostic.stage == :style and diagnostic.operation == :compute
      assert diagnostic.source == "base" and diagnostic.message =~ "metadata placement"
    end

    assert {:error, {:invalid_document, %{stage: :html, source: "base", operation: :compute}}} =
             Style.compute_detailed(%{type: :document, children: [%{base | attributes: %{}}]})
  end

  test "base contexts preserve authored attributes and use the first eligible href" do
    alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
    alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator

    html = """
    <head><base target=_blank><template><base href='https://inert.test/'></template>
    <base href=' ../assets/ '><base href='https://ignored.test/'></head>
    <body><a href='chapter.html' rel='help next' hreflang=en target=_self
    download=chapter ping='https://ping.test/' type=text/html referrerpolicy=no-referrer>Read</a></body>
    """

    assert {:ok, dom} = HtmlParser.parse_document_detailed(html)
    assert {:ok, context} = HtmlUrlValidator.document_context(dom, base_url: "/allowed/docs")
    assert context.fallback_base == "file:///allowed/docs/"
    assert context.resolution_base == "file:///allowed/assets/"
    assert context.authored_base?
    [html_node] = dom.children
    [head, body] = html_node.children
    assert Enum.at(head.children, 2).attributes["href"] == " ../assets/ "
    [anchor] = body.children
    assert anchor.attributes["href"] == "chapter.html"
    assert anchor.attributes["rel"] == "help next"
    assert anchor.attributes["download"] == "chapter"

    assert {:ok, {:uri, "file:///allowed/assets/chapter.html"}} =
             HtmlUrlValidator.prepare_anchor(anchor.attributes["href"], context)

    foreign_base = %{
      tag: "base",
      namespace: :svg,
      attributes: %{"href" => "https://foreign.test/"}
    }

    assert {:ok, %{resolution_base: nil, authored_base?: false}} =
             HtmlUrlValidator.document_context(
               %{children: [foreign_base, %{type: :text, text: "x"}]},
               []
             )
  end

  test "base fallback and URL preparation preserve document identity and diagnostics" do
    alias NativeElixirPdfUtilities.Validators.HtmlUrlValidator
    fallback = "https://example.test/docs/document.html"

    for href <- ["http://", "data:text/plain,ignored", "relative/", ""] do
      dom = %{
        children: [
          %{tag: "base", attributes: %{"href" => href}},
          %{tag: "base", attributes: %{"href" => "https://ignored.test/"}}
        ]
      }

      assert {:ok, context} = HtmlUrlValidator.document_context(dom, base_url: fallback)
      expected = if href == "relative/", do: "https://example.test/docs/relative/", else: fallback
      assert context.resolution_base == expected
    end

    assert {:ok, empty} = HtmlUrlValidator.document_context(%{children: []}, [])

    assert {:ok, {:fragment, "target space"}} =
             HtmlUrlValidator.prepare_anchor("#target%20space", empty)

    assert {:ok, {:fragment, "�"}} = HtmlUrlValidator.prepare_anchor("#%FF", empty)
    assert {:ok, {:fragment, "#target"}} = HtmlUrlValidator.prepare_anchor("##target", empty)
    assert {:ok, {:fragment, ""}} = HtmlUrlValidator.prepare_anchor("#", empty)

    assert {:ok, {:uri, "https://example.test/path%20with%20spaces"}} =
             HtmlUrlValidator.prepare_anchor(" \nhttps://example.test/path with spaces\t ", empty)

    assert {:ok, {:uri, "mailto:test@example.test"}} =
             HtmlUrlValidator.prepare_anchor("mailto:test@example.test", empty)

    assert {:ok, {:fragment, ""}} = HtmlUrlValidator.prepare_anchor("", empty)

    assert {:ok, identity} =
             HtmlUrlValidator.document_context(%{children: []}, base_url: fallback)

    assert {:ok, {:fragment, "end"}} =
             HtmlUrlValidator.prepare_anchor(fallback <> "#end", identity)

    assert {:ok, {:fragment, ""}} = HtmlUrlValidator.prepare_anchor("", identity)

    assert {:ok, same_base} =
             HtmlUrlValidator.document_context(
               %{children: [%{tag: "base", attributes: %{"href" => fallback <> "#ignored"}}]},
               base_url: fallback
             )

    assert {:ok, {:fragment, "end"}} = HtmlUrlValidator.prepare_anchor("#end", same_base)

    assert {:ok, remote} =
             HtmlUrlValidator.document_context(
               %{children: [%{tag: "base", attributes: %{"href" => "https://other.test/"}}]},
               []
             )

    assert {:ok, {:uri, "https://other.test/#target"}} =
             HtmlUrlValidator.prepare_anchor("#target", remote)

    assert {:ok, {:uri, "https://cdn.test/a"}} =
             HtmlUrlValidator.prepare_anchor("//cdn.test/a", remote)

    assert {:ok, {:uri, "https://other.test/?q=value"}} =
             HtmlUrlValidator.prepare_anchor("?q=value", remote)

    for href <- [
          nil,
          "relative.html",
          "//example.test/a",
          "mailto:missing",
          "ftp://example.test/a",
          "http://",
          "https://bad host/a",
          "https://x.test:99999/a",
          "http://[",
          "https://x.test:bad/",
          "https://x.test\\@evil.test/path",
          "https://x.test/%escape",
          "https://x%20.test/a",
          "file://other.test/a"
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               HtmlUrlValidator.prepare_anchor(href, empty)

      assert diagnostic.stage == :style and diagnostic.reason == :invalid_document
      assert is_binary(diagnostic.message) and byte_size(diagnostic.message) > 0
      assert is_binary(diagnostic.source)
    end

    for href <- ["javascript:run()", "\tvbscript:run()", "java\nscript:run()"] do
      assert {:error, {:unsupported_html, %{stage: :html, source: "href"}}} =
               HtmlUrlValidator.prepare_anchor(href, empty)

      assert {:error, {:unsupported_html, %{stage: :html}}} =
               HtmlUrlValidator.document_context(
                 %{children: [%{tag: "base", attributes: %{"href" => href}}]},
                 []
               )
    end

    assert {:error, {:invalid_document, %{source: "base_url"}}} =
             HtmlUrlValidator.document_context(%{children: []}, base_url: 1)

    assert {:error, {:invalid_document, _}} =
             HtmlUrlValidator.document_context(%{children: []}, base_url: "http://")

    assert {:ok, %{fallback_base: "file:///allowed/space%20dir/"}} =
             HtmlUrlValidator.document_context(%{children: []},
               base_url: "file:///allowed/space%20dir"
             )

    assert {:error, {:invalid_document, %{source: "child.html"}}} =
             HtmlUrlValidator.resolve_reference("child.html", "mailto:test@example.test")

    assert {:error, {:invalid_document, %{source: "<<255>>"}}} =
             HtmlUrlValidator.resolve_reference(<<255>>, nil)

    assert {:ok, "mailto:test@example.test#section"} =
             HtmlUrlValidator.resolve_reference("#section", "mailto:test@example.test")

    assert {:ok, "data:image/png;base64,eA=="} =
             HtmlUrlValidator.resolve_reference("data:image/png;base64,eA==", nil)

    assert {:ok, "unchanged.png"} = HtmlUrlValidator.resource_reference("unchanged.png", empty)

    assert {:ok, "https://other.test/a.png"} =
             HtmlUrlValidator.resource_reference("a.png", remote)

    assert {:error, {:invalid_document, _}} =
             HtmlUrlValidator.resource_reference("a.png", %{empty | authored_base?: true})

    assert :ok = HtmlUrlValidator.validate_base_element(%{"target" => "_blank"}, [])

    assert {:error, {:invalid_document, %{source: "base"}}} =
             HtmlUrlValidator.validate_base_element(%{}, [])

    assert {:error, {:invalid_document, %{source: "base"}}} =
             HtmlUrlValidator.validate_base_element(%{"href" => "/"}, [%{type: :text, text: "x"}])
  end

  test "direct disclosure trees use canonical structure and group validation" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
    assert {:ok, dom} = HtmlParser.parse_detailed("<details><summary>Legend</summary></details>")
    [details] = dom.children
    [summary] = details.children

    forged =
      summary |> Map.put(:_default_disclosure_summary, true) |> Map.put(:_disclosure_open, true)

    html = %{dom | children: [%{details | children: [forged]}]}
    assert {:ok, %{children: [%{children: [styled]}]}} = Style.compute_detailed(html)
    assert styled.style.list_marker_type == :disclosure_closed

    orphan = %{dom | children: [summary]}

    assert {:error, {:invalid_document, diagnostic}} =
             Style.compute_detailed(tree_content(orphan))

    assert diagnostic.stage == :style and diagnostic.source == "summary"
    assert {:ok, foreign} = HtmlParser.parse_detailed("<svg><details name=group></details></svg>")
    assert {:error, {:unsupported_html, diagnostic}} = Style.compute_detailed(foreign)
    assert diagnostic.stage == :layout

    for namespace <- [:svg, :mathml] do
      foreign_summary = Map.put(summary, :namespace, namespace)
      selected = HtmlValidator.disclosure_children(%{details | children: [foreign_summary]})
      assert length(selected) == 1
      assert Enum.map(hd(selected).children, & &1.text) == ["Details"]
    end
  end

  test "required and optional matching uses applicability independently of mutability" do
    for type <- [
          "text",
          "search",
          "tel",
          "url",
          "email",
          "password",
          "checkbox",
          "radio",
          "unknown"
        ],
        value <- ["", "false", "required"] do
      node = %{
        tag: "input",
        attributes: %{"type" => type, "required" => value, "disabled" => "", "readonly" => ""}
      }

      assert HtmlValidator.control_state_matches?(:required, node)
      refute HtmlValidator.control_state_matches?(:optional, node)
      optional = update_in(node.attributes, &Map.delete(&1, "required"))
      refute HtmlValidator.control_state_matches?(:required, optional)
      assert HtmlValidator.control_state_matches?(:optional, optional)

      for namespace <- [:svg, :mathml], candidate <- [node, optional] do
        foreign = Map.put(candidate, :namespace, namespace)
        refute HtmlValidator.control_state_matches?(:required, foreign)
        refute HtmlValidator.control_state_matches?(:optional, foreign)
      end
    end

    for tag <- ["select", "textarea"] do
      assert HtmlValidator.valid_boolean_form_attribute?(tag, "required")
      assert HtmlValidator.valid_form_attribute?(tag, "required", "false")

      assert HtmlValidator.control_state_matches?(:required, %{
               tag: tag,
               attributes: %{"required" => ""}
             })

      assert HtmlValidator.control_state_matches?(:optional, %{tag: tag, attributes: %{}})
    end

    assert HtmlValidator.valid_boolean_form_attribute?("input", "required")
    refute HtmlValidator.valid_boolean_form_attribute?("button", "required")

    for type <- ["hidden", "button", "submit", "reset"],
        attributes <- [%{}, %{"required" => ""}] do
      node = %{tag: "input", attributes: Map.put(attributes, "type", type)}
      refute HtmlValidator.control_state_matches?(:required, node)
      refute HtmlValidator.control_state_matches?(:optional, node)
    end

    for tag <- ["button", "option", "optgroup", "p", "custom-element"],
        attributes <- [%{}, %{"required" => ""}] do
      refute HtmlValidator.control_state_matches?(:required, %{tag: tag, attributes: attributes})
      refute HtmlValidator.control_state_matches?(:optional, %{tag: tag, attributes: attributes})
    end
  end

  test "margin grammar and horizontal rule hints delegate to shared validators" do
    assert {:ok, :auto} = HtmlValidator.parse_css_margin(" AUTO ")
    assert {:ok, -1.5} = HtmlValidator.parse_css_margin("-2px")

    assert {:ok, margins} = HtmlValidator.parse_css_box_lengths("0 auto", :margin)
    assert margins == %{top: 0.0, right: :auto, bottom: 0.0, left: :auto}

    for value <- [nil, 10, "", "auto auto auto auto auto", "bogus"] do
      assert :error = HtmlValidator.parse_css_box_lengths(value, :margin)
    end

    assert :error = HtmlValidator.parse_css_box_lengths("auto", :nonnegative)
    assert :error = HtmlValidator.parse_css_box_lengths("1pt", :unknown)

    assert {:ok, %{top: 1.0, right: 2.0, bottom: 3.0, left: 4.0}} =
             HtmlValidator.parse_css_box_lengths("1pt 2pt 3pt 4pt")

    assert {:ok, %{top: 1.0, right: 2.0, bottom: 3.0, left: 2.0}} =
             HtmlValidator.parse_css_box_lengths("1pt 2pt 3pt")

    assert {:ok, %{top: 1.0, right: 1.0, bottom: 1.0, left: 1.0}} =
             HtmlValidator.parse_css_box_lengths("1pt")

    for {attributes, expected} <- [
          {%{"size" => "1"}, %{top: 0.75, right: 0.75, bottom: 0.0, left: 0.75}},
          {%{"size" => "4", "noshade" => "false"},
           %{top: 1.5, right: 1.5, bottom: 1.5, left: 1.5}},
          {%{"size" => "-0", "color" => "red"}, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}}
        ] do
      assert {:ok, hints} = HtmlValidator.horizontal_rule_hints(attributes)
      assert hints.border_widths == expected
    end

    assert {:ok, hints} =
             HtmlValidator.horizontal_rule_hints(%{
               "width" => "25%",
               "height" => "9999",
               "size" => " +5junk",
               "color" => "chucknorris",
               "align" => "RIGHT"
             })

    assert hints.width == {:percent, 0.25}
    refute Map.has_key?(hints, :height)
    assert hints.color == {192 / 255, 0.0, 0.0}
    assert hints.margin.left == :auto and hints.margin.right == 0.0
    assert {:ok, %{height: 2.25}} = HtmlValidator.horizontal_rule_hints(%{"size" => "5"})

    assert {:ok, zero_size} = HtmlValidator.horizontal_rule_hints(%{"size" => "0"})
    refute Map.has_key?(zero_size, :height) or Map.has_key?(zero_size, :border_widths)
    assert zero_size.border_styles.top == :inset

    assert :error = HtmlValidator.parse_css_margin(nil)

    for size <- ["", "-1", "garbage"] do
      assert {:ok, hints} =
               HtmlValidator.horizontal_rule_hints(%{"size" => size, "color" => "transparent"})

      refute Map.has_key?(hints, :height) or Map.has_key?(hints, :border_widths) or
               Map.has_key?(hints, :color)

      assert hints.border_styles.top == :solid
    end
  end

  test "legacy HTML colors follow their separate named and truncated channel algorithm" do
    for {input, expected} <- [
          {" red ", {1, 0, 0}},
          {"REBECCAPURPLE", {102 / 255, 51 / 255, 153 / 255}},
          {"#abc", {170 / 255, 187 / 255, 204 / 255}},
          {"fff", {15 / 255, 15 / 255, 15 / 255}},
          {"#abcdef12", {171 / 255, 222 / 255, 18 / 255}},
          {"chucknorris", {192 / 255, 0.0, 0.0}},
          {"💩red", {0.0, 14 / 255, 208 / 255}},
          {" \t\n", {0.0, 0.0, 0.0}},
          {"#", {0.0, 0.0, 0.0}},
          {"#000100020003", {1 / 255, 2 / 255, 3 / 255}},
          {String.duplicate("0", 127) <> "f" <> String.duplicate("a", 1000),
           {0.0, 0.0, 240 / 255}}
        ] do
      assert {:ok, ^expected} = HtmlValidator.parse_html_legacy_color(input)
    end

    for invalid <- ["", "transparent", " TrAnSpArEnT ", nil, 10, <<255>>] do
      assert :error = HtmlValidator.parse_html_legacy_color(invalid)
    end

    assert {:ok, color} = HtmlValidator.parse_html_legacy_color("rgb(1,2,3)")
    refute color == {1 / 255, 2 / 255, 3 / 255}
  end

  alias NativeElixirPdfUtilities.HtmlToPdf.Style

  test "CSS named colors use published channels, aliases and ASCII keyword matching" do
    assert {:error, :invalid_css} = HtmlValidator.validate_css_declaration(nil)
    assert {:error, :invalid_css} = HtmlValidator.validate_css_declaration("missing-colon")
    assert {:error, :invalid_css} = HtmlValidator.validate_css_declaration("color: !important")

    assert {:ok, {"color", "red!urgent"}} =
             HtmlValidator.validate_css_declaration("color:red!urgent")

    assert {:ok, {"color", "red!\u0131mportant"}} =
             HtmlValidator.validate_css_declaration("color:red!\u0131mportant")

    assert HtmlValidator.split_css_tokens("red\u00a0blue green") == ["red\u00a0blue", "green"]
    assert HtmlValidator.split_css_tokens("") == []
    assert HtmlValidator.split_css_tokens(" \t\n") == []
    assert HtmlValidator.split_css_tokens("red green ") == ["red", "green"]
    assert :error = HtmlValidator.parse_css_color("rgb(\u00a00,0,0)", nil)
    assert :error = HtmlValidator.parse_css_color("rgba(0,0,0,\u00a01)", nil)

    for {name, channels} <- [
          {"aliceblue", {240 / 255, 248 / 255, 1}},
          {"rebeccapurple", {0.4, 0.2, 0.6}},
          {"lightgoldenrodyellow", {250 / 255, 250 / 255, 210 / 255}},
          {"khaki", {240 / 255, 230 / 255, 140 / 255}},
          {"aqua", {0, 1, 1}},
          {"fuchsia", {1, 0, 1}},
          {"darkslategrey", {47 / 255, 79 / 255, 79 / 255}}
        ] do
      for token <- [name, String.upcase(name), " \t" <> name <> "\n"] do
        assert {:ok, ^channels} = HtmlValidator.parse_css_color(token, nil)
      end
    end

    for {a, b} <- [
          {"aqua", "cyan"},
          {"fuchsia", "magenta"},
          {"darkgray", "darkgrey"},
          {"dimgray", "dimgrey"},
          {"lightslategray", "lightslategrey"}
        ] do
      assert HtmlValidator.parse_css_color(a, nil) == HtmlValidator.parse_css_color(b, nil)
    end

    for {input, expected} <- [
          {"", ""},
          {" \t\n\f\r", ""},
          {"khaki", "khaki"},
          {" ", ""},
          {" x", "x"},
          {"x\t", "x"},
          {"x \n", "x"},
          {" \u0301", "\u0301"},
          {"\rkhaki\f", "khaki"},
          {"a b", "a b"},
          {"\u00a0khaki\u00a0", "\u00a0khaki\u00a0"},
          {" \u00a0khaki\u00a0 ", "\u00a0khaki\u00a0"}
        ] do
      assert HtmlValidator.trim_ascii_whitespace(input) == expected
    end

    assert {:ok, nil} = HtmlValidator.parse_css_background_color(" NONE ", nil)
    assert :error = HtmlValidator.parse_css_background_color("Khaki", nil)

    for property <- ["text-decoration", "text-decoration-color"] do
      assert :error = HtmlValidator.parse_css_text_decoration(property, "Khaki", 10, {0, 0, 0})
    end

    assert {:ok, {0.4, 0.2, 0.6}} = HtmlValidator.parse_css_background_color("rebeccapurple", nil)
    assert {:ok, :transparent} = HtmlValidator.parse_css_color("TRANSPARENT", nil)
    assert {:ok, {1, 0, 0}} = HtmlValidator.parse_css_color("currentColor", {1, 0, 0})

    for token <- [
          "Khaki",
          "\u00a0khaki\u00a0",
          "rebecca-purple",
          "alice blue",
          "Canvas",
          "inherit"
        ] do
      assert :error = HtmlValidator.parse_css_color(token, nil)
    end
  end

  test "popover keywords retain raw values and use the pinned invalid-value default" do
    assert HtmlValidator.html_popover_state(%{}) == :none

    for {value, state} <- [
          {"", :auto},
          {"AUTO", :auto},
          {"HiNt", :hint},
          {"manual", :manual},
          {" auto ", :manual},
          {"invalid", :manual},
          {String.duplicate("x", 1000), :manual}
        ] do
      assert HtmlValidator.html_popover_state(%{"popover" => value}) == state
      assert :ok = HtmlValidator.validate_html_element_attributes("x-box", %{"popover" => value})
    end

    defaults = HtmlValidator.html_popover_defaults(%{}, "p", 12.0)
    assert defaults.display == :none and defaults.position == :fixed
    assert defaults.width == :fit_content and defaults.height == :fit_content
    assert defaults.offsets == %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
    assert defaults.padding == %{top: 3.0, right: 3.0, bottom: 3.0, left: 3.0}

    assert HtmlValidator.html_popover_defaults(%{"open" => "false"}, "dialog", 12.0).display ==
             :block

    refute HtmlValidator.control_state_matches?(:popover_open, %{
             attributes: %{"popover" => "auto"}
           })
  end

  test "static URL validation rejects normalized active schemes without rejecting ordinary references" do
    for name <- ["href", "src", "xlink:href"],
        value <- [
          "javascript:run()",
          " \tJaVaScRiPt:run()",
          "java\tscr\nipt\r:run()",
          "VBSCRIPT:run()",
          "vb\tscript:run()"
        ] do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlValidator.validate_static_url_attributes(%{name => value})

      assert diagnostic.stage == :html
      assert diagnostic.source == name
      assert diagnostic.message =~ "non-executable"
    end

    for value <- [
          "https://example.com/a",
          "mailto:a@example.com",
          "local.png",
          "data:image/png;base64,AA==",
          "#local",
          "java script:run()"
        ] do
      assert :ok = HtmlValidator.validate_static_url_attributes(%{"src" => value})
    end

    assert :ok =
             HtmlValidator.validate_static_url_attributes(
               %{"href" => "javascript:metadata"},
               "x-box"
             )

    assert {:error, {:unsupported_html, _}} =
             HtmlValidator.validate_static_url_attributes(%{"href" => "javascript:run()"}, "a")

    assert {:ok, nil} = HtmlValidator.parse_background_image_reference(" NONE ")

    for value <- [
          "url(local.png)",
          "url('local.png')",
          ~s|url("local.png")|,
          ~S|url('lo\63 al.png')|
        ] do
      assert {:ok, "local.png"} = HtmlValidator.parse_background_image_reference(value)
    end

    assert {:error, :invalid_document} = HtmlValidator.parse_background_image_reference("url()")

    for value <- ["url(javascript:run)", ~S|url('\6a avascript:run')|] do
      assert {:error, {:unsupported_html, %{stage: :css, source: "background-image"}}} =
               HtmlValidator.parse_background_image_reference(value)
    end
  end

  test "validated control detection excludes dormant templates and preserves legacy validation results" do
    alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser

    for {html, expected} <- [
          {"<p>Ordinary</p>", false},
          {"<template><input type='radio' checked></template><p>Visible</p>", false},
          {"<input>", true},
          {"<form></form>", true},
          {"<label>Caption</label>", true},
          {"<div inert><input type='checkbox' checked></div>", true}
        ] do
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      opts = [system_font_discovery: false]
      font_options = Font.normalize_options(opts)
      assert {:ok, ^expected} = HtmlValidator.prepare_style_input(dom, opts, font_options)
      assert :ok = HtmlValidator.validate_style_input(dom, opts, font_options)
    end

    for invalid <- [
          nil,
          %{
            type: :document,
            children: [
              %{
                type: :element,
                tag: "input",
                attributes: %{"type" => "text"},
                children: [%{type: :text, text: "Invalid void contents"}]
              }
            ]
          }
        ] do
      opts = [system_font_discovery: false]

      assert {:error, {reason, diagnostic}} =
               HtmlValidator.validate_style_input(invalid, opts, Font.normalize_options(opts))

      assert diagnostic.reason == reason and diagnostic.stage == :style
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end
  end

  test "nested style computation restores the containing default language" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, RenderCache}
    alias NativeElixirPdfUtilities.Validators.LanguageValidator

    outer_children = [
      %{tag: "meta", attributes: %{"http-equiv" => "content-language", "content" => "en"}}
    ]

    outer_node = %{attributes: %{}, _selector_id: make_ref()}

    assert {:ok, inner} =
             HtmlParser.parse_detailed(
               "<meta http-equiv='content-language' content='fr'><p>Inner document</p>"
             )

    RenderCache.run(fn cache ->
      LanguageValidator.with_document(outer_children, cache, fn ->
        assert {:ok, %{children: [paragraph]}} =
                 Style.compute_detailed(inner, stylesheets: [{:css, "p:lang(fr){color:blue}"}])

        assert paragraph.style.color == {0, 0, 1}
        assert LanguageValidator.resolved_language(outer_node, []) == ["en"]
      end)
    end)
  end

  test "language matching uses canonical aliases, extlang forms and extended filtering" do
    alias NativeElixirPdfUtilities.Validators.LanguageValidator

    for {range, tag} <- [
          {"de-DE", "de-Latn-DE"},
          {"*-DE", "de-Latn-DE"},
          {"DE-*-DE", "de-DE-1996"},
          {"de-*-*", "de"},
          {"iw", "he-IL"},
          {"i-klingon", "tlh"},
          {"en-BU", "en-MM"},
          {"ja-Latn-heploc", "ja-Latn-alalc97"},
          {"cmn-Hans", "zh-cmn-Hans-CN"},
          {"zh-cmn", "cmn-Hans"},
          {"en-a-foo-b-bar", "en-b-bar-a-foo"},
          {"qaa", "QAA-x-private"},
          {"x-private", "X-PRIVATE"},
          {"i-default", "i-default"},
          {"", ""}
        ] do
      assert LanguageValidator.matches?(
               LanguageValidator.canonicalize(range),
               LanguageValidator.canonicalize(tag)
             ),
             inspect({range, tag})
    end

    for {range, tag} <- [
          {"*", ""},
          {"en", ""},
          {"", "und"},
          {"en-US", "en-x-US"},
          {"de-DE", "de-a-DE"},
          {"en", " en "},
          {"he", "iw-INVALID-INVALID"}
        ] do
      refute LanguageValidator.matches?(
               LanguageValidator.canonicalize(range),
               LanguageValidator.canonicalize(tag)
             ),
             inspect({range, tag})
    end

    assert LanguageValidator.canonicalize("en-abc") == ["en", "abc"]
    assert LanguageValidator.canonicalize("cmn-*") == ["zh", "cmn", "*"]
    assert LanguageValidator.canonicalize("en-x-*-BU") == ["en", "x", "*", "bu"]
    refute LanguageValidator.matches?([], ["en"])
    refute LanguageValidator.matches?(["en", "us"], ["en"])
    assert LanguageValidator.canonicalize("en-x-BU-Qaai") == ["en", "x", "bu", "qaai"]
    assert LanguageValidator.canonicalize("en-a-foo-a-bar") == ["en", "a", "foo", "a", "bar"]

    assert LanguageValidator.canonicalize("iw-abc-abc-abc-abc") == [
             "iw",
             "abc",
             "abc",
             "abc",
             "abc"
           ]

    assert LanguageValidator.canonicalize("en-Qaai") == ["en", "qaai"]
    assert LanguageValidator.canonicalize("en-Latn") == ["en", "latn"]
  end

  test "functional selectors reject missing delimiters and unterminated CSS strings" do
    for source <- [":lang", ":lang(", ":lang(\"unfinished)"] do
      assert HtmlValidator.parse_css_language_selector(source) == :error
    end

    for source <- [":not", ":not(", ":not(\"unfinished)"] do
      assert HtmlValidator.parse_css_negated_selector(source) == :error
    end

    assert HtmlValidator.parse_css_negated_selector("plain") == nil
  end

  test "only XML namespace language overrides the HTML lang attribute" do
    alias NativeElixirPdfUtilities.Validators.LanguageValidator
    node = %{attributes: %{"lang" => "en", "xml:lang" => "fr"}}
    assert LanguageValidator.language_attribute(node) == {:ok, "en"}
    assert LanguageValidator.language_attribute(%{attributes: %{"xml:lang" => "fr"}}) == :inherit

    node =
      Map.put(node, :attribute_namespaces, %{"xml:lang" => "http://www.w3.org/XML/1998/namespace"})

    assert LanguageValidator.language_attribute(node) == {:ok, "fr"}
    assert LanguageValidator.document_default(%{children: []}) == ""
  end

  test "heading context adds inclusive nonnegative offsets and resets by attribute presence" do
    for {value, own} <- [
          {"2", 2},
          {" +03tail", 3},
          {"3.5", 3},
          {"-0", 0},
          {"-2", 0},
          {"", 0},
          {"invalid", 0},
          {"\u00a02", 0},
          {"80", 80}
        ] do
      node = %{tag: "h2", attributes: %{"headingoffset" => value}}
      assert HtmlValidator.resolve_html_heading_context(node, 3) == {3 + own, min(5 + own, 9)}
    end

    for reset <- ["", "false", "headingreset", "0"] do
      node = %{tag: "h2", attributes: %{"headingoffset" => "1", "headingreset" => reset}}
      assert HtmlValidator.resolve_html_heading_context(node, 6) == {1, 3}
    end

    assert HtmlValidator.resolve_html_heading_context(%{tag: "h6", attributes: %{}}, 0) ==
             {0, nil}

    assert HtmlValidator.resolve_html_heading_context(%{tag: "div", attributes: %{}}, 4) ==
             {4, nil}

    assert HtmlValidator.resolve_html_heading_context(
             %{tag: "h2", attributes: %{"headingreset" => ""}},
             4
           ) == {0, nil}
  end

  test "foreign elements and namespaced attributes do not add or reset heading offsets" do
    node = %{tag: "h1", attributes: %{"headingoffset" => "4", "headingreset" => ""}}

    for namespace <- [:svg, :mathml] do
      assert HtmlValidator.resolve_html_heading_context(Map.put(node, :namespace, namespace), 2) ==
               {2, nil}
    end

    qualified =
      Map.put(node, :attribute_namespaces, %{
        "headingoffset" => "urn:example",
        "headingreset" => "urn:example"
      })

    assert HtmlValidator.resolve_html_heading_context(qualified, 2) == {2, 3}

    assert HtmlValidator.resolve_html_heading_context(Map.put(node, :namespace, :html), 2) ==
             {4, 5}
  end

  test "supplied styled trees validate heading levels through the shared diagnostic contract" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Layout}
    assert {:ok, dom} = HtmlParser.parse_detailed("<h2>Heading</h2>")
    assert {:ok, tree} = Style.compute_detailed(dom)

    for level <- [2, 6, 7, 9] do
      valid =
        update_in(tree, [:children, Access.at(0), :style], &Map.put(&1, :heading_level, level))

      assert {:ok, layout} = Layout.layout(valid)

      assert Enum.find(layout.boxes, &Map.has_key?(&1, :outline_anchor)).outline_anchor.level ==
               level
    end

    for level <- [nil, 0, 1, 10, 2.0, "2", :invalid] do
      invalid =
        update_in(tree, [:children, Access.at(0), :style], &Map.put(&1, :heading_level, level))

      assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(invalid)
      assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
      assert diagnostic.operation == :layout and diagnostic.source == "h2"
      assert diagnostic.message =~ "heading_level"
    end

    invalid =
      update_in(tree, [:children, Access.at(0)], fn node ->
        %{node | tag: "p", style: Map.put(node.style, :heading_level, 2)}
      end)

    assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(invalid)
    assert diagnostic.source == "p"
  end

  test "supplied wbr trees enforce void content and canonical soft-break layout" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}
    assert {:ok, dom} = HtmlParser.parse_detailed("<p>A<wbr>B</p>")

    invalid =
      update_in(dom, [:children, Access.at(0), :children, Access.at(1), :children], fn _ ->
        [%{type: :text, text: "Invalid"}]
      end)

    assert {:error, {:invalid_document, diagnostic}} =
             Style.compute_detailed(tree_content(invalid))

    assert diagnostic.stage == :style and diagnostic.operation == :compute
    assert diagnostic.source == "wbr" and diagnostic.message =~ "empty children"
    assert {:ok, tree} = Style.compute_detailed(dom)

    for change <- [
          %{tag: "span"},
          %{children: [%{type: :text, text: "Invalid", style: %{}}]},
          %{style: %{display: :soft_break, overflow_wrap: :invalid}}
        ] do
      invalid =
        update_in(
          tree,
          [:children, Access.at(0), :children, Access.at(1)],
          &Map.merge(&1, change)
        )

      assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(invalid)
      assert diagnostic.stage == :layout and diagnostic.operation == :layout
      assert is_binary(diagnostic.message) and diagnostic.message != ""
    end
  end

  test "advanced styled trees reject invalid wrapping values through shared diagnostics" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style, Layout}
    assert {:ok, dom} = HtmlParser.parse_detailed("<p>Text</p>")
    assert {:ok, tree} = Style.compute_detailed(dom)

    for {property, values} <- [
          line_break: [:normal, :anywhere, :break_word],
          overflow_wrap: [:normal, :anywhere, :break_word],
          word_break: [:normal, :break_word]
        ] do
      for value <- values do
        valid = update_in(tree, [:children, Access.at(0), :style], &Map.put(&1, property, value))
        assert {:ok, _} = Layout.layout(valid)
      end

      for value <- [nil, :invalid, "normal", [], %{}] do
        invalid =
          update_in(tree, [:children, Access.at(0), :style], &Map.put(&1, property, value))

        assert {:error, {:invalid_layout, diagnostic}} = Layout.layout(invalid)
        assert diagnostic.stage == :layout and diagnostic.reason == :invalid_layout
        assert diagnostic.operation == :layout and is_binary(diagnostic.message)
      end
    end
  end

  test "wrapping keywords distinguish unconditional and emergency opportunities" do
    assert HtmlValidator.parse_css_text_wrapping("line-break", "anywhere") ==
             {:ok, :line_break, :anywhere}

    assert HtmlValidator.parse_css_text_wrapping("line-break", "AUTO") ==
             {:ok, :line_break, :normal}

    for property <- ["overflow-wrap", "word-wrap"] do
      assert HtmlValidator.parse_css_text_wrapping(property, "anywhere") ==
               {:ok, :overflow_wrap, :anywhere}

      assert HtmlValidator.parse_css_text_wrapping(property, "break-word") ==
               {:ok, :overflow_wrap, :break_word}

      assert HtmlValidator.parse_css_text_wrapping(property, "normal") ==
               {:ok, :overflow_wrap, :normal}
    end

    assert HtmlValidator.parse_css_text_wrapping("word-break", "break-word") ==
             {:ok, :word_break, :break_word}

    assert HtmlValidator.parse_css_text_wrapping("word-break", "normal") ==
             {:ok, :word_break, :normal}

    for {property, value} <- [
          {"word-break", "anywhere"},
          {"line-break", "break-word"},
          {"overflow-wrap", "auto"}
        ] do
      assert HtmlValidator.parse_css_text_wrapping(property, value) == :error
    end
  end

  test "modification containers use transparent ancestry and highlight markup remains phrasing" do
    for tag <- ["mark", "ins", "del", "strike"],
        do: assert(HtmlValidator.supported_html_tag?(tag))

    for tag <- ["ins", "del"] do
      assert HtmlValidator.valid_html_child?(tag, "div", ["body"])
      refute HtmlValidator.valid_html_child?(tag, "div", ["p", "body"])
      assert HtmlValidator.valid_html_child?(tag, "span", ["p", "body"])
      refute HtmlValidator.valid_html_child?("ul", tag, ["body"])
      refute HtmlValidator.valid_html_child?(tag, "figcaption", ["figure", "body"])
    end

    refute HtmlValidator.valid_html_child?("mark", "div", ["body"])
    refute HtmlValidator.valid_html_child?("strike", "div", ["body"])
  end

  test "inert presence and ancestry use shared boolean state without changing enabled semantics" do
    refute HtmlValidator.html_inert?(%{}, false)
    assert HtmlValidator.html_inert?(%{}, true)

    for value <- ["", "inert", "false", "anything"] do
      assert HtmlValidator.html_inert?(%{"inert" => value}, false)
    end

    assert :ok = HtmlValidator.validate_html_element_attributes("x-box", %{"inert" => "false"})
  end

  test "hidden keywords use missing empty invalid and exact ASCII enumerated defaults" do
    assert HtmlValidator.html_hidden_state(%{}) == :not_hidden

    for value <- ["", "hidden", "HIDDEN", "false", "anything", " until-found "] do
      assert HtmlValidator.html_hidden_state(%{"hidden" => value}) == :hidden
      assert :ok = HtmlValidator.validate_html_element_attributes("div", %{"hidden" => value})
    end

    for value <- ["until-found", "UNTIL-FOUND", "Until-Found"] do
      assert HtmlValidator.html_hidden_state(%{"hidden" => value}) == :until_found
    end
  end

  test "custom name validation uses fixed HTML name and reserved vocabulary rules" do
    for name <- ["x-", "x-É", "x-😀", "x-:colon", ~S|x-"quote|, "x-<angle", "x-=eq", "x-@mark"] do
      assert HtmlValidator.custom_html_tag?(name)
      assert HtmlValidator.supported_html_tag?(name)
    end

    for name <- [
          nil,
          12,
          "",
          "span",
          "nohyphen",
          "-x",
          "X-name",
          "x-Name",
          "x-/",
          "x->",
          "x- ",
          "x-\0",
          <<"x-", 255>>,
          "annotation-xml",
          "color-profile",
          "font-face",
          "font-face-src",
          "font-face-uri",
          "font-face-format",
          "font-face-name",
          "missing-glyph"
        ] do
      refute HtmlValidator.custom_html_tag?(name)
    end

    assert HtmlValidator.valid_html_child?("x-box", "div", ["body"])
    refute HtmlValidator.valid_html_child?("x-box", "div", ["p", "body"])
    refute HtmlValidator.valid_html_child?("x-box", "figcaption", ["figure", "body"])
    assert HtmlValidator.valid_html_child?("x-box", "span", [])

    refute HtmlValidator.valid_form_element?("button", %{}, [
             %{type: :comment, text: "invalid DOM child"}
           ])

    assert HtmlValidator.valid_form_element?("button", %{}, [
             %{
               type: :element,
               tag: "x-label",
               attributes: %{},
               children: [%{type: :text, text: "Label"}]
             }
           ])

    for attribute <- ~w(dir) do
      assert {:error, {:unsupported_html, diagnostic}} =
               HtmlValidator.validate_html_element_attributes("x-box", %{attribute => ""})

      assert diagnostic.stage == :html
      assert diagnostic.source == "x-box"
      assert diagnostic.message =~ attribute
    end
  end

  test "CSS identifier helpers preserve escape boundaries and fixed grammar" do
    assert {:ok, "café", ".next"} = HtmlValidator.parse_css_identifier(~S|\63 afé.next|)
    assert {:ok, "c\u0301", ""} = HtmlValidator.parse_css_identifier("c\u0301")
    assert HtmlValidator.parse_css_identifier("-1") == :error
    assert HtmlValidator.parse_css_identifier("\\\n") == :error
    assert HtmlValidator.parse_css_identifier("") == :error
    assert HtmlValidator.css_syntax_units(~S|a\2c b,c|) == ["a", ~S|\2c |, "b", ",", "c"]
    assert HtmlValidator.trim_css_whitespace(" \t.a\\ \r\n") == ".a\\ "
    assert HtmlValidator.trim_css_whitespace(" \u00A0 ") == "\u00A0"
    assert HtmlValidator.trim_css_whitespace("") == ""

    assert HtmlValidator.html_class_tokens("a\u00A0b c\t d\nf\rf\fg") == [
             "a\u00A0b",
             "c",
             "d",
             "f",
             "f",
             "g"
           ]

    assert HtmlValidator.parse_css_pseudo_element("not-a-pseudo") == :error
    assert HtmlValidator.parse_css_pseudo_element("::") == :error
    assert HtmlValidator.parse_css_pseudo_element("::missing") == :error
    assert HtmlValidator.parse_css_pseudo_element("::before.extra") == :error
    assert {:ok, :after} = HtmlValidator.parse_css_pseudo_element(~S|::\61 fter|)
    assert HtmlValidator.parse_css_negated_selector(":other(.name)") == nil
    assert HtmlValidator.html_class_tokens("") == []
    assert Regex.match?(Regex.compile!(HtmlValidator.css_opaque_pattern(), "u"), ~S|\@media|)
    assert Regex.match?(Regex.compile!(HtmlValidator.css_opaque_pattern(), "u"), ~S|"\\text"|)
  end

  test "anonymous inline validation handles direct trees without losing positioned children" do
    block = %{type: :element, tag: "div", style: %{display: :block}, children: []}
    inline = %{type: :element, tag: "span", style: %{display: :inline}, children: [block]}

    validate = fn node ->
      HtmlValidator.validate_layout_input(
        %{type: :document, children: [node]},
        [],
        {:ok, {100.0, 100.0}},
        {:ok, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}}
      )
    end

    assert :ok = validate.(inline)
    assert :ok = validate.(%{inline | children: []})

    for invalid <- [
          nil,
          false,
          %{left: true},
          %{left: true, right: 1},
          %{left: true, right: false, top: true}
        ] do
      altered = %{block | style: Map.put(block.style, :margin_inline_auto, invalid)}
      assert {:error, {:invalid_layout, diagnostic}} = validate.(altered)
      assert diagnostic.stage == :layout and diagnostic.operation == :layout
      assert diagnostic.source == "div"
      assert diagnostic.message =~ "boolean left and right"
    end

    for auto <- [%{left: true, right: true}, %{left: false, right: false}] do
      assert :ok = validate.(%{block | style: Map.put(block.style, :margin_inline_auto, auto)})
    end

    for altered <- [
          Map.put(inline, :positioned_children, [block]),
          %{inline | style: Map.put(inline.style, :padding, :invalid)},
          %{
            inline
            | style:
                Map.put(inline.style, :border_widths, %{top: 1, right: 0, bottom: 0, left: 0})
          }
        ] do
      assert {:error, {:invalid_layout, %{stage: :layout, operation: :layout, source: "span"}}} =
               validate.(altered)
    end

    image = %{
      type: :element,
      tag: "img",
      style: %{display: :image, position: :absolute, image: %{width: 1.0, height: 1.0}},
      children: []
    }

    assert :ok = validate.(%{inline | children: [image]})

    assert {:error, :invalid_layout} =
             validate.(%{
               inline
               | children: [%{image | style: %{display: :image, _text_decorations: nil}}]
             })

    assert {:error, :invalid_layout} = validate.(%{inline | children: nil})
    assert {:error, :invalid_layout} = validate.(%{inline | style: nil})
  end

  test "noscript validator rejects fragment nodes and preserves contextual content rules" do
    alias NativeElixirPdfUtilities.Validators.ForeignContentValidator
    text = %{type: :text, text: "Fallback"}
    noscript = %{namespace: :html, tag: "noscript", attributes: %{}}

    style = %{
      type: :element,
      tag: "style",
      attributes: %{},
      children: [%{type: :text, text: "p{color:red}"}]
    }

    assert HtmlValidator.valid_html_element?("noscript", %{}, [text])

    refute HtmlValidator.valid_html_element?("noscript", %{}, [
             %{type: :document_fragment, children: [text]}
           ])

    assert ForeignContentValidator.valid_element?(noscript, [text])
    assert ForeignContentValidator.valid_element?(noscript, [style], "head")
    refute ForeignContentValidator.valid_element?(noscript, [text], "head", [])
    assert HtmlValidator.valid_noscript_element?([%{type: :text, text: " \t\r\n"}], "head", [])

    refute HtmlValidator.valid_noscript_element?(
             [%{type: :element, tag: "noscript", attributes: %{}, children: [text]}],
             "body",
             []
           )
  end

  test "script kinds use exact ASCII MIME essences and legacy language defaults" do
    alias NativeElixirPdfUtilities.Validators.MimeValidator

    essences =
      ~w(application/ecmascript application/javascript application/x-ecmascript application/x-javascript text/ecmascript text/javascript text/javascript1.0 text/javascript1.1 text/javascript1.2 text/javascript1.3 text/javascript1.4 text/javascript1.5 text/jscript text/livescript text/x-ecmascript text/x-javascript)

    for essence <- essences do
      assert MimeValidator.javascript_essence_match?(String.upcase(essence))

      assert HtmlValidator.script_type(%{"type" => " \t" <> String.upcase(essence) <> "\r\n"}) ==
               :classic

      refute MimeValidator.javascript_essence_match?(essence <> ";charset=utf-8")
      assert HtmlValidator.script_type(%{"type" => essence <> ";charset=utf-8"}) == :data
    end

    for attrs <- [
          %{},
          %{"type" => ""},
          %{"language" => ""},
          %{"language" => "JAVASCRIPT"},
          %{"type" => "", "language" => "json"}
        ] do
      assert HtmlValidator.script_type(attrs) == :classic
    end

    for {type, expected} <- [
          {" MoDuLe ", :module},
          {"IMPORTMAP", :importmap},
          {"speculationrules", :speculationrules},
          {"text/javascript", :classic},
          {"   ", :data},
          {"application/json", :data},
          {"module;charset=utf-8", :data},
          {"text/javaſcript", :data},
          {"\u00A0module\u00A0", :data},
          {String.duplicate("x", 100_000), :data}
        ] do
      assert HtmlValidator.script_type(%{"type" => type}) == expected
    end

    assert HtmlValidator.script_type(%{"language" => "json"}) == :data
    assert HtmlValidator.script_type(%{"language" => " javascript "}) == :data

    assert :ok ==
             HtmlValidator.validate_html_element_attributes("script", %{
               "type" => "application/json",
               "src" => "https://invalid.test/data.json"
             })

    for attributes <- [
          %{},
          %{"type" => "module"},
          %{"type" => "importmap"},
          %{"type" => "speculationrules"},
          %{"type" => "text/plain", "onclick" => "x"},
          %{"type" => "text/plain", "unknown" => "x"}
        ] do
      assert {:error,
              {:unsupported_html, %{stage: :html, reason: :unsupported_html, message: message}}} =
               HtmlValidator.validate_html_element_attributes("script", attributes)

      assert message =~ "script"
    end
  end

  test "attribute operators honor default, explicit and ASCII-only case comparisons" do
    for {operator, actual, query} <- [
          {:equals, "SeArCh", "search"},
          {:includes, "one\tSeArCh\nlast", "search"},
          {:dash_match, "SeArCh-US", "search"},
          {:prefix, "SeArCh-us", "sea"},
          {:suffix, "us-SeArCh", "rch"},
          {:substring, "pre-SeArCh-post", "ear"}
        ] do
      node = %{attributes: %{"type" => actual, "data-kind" => actual}}
      assert HtmlValidator.attribute_selector_matches?({operator, "type", query}, node)

      refute HtmlValidator.attribute_selector_matches?(
               {operator, "type", query, :sensitive},
               node
             )

      refute HtmlValidator.attribute_selector_matches?({operator, "data-kind", query}, node)

      assert HtmlValidator.attribute_selector_matches?(
               {operator, "data-kind", query, :insensitive},
               node
             )

      refute HtmlValidator.attribute_selector_matches?(
               {operator, "missing", query, :insensitive},
               node
             )

      for namespace <- [:svg, :mathml] do
        foreign = Map.put(node, :namespace, namespace)
        refute HtmlValidator.attribute_selector_matches?({operator, "type", query}, foreign)

        assert HtmlValidator.attribute_selector_matches?(
                 {operator, "type", query, :insensitive},
                 foreign
               )
      end
    end

    for operator <- [:includes, :prefix, :suffix, :substring], actual <- ["", "abc", " abc "] do
      refute HtmlValidator.attribute_selector_matches?({operator, "title", "", :insensitive}, %{
               attributes: %{"title" => actual}
             })
    end

    for separator <- [" ", "\t", "\r", "\n", "\f"] do
      assert HtmlValidator.attribute_selector_matches?({:includes, "title", "two"}, %{
               attributes: %{"title" => "one" <> separator <> "two"}
             })

      refute HtmlValidator.attribute_selector_matches?(
               {:includes, "title", "one" <> separator <> "two"},
               %{attributes: %{"title" => "one" <> separator <> "two"}}
             )
    end

    for separator <- ["\v", "\u00a0", "\u2003"] do
      refute HtmlValidator.attribute_selector_matches?({:includes, "title", "two"}, %{
               attributes: %{"title" => "one" <> separator <> "two"}
             })

      assert HtmlValidator.attribute_selector_matches?(
               {:includes, "title", "one" <> separator <> "two"},
               %{attributes: %{"title" => "one" <> separator <> "two"}}
             )
    end

    for {actual, expected} <- [
          {"en", true},
          {"en-US", true},
          {"english", false},
          {"en_US", false},
          {"", false}
        ] do
      assert HtmlValidator.attribute_selector_matches?({:dash_match, "title", "en"}, %{
               attributes: %{"title" => actual}
             }) == expected
    end

    for {actual, expected} <- [{"", true}, {"-en", true}, {"en", false}] do
      assert HtmlValidator.attribute_selector_matches?({:dash_match, "title", ""}, %{
               attributes: %{"title" => actual}
             }) == expected
    end

    for {operator, actual, query} <- [
          {:equals, "Ö", "ö"},
          {:prefix, "Kelvin", "k"},
          {:suffix, "SS", "ß"},
          {:substring, "ÖA", "ö"},
          {:includes, "Ö A", "ö"}
        ] do
      refute HtmlValidator.attribute_selector_matches?(
               {operator, "title", query, :insensitive},
               %{attributes: %{"title" => actual}}
             )
    end

    assert HtmlValidator.attribute_selector_matches?({:equals, "type", "SEARCH", :sensitive}, %{
             attributes: %{"type" => "SEARCH"}
           })

    assert {:ok, {:equals, "title", "a'b", :sensitive}, ".note"} =
             HtmlValidator.parse_css_attribute_selector(~S|[title='a\'b' S].note|)

    assert {:ok, {:present, "title"}, ""} = HtmlValidator.parse_css_attribute_selector("[title]")
    assert :error = HtmlValidator.parse_css_attribute_selector("[title!='x']")
  end

  test "HTML attribute selectors fold only the fixed ASCII-insensitive value list" do
    names =
      ~w(accept accept-charset align alink axis bgcolor charset checked clear codetype color compact declare defer dir direction disabled enctype face frame hreflang http-equiv lang language link media method multiple nohref noresize noshade nowrap readonly rel rev rules scope scrolling selected shape target text type valign valuetype vlink)

    for name <- names do
      node = %{attributes: %{name => "AbCÖ"}}
      assert HtmlValidator.attribute_selector_matches?({:equals, name, "aBcÖ"}, node)
      refute HtmlValidator.attribute_selector_matches?({:equals, name, "abcö"}, node)
      refute HtmlValidator.attribute_selector_matches?({:equals, name, " ABCÖ "}, node)
      assert node.attributes[name] == "AbCÖ"

      for namespace <- [:svg, :mathml] do
        refute HtmlValidator.attribute_selector_matches?(
                 {:equals, name, "aBcÖ"},
                 Map.put(node, :namespace, namespace)
               )

        assert HtmlValidator.attribute_selector_matches?(
                 {:equals, name, "AbCÖ"},
                 Map.put(node, :namespace, namespace)
               )
      end

      refute HtmlValidator.attribute_selector_matches?(
               {:equals, name, "aBcÖ"},
               Map.put(node, :attribute_namespaces, %{name => "urn:example"})
             )
    end

    for name <- ["id", "class", "value", "name", "title", "data-kind", "role", "xlink:type"] do
      node = %{attributes: %{name => "ABC"}}
      refute HtmlValidator.attribute_selector_matches?({:equals, name, "abc"}, node)
      assert HtmlValidator.attribute_selector_matches?({:equals, name, "ABC"}, node)
    end

    for value <- ["", "ABC", "K", "K", "ß", "SS"] do
      node = %{namespace: :html, attributes: %{"type" => value}}
      assert HtmlValidator.attribute_selector_matches?({:present, "type"}, node)
      assert HtmlValidator.attribute_selector_matches?({:equals, "type", value}, node)
      refute HtmlValidator.attribute_selector_matches?({:present, "missing"}, node)
      refute HtmlValidator.attribute_selector_matches?({:equals, "missing", ""}, node)
    end

    refute HtmlValidator.attribute_selector_matches?({:equals, "type", "K"}, %{
             attributes: %{"type" => "K"}
           })

    refute HtmlValidator.attribute_selector_matches?({:equals, "type", "SS"}, %{
             attributes: %{"type" => "ß"}
           })
  end

  test "text and search input states strip only CR and LF while preserving authored attributes" do
    value = " \t\v\fα\u2028\u2029 \rA\nB\r\nC "
    expected = " \t\v\fα\u2028\u2029 ABC "

    for attributes <- [
          %{"value" => value},
          %{"type" => "text", "value" => value},
          %{"type" => "TEXT", "value" => value},
          %{"type" => "search", "value" => value},
          %{"type" => "SeArCh", "value" => value}
        ] do
      assert HtmlValidator.prepare_input_attributes(attributes)["value"] == expected
      assert attributes["value"] == value
    end

    for attributes <- [%{}, %{"type" => "text"}, %{"value" => ""}, %{"value" => "Plain"}] do
      assert HtmlValidator.prepare_input_attributes(attributes) == attributes
    end

    for type <- ["hidden", "checkbox", "radio", "button", "reset", "submit"] do
      attributes = %{"type" => type, "value" => value}
      assert HtmlValidator.prepare_input_attributes(attributes) == attributes
    end
  end

  test "decoration properties keep validation in one place" do
    assert :error = HtmlValidator.parse_css_text_decoration("unsupported", "solid", 12, {0, 0, 0})

    assert {:ok, %{text_decoration_style: :solid}} =
             HtmlValidator.parse_css_text_decoration(
               "text-decoration-style",
               "solid",
               12,
               {0, 0, 0}
             )

    assert {:ok, %{text_decoration_thickness: :auto}} =
             HtmlValidator.parse_css_text_decoration(
               "text-decoration-thickness",
               "auto",
               12,
               {0, 0, 0}
             )

    assert {:ok, %{text_decoration_color: :current_color}} =
             HtmlValidator.parse_css_text_decoration(
               "text-decoration-color",
               "currentColor",
               12,
               {0, 0, 0}
             )
  end

  test "metadata selection preserves legacy trees and empty titles" do
    text = %{type: :text, text: "  Legacy\t title  "}
    title = %{type: :element, tag: "title", children: [text]}

    assert [metadata: %{title: "Legacy title"}] =
             HtmlValidator.document_metadata_options(%{type: :document, children: [title]}, [])

    assert [] = HtmlValidator.document_metadata_options(%{type: :document, children: []}, [])

    assert [] =
             HtmlValidator.document_metadata_options(
               %{type: :document, children: [%{title | children: []}]},
               []
             )

    assert [] =
             HtmlValidator.document_metadata_options(
               %{type: :document, children: [%{type: :text, text: "Body"}]},
               []
             )
  end

  test "CSS number validation bounds operands and converted units" do
    assert {72.0, ""} = HtmlValidator.parse_css_number("1", 72.0)
    assert {100.0, "pt"} = HtmlValidator.parse_css_number("+1e2pt")
    assert {1.0, "."} = HtmlValidator.parse_css_number("1.")
    assert {-0.5, "%"} = HtmlValidator.parse_css_number("-0.5%")
    assert {1_000_000_000.0, ""} = HtmlValidator.parse_css_number("1000000000")

    for {value, scale} <- [
          {"1000000001", 1},
          {"1000000000", 72},
          {"1", 1_000_000_001},
          {String.duplicate("9", 400), 1},
          {"invalid", 1},
          {123, 1},
          {"1", :invalid}
        ] do
      assert :error = HtmlValidator.parse_css_number(value, scale)
    end
  end

  test "CSS leading decimals share length, sign, and magnitude validation" do
    for {source, expected, rest} <- [
          {".5px", 0.5, "px"},
          {"-.5pt", -0.5, "pt"},
          {"+.5em", 0.5, "em"},
          {".5e2%", 50.0, "%"}
        ] do
      assert {^expected, ^rest} = HtmlValidator.parse_css_number(source)
    end

    for {unit, expected} <- [
          {"px", 0.375},
          {"pt", 0.5},
          {"mm", 36 / 25.4},
          {"cm", 36 / 2.54},
          {"in", 36.0}
        ] do
      assert {:ok, ^expected} = HtmlValidator.parse_css_length(".5" <> unit, :nonnegative)
    end

    assert {:ok, {:rem, 0.5}} = HtmlValidator.parse_css_length(".5rem", :nonnegative)
    assert {:ok, -0.5} = HtmlValidator.parse_css_length("-.5pt", :margin)
    assert {:ok, 5.0} = HtmlValidator.parse_css_length(".5em", :margin, 10)
    assert HtmlValidator.parse_css_length("-0", :nonnegative) == {:ok, 0.0}

    for value <- [".px", "-.pt", ".5", "1.", ".5unknown", "-.5pt", ".5em", "1000000000in", nil] do
      assert :error = HtmlValidator.parse_css_length(value, :nonnegative)
    end

    assert :error = HtmlValidator.parse_css_length(".5em", :margin, 1_000_000_001)
  end

  test "SVG authorization permits literal fragments and rejects entity declarations" do
    svg =
      ~s(<svg width="1" height="1"><defs><rect id="pixel" width="1" height="1"/></defs><use href="#pixel"/></svg>)

    assert {:ok, [width: 1, height: 1]} = HtmlValidator.validate_svg_raster(svg, [])

    for source <- [
          ~s(<!DOCTYPE svg [<!ENTITY resource "/tmp/private.png">]>#{svg}),
          ~s(<svg width="1" height="1"><image href="#private.png"/></svg>),
          ~s(<svg width="1" height="1"><s:image xmlns:s="http://www.w3.org/2000/svg" href="#private.png"/></svg>),
          ~s(<svg width="1" height="1"><filter><feImage href="#private.png"/></filter></svg>),
          ~s(<svg width="1" height="1"><use href="/tmp/private.svg#shape"/></svg>),
          ~s(<svg width="1" height="1"><image href = "/tmp/private.png"/></svg>),
          ~s(<svg width="1" height="1"><image href=" &amp;resource;"/></svg>)
        ] do
      assert {:error, {:invalid_document, %{stage: :style, message: message}}} =
               HtmlValidator.validate_svg_raster(source, [])

      assert message =~ "SVG resource references are not authorized"
    end
  end

  test "source, link, furniture-fit, and styled-tree rules are validator owned" do
    assert {:ok, %{"class" => "copy", "data-note" => "plain"}, false} =
             HtmlValidator.validate_html_attributes(" class=copy data-note='plain'", "p")

    assert {:ok, %{"xml:lang" => "en"}, false} =
             HtmlValidator.validate_html_attributes(" xml:lang=en", "p")

    assert {:ok, "<p>Hello</p>"} = HtmlValidator.validate_html_source("<p>Hello</p>")
    assert {:error, {:invalid_html, %{stage: :html}}} = HtmlValidator.validate_html_source(nil)

    assert {:ok, "p { color: red }"} =
             HtmlValidator.validate_css_source("p { color: red }")

    assert {:error, {:invalid_css, %{stage: :css, message: "CSS input must be valid UTF-8"}}} =
             HtmlValidator.validate_css_source(<<255>>)

    assert {:error, {:invalid_css, %{stage: :css}}} =
             HtmlValidator.validate_css_source(nil, :declarations)

    assert HtmlValidator.valid_link_url?("https://example.com/report")
    refute HtmlValidator.valid_link_url?("javascript:alert(1)")
    refute HtmlValidator.valid_link_url?(nil)

    Enum.each(~w(row col rowgroup colgroup COL), fn scope ->
      assert HtmlValidator.valid_table_header_scope?(scope)
    end)

    refute HtmlValidator.valid_table_header_scope?("column")
    refute HtmlValidator.valid_table_header_scope?(nil)

    assert {:ok, 1} = HtmlValidator.validate_table_span_attribute(%{}, "colspan")

    assert {:ok, 1_000} =
             HtmlValidator.validate_table_span_attribute(%{"rowspan" => " 1000 "}, "rowspan")

    assert {:ok, 1} = HtmlValidator.validate_table_span_attribute(%{"span" => "0"}, "span")
    assert {:ok, 1} = HtmlValidator.validate_table_span_attribute(%{"span" => "many"}, "span")
    assert :error = HtmlValidator.validate_table_span_attribute(%{"span" => 2}, "span")
    assert :error = HtmlValidator.validate_table_span_attribute(:bad, "span")
    assert :error = HtmlValidator.validate_table_span_attribute(%{}, "width")

    for kind <- [:grid_placement, :grid_tracks, :table_span] do
      assert :ok = HtmlValidator.validate_layout_cardinality(kind, 1_000)

      assert {:error,
              {:resource_limit_exceeded,
               %{stage: :limits, reason: :resource_limit_exceeded, message: message}}} =
               HtmlValidator.validate_layout_cardinality(kind, 1_001)

      assert message =~ "1000-item limit"
    end

    assert :error = HtmlValidator.validate_layout_cardinality(:grid_tracks, 0)
    assert :error = HtmlValidator.validate_layout_cardinality(:unsupported, 1)

    assert :ok = HtmlValidator.validate_background_image_tile_count(10_000)

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :limits, reason: :resource_limit_exceeded, message: tile_message}}} =
             HtmlValidator.validate_background_image_tile_count(10_001)

    assert tile_message == "background image tile count exceeds the 10000-tile limit"
    assert :error = HtmlValidator.validate_background_image_tile_count(0)

    assert :ok =
             HtmlValidator.validate_background_image_tile_dimensions(
               5.0,
               5.0,
               30.0,
               30.0,
               :repeat
             )

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlValidator.validate_background_image_tile_dimensions(
               0.000_001,
               0.000_001,
               30.0,
               30.0,
               :repeat
             )

    assert :error =
             HtmlValidator.validate_background_image_tile_dimensions(
               :invalid,
               5.0,
               30.0,
               30.0,
               :repeat
             )

    assert :ok = HtmlValidator.validate_furniture_fit(:header, 12.0, 12.0)

    assert {:error, {:invalid_layout, %{stage: :layout}}} =
             HtmlValidator.validate_furniture_fit(:footer, 12.0, 10.0)

    assert {:error, {:invalid_layout, %{stage: :layout}}} =
             HtmlValidator.validate_furniture_fit(:sidebar, :bad, :bad)

    assert HtmlValidator.validate_layout_input(
             %{type: :document, children: [%{type: :text, text: :invalid, style: %{}}]},
             [],
             {:ok, {100.0, 100.0}},
             {:ok, %{top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}}
           ) == {:error, :invalid_layout}

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_fallback_input(
               %{
                 type: :document,
                 children: [
                   %{type: :element, children: [%{type: :text, text: "Missing", style: %{}}]}
                 ]
               },
               :replace
             )

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_coverage(:not_a_document, :replace, "\uFFFD")

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_coverage(
               %{
                 type: :document,
                 children: [%{type: :element, children: [%{type: :invalid}]}]
               },
               :replace,
               "\uFFFD"
             )

    assert {:error, {:invalid_document, %{stage: :font}}} =
             HtmlValidator.validate_font_coverage(
               %{
                 type: :document,
                 children: [
                   %{type: :text, _font_candidates: [%{}], _font_graphemes: [:invalid]}
                 ]
               },
               :replace,
               "\uFFFD"
             )
  end

  test "local document resource paths reject traversal and existing symlinks" do
    fixture_dir = Path.join(System.tmp_dir!(), "native-elixir-pdf-resource-validator")
    resource_root = Path.join(fixture_dir, "root")
    inside_path = Path.join(resource_root, "inside.png")
    outside_path = Path.join(fixture_dir, "outside.png")
    outside_directory = Path.join(fixture_dir, "outside-directory")
    symlink_path = Path.join(resource_root, "linked.png")
    directory_symlink_path = Path.join(resource_root, "linked-directory")
    File.mkdir_p!(resource_root)
    File.mkdir_p!(outside_directory)
    File.write!(inside_path, "inside")
    File.write!(outside_path, "outside")
    File.write!(Path.join(outside_directory, "nested.png"), "outside")
    :ok = File.ln_s(outside_path, symlink_path)
    :ok = File.ln_s(outside_directory, directory_symlink_path)

    assert {:ok, ^inside_path} =
             HtmlValidator.validate_local_resource_path("inside.png", resource_root)

    assert {:ok, ^inside_path} =
             HtmlValidator.validate_local_resource_path(inside_path, resource_root)

    assert {:ok, ^inside_path} =
             HtmlValidator.validate_local_resource_path(
               "inside.png",
               "file://localhost#{resource_root}"
             )

    assert {:ok, ^resource_root} =
             HtmlValidator.validate_local_resource_path(".", resource_root)

    for relative <- ["linked.png", "linked-directory/nested.png", "missing.png"] do
      assert {:error, {:invalid_document, _}} =
               HtmlValidator.validate_local_resource_path(relative, resource_root)
    end

    for {source, base_url} <- [
          {outside_path, resource_root},
          {"../outside.png", resource_root},
          {"inside.png", nil},
          {"inside.png", "https://example.com"},
          {"inside.png", "file://remote#{resource_root}"},
          {"bad\0name.png", resource_root},
          {"https://example.com/image.png", resource_root},
          {:invalid, resource_root}
        ] do
      assert {:error,
              {:invalid_document,
               %{
                 stage: :style,
                 reason: :invalid_document,
                 message: "local document resource path is not authorized by base_url"
               }}} = HtmlValidator.validate_local_resource_path(source, base_url)
    end
  after
    File.rm_rf(Path.join(System.tmp_dir!(), "native-elixir-pdf-resource-validator"))
  end

  test "image resource budgets bound count, source bytes, and retained decoded bytes" do
    count_budget = HtmlValidator.new_image_budget()

    for _image <- 1..1_000 do
      assert :ok = HtmlValidator.reserve_image_source(count_budget, 0)
    end

    assert {:error, {:resource_limit_exceeded, count_diagnostic}} =
             HtmlValidator.reserve_image_source(count_budget, 0)

    assert count_diagnostic.stage == :limits
    assert count_diagnostic.message == "image count exceeds the limit"

    source_budget = HtmlValidator.new_image_budget()

    for _image <- 1..5 do
      assert :ok = HtmlValidator.reserve_image_source(source_budget, 10_000_000)
    end

    assert {:error, {:resource_limit_exceeded, source_diagnostic}} =
             HtmlValidator.reserve_image_source(source_budget, 1)

    assert source_diagnostic.stage == :limits
    assert source_diagnostic.message == "aggregate image source bytes exceed the limit"

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlValidator.reserve_image_source(HtmlValidator.new_image_budget(), 10_000_001)

    decoded_budget = HtmlValidator.new_image_budget()

    for _image <- 1..20 do
      assert :ok = HtmlValidator.reserve_decoded_image(decoded_budget, 1_000, 1_000, 4)
    end

    assert {:error, {:resource_limit_exceeded, decoded_diagnostic}} =
             HtmlValidator.reserve_decoded_image(decoded_budget, 1, 1, 1)

    assert decoded_diagnostic.stage == :limits
    assert decoded_diagnostic.message == "aggregate decoded image bytes exceed the limit"

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlValidator.reserve_decoded_image(
               HtmlValidator.new_image_budget(),
               10_000,
               2_000,
               3
             )

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.reserve_image_source(HtmlValidator.new_image_budget(), -1)

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.reserve_decoded_image(HtmlValidator.new_image_budget(), 0, 1, 3)

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.reserve_image_source(make_ref(), 1)
  end

  test "render requests and paths are validated at the shared boundary" do
    assert :ok =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [margin: 10],
               Font.normalize_options(margin: 10)
             )

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [margni: 10],
               Font.normalize_options(margni: 10)
             )

    assert {:error, {:invalid_encoding, %{stage: :html}}} =
             HtmlValidator.validate_render_request(<<255>>, [], Font.normalize_options([]))

    assert :ok =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [unsupported_glyphs: :replace],
               Font.normalize_options(unsupported_glyphs: :replace)
             )

    assert :ok =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [system_font_discovery: false],
               Font.normalize_options(system_font_discovery: false)
             )

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               message: "system_font_discovery must be true or false"
             }}} =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [system_font_discovery: :sometimes],
               Font.normalize_options(system_font_discovery: :sometimes)
             )

    assert {:error,
            {:invalid_options,
             %{
               stage: :options,
               reason: :invalid_options,
               message: "unsupported_glyphs must be :replace or :error"
             }}} =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [unsupported_glyphs: :ignore],
               Font.normalize_options(unsupported_glyphs: :ignore)
             )

    assert {:ok, %{input_path: "input.html", output_path: "output.pdf"}} =
             HtmlValidator.validate_paths("input.html", "output.pdf")

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [base_url: 123],
               Font.normalize_options(base_url: 123)
             )

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [default_font: 123],
               Font.normalize_options(default_font: 123)
             )

    assert Style.load_stylesheets(%{type: :document, children: []}, stylesheets: [123]) ==
             {:error, :invalid_stylesheet_options}
  end

  test "asset maps, resolvers, and resolver results are validated at the shared boundary" do
    resolver = fn _request -> :not_found end

    assert :ok =
             HtmlValidator.validate_render_request(
               "<p>Hello</p>",
               [
                 assets: %{
                   "asset:logo" => {:bytes, <<1, 2, 3>>},
                   "asset:file" => {:file, "/tmp/logo.png"}
                 },
                 asset_resolver: resolver
               ],
               Font.normalize_options(
                 assets: %{
                   "asset:logo" => {:bytes, <<1, 2, 3>>},
                   "asset:file" => {:file, "/tmp/logo.png"}
                 },
                 asset_resolver: resolver
               )
             )

    for opts <- [
          [assets: []],
          [assets: %{1 => {:bytes, <<1>>}}],
          [assets: %{"asset:x" => <<1>>}],
          [assets: %{"asset:x" => {:file, ""}}],
          [asset_resolver: :not_a_function]
        ] do
      assert {:error, {:invalid_options, %{stage: :options}}} =
               HtmlValidator.validate_render_request(
                 "<p>Hello</p>",
                 opts,
                 Font.normalize_options(opts)
               )
    end

    assert {:ok, "bytes"} =
             HtmlValidator.validate_asset_resolver_result({:ok, "bytes"}, "asset:x")

    assert :not_found = HtmlValidator.validate_asset_resolver_result(:not_found, "asset:x")

    assert {:error, {:invalid_document, %{stage: :asset, source: "asset:x"}}} =
             HtmlValidator.validate_asset_resolver_result({:ok, 123}, "asset:x")
  end

  test "SVG raster budgets are validated before native rendering" do
    svg = ~s(<svg xmlns="http://www.w3.org/2000/svg" width="200" height="100"></svg>)

    assert {:ok, [width: 400, height: 200]} =
             HtmlValidator.validate_svg_raster(svg, width: 400)

    assert {:ok, [width: 200, height: 100]} =
             HtmlValidator.validate_svg_raster(svg, [])

    percentage_svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="50%" height="50%" viewBox="0 0 20 10"></svg>)

    assert {:ok, [width: 10, height: 5]} =
             HtmlValidator.validate_svg_raster(percentage_svg, [])

    for {unit, expected_width} <- [
          {"px", 1},
          {"pt", 1},
          {"pc", 16},
          {"mm", 4},
          {"cm", 38},
          {"in", 96},
          {"q", 1}
        ] do
      unit_svg =
        ~s(<svg xmlns="http://www.w3.org/2000/svg" width="1#{unit}" height="1"></svg>)

      assert {:ok, [width: ^expected_width, height: 1]} =
               HtmlValidator.validate_svg_raster(unit_svg, [])
    end

    height_sized_svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="2" height="1"></svg>)

    assert {:ok, [width: 20, height: 10]} =
             HtmlValidator.validate_svg_raster(height_sized_svg, height: 10)

    assert {:error,
            {:resource_limit_exceeded,
             %{
               stage: :limits,
               reason: :resource_limit_exceeded,
               message: dimension_message
             }}} = HtmlValidator.validate_svg_raster(svg, width: 8_193)

    assert dimension_message =~ "8192-pixel per-axis limit"

    extreme_intrinsic_svg =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="4294967295" height="1"></svg>)

    assert {:error, {:resource_limit_exceeded, %{stage: :limits}}} =
             HtmlValidator.validate_svg_raster(extreme_intrinsic_svg, [])

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :limits, reason: :resource_limit_exceeded, message: pixel_message}}} =
             HtmlValidator.validate_svg_raster(svg, width: 8_192, height: 4_096)

    assert pixel_message =~ "16777216-pixel limit"

    oversized_source =
      ~s(<svg xmlns="http://www.w3.org/2000/svg" width="1" height="1">) <>
        :binary.copy(" ", 5_000_000) <> "</svg>"

    assert {:error,
            {:resource_limit_exceeded,
             %{stage: :limits, reason: :resource_limit_exceeded, message: source_message}}} =
             HtmlValidator.validate_svg_raster(oversized_source, [])

    assert source_message =~ "5000000-byte limit"

    for invalid_svg <- [
          "not SVG",
          "<svg></svg>",
          ~s(<svg width="1" height="1" viewBox="0 0 nope 1"></svg>),
          ~s(<svg width="0" height="1"></svg>),
          ~s(<svg width="0px" height="1"></svg>),
          ~s(<svg width="auto" height="1"></svg>),
          ~s(<svg width="1e9999" height="1"></svg>)
        ] do
      assert {:error, {:invalid_document, %{stage: :style}}} =
               HtmlValidator.validate_svg_raster(invalid_svg, [])
    end

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.validate_svg_raster(svg, width: 0)

    assert {:error, {:invalid_document, %{stage: :style}}} =
             HtmlValidator.validate_svg_raster(:not_svg, [])
  end

  test "layout pagination and furniture inputs share normalized page geometry" do
    styled_tree = %{type: :document, children: []}

    assert {:ok, {792.0, 612.0} = page_size} =
             PageGeometry.normalize_page_size("letter landscape")

    assert :ok = HtmlValidator.validate_page_size(page_size)
    assert {:error, :invalid_page_size} = HtmlValidator.validate_page_size({0.0, 612.0})

    assert {:ok, canonical_margins} = PageGeometry.normalize_margins("5pt 10pt")
    assert :ok = HtmlValidator.validate_margins(canonical_margins)
    assert :ok = HtmlValidator.validate_printable_area(page_size, canonical_margins)

    assert {:error, :invalid_margin} =
             HtmlValidator.validate_printable_area({10.0, 10.0}, canonical_margins)

    assert {:error, :invalid_margin} =
             HtmlValidator.validate_margins(Map.put(canonical_margins, :extra, 0.0))

    assert {:ok, margins} = PageGeometry.normalize_margins("5pt 10pt")

    assert :ok =
             HtmlValidator.validate_layout_input(
               styled_tree,
               [page_size: {100, 80}, margin: "5pt 10pt"],
               PageGeometry.normalize_page_size({100, 80}),
               PageGeometry.normalize_margins("5pt 10pt")
             )

    assert margins == %{top: 5.0, right: 10.0, bottom: 5.0, left: 10.0}

    assert HtmlValidator.validate_layout_input(
             styled_tree,
             [:not_options],
             {:error, :invalid_page_size},
             {:error, :invalid_margin}
           ) ==
             {:error, :invalid_layout}

    layout_tree = %{
      type: :layout,
      page_size: {100.0, 80.0},
      margins: margins,
      boxes: []
    }

    assert :ok = HtmlValidator.validate_pagination_input(layout_tree, [], {:ok, margins})

    pages = [%{size: {100.0, 80.0}, boxes: []}]

    assert :ok =
             HtmlValidator.validate_furniture_input(
               pages,
               layout_tree,
               [page_furniture: [header: "Page {{page}}"]],
               {:ok, margins},
               PageFurniture.normalize_option(header: "Page {{page}}")
             )

    assert {:ok, canonical_furniture} =
             PageFurniture.normalize_option(header: "Page {{page}}")

    assert :ok = HtmlValidator.validate_furniture_option(canonical_furniture)

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlValidator.validate_furniture_option(%{header: %{}, footer: %{}, extra: %{}})

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlValidator.validate_furniture_input(
               pages,
               layout_tree,
               [],
               {:ok, margins},
               {:ok, %{header: %{}, footer: %{}, extra: %{}}}
             )

    assert {:error, {:invalid_options, %{stage: :options}}} =
             HtmlValidator.validate_furniture_input(
               [],
               %{},
               [:not_options],
               {:error, :invalid_margin},
               {:error, :invalid_furniture_container}
             )

    assert {:error, {:invalid_layout, %{stage: :layout}}} =
             HtmlValidator.validate_furniture_input(
               :not_pages,
               :not_layout,
               :not_options,
               {:error, :invalid_margin},
               {:error, :invalid_furniture_container}
             )
  end

  test "font configuration is normalized before the loader consumes it" do
    assert {:ok,
            [
              %{
                family: "Example Sans",
                path: ["/tmp/example.ttf"],
                weight: 700,
                style: :italic
              }
            ] = normalized} =
             Font.normalize_configs([
               [
                 family: "Example Sans",
                 path: "/tmp/example.ttf",
                 weight: "bold",
                 style: "italic"
               ]
             ])

    assert :ok = HtmlValidator.validate_font_configs(normalized)

    assert :error = Font.normalize_configs([[family: "Missing path"]])
    assert :error = Font.normalize_configs([[family: "Empty", path: ""]])

    assert :error =
             HtmlValidator.validate_font_configs([
               %{family: "Example", path: [], weight: 400, style: :normal}
             ])

    assert :error = HtmlValidator.validate_font_configs(:not_a_list)
    assert :error = HtmlValidator.validate_font_configs([%{unexpected: true}])

    assert :ok = HtmlValidator.validate_font_embedding("Legacy Sans", nil, false)

    assert {:error,
            {:invalid_document,
             %{
               stage: :font,
               source: "Variable Sans",
               message:
                 "font \"Variable Sans\" uses OpenType variations; provide a static font instance for PDF embedding"
             }}} = HtmlValidator.validate_font_embedding("Variable Sans", 0, true)
  end

  test "validates static form attributes and element semantics" do
    for pair <- [
          {"input", "checked"},
          {"input", "disabled"},
          {"select", "disabled"},
          {"option", "selected"},
          {"option", "disabled"},
          {"textarea", "disabled"},
          {"button", "disabled"}
        ] do
      assert apply(HtmlValidator, :valid_boolean_form_attribute?, Tuple.to_list(pair))
    end

    refute HtmlValidator.valid_boolean_form_attribute?("input", "value")

    for {tag, name, value} <- [
          {"input", "type", "RADIO"},
          {"input", "name", "answer"},
          {"select", "name", "status"},
          {"option", "value", "yes"},
          {"textarea", "value", "notes"},
          {"button", "type", "SUBMIT"},
          {"button", "value", "Save"}
        ] do
      assert HtmlValidator.valid_form_attribute?(tag, name, value)
    end

    assert HtmlValidator.valid_form_attribute?("input", "type", "range")
    assert HtmlValidator.valid_form_attribute?("select", "multiple", "multiple")

    text = %{type: :text, text: "Choice"}
    option = %{type: :element, tag: "option", attributes: %{}, children: [text]}
    selected = put_in(option.attributes, %{"selected" => ""})

    assert HtmlValidator.valid_form_element?("input", %{}, [])
    refute HtmlValidator.valid_form_element?("input", %{}, [text])
    assert HtmlValidator.valid_form_element?("select", %{}, [option, selected])
    separator = %{type: :element, tag: "hr", attributes: %{}, children: []}
    assert HtmlValidator.valid_form_element?("select", %{}, [option, separator, selected])
    refute HtmlValidator.valid_form_element?("select", %{}, [%{separator | children: [text]}])
    assert length(HtmlValidator.select_options([option, separator, selected])) == 2
    assert HtmlValidator.valid_form_element?("option", %{}, [text])
    assert HtmlValidator.valid_form_element?("textarea", %{}, [])
    assert HtmlValidator.valid_form_element?("button", %{}, [text])
    refute HtmlValidator.valid_form_element?("unknown", %{}, [])
  end

  test "readonly applicability belongs to the control validator" do
    assert HtmlValidator.valid_boolean_form_attribute?("input", "readonly")
    assert HtmlValidator.valid_boolean_form_attribute?("textarea", "readonly")

    for tag <- ["select", "option", "button"] do
      refute HtmlValidator.valid_boolean_form_attribute?(tag, "readonly")
      assert HtmlValidator.valid_form_attribute?(tag, "readonly", "")
    end

    for type <- ["text", "TEXT", "search", "SeArCh"] do
      assert HtmlValidator.valid_form_element?("input", %{"type" => type, "readonly" => ""}, [])

      assert HtmlValidator.valid_form_element?(
               "input",
               %{"type" => type, "readonly" => "", "disabled" => ""},
               []
             )
    end

    assert HtmlValidator.valid_form_element?("input", %{"readonly" => "false"}, [])

    for type <- ["hidden", "checkbox", "radio", "button", "reset", "submit"] do
      assert HtmlValidator.valid_form_element?("input", %{"type" => type, "readonly" => ""}, [])
    end

    assert HtmlValidator.valid_form_element?("textarea", %{"readonly" => ""}, [
             %{type: :text, text: "Notes"}
           ])

    for tag <- ["input", "textarea"], value <- ["", "readonly", "READONLY", "false"] do
      assert HtmlValidator.valid_form_attribute?(tag, "readonly", value)
    end
  end

  test "telephone URL and single email sanitation follows HTML ASCII whitespace" do
    for type <- ["tel", "TEL", "url", "URL", "email", "EmAiL"] do
      normalized = String.downcase(type)
      assert HtmlValidator.text_input_type?(normalized)
      assert HtmlValidator.valid_form_attribute?("input", "type", type)

      assert HtmlValidator.valid_form_element?(
               "input",
               %{"type" => type, "readonly" => "false"},
               []
             )

      assert HtmlValidator.valid_form_element?("input", %{"type" => type, "checked" => ""}, [])

      for {value, stripped, trimmed} <- [
            {" \t\f\rA B\nC \r\n\t", " \t\fA BC \t", "A BC"},
            {" \t\r\n\f", " \t\f", ""},
            {"\u00A0 A B \u00A0", "\u00A0 A B \u00A0", "\u00A0 A B \u00A0"},
            {"\v A\u2028B \v", "\v A\u2028B \v", "\v A\u2028B \v"}
          ] do
        attributes = %{"type" => type, "value" => value}
        expected = if normalized == "tel", do: stripped, else: trimmed
        assert HtmlValidator.prepare_input_attributes(attributes)["value"] == expected
        assert attributes["value"] == value
      end

      assert HtmlValidator.prepare_input_attributes(%{"type" => type}) == %{"type" => type}
    end

    for type <- ["checkbox", "radio", "hidden", "textarea", "unsupported"] do
      refute HtmlValidator.text_input_type?(type)
    end
  end

  test "multiple email preserves token order and trims ASCII token edges" do
    assert HtmlValidator.valid_boolean_form_attribute?("input", "multiple")

    for type <- ["email", "EMAIL", "EmAiL"], multiple <- ["", "multiple", "false"] do
      assert HtmlValidator.valid_form_element?(
               "input",
               %{"type" => type, "multiple" => multiple},
               []
             )

      assert HtmlValidator.valid_form_element?(
               "input",
               %{"type" => type, "multiple" => multiple, "readonly" => ""},
               []
             )

      for {value, expected} <- [
            {" \ta@example.com ,\n b@example.com\f ", "a@example.com,b@example.com"},
            {" \t\r\n\f ", ""},
            {" , , ", ",,"},
            {" A,, B, ", "A,,B,"},
            {"\u00A0A\u00A0 , B\v", "\u00A0A\u00A0,B\v"},
            {" A\nB , C\rD ", "AB,CD"}
          ] do
        attributes = %{"type" => type, "multiple" => multiple, "value" => value}
        assert HtmlValidator.prepare_input_attributes(attributes)["value"] == expected
        assert attributes["value"] == value
      end

      assert HtmlValidator.prepare_input_attributes(%{"type" => type, "multiple" => multiple}) ==
               %{"type" => type, "multiple" => multiple}
    end

    for type <- [
          "text",
          "search",
          "tel",
          "url",
          "hidden",
          "checkbox",
          "radio",
          "button",
          "reset",
          "submit"
        ] do
      assert HtmlValidator.valid_form_element?("input", %{"type" => type, "multiple" => ""}, [])
    end
  end

  test "input type defaults use ASCII enumerated keywords" do
    for value <- [
          "",
          " ",
          "unregistered",
          " text",
          "hidden ",
          "datetime",
          "unregistered-input-type",
          "checKbox",
          "ſearch"
        ] do
      assert HtmlValidator.input_type(value) == "text"
      assert HtmlValidator.valid_form_attribute?("input", "type", value)
      assert HtmlValidator.valid_form_element?("input", %{"type" => value, "readonly" => ""}, [])
      assert HtmlValidator.valid_form_element?("input", %{"type" => value, "checked" => ""}, [])
      assert HtmlValidator.valid_form_element?("input", %{"type" => value, "multiple" => ""}, [])

      assert HtmlValidator.prepare_input_attributes(%{"type" => value, "value" => "A\rB\nC"})[
               "value"
             ] == "ABC"
    end

    assert HtmlValidator.input_type(String.duplicate("X", 100_000)) == "text"

    for type <- [
          "text",
          "search",
          "tel",
          "url",
          "email",
          "hidden",
          "checkbox",
          "radio",
          "button",
          "reset",
          "submit",
          "number",
          "date",
          "month",
          "week",
          "time",
          "datetime-local"
        ] do
      assert HtmlValidator.input_type(String.upcase(type)) == type
    end

    for type <- [
          "color",
          "file",
          "image"
        ] do
      assert HtmlValidator.input_type(String.upcase(type)) == type
      assert HtmlValidator.valid_form_attribute?("input", "type", type)
      assert HtmlValidator.valid_form_element?("input", %{"type" => type}, [])
    end
  end

  test "button input labels and applicability are shared validator semantics" do
    for {type, label} <- [{"button", ""}, {"reset", "Reset"}, {"submit", "Submit"}] do
      assert HtmlValidator.button_input_type?(type)
      assert HtmlValidator.valid_form_attribute?("input", "type", String.upcase(type))

      assert HtmlValidator.valid_form_element?(
               "input",
               %{"type" => String.upcase(type), "disabled" => "false"},
               []
             )

      assert HtmlValidator.prepare_input_attributes(%{"type" => String.upcase(type)}) == %{
               "type" => String.upcase(type),
               "value" => label
             }

      for value <- ["", "Custom label", " A\rB\nC "] do
        attributes = %{"type" => type, "value" => value}
        assert HtmlValidator.prepare_input_attributes(attributes) == attributes
      end

      for name <- ["checked", "readonly", "multiple"] do
        assert HtmlValidator.valid_form_element?("input", %{"type" => type, name => ""}, [])
      end

      refute HtmlValidator.valid_form_element?("input", %{"type" => type}, [
               %{type: :text, text: "Child"}
             ])
    end

    refute HtmlValidator.button_input_type?("text")
    assert HtmlValidator.prepare_input_attributes(%{"type" => "hidden"}) == %{"type" => "hidden"}
  end

  test "initial enabled and disabled matching uses applicable HTML controls and boolean presence" do
    for tag <- ["input", "select", "textarea", "button", "option"],
        value <- ["", "false", "disabled"] do
      node = %{type: :element, tag: tag, attributes: %{"disabled" => value}}
      assert HtmlValidator.control_state_matches?(:disabled, node)
      refute HtmlValidator.control_state_matches?(:enabled, node)
      enabled = put_in(node, [:attributes], %{"readonly" => "", "aria-disabled" => "true"})
      assert HtmlValidator.control_state_matches?(:enabled, enabled)
      refute HtmlValidator.control_state_matches?(:disabled, enabled)

      for namespace <- [:svg, :mathml] do
        foreign = Map.put(node, :namespace, namespace)
        refute HtmlValidator.control_state_matches?(:enabled, foreign)
        refute HtmlValidator.control_state_matches?(:disabled, foreign)
      end
    end

    for tag <- ["p", "div", "span", "custom-element"] do
      node = %{type: :element, tag: tag, attributes: %{"disabled" => "", "role" => "button"}}
      refute HtmlValidator.control_state_matches?(:enabled, node)
      refute HtmlValidator.control_state_matches?(:disabled, node)
    end

    for {source, state, rest} <- [
          {":enabled", :enabled, ""},
          {":DISABLED.foo", :disabled, ".foo"},
          {":First-Child:enabled", :first_child, ":enabled"},
          {":LAST-OF-TYPE", :last_of_type, ""}
        ] do
      assert HtmlValidator.parse_css_simple_pseudo_class(source) == {:ok, state, rest}
    end

    for source <- ["", ":enabledness", ":diſabled", ".enabled", ":nth-child(2)", ":not(.x)"] do
      assert HtmlValidator.parse_css_simple_pseudo_class(source) == nil
    end
  end

  test "image dimension hints use HTML prefix and immediate percentage rules" do
    assert HtmlValidator.image_dimension_hints(%{}) == {:ok, %{}}

    for {value, expected} <- [
          {"24", 18.0},
          {"24.5px", 18.375},
          {"24.%", {:percent, 0.24}},
          {"24.5%junk", {:percent, 0.245}},
          {"24.5 %", 18.375},
          {"\f\t12e3", 9.0},
          {"0", 0.0}
        ] do
      assert HtmlValidator.image_dimension_hints(%{"width" => value}) == {:ok, %{width: expected}}
    end

    for value <- ["", "invalid", "+10", "-10", ".5", "\u00a010"] do
      assert HtmlValidator.image_dimension_hints(%{"width" => value}) == {:ok, %{}}
    end

    assert HtmlValidator.image_dimension_hints(%{"width" => "24", "height" => "12"}) ==
             {:ok, %{width: 18.0, height: 9.0}}

    assert HtmlValidator.image_dimension_hints(%{"width" => "24", "height" => "invalid"}) ==
             {:ok, %{width: 18.0}}
  end

  test "automatic width and height use the canonical size validator" do
    for property <- [:width, :height], value <- ["auto", "AUTO", " auto "] do
      assert {:ok, :auto} = HtmlValidator.parse_css_size(value, property)
    end

    assert :error = HtmlValidator.parse_css_size("auto")
    assert :error = HtmlValidator.parse_css_size("auto", :min_height)
    assert :error = HtmlValidator.parse_css_size("min(auto, 10pt)", :width)

    assert {:ok, {:min, [10.0, {:percent, 0.5}]}} =
             HtmlValidator.parse_css_size("min(10pt, 50%)", :width)

    assert :error = HtmlValidator.parse_css_size("-1pt", :height)
  end

  test "password state sanitizes line breaks without trimming or exposing mask data" do
    assert HtmlValidator.text_input_type?("password")
    assert HtmlValidator.valid_form_attribute?("input", "type", "PASSWORD")
    attrs = %{"type" => "PaSsWoRd", "value" => " A\rB\n é🦊 ", "readonly" => "false"}
    assert HtmlValidator.valid_form_element?("input", attrs, [])
    assert HtmlValidator.prepare_input_attributes(attrs)["value"] == " AB é🦊 "
    assert HtmlValidator.valid_form_element?("input", Map.put(attrs, "multiple", ""), [])
    assert HtmlValidator.valid_form_element?("input", Map.put(attrs, "checked", ""), [])

    assert {:ok, "•••"} =
             NativeElixirPdfUtilities.Validators.FormValidator.password_display_text("Aé🦊")

    assert {:ok, ""} = NativeElixirPdfUtilities.Validators.FormValidator.password_display_text("")
  end

  test "pagination rejects non-map decoration transport before executing geometry" do
    tree = %{type: :layout, page_size: {100, 100}, boxes: [:invalid]}

    assert {:error, {:invalid_layout, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(tree)

    assert diagnostic.stage == :pagination and diagnostic.operation == :paginate

    assert is_binary(diagnostic.source) and is_binary(diagnostic.message)
  end

  test "pagination rejects forged fieldset decoration metadata with actionable diagnostics" do
    id = make_ref()

    box = %{
      type: :rect,
      x: 0,
      y: 0,
      width: 20,
      height: 20,
      fieldset_paint_id: id,
      fieldset_ancestors: [id],
      fieldset_bounds: %{top: 20, bottom: 0}
    }

    for invalid <- [
          Map.delete(box, :fieldset_bounds),
          Map.put(box, :fieldset_paint_id, "id"),
          Map.put(box, :fieldset_ancestors, :invalid),
          Map.put(box, :fieldset_bounds, %{top: 0, bottom: 20}),
          Map.put(box, :fieldset_ancestors, [make_ref()]),
          Map.put(box, :type, :image)
        ] do
      tree = %{type: :layout, page_size: {100, 100}, boxes: [invalid]}

      assert {:error, {:invalid_layout, diagnostic}} =
               NativeElixirPdfUtilities.HtmlToPdf.Pagination.paginate(tree)

      assert diagnostic.stage == :pagination and diagnostic.operation == :paginate

      assert diagnostic.source == "fieldset_paint_id" and
               diagnostic.message =~ "matching paint boxes"
    end
  end

  test "effective fieldset disabling uses only each ancestor's first direct HTML legend exemption" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
    alias NativeElixirPdfUtilities.Validators.HtmlFormValidator

    html = """
    <fieldset id=outer disabled=false><legend style='display:none'>
    <input id=exempt><input id=own disabled><span><input id=deep></span></legend>
    <legend><input id=second></legend><input id=body>
    <fieldset id=nested><legend><input id=nested_caption></legend><input id=nested_body></fieldset>
    <select id=choice><option id=option>A</option></select><textarea id=notes></textarea>
    <button id=button>Button</button><input id=hidden type=hidden></fieldset>
    <fieldset id=enabled><legend><input id=outside></legend>
    <fieldset id=inner disabled><legend><input id=inner_exempt></legend><input id=inner_body></fieldset></fieldset>
    """

    assert {:ok, dom} = HtmlParser.parse_detailed(html)
    nodes = HtmlFormValidator.prepare_initial_nodes(dom.children)

    collect = fn collect, nodes ->
      Enum.flat_map(nodes, fn node ->
        if node.type == :element, do: [node | collect.(collect, node.children)], else: []
      end)
    end

    nodes = Map.new(collect.(collect, nodes), &{Map.get(&1.attributes, "id"), &1})

    for id <-
          ~w(outer own second body nested nested_caption nested_body choice notes button hidden inner inner_body) do
      assert nodes[id]._initial_form_state.disabled
      assert HtmlValidator.control_state_matches?(:disabled, nodes[id])
      refute HtmlValidator.control_state_matches?(:enabled, nodes[id])
    end

    for id <- ~w(exempt deep enabled outside inner_exempt) do
      refute nodes[id]._initial_form_state.disabled
      assert HtmlValidator.control_state_matches?(:enabled, nodes[id])
    end

    refute HtmlValidator.control_state_matches?(:disabled, nodes["option"])
    refute Map.has_key?(nodes["body"].attributes, "disabled")
    assert nodes["outer"].attributes["disabled"] == "false"
    assert {:ok, _} = Style.compute_detailed(dom)

    for invalid <- [
          "<legend>Orphan</legend>",
          "<fieldset><legend><div>Block</div></legend></fieldset>"
        ] do
      assert {:ok, ordinary} = HtmlParser.parse_detailed("<div><span>Text</span></div>")
      [container] = ordinary.children
      [span] = container.children
      legend = %{span | tag: "legend"}

      tree =
        if invalid =~ "Block",
          do: %{
            ordinary
            | children: [
                %{container | tag: "fieldset", children: [%{legend | children: [container]}]}
              ]
          },
          else: %{ordinary | children: [legend]}

      assert {:error, {:invalid_document, diagnostic}} =
               Style.compute_detailed(NativeElixirPdfUtilities.HtmlTreeHelpers.tree_content(tree))

      assert diagnostic.stage == :style and diagnostic.message =~ "content models"
    end
  end

  test "fieldset ownership does not reassign descendant controls or make fieldsets labelable" do
    alias NativeElixirPdfUtilities.HtmlToPdf.HtmlParser
    alias NativeElixirPdfUtilities.Validators.HtmlFormValidator

    assert {:ok, dom} =
             HtmlParser.parse_detailed("""
             <form id=ancestor><fieldset id=group form=forward name=group>
             <legend>Caption</legend><input id=child></fieldset></form>
             <label for=group>Group label</label><label for=child>Control label</label>
             <label><input type=hidden><input id=implicit></label><form id=forward></form>
             <template><form id=forward><fieldset disabled><input id=child></fieldset></form></template>
             """)

    [ancestor, group_label, control_label, implicit_label, forward, template] =
      HtmlFormValidator.prepare_initial_nodes(dom.children)

    [fieldset] = ancestor.children
    [legend, child] = Enum.filter(fieldset.children, &(&1.type == :element))
    assert fieldset._initial_form_state.owner == forward._selector_id
    assert legend._initial_legend_form_owner == forward._selector_id
    assert child._initial_form_state.owner == ancestor._selector_id
    assert group_label._initial_label_control == nil
    assert control_label._initial_label_control == child._selector_id
    [_, implicit] = implicit_label.children
    assert implicit_label._initial_label_control == implicit._selector_id
    assert implicit._initial_form_state.owner == nil
    refute child._initial_form_state.disabled
    assert template.children == []

    [empty] =
      HtmlFormValidator.prepare_initial_nodes([
        %{type: :element, tag: "fieldset", attributes: %{}, children: []}
      ])

    assert %{owner: nil, disabled: false, checked: false} = empty._initial_form_state
    assert empty._initial_form_state.constraint_validation.valid
    refute empty._initial_form_state.constraint_validation.will_validate
    assert HtmlValidator.valid_form_attribute?("fieldset", "name", "Group")
    assert HtmlValidator.valid_boolean_form_attribute?("fieldset", "disabled")
    refute HtmlValidator.valid_boolean_form_attribute?("legend", "disabled")
  end

  test "metadata-free picture trees retain actionable content-model diagnostics" do
    alias NativeElixirPdfUtilities.HtmlToPdf.Style

    for {tag, children} <- [
          {"source", []},
          {"picture", [%{type: :text, text: "Invalid artwork"}]}
        ] do
      dom = %{
        type: :document,
        children: [%{type: :element, tag: tag, attributes: %{}, children: children}]
      }

      assert {:error, {:invalid_document, diagnostic}} = Style.compute_detailed(dom)
      assert diagnostic.stage == :style and diagnostic.operation == :compute
      assert diagnostic.source == tag
      assert diagnostic.message =~ "picture"
    end
  end

  test "SVG preparation validation cannot authorize rendering in the same budget" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}

    original = "<svg><foreignObject><p>Prepared only</p></foreignObject></svg>"
    assert {:ok, dom} = HtmlParser.parse_detailed(original)

    assert {:error, {:unsupported_html, diagnostic}} =
             NativeElixirPdfUtilities.HtmlToPdf.render(original)

    assert diagnostic.source == "foreignObject" and diagnostic.stage == :layout
    assert diagnostic.reason == :unsupported_html
    assert diagnostic.message =~ "cannot be omitted"

    filtered =
      "<svg><defs><filter id='f'><feGaussianBlur stdDeviation='1'/></filter></defs><g filter='url(#f)'><foreignObject width='100' height='30'><p>Real HTML input</p></foreignObject></g></svg>"

    assert {:ok, filtered_dom} = HtmlParser.parse_detailed(filtered)
    options = Font.normalize_options([])

    assert :ok ==
             NativeElixirPdfUtilities.HtmlToPdf.ResourceContext.run(
               [],
               HtmlValidator.new_image_budget(),
               fn _context ->
                 assert {:ok, false} =
                          HtmlValidator.prepare_style_input(dom, [], options, :prepare_svg)

                 assert {:ok, false} =
                          HtmlValidator.prepare_style_input(dom, [], options, :render)

                 assert {:ok, false} =
                          HtmlValidator.prepare_style_input(
                            filtered_dom,
                            [],
                            options,
                            :prepare_svg
                          )

                 assert {:ok, _styled} = Style.compute_detailed(filtered_dom, [])

                 # Preparation cannot authorize invalid source parameters in the
                 # same budget. Canonical validation remains independent of painting.
                 invalid = String.replace(filtered, "stdDeviation='1'", "stdDeviation='-1'")
                 assert {:ok, invalid_dom} = HtmlParser.parse_detailed(invalid)

                 assert {:ok, false} =
                          HtmlValidator.prepare_style_input(
                            invalid_dom,
                            [],
                            options,
                            :prepare_svg
                          )

                 assert {:error, {:invalid_document, diagnostic}} =
                          Style.compute_detailed(invalid_dom, [])

                 assert diagnostic.source == "feGaussianBlur"
                 assert diagnostic.stage == :layout and diagnostic.operation == :prepare
                 assert diagnostic.reason == :invalid_document
                 assert diagnostic.message =~ "stdDeviation"

                 assert diagnostic.module ==
                          NativeElixirPdfUtilities.Validators.SvgFilterValidator

                 assert {:ok, false} =
                          HtmlValidator.prepare_style_input(
                            filtered_dom,
                            [],
                            options,
                            :prepare_svg
                          )

                 assert {:error, {:invalid_document, _}} =
                          HtmlValidator.prepare_style_input(dom, [], options, :unknown)

                 :ok
               end
             )
  end

  test "validated DOM reuse rejects changed contents and options with retained canonical IDs" do
    alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Font}
    assert {:ok, dom} = HtmlParser.parse("<p>Text</p>")
    options = Font.normalize_options([])

    assert :ok ==
             HtmlValidator.with_render_budget(fn ->
               assert {:ok, false} = HtmlValidator.prepare_style_input(dom, [], options)
               [node] = dom.children
               changed = %{dom | children: [%{node | tag: "unsupported"}]}

               assert {:error, {:invalid_document, diagnostic}} =
                        HtmlValidator.prepare_style_input(changed, [], options)

               assert diagnostic.stage == :style and diagnostic.source == "unsupported"

               assert {:error, {:invalid_options, diagnostic}} =
                        HtmlValidator.prepare_style_input(dom, [outlines: :invalid], options)

               assert diagnostic.stage == :style or diagnostic.stage == :options
               assert {:ok, false} = HtmlValidator.prepare_style_input(dom, [], options)
               checked_work = Process.get({HtmlValidator, :render_budget})
               assert :ok = HtmlValidator.release_style_input(changed)
               assert {:ok, false} = HtmlValidator.prepare_style_input(dom, [], options)
               assert Process.get({HtmlValidator, :render_budget}) == checked_work

               assert :ok = HtmlValidator.release_style_input(dom)
               assert :ok = HtmlValidator.release_style_input(dom)
               assert {:ok, false} = HtmlValidator.prepare_style_input(dom, [], options)

               assert Process.get({HtmlValidator, :render_budget}).max_html_tree_work >
                        checked_work.max_html_tree_work

               :ok
             end)
  end
end
