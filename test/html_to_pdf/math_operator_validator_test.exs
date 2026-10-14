defmodule NativeElixirPdfUtilities.HtmlToPdf.MathOperatorValidatorTest do
  use ExUnit.Case

  alias NativeElixirPdfUtilities.HtmlToPdf.MathOperatorDictionary
  alias NativeElixirPdfUtilities.Limits
  alias NativeElixirPdfUtilities.Validators.MathOperatorValidator

  setup do
    limits = Limits.effective()
    on_exit(fn -> Limits.install(limits) end)
    :ok
  end

  test "pinned normative tables retain every category, axis and special entry" do
    data = MathOperatorDictionary.data()
    assert data.source_url == "https://www.w3.org/TR/2025/CR-mathml-core-20250624/"

    assert data.source_sha256 ==
             "f61ebea74ba54c489646e6969983ef977f448c26602edff75ec4e3ba77216373"

    assert map_size(data.entries) == 725
    assert map_size(data.ascii_pairs) == 18
    assert MapSet.size(data.fences) == 61
    assert MapSet.size(data.separators) == 3
    assert MapSet.size(data.inline_axes) == 246

    assert Enum.frequencies(Map.values(data.entries)) == %{
             a: 313,
             b: 108,
             c: 64,
             d: 52,
             e: 40,
             f: 30,
             g: 30,
             h: 27,
             i: 22,
             j: 22,
             k: 8,
             l: 6,
             m: 3
           }

    # Exercise all normative table entries through the validator. Reserved
    # synthetic keys represent two-ASCII-character strings, never raw Unicode.
    aliases = Map.new(data.ascii_pairs, fn {text, codepoint} -> {codepoint, text} end)

    for {{codepoint, form}, category} <- data.entries do
      text = Map.get(aliases, codepoint, <<codepoint::utf8>>)

      assert {:ok, prepared} =
               MathOperatorValidator.prepare(text, %{"form" => Atom.to_string(form)})

      assert prepared.category == category
      assert prepared.text == text
      defaults = Map.fetch!(data.categories, category)
      assert_in_delta prepared.lspace, defaults.lspace * 12, 1.0e-12
      assert_in_delta prepared.rspace, defaults.rspace * 12, 1.0e-12

      for key <- [:stretchy, :symmetric, :largeop, :movablelimits] do
        assert Map.fetch!(prepared, key) == Map.fetch!(defaults, key)
      end
    end
  end

  test "representative categories expose spacing and large-operator semantics" do
    for {text, form, category, left, right, flags} <- [
          {"→", "infix", :a, 5 / 18, 5 / 18, [:stretchy]},
          {"+", "infix", :b, 4 / 18, 4 / 18, []},
          {"×", "infix", :c, 3 / 18, 3 / 18, []},
          {"+", "prefix", :d, 0, 0, []},
          {"!", "postfix", :e, 0, 0, []},
          {"(", "prefix", :f, 0, 0, [:stretchy, :symmetric]},
          {")", "postfix", :g, 0, 0, [:stretchy, :symmetric]},
          {"∫", "prefix", :h, 3 / 18, 3 / 18, [:largeop, :symmetric]},
          {"⏞", "postfix", :i, 0, 0, [:stretchy]},
          {"∑", "prefix", :j, 3 / 18, 3 / 18, [:largeop, :symmetric, :movablelimits]},
          {"/", "infix", :k, 0, 0, []},
          {"∂", "prefix", :l, 3 / 18, 0, []},
          {",", "infix", :m, 0, 3 / 18, []},
          {"unknown", "infix", :default, 5 / 18, 5 / 18, []}
        ] do
      assert {:ok, operator} =
               MathOperatorValidator.prepare(text, %{"form" => form}, %{font_size: 18})

      assert operator.category == category
      assert_in_delta operator.lspace, left * 18, 1.0e-12
      assert_in_delta operator.rspace, right * 18, 1.0e-12
      assert operator.minsize == {:percentage, 100.0}
      assert operator.maxsize == :infinity

      for key <- [:stretchy, :symmetric, :largeop, :movablelimits] do
        assert Map.fetch!(operator, key) == key in flags
      end
    end
  end

  test "selected-font cap, zero advance and line height resolve authored operator lengths" do
    assert {:ok, operator} =
             MathOperatorValidator.prepare(
               "(",
               %{
                 "lspace" => "2ch",
                 "rspace" => "-0.5cap",
                 "minsize" => "1.5lh",
                 "maxsize" => "3cap"
               },
               %{font_size: 20, zero_advance: 11, cap_height: 13, line_height: 24}
             )

    assert operator.lspace == 22
    assert operator.rspace == -6.5
    assert operator.minsize == 36
    assert operator.maxsize == 39

    for {unit, key} <- [{"ch", :zero_advance}, {"cap", :cap_height}, {"lh", :line_height}] do
      assert {:error, {:invalid_document, diagnostic}} =
               MathOperatorValidator.prepare("(", %{"minsize" => "2#{unit}"})

      assert diagnostic.stage == :layout and diagnostic.source == "2#{unit}"
      assert diagnostic.message =~ "selected-font #{key}"

      assert {:error, {:invalid_document, _}} =
               MathOperatorValidator.prepare("(", %{}, %{key => -1})
    end
  end

  test "outer embellished position determines prefix, middle infix and postfix" do
    for parent <- [
          :grouping,
          "maction",
          "math",
          "mrow",
          "mstyle",
          "merror",
          "mphantom",
          "mprescripts",
          "semantics",
          "mpadded",
          "msqrt"
        ] do
      for {index, expected} <- [{0, :prefix}, {1, :infix}, {2, :postfix}] do
        assert {:ok, operator} =
                 MathOperatorValidator.prepare("+", %{}, %{parent: parent, index: index, count: 3})

        assert operator.form == expected
      end

      assert {:ok, alone} = MathOperatorValidator.prepare("+", %{}, %{parent: parent})
      assert alone.form == :infix
    end

    assert {:ok, empty} = MathOperatorValidator.prepare("", %{}, %{parent: "mrow", count: 0})
    assert empty.form == :infix

    assert {:ok, unknown_parent} =
             MathOperatorValidator.prepare("+", %{}, %{parent: "mfrac", count: 2})

    assert unknown_parent.form == :infix
  end

  test "scripts use the in-flow child index including space-like children" do
    for parent <- ["msub", "msup", "msubsup", "munder", "mover", "munderover", "mmultiscripts"] do
      assert {:ok, base} = MathOperatorValidator.prepare("!", %{}, %{parent: parent, count: 2})
      assert base.form == :infix

      assert {:ok, script} =
               MathOperatorValidator.prepare("!", %{}, %{parent: parent, count: 2, index: 1})

      assert script.form == :postfix

      assert {:ok, script} =
               MathOperatorValidator.prepare("!", %{}, %{parent: parent, child_index: 2})

      assert script.form == :postfix
    end
  end

  test "valid explicit forms suppress dictionary fallback and invalid forms do not" do
    assert {:ok, explicit} =
             MathOperatorValidator.prepare("+", %{"form" => "PoStFiX"}, %{
               parent: "mrow",
               count: 2
             })

    assert explicit.form == :postfix
    assert explicit.category == :default
    assert explicit.explicit_form

    for value <- ["sideways", " postfix ", "", "İNFiX"] do
      assert {:ok, fallback} =
               MathOperatorValidator.prepare("+", %{"form" => value}, %{parent: "mrow", count: 2})

      assert fallback.form == :prefix
      assert fallback.category == :d
      refute fallback.explicit_form
    end

    # Fallback order is infix, postfix, prefix without changing effective form.
    assert {:ok, inferred} = MathOperatorValidator.prepare("!", %{})
    assert inferred.form == :infix
    assert inferred.category == :e
    assert {:ok, opening} = MathOperatorValidator.prepare("(", %{})
    assert opening.form == :infix
    assert opening.category == :f
    assert {:ok, prefix} = MathOperatorValidator.prepare("×", %{}, %{parent: "mrow", count: 2})
    assert prefix.form == :prefix
    assert prefix.category == :c
  end

  test "forced defaults prevent inferred fence or prefix semantics" do
    for text <- ["|", "∼"] do
      assert {:ok, infix} = MathOperatorValidator.prepare(text, %{})
      assert infix.category == :force_default
      refute infix.stretchy
      refute infix.symmetric
      assert_in_delta infix.lspace, 12 * 5 / 18, 1.0e-12
    end

    assert {:ok, prefix} = MathOperatorValidator.prepare("|", %{"form" => "prefix"})
    assert prefix.category == :f
    assert prefix.stretchy
    assert prefix.fence
  end

  test "Unicode keys use UTF-16 rules without changing extraction text" do
    for {text, form, expected} <- [
          {"+̸", "infix", :b},
          {"→⃒", "infix", :a},
          {"++", "postfix", :e},
          {"&&", "infix", :b},
          {"!!", "postfix", :e},
          {"abc", "infix", :default},
          {"̱", "prefix", :default},
          {"😀", "postfix", :default},
          {"𞻰", "postfix", :i},
          {"𞻱", "postfix", :i},
          {"𞻰", "prefix", :default},
          {"𞻰̸", "postfix", :default},
          {"ab", "infix", :default},
          {"", "infix", :default}
        ] do
      assert {:ok, operator} = MathOperatorValidator.prepare(text, %{"form" => form})
      assert operator.category == expected
      assert operator.text == text
    end

    assert {:ok, text_content} = MathOperatorValidator.prepare("→", %{}, %{text_only: false})
    assert text_content.category == :default
    refute text_content.single_character
    assert text_content.stretch_axis == :vertical
  end

  test "all intrinsic axes are form-independent but multi-character layout is not stretch layout" do
    for codepoint <- MathOperatorDictionary.data().inline_axes,
        form <- ["infix", "prefix", "postfix"] do
      assert {:ok, operator} =
               MathOperatorValidator.prepare(<<codepoint::utf8>>, %{"form" => form})

      assert operator.stretch_axis == :horizontal
      assert operator.single_character
    end

    for text <- ["(", "↑", "↛", "->", "", "=="] do
      assert {:ok, operator} = MathOperatorValidator.prepare(text, %{})
      assert operator.stretch_axis == :vertical
      assert operator.single_character == match?(<<_::utf8>>, text)
    end
  end

  test "invisible operators retain every original codepoint and zero dictionary spacing" do
    for codepoint <- 0x2061..0x2064 do
      text = <<codepoint::utf8>>
      assert {:ok, operator} = MathOperatorValidator.prepare(text, %{})
      assert operator.text == text
      assert operator.invisible
      assert operator.lspace == 0
      assert operator.rspace == 0
      refute operator.stretchy
      assert operator.separator == (codepoint == 0x2063)
    end

    assert {:ok, regular} = MathOperatorValidator.prepare("+", %{})
    refute regular.invisible
  end

  test "fence/separator metadata and boolean overrides never change spacing or axis" do
    for codepoint <- MathOperatorDictionary.data().fences do
      text = if codepoint == 0x0331, do: "||", else: <<codepoint::utf8>>
      assert {:ok, operator} = MathOperatorValidator.prepare(text, %{})
      assert operator.fence
    end

    assert {:ok, combining_line} = MathOperatorValidator.prepare("̱", %{})
    refute combining_line.fence
    assert {:ok, overlaid_fence} = MathOperatorValidator.prepare("(̸", %{})
    assert overlaid_fence.fence
    assert overlaid_fence.text == "(̸"

    attrs =
      Map.new(
        [:stretchy, :symmetric, :largeop, :movablelimits, :fence, :separator],
        &{Atom.to_string(&1), "TrUe"}
      )

    assert {:ok, override} = MathOperatorValidator.prepare("x", attrs)

    assert override.stretchy and override.symmetric and override.largeop and
             override.movablelimits

    assert override.fence and override.separator
    assert_in_delta override.lspace, 12 * 5 / 18, 1.0e-12
    assert override.stretch_axis == :vertical

    attrs = Map.new(attrs, fn {key, _} -> {key, "FALSE"} end)
    assert {:ok, cleared} = MathOperatorValidator.prepare("∑", attrs)

    refute cleared.largeop or cleared.movablelimits or cleared.symmetric or cleared.fence or
             cleared.separator or cleared.stretchy

    assert {:ok, defaults} = MathOperatorValidator.prepare("(", %{"stretchy" => " true "})
    assert defaults.stretchy
  end

  test "lengths resolve in core font points and spacing percentages use dictionary values" do
    assert {:ok, operator} =
             MathOperatorValidator.prepare(
               "+",
               %{"lspace" => "150%", "rspace" => "-50%", "minsize" => "200%", "maxsize" => "50%"},
               %{font_size: 18}
             )

    assert_in_delta operator.lspace, 6, 1.0e-12
    assert_in_delta operator.rspace, -2, 1.0e-12
    assert operator.minsize == {:percentage, 200.0}
    assert operator.maxsize == {:percentage, 50.0}

    for {length, expected} <- [
          {"2em", 36},
          {"1rem", 30},
          {"1ex", 7},
          {"12px", 9},
          {"72pt", 72},
          {"1in", 72},
          {"6pc", 72},
          {"101.6q", 72},
          {"25.4mm", 72},
          {"2.54cm", 72},
          {"0", 0},
          {"-3pt", -3},
          {" \t+2EM\n", 36},
          {"1e1pt", 10}
        ] do
      attrs = Map.new(["lspace", "rspace", "minsize", "maxsize"], &{&1, length})

      assert {:ok, prepared} =
               MathOperatorValidator.prepare("+", attrs, %{
                 font_size: 18,
                 root_font_size: 30,
                 x_height: 7
               })

      for key <- [:lspace, :rspace, :minsize, :maxsize],
          do: assert_in_delta(Map.fetch!(prepared, key), expected, 1.0e-10)
    end

    assert {:ok, zero_base} = MathOperatorValidator.prepare("(", %{"lspace" => "300%"})
    assert zero_base.lspace == 0
  end

  test "invalid length syntax uses defaults and negative size constraints remain valid" do
    for value <- [
          "",
          "garbage",
          "1",
          "thinmathspace",
          "infinity",
          "1pt junk",
          "1e100pt",
          "\u00A02em"
        ] do
      attrs = Map.new(["lspace", "rspace", "minsize", "maxsize"], &{&1, value})
      assert {:ok, operator} = MathOperatorValidator.prepare("+", attrs)
      assert_in_delta operator.lspace, 12 * 4 / 18, 1.0e-12
      assert operator.minsize == {:percentage, 100.0}
      assert operator.maxsize == :infinity
    end

    assert {:ok, negative} =
             MathOperatorValidator.prepare("(", %{"minsize" => "-2em", "maxsize" => "-10pt"})

    assert negative.minsize == -24
    assert negative.maxsize == -10
    assert {:ok, zero} = MathOperatorValidator.prepare("+", %{}, %{font_size: 0})
    assert zero.lspace == 0
  end

  test "public argument failures return actionable shared diagnostics" do
    for {text, attrs, context} <- [
          {nil, %{}, %{}},
          {<<255>>, %{}, %{}},
          {"+", [], %{}},
          {"+", ~D[2026-10-12], %{}},
          {"+", %{form: "infix"}, %{}},
          {"+", %{"form" => true}, %{}},
          {"+", %{<<255>> => "infix"}, %{}},
          {"+", %{"form" => <<255>>}, %{}},
          {"+", %{}, []},
          {"+", %{}, %{unknown: 1}},
          {"+", %{}, %{parent: 7}},
          {"+", %{}, %{parent: <<255>>}},
          {"+", %{}, %{index: -1}},
          {"+", %{}, %{count: -1}},
          {"+", %{}, %{count: 0, index: 1}},
          {"+", %{}, %{count: 1, index: 1}},
          {"+", %{}, %{child_index: -1}},
          {"+", %{}, %{text_only: nil}},
          {"+", %{}, %{font_size: -1}},
          {"+", %{}, %{root_font_size: "12"}},
          {"+", %{}, %{x_height: -1}},
          {"+", %{}, %{font_size: 1.0e20}}
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               MathOperatorValidator.prepare(text, attrs, context)

      assert diagnostic.stage == :layout
      assert diagnostic.reason == :invalid_document
      assert diagnostic.operation == :prepare_math_operator
      assert diagnostic.module == MathOperatorValidator
      assert byte_size(diagnostic.message) > 10
    end

    assert {:error, {:invalid_document, diagnostic}} =
             MathOperatorValidator.prepare("+", %{"lspace" => "1ex"})

    assert diagnostic.message =~ "selected-font x_height"
    assert diagnostic.source == "1ex"
  end

  test "resource diagnostics cover text, positional work, numeric tokens and resolved spacing" do
    Limits.install(
      Map.merge(Limits.effective(), %{
        max_rendered_text_bytes: 1,
        max_html_nodes: 3,
        max_html_numeric_token_bytes: 4,
        max_css_numeric_magnitude: 100
      })
    )

    for {text, attrs, context, message} <- [
          {"++", %{}, %{}, "max_rendered_text_bytes"},
          {"+", %{}, %{count: 4}, "max_html_nodes"},
          {"+", %{}, %{child_index: 4}, "max_html_nodes"},
          {"+", %{"lspace" => "100pt"}, %{}, "max_html_numeric_token_bytes"},
          {"+", %{"lspace" => "100%"}, %{font_size: 100}, nil}
        ] do
      case message do
        nil ->
          assert {:ok, _} = MathOperatorValidator.prepare(text, attrs, context)

        _ ->
          assert {:error, {:resource_limit_exceeded, diagnostic}} =
                   MathOperatorValidator.prepare(text, attrs, context)

          assert diagnostic.stage == :layout
          assert diagnostic.operation == :prepare_math_operator
          assert diagnostic.message =~ message
      end
    end

    Limits.install(
      Map.merge(Limits.effective(), %{
        max_html_numeric_token_bytes: 100,
        max_css_numeric_magnitude: 1_000
      })
    )

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             MathOperatorValidator.prepare("+", %{"lspace" => "1000%"}, %{font_size: 1_000})

    assert diagnostic.message =~ "max_css_numeric_magnitude"
    assert diagnostic.source == "1000%"

    for {attribute, value, context} <- [
          {"minsize", "2rem", %{root_font_size: 1_000}},
          {"minsize", "2em", %{font_size: 1_000}},
          {"maxsize", "100pc", %{}},
          {"minsize", "2ex", %{x_height: 1_000}}
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} =
               MathOperatorValidator.prepare("+", %{attribute => value}, context)

      assert diagnostic.message =~ "max_css_numeric_magnitude"
      assert diagnostic.source == value
    end
  end

  test "resolved viewport operator lengths retain shared calculation diagnostics" do
    Limits.install(Map.put(Limits.effective(), :max_css_numeric_magnitude, 1_000))

    assert {:error, {:invalid_document, diagnostic}} =
             MathOperatorValidator.prepare("+", %{"lspace" => "200vw"}, %{
               viewport_width: 1_000,
               viewport_height: 500
             })

    assert diagnostic.stage == :layout
    assert diagnostic.operation == :prepare_math_operator
    assert diagnostic.module == MathOperatorValidator
    assert diagnostic.source == "200vw"
    assert diagnostic.message =~ "operator lspace"
    assert diagnostic.message =~ "validated expression"
  end

  test "valid CSS lengths requiring unimplemented geometry diagnose instead of taking a dictionary fallback" do
    for value <- [
          "2vw",
          "1cqw"
        ] do
      assert {:error, {:invalid_document, diagnostic}} =
               MathOperatorValidator.prepare("+", %{"lspace" => value})

      assert diagnostic.stage == :layout
      assert diagnostic.operation == :prepare_math_operator
      assert diagnostic.source == value
      assert diagnostic.message =~ "operator lspace"
      assert diagnostic.message =~ "CSS"
    end
  end

  test "Chromium ignores calculated operator attributes before metric resolution" do
    for {value, _css_result} <- [
          {"calc(1em + 2px)", 19.5},
          {"min(1em, 2px)", 1.5},
          {"max(1em, 2px)", 18},
          {"clamp(1px, 1em, 2em)", 18},
          {"clamp(2em, 1em, 0px)", 36},
          {"calc((1em - 4pt) / 2 + 3px * 2)", 11.5},
          {"calc(150% - 1pt)", 5},
          {"min(200%, 12px)", 8},
          {"calc(1em * 2em / 4em)", 9},
          {"calc(1rch + 1rcap + 1rex + 1rlh + 1ric)", 52},
          {"calc(1vw + 2vh + 1vmin + 1vmax + 1vi + 1vb)", 26}
        ] do
      assert {:ok, operator} =
               MathOperatorValidator.prepare("+", %{"lspace" => value}, %{
                 font_size: 18,
                 root_zero_advance: 6,
                 root_cap_height: 9,
                 root_x_height: 5,
                 root_line_height: 20,
                 root_ideographic_advance: 12,
                 viewport_width: 600,
                 viewport_height: 200
               })

      assert_in_delta operator.lspace, 18 * 4 / 18, 1.0e-10
    end
  end

  test "malformed and dimensionally invalid calculations use Core dictionary fallback" do
    for value <- [
          "calc(1em+2px)",
          "calc(1em -2px)",
          "calc(1em + 2)",
          "calc(1em * 2px)",
          "calc(1em / 2px)",
          "calc(2)",
          "min()",
          "clamp(1em, 2em)",
          "calc(1bogus + 2pt)",
          "calc(1em) trailing",
          "calc(1em +)"
        ] do
      assert {:ok, operator} = MathOperatorValidator.prepare("+", %{"lspace" => value})
      assert_in_delta operator.lspace, 12 * 4 / 18, 1.0e-10
    end
  end

  test "ignored calculated attributes consume no calculation budget and need no font metrics" do
    Limits.install(Map.put(Limits.effective(), :max_css_calculation_depth, 2))

    for value <- ["calc(1em / 0)", "calc(1em + 1rch)", "calc(min(max(1em, 2em), 3em))"] do
      assert {:ok, operator} = MathOperatorValidator.prepare("+", %{"lspace" => value})
      assert_in_delta operator.lspace, 12 * 4 / 18, 1.0e-10
    end
  end
end
