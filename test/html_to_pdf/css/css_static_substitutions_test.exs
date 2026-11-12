defmodule NativeElixirPdfUtilities.HtmlToPdf.CssStaticSubstitutionsTest do
  use ExUnit.Case
  alias NativeElixirPdfUtilities.TestSupport.CssStyles
  alias NativeElixirPdfUtilities.Validators.{CssSubstitutionValidator, HtmlValidator}

  # CSS Values 5 editor draft 2da1cb17a43306d3ef80d09d328883c71cdb54b2, attr().
  # CSS Environment Variables 1, 23 September 2025, env() and safe-area variables.
  test "attribute variable expansion failures survive custom-property preparation" do
    alias NativeElixirPdfUtilities.Limits
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(Map.put(original, :max_decoded_bytes, 1))

    context = %{
      _css_attributes: %{"value" => "var(--text)"},
      _custom_properties: %{"--text" => "ABCDE"}
    }

    for operation <- [
          fn -> CssSubstitutionValidator.resolve("attr(value type(*))", context) end,
          fn ->
            CssSubstitutionValidator.prepare_properties(
              %{"--result" => "attr(value type(*))", "--text" => "ABCDE"},
              context,
              HtmlValidator.new_css_budget()
            )
          end
        ] do
      assert {:error, {:resource_limit_exceeded, diagnostic}} = operation.()
      assert diagnostic.stage == :limits
      assert diagnostic.message =~ "max_decoded_bytes"
    end
  end

  test "typed attributes, raw strings and fallbacks use the originating element" do
    styles =
      CssStyles.by_id(
        "<div id='parent' data-size='24' data-gap='2pt' style='--size:attr(data-size pt);width:var(--size);padding:attr(data-gap type(<length>), 1pt)'><span id='child' data-size='99' style='width:var(--size);height:attr(missing type(<length>), 10pt)'>Text</span></div>"
      )

    assert styles["parent"].width == 24
    assert styles["parent"].padding.left == 2
    assert styles["child"].width == 24
    assert styles["child"].height == 10
    context = %{_css_attributes: %{"data-text" => "a\"b\nvar(--x)"}}
    assert {:ok, raw} = CssSubstitutionValidator.resolve("attr(data-text)", context)
    assert raw =~ "\\\""
    assert raw =~ "\\a "
    assert {:ok, _} = CssSubstitutionValidator.resolve("attr(missing)", context)
  end

  test "invalid typed attributes default without resurrecting an earlier declaration" do
    styles =
      CssStyles.by_id(
        "<div id='value' data-size='bad' style='width:10pt;width:attr(data-size type(<length>));height:attr(data-size type(<length>), 3pt)'></div>"
      )

    assert styles["value"].width == :auto
    assert styles["value"].height == 3
  end

  test "environment values are deterministic and unknown values choose only their fallback" do
    context = %{_custom_properties: %{}}
    budget = HtmlValidator.new_css_budget()

    assert {:ok, value} =
             CssSubstitutionValidator.resolve_value(
               "calc(2pt + env(safe-area-inset-left))",
               context,
               budget
             )

    assert value == "calc(2pt + 0px)"
    assert {:ok, "1"} = CssSubstitutionValidator.resolve("env(preferred-text-scale)", context)

    assert {:ok, "3pt"} =
             CssSubstitutionValidator.resolve("env(viewport-segment-width 0 0, 3pt)", context)

    assert :error = CssSubstitutionValidator.resolve("env(viewport-segment-width 0 0)", context)
    assert :error = CssSubstitutionValidator.resolve("env(unknown)", context)

    assert {:ok, "0px"} =
             CssSubstitutionValidator.resolve_value(
               "env(safe-area-inset-top, var(--missing))",
               context,
               budget
             )
  end

  test "attribute values and variable fallbacks cannot create resource URLs" do
    for value <- ["url(secret.png)", "url(\"secret.png\")", "var(--resource)"] do
      context = %{
        _css_attributes: %{"data-value" => value},
        _custom_properties: %{"--resource" => "url(secret.png)"}
      }

      assert :error = CssSubstitutionValidator.resolve("attr(data-value type(*))", context)

      assert :error =
               CssSubstitutionValidator.resolve(
                 "attr(missing type(<length>), var(--resource))",
                 context
               )
    end

    assert :error =
             CssSubstitutionValidator.resolve("attr(data-value type(<url>))", %{
               _css_attributes: %{"data-value" => "url(secret.png)"}
             })
  end

  test "syntax inspection and typed scalar, percentage and string values share substitution grammar" do
    for value <- [
          "(env(safe-area-inset-left))",
          "calc(attr(data-size number) * 1pt)",
          "env(custom 0, 2pt)",
          "attr(data-text type(<string>))"
        ] do
      assert CssSubstitutionValidator.contains?(value)
      assert CssSubstitutionValidator.valid_syntax?(value)
    end

    for value <- [
          "env()",
          "env(default)",
          "env(custom -1)",
          "attr()",
          "attr(name type(<url>))",
          "attr(name number extra)",
          "(env(custom)",
          "env(custom) ["
        ] do
      refute CssSubstitutionValidator.valid_syntax?(value), value
    end

    assert CssSubstitutionValidator.valid_syntax?("(1pt)")
    refute CssSubstitutionValidator.contains?("(1pt)")
    context = %{_css_attributes: %{"N" => "5", "text" => "\"hello\""}, _css_attribute_case: :xml}
    assert {:ok, "5"} = CssSubstitutionValidator.resolve("attr(N number)", context)
    assert {:ok, "5.0%"} = CssSubstitutionValidator.resolve("attr(N %)", context)

    assert {:ok, "\"hello\""} =
             CssSubstitutionValidator.resolve("attr(text type(<string>))", context)

    assert {:ok, "7pt"} =
             CssSubstitutionValidator.resolve("attr(text type(<length>), 7pt)", context)

    assert {:ok, "9pt"} = CssSubstitutionValidator.resolve("env(unknown, 9pt)", context)

    assert {:ok, "(0px)"} =
             CssSubstitutionValidator.resolve("(env(safe-area-inset-left))", context)

    assert :error = CssSubstitutionValidator.resolve("env()", context)
    budget = HtmlValidator.new_css_budget()

    assert {:ok, %{"--bad" => nil}} =
             CssSubstitutionValidator.prepare_properties(
               %{"--bad" => "attr(missing number)"},
               context,
               budget
             )

    assert {:ok, nil} =
             CssSubstitutionValidator.resolve_custom_value(
               "attr(missing number)",
               context,
               budget
             )
  end

  test "typed attribute substitutions cannot invoke the caller resource resolver" do
    owner = self()

    for declaration <- [
          "background-image:attr(data-url type(*))",
          "--url:attr(data-url type(*));background-image:var(--url)",
          "background-image:attr(missing type(*), var(--url));--url:url(secret.png)"
        ] do
      assert {:ok, _pdf} =
               NativeElixirPdfUtilities.HtmlToPdf.render(
                 "<div data-url='url(secret.png)' style='width:20pt;height:10pt;#{declaration}'></div>",
                 system_font_discovery: false,
                 asset_resolver: fn request ->
                   send(owner, {:unexpected_resource, request})
                   :not_found
                 end
               )

      refute_received {:unexpected_resource, _}
    end
  end

  test "substitution budgets fail before expanding custom properties or accepting attribute URLs" do
    alias NativeElixirPdfUtilities.Limits
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(Map.put(original, :max_depth, 0))
    context = %{_css_attributes: %{"n" => "1pt"}}

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               CssSubstitutionValidator.prepare_properties(
                 %{"--n" => "attr(n type(<length>))"},
                 context,
                 HtmlValidator.new_css_budget()
               )
             end)

    assert diagnostic.message =~ "max_depth"
    Limits.install(Map.put(original, :max_input_bytes, 4))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               CssSubstitutionValidator.resolve("attr(n)", context)
             end)

    assert diagnostic.message =~ "max_input_bytes"
  end

  test "unused attribute and environment fallbacks retain custom-property cycles" do
    styles =
      CssStyles.by_id("""
        <div id="env" style="--x:env(safe-area-inset-left,var(--x));width:var(--x,5pt)"></div>
        <div id="attr" data-n="8" style="--x:attr(data-n pt,var(--x));width:var(--x,5pt)"></div>
        <div id="valid" style="--x:env(safe-area-inset-left,var(--missing));width:var(--x,5pt)"></div>
      """)

    assert styles["env"].width == 5
    assert styles["attr"].width == 5
    assert styles["valid"].width == 0
  end

  test "typed strings reject unquoted values and variable expansion keeps its cumulative resource diagnostic" do
    context = %{_css_attributes: %{"data-text" => "unquoted text"}}

    assert {:ok, "2pt"} =
             CssSubstitutionValidator.resolve("attr(data-text type(<string>), 2pt)", context)

    alias NativeElixirPdfUtilities.Validators.CssPropertyRegistrationValidator
    assert {:ok, syntax} = CssPropertyRegistrationValidator.syntax("\"<resolution>\"")
    refute CssPropertyRegistrationValidator.matches_syntax?(syntax, "calc(-1dppx)")
    alias NativeElixirPdfUtilities.Limits
    original = Limits.effective()
    on_exit(fn -> Limits.install(original) end)
    Limits.install(Map.put(original, :max_work, 0))

    assert {:error, {:resource_limit_exceeded, diagnostic}} =
             HtmlValidator.with_render_budget(fn ->
               CssSubstitutionValidator.prepare_properties(
                 %{"--n" => "attr(data-n type(*))"},
                 %{
                   _css_attributes: %{"data-n" => "var(--value)"},
                   _custom_properties: %{"--value" => "1pt"}
                 },
                 HtmlValidator.new_css_budget()
               )
             end)

    assert diagnostic.message =~ "max_work"
  end

  test "unapplied substitutions validate grammar without borrowing an element's attributes" do
    alias NativeElixirPdfUtilities.HtmlToPdf.CssParser
    alias NativeElixirPdfUtilities.Validators.{CssValueValidator, HtmlCssSourceValidator}

    for value <- ["attr(data-width pt)", "env(unknown, 20pt)"] do
      css = "width:" <> value

      assert {:ok, [record]} =
               CssParser.prepare_source_declarations(css, HtmlCssSourceValidator.local(css))

      assert {:ok,
              %{kind: :substitution_tokens, dependencies: [:element_attributes_or_environment]}} =
               CssValueValidator.prepare_unapplied_declaration(
                 record,
                 HtmlValidator.new_css_budget()
               )
    end

    css = "width:attr()"

    assert {:ok, [record]} =
             CssParser.prepare_source_declarations(css, HtmlCssSourceValidator.local(css))

    assert {:error, {:invalid_css, diagnostic}} =
             CssValueValidator.prepare_unapplied_declaration(
               record,
               HtmlValidator.new_css_budget()
             )

    assert diagnostic.source == css and diagnostic.line == 1
  end
end
