defmodule NativeElixirPdfUtilities.Validators.CssPageValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry
  alias NativeElixirPdfUtilities.Validators.HtmlValidator

  @page_context_properties [
    "background",
    "background-attachment",
    "background-color",
    "background-image",
    "background-position",
    "background-repeat",
    "bleed",
    "border",
    "border-bottom",
    "border-bottom-color",
    "border-bottom-style",
    "border-bottom-width",
    "border-color",
    "border-left",
    "border-left-color",
    "border-left-style",
    "border-left-width",
    "border-right",
    "border-right-color",
    "border-right-style",
    "border-right-width",
    "border-style",
    "border-top",
    "border-top-color",
    "border-top-style",
    "border-top-width",
    "border-width",
    "color",
    "counter-increment",
    "counter-reset",
    "counter-set",
    "direction",
    "font",
    "font-family",
    "font-size",
    "math-style",
    "math-shift",
    "math-depth",
    "font-style",
    "font-variant",
    "font-weight",
    "height",
    "letter-spacing",
    "line-height",
    "margin",
    "margin-bottom",
    "margin-left",
    "margin-right",
    "margin-top",
    "marks",
    "max-height",
    "max-width",
    "min-height",
    "min-width",
    "outline",
    "outline-color",
    "outline-style",
    "outline-width",
    "padding",
    "padding-bottom",
    "padding-left",
    "padding-right",
    "padding-top",
    "page-orientation",
    "quotes",
    "size",
    "text-align",
    "text-decoration",
    "text-indent",
    "text-transform",
    "visibility",
    "white-space",
    "width",
    "word-spacing"
  ]
  @page_size_names ~w(a5 a4 a3 b5 b4 jis-b5 jis-b4 letter legal ledger)
  @page_length_units ~w(cm mm q in pc pt px em ex ch rem lh rlh vw vh vmin vmax)
  @page_number_pattern "(?:\\d+(?:\\.\\d*)?|\\.\\d+)(?:e[+-]?\\d+)?"
  @page_length_pattern Enum.join(@page_length_units, "|")
  @page_length_regex Regex.compile!(
                       "^[+-]?#{@page_number_pattern}(?:#{@page_length_pattern})$",
                       "u"
                     )
  @page_nonnegative_length_regex Regex.compile!(
                                   "^\\+?#{@page_number_pattern}(?:#{@page_length_pattern})$",
                                   "u"
                                 )
  @page_percentage_regex Regex.compile!("^[+-]?#{@page_number_pattern}%$", "u")
  @css_wide_keywords ~w(initial inherit unset revert revert-layer)
  @doc false
  @spec validate_declaration(NativeElixirPdfUtilities.HtmlToPdf.CssParser.declaration()) ::
          {:ok, map()} | :error
  def validate_declaration(declaration) do
    {property, value} =
      case declaration do
        {property, value} -> {property, value}
        {property, value, :important} -> {property, value}
      end

    with {:ok, option} <- declaration_option(property, value) do
      policy =
        cond do
          not is_nil(option) ->
            :checked_static_page_value

          property not in ~w(size margin margin-top margin-right margin-bottom margin-left page-orientation marks bleed) ->
            :requires_page_context

          valid_page_function?(String.downcase(String.trim(value))) ->
            :requires_page_context

          true ->
            :checked_page_descriptor_syntax
        end

      {:ok, %{option: option, policy: policy}}
    end
  end

  @doc false
  @spec declaration_option(String.t(), String.t()) ::
          {:ok, nil | {:page_size | :margin, term()} | {:margin_side, atom(), term()}} | :error
  def declaration_option(property, value) do
    case property do
      "size" ->
        case valid_page_size?(value) do
          true ->
            case page_size_option(value) do
              nil -> {:ok, nil}
              page_size -> {:ok, {:page_size, page_size}}
            end

          false ->
            :error
        end

      "margin" ->
        case valid_page_margin?(value, 4) do
          true ->
            case page_margin_option(value) do
              nil -> {:ok, nil}
              margin -> {:ok, {:margin, margin}}
            end

          false ->
            :error
        end

      property when property in ["margin-top", "margin-right", "margin-bottom", "margin-left"] ->
        case valid_page_margin?(value, 1) do
          true ->
            case PageGeometry.css_margin_option(value) do
              nil -> {:ok, nil}
              margin -> {:ok, {:margin_side, page_margin_side(property), margin}}
            end

          false ->
            :error
        end

      "page-orientation" ->
        case value |> String.trim() |> String.downcase() do
          value when value in ["upright", "rotate-left", "rotate-right"] -> {:ok, nil}
          value when value in @css_wide_keywords -> {:ok, nil}
          value -> if valid_page_function?(value), do: {:ok, nil}, else: :error
        end

      "marks" ->
        marks = value |> String.trim() |> String.downcase() |> String.split(~r/\s+/u, trim: true)

        case marks do
          ["none"] ->
            {:ok, nil}

          [value] when value in @css_wide_keywords ->
            {:ok, nil}

          [value] ->
            if valid_page_function?(value), do: {:ok, nil}, else: page_marks_option(marks)

          marks when length(marks) in 1..2 ->
            page_marks_option(marks)

          _ ->
            :error
        end

      "bleed" ->
        case value |> String.trim() |> String.downcase() do
          "auto" ->
            {:ok, nil}

          value when value in @css_wide_keywords ->
            {:ok, nil}

          value ->
            if valid_page_length?(value, true) or valid_page_function?(value),
              do: {:ok, nil},
              else: :error
        end

      property ->
        case property in @page_context_properties and String.trim(value) != "" do
          true -> {:ok, nil}
          false -> :error
        end
    end
  end

  @margin_boxes ~w(top-left-corner top-left top-center top-right top-right-corner bottom-left-corner bottom-left bottom-center bottom-right bottom-right-corner left-top left-middle left-bottom right-top right-middle right-bottom)

  @doc "Returns the sixteen fixed CSS page-margin box names."
  @spec margin_boxes() :: [String.t()]
  def margin_boxes, do: @margin_boxes

  @doc "Prepares named page selectors, page descriptors and nested margin boxes."
  @spec rules(String.t()) :: {:ok, [map()]} | {:error, term()}
  def rules(css) do
    if not Regex.match?(~r/@page/iu, css) do
      {:ok, []}
    else
      alias NativeElixirPdfUtilities.Validators.{CssSyntaxValidator, CssCounterStyleValidator}

      css
      |> CssSyntaxValidator.rules()
      |> Enum.filter(&Regex.match?(~r/^@page(?:\s|:|$)/iu, String.trim(&1.selectors)))
      |> Enum.reduce_while({:ok, []}, fn rule, {:ok, pages} ->
        selector =
          rule.selectors |> String.trim() |> String.slice(5..-1//1) |> String.trim()

        parsed = Enum.map(CssSyntaxValidator.split_trimmed(selector, ","), &page_selector/1)

        items = CssSyntaxValidator.block_items(rule.declarations)

        declarations =
          CssSyntaxValidator.declarations(rule.declarations) |> Enum.map(& &1.declaration)

        boxes = Enum.filter(items, &(&1.kind == :rule))

        valid =
          Enum.all?(parsed, &is_map/1) and
            Enum.all?(declarations, &match?({:ok, _}, validate_declaration(&1))) and
            Enum.all?(boxes, fn box ->
              String.trim_leading(String.trim(box.selectors), "@") in (@margin_boxes ++
                                                                         ["footnote"])
            end)

        if valid do
          margins =
            Enum.reduce(boxes, %{}, fn box, definitions ->
              name = String.trim_leading(String.trim(box.selectors), "@")

              declarations =
                CssSyntaxValidator.declarations(box.declarations) |> Enum.map(& &1.declaration)

              Map.update(definitions, name, declarations, &(&1 ++ declarations))
            end)

          {:cont,
           {:ok,
            pages ++
              [
                %{
                  selectors: parsed,
                  declarations: declarations,
                  margin_boxes: margins,
                  source: rule.source
                }
              ]}}
        else
          source = String.trim(rule.selectors)

          offset =
            rule.offset + byte_size(rule.selectors) -
              byte_size(String.trim_leading(rule.selectors))

          prefix = binary_part(css, 0, offset)
          lines = String.split(prefix, "\n")
          line = length(lines)
          column = String.length(List.last(lines)) + 1

          {:halt,
           NativeElixirPdfUtilities.Diagnostics.error(
             :css,
             :invalid_css,
             ~s(line #{line}: page rule "#{source}" is invalid or unsupported),
             module: __MODULE__,
             source: source,
             line: line,
             column: column
           )}
        end
      end)
    end
  end

  defp page_selector(source) do
    alias NativeElixirPdfUtilities.Validators.{CssCounterStyleValidator, CssSyntaxValidator}

    [name | pseudos] = CssSyntaxValidator.split_trimmed(source, ":")

    pseudos =
      Enum.map(pseudos, fn
        name when name in ~w(first left right blank) ->
          name

        source ->
          with {:ok, body, ""} <- CssSyntaxValidator.function(":" <> source, "nth"),
               [expression | groups] <- Regex.split(~r/\s+of\s+/i, String.trim(body), parts: 2),
               group = List.first(groups) || "",
               true <- group == "" or CssCounterStyleValidator.identifier?(group),
               {:ok, {:nth_child, series}, ""} <-
                 HtmlValidator.parse_css_nth_pseudo_class(":nth-child(" <> expression <> ")") do
            {:nth, series, group}
          else
            _ -> nil
          end
      end)

    if (name == "" or (name != "auto" and CssCounterStyleValidator.identifier?(name))) and
         Enum.all?(pseudos, &(not is_nil(&1))) do
      priority = Enum.count(pseudos, &(&1 in ["first", "blank"] or match?({:nth, _, _}, &1)))
      side = Enum.count(pseudos, &(&1 in ["left", "right"]))

      %{
        name: name,
        pseudo: pseudos,
        specificity: {if(name == "", do: 0, else: 1), priority, side}
      }
    end
  end

  @doc "Returns the page-group names that referenced positions need to track."
  @spec group_names([map()]) :: MapSet.t()
  def group_names(rules) do
    MapSet.new(
      for rule <- rules,
          selector <- rule.selectors,
          {:nth, _, group} <- selector.pseudo,
          group != "",
          do: group
    )
  end

  @doc "Checks for positional rules with descriptors that can change the layout of a later page."
  @spec positional_geometry?([map()]) :: boolean()
  def positional_geometry?(rules) do
    Enum.any?(rules, fn rule ->
      Enum.any?(rule.selectors, fn selector ->
        Enum.any?(selector.pseudo, &match?({:nth, _, _}, &1))
      end) and
        Enum.any?(rule.declarations, fn declaration ->
          property = elem(declaration, 0)

          property in ~w(size page-orientation line-height) or
            String.starts_with?(property, ["margin", "padding", "border", "font"])
        end)
    end)
  end

  @doc "Cascades matching page rules by page specificity, importance and source order."
  @spec select([map()], String.t(), pos_integer(), boolean(), :ltr | :rtl | PageGeometry.side()) ::
          map()
  @spec select(
          [map()],
          String.t(),
          pos_integer(),
          boolean(),
          :ltr | :rtl | PageGeometry.side(),
          map()
        ) :: map()
  def select(rules, name, number, blank, direction \\ :ltr, group_positions \\ %{}) do
    first = if direction in [:rtl, :left], do: :left, else: :right
    side = Atom.to_string(PageGeometry.page_side(number, first))

    {page, boxes} =
      rules
      |> Enum.with_index()
      |> Enum.reduce({%{}, %{}}, fn {rule, order}, {page, boxes} ->
        matching =
          Enum.filter(rule.selectors, fn selector ->
            selector.name in ["", name] and
              Enum.all?(selector.pseudo, fn
                "first" ->
                  number == 1

                "blank" ->
                  blank

                {:nth, index, ""} ->
                  HtmlValidator.css_nth_matches?(index, number)

                {:nth, index, group} ->
                  Enum.any?(
                    Map.get(group_positions, group, []),
                    &HtmlValidator.css_nth_matches?(index, &1)
                  )

                pseudo ->
                  pseudo == side
              end)
          end)

        if matching == [] do
          {page, boxes}
        else
          specificity = matching |> Enum.map(& &1.specificity) |> Enum.max()

          merge = fn declarations, old ->
            Enum.reduce(Enum.with_index(declarations), old, fn {declaration, declaration_order},
                                                               acc ->
              {property, value, important} =
                case declaration do
                  {p, v} -> {p, v, false}
                  {p, v, :important} -> {p, v, true}
                end

              priority = {important, specificity, order, declaration_order}

              case Map.get(acc, property) do
                {previous, _} when previous > priority -> acc
                _ -> Map.put(acc, property, {priority, {property, value}})
              end
            end)
          end

          {merge.(rule.declarations, page),
           Enum.reduce(rule.margin_boxes, boxes, fn {name, declarations}, boxes ->
             Map.update(boxes, name, merge.(declarations, %{}), &merge.(declarations, &1))
           end)}
        end
      end)

    values = fn map ->
      map
      |> Enum.sort_by(fn {_p, {priority, _}} -> priority end)
      |> Enum.map(fn {_p, {_priority, d}} -> d end)
    end

    resource = fn
      nil -> nil
      {priority, _} -> rules |> Enum.at(elem(priority, 2)) |> Map.get(:resource_context)
    end

    background = fn declarations ->
      resource.(
        max(Map.get(declarations, "background-image"), Map.get(declarations, "background"))
      )
    end

    %{
      declarations: values.(page),
      resource_context: background.(page),
      margin_boxes: Map.new(boxes, fn {name, d} -> {name, values.(d)} end),
      margin_contexts:
        Map.new(boxes, fn {name, declarations} ->
          {name,
           %{
             content: resource.(Map.get(declarations, "content")),
             background: background.(declarations)
           }}
        end)
    }
  end

  @doc "Checks optional PDF trim and bleed rectangles against the physical sheet."
  @spec valid_pdf_boxes?(map()) :: boolean()
  def valid_pdf_boxes?(page) do
    Enum.all?([:trim_box, :bleed_box], fn key ->
      case Map.fetch(page, key) do
        :error ->
          true

        {:ok, {left, bottom, right, top}} ->
          Enum.all?([left, bottom, right, top], &is_number/1) and
            left >= 0 and bottom >= 0 and right > left and top > bottom and
            right <= elem(page.size, 0) and top <= elem(page.size, 1)

        _ ->
          false
      end
    end)
  end

  @doc "Resolves page descriptors using selected metrics and the physical page percentage bases."
  @spec resolve_geometry(map(), keyword(), keyword(), map()) ::
          {:ok, keyword()} | {:error, term()}
  def resolve_geometry(selected, opts, explicit, style) do
    descriptors = Map.new(selected.declarations)
    size = Map.get(descriptors, "size", "auto")

    with {:ok, size} <- resolve_size(size, Keyword.get(opts, :page_size, :a4), style),
         size = Keyword.get(explicit, :page_size, size),
         {:ok, {width, height}} <- PageGeometry.normalize_page_size(size),
         {:ok, margins} <- PageGeometry.normalize_margins(Keyword.get(opts, :margin, 0)),
         {:ok, margins} <- resolve_margins(selected.declarations, margins, width, height, style) do
      {:ok,
       opts
       |> Keyword.put(:page_size, size)
       |> Keyword.put(:margin, margins)
       |> Keyword.merge(Keyword.take(explicit, [:page_size, :margin]))}
    else
      {:error, _} = error ->
        error

      _ ->
        NativeElixirPdfUtilities.Diagnostics.error(
          :css,
          :invalid_css,
          "page dimensions or margins cannot be resolved in the page context",
          source: inspect(descriptors)
        )
    end
  end

  defp resolve_size(value, fallback, style) do
    normalized = value |> String.trim() |> String.downcase()

    case {normalized, page_size_option(value)} do
      {orientation, _} when orientation in ["portrait", "landscape"] ->
        with {:ok, {w, h}} <- PageGeometry.normalize_page_size(fallback) do
          {width, height} =
            if(orientation == "landscape",
              do: {max(w, h), min(w, h)},
              else: {min(w, h), max(w, h)}
            )

          {:ok, "#{width}pt #{height}pt"}
        end

      {value, nil} ->
        cond do
          value in ["auto" | @css_wide_keywords] ->
            {:ok, fallback}

          true ->
            tokens = value_tokens(value)

            case tokens do
              [a] ->
                with {:ok, a} <- page_length(a, 0, style),
                     true <- a > 0,
                     do: {:ok, "#{a}pt #{a}pt"}

              [a, b] ->
                with {:ok, a} <- page_length(a, 0, style),
                     {:ok, b} <- page_length(b, 0, style),
                     true <- a > 0 and b > 0,
                     do: {:ok, "#{a}pt #{b}pt"}

              _ ->
                :error
            end
        end

      {_, size} ->
        {:ok, size}
    end
  end

  defp resolve_margins(declarations, margins, width, height, style) do
    Enum.reduce_while(declarations, {:ok, margins}, fn {property, value}, {:ok, margins} ->
      entries =
        case property do
          "margin" ->
            case value_tokens(value) do
              [a] -> Enum.zip([:top, :right, :bottom, :left], [a, a, a, a])
              [a, b] -> Enum.zip([:top, :right, :bottom, :left], [a, b, a, b])
              [a, b, c] -> Enum.zip([:top, :right, :bottom, :left], [a, b, c, b])
              [a, b, c, d] -> Enum.zip([:top, :right, :bottom, :left], [a, b, c, d])
            end

          p when p in ~w(margin-top margin-right margin-bottom margin-left) ->
            [{page_margin_side(p), value}]

          _ ->
            []
        end

      result =
        Enum.reduce_while(entries, {:ok, margins}, fn {side, source}, {:ok, margins} ->
          basis = if side in [:top, :bottom], do: height, else: width

          case page_length(source, basis, style) do
            {:ok, n} -> {:cont, {:ok, Map.put(margins, side, n)}}
            error -> {:halt, error}
          end
        end)

      case result do
        {:ok, margins} -> {:cont, {:ok, margins}}
        error -> {:halt, error}
      end
    end)
  end

  defp page_length(value, basis, style) do
    if value in ["auto" | @css_wide_keywords],
      do: {:ok, 0.0},
      else:
        NativeElixirPdfUtilities.Validators.CssPositionValidator.resolve(
          {:css_position_math, value},
          basis,
          style
        )
  end

  @doc "Resolves the nonnegative physical bleed amount using page font metrics."
  @spec resolve_bleed(String.t(), [String.t()], map()) :: {:ok, number()} | {:error, term()}
  def resolve_bleed(value, marks, style) do
    normalized = value |> String.trim() |> String.downcase()

    if normalized in ["auto" | @css_wide_keywords] do
      {:ok, if("crop" in marks, do: 6.0, else: 0.0)}
    else
      with {:ok, value} <-
             NativeElixirPdfUtilities.Validators.CssSubstitutionValidator.resolve_value(
               value,
               style,
               HtmlValidator.new_css_budget()
             ),
           {:ok, length} <- page_length(value, 0.0, style),
           true <- length >= 0 do
        {:ok, length}
      else
        _ ->
          NativeElixirPdfUtilities.Diagnostics.error(
            :css,
            :invalid_css,
            "page bleed must resolve to a nonnegative length",
            source: value
          )
      end
    end
  end

  defp value_tokens(value) do
    value
    |> NativeElixirPdfUtilities.Validators.CssSyntaxValidator.components()
    |> Enum.reject(&(String.trim(&1.text) == "" or &1.kind == :comment))
    |> Enum.map(& &1.text)
  end

  defp valid_page_size?(value) do
    normalized = value |> String.trim() |> String.downcase()
    tokens = value_tokens(normalized)

    case tokens do
      ["auto"] ->
        true

      [value] when value in @css_wide_keywords ->
        true

      [orientation] when orientation in ["portrait", "landscape"] ->
        true

      [page_size] ->
        page_size in @page_size_names or valid_page_length?(page_size, false) or
          valid_page_function?(normalized)

      [first, second] ->
        (first in @page_size_names and second in ["portrait", "landscape"]) or
          (second in @page_size_names and first in ["portrait", "landscape"]) or
          ((valid_page_length?(first, false) or valid_page_function?(first)) and
             (valid_page_length?(second, false) or valid_page_function?(second)))

      _ ->
        false
    end
  end

  defp valid_page_margin?(value, maximum_values) do
    normalized = value |> String.trim() |> String.downcase()

    values = value_tokens(normalized)

    length(values) in 1..maximum_values and
      Enum.all?(values, fn value ->
        value == "auto" or value in @css_wide_keywords or
          Regex.match?(@page_percentage_regex, value) or valid_page_length?(value, true) or
          valid_page_function?(value)
      end)
  end

  defp valid_page_length?(value, allow_negative?) do
    length_regex =
      if allow_negative?, do: @page_length_regex, else: @page_nonnegative_length_regex

    value == "0" or Regex.match?(length_regex, value)
  end

  defp valid_page_function?(value) do
    Regex.match?(~r/^(?:calc|min|max|clamp|var|env)\(.+\)$/u, value)
  end

  defp page_marks_option(marks) do
    case Enum.all?(marks, &(&1 in ["crop", "cross"])) and Enum.uniq(marks) == marks do
      true -> {:ok, nil}
      false -> :error
    end
  end

  defp page_size_option(value) do
    PageGeometry.css_page_size_option(value)
  end

  defp page_margin_option(value) do
    PageGeometry.css_margin_option(value)
  end

  defp page_margin_side(property) do
    case property do
      "margin-top" -> :top
      "margin-right" -> :right
      "margin-bottom" -> :bottom
      "margin-left" -> :left
    end
  end
end
