defmodule NativeElixirPdfUtilities.Validators.CssPageValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.HtmlToPdf.PageGeometry

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

  defp valid_page_size?(value) do
    normalized = value |> String.trim() |> String.downcase()
    tokens = String.split(normalized, ~r/\s+/u, trim: true)

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
          (valid_page_length?(first, false) and valid_page_length?(second, false))

      _ ->
        false
    end
  end

  defp valid_page_margin?(value, maximum_values) do
    normalized = value |> String.trim() |> String.downcase()

    values =
      case valid_page_function?(normalized) do
        true -> [normalized]
        false -> String.split(normalized, ~r/\s+/u, trim: true)
      end

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
