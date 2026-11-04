defmodule NativeElixirPdfUtilities.Validators.CssSizingValidator do
  @moduledoc "Owns shared calculated box dimensions and computed root sizing metadata."
  alias NativeElixirPdfUtilities.Diagnostics

  alias NativeElixirPdfUtilities.Validators.{
    CssMathValidator,
    CssPositionValidator,
    HtmlValidator
  }

  @dimension_properties [:width, :height, :min_width, :max_width, :min_height, :max_height]

  @properties [
    :calculation_context,
    :width,
    :min_width,
    :max_width,
    :height,
    :min_height,
    :max_height,
    :box_sizing,
    :padding,
    :margin,
    :border_widths,
    :border_styles,
    :border_colors,
    :border_color,
    :color,
    :border_radius,
    :margin_inline_auto,
    :margin_block_auto
  ]

  @typedoc "A validated calculation whose percentage dependency remains tied to the containing axis."
  @type calculation :: {:css_size_math, {String.t(), boolean()}}

  @doc "Prepares general box size arithmetic through the shared CSS calculation grammar."
  @spec prepare_math(term()) ::
          {:ok, calculation()} | :error | {:error, Diagnostics.detailed_error()}
  def prepare_math(value) do
    with value when is_binary(value) <- value,
         true <- String.valid?(value),
         normalized = NativeElixirPdfUtilities.HtmlToPdf.CssParser.strip_comments(value),
         {:ok, _identifier, "(" <> _} <-
           HtmlValidator.parse_css_identifier(String.trim(normalized)),
         {:ok, dependencies} <- CssMathValidator.length_dependencies(value) do
      {:ok, {:css_size_math, {value, dependencies.percentage?}}}
    else
      {:error, _} = error -> error
      _ -> :error
    end
  end

  @doc "Validates prepared box calculations including their genuine percentage dependency."
  @spec valid_calculation?(term()) :: boolean()
  def valid_calculation?(value) do
    case value do
      {:css_size_math, {source, percentage?}} ->
        prepare_math(source) == {:ok, {:css_size_math, {source, percentage?}}}

      _ ->
        false
    end
  end

  @doc "Checks calculation metadata in externally supplied computed styles before used-size resolution."
  @spec validate_calculations(map(), String.t()) :: :ok | {:error, Diagnostics.detailed_error()}
  def validate_calculations(style, source) do
    style
    |> Map.take(@dimension_properties)
    |> Enum.reduce_while(:ok, fn {property, value}, :ok ->
      calculated? =
        is_tuple(value) and tuple_size(value) > 0 and elem(value, 0) in [:css_size_math, :min]

      if calculated? and HtmlValidator.validate_prepared_css_size(value, property) != :ok do
        {:halt,
         Diagnostics.error(
           :layout,
           :invalid_layout,
           "#{property} requires a validated CSS length calculation and its original percentage dependency",
           operation: :layout,
           module: __MODULE__,
           source: source
         )}
      else
        {:cont, :ok}
      end
    end)
  end

  @doc "Resolves box calculations against selected font metrics and the actual containing dimension."
  @spec resolve_math(calculation(), number() | nil, map()) ::
          {:ok, number() | nil} | {:error, Diagnostics.detailed_error()}
  def resolve_math({:css_size_math, {source, percentage?}}, basis, style) do
    if percentage? and is_nil(basis) do
      {:ok, nil}
    else
      with {:ok, value} <-
             CssPositionValidator.resolve(
               {:css_position_math, source},
               basis || 0.0,
               Map.get(style, :calculation_context, style)
             ),
           do: {:ok, max(value, 0.0)}
    end
  end

  @doc "Retains computed root dimensions without changing document canvas paint semantics."
  @spec prepare_root(map() | nil) :: map() | nil
  def prepare_root(style) do
    case style do
      style when is_map(style) ->
        root = Map.take(style, @properties)

        calculated? =
          Enum.any?(
            @dimension_properties,
            fn property ->
              value = Map.get(style, property)

              is_tuple(value) and tuple_size(value) > 0 and
                elem(value, 0) in [:css_size_math, :min]
            end
          )

        if calculated? do
          context =
            Map.take(style, [
              :font_size,
              :font_face,
              :line_height,
              :_root_font_size,
              :_root_font_face,
              :_root_line_height,
              :_position_viewport
            ])

          Map.put(root, :calculation_context, context)
        else
          root
        end

      nil ->
        nil
    end
  end

  @doc "Checks canonical root sizing metadata before layout uses a genuine page or child viewport."
  @spec validate_root(term()) :: :ok | {:error, {atom(), map()}}
  def validate_root(root) do
    HtmlValidator.with_render_budget(fn ->
      valid? =
        case root do
          nil ->
            true

          root when is_map(root) ->
            Enum.all?(Map.keys(root), &(&1 in @properties)) and
              Enum.all?(root, fn {property, value} ->
                case property do
                  :calculation_context ->
                    is_map(value) and
                      match?(
                        {:ok, 1.0},
                        CssPositionValidator.resolve(
                          {:css_position_math, "calc(1pt)"},
                          0.0,
                          value
                        )
                      )

                  :box_sizing ->
                    value in [:content_box, :border_box]

                  flag when flag in [:margin_inline_auto, :margin_block_auto] ->
                    validate_auto_margins(%{flag => value}, "document_root_sizing") == :ok

                  :margin ->
                    is_map(value) and
                      Enum.sort(Map.keys(value)) == [:bottom, :left, :right, :top] and
                      Enum.all?(value, fn {_edge, number} ->
                        is_number(number) and valid_size?(abs(number), 0)
                      end)

                  :border_styles ->
                    is_map(value) and
                      Enum.sort(Map.keys(value)) == [:bottom, :left, :right, :top] and
                      NativeElixirPdfUtilities.Validators.WriterValidator.valid_border_styles?(
                        value
                      )

                  :border_colors ->
                    is_map(value) and
                      Enum.sort(Map.keys(value)) == [:bottom, :left, :right, :top] and
                      NativeElixirPdfUtilities.Validators.WriterValidator.valid_border_colors?(
                        value
                      )

                  color when color in [:border_color, :color] ->
                    NativeElixirPdfUtilities.Validators.WriterValidator.valid_optional_color?(
                      value
                    )

                  :border_radius ->
                    is_number(value) and valid_size?(value, 0)

                  edge when edge in [:padding, :border_widths] ->
                    is_map(value) and
                      Enum.sort(Map.keys(value)) == [:bottom, :left, :right, :top] and
                      Enum.all?(value, fn {_edge, number} ->
                        valid_size?(number, 0) and is_number(number)
                      end)

                  _ ->
                    valid_size?(value, 0)
                end
              end)

          _ ->
            false
        end

      if valid?,
        do: :ok,
        else:
          Diagnostics.error(
            :layout,
            :invalid_layout,
            "document root sizing requires canonical nonnegative CSS dimensions and box edges",
            module: __MODULE__,
            source: "document_root_sizing"
          )
    end)
  end

  @doc "Resolves final authored automatic margins numerically while retaining their axis intent."
  @spec prepare_auto_margins(map()) :: map()
  def prepare_auto_margins(style) do
    case Map.get(style, :margin) do
      %{top: top, right: right, bottom: bottom, left: left} = margin
      when top == :auto or right == :auto or bottom == :auto or left == :auto ->
        margin =
          Map.new(margin, fn {side, value} -> {side, if(value == :auto, do: 0.0, else: value)} end)

        style = style |> Map.put(:margin, margin) |> Map.put(:margin_after, margin.bottom)

        style =
          if left == :auto or right == :auto,
            do:
              Map.put(style, :margin_inline_auto, %{left: left == :auto, right: right == :auto}),
            else: style

        if top == :auto or bottom == :auto,
          do: Map.put(style, :margin_block_auto, %{top: top == :auto, bottom: bottom == :auto}),
          else: style

      _ ->
        style
    end
  end

  @doc "Validates canonical automatic-margin flags for computed styles and root sizing."
  @spec validate_auto_margins(map(), String.t()) :: :ok | {:error, {atom(), map()}}
  def validate_auto_margins(style, source) do
    Enum.reduce_while(
      [{:margin_inline_auto, [:left, :right]}, {:margin_block_auto, [:top, :bottom]}],
      :ok,
      fn {property, sides}, :ok ->
        case Map.fetch(style, property) do
          :error ->
            {:cont, :ok}

          {:ok, flags} ->
            if is_map(flags) and Enum.sort(Map.keys(flags)) == Enum.sort(sides) and
                 Enum.all?(flags, fn {_side, flag} -> is_boolean(flag) end),
               do: {:cont, :ok},
               else:
                 {:halt,
                  Diagnostics.error(
                    :layout,
                    :invalid_layout,
                    "#{property} requires exactly boolean #{Enum.join(sides, " and ")} entries; use validated automatic-margin intent",
                    operation: :layout,
                    module: __MODULE__,
                    source: source
                  )}
        end
      end
    )
  end

  @doc false
  @spec margin_flags(map(), :width | :height) :: {boolean(), boolean()}
  def margin_flags(style, axis) do
    {property, start, finish} =
      case axis do
        :width -> {:margin_inline_auto, :left, :right}
        :height -> {:margin_block_auto, :top, :bottom}
      end

    flags = Map.get(style, property, %{})
    {Map.get(flags, start, false), Map.get(flags, finish, false)}
  end

  @doc "Computes the starting automatic margin from actual free space for a validated axis."
  @spec auto_margin_offset(map(), :width | :height, number(), :flow | :positioned) :: number()
  def auto_margin_offset(style, axis, free, policy) do
    free = if policy == :flow, do: max(free, 0.0), else: free

    case margin_flags(style, axis) do
      {true, true} ->
        if policy == :positioned and axis == :width and free < 0, do: 0.0, else: free / 2

      {true, false} ->
        free

      _ ->
        0.0
    end
  end

  defp valid_size?(size, depth) do
    HtmlValidator.check_render_resource(:max_media_query_depth, depth, :layout)

    case size do
      size when size in [nil, :auto] ->
        true

      {:percent, ratio} ->
        valid_size?(ratio, depth + 1) and is_number(ratio)

      {:css_size_math, _} = calculation ->
        valid_calculation?(calculation)

      {:min, sizes} when is_list(sizes) and sizes != [] ->
        HtmlValidator.reserve_render_resource(:max_css_work, length(sizes), :layout)
        Enum.all?(sizes, &valid_size?(&1, depth + 1))

      number when is_number(number) and number >= 0 ->
        HtmlValidator.check_render_resource(:max_css_numeric_magnitude, number, :layout)
        match?({:ok, _}, HtmlValidator.normalize_geometry_number(number))

      _ ->
        false
    end
  end
end
