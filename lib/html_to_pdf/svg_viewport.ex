defmodule NativeElixirPdfUtilities.HtmlToPdf.SvgViewport do
  @moduledoc false
  alias NativeElixirPdfUtilities.Validators.SvgViewportValidator

  @doc "Measures an SVG viewport against canonical host dimensions without painting or clamping zero geometry."
  @spec measure(term(), term(), term()) :: {:ok, map()} | {:error, {atom(), map()}}
  def measure(attributes, style, host) do
    with {:ok, request} <- SvgViewportValidator.prepare(attributes, style, host) do
      result = measure_request(request)
      SvgViewportValidator.measured_geometry(result)
    end
  rescue
    ArithmeticError ->
      NativeElixirPdfUtilities.Diagnostics.error(
        :layout,
        :invalid_document,
        "SVG viewport arithmetic exceeds the numeric range; reduce size or viewBox scale",
        module: __MODULE__,
        operation: :measure,
        source: "svg viewport"
      )
  end

  defp measure_request(request) do
    nested? = Map.get(request.host, :nested, false)
    unit_scale = if nested?, do: 1.0, else: 0.75
    intrinsic_width = intrinsic_length(request.width, request, unit_scale)
    intrinsic_height = intrinsic_length(request.height, request, unit_scale)

    ratio =
      cond do
        is_number(intrinsic_width) and is_number(intrinsic_height) and intrinsic_height > 0 ->
          intrinsic_width / intrinsic_height

        match?({_, _, width, height} when width > 0 and height > 0, request.view_box) ->
          {_, _, width, height} = request.view_box
          width / height

        true ->
          nil
      end

    width = resolve_length(request.width, request.host.width, request, unit_scale)
    height = resolve_length(request.height, request.host.height, request, unit_scale)
    width = if nested?, do: width || request.host.width, else: width
    height = if nested?, do: height || request.host.height, else: height

    {width, height} =
      replaced_size(width, height, ratio, unit_scale)

    {intrinsic_contribution, _height_contribution} =
      if nested?,
        do: {width, height},
        else: replaced_size(intrinsic_width, intrinsic_height, ratio, unit_scale)

    x = if nested?, do: resolve_length(request.x, request.host.width, request, 1.0), else: 0.0
    y = if nested?, do: resolve_length(request.y, request.host.height, request, 1.0), else: 0.0

    zero_view_box? =
      match?({_, _, width, height} when width == 0 or height == 0, request.view_box)

    paint? = width > 0 and height > 0 and not zero_view_box?

    {ctm, user_width, user_height} =
      case {request.view_box, paint?} do
        {{min_x, min_y, vb_width, vb_height}, true} ->
          {align_x, align_y, mode} = request.aspect
          sx = width / vb_width
          sy = height / vb_height

          {sx, sy} =
            case mode do
              :none -> {sx, sy}
              :meet -> {min(sx, sy), min(sx, sy)}
              :slice -> {max(sx, sy), max(sx, sy)}
            end

          {{sx, 0.0, 0.0, sy, x - min_x * sx + (width - vb_width * sx) * align_x,
            y - min_y * sy + (height - vb_height * sy) * align_y}, vb_width, vb_height}

        _ ->
          {{unit_scale, 0.0, 0.0, unit_scale, x, y}, width / unit_scale, height / unit_scale}
      end

    %{
      width: width,
      height: height,
      x: x,
      y: y,
      layout_extent: {0.0, 0.0, width, height},
      intrinsic_width: intrinsic_width,
      intrinsic_height: intrinsic_height,
      intrinsic_ratio: ratio,
      intrinsic_widths: %{min: intrinsic_contribution, max: intrinsic_contribution},
      baseline: height,
      baseline_from_bottom: 0.0,
      units: if(nested?, do: :user, else: :points),
      coordinate_system: :svg_top_left,
      ctm: ctm,
      user_viewport: {user_width, user_height},
      view_box: request.view_box,
      clip:
        if(request.overflow in [:hidden, :scroll, :auto], do: {x, y, width, height}, else: nil),
      paint?: paint?,
      overflow: request.overflow
    }
  end

  # Both used geometry and intrinsic contributions follow CSS replaced-object
  # sizing. Nested SVG auto axes resolve against their parent viewport first.
  defp replaced_size(width, height, ratio, scale) do
    case {width, height} do
      {nil, nil} ->
        # CSS default object size is a fixed 300×150 CSS pixels.
        if ratio,
          do: {min(300.0, 150.0 * ratio) * scale, min(150.0, 300.0 / ratio) * scale},
          else: {300.0 * scale, 150.0 * scale}

      {nil, height} ->
        {if(ratio, do: height * ratio, else: 300.0 * scale), height}

      {width, nil} ->
        {width, if(ratio, do: width / ratio, else: 150.0 * scale)}

      {width, height} ->
        {width, height}
    end
  end

  defp intrinsic_length(length, request, scale) do
    case length do
      :auto -> nil
      {:percent, _} -> nil
      _ -> resolve_length(length, nil, request, scale)
    end
  end

  defp resolve_length(length, base, request, scale) do
    case length do
      :auto -> nil
      {:percent, fraction} -> if is_number(base), do: base * fraction, else: nil
      {:user, number} -> number * scale
      {:points, number} -> if(scale == 1.0, do: number / 0.75, else: number)
      {:svg_em, number} -> number * request.font_size * scale / 0.75
      {:svg_rem, number} -> number * request.root_font_size * scale / 0.75
    end
  end
end
