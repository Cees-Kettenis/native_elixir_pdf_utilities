defmodule NativeElixirPdfUtilities.Validators.CssColorValidator do
  @moduledoc false

  # Fixed CSS named-color vocabulary, not a configurable resource limit.
  # CSS Color 4, 2025-04-24, section 6.1. Existing channel values remain intact.
  @named_colors %{
    "aliceblue" => {0.9411764705882353, 0.9725490196078431, 1},
    "antiquewhite" => {0.9803921568627451, 0.9215686274509803, 0.8431372549019608},
    "aqua" => {0, 1, 1},
    "aquamarine" => {0.4980392156862745, 1, 0.8313725490196079},
    "azure" => {0.9411764705882353, 1, 1},
    "beige" => {0.9607843137254902, 0.9607843137254902, 0.8627450980392157},
    "bisque" => {1, 0.8941176470588236, 0.7686274509803922},
    "black" => {0, 0, 0},
    "blanchedalmond" => {1, 0.9215686274509803, 0.803921568627451},
    "blue" => {0, 0, 1},
    "blueviolet" => {0.5411764705882353, 0.16862745098039217, 0.8862745098039215},
    "brown" => {0.6470588235294118, 0.16470588235294117, 0.16470588235294117},
    "burlywood" => {0.8705882352941177, 0.7215686274509804, 0.5294117647058824},
    "cadetblue" => {0.37254901960784315, 0.6196078431372549, 0.6274509803921569},
    "chartreuse" => {0.4980392156862745, 1, 0},
    "chocolate" => {0.8235294117647058, 0.4117647058823529, 0.11764705882352941},
    "coral" => {1, 0.4980392156862745, 0.3137254901960784},
    "cornflowerblue" => {0.39215686274509803, 0.5843137254901961, 0.9294117647058824},
    "cornsilk" => {1, 0.9725490196078431, 0.8627450980392157},
    "crimson" => {0.8627450980392157, 0.0784313725490196, 0.23529411764705882},
    "cyan" => {0, 1, 1},
    "darkblue" => {0, 0, 0.5450980392156862},
    "darkcyan" => {0, 0.5450980392156862, 0.5450980392156862},
    "darkgoldenrod" => {0.7215686274509804, 0.5254901960784314, 0.043137254901960784},
    "darkgray" => {0.6627450980392157, 0.6627450980392157, 0.6627450980392157},
    "darkgreen" => {0, 0.39215686274509803, 0},
    "darkgrey" => {0.6627450980392157, 0.6627450980392157, 0.6627450980392157},
    "darkkhaki" => {0.7411764705882353, 0.7176470588235294, 0.4196078431372549},
    "darkmagenta" => {0.5450980392156862, 0, 0.5450980392156862},
    "darkolivegreen" => {0.3333333333333333, 0.4196078431372549, 0.1843137254901961},
    "darkorange" => {1, 0.5490196078431373, 0},
    "darkorchid" => {0.6, 0.19607843137254902, 0.8},
    "darkred" => {0.5450980392156862, 0, 0},
    "darksalmon" => {0.9137254901960784, 0.5882352941176471, 0.47843137254901963},
    "darkseagreen" => {0.5607843137254902, 0.7372549019607844, 0.5607843137254902},
    "darkslateblue" => {0.2823529411764706, 0.23921568627450981, 0.5450980392156862},
    "darkslategray" => {0.1843137254901961, 0.30980392156862746, 0.30980392156862746},
    "darkslategrey" => {0.1843137254901961, 0.30980392156862746, 0.30980392156862746},
    "darkturquoise" => {0, 0.807843137254902, 0.8196078431372549},
    "darkviolet" => {0.5803921568627451, 0, 0.8274509803921568},
    "deeppink" => {1, 0.0784313725490196, 0.5764705882352941},
    "deepskyblue" => {0, 0.7490196078431373, 1},
    "dimgray" => {0.4117647058823529, 0.4117647058823529, 0.4117647058823529},
    "dimgrey" => {0.4117647058823529, 0.4117647058823529, 0.4117647058823529},
    "dodgerblue" => {0.11764705882352941, 0.5647058823529412, 1},
    "firebrick" => {0.6980392156862745, 0.13333333333333333, 0.13333333333333333},
    "floralwhite" => {1, 0.9803921568627451, 0.9411764705882353},
    "forestgreen" => {0.13333333333333333, 0.5450980392156862, 0.13333333333333333},
    "fuchsia" => {1, 0, 1},
    "gainsboro" => {0.8627450980392157, 0.8627450980392157, 0.8627450980392157},
    "ghostwhite" => {0.9725490196078431, 0.9725490196078431, 1},
    "gold" => {1, 0.8431372549019608, 0},
    "goldenrod" => {0.8549019607843137, 0.6470588235294118, 0.12549019607843137},
    "gray" => {0.5019607843, 0.5019607843, 0.5019607843},
    "green" => {0, 0.5019607843, 0},
    "greenyellow" => {0.6784313725490196, 1, 0.1843137254901961},
    "grey" => {0.5019607843, 0.5019607843, 0.5019607843},
    "honeydew" => {0.9411764705882353, 1, 0.9411764705882353},
    "hotpink" => {1, 0.4117647058823529, 0.7058823529411765},
    "indianred" => {0.803921568627451, 0.3607843137254902, 0.3607843137254902},
    "indigo" => {0.29411764705882354, 0, 0.5098039215686274},
    "ivory" => {1, 1, 0.9411764705882353},
    "khaki" => {0.9411764705882353, 0.9019607843137255, 0.5490196078431373},
    "lavender" => {0.9019607843137255, 0.9019607843137255, 0.9803921568627451},
    "lavenderblush" => {1, 0.9411764705882353, 0.9607843137254902},
    "lawngreen" => {0.48627450980392156, 0.9882352941176471, 0},
    "lemonchiffon" => {1, 0.9803921568627451, 0.803921568627451},
    "lightblue" => {0.6784313725490196, 0.8470588235294118, 0.9019607843137255},
    "lightcoral" => {0.9411764705882353, 0.5019607843137255, 0.5019607843137255},
    "lightcyan" => {0.8784313725490196, 1, 1},
    "lightgoldenrodyellow" => {0.9803921568627451, 0.9803921568627451, 0.8235294117647058},
    "lightgray" => {0.8274509803921568, 0.8274509803921568, 0.8274509803921568},
    "lightgreen" => {0.5647058823529412, 0.9333333333333333, 0.5647058823529412},
    "lightgrey" => {0.8274509803921568, 0.8274509803921568, 0.8274509803921568},
    "lightpink" => {1, 0.7137254901960784, 0.7568627450980392},
    "lightsalmon" => {1, 0.6274509803921569, 0.47843137254901963},
    "lightseagreen" => {0.12549019607843137, 0.6980392156862745, 0.6666666666666666},
    "lightskyblue" => {0.5294117647058824, 0.807843137254902, 0.9803921568627451},
    "lightslategray" => {0.4666666666666667, 0.5333333333333333, 0.6},
    "lightslategrey" => {0.4666666666666667, 0.5333333333333333, 0.6},
    "lightsteelblue" => {0.6901960784313725, 0.7686274509803922, 0.8705882352941177},
    "lightyellow" => {1, 1, 0.8784313725490196},
    "lime" => {0, 1, 0},
    "limegreen" => {0.19607843137254902, 0.803921568627451, 0.19607843137254902},
    "linen" => {0.9803921568627451, 0.9411764705882353, 0.9019607843137255},
    "magenta" => {1, 0, 1},
    "maroon" => {0.5019607843137255, 0, 0},
    "mediumaquamarine" => {0.4, 0.803921568627451, 0.6666666666666666},
    "mediumblue" => {0, 0, 0.803921568627451},
    "mediumorchid" => {0.7294117647058823, 0.3333333333333333, 0.8274509803921568},
    "mediumpurple" => {0.5764705882352941, 0.4392156862745098, 0.8588235294117647},
    "mediumseagreen" => {0.23529411764705882, 0.7019607843137254, 0.44313725490196076},
    "mediumslateblue" => {0.4823529411764706, 0.40784313725490196, 0.9333333333333333},
    "mediumspringgreen" => {0, 0.9803921568627451, 0.6039215686274509},
    "mediumturquoise" => {0.2823529411764706, 0.8196078431372549, 0.8},
    "mediumvioletred" => {0.7803921568627451, 0.08235294117647059, 0.5215686274509804},
    "midnightblue" => {0.09803921568627451, 0.09803921568627451, 0.4392156862745098},
    "mintcream" => {0.9607843137254902, 1, 0.9803921568627451},
    "mistyrose" => {1, 0.8941176470588236, 0.8823529411764706},
    "moccasin" => {1, 0.8941176470588236, 0.7098039215686275},
    "navajowhite" => {1, 0.8705882352941177, 0.6784313725490196},
    "navy" => {0, 0, 0.5019607843137255},
    "oldlace" => {0.9921568627450981, 0.9607843137254902, 0.9019607843137255},
    "olive" => {0.5019607843137255, 0.5019607843137255, 0},
    "olivedrab" => {0.4196078431372549, 0.5568627450980392, 0.13725490196078433},
    "orange" => {1, 0.6470588235294118, 0},
    "orangered" => {1, 0.27058823529411763, 0},
    "orchid" => {0.8549019607843137, 0.4392156862745098, 0.8392156862745098},
    "palegoldenrod" => {0.9333333333333333, 0.9098039215686274, 0.6666666666666666},
    "palegreen" => {0.596078431372549, 0.984313725490196, 0.596078431372549},
    "paleturquoise" => {0.6862745098039216, 0.9333333333333333, 0.9333333333333333},
    "palevioletred" => {0.8588235294117647, 0.4392156862745098, 0.5764705882352941},
    "papayawhip" => {1, 0.9372549019607843, 0.8352941176470589},
    "peachpuff" => {1, 0.8549019607843137, 0.7254901960784313},
    "peru" => {0.803921568627451, 0.5215686274509804, 0.24705882352941178},
    "pink" => {1, 0.7529411764705882, 0.796078431372549},
    "plum" => {0.8666666666666667, 0.6274509803921569, 0.8666666666666667},
    "powderblue" => {0.6901960784313725, 0.8784313725490196, 0.9019607843137255},
    "purple" => {0.5019607843137255, 0, 0.5019607843137255},
    "rebeccapurple" => {0.4, 0.2, 0.6},
    "red" => {1, 0, 0},
    "rosybrown" => {0.7372549019607844, 0.5607843137254902, 0.5607843137254902},
    "royalblue" => {0.2549019607843137, 0.4117647058823529, 0.8823529411764706},
    "saddlebrown" => {0.5450980392156862, 0.27058823529411763, 0.07450980392156863},
    "salmon" => {0.9803921568627451, 0.5019607843137255, 0.4470588235294118},
    "sandybrown" => {0.9568627450980393, 0.6431372549019608, 0.3764705882352941},
    "seagreen" => {0.1803921568627451, 0.5450980392156862, 0.3411764705882353},
    "seashell" => {1, 0.9607843137254902, 0.9333333333333333},
    "sienna" => {0.6274509803921569, 0.3215686274509804, 0.17647058823529413},
    "silver" => {0.7529411764705882, 0.7529411764705882, 0.7529411764705882},
    "skyblue" => {0.5294117647058824, 0.807843137254902, 0.9215686274509803},
    "slateblue" => {0.41568627450980394, 0.35294117647058826, 0.803921568627451},
    "slategray" => {0.4392156862745098, 0.5019607843137255, 0.5647058823529412},
    "slategrey" => {0.4392156862745098, 0.5019607843137255, 0.5647058823529412},
    "snow" => {1, 0.9803921568627451, 0.9803921568627451},
    "springgreen" => {0, 1, 0.4980392156862745},
    "steelblue" => {0.27450980392156865, 0.5098039215686274, 0.7058823529411765},
    "tan" => {0.8235294117647058, 0.7058823529411765, 0.5490196078431373},
    "teal" => {0, 0.5019607843137255, 0.5019607843137255},
    "thistle" => {0.8470588235294118, 0.7490196078431373, 0.8470588235294118},
    "tomato" => {1, 0.38823529411764707, 0.2784313725490196},
    "turquoise" => {0.25098039215686274, 0.8784313725490196, 0.8156862745098039},
    "violet" => {0.9333333333333333, 0.5098039215686274, 0.9333333333333333},
    "wheat" => {0.9607843137254902, 0.8705882352941177, 0.7019607843137254},
    "white" => {1, 1, 1},
    "whitesmoke" => {0.9607843137254902, 0.9607843137254902, 0.9607843137254902},
    "yellow" => {1, 1, 0},
    "yellowgreen" => {0.6039215686274509, 0.803921568627451, 0.19607843137254902}
  }

  @doc false
  @spec named_color(String.t()) :: {number(), number(), number()} | nil
  def named_color(normalized_name) do
    Map.get(@named_colors, normalized_name)
  end

  alias NativeElixirPdfUtilities.Validators.{HtmlValidator, CssMathValidator}
  alias NativeElixirPdfUtilities.HtmlToPdf.CssColor

  # Chromium's static light input palette. This does not read a host theme.
  @input_palette %{
    "accentcolor" => 0x000000,
    "accentcolortext" => 0x000000,
    "activetext" => 0xFF0000,
    "buttonborder" => 0x767676,
    "buttonface" => 0xEFEFEF,
    "buttontext" => 0x000000,
    "canvas" => 0xFFFFFF,
    "canvastext" => 0x000000,
    "field" => 0xFFFFFF,
    "fieldtext" => 0x000000,
    "graytext" => 0x808080,
    "highlight" => 0x0041C6,
    "highlighttext" => 0xFFFFFF,
    "linktext" => 0x0000EE,
    "mark" => 0xFFFF00,
    "marktext" => 0x000000,
    "selecteditem" => 0x1967D2,
    "selecteditemtext" => 0xFFFFFF,
    "visitedtext" => 0x551A8B,
    "activeborder" => 0x767676,
    "activecaption" => 0xFFFFFF,
    "appworkspace" => 0xFFFFFF,
    "background" => 0xFFFFFF,
    "buttonhighlight" => 0xEFEFEF,
    "buttonshadow" => 0xEFEFEF,
    "captiontext" => 0x000000,
    "inactiveborder" => 0x767676,
    "inactivecaption" => 0xFFFFFF,
    "inactivecaptiontext" => 0x808080,
    "infobackground" => 0xFFFFFF,
    "infotext" => 0x000000,
    "menu" => 0xFFFFFF,
    "menutext" => 0x000000,
    "scrollbar" => 0xFFFFFF,
    "threeddarkshadow" => 0x767676,
    "threedface" => 0xEFEFEF,
    "threedhighlight" => 0x767676,
    "threedlightshadow" => 0x767676,
    "threedshadow" => 0x767676,
    "window" => 0xFFFFFF,
    "windowframe" => 0x767676,
    "windowtext" => 0x000000
  }

  @doc false
  @spec parse_initial_input_color(String.t()) :: {:ok, term()} | :error
  def parse_initial_input_color(source) do
    HtmlValidator.check_render_resource(:max_input_bytes, byte_size(source), :style)

    normalized =
      source
      |> HtmlValidator.normalize_css_comments(:value)
      |> String.trim()
      |> String.downcase(:ascii)

    # HTML's initial color-input sanitizer retains its independently pinned
    # browser grammar, including rejection of CSS mixing and relative colors.
    if Regex.match?(~r/\A(?:(?:color-mix|light-dark)\(|[a-z]+\(\s*from\b)/, normalized),
      do: :error,
      else: parse_color(normalized, nil, true)
  end

  @doc false
  @spec parse_function_color(String.t(), term()) :: {:ok, term()} | :error
  def parse_function_color(source, current_color), do: parse_color(source, current_color, false)

  @doc "Prepares a color without discarding its space, missing components or out-of-gamut channels."
  @spec prepare(String.t(), term()) :: {:ok, map()} | :error
  def prepare(source, current_color), do: parse_value(source, current_color, 0)

  @doc "Checks a prepared color's channel representation before it reaches interpolation or conversion."
  @spec valid_prepared?(term()) :: boolean()
  def valid_prepared?(value) do
    case value do
      %{space: space, channels: [a, b, c], alpha: alpha} when map_size(value) == 3 ->
        space in ~w(rgb rgba srgb hsl hsla hwb lab lch oklab oklch srgb-linear display-p3 display-p3-linear a98-rgb prophoto-rgb rec2020 xyz xyz-d50 xyz-d65) and
          Enum.all?([a, b, c], fn n ->
            n == :none or
              (is_number(n) and
                 (abs(n) <= NativeElixirPdfUtilities.Limits.get(:max_numeric_magnitude) or
                    abs(n) == 1.0e30))
          end) and
          (alpha == :none or (is_number(alpha) and alpha >= 0 and alpha <= 1))

      _ ->
        false
    end
  end

  defp parse_color(source, current_color, initial_input) do
    with {:ok, value} <- parse_value(source, current_color, 0) do
      channels = Enum.map(value.channels, fn n -> if n == :none, do: 0.0, else: n end)

      channels =
        if initial_input and value.space == "a98-rgb" do
          # Chromium's pinned HTML input serialization uses the Adobe gamma
          # 2.2 profile. CSS conversions use the Color 4 exponent 563/256.
          Enum.map(channels, fn n ->
            :math.pow(abs(n), 2.2 / (563 / 256)) * if(n < 0, do: -1, else: 1)
          end)
        else
          channels
        end

      [red, green, blue] = CssColor.to_srgb(value.space, channels)
      alpha = if value.alpha == :none, do: 0.0, else: value.alpha

      if source |> String.trim() |> String.downcase(:ascii) == "transparent" and not initial_input,
        do: {:ok, :transparent},
        else: {:ok, if(alpha == 1.0, do: {red, green, blue}, else: {red, green, blue, alpha})}
    end
  catch
    {:render_resource_limit, {:error, {reason, diagnostic}}} ->
      throw(
        {:render_resource_limit,
         {:error,
          {reason,
           NativeElixirPdfUtilities.Diagnostics.with_context(diagnostic,
             source: if(initial_input, do: "input/color", else: "CSS color")
           )}}}
      )
  end

  defp parse_value(source, current_color, depth) do
    HtmlValidator.check_render_resource(:max_depth, depth, :style)
    HtmlValidator.check_render_resource(:max_input_bytes, byte_size(source), :style)
    HtmlValidator.reserve_render_resource(:max_work, byte_size(source), :style)

    source =
      source
      |> HtmlValidator.normalize_css_comments(:value)
      |> HtmlValidator.trim_css_whitespace()
      |> String.downcase(:ascii)

    cond do
      source == "currentcolor" ->
        case if(is_map(current_color), do: Map.get(current_color, :color), else: current_color) do
          {r, g, b} -> {:ok, %{space: "srgb", channels: [r, g, b], alpha: 1.0}}
          {r, g, b, a} -> {:ok, %{space: "srgb", channels: [r, g, b], alpha: a}}
          :transparent -> {:ok, %{space: "srgb", channels: [0.0, 0.0, 0.0], alpha: 0.0}}
          _ -> :error
        end

      source == "transparent" ->
        {:ok, %{space: "srgb", channels: [0.0, 0.0, 0.0], alpha: 0.0}}

      Map.has_key?(@named_colors, source) ->
        {:ok,
         %{space: "srgb", channels: Tuple.to_list(Map.fetch!(@named_colors, source)), alpha: 1.0}}

      Map.has_key?(@input_palette, source) ->
        palette =
          if is_map(current_color) and Map.get(current_color, :color_scheme) == :dark do
            Map.merge(@input_palette, %{
              "canvas" => 0x121212,
              "canvastext" => 0xFFFFFF,
              "field" => 0x3B3B3B,
              "fieldtext" => 0xFFFFFF,
              "buttonface" => 0x6B6B6B,
              "buttontext" => 0xFFFFFF,
              "graytext" => 0xAAAAAA,
              "linktext" => 0x9E9EFF,
              "visitedtext" => 0xD0ADF0
            })
          else
            @input_palette
          end

        value = Map.fetch!(palette, source)

        {:ok,
         %{
           space: "srgb",
           channels:
             Enum.map(
               [div(value, 65536), rem(div(value, 256), 256), rem(value, 256)],
               fn channel ->
                 if channel in [0, 255], do: div(channel, 255), else: channel / 255
               end
             ),
           alpha: 1.0
         }}

      Regex.match?(~r/\A\#[0-9a-f]{3,4}(?:[0-9a-f]{2}){0,2}\z/, source) and
          byte_size(source) in [4, 5, 7, 9] ->
        hex = binary_part(source, 1, byte_size(source) - 1)
        hex = if byte_size(hex) < 5, do: for(<<c <- hex>>, into: "", do: <<c, c>>), else: hex
        channels = for <<pair::binary-size(2) <- hex>>, do: String.to_integer(pair, 16) / 255

        {:ok,
         %{space: "srgb", channels: Enum.take(channels, 3), alpha: Enum.at(channels, 3, 1.0)}}

      true ->
        case Regex.run(~r/\A([a-z-]+)\((.*)\)\z/s, source) do
          [_, "light-dark", body] ->
            case HtmlValidator.split_css_comma(body, true) do
              [light, dark] ->
                parse_value(
                  if(is_map(current_color) and Map.get(current_color, :color_scheme) == :dark,
                    do: dark,
                    else: light
                  ),
                  current_color,
                  depth + 1
                )

              _ ->
                :error
            end

          [_, "color-mix", body] ->
            mix(body, current_color, depth + 1)

          [_, function, body]
          when function in ~w(rgb rgba hsl hsla hwb lab lch oklab oklch color) ->
            case HtmlValidator.split_css_tokens(body) do
              ["from", origin | rest] ->
                relative(function, origin, rest, current_color, depth + 1)

              _ ->
                with {:ok, tokens, legacy?} <- color_arguments(body),
                     {:ok, space, channels, alpha} <- color_channels(function, tokens, legacy?),
                     do: {:ok, %{space: space, channels: channels, alpha: alpha}}
            end

          _ ->
            :error
        end
    end
  end

  defp relative(function, origin, tokens, current_color, depth) do
    {space, tokens} =
      case {function, tokens} do
        {"color", [space | rest]} -> {space, rest}
        _ -> {function, tokens}
      end

    names =
      case space do
        s
        when s in ~w(rgb rgba srgb srgb-linear display-p3 display-p3-linear a98-rgb prophoto-rgb rec2020) ->
          ~w(r g b)

        s when s in ~w(xyz xyz-d50 xyz-d65) ->
          ~w(x y z)

        s when s in ~w(hsl hsla) ->
          ~w(h s l)

        "hwb" ->
          ~w(h w b)

        s when s in ~w(lab oklab) ->
          ~w(l a b)

        s when s in ~w(lch oklch) ->
          ~w(l c h)

        _ ->
          []
      end

    with true <- names != [], {:ok, original} <- parse_value(origin, current_color, depth) do
      channels =
        CssColor.convert(
          original.space,
          Enum.map(original.channels, fn n -> if n == :none, do: 0.0, else: n end),
          space
        )

      channels =
        cond do
          space in ~w(rgb rgba) ->
            Enum.map(channels, &(&1 * 255))

          space in ~w(hsl hsla hwb) ->
            Enum.with_index(channels)
            |> Enum.map(fn {n, i} -> if i == 0, do: n, else: n * 100 end)

          true ->
            channels
        end

      environment =
        Map.new(Enum.zip(names, channels))
        |> Map.put("alpha", if(original.alpha == :none, do: 0.0, else: original.alpha))

      {components, alpha} =
        case tokens do
          [a, b, c] -> {[a, b, c], Map.fetch!(environment, "alpha") |> Float.to_string()}
          [a, b, c, "/", alpha] -> {[a, b, c], alpha}
          _ -> {[], ""}
        end

      substitute = fn token ->
        Regex.replace(~r/[a-z][a-z0-9-]*/, token, fn name ->
          case Map.fetch(environment, name) do
            {:ok, value} -> "(" <> :erlang.float_to_binary(value * 1.0, [:short]) <> ")"
            :error -> name
          end
        end)
        |> then(fn value ->
          if String.starts_with?(value, "("), do: "calc" <> value, else: value
        end)
      end

      with true <- length(components) == 3,
           {:ok, channels} <- prepare_channels(space, Enum.map(components, substitute), false),
           {:ok, alpha} <- component(substitute.(alpha), :alpha),
           do:
             {:ok,
              %{
                space: space,
                channels: channels,
                alpha: if(alpha == :none, do: :none, else: min(max(alpha, 0.0), 1.0))
              }},
           else: (_ -> :error)
    else
      _ -> :error
    end
  end

  defp mix(body, current_color, depth) do
    with [method, first, last] <- HtmlValidator.split_css_comma(body, true),
         ["in", space | modifiers] <- HtmlValidator.split_css_tokens(method),
         true <-
           space in ~w(srgb srgb-linear display-p3 display-p3-linear a98-rgb prophoto-rgb rec2020 xyz xyz-d50 xyz-d65 lab lch oklab oklch hsl hwb),
         {:ok, hue} <- interpolation_method(space, modifiers),
         {:ok, first, p} <- mix_operand(first, current_color, depth),
         {:ok, last, q} <- mix_operand(last, current_color, depth) do
      {p, q} =
        case {p, q} do
          {nil, nil} -> {0.5, 0.5}
          {nil, q} -> {1 - q, q}
          {p, nil} -> {p, 1 - p}
          pair -> pair
        end

      if p + q > 0 do
        value = CssColor.interpolate(first, last, q / (p + q), space, hue)
        {:ok, %{value | alpha: value.alpha * min(p + q, 1.0)}}
      else
        :error
      end
    else
      _ -> :error
    end
  end

  @doc "Checks an interpolation hue modifier for a prepared color space."
  @spec interpolation_method(String.t(), [String.t()]) :: {:ok, atom()} | :error
  def interpolation_method(space, tokens) do
    case tokens do
      [] ->
        {:ok, :shorter}

      [method, "hue"]
      when space in ~w(hsl hwb lch oklch) and method in ~w(shorter longer increasing decreasing) ->
        {:ok, String.to_existing_atom(method)}

      _ ->
        :error
    end
  end

  defp mix_operand(source, current_color, depth) do
    case HtmlValidator.split_css_tokens(source) do
      [color] ->
        with {:ok, color} <- parse_value(color, current_color, depth), do: {:ok, color, nil}

      [color, percentage] ->
        with {p, "%"} when p >= 0 and p <= 100 <- HtmlValidator.parse_css_number(percentage),
             {:ok, color} <- parse_value(color, current_color, depth),
             do: {:ok, color, p / 100},
             else: (_ -> :error)

      _ ->
        :error
    end
  end

  # Preserve nested calculation tokens; only outer commas/slashes delimit channels.
  defp color_arguments(body) do
    {parts, token, depth} =
      for <<char::utf8 <- body>>, reduce: {[], [], 0} do
        {parts, token, depth} ->
          cond do
            char == ?( ->
              {parts, [<<char::utf8>> | token], depth + 1}

            char == ?) ->
              {parts, [<<char::utf8>> | token], depth - 1}

            depth == 0 and char in [?,, ?/] ->
              part = token |> Enum.reverse() |> IO.iodata_to_binary()
              {[<<char>>, part | parts], [], depth}

            true ->
              {parts, [<<char::utf8>> | token], depth}
          end
      end

    if depth == 0 do
      parts = Enum.reverse([token |> Enum.reverse() |> IO.iodata_to_binary() | parts])
      legacy? = "," in parts

      tokens =
        if legacy?,
          do: Enum.map(parts, &HtmlValidator.trim_css_whitespace/1),
          else: Enum.flat_map(parts, &HtmlValidator.split_css_tokens/1)

      {:ok, tokens, legacy?}
    else
      :error
    end
  end

  defp color_channels(function, tokens, legacy?) do
    {space, tokens} =
      case {function, tokens} do
        {"color", [space | rest]} -> {space, rest}
        _ -> {function, tokens}
      end

    tokens =
      if legacy? do
        case tokens do
          [a, ",", b, ",", c] -> [a, b, c]
          [a, ",", b, ",", c, ",", alpha] -> [a, b, c, alpha]
          _ -> []
        end
      else
        tokens
      end

    valid_legacy? = not legacy? or function in ~w(rgb rgba hsl hsla)

    {colors, alpha} =
      case tokens do
        [a, b, c] -> {[a, b, c], "1"}
        [a, b, c, "/", alpha] when not legacy? -> {[a, b, c], alpha}
        [a, b, c, alpha] when legacy? -> {[a, b, c], alpha}
        _ -> {[], ""}
      end

    with true <- valid_legacy? and length(colors) == 3,
         true <- not legacy? or Enum.all?(colors ++ [alpha], &(&1 != "none")),
         {:ok, alpha_value} <- component(alpha, :alpha),
         {:ok, channels} <- prepare_channels(space, colors, legacy?) do
      {:ok, space, channels,
       if(alpha_value == :none, do: :none, else: alpha_value |> max(0.0) |> min(1.0))}
    else
      _ -> :error
    end
  end

  defp prepare_channels(space, colors, legacy?) do
    kinds =
      case space do
        s when s in ~w(rgb rgba) ->
          [:rgb, :rgb, :rgb]

        s when s in ~w(hsl hsla) ->
          [:hue, :percent, :percent]

        "hwb" ->
          [:hue, :percent, :percent]

        "lab" ->
          [{:scaled, 100}, {:scaled, 125}, {:scaled, 125}]

        "lch" ->
          [{:scaled, 100}, {:scaled, 150}, :hue]

        "oklab" ->
          [{:scaled, 1}, {:scaled, 0.4}, {:scaled, 0.4}]

        "oklch" ->
          [{:scaled, 1}, {:scaled, 0.4}, :hue]

        s
        when s in ~w(srgb srgb-linear display-p3 display-p3-linear a98-rgb prophoto-rgb rec2020 xyz xyz-d50 xyz-d65) ->
          [:alpha, :alpha, :alpha]

        _ ->
          []
      end

    mixed_rgb? =
      legacy? and space in ~w(rgb rgba) and Enum.any?(colors, &String.ends_with?(&1, "%")) and
        not Enum.all?(colors, &String.ends_with?(&1, "%"))

    invalid_hsl? =
      legacy? and space in ~w(hsl hsla) and not Enum.all?(tl(colors), &String.ends_with?(&1, "%"))

    if kinds == [] or mixed_rgb? or invalid_hsl? do
      :error
    else
      Enum.zip(colors, kinds)
      |> Enum.reduce_while({:ok, []}, fn {source, kind}, {:ok, values} ->
        case component(source, kind) do
          {:ok, value} -> {:cont, {:ok, [value | values]}}
          :error -> {:halt, :error}
        end
      end)
      |> case do
        {:ok, values} -> {:ok, Enum.reverse(values)}
        :error -> :error
      end
    end
  end

  defp component(source, kind) do
    HtmlValidator.reserve_render_resource(:max_work, 1, :style)

    parsed =
      if source == "none",
        do: {:ok, :none, %{}},
        else: CssMathValidator.evaluate_color_component(source)

    case parsed do
      {:ok, value, type} when type == %{} or type == %{percentage: 1} or type == %{angle: 1} ->
        if is_number(value),
          do: HtmlValidator.check_render_resource(:max_numeric_magnitude, abs(value), :style)

        value =
          case value do
            :nan -> 0.0
            :infinity -> 1.0e30
            :negative_infinity -> -1.0e30
            value -> value
          end

        cond do
          value == :none ->
            {:ok, :none}

          kind == :hue and type == %{angle: 1} ->
            {:ok, value * 180 / :math.pi()}

          kind == :hue and type == %{} ->
            {:ok, value}

          kind == :hue ->
            :error

          type == %{angle: 1} ->
            :error

          kind == :rgb ->
            {:ok, if(type == %{}, do: value / 255, else: value / 100)}

          kind == :percent ->
            {:ok, value / 100}

          kind == :alpha ->
            {:ok, if(type == %{}, do: value, else: value / 100)}

          true ->
            {:scaled, scale} = kind
            {:ok, if(type == %{}, do: value, else: value / 100 * scale)}
        end

      _ ->
        :error
    end
  end
end
