# Regenerate the browser fixture without an external image encoder.
Code.require_file("../test/support/png_fixture.ex", __DIR__)
alias NativeElixirPdfUtilities.TestSupport.PngFixture, as: PNG

images =
  for {type, depths} <- [
        {0, [1, 2, 4, 8, 16]},
        {2, [8, 16]},
        {3, [1, 2, 4, 8]},
        {4, [8, 16]},
        {6, [8, 16]}
      ],
      depth <- depths,
      interlace <- [0, 1] do
    maximum = 2 ** depth - 1

    pixel = fn x, y ->
      color = if x < 32, do: maximum, else: div(maximum, 3)
      alpha = if y < 16, do: maximum, else: div(maximum, 2)

      case type do
        0 -> [color]
        2 -> [color, div(maximum, 2), div(maximum, 4)]
        3 -> [if(x < 32, do: 0, else: 1)]
        4 -> [color, alpha]
        6 -> [color, div(maximum, 2), div(maximum, 4), alpha]
      end
    end

    chunks =
      if type == 3,
        do: PNG.chunk("PLTE", <<230, 80, 40, 40, 100, 210>>) <> PNG.chunk("tRNS", <<255, 128>>),
        else: ""

    png = PNG.build(64, 32, type, depth, interlace, pixel, chunks: chunks, filters: true)

    """
    <div style="width: 120px; height: 60px"><div>#{type}/#{depth}/#{interlace}</div><img style="display: block; width: 64px; height: 32px" src="data:image/png;base64,#{Base.encode64(png)}"></div>
    """
  end

File.write!(
  Path.expand("../test/fixtures/html_to_pdf/browser_parity/png_formats.html", __DIR__),
  """
  <!doctype html>
  <html><head><meta charset="utf-8"><style>
  @page { size: letter; margin: 36pt; }
  body { margin: 0; font-family: sans-serif; font-size: 10px; line-height: 14px; }
  </style></head><body><div style="display: flex; flex-wrap: wrap; width: 600px; background-color: #dedede">
  #{Enum.join(images)}
  </div></body></html>
  """
)
