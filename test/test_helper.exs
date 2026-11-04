Code.require_file("support/paths.ex", __DIR__)

alias NativeElixirPdfUtilities.TestSupport.Paths

ExUnit.start(exclude: [:browser_parity])

Code.require_file(Paths.support("html_tree_helpers.ex"))

Code.require_file(Paths.support("pdf_visual_compare.ex"))

ExUnit.after_suite(fn _ -> NativeElixirPdfUtilities.TestSupport.ChromiumPool.stop() end)

Code.require_file(Paths.support("jpeg_fixture.ex"))

Code.require_file(Paths.support("png_fixture.ex"))

Code.require_file(Paths.support("svg_converted_image.ex"))

Code.require_file(Paths.support("svg_public_workflow.ex"))

Code.require_file(Paths.support("table_geometry.ex"))

Code.require_file(Paths.support("math_paint_geometry.ex"))
