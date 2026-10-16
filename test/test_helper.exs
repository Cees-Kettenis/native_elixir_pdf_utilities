ExUnit.start(exclude: [:browser_parity])

Code.require_file("support/html_tree_helpers.ex", __DIR__)

Code.require_file("support/pdf_visual_compare.ex", __DIR__)

ExUnit.after_suite(fn _ -> NativeElixirPdfUtilities.TestSupport.ChromiumPool.stop() end)

Code.require_file("support/jpeg_fixture.ex", __DIR__)

Code.require_file("support/png_fixture.ex", __DIR__)

Code.require_file("support/svg_converted_image.ex", __DIR__)

Code.require_file("support/svg_public_workflow.ex", __DIR__)

Code.require_file("support/table_geometry.ex", __DIR__)

Code.require_file("support/math_paint_geometry.ex", __DIR__)
