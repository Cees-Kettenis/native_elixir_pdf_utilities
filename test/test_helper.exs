ExUnit.start(exclude: [:browser_parity])

Code.require_file("support/html_tree_helpers.ex", __DIR__)

Code.require_file("support/pdf_visual_compare.ex", __DIR__)

Code.require_file("support/jpeg_fixture.ex", __DIR__)

Code.require_file("support/table_geometry.ex", __DIR__)

Code.require_file("support/math_paint_geometry.ex", __DIR__)
