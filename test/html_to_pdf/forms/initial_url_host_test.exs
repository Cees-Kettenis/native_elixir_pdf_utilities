defmodule NativeElixirPdfUtilities.HtmlToPdf.InitialUrlHostTest do
  use ExUnit.Case, async: false
  alias NativeElixirPdfUtilities.HtmlToPdf.{HtmlParser, Style}
  alias NativeElixirPdfUtilities.Validators.{HtmlUrlIdnaValidator, HtmlUrlValidator}

  test "Unicode and ACE hosts share admission with mapping, joiner and bidi rules" do
    for value <- [
          "http://bücher.example/",
          "http://xn--bcher-kva.example/",
          "http://faß.example/",
          "http://xn--fa-hia.example/",
          "http://_é.example/",
          "http://xn--_-bga.example/",
          "http://cafe\u0301.example/",
          "http://ＥＸＡＭＰＬＥ。test/",
          "http://a\u00adb.example/",
          "http://क्\u200dष.example/",
          "http://क्\u200cष.example/",
          "http://ب\u064e\u200c\u064eب.example/",
          "http://אבג.abc1/",
          "http://１２７。０。０。１/",
          "http://xn--/",
          "http://xn--abc-/",
          "http://xn--0/",
          "http://xn--u-ccb/"
        ] do
      assert HtmlUrlValidator.valid_initial_input_url?(value) == {:ok, true}, value
    end

    for value <- [
          "http://a\u200cb/",
          "http://a\u200db/",
          "http://ب\u200c/",
          "http://\u0301a/",
          "http://אבג.1abc/",
          "http://אבa/",
          "http://ا١1/",
          "http://\u0378/",
          "http://xn--é/",
          "http://ｘｎ--u-ccb/",
          "http://ｘｎ--a-ecp/",
          "http://%23.example/",
          "http://%2f.example/",
          "http://%00.example/",
          "http://%7f.example/",
          "http://%25.example/",
          "http://%ff.example/"
        ] do
      assert HtmlUrlValidator.valid_initial_input_url?(value) == {:ok, false}, value
    end
  end

  test "UTS46 rejects malformed decoded ACE rather than remapping it into another label" do
    for host <- [
          "xn--",
          "xn--abc-",
          "xn--0",
          "xn--é",
          "xn--u-ccb",
          "xn--a-ecp",
          "xn--!",
          "xn--a-b-c"
        ] do
      assert :error = HtmlUrlIdnaValidator.domain(host), host
    end

    assert :error = HtmlUrlIdnaValidator.domain("xn--bn32g")
    refute HtmlUrlValidator.absolute_input_url?(<<255>>)
    assert :error = HtmlUrlIdnaValidator.domain(<<255>>)
    assert :error = HtmlUrlIdnaValidator.domain("")
    assert {:ok, "bücher.example"} = HtmlUrlIdnaValidator.domain("xn--bcher-kva.example")
    assert {:ok, "_é.example"} = HtmlUrlIdnaValidator.domain("xn--_-bga.example")
    assert {:ok, "example.test"} = HtmlUrlIdnaValidator.domain("EXAMPLE.TEST")
  end

  test "authority boundaries distinguish special, opaque, file and path URLs" do
    for value <- [
          "custom://",
          "custom:///path",
          "custom://[::1]:80/",
          "custom://%zz/",
          "custom://host%23name/",
          "custom://例え.test/",
          "custom://user:pass@host/",
          "custom:opaque [path]",
          "data:text/plain,hello",
          "mailto:a@b",
          "http:example.test",
          "http://user@@host/",
          "http://host:00065535/",
          "http://host:/",
          "http://host:000000/",
          "http://127.1/",
          "http://0x7f.1/",
          "http://0177.0.0.1/",
          "http://0xffffffff/",
          "http://[::ffff:192.168.1.1]/",
          "http://[1:2:3:4:5:6:7:8]/",
          "file:///tmp/a",
          "file://localhost/path",
          "file://server/path",
          "file://[::1]/path",
          "file://C:/path",
          "file://C|/path",
          "file:C:/path"
        ] do
      assert HtmlUrlValidator.valid_initial_input_url?(value) == {:ok, true}, value
    end

    for value <- [
          "custom://[invalid",
          "custom://[::1]junk",
          "custom://host:bad",
          "custom://:80/",
          "custom://user@/",
          "custom://host name/",
          "custom://host\\name/",
          "custom://host|name/",
          "http://[::1",
          "http://[::1]:65536",
          "http://host:1x/",
          "http://host:65536/",
          "http://256.1/",
          "http://1.2.65536/",
          "http://4294967296/",
          "http://08/",
          "http://example.123/",
          "http://[::ffff:192.168.01.1]/",
          "http://[::ffff:256.1.1.1]/",
          "http://[::1%25zone]/",
          "http://[1::2::3]/",
          "file://user@host/path",
          "file://host:80/path",
          "file://host:/path"
        ] do
      assert HtmlUrlValidator.valid_initial_input_url?(value) == {:ok, false}, value
    end
  end

  test "constraint invalid URL values render without granting resource access" do
    for value <- ["custom://[invalid", "http://a\u200cb/", "http://bücher.example/"] do
      html = "<input type=url value='#{value}'>"
      assert {:ok, dom} = HtmlParser.parse_detailed(html)
      assert {:ok, %{children: [node]}} = Style.compute_detailed(dom, forms: :static)
      state = node.style._form_control.initial_state
      assert state.value == value

      assert state.constraint_validation.checks.type_mismatch ==
               (value != "http://bücher.example/")

      assert {:ok, _} =
               NativeElixirPdfUtilities.HtmlToPdf.render(html,
                 forms: :static,
                 asset_resolver: fn request ->
                   flunk("URL value requested #{inspect(request)}")
                 end
               )
    end
  end
end
