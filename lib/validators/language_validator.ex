defmodule NativeElixirPdfUtilities.Validators.LanguageValidator do
  @moduledoc false

  alias NativeElixirPdfUtilities.Validators.HtmlValidator
  alias NativeElixirPdfUtilities.HtmlToPdf.RenderCache

  # Fixed BCP 47 registry data, not caller options or configurable ceilings.
  # IANA File-Date 2026-09-17, source SHA-256
  # 755fad43283be7b41ebe3c89ad054b6eaf928f404f9c0edb74799e0eab74beb1.
  @preferred_tags %{
    "art-lojban" => "jbo",
    "en-gb-oed" => "en-gb-oxendict",
    "i-ami" => "ami",
    "i-bnn" => "bnn",
    "i-hak" => "hak",
    "i-klingon" => "tlh",
    "i-lux" => "lb",
    "i-navajo" => "nv",
    "i-pwn" => "pwn",
    "i-tao" => "tao",
    "i-tay" => "tay",
    "i-tsu" => "tsu",
    "no-bok" => "nb",
    "no-nyn" => "nn",
    "sgn-be-fr" => "sfb",
    "sgn-be-nl" => "vgt",
    "sgn-br" => "bzs",
    "sgn-ch-de" => "sgg",
    "sgn-co" => "csn",
    "sgn-de" => "gsg",
    "sgn-dk" => "dsl",
    "sgn-es" => "ssp",
    "sgn-fr" => "fsl",
    "sgn-gb" => "bfi",
    "sgn-gr" => "gss",
    "sgn-ie" => "isg",
    "sgn-it" => "ise",
    "sgn-jp" => "jsl",
    "sgn-mx" => "mfs",
    "sgn-ni" => "ncs",
    "sgn-nl" => "dse",
    "sgn-no" => "nsl",
    "sgn-pt" => "psr",
    "sgn-se" => "swl",
    "sgn-us" => "ase",
    "sgn-za" => "sfs",
    "zh-cmn" => "cmn",
    "zh-cmn-hans" => "cmn-hans",
    "zh-cmn-hant" => "cmn-hant",
    "zh-gan" => "gan",
    "zh-guoyu" => "cmn",
    "zh-hakka" => "hak",
    "zh-min-nan" => "nan",
    "zh-wuu" => "wuu",
    "zh-xiang" => "hsn",
    "zh-yue" => "yue"
  }
  @preferred_subtags %{
    {"language", "aam"} => "aas",
    {"language", "adp"} => "dz",
    {"language", "ajp"} => "apc",
    {"language", "ajt"} => "aeb",
    {"language", "asd"} => "snz",
    {"language", "aue"} => "ktz",
    {"language", "ayx"} => "nun",
    {"language", "bgm"} => "bcg",
    {"language", "bh"} => "bih",
    {"language", "bic"} => "bir",
    {"language", "bjd"} => "drl",
    {"language", "blg"} => "iba",
    {"language", "ccq"} => "rki",
    {"language", "cjr"} => "mom",
    {"language", "cka"} => "cmr",
    {"language", "cmk"} => "xch",
    {"language", "coy"} => "pij",
    {"language", "cqu"} => "quh",
    {"language", "dek"} => "sqm",
    {"language", "dit"} => "dif",
    {"language", "drh"} => "khk",
    {"language", "drr"} => "kzk",
    {"language", "drw"} => "prs",
    {"language", "gav"} => "dev",
    {"language", "gfx"} => "vaj",
    {"language", "ggn"} => "gvr",
    {"language", "gli"} => "kzk",
    {"language", "gti"} => "nyc",
    {"language", "guv"} => "duz",
    {"language", "hrr"} => "jal",
    {"language", "ibi"} => "opa",
    {"language", "ilw"} => "gal",
    {"language", "in"} => "id",
    {"language", "iw"} => "he",
    {"language", "jeg"} => "oyb",
    {"language", "ji"} => "yi",
    {"language", "jw"} => "jv",
    {"language", "kgc"} => "tdf",
    {"language", "kgh"} => "kml",
    {"language", "kgm"} => "plu",
    {"language", "koj"} => "kwv",
    {"language", "krm"} => "bmf",
    {"language", "ktr"} => "dtp",
    {"language", "kvs"} => "gdj",
    {"language", "kwq"} => "yam",
    {"language", "kxe"} => "tvd",
    {"language", "kxl"} => "kru",
    {"language", "kzj"} => "dtp",
    {"language", "kzt"} => "dtp",
    {"language", "lak"} => "ksp",
    {"language", "lii"} => "raq",
    {"language", "llo"} => "ngt",
    {"language", "lmm"} => "rmx",
    {"language", "meg"} => "cir",
    {"language", "mo"} => "ro",
    {"language", "mrd"} => "mgp",
    {"language", "mst"} => "mry",
    {"language", "mwj"} => "vaj",
    {"language", "myd"} => "aog",
    {"language", "myt"} => "mry",
    {"language", "nad"} => "xny",
    {"language", "ncp"} => "kdz",
    {"language", "nns"} => "nbr",
    {"language", "nnx"} => "ngv",
    {"language", "nom"} => "cbr",
    {"language", "nte"} => "eko",
    {"language", "nts"} => "pij",
    {"language", "nxu"} => "bpp",
    {"language", "oun"} => "vaj",
    {"language", "pat"} => "kxr",
    {"language", "pcr"} => "adx",
    {"language", "pmc"} => "huw",
    {"language", "pmk"} => "crr",
    {"language", "pmu"} => "phr",
    {"language", "ppa"} => "bfy",
    {"language", "ppr"} => "lcq",
    {"language", "prp"} => "gu",
    {"language", "pry"} => "prt",
    {"language", "puz"} => "pub",
    {"language", "sca"} => "hle",
    {"language", "shl"} => "mrh",
    {"language", "skk"} => "oyb",
    {"language", "smd"} => "kmb",
    {"language", "snb"} => "iba",
    {"language", "szd"} => "umi",
    {"language", "tdu"} => "dtp",
    {"language", "thc"} => "tpo",
    {"language", "thw"} => "ola",
    {"language", "thx"} => "oyb",
    {"language", "tie"} => "ras",
    {"language", "tkk"} => "twm",
    {"language", "tlw"} => "weo",
    {"language", "tmk"} => "tdg",
    {"language", "tmp"} => "tyj",
    {"language", "tne"} => "kak",
    {"language", "tnf"} => "prs",
    {"language", "tpw"} => "tpn",
    {"language", "tsf"} => "taj",
    {"language", "uok"} => "ema",
    {"language", "xba"} => "cax",
    {"language", "xia"} => "acn",
    {"language", "xkh"} => "waw",
    {"language", "xrq"} => "dmw",
    {"language", "xss"} => "zko",
    {"language", "ybd"} => "rki",
    {"language", "yma"} => "lrr",
    {"language", "ymt"} => "mtm",
    {"language", "yol"} => "enm",
    {"language", "yos"} => "zom",
    {"language", "yuu"} => "yug",
    {"language", "zir"} => "scv",
    {"language", "zkb"} => "kjh",
    {"region", "bu"} => "mm",
    {"region", "dd"} => "de",
    {"region", "fx"} => "fr",
    {"region", "tp"} => "tl",
    {"region", "yd"} => "ye",
    {"region", "zr"} => "cd",
    {"variant", "heploc"} => "alalc97"
  }
  @extlangs %{
    "aao" => {"ar", "aao"},
    "abh" => {"ar", "abh"},
    "abv" => {"ar", "abv"},
    "acm" => {"ar", "acm"},
    "acq" => {"ar", "acq"},
    "acw" => {"ar", "acw"},
    "acx" => {"ar", "acx"},
    "acy" => {"ar", "acy"},
    "adf" => {"ar", "adf"},
    "ads" => {"sgn", "ads"},
    "aeb" => {"ar", "aeb"},
    "aec" => {"ar", "aec"},
    "aed" => {"sgn", "aed"},
    "aen" => {"sgn", "aen"},
    "afb" => {"ar", "afb"},
    "afg" => {"sgn", "afg"},
    "ajp" => {"ar", "ajp"},
    "ajs" => {"sgn", "ajs"},
    "apc" => {"ar", "apc"},
    "apd" => {"ar", "apd"},
    "arb" => {"ar", "arb"},
    "arq" => {"ar", "arq"},
    "ars" => {"ar", "ars"},
    "ary" => {"ar", "ary"},
    "arz" => {"ar", "arz"},
    "ase" => {"sgn", "ase"},
    "asf" => {"sgn", "asf"},
    "asp" => {"sgn", "asp"},
    "asq" => {"sgn", "asq"},
    "asw" => {"sgn", "asw"},
    "auz" => {"ar", "auz"},
    "avl" => {"ar", "avl"},
    "ayh" => {"ar", "ayh"},
    "ayl" => {"ar", "ayl"},
    "ayn" => {"ar", "ayn"},
    "ayp" => {"ar", "ayp"},
    "bbz" => {"ar", "bbz"},
    "bfi" => {"sgn", "bfi"},
    "bfk" => {"sgn", "bfk"},
    "bjn" => {"ms", "bjn"},
    "bog" => {"sgn", "bog"},
    "bqn" => {"sgn", "bqn"},
    "bqy" => {"sgn", "bqy"},
    "btj" => {"ms", "btj"},
    "bve" => {"ms", "bve"},
    "bvl" => {"sgn", "bvl"},
    "bvu" => {"ms", "bvu"},
    "bzs" => {"sgn", "bzs"},
    "cdo" => {"zh", "cdo"},
    "cds" => {"sgn", "cds"},
    "cjy" => {"zh", "cjy"},
    "cmn" => {"zh", "cmn"},
    "cnp" => {"zh", "cnp"},
    "coa" => {"ms", "coa"},
    "cpx" => {"zh", "cpx"},
    "csc" => {"sgn", "csc"},
    "csd" => {"sgn", "csd"},
    "cse" => {"sgn", "cse"},
    "csf" => {"sgn", "csf"},
    "csg" => {"sgn", "csg"},
    "csl" => {"sgn", "csl"},
    "csn" => {"sgn", "csn"},
    "csp" => {"zh", "csp"},
    "csq" => {"sgn", "csq"},
    "csr" => {"sgn", "csr"},
    "csx" => {"sgn", "csx"},
    "czh" => {"zh", "czh"},
    "czo" => {"zh", "czo"},
    "doq" => {"sgn", "doq"},
    "dse" => {"sgn", "dse"},
    "dsl" => {"sgn", "dsl"},
    "dsz" => {"sgn", "dsz"},
    "dup" => {"ms", "dup"},
    "dyl" => {"sgn", "dyl"},
    "ecs" => {"sgn", "ecs"},
    "ehs" => {"sgn", "ehs"},
    "esl" => {"sgn", "esl"},
    "esn" => {"sgn", "esn"},
    "eso" => {"sgn", "eso"},
    "eth" => {"sgn", "eth"},
    "fcs" => {"sgn", "fcs"},
    "fse" => {"sgn", "fse"},
    "fsl" => {"sgn", "fsl"},
    "fss" => {"sgn", "fss"},
    "gan" => {"zh", "gan"},
    "gds" => {"sgn", "gds"},
    "gom" => {"kok", "gom"},
    "gse" => {"sgn", "gse"},
    "gsg" => {"sgn", "gsg"},
    "gsm" => {"sgn", "gsm"},
    "gss" => {"sgn", "gss"},
    "gus" => {"sgn", "gus"},
    "hab" => {"sgn", "hab"},
    "haf" => {"sgn", "haf"},
    "hak" => {"zh", "hak"},
    "hds" => {"sgn", "hds"},
    "hji" => {"ms", "hji"},
    "hks" => {"sgn", "hks"},
    "hnm" => {"zh", "hnm"},
    "hos" => {"sgn", "hos"},
    "hps" => {"sgn", "hps"},
    "hsh" => {"sgn", "hsh"},
    "hsl" => {"sgn", "hsl"},
    "hsn" => {"zh", "hsn"},
    "icl" => {"sgn", "icl"},
    "iks" => {"sgn", "iks"},
    "ils" => {"sgn", "ils"},
    "inl" => {"sgn", "inl"},
    "ins" => {"sgn", "ins"},
    "ise" => {"sgn", "ise"},
    "isg" => {"sgn", "isg"},
    "isr" => {"sgn", "isr"},
    "jak" => {"ms", "jak"},
    "jax" => {"ms", "jax"},
    "jcs" => {"sgn", "jcs"},
    "jhs" => {"sgn", "jhs"},
    "jks" => {"sgn", "jks"},
    "jls" => {"sgn", "jls"},
    "jos" => {"sgn", "jos"},
    "jsl" => {"sgn", "jsl"},
    "jus" => {"sgn", "jus"},
    "kgi" => {"sgn", "kgi"},
    "knn" => {"kok", "knn"},
    "kvb" => {"ms", "kvb"},
    "kvk" => {"sgn", "kvk"},
    "kvr" => {"ms", "kvr"},
    "kxd" => {"ms", "kxd"},
    "lbs" => {"sgn", "lbs"},
    "lce" => {"ms", "lce"},
    "lcf" => {"ms", "lcf"},
    "lgs" => {"sgn", "lgs"},
    "liw" => {"ms", "liw"},
    "lls" => {"sgn", "lls"},
    "lsb" => {"sgn", "lsb"},
    "lsc" => {"sgn", "lsc"},
    "lsg" => {"sgn", "lsg"},
    "lsl" => {"sgn", "lsl"},
    "lsn" => {"sgn", "lsn"},
    "lso" => {"sgn", "lso"},
    "lsp" => {"sgn", "lsp"},
    "lst" => {"sgn", "lst"},
    "lsv" => {"sgn", "lsv"},
    "lsw" => {"sgn", "lsw"},
    "lsy" => {"sgn", "lsy"},
    "ltg" => {"lv", "ltg"},
    "luh" => {"zh", "luh"},
    "lvs" => {"lv", "lvs"},
    "lws" => {"sgn", "lws"},
    "lzh" => {"zh", "lzh"},
    "max" => {"ms", "max"},
    "mdl" => {"sgn", "mdl"},
    "meo" => {"ms", "meo"},
    "mfa" => {"ms", "mfa"},
    "mfb" => {"ms", "mfb"},
    "mfs" => {"sgn", "mfs"},
    "min" => {"ms", "min"},
    "mnp" => {"zh", "mnp"},
    "mqg" => {"ms", "mqg"},
    "mre" => {"sgn", "mre"},
    "msd" => {"sgn", "msd"},
    "msi" => {"ms", "msi"},
    "msr" => {"sgn", "msr"},
    "mui" => {"ms", "mui"},
    "mzc" => {"sgn", "mzc"},
    "mzg" => {"sgn", "mzg"},
    "mzy" => {"sgn", "mzy"},
    "nan" => {"zh", "nan"},
    "nbs" => {"sgn", "nbs"},
    "ncs" => {"sgn", "ncs"},
    "nsi" => {"sgn", "nsi"},
    "nsl" => {"sgn", "nsl"},
    "nsp" => {"sgn", "nsp"},
    "nsr" => {"sgn", "nsr"},
    "nzs" => {"sgn", "nzs"},
    "okl" => {"sgn", "okl"},
    "orn" => {"ms", "orn"},
    "ors" => {"ms", "ors"},
    "pel" => {"ms", "pel"},
    "pga" => {"ar", "pga"},
    "pgz" => {"sgn", "pgz"},
    "pks" => {"sgn", "pks"},
    "prl" => {"sgn", "prl"},
    "prz" => {"sgn", "prz"},
    "psc" => {"sgn", "psc"},
    "psd" => {"sgn", "psd"},
    "pse" => {"ms", "pse"},
    "psg" => {"sgn", "psg"},
    "psl" => {"sgn", "psl"},
    "pso" => {"sgn", "pso"},
    "psp" => {"sgn", "psp"},
    "psr" => {"sgn", "psr"},
    "pys" => {"sgn", "pys"},
    "rib" => {"sgn", "rib"},
    "rms" => {"sgn", "rms"},
    "rnb" => {"sgn", "rnb"},
    "rsi" => {"sgn", "rsi"},
    "rsl" => {"sgn", "rsl"},
    "rsm" => {"sgn", "rsm"},
    "rsn" => {"sgn", "rsn"},
    "sdl" => {"sgn", "sdl"},
    "sfb" => {"sgn", "sfb"},
    "sfs" => {"sgn", "sfs"},
    "sgg" => {"sgn", "sgg"},
    "sgx" => {"sgn", "sgx"},
    "shu" => {"ar", "shu"},
    "sjc" => {"zh", "sjc"},
    "slf" => {"sgn", "slf"},
    "sls" => {"sgn", "sls"},
    "sqk" => {"sgn", "sqk"},
    "sqs" => {"sgn", "sqs"},
    "sqx" => {"sgn", "sqx"},
    "ssh" => {"ar", "ssh"},
    "ssp" => {"sgn", "ssp"},
    "ssr" => {"sgn", "ssr"},
    "svk" => {"sgn", "svk"},
    "swc" => {"sw", "swc"},
    "swh" => {"sw", "swh"},
    "swl" => {"sgn", "swl"},
    "syy" => {"sgn", "syy"},
    "szs" => {"sgn", "szs"},
    "tmw" => {"ms", "tmw"},
    "tse" => {"sgn", "tse"},
    "tsm" => {"sgn", "tsm"},
    "tsq" => {"sgn", "tsq"},
    "tss" => {"sgn", "tss"},
    "tsy" => {"sgn", "tsy"},
    "tza" => {"sgn", "tza"},
    "ugn" => {"sgn", "ugn"},
    "ugy" => {"sgn", "ugy"},
    "ukl" => {"sgn", "ukl"},
    "uks" => {"sgn", "uks"},
    "urk" => {"ms", "urk"},
    "uzn" => {"uz", "uzn"},
    "uzs" => {"uz", "uzs"},
    "vgt" => {"sgn", "vgt"},
    "vkk" => {"ms", "vkk"},
    "vkt" => {"ms", "vkt"},
    "vsi" => {"sgn", "vsi"},
    "vsl" => {"sgn", "vsl"},
    "vsv" => {"sgn", "vsv"},
    "wbs" => {"sgn", "wbs"},
    "wuu" => {"zh", "wuu"},
    "xki" => {"sgn", "xki"},
    "xml" => {"sgn", "xml"},
    "xmm" => {"ms", "xmm"},
    "xms" => {"sgn", "xms"},
    "yds" => {"sgn", "yds"},
    "ygs" => {"sgn", "ygs"},
    "yhs" => {"sgn", "yhs"},
    "ysl" => {"sgn", "ysl"},
    "ysm" => {"sgn", "ysm"},
    "yue" => {"zh", "yue"},
    "zhk" => {"sgn", "zhk"},
    "zib" => {"sgn", "zib"},
    "zlm" => {"ms", "zlm"},
    "zmi" => {"ms", "zmi"},
    "zsl" => {"sgn", "zsl"},
    "zsm" => {"ms", "zsm"}
  }

  # Subtag widths and extension singleton grammar are fixed RFC 5646 rules.
  @tag_regex ~r/\A(?<language>[a-z]{2,3}(?:-[a-z]{3}){0,3}|[a-z]{4}|[a-z]{5,8})(?<script>-[a-z]{4})?(?<region>-(?:[a-z]{2}|[0-9]{3}))?(?<variants>(?:-(?:[a-z0-9]{5,8}|[0-9][a-z0-9]{3}))*)(?<extensions>(?:-[0-9a-wy-z](?:-[a-z0-9]{2,8})+)*)(?<private>(?:-x(?:-[a-z0-9]{1,8})+)?)\z/
  @private_regex ~r/\Ax(?:-[a-z0-9]{1,8})+\z/
  @extended_range_regex ~r/\A(?:[a-z]{1,8}|\*)(?:-(?:[a-z0-9]{1,8}|\*))*\z/

  @doc false
  @spec canonicalize(binary()) :: [binary()]
  def canonicalize(value) do
    HtmlValidator.reserve_render_resource(:max_css_work, byte_size(value), :css)
    folded = String.downcase(value, :ascii)
    preferred = Map.get(@preferred_tags, folded, folded)
    parts = Regex.named_captures(@tag_regex, preferred)

    cond do
      Regex.match?(@private_regex, preferred) ->
        String.split(preferred, "-")

      is_map(parts) ->
        variants = String.split(parts["variants"], "-", trim: true)
        extensions = Regex.scan(~r/-([0-9a-wy-z])((?:-[a-z0-9]{2,8})+)/, parts["extensions"])
        singletons = Enum.map(extensions, &Enum.at(&1, 1))

        if length(variants) == length(Enum.uniq(variants)) and
             length(singletons) == length(Enum.uniq(singletons)) do
          [language | extlangs] = String.split(parts["language"], "-")
          language = Map.get(@preferred_subtags, {"language", language}, language)

          {language, extlangs} =
            case extlangs do
              [extlang | tail] ->
                case Map.get(@extlangs, extlang) do
                  {^language, replacement} -> {replacement, tail}
                  _ -> {language, extlangs}
                end

              [] ->
                {language, []}
            end

          prefix =
            case Map.get(@extlangs, language) do
              {prefix, _replacement} -> [prefix, language]
              nil -> [language]
            end

          script = String.trim_leading(parts["script"], "-")
          region = String.trim_leading(parts["region"], "-")

          canonical =
            prefix ++
              extlangs ++
              [
                Map.get(@preferred_subtags, {"script", script}, script),
                Map.get(@preferred_subtags, {"region", region}, region)
              ] ++
              Enum.map(variants, &Map.get(@preferred_subtags, {"variant", &1}, &1))

          extension_subtags =
            extensions
            |> Enum.sort_by(&Enum.at(&1, 1))
            |> Enum.flat_map(fn [_whole, singleton, subtags] ->
              [singleton | String.split(subtags, "-", trim: true)]
            end)

          Enum.reject(canonical, &(&1 == "")) ++
            extension_subtags ++
            String.split(parts["private"], "-", trim: true)
        else
          String.split(folded, "-")
        end

      String.contains?(folded, "*") and Regex.match?(@extended_range_regex, folded) ->
        [language | rest] = String.split(folded, "-")
        language = Map.get(@preferred_subtags, {"language", language}, language)

        prefix =
          case Map.get(@extlangs, language) do
            {prefix, _replacement} -> [prefix, language]
            nil -> [language]
          end

        {rest, _extension?} =
          Enum.map_reduce(rest, false, fn subtag, extension? ->
            extension? = extension? or (byte_size(subtag) == 1 and subtag != "*")

            kind =
              cond do
                extension? -> nil
                byte_size(subtag) == 4 -> "script"
                byte_size(subtag) in [2, 3] -> "region"
                true -> "variant"
              end

            {Map.get(@preferred_subtags, {kind, subtag}, subtag), extension?}
          end)

        prefix ++ rest

      true ->
        # HTML preserves empty, unknown and malformed language identifiers.
        # Invalid identifiers must not acquire meaning through alias rewriting.
        String.split(folded, "-")
    end
  end

  @doc false
  @spec matches?([binary()], [binary()]) :: boolean()
  def matches?(range, language) do
    HtmlValidator.reserve_render_resource(:max_css_work, length(range) + length(language), :css)

    case {range, language} do
      {[""], [""]} ->
        true

      {_, [""]} ->
        false

      {[first | rest], [actual | tail]} ->
        (first == "*" or first == actual) and matching_subtags?(rest, tail)

      _ ->
        false
    end
  end

  # RFC 4647 extended filtering skips non-singleton language subtags, while a
  # singleton stops a search so extension/private subtags cannot match by accident.
  defp matching_subtags?(range, language) do
    case {range, language} do
      {[], _} ->
        true

      {["*" | rest], language} ->
        matching_subtags?(rest, language)

      {[wanted | rest], [actual | tail]} ->
        cond do
          wanted == actual -> matching_subtags?(rest, tail)
          byte_size(actual) == 1 -> false
          true -> matching_subtags?(range, tail)
        end

      _ ->
        false
    end
  end

  @doc false
  @spec with_document([map()], reference(), (-> result)) :: result when result: term()
  def with_document(children, cache, fun) do
    with_language_context(children, cache, %{}, %{}, fun)
  end

  @doc "Resolves language through actual logical parents and shadow hosts while retaining document-only language pragmas."
  @spec with_composed_document(map(), map(), map(), reference(), (-> result)) :: result
        when result: term()
  def with_composed_document(logical, roots, clones, cache, fun) do
    if map_size(roots) == 0 and map_size(clones) == 0 do
      with_document(logical.children, cache, fun)
    else
      index =
        NativeElixirPdfUtilities.Validators.HtmlSelectedContentValidator.logical_index(logical)
        |> Map.merge(clones)

      hosts = Map.new(roots, fn {id, root} -> {id, root.host_id} end)
      with_language_context(logical.children, cache, index, hosts, fun)
    end
  end

  defp with_language_context(children, cache, index, hosts, fun) do
    key = {__MODULE__, :document}
    previous = Process.put(key, {cache, children, make_ref(), index, hosts})

    try do
      fun.()
    after
      case previous do
        nil -> Process.delete(key)
        context -> Process.put(key, context)
      end
    end
  end

  @doc false
  @spec resolved_language(map() | nil, [map()]) :: [binary()]
  def resolved_language(node, ancestors) do
    {cache, children, document_scope, index, hosts} = Process.get({__MODULE__, :document})

    if node == nil do
      cached_document_language(children, cache, document_scope)
    else
      RenderCache.fetch(cache, {:selector_language, node._selector_id}, fn ->
        if Map.has_key?(index, Map.get(node, :node_id)) do
          logical_language(node.node_id, index, hosts, cache, children, document_scope, 0)
        else
          case language_attribute(node) do
            {:ok, value} ->
              canonicalize(value)

            :inherit ->
              case ancestors do
                [parent | rest] -> resolved_language(parent, rest)
                [] -> cached_document_language(children, cache, document_scope)
              end
          end
        end
      end)
    end
  end

  # Shadow descendants inherit from their host; slot distribution does not
  # change the assigned node's actual language parent. Cached logical records
  # carry node IDs, not renderer selector IDs or resource permissions.
  defp logical_language(id, index, hosts, cache, children, scope, depth) do
    HtmlValidator.check_render_resource(:max_html_depth, depth, :html)

    RenderCache.fetch(cache, {:logical_language, scope, id}, fn ->
      HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)
      node = Map.fetch!(index, id)

      case language_attribute(node) do
        {:ok, value} ->
          canonicalize(value)

        :inherit ->
          parent_id = Map.get(node, :parent_id)
          parent = Map.get(index, parent_id)
          parent_id = if Map.has_key?(hosts, parent_id), do: hosts[parent_id], else: parent_id

          if Map.has_key?(hosts, Map.get(node, :parent_id)) or match?(%{type: :element}, parent),
            do: logical_language(parent_id, index, hosts, cache, children, scope, depth + 1),
            else: cached_document_language(children, cache, scope)
      end
    end)
  end

  defp cached_document_language(children, cache, scope) do
    RenderCache.fetch(cache, {:language_document_default, scope}, fn ->
      canonicalize(document_default(%{children: children}))
    end)
  end

  @doc false
  @spec language_attribute(map()) :: {:ok, binary()} | :inherit
  def language_attribute(node) do
    attributes = Map.get(node, :attributes, %{})
    namespaces = Map.get(node, :attribute_namespaces, %{})

    cond do
      Map.get(namespaces, "xml:lang") == "http://www.w3.org/XML/1998/namespace" ->
        {:ok, Map.fetch!(attributes, "xml:lang")}

      Map.get(node, :namespace, :html) in [:html, :svg] and Map.has_key?(attributes, "lang") and
          is_nil(Map.get(namespaces, "lang")) ->
        {:ok, Map.fetch!(attributes, "lang")}

      true ->
        :inherit
    end
  end

  @doc false
  @spec document_default(map()) :: binary()
  def document_default(root) do
    document_language([root], "")
  end

  # Tree order is significant: the last successfully processed pragma wins.
  # Template contents are inert and cannot set the containing document language.
  defp document_language(nodes, language) do
    Enum.reduce(nodes, language, fn node, language ->
      HtmlValidator.reserve_render_resource(:max_css_work, 1, :css)

      case node do
        %{tag: "template"} ->
          language

        %{tag: "meta", attributes: %{"http-equiv" => pragma, "content" => content}} ->
          if Map.get(node, :namespace, :html) == :html and
               is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "http-equiv")) and
               is_nil(Map.get(Map.get(node, :attribute_namespaces, %{}), "content")) and
               String.downcase(pragma, :ascii) == "content-language" and
               not String.contains?(content, ",") do
            case HtmlValidator.html_class_tokens(content) do
              [first | _rest] -> first
              [] -> language
            end
          else
            language
          end

        %{children: children} when is_list(children) ->
          document_language(children, language)

        _ ->
          language
      end
    end)
  end
end
