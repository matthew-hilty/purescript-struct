{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "struct"
, dependencies =
    [ "argonaut-core"
    , "argonaut-codecs"
    , "console"
    , "effect"
    , "prelude"
    , "proxying"
    , "psci-support"
    , "record"
    , "record-extra"
    , "subcategory"
    , "test-unit"
    , "typelevel-prelude"
    , "variant"
    ]
, packages =
    ./packages.dhall
}
