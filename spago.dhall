{ sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, name =
    "hyper"
, dependencies =
    [ "aff"
    , "avar"
    , "argonaut"
    , "arrays"
    , "console"
    , "control"
    , "effect"
    , "foldable-traversable"
    , "http-methods"
    , "indexed-monad"
    , "media-types"
    , "node-buffer"
    , "node-fs-aff"
    , "node-http"
    , "ordered-collections"
    , "psci-support"
    , "random"
    , "smolder"
    , "spec"
    , "spec-discovery"
    , "strings"
    , "transformers"
    , "record-extra"
    ]
, packages =
    ./packages.dhall
}
