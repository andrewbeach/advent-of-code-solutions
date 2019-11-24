{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "aff"
    , "console"
    , "crypto"
    , "effect"
    , "either"
    , "integers"
    , "node-buffer"
    , "node-fs-aff"
    , "psci-support"
    , "string-parsers"
    , "strings"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
