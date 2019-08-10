{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "effect"
	, "console"
	, "foldable-traversable"
	, "psci-support"
	, "string-parsers"
	, "strings"
	, "tuples"
	]
, packages =
    ./packages.dhall
}
