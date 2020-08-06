{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "tris"
, dependencies =
    [ "maybe"
    , "either"
    , "ordered-collections"
    , "console"
    , "effect"
    , "psci-support"
    , "canvas"
    , "math"
    , "random"
    , "refs"
    , "web-dom"
    , "web-html"
    , "web-events"
    , "web-uievents"
    , "numbers"
    , "aff"
    , "foldable-traversable"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
