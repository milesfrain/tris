{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "tris"
, dependencies =
  [ "canvas", "colors", "heterogeneous", "quickcheck", "signal", "web-html" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
