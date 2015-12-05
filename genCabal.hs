#!/usr/bin/env stack
-- stack --resolver=lts-3.14 --install-ghc runghc --package=cartel

module Main where

import Cartel

pinchotVer :: [Word]
pinchotVer = [0,1,0,0]

atleast :: NonEmptyString -> Version -> Package
atleast n v = package n (gtEq v)

base :: Package
base = closedOpen "base" [4,8,0,0] [5]

containers :: Package
containers = atleast "containers" [0,5,6,2]

transformers :: Package
transformers = atleast "transformers" [0,4,2,0]

templateHaskell :: Package
templateHaskell = atleast "template-haskell" [2,10]

earley :: Package
earley = atleast "Earley" [0,10,1,0]

prettyShow :: Package
prettyShow = atleast "pretty-show" [1,6,9]

commonOptions :: HasBuildInfo a => [a]
commonOptions =
  [ ghcOptions ["-Wall"]
  , otherExtensions ["TemplateHaskell"]
  , haskell2010
  , hsSourceDirs ["lib"]
  ]

libraryDepends :: [Package]
libraryDepends = [ base, containers, transformers, templateHaskell,
  earley ]

props :: Properties
props = blank
  { name = "pinchot"
  , version = pinchotVer
  , cabalVersion = Just (1,14)
  , buildType = Just simple
  , license = Just bsd3
  , licenseFile = "LICENSE"
  , copyright = "2015 Omari Norman"
  , author = "Omari Norman"
  , maintainer = "omari@smileystation.com"
  , stability = "Experimental"
  , homepage = "http://www.github.com/massysett/pinchot"
  , bugReports = "http://www.github.com/massysett/pinchot/issues"
  , synopsis = "Build parsers and ASTs for context-free grammars"
  , extraSourceFiles = ["README.md"]
  , description =
    [ "Pinchot builds parsers and ASTs for context-free grammars."
    , ""
    , "For more information, please see the README.md file, which"
    , "is available in the source tarball or is visible at the bottom"
    , "of the Pinchot homepage:"
    , ""
    , "<http://www.github.com/massysett/pinchot>"
    ]
  , category = "Development"
  }

main :: IO ()
main = defaultMain $ do
  libMods <- modules "lib"
  buildExe <- makeFlag "executables" $ FlagOpts
    { flagDescription = "Build executables"
    , flagDefault = False
    , flagManual = True
    }
  return
    ( props
    ,   exposedModules libMods
      : buildDepends libraryDepends
      : commonOptions
    , [ githubHead "massysett" "penny"
      , executable "print-postal-parser" $
        [ mainIs "print-postal-parser.hs"
        , condBlock (flag buildExe)
            (buildable True, ( [ otherModules libMods
                               , hsSourceDirs ["exe"]
                               , buildDepends libraryDepends
                               ] ++ commonOptions
                             )
            )
            [buildable False]
        ]

      , executable "postal-parser" $
        [ mainIs "postal-parser.hs"
        , condBlock (flag buildExe)
            (buildable True, ( [ otherModules libMods
                               , hsSourceDirs ["exe"]
                               , buildDepends (prettyShow : libraryDepends)
                               ] ++ commonOptions
                             )
            )
            [buildable False]
        ]
      ]
    )
