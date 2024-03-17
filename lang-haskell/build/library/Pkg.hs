{-# LANGUAGE ScopedTypeVariables #-}

module Pkg (buildRules) where

import Development.Shake
    ( Rules
    , CmdOption(Cwd, Shell)
    , (%>)
    , phony
    , cmd
    , need
    )

import Development.Shake.FilePath ((<.>), (</>))

type Folder = String
type Pkg = String

-- | The names of the folders for dhall-format and hpack-dhall.
dhallPkgs :: [Folder]
dhallPkgs = fst <$> dhallCabal

-- | Pairs of package folder name used by dhall and the produced cabal file
-- name.
dhallCabal :: [(Folder, Pkg)]
dhallCabal =
    [ ("lang-haskell/detour-via-sci", "detour-via-sci")
    , ("lang-haskell/detour-via-uom", "detour-via-uom")
    , ("lang-haskell/build", "ft-build")
    , ("lang-haskell/earth", "flight-earth")
    , ("lang-haskell/latlng", "flight-latlng")
    , ("lang-haskell/route", "flight-route")
    , ("lang-haskell/siggy-chardust", "siggy-chardust")
    , ("lang-haskell/span", "flight-span")
    , ("lang-haskell/task", "flight-task")
    , ("lang-haskell/units", "flight-units")
    , ("lang-haskell/zone", "flight-zone")
    , ("lang-haskell/tasty-compare", "tasty-compare")
    ]

dhallRootImports :: [String]
dhallRootImports =
    [ "defaults"
    -- NOTE: Don't include default-tests.dhall as it wipes the embedded comments.
    -- , "default-tests"
    , "default-extensions"
    , "default-extensions-ghcjs"
    , "hlint"
    ]

formatPkg :: Folder -> Rules ()
formatPkg folder =
    phony ("dhall-format-" ++ folder)
    $ cmd Shell ("dhall format --inplace " ++ (folder </> "package.dhall"))

formatRoot :: String -> Rules ()
formatRoot x =
    phony ("dhall-format-" ++ x)
    $ cmd Shell ("dhall format --inplace " ++ (x <.> ".dhall"))

hpack :: Folder -> Rules ()
hpack folder =
    phony ("hpack-dhall-" ++ folder) $ do
        need ["dhall-format-" ++ folder]
        cmd (Cwd folder) Shell ("dhall-hpack-cabal --package-dhall=package.dhall")

cabal :: (Folder, Pkg) -> Rules ()
cabal (folder, pkg) =
    folder </> pkg <.> "cabal" %> \_ -> need ["hpack-dhall-" ++ folder]

buildRules :: Rules ()
buildRules = do
    sequence_ $ formatRoot <$> dhallRootImports
    sequence_ $ formatPkg <$> dhallPkgs
    sequence_ $ hpack <$> dhallPkgs
    sequence_ $ cabal <$> dhallCabal
    phony "dhall-format" $ need $ (\x -> "dhall-format-" ++ x) <$> dhallPkgs ++ dhallRootImports
    phony "hpack-dhall" $ need $ (\x -> "hpack-dhall-" ++ x) <$> dhallPkgs
    phony "cabal-files" $ need $ (\(x, y) -> x </> y <.> "cabal") <$> dhallCabal
