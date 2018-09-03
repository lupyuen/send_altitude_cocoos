#!/usr/bin/env stack
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{- Code Reflection from https://github.com/chpatrick/clang-pure/tree/master/examples
stack scripts/FindClasses.hs main.cpp
TODO: -- stack --resolver lts-12.8 script --package clang-pure,lens
-}

module Main where

import qualified Data.ByteString as BS
import           Data.List
import qualified Data.HashMap.Strict as HMS
import           Data.Hashable
import           Language.C.Clang
import           Language.C.Clang.Cursor
import           Control.Lens
import           Data.Traversable
import           Data.Maybe
import           GHC.Generics (Generic)
import           System.Environment

deriving instance Generic CursorKind
instance Hashable CursorKind

data HasClass = HasClass
  { combineResults :: Bool -> Bool -> Bool
  , predicate :: Cursor -> Bool
  }

classes :: HMS.HashMap String HasClass
classes = HMS.fromList
  [ ( "HasType",     HasClass (&&) (isJust . cursorType)            )
  , ( "HasChildren", HasClass (||) (notNullOf cursorChildrenF)      )
  , ( "HasExtent",   HasClass (&&) (isJust . cursorExtent)          )
  , ( "HasSpelling", HasClass (&&) (not . BS.null . cursorSpelling) )
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: find-classes file1 [file2] [fileN...]"

    paths -> do
      idx <- createIndex

      pathClassResults <- for paths $ \path -> do
        tu <- parseTranslationUnit idx path 
          [ "-Ilib/cocoOS_5.0.2/src"
          , "-Ilib/cocoOS_5.0.3/src"
          ]
        let root = translationUnitCursor tu
        return $ HMS.fromList
          [ ( className, findClass predicate root )
          | ( className, predicate ) <- HMS.toList classes
          ]

      let classResults = foldl1' (HMS.unionWithKey $ \className -> HMS.unionWith (combineResults (classes HMS.! className))) pathClassResults

      let allInstances =
            intercalate "\n"
              [ instances
              | ( className, kindResults ) <- HMS.toList classResults
              , let sortedNames = sort [ show kind | ( kind, matches ) <- HMS.toList kindResults, matches ]
              , let instances = unlines $ map (\kindName -> "instance " ++ className ++ " '" ++ kindName) sortedNames
              ]

      putStrLn allInstances

findClass :: HasClass -> Cursor -> HMS.HashMap CursorKind Bool
findClass HasClass {..} root = HMS.fromListWith combineResults kindResults
  where
    kindResults = root ^.. cursorDescendantsF . to (\c -> ( cursorKind c, predicate c ) )