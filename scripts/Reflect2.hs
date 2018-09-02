#!/usr/bin/env stack
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{- Code Reflection from https://github.com/chpatrick/clang-pure/tree/master/examples
stack scripts/Reflect2.hs main.cpp -Ilib/cocoOS_5.0.2/src
TODO: -- stack --resolver lts-12.8 script --package clang-pure,lens
-}

module Main where

import qualified Data.ByteString as BS
import           Data.Word
import           Language.C.Clang  --  stack install clang-pure
import           Language.C.Clang.Cursor
import           Control.Lens
import           System.Environment
import           Text.Pretty.Simple (pPrint)  -- stack install pretty-simple

-- Return the [ start, end ] location of the cursor.
getLocation :: Cursor -> [Location]
getLocation cursor =
  let extent = cursorExtent cursor
    in case extent of
      Nothing -> []
      Just ext -> 
        [ spellingLocation (rangeStart ext)
        , spellingLocation (rangeEnd ext)
        ]

-- Return the offset of this member in the parent struct.
getOffset :: Cursor -> [Word64]
getOffset cursor =
  let offset = offsetOfField cursor
    in case offset of
      Left _ -> []  -- TypeLayoutError
      Right os -> [os]

findChildren :: Cursor -> [( CursorKind, BS.ByteString, BS.ByteString, [Word64], [Location] )]
findChildren root = root 
  ^.. cursorDescendantsF 
    . to (\c -> 
      ( cursorKind c
      , cursorSpelling c
      , cursorUSR c
      , getOffset c
      , getLocation c 
      ) )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: find-classes file.cpp [clang opts]"

    paths -> do
      idx <- createIndex
      let path = head paths
      let options = tail paths
      tu <- parseTranslationUnit idx path options
      let root = translationUnitCursor tu
      let childList = findChildren root
      -- pPrint childList
      mapM_ print childList
