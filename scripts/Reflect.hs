#!/usr/bin/env stack
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{- Code Reflection from https://github.com/chpatrick/clang-pure/tree/master/examples
stack scripts/Reflect.hs main.cpp -Ilib/cocoOS_5.0.2/src
TODO: -- stack --resolver lts-12.8 script --package clang-pure,lens
-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Word
import           Language.C.Clang  --  stack install clang-pure
import           Language.C.Clang.Cursor
import           Control.Lens
import           System.Environment
import           Text.Pretty.Simple (pPrint)  -- stack install pretty-simple

-- Convert the cursor into a text array pattern for matching.
getPattern :: Cursor -> BS.ByteString
getPattern cursor =
  let kind = cursorKind cursor
  in case kind of
    VarDecl {} -> ( cursorSpelling cursor ) -- task_id
    CallExpr {} -> BS.empty -- task_create

    TypeRef {} -> ( cursorSpelling cursor )  -- uint8_t
    FirstExpr {} -> BS.concat ( removeLast (getTokens cursor) )  --  task_func,

    -- DeclRefExpr {} -> [ cursorSpelling cursor ]  -- task_func
    -- IntegerLiteral {} -> getTokens cursor
    -- FloatingLiteral {} -> getTokens cursor
    -- CharacterLiteral {} -> getTokens cursor
    _ -> BS.empty

-- Return the tokens for specific literals.
getTokens :: Cursor -> [BS.ByteString]
getTokens cursor =
  let kind = cursorKind cursor
      extent = cursorExtent cursor
  in case extent of
    Nothing -> []
    Just ext -> -- Only cursors with extents
      let tokenSet = tokenize ext
          tokens = tokenSetTokens tokenSet
          tokenList = take 10 (map tokenSpelling tokens) -- Limit to 10 tokens
      in case kind of
        IntegerLiteral {} -> tokenList
        FloatingLiteral {} -> tokenList
        CharacterLiteral {} -> tokenList
        UnaryOperator {} -> tokenList
        BinaryOperator {} -> tokenList
        
        CompoundStmt {} -> tokenList
        DeclStmt {} -> tokenList
        VarDecl {} -> tokenList

        FirstExpr {} -> tokenList
        CallExpr {} -> tokenList
        CXXBoolLiteralExpr {} -> tokenList
        _ -> []

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

-- Recursively find all child cursors for specific lines and generate a text array pattern for matching.
-- uint8_t task_id = task_create(task_func, ...) -> ( task_id, uint8_t, [ task_create, task_func, ... ] )
-- task_create(task_func, ...) -> ( "", "", [ task_create, task_func, ... ] )
getChildrenPattern :: Cursor -> ( BS.ByteString, BS.ByteString, [BS.ByteString] )
getChildrenPattern root = 
  if not (isCursorFromMainFile root)
    then (BS.empty, BS.empty, [])  -- Skip declarations that don't come from our main file.
    else
      let patterns = root 
            ^.. cursorDescendantsF . to (\c -> getPattern c)
          filteredPatterns = filter (\bs -> not (BS.null bs)) patterns -- Skip empty strings
          kind = cursorKind root
      in case kind of
        -- If: uint8_t task_id = task_create(...)
        VarDecl {} ->
          if length filteredPatterns > 2
            then (filteredPatterns !! 0, filteredPatterns !! 1, drop 2 filteredPatterns)
            else (BS.empty, BS.empty, filteredPatterns)
        -- If: task_create(...)
        CallExpr {} ->  
          (BS.empty, BS.empty, filteredPatterns)
        -- Else show the pattern for debugging
        _ -> 
          (BS.empty, BS.empty, [ getPattern root ]) 

-- Recursively find all child cursors and process them.
getChildren :: Cursor -> [
  ( CursorKind     -- Kind
  , BS.ByteString  -- Spelling
  , ( BS.ByteString, BS.ByteString, [BS.ByteString] ) -- Pattern: (var, type, func)
  , [BS.ByteString] -- Tokens
  , BS.ByteString  -- USR
  , [Word64]       -- Offset
  , [Location]     -- Location
  , Cursor         -- Cursor
  )]
getChildren root = root 
  ^.. cursorDescendantsF 
    -- . filtered (isFromMainFile)
    . to (\c -> 
      ( cursorKind c
      , cursorSpelling c
      , getChildrenPattern c -- Returns (var, type, func)
      , getTokens c
      , cursorUSR c
      , getOffset c
      , getLocation c
      , c 
      ) )

-- Main function reads a C++ source file and parses the file plus dependencies.
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
      let childList = getChildren root
      -- pPrint childList
      mapM_ print childList

--  Return true if the cursor is from main source file 
isCursorFromMainFile :: Cursor -> Bool
isCursorFromMainFile cursor = 
  let extent = cursorExtent cursor
  in case extent of
    Nothing -> False
    Just ext -> isFromMainFile (rangeStart ext)
      
-- Remove the last item in a list of tokens if it is a ',' or '(' or ')'
removeLast :: [BS.ByteString] -> [BS.ByteString]
removeLast tokens =
  if null tokens
    then tokens
    else 
      let tokenLength = length tokens
          lastToken = last tokens
      in if lastToken == (packStr ",") 
        || lastToken == (packStr "(")
        || lastToken == (packStr ")")
        then take (tokenLength - 1) tokens
        else tokens

--  Convert string to bytestring.
packStr :: String -> BS.ByteString
packStr = encodeUtf8 . T.pack
