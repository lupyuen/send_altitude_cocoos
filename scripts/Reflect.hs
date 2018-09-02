#!/usr/bin/env stack
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{- Code Reflection from https://github.com/chpatrick/clang-pure/tree/master/examples
stack scripts/Reflect.hs main.cpp -Ilib/cocoOS_5.0.2/src
TODO: -- stack --resolver lts-12.8 script --package clang-pure,lens
-}

import           Language.C.Clang
import           Language.C.Clang.Cursor.Typed
import           Control.Lens
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import           Data.Word
import           System.Environment
import           Debug.Trace
-- import           Text.Pretty.Simple (pPrint)

data CType =
  CArrayType  { cType :: Type
              , cCanonicalType :: Type
              , cElementType :: CType
              , cArraySize :: Word64, cSize :: Word64} |
  CScalarType { cType :: Type
              , cCanonicalType :: Type
              , cSize :: Word64
              } deriving (Show)

data CField = CField
  { cFieldName :: BS.ByteString
  , cFieldOffset :: Word64
  , cFieldType :: CType
  } deriving (Show)

data CStruct = CStruct
  { cStructName :: BS.ByteString
  , cStructFields :: [ CField ]
  } deriving (Show)

data CExtract = CExtract
  { cExtractCursor :: Cursor
  -- , cExtractOffset :: Either TypeLayoutError Word64
  } deriving (Show)

-- toCType :: Type -> CType
toCType tp = let canonicalType = typeCanonicalType tp
                  in case typeKind canonicalType of
                    ConstantArray -> do
                      elementType <- toCType . fromJust . typeElementType $ canonicalType
                      size <- typeSizeOf tp
                      pure $ CArrayType
                        { cType = tp
                        , cCanonicalType = canonicalType
                        , cElementType = elementType
                        , cArraySize = fromJust (typeArraySize canonicalType)
                        , cSize = size
                        }
                    _ -> do
                      size <- typeSizeOf tp
                      pure $ CScalarType
                        { cType = tp
                        , cCanonicalType = canonicalType
                        , cSize = size
                        }

main :: IO ()
main = do
  args <- getArgs
  case args of
    path : clangArgs -> do
      idx <- createIndex
      tu <- parseTranslationUnit idx path clangArgs

      let toCField fieldDecC = do
            fieldOffset <- offsetOfField fieldDecC
            fieldType <- toCType (cursorType fieldDecC)
            return $ CField
              { cFieldName = cursorSpelling fieldDecC
              , cFieldOffset = fieldOffset
              , cFieldType = fieldType
              }

      let toCStruct structDecC = do
            let fieldDecs =
                  structDecC
                    ^.. cursorDescendantsF
                      . folding (matchKind @'FieldDecl)
            cFields <- traverse toCField fieldDecs
            return CStruct
              { cStructName = cursorSpelling structDecC
              , cStructFields = cFields
              }

      -- transformCursor :: Cursor -> CExtract
      let transformCursor cursor = do
          -- let result = [cursor, (cursorExtent cursor), (cursorType cursor), (offsetOfField cursor)]
          return cursor
          -- return CExtract
            -- { cExtractCursor = cursor
            -- , cExtractOffset = offsetOfField cursor
            -- }

      let cStructs =
            translationUnitCursor tu
              ^.. cursorDescendantsF
                -- . folding (matchKind @'StructDecl)
                -- . to toCStruct
                . to transformCursor
                . _Right

      -- pPrint cStructs
      mapM_ print cStructs
      -- print "OK\n"

    _ -> putStrLn "usage: list-structs path [clang opts]"
