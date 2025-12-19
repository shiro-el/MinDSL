{-# LANGUAGE OverloadedStrings #-}

-- | Shared Aeson options for MinDSL JSON encoding and TypeScript generation
--
-- This module defines the common options used by both JSON.hs (for ToJSON instances)
-- and TypeScript.hs (for TypeScript type generation), ensuring consistency
-- between the JSON output and TypeScript types.
module MinDSL.Options
  ( minDSLOptions
  ) where

import Data.Aeson (Options(..), defaultOptions, SumEncoding(..))
import Data.Char (toLower, isUpper)
import Data.List (stripPrefix)

-- | Shared options for JSON encoding and TypeScript generation
-- This ensures both use identical field naming conventions
minDSLOptions :: Options
minDSLOptions = defaultOptions
  { fieldLabelModifier = camelToSnakeOrStrip
  , constructorTagModifier = map toLower
  , sumEncoding = TaggedObject
      { tagFieldName = "type"
      , contentsFieldName = "data"
      }
  , omitNothingFields = False
  , unwrapUnaryRecords = False
  }

-- | Convert camelCase field names by stripping common prefixes
-- Examples:
--   posLine -> line
--   posColumn -> column
--   textKo -> ko
--   likertMin -> min
--   optionValue -> value
--   scaleName -> name
camelToSnakeOrStrip :: String -> String
camelToSnakeOrStrip s = case findPrefix s of
  Just rest -> lowerFirst rest
  Nothing   -> lowerFirst s
  where
    -- Common prefixes to strip
    prefixes = ["pos", "span", "text", "likert", "option", "item", "subscale",
                "section", "func", "scoring", "meta", "scale"]

    findPrefix str = foldr (\p acc -> case stripPrefix p str of
      Just rest@(c:_) | isUpper c -> Just rest
      _ -> acc) Nothing prefixes

    lowerFirst [] = []
    lowerFirst (c:cs) = toLower c : cs
