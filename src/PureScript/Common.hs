{-# LANGUAGE OverloadedStrings #-}
module PureScript.Common  where

import           Data.Text  (Text)
import           Formatting

data Options =
  Options {fieldLabelModifier :: Text -> Text}

defaultOptions :: Options
defaultOptions = Options {fieldLabelModifier = id}

cr :: Format r r
cr = now "\n"
