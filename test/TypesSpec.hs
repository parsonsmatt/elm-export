{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module TypesSpec where

import PureScript
import GHC.Generics
import           Test.Hspec   as Hspec

-- All the types in this file should be PureScript-encodable.
data Person = Person
    { personName :: String
    } deriving (Generic, PureScriptType)

spec :: Hspec.Spec
spec = return ()
