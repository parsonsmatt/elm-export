{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ExportSpec where

import           Data.Char
import           Data.Int
import           Data.Map
import           Data.Monoid
import           Data.Proxy
import           Data.Text    hiding (unlines)
import           Data.Time
import           PureScript
import           GHC.Generics
import           Test.Hspec   hiding (Spec)
import           Test.Hspec   as Hspec
import           Text.Printf

-- Debugging hint:
-- ghci> import GHC.Generics
-- ghci> :kind! Rep Post
-- ...

data Post = Post
    { id       :: Int
    , name     :: String
    , age      :: Maybe Double
    , comments :: [Comment]
    , promoted :: Maybe Comment
    , author   :: Maybe String
    } deriving (Generic, PureScriptType)

data Comment = Comment
    { postId         :: Int
    , text           :: Text
    , mainCategories :: (String, String)
    , published      :: Bool
    , created        :: UTCTime
    , tags           :: Map String Int
    } deriving (Generic, PureScriptType)

data Position
  = Beginning
  | Middle
  | End
  deriving (Generic,PureScriptType)

data Timing
  = Start
  | Continue Double
  | Stop
  deriving (Generic,PureScriptType)

newtype Useless =
    Useless ()
     deriving (Generic, PureScriptType)

newtype FavoritePlaces =
  FavoritePlaces {positionsByUser :: Map String [Position]}
  deriving (Generic,PureScriptType)

-- | We don't actually use this type, we just need to see that it compiles.
data LotsOfInts = LotsOfInts
    { intA :: Int8
    , intB :: Int16
    , intC :: Int32
    , intD :: Int64
    } deriving (Generic, PureScriptType)

spec :: Hspec.Spec
spec =
  do toPureScriptTypeSpec
     toPureScriptDecoderSpec
     toPureScriptEncoderSpec

toPureScriptTypeSpec :: Hspec.Spec
toPureScriptTypeSpec =
  describe "Convert to PureScript types." $
  do it "toPureScriptTypeSource Post" $
       shouldMatchTypeSource
         (unlines ["module PostType exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Post)
         "test/PostType.elm"
     it "toPureScriptTypeSource Comment" $
       shouldMatchTypeSource
         (unlines ["module CommentType exposing (..)"
                  ,""
                  ,"import Date exposing (Date)"
                  ,"import Dict exposing (Dict)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Comment)
         "test/CommentType.elm"
     it "toPureScriptTypeSource Position" $
       shouldMatchTypeSource
         (unlines ["module PositionType exposing (..)","","","%s"])
         defaultOptions
         (Proxy :: Proxy Position)
         "test/PositionType.elm"
     it "toPureScriptTypeSource Timing" $
       shouldMatchTypeSource
         (unlines ["module TimingType exposing (..)","","","%s"])
         defaultOptions
         (Proxy :: Proxy Timing)
         "test/TimingType.elm"
     it "toPureScriptTypeSource Useless" $
       shouldMatchTypeSource
         (unlines ["module UselessType exposing (..)","","","%s"])
         defaultOptions
         (Proxy :: Proxy Useless)
         "test/UselessType.elm"
     it "toPureScriptTypeSource FavoritePlaces" $
       shouldMatchTypeSource
         (unlines ["module FavoritePlacesType exposing (..)"
                  ,""
                  ,"import PositionType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy FavoritePlaces)
         "test/FavoritePlacesType.elm"
     it "toPureScriptTypeSourceWithOptions Post" $
       shouldMatchTypeSource
         (unlines ["module PostTypeWithOptions exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy Post)
         "test/PostTypeWithOptions.elm"
     it "toPureScriptTypeSourceWithOptions Comment" $
       shouldMatchTypeSource
         (unlines ["module CommentTypeWithOptions exposing (..)"
                  ,""
                  ,"import Date exposing (Date)"
                  ,"import Dict exposing (Dict)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "comment"})
         (Proxy :: Proxy Comment)
         "test/CommentTypeWithOptions.elm"
     describe "Convert to PureScript type references." $
       do it "toPureScriptTypeRef Post" $
            toPureScriptTypeRef (Proxy :: Proxy Post)
            `shouldBe` "Post"
          it "toPureScriptTypeRef [Comment]" $
            toPureScriptTypeRef (Proxy :: Proxy [Comment])
            `shouldBe` "List (Comment)"
          it "toPureScriptTypeRef String" $
            toPureScriptTypeRef (Proxy :: Proxy String)
            `shouldBe` "String"
          it "toPureScriptTypeRef (Maybe String)" $
            toPureScriptTypeRef (Proxy :: Proxy (Maybe String))
            `shouldBe` "Maybe (String)"
          it "toPureScriptTypeRef [Maybe String]" $
            toPureScriptTypeRef (Proxy :: Proxy [Maybe String])
            `shouldBe` "List (Maybe (String))"
          it "toPureScriptTypeRef (Map String (Maybe String))" $
            toPureScriptTypeRef (Proxy :: Proxy (Map String (Maybe String)))
            `shouldBe` "Dict (String) (Maybe (String))"

toPureScriptDecoderSpec :: Hspec.Spec
toPureScriptDecoderSpec =
  describe "Convert to PureScript decoders." $
  do it "toPureScriptDecoderSource Comment" $
       shouldMatchDecoderSource
         (unlines ["module CommentDecoder exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,"import Date"
                  ,"import Dict"
                  ,"import Json.Decode exposing (..)"
                  ,"import Json.Decode.Pipeline exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Comment)
         "test/CommentDecoder.elm"
     it "toPureScriptDecoderSource Post" $
       shouldMatchDecoderSource
         (unlines ["module PostDecoder exposing (..)"
                  ,""
                  ,"import CommentDecoder exposing (..)"
                  ,"import Json.Decode exposing (..)"
                  ,"import Json.Decode.Pipeline exposing (..)"
                  ,"import PostType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Post)
         "test/PostDecoder.elm"
     it "toPureScriptDecoderSourceWithOptions Post" $
       shouldMatchDecoderSource
         (unlines ["module PostDecoderWithOptions exposing (..)"
                  ,""
                  ,"import CommentDecoder exposing (..)"
                  ,"import Json.Decode exposing (..)"
                  ,"import Json.Decode.Pipeline exposing (..)"
                  ,"import PostType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy Post)
         "test/PostDecoderWithOptions.elm"
     it "toPureScriptDecoderSourceWithOptions Comment" $
       shouldMatchDecoderSource
         (unlines ["module CommentDecoderWithOptions exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,"import Date"
                  ,"import Dict"
                  ,"import Json.Decode exposing (..)"
                  ,"import Json.Decode.Pipeline exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "comment"})
         (Proxy :: Proxy Comment)
         "test/CommentDecoderWithOptions.elm"
     describe "Convert to PureScript decoder references." $
       do it "toPureScriptDecoderRef Post" $
            toPureScriptDecoderRef (Proxy :: Proxy Post)
            `shouldBe` "decodePost"
          it "toPureScriptDecoderRef [Comment]" $
            toPureScriptDecoderRef (Proxy :: Proxy [Comment])
            `shouldBe` "(list decodeComment)"
          it "toPureScriptDecoderRef String" $
            toPureScriptDecoderRef (Proxy :: Proxy String)
            `shouldBe` "string"
          it "toPureScriptDecoderRef (Maybe String)" $
            toPureScriptDecoderRef (Proxy :: Proxy (Maybe String))
            `shouldBe` "(maybe string)"
          it "toPureScriptDecoderRef [Maybe String]" $
            toPureScriptDecoderRef (Proxy :: Proxy [Maybe String])
            `shouldBe` "(list (maybe string))"
          it "toPureScriptDecoderRef (Map String (Maybe String))" $
            toPureScriptDecoderRef (Proxy :: Proxy (Map String (Maybe String)))
            `shouldBe` "(map Dict.fromList (list (tuple2 (,) string (maybe string))))"

toPureScriptEncoderSpec :: Hspec.Spec
toPureScriptEncoderSpec =
  describe "Convert to PureScript encoders." $
  do it "toPureScriptEncoderSource Comment" $
       shouldMatchEncoderSource
         (unlines ["module CommentEncoder exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,"import Exts.Date exposing (..)"
                  ,"import Exts.Json.Encode exposing (..)"
                  ,"import Json.Encode"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Comment)
         "test/CommentEncoder.elm"
     it "toPureScriptEncoderSource Post" $
       shouldMatchEncoderSource
         (unlines ["module PostEncoder exposing (..)"
                  ,""
                  ,"import CommentEncoder exposing (..)"
                  ,"import Json.Encode"
                  ,"import PostType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         defaultOptions
         (Proxy :: Proxy Post)
         "test/PostEncoder.elm"
     it "toPureScriptEncoderSourceWithOptions Comment" $
       shouldMatchEncoderSource
         (unlines ["module CommentEncoderWithOptions exposing (..)"
                  ,""
                  ,"import CommentType exposing (..)"
                  ,"import Exts.Date exposing (..)"
                  ,"import Exts.Json.Encode exposing (..)"
                  ,"import Json.Encode"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "comment"})
         (Proxy :: Proxy Comment)
         "test/CommentEncoderWithOptions.elm"
     it "toPureScriptEncoderSourceWithOptions Post" $
       shouldMatchEncoderSource
         (unlines ["module PostEncoderWithOptions exposing (..)"
                  ,""
                  ,"import CommentEncoder exposing (..)"
                  ,"import Json.Encode"
                  ,"import PostType exposing (..)"
                  ,""
                  ,""
                  ,"%s"])
         (defaultOptions {fieldLabelModifier = withPrefix "post"})
         (Proxy :: Proxy Post)
         "test/PostEncoderWithOptions.elm"
     describe "Convert to PureScript encoder references." $
       do it "toPureScriptEncoderRef Post" $
            toPureScriptEncoderRef (Proxy :: Proxy Post)
            `shouldBe` "encodePost"
          it "toPureScriptEncoderRef [Comment]" $
            toPureScriptEncoderRef (Proxy :: Proxy [Comment])
            `shouldBe` "(Json.Encode.list << List.map encodeComment)"
          it "toPureScriptEncoderRef String" $
            toPureScriptEncoderRef (Proxy :: Proxy String)
            `shouldBe` "Json.Encode.string"
          it "toPureScriptEncoderRef (Maybe String)" $
            toPureScriptEncoderRef (Proxy :: Proxy (Maybe String))
            `shouldBe` "(Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string)"
          it "toPureScriptEncoderRef [Maybe String]" $
            toPureScriptEncoderRef (Proxy :: Proxy [Maybe String])
            `shouldBe` "(Json.Encode.list << List.map (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string))"
          it "toPureScriptEncoderRef (Map String (Maybe String))" $
            toPureScriptEncoderRef (Proxy :: Proxy (Map String (Maybe String)))
            `shouldBe` "(dict Json.Encode.string (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string))"

shouldMatchTypeSource
  :: PureScriptType a
  => String -> Options -> a -> FilePath -> IO ()
shouldMatchTypeSource wrapping options x =
  shouldMatchFile . printf wrapping $ toPureScriptTypeSourceWith options x

shouldMatchDecoderSource
  :: PureScriptType a
  => String -> Options -> a -> FilePath -> IO ()
shouldMatchDecoderSource wrapping options x =
  shouldMatchFile . printf wrapping $ toPureScriptDecoderSourceWith options x

shouldMatchEncoderSource
  :: PureScriptType a
  => String -> Options -> a -> FilePath -> IO ()
shouldMatchEncoderSource wrapping options x =
  shouldMatchFile . printf wrapping $ toPureScriptEncoderSourceWith options x

shouldMatchFile :: String -> FilePath -> IO ()
shouldMatchFile actual fileExpected =
  do source <- readFile fileExpected
     actual `shouldBe` source

initCap :: Text -> Text
initCap t =
    case uncons t of
        Nothing -> t
        Just (c, cs) -> cons (Data.Char.toUpper c) cs

withPrefix :: Text -> Text -> Text
withPrefix prefix s = prefix <> ( initCap  s)
