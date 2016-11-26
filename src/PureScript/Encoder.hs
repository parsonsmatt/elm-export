{-# LANGUAGE OverloadedStrings #-}
module PureScript.Encoder
  ( toPureScriptEncoderRef
  , toPureScriptEncoderRefWith
  , toPureScriptEncoderSource
  , toPureScriptEncoderSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           PureScript.Common
import           PureScript.Type
import           Formatting

class HasEncoder a where
  render :: a -> Reader Options Text

class HasEncoderRef a where
  renderRef :: a -> Reader Options Text

instance HasEncoder PureScriptDatatype where
    render d@(PureScriptDatatype name constructor) = do
        fnName <- renderRef d
        sformat
            (stext % " : " % stext % " -> Json.Encode.Value" % cr % stext % " x =" % stext)
            fnName
            name
            fnName <$>
            render constructor
    render (PureScriptPrimitive primitive) = renderRef primitive

instance HasEncoderRef PureScriptDatatype where
    renderRef (PureScriptDatatype name _) =
        pure $ sformat ("encode" % stext) name

    renderRef (PureScriptPrimitive primitive) =
        renderRef primitive

instance HasEncoder PureScriptConstructor where
    render (RecordConstructor _ value) =
      sformat (cr % "    Json.Encode.object" % cr % "        [ " % stext % cr % "        ]") <$> render value

instance HasEncoder PureScriptValue where
    render (PureScriptField name value) = do
        fieldModifier <- asks fieldLabelModifier
        valueBody <- render value
        pure $
            sformat
                ("( \"" % stext % "\", " % stext % " x." % stext % " )")
                (fieldModifier name)
                valueBody
                name
    render (PureScriptPrimitiveRef primitive) = renderRef primitive
    render (PureScriptRef name) = pure $ sformat ("encode" % stext) name
    render (Values x y) = sformat (stext % cr % "        , " % stext) <$> render x <*> render y

instance HasEncoderRef PureScriptPrimitive where
    renderRef EDate = pure "(Json.Encode.string << toISOString)"
    renderRef EUnit = pure "Json.Encode.null"
    renderRef EInt = pure "Json.Encode.int"
    renderRef EChar = pure "Json.Encode.char"
    renderRef EBool = pure "Json.Encode.bool"
    renderRef EFloat = pure "Json.Encode.float"
    renderRef EString = pure "Json.Encode.string"
    renderRef (EList (PureScriptPrimitive EChar)) = pure "Json.Encode.string"
    renderRef (EList datatype) = sformat ("(Json.Encode.list << List.map " % stext % ")") <$> renderRef datatype
    renderRef (EMaybe datatype) =
        sformat ("(Maybe.withDefault Json.Encode.null << Maybe.map " % stext % ")") <$>
        renderRef datatype
    renderRef (ETuple2 x y) =
        sformat ("(tuple2 " % stext % " " % stext % ")") <$> renderRef x <*>
        renderRef y
    renderRef (EDict k datatype) =
        sformat ("(dict " % stext % " " % stext % ")") <$> renderRef k <*> renderRef datatype

toPureScriptEncoderRefWith :: PureScriptType a => Options -> a -> Text
toPureScriptEncoderRefWith options x = runReader (renderRef (toPureScriptType x)) options

toPureScriptEncoderRef :: PureScriptType a => a -> Text
toPureScriptEncoderRef = toPureScriptEncoderRefWith defaultOptions

toPureScriptEncoderSourceWith :: PureScriptType a => Options -> a -> Text
toPureScriptEncoderSourceWith options x = runReader (render (toPureScriptType x)) options

toPureScriptEncoderSource :: PureScriptType a => a -> Text
toPureScriptEncoderSource = toPureScriptEncoderSourceWith defaultOptions
