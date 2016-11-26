{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PureScript.Decoder
  ( toPureScriptDecoderRef
  , toPureScriptDecoderRefWith
  , toPureScriptDecoderSource
  , toPureScriptDecoderSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           PureScript.Common
import           PureScript.Type
import           Formatting

class HasDecoder a where
  render :: a -> Reader Options Text

class HasDecoderRef a where
  renderRef :: a -> Reader Options Text

instance HasDecoder PureScriptDatatype where
    render d@(PureScriptDatatype name constructor) = do
        fnName <- renderRef d
        sformat
            (stext % " : Decoder " % stext % cr % stext % " =" % cr % stext)
            fnName
            name
            fnName <$>
            render constructor
    render (PureScriptPrimitive primitive) = renderRef primitive

instance HasDecoderRef PureScriptDatatype where
    renderRef (PureScriptDatatype name _) =
        pure $ sformat ("decode" % stext) name

    renderRef (PureScriptPrimitive primitive) =
        renderRef primitive


instance HasDecoder PureScriptConstructor where
    render (NamedConstructor name value) =
        sformat ("    decode " % stext % cr % stext) name <$> render value
    render (RecordConstructor name value) =
        sformat ("    decode " % stext % cr % stext) name <$> render value


instance HasDecoder PureScriptValue where
    render (PureScriptRef name) = pure (sformat ("decode" % stext) name)
    render (PureScriptPrimitiveRef primitive) = renderRef primitive
    render (Values x y) = sformat (stext % cr % stext) <$> render x <*> render y
    render (PureScriptField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat
            ("        |> required \"" % stext % "\" " % stext)
            (fieldModifier name) <$>
            render value


instance HasDecoderRef PureScriptPrimitive where
    renderRef (EList (PureScriptPrimitive EChar)) = pure "string"
    renderRef (EList datatype) =
        sformat ("(list " % stext % ")") <$> renderRef datatype
    renderRef (EDict key value) =
        sformat ("(map Dict.fromList " % stext % ")") <$>
        renderRef (EList (PureScriptPrimitive (ETuple2 (PureScriptPrimitive key) value)))
    renderRef (EMaybe datatype) =
        sformat ("(maybe " % stext % ")") <$> renderRef datatype
    renderRef (ETuple2 x y) =
        sformat ("(tuple2 (,) " % stext % " " % stext % ")") <$> renderRef x <*>
        renderRef y
    renderRef EUnit = pure "(succeed ())"
    renderRef EDate = pure "(customDecoder string Date.fromString)"
    renderRef EInt = pure "int"
    renderRef EBool = pure "bool"
    renderRef EChar = pure "char"
    renderRef EFloat = pure "float"
    renderRef EString = pure "string"


toPureScriptDecoderRefWith :: PureScriptType a => Options -> a -> Text
toPureScriptDecoderRefWith options x = runReader (renderRef (toPureScriptType x)) options


toPureScriptDecoderRef :: PureScriptType a => a -> Text
toPureScriptDecoderRef = toPureScriptDecoderRefWith defaultOptions


toPureScriptDecoderSourceWith :: PureScriptType a => Options -> a -> Text
toPureScriptDecoderSourceWith options x = runReader (render (toPureScriptType x)) options


toPureScriptDecoderSource :: PureScriptType a => a -> Text
toPureScriptDecoderSource = toPureScriptDecoderSourceWith defaultOptions
