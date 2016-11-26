{-# LANGUAGE OverloadedStrings #-}
module PureScript.Record
  ( toPureScriptTypeRef
  , toPureScriptTypeRefWith
  , toPureScriptTypeSource
  , toPureScriptTypeSourceWith
  ) where

import           Control.Monad.Reader
import           Data.Text
import           PureScript.Common
import           PureScript.Type
import           Formatting

class HasType a where
  render :: a -> Reader Options Text

class HasTypeRef a where
  renderRef :: a -> Reader Options Text

instance HasType PureScriptDatatype where
    render d@(PureScriptDatatype _ constructor@(RecordConstructor _ _)) =
        sformat ("type alias " % stext % " =" % cr % stext) <$> renderRef d <*> render constructor
    render d@(PureScriptDatatype _ constructor@(MultipleConstructors _)) =
        sformat ("type " % stext % cr % "    = " % stext) <$> renderRef d <*> render constructor
    render d@(PureScriptDatatype _ constructor@(NamedConstructor _ _)) =
        sformat ("type " % stext % cr % "    = " % stext) <$> renderRef d <*> render constructor
    render (PureScriptPrimitive primitive) = renderRef primitive

instance HasTypeRef PureScriptDatatype where
    renderRef (PureScriptDatatype typeName _) =
        pure typeName

    renderRef (PureScriptPrimitive primitive) =
        renderRef primitive


instance HasType PureScriptConstructor where
    render (RecordConstructor _ value) =
        sformat ("    { " % stext % cr % "    }") <$> render value
    render (NamedConstructor constructorName value) =
        sformat (stext % stext) constructorName <$> render value
    render (MultipleConstructors constructors) =
        fmap (Data.Text.intercalate "\n    | ") . sequence $ render <$> constructors


instance HasType PureScriptValue where
    render (PureScriptRef name) = pure name
    render PureScriptEmpty = pure ""
    render (Values x y) =
        sformat (stext % cr % "    , " % stext) <$> render x <*> render y
    render (PureScriptPrimitiveRef primitive) = sformat (" " % stext) <$> renderRef primitive
    render (PureScriptField name value) = do
        fieldModifier <- asks fieldLabelModifier
        sformat (stext % " :" % stext) (fieldModifier name) <$> render value


instance HasTypeRef PureScriptPrimitive where
    renderRef (EList (PureScriptPrimitive EChar)) = renderRef EString
    renderRef (EList datatype) = sformat ("List (" % stext % ")") <$> renderRef datatype
    renderRef (ETuple2 x y) =
        sformat ("( " % stext % ", " % stext % " )") <$> renderRef x <*> renderRef y
    renderRef (EMaybe datatype) =
        sformat ("Maybe (" % stext % ")") <$> renderRef datatype
    renderRef (EDict k v) =
        sformat ("Dict (" % stext % ") (" % stext % ")") <$> renderRef k <*> renderRef v
    renderRef EInt = pure "Int"
    renderRef EDate = pure "Date"
    renderRef EBool = pure "Bool"
    renderRef EChar = pure "Char"
    renderRef EString = pure "String"
    renderRef EUnit = pure "()"
    renderRef EFloat = pure "Float"

toPureScriptTypeRefWith :: PureScriptType a => Options -> a -> Text
toPureScriptTypeRefWith options x = runReader (renderRef (toPureScriptType x)) options

toPureScriptTypeRef :: PureScriptType a => a -> Text
toPureScriptTypeRef = toPureScriptTypeRefWith defaultOptions

toPureScriptTypeSourceWith :: PureScriptType a => Options -> a -> Text
toPureScriptTypeSourceWith options x = runReader (render (toPureScriptType x)) options

toPureScriptTypeSource :: PureScriptType a => a -> Text
toPureScriptTypeSource = toPureScriptTypeSourceWith defaultOptions
