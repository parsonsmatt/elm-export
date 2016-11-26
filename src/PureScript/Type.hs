{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PureScript.Type where

import           Data.Int     (Int16, Int32, Int64, Int8)
import           Data.Map
import           Data.Proxy
import           Data.Text
import           Data.Time
import           GHC.Generics
import           Prelude

data PureScriptDatatype
    = PureScriptDatatype Text
                  PureScriptConstructor
    | PureScriptPrimitive PureScriptPrimitive
     deriving (Show, Eq)

data PureScriptPrimitive
    = EInt
    | EBool
    | EChar
    | EDate
    | EFloat
    | EString
    | EUnit
    | EList PureScriptDatatype
    | EMaybe PureScriptDatatype
    | ETuple2 PureScriptDatatype
              PureScriptDatatype
    | EDict PureScriptPrimitive
            PureScriptDatatype
     deriving (Show, Eq)


data PureScriptConstructor
    = NamedConstructor Text
                       PureScriptValue
    | RecordConstructor Text
                        PureScriptValue
    | MultipleConstructors [PureScriptConstructor]
     deriving (Show, Eq)

data PureScriptValue
    = PureScriptRef Text
    | PureScriptEmpty
    | PureScriptPrimitiveRef PureScriptPrimitive
    | Values PureScriptValue
             PureScriptValue
    | PureScriptField Text
               PureScriptValue
     deriving (Show, Eq)

------------------------------------------------------------

class PureScriptType a  where
    toPureScriptType :: a -> PureScriptDatatype
    toPureScriptType = genericToPureScriptDatatype . from
    default toPureScriptType :: (Generic a, GenericPureScriptDatatype (Rep a)) => a -> PureScriptDatatype

------------------------------------------------------------

class GenericPureScriptDatatype f  where
    genericToPureScriptDatatype :: f a -> PureScriptDatatype

instance (Datatype d, GenericPureScriptConstructor f) =>
         GenericPureScriptDatatype (D1 d f) where
    genericToPureScriptDatatype datatype =
        PureScriptDatatype
            (pack (datatypeName datatype))
            (genericToPureScriptConstructor (unM1 datatype))

-- ------------------------------------------------------------
class GenericPureScriptConstructor f  where
    genericToPureScriptConstructor :: f a -> PureScriptConstructor

instance (Constructor c, GenericPureScriptValue f) =>
         GenericPureScriptConstructor (C1 c f) where
    genericToPureScriptConstructor constructor =
        if conIsRecord constructor
            then RecordConstructor name (genericToPureScriptValue (unM1 constructor))
            else NamedConstructor name (genericToPureScriptValue (unM1 constructor))
      where
        name = pack $ conName constructor

instance (GenericPureScriptConstructor f, GenericPureScriptConstructor g) =>
         GenericPureScriptConstructor (f :+: g) where
    genericToPureScriptConstructor _ =
        MultipleConstructors
            [ genericToPureScriptConstructor (undefined :: f p)
            , genericToPureScriptConstructor (undefined :: g p)]

------------------------------------------------------------

class GenericPureScriptValue f  where
    genericToPureScriptValue :: f a -> PureScriptValue

instance (Selector s, GenericPureScriptValue a) =>
         GenericPureScriptValue (S1 s a) where
    genericToPureScriptValue selector =
        case selName selector of
            "" -> genericToPureScriptValue (undefined :: a p)
            name -> PureScriptField (pack name) (genericToPureScriptValue (undefined :: a p))

instance (GenericPureScriptValue f, GenericPureScriptValue g) =>
         GenericPureScriptValue (f :*: g) where
    genericToPureScriptValue _ =
        Values
            (genericToPureScriptValue (undefined :: f p))
            (genericToPureScriptValue (undefined :: g p))

instance GenericPureScriptValue U1 where
    genericToPureScriptValue _ = PureScriptEmpty

instance PureScriptType a =>
         GenericPureScriptValue (Rec0 a) where
    genericToPureScriptValue _ =
        case toPureScriptType (undefined :: a) of
            PureScriptPrimitive primitive -> PureScriptPrimitiveRef primitive
            PureScriptDatatype name _ -> PureScriptRef name

instance PureScriptType a => PureScriptType [a] where
    toPureScriptType _ = PureScriptPrimitive (EList (toPureScriptType (undefined :: a)))

instance PureScriptType a => PureScriptType (Maybe a) where
    toPureScriptType _ = PureScriptPrimitive (EMaybe (toPureScriptType (undefined :: a)))

instance PureScriptType () where
    toPureScriptType _ = PureScriptPrimitive EUnit

instance PureScriptType Text where
    toPureScriptType _ = PureScriptPrimitive EString

instance PureScriptType Day where
    toPureScriptType _ = PureScriptPrimitive EDate

instance PureScriptType UTCTime where
    toPureScriptType _ = PureScriptPrimitive EDate

instance PureScriptType Float where
    toPureScriptType _ = PureScriptPrimitive EFloat

instance PureScriptType Double where
    toPureScriptType _ = PureScriptPrimitive EFloat

instance PureScriptType Int8 where
    toPureScriptType _ = PureScriptPrimitive EInt

instance PureScriptType Int16 where
    toPureScriptType _ = PureScriptPrimitive EInt

instance PureScriptType Int32 where
    toPureScriptType _ = PureScriptPrimitive EInt

instance PureScriptType Int64 where
    toPureScriptType _ = PureScriptPrimitive EInt

instance (PureScriptType a, PureScriptType b) =>
         PureScriptType (a, b) where
    toPureScriptType _ =
        PureScriptPrimitive $
        ETuple2 (toPureScriptType (undefined :: a)) (toPureScriptType (undefined :: b))


instance (PureScriptType a) =>
         PureScriptType (Proxy a) where
    toPureScriptType _ = toPureScriptType (undefined :: a)

instance (HasPureScriptComparable k, PureScriptType v) =>
         PureScriptType (Map k v) where
    toPureScriptType _ =
        PureScriptPrimitive $
        EDict (toPureScriptComparable (undefined :: k)) (toPureScriptType (undefined :: v))

class HasPureScriptComparable a where
  toPureScriptComparable :: a -> PureScriptPrimitive

instance HasPureScriptComparable String where
  toPureScriptComparable _ = EString

instance PureScriptType Int where
  toPureScriptType _ = PureScriptPrimitive EInt

instance PureScriptType Char where
  toPureScriptType _ = PureScriptPrimitive EChar

instance PureScriptType Bool where
  toPureScriptType _ = PureScriptPrimitive EBool
