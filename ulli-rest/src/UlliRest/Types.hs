{-# LANGUAGE OverloadedStrings #-}

module UlliRest.Types ( delete
  , insertAt
  , listInterpreter
  , noop
  , push
  , set) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Free (Free (Free, Pure))
import Data.Aeson ((.:), (.=), FromJSON(..), object, ToJSON(..), Value(Null, Object, String))
import Data.Functor()
-- import Data.Text (Text)

-- | Algebra operations as basic data types for simple pattern matching
data ListAlgOp = PushEdit
               | InsertAtEdit
               | SetEdit
               | DeleteEdit

instance ToJSON ListAlgOp where
  toJSON PushEdit     = String "push"
  toJSON InsertAtEdit = String "insert"
  toJSON SetEdit      = String "set"
  toJSON DeleteEdit   = String "delete"

instance FromJSON ListAlgOp where
  parseJSON (String "push")   = return PushEdit
  parseJSON (String "insert") = return InsertAtEdit
  parseJSON (String "set")    = return SetEdit
  parseJSON (String "delete") = return DeleteEdit
  parseJSON _                 = mzero

-- | an Algebra of seralizable list operations
data ListAlg a next = Push a next
                    | InsertAt Int a next
                    | Set Int a next
                    | Delete Int next
                    | Noop

-- | We can serialize the List Algebra in two ways: a tree structure 
-- | or a list structure. The only difference is the tree structure has
-- | the next instruction in the JSON document, whereas the list structure
-- | implicitly has the next instruction behind it in a list. 
-- | therefore, we use two newtypes to give serialized versions of both these.
-- newtype ListAlgTree a next = ListAlgTree { unListAlgTree :: ListAlg a next }

instance (ToJSON a, ToJSON next) => ToJSON (ListAlg a next) where
  toJSON (Push a next) = object [ "edit" .= PushEdit
                                , "value" .= a
                                , "next" .= next
                                ]

  toJSON (InsertAt i a next) = object [ "edit" .= InsertAtEdit
                                      , "value" .= a
                                      , "index" .= i
                                      , "next" .= next
                                      ]

  toJSON (Set i a next) = object [ "edit" .= SetEdit
                                 , "value" .= a
                                 , "index" .= i
                                 , "next" .= next
                                 ]

  toJSON (Delete i next) = object [ "edit" .= DeleteEdit
                                  , "index" .= i
                                  , "next" .= next
                                  ]

  toJSON (Noop) = Null

instance (FromJSON a, FromJSON next) => FromJSON (ListAlg a next) where
  parseJSON (Object v) = (v .: "edit") >>= handleEdit where
    handleEdit e = case e of
      PushEdit      -> Push <$> v .: "value" <*> v .: "next"
      InsertAtEdit  -> InsertAt <$> v .: "value" <*> v .: "index" <*> v .: "next"
      SetEdit       -> Set <$> v .: "value" <*> v .: "index" <*> v .: "next"
      DeleteEdit    -> Delete <$> v .: "index" <*> v .: "next"

  parseJSON _ = mzero

instance Functor (ListAlg a) where
  fmap f (Push a n) = Push a (f n)
  fmap f (InsertAt i a n) = InsertAt i a (f n)
  fmap f (Set i a n) = Set i a (f n)
  fmap f (Delete i n) = Delete i (f n)
  fmap _ Noop = Noop

push :: a -> Free (ListAlg a) ()
push a = Free (Push a (Pure ()))

insertAt :: Int -> a -> Free (ListAlg a) ()
insertAt i a = Free (InsertAt i a (Pure ()))

set :: Int -> a -> Free (ListAlg a) ()
set i a = Free (Set i a (Pure ()))

delete :: Int -> Free (ListAlg a) ()
delete i = Free (Delete i (Pure ()))

noop :: Free (ListAlg a) ()
noop = Pure ()

-- | we assume the index starts at 0
listInterpreter :: Free (ListAlg a) () -> [a]
listInterpreter = go [] where

  go as (Free (Push a n)) = go (a:as) n

  go as (Free (InsertAt i a n)) = go newList n where
    newList = let (h,t) = splitAt i as in (h ++ (a:t))

  go as (Free (Set i a n)) = go newList n where
    newList = case splitAt i as of
      (_,[]) -> as
      (h,_:ts) -> h ++ (a:ts)

  go as (Free (Delete i n)) = go newList n where
    newList = case splitAt i as of
      (_,[]) -> as
      (h,_:ts) -> h ++ ts

  go as (Free Noop) = as

  go as (Pure ()) = as
