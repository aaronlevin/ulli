{-# LANGUAGE OverloadedStrings, ExistentialQuantification, Rank2Types, TypeOperators #-}

module UlliRest.Types ( delete
  , insertAt
  , jsonToAlg
  , ListAlg
  , listInterpreter
  , noop
  , push
  , set
  , stringInterpreter
  , toJsonInterpreter) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (mzero)
import Control.Monad.Free (Free (Free, Pure))
import Data.Aeson ((.:), (.=), FromJSON(..), object, Result, ToJSON(..), Value(Object, String))
import Data.Aeson.Types (parse, Parser)
import Data.Functor()
import Data.Functor.Coproduct (Coproduct(Coproduct), coproduct, getCoproduct, left, right)
import Data.Vector (empty, foldr', snoc, Vector)

data UlliAlgOp = UpdateTitle
               | UpdateDescription

instance ToJSON UlliAlgOp where
  toJSON UpdateTitle = "up-title"
  toJSON UpdateDescription = "up-desc"

instance FromJSON UlliAlgOp where
  parseJSON (String "up-title") = return UpdateTitle
  parseJSON (String "up-desc")  = return UpdateDescription
  parseJSON _                   = mzero

data UlliAlg next = Title String next
                  | Description String next

instance Functor UlliAlg where
  fmap f (Title s a) = Title s (f a)
  fmap f (Description s a) = Description s (f a)

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

instance Functor (ListAlg a) where
  fmap f (Push a n) = Push a (f n)
  fmap f (InsertAt i a n) = InsertAt i a (f n)
  fmap f (Set i a n) = Set i a (f n)
  fmap f (Delete i n) = Delete i (f n)

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

  go as (Pure ()) = as


toJsonInterpreter :: ToJSON a => Free (ListAlg a) () -> Vector Value
toJsonInterpreter = go empty where
  go v (Free (Push a n)) = go (snoc v json) n where
    json = object [ "edit" .= PushEdit
                  , "value" .= a
                  ]

  go v (Free (InsertAt i a n)) = go (snoc v json) n where
    json = object [ "edit" .= InsertAtEdit
                  , "value" .= a
                  , "index" .= i
                  ]

  go v (Free (Set i a n)) = go (snoc v json) n where
    json = object [ "edit" .= SetEdit
                  , "value" .= a
                  , "index" .= i
                  ]

  go v (Free (Delete i n)) = go (snoc v json) n where
    json = object [ "edit" .= DeleteEdit
                  , "index" .= i
                  ]

  go v (Pure ()) = v

stringInterpreter :: Show a => Free (ListAlg a) () -> String
stringInterpreter = go [] where
  go str (Free (Push a n)) = go (("Push: " ++ show a) : str) n
  go str (Free (Delete i n)) = go (("Delete[" ++ show i ++ "]") : str) n
  go str (Free (InsertAt i a n)) = go (("InsertAt[" ++ show i ++ "]: " ++ show a) : str) n
  go str (Free (Set i a n)) = go (("Set[" ++ show i ++ "]: " ++ show a) : str) n
  go str (Pure ()) = foldr (++) "" $ reverse $ map (++ "\n") str

fromJsonParser :: FromJSON a => Vector Value -> Parser (Free (ListAlg a) ())
fromJsonParser = foldr' go (pure (Pure()))
  where
    -- we'll keep my desperate attempt at point style here. do notation is clearer. 
    -- go (Object v) p = p >>= (\f -> (>>=) (v .: "edit" >>= handleEdit) (return . (flip (>>)) f))
    go (Object v) p = do 
      freeAlg <- p
      editAlg <- (v .: "edit") >>= handleEdit
      return (editAlg >> freeAlg)
      where 
        handleEdit e = case e of
          PushEdit     -> push <$> (v .: "value")
          InsertAtEdit -> insertAt <$> (v .: "index") <*> (v .: "value")
          SetEdit      -> set <$> (v .: "index") <*> (v .: "value")
          DeleteEdit   -> delete <$> (v .: "index")
    go _ p = p

jsonToAlg :: FromJSON a => Vector Value -> Result (Free (ListAlg a) ())
jsonToAlg = parse fromJsonParser

newtype FullAlgebra a b = FullAlgebra { getAlgebra :: Coproduct (ListAlg a) UlliAlg b }

instance Functor (FullAlgebra a) where
  fmap f (FullAlgebra copro) = FullAlgebra $ fmap f copro

push2 :: a -> Free (FullAlgebra a) ()
push2 a = Free (FullAlgebra . left $ Push a (Pure ()))

stringList :: (Show i) => ListAlg i a -> String
stringList (Push i _) = show i
stringList _ = ""

ulliList :: UlliAlg a -> String
ulliList (Title str _) = str
ulliList (Description str _) = str

--stringInterpreter2 :: Show a => Free (FullAlgebra a) () -> String
--stringInterpreter2 = go []
  --where go xs Free(FullAlgebra(Left(Push i n))) = 
--stringInterpreter2 Free(FullAlgebra(Left(Push i n))) = 
--stringInterpreter2 (Free x) = go [] fa 
  --where go xs fa            = go' xs ((getCoproduct . getAlgebra) fa)
        --go' xs (Left list)  = 

type f :~> g = forall a. f a -> g a 
infixr 0 :~>

data X a = X a
data Y a = Y a

instance Functor X where
  fmap f (X a) = X (f a)

instance Functor Y where
  fmap f (Y a) = Y (f a)

foo :: X a -> Y a
foo (X a) = Y a

xy :: X :~> Y
xy = foo



































