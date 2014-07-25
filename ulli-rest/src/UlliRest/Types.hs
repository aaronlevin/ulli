module UlliRest.Types ( delete
  , get
  , insertAt
  , listInterpreter
  , noop
  , pop
  , push
  , set) where

import Control.Monad.Free (Free (Free, Pure))
import Data.Functor()

data ListEdit a next = Push a next
                     | InsertAt Int a next
                     | Set Int a next
                     | Delete Int next
                     | Pop (Maybe a -> next)
                     | Get Int (Maybe a -> next)

instance Functor (ListEdit a) where
  fmap f (Push a n) = Push a (f n)
  fmap f (InsertAt i a n) = InsertAt i a (f n)
  fmap f (Set i a n) = Set i a (f n)
  fmap f (Pop g) = Pop (f . g)
  fmap f (Delete i n) = Delete i (f n)
  fmap f (Get i g) = Get i (f . g)

push :: a -> Free (ListEdit a) ()
push a = Free (Push a (Pure ()))

insertAt :: Int -> a -> Free (ListEdit a) ()
insertAt i a = Free (InsertAt i a (Pure ()))

set :: Int -> a -> Free (ListEdit a) ()
set i a = Free (Set i a (Pure ()))

delete :: Int -> Free (ListEdit a) ()
delete i = Free (Delete i (Pure ()))

pop :: (Maybe a -> Free (ListEdit a) ()) -> Free (ListEdit a) ()
pop f = Free (Pop f)

get :: Int -> (Maybe a -> Free (ListEdit a) ()) -> Free (ListEdit a) ()
get i f = Free (Get i f)

noop :: Free (ListEdit a) ()
noop = Pure ()

-- | we assume the index starts at 0
listInterpreter :: Free (ListEdit a) () -> [a]
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

  go [] (Free (Pop g)) = go [] (g Nothing)
  go as@(a:_) (Free (Pop g)) = go as (g (Just a))

  go as (Free (Get i g)) = go as (g element) where
    element = case splitAt i as of
      (_,[]) -> Nothing
      (_,x:_) -> Just x

  go as (Pure ()) = as
 
