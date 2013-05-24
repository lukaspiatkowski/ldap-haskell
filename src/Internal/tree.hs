{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
  FunctionalDependencies, RankNTypes #-}
module Tree where

import Control.Monad.Cont
import Data.List

data Tree a = Tree a [Tree a] deriving (Eq, Show)

foldAll :: (a -> b -> a) -> a -> Tree b -> a
foldAll f z (Tree v ts) = foldl (foldAll f) (f z v) ts

foldOne :: (a -> b -> a) -> a -> Tree b -> a
foldOne f z (Tree v _) = f z v

foldChildren :: (a -> b -> a) -> a -> Tree b -> a
foldChildren f z (Tree _ ts) = foldl (foldOne f) z ts

type CompTree a = forall b. Cont (Bool -> (Maybe (Tree b))) a

class (Monad m) => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance MonadState s (Cont (s -> r)) where
  get = cont (\asr s -> asr s s)
  put s = cont (\asr _ -> asr () s)

travers :: Eq a => (Maybe (Tree a) -> a -> (Maybe (Tree a))) ->
  Maybe (Tree a) -> [a] -> Maybe (Tree a)
travers f Nothing (a:as) = case f Nothing a of
  Nothing -> Nothing
  Just (Tree v ts) -> Just $ Tree v $ traversChildren (f . Just) ts as
travers f (Just tree) as = traversT (f . Just) tree as

traversT :: Eq a => (Tree a -> a -> Maybe (Tree a)) ->
  Tree a -> [a] -> Maybe (Tree a)
traversT f tree [] = Just tree
traversT f tree (a:as) = case f tree a of
  Nothing -> Nothing
  Just (Tree v ts) -> Just $ Tree v $ traversChildren f ts as

traversChildren :: Eq a => (Tree a -> a -> Maybe (Tree a)) ->
  [Tree a] -> [a] -> [Tree a]
traversChildren f ts as =
  case sequence $ filter (/= Nothing) $ (map $ (flip $ traversT f) as) ts of
    Nothing -> []
    Just l -> l

add :: Eq a => Maybe (Tree a) -> [a] -> Maybe (Tree a)
add = undefined

remove :: Eq a => Tree a -> [a] -> Maybe (Tree a)
remove = undefined
