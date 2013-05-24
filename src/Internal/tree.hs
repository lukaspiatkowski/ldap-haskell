{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
  FunctionalDependencies, RankNTypes #-}
module Tree where

import Control.Monad.Cont
import Data.List

data Tree a = Root [Tree a] | Tree a [Tree a] deriving (Eq, Show)
data Scope = Base | One | Subtree deriving (Eq, Show)

foldSubtree :: (a -> b -> a) -> a -> Tree b -> a
foldSubtree f z (Tree v ts) = foldl (foldSubtree f) (f z v) ts

foldBase :: (a -> b -> a) -> a -> Tree b -> a
foldBase f z (Tree v _) = f z v

foldOne :: (a -> b -> a) -> a -> Tree b -> a
foldOne f z (Tree _ ts) = foldl (foldBase f) z ts

chooseFold :: Scope -> (a -> b -> a) -> a -> Tree b -> a
chooseFold scope = case scope of
  Base -> foldBase
  One -> foldOne
  Subtree -> foldSubtree

type CompTree a = forall b. Cont (Bool -> (Maybe (Tree b))) a

class (Monad m) => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

instance MonadState s (Cont (s -> r)) where
  get = cont (\asr s -> asr s s)
  put s = cont (\asr _ -> asr () s)

changeChildren :: ([Tree a] -> [Tree a]) -> Tree a -> Tree a
changeChildren f t = case t of
  Root ts -> Root $ f ts
  Tree v ts -> Tree v $ f ts

unJust :: Maybe a -> a
unJust (Just a) = a

unMaybeList :: Eq a => [Maybe a] -> [a]
unMaybeList = unJust . sequence . filter (/= Nothing)

selectMap :: Eq a => (a -> Maybe a) -> (a -> Bool) -> [a] -> [a]
selectMap f p = unMaybeList . map (\a -> if p a then f a else Just a)

match :: Eq a => a -> Tree a -> Bool
match a (Tree v _) = v == a

getChildren :: Tree a -> [Tree a]
getChildren t = case t of
  Root ts -> ts
  Tree _ ts -> ts

add :: Eq a => [a] -> Tree a -> Tree a
add as = let
    addList [] ts = ts
    addList (ax:as) ts = if any (\(Tree v _) -> v == ax) ts then
      selectMap (Just . add as) (match ax) ts else
      ((Tree ax $ addList as []):ts)
  in case as of
    [] -> id
    _ -> changeChildren (addList as)

findAndChange :: Eq a => (Tree a -> Maybe (Tree a)) -> [a] ->
  Tree a -> Maybe (Tree a)
findAndChange f [] t = f t
findAndChange f (ax:as) t = let
    xTs = selectMap (findAndChange f as) (match ax)
  in Just $ case t of
    Root ts -> Root $ xTs ts
    Tree v ts -> Tree v $ xTs ts

remove :: Eq a => [a] -> Tree a -> Tree a
remove (ax:as) = unJust . findAndChange (\_ -> Nothing) (ax:as)

modify :: Eq a => [a] -> a -> Tree a -> Tree a
modify (ax:as) a =
  unJust . findAndChange (\(Tree _ ts) -> Just $ Tree a ts) (ax:as)

search :: Eq a => [a] -> (a -> Bool) -> Scope -> Tree a -> [a]
search [] p scope t =
  (chooseFold scope) (\acc v -> if p v then (v:acc) else acc) [] t
search (ax:as) p scope t = searchList (ax:as) p scope $ getChildren t
  where
    searchList (ax:as) p scope ts = case find (match ax) ts of
      Nothing -> []
      Just t -> search as p scope t
