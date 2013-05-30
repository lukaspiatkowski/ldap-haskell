{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
  FunctionalDependencies, RankNTypes #-}
module Internal.Tree where

import Data.List
import Data.Tree
import Encoding.LDAPDefs

foldSubtree :: (a -> b -> a) -> a -> Tree b -> a
foldSubtree f z (Node v ts) = foldl (foldSubtree f) (f z v) ts

foldBase :: (a -> b -> a) -> a -> Tree b -> a
foldBase f z  = (f z) . rootLabel

foldOne :: (a -> b -> a) -> a -> Tree b -> a
foldOne f z = foldl (foldBase f) z . subForest

chooseFold :: SearchScope -> (a -> b -> a) -> a -> Tree b -> a
chooseFold scope = case scope of
  BaseObject -> foldBase
  SingleLevel -> foldOne
  WholeSubtree -> foldSubtree

matchingMap :: (a -> Bool) -> (a -> a) -> [a] -> [a]
matchingMap p f = map (\a -> if p a then f a else a)

add :: [a -> Bool] -> a -> Forest a -> Forest a
add [] a = (:) $ return a
add (fx:fs) a =
  matchingMap (fx . rootLabel) (\(Node v ts) -> Node v $ add fs a ts)

remove :: [a -> Bool] -> Forest a -> Forest a
remove [fx] = filter (not . fx . rootLabel)
remove (fx:fs) =
  matchingMap (fx . rootLabel) (\(Node v ts) -> Node v $ remove fs ts)

search :: [a -> Bool] -> (a -> Bool) -> SearchScope -> Forest a -> [a]
search [fx] p scope frst = case find (fx . rootLabel) frst of
  Nothing -> []
  Just tree ->
    (chooseFold scope) (\acc v -> if p v then (v:acc) else acc) [] tree
search (fx:fs) p scope frst = case find (fx . rootLabel) frst of
  Nothing -> []
  Just tree -> search fs p scope (subForest tree)

modify :: [a -> Bool] -> (a -> a) -> Forest a -> Forest a
modify [fx] f = matchingMap (fx . rootLabel) (\(Node v ts) -> Node (f v) ts)
modify (fx:fs) f =
  matchingMap (fx . rootLabel) (\(Node v ts) -> Node v $ modify fs f ts)
