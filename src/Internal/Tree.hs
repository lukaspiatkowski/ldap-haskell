{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
  FunctionalDependencies, RankNTypes #-}
module Internal.Tree where

import Data.List
import Data.Tree
import Encoding.LDAPDefs

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

search :: [a -> Bool] -> (a -> Bool) -> SearchScope -> Forest a -> [[a]]
search = searchP [] where
  addIfPred p path acc (Node v _) = if p v then (v:path):acc else acc
  searchBase = addIfPred
  searchOne p path acc (Node v ts) = foldl (addIfPred p (v:path)) acc ts
  searchSub p path acc tree = foldl
    (searchSub p (rootLabel tree:path))
    (searchBase p path acc tree) $
    subForest tree
  chooseSearch scope = case scope of
    BaseObject -> searchBase
    SingleLevel -> searchOne
    WholeSubtree -> searchSub
  searchP path [fx] p scope frst = case find (fx . rootLabel) frst of
    Nothing -> []
    Just tree -> (chooseSearch scope) p path [] tree
  searchP path (fx:fs) p scope frst = case find (fx . rootLabel) frst of
    Nothing -> []
    Just (Node v ts) -> searchP (v:path) fs p scope ts

modify :: [a -> Bool] -> (a -> a) -> Forest a -> Forest a
modify [fx] f = matchingMap (fx . rootLabel) (\(Node v ts) -> Node (f v) ts)
modify (fx:fs) f =
  matchingMap (fx . rootLabel) (\(Node v ts) -> Node v $ modify fs f ts)
