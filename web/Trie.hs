{-# LANGUAGE ConstraintKinds #-}
module Trie where

import X.Prelude hiding (lookup, Empty)
import qualified Data.Hashable as H
import qualified Data.HashMap.Lazy as HM

data Trie k v
  = Branch (Maybe v) (HM.HashMap k (Trie k v))
  | Tip v
  | Empty
  deriving (Show)

type C k = (H.Hashable k, Eq k)

-- * Construction

fromList :: C k => [([k], v)] -> Trie k v
fromList kvs = foldl (\t (ks, v) -> insert ks v t) Empty $ kvs

singleton :: C k => [k] -> v -> Trie k v
singleton ks v = insert ks v Empty

-- * Querying

-- | Lookup exact key sequence
lookup :: (Show k, C k) => [k] -> Trie k v -> Maybe v
lookup (k : ks) t = case t of
  Branch _ hm -> HM.lookup k hm >>= lookup ks
  _ -> Nothing
lookup _ t = case t of
  Branch (Just v) _ -> Just v
  Tip v -> Just v
  _ -> Nothing

t = lookupPrefix ["b", "g"] t
  where
    v li n = (li, n)
    t = fromList $ reverse $
      [ v ["a"] 1
      , v ["b", "c"] 2
      , v ["b", "d", "f"] 2
      , v ["b", "e"] 2
      , v ["b", "e"] 9
      , v ["b"] 3
      ]

-- | Lookup the longest prefix that matches, returning the prefix that
-- matched, along with a maybe value at that point and a subtrie from
-- that point.
lookupPrefix
  :: C k
  => [k] -> Trie k v -> Maybe ([k], Maybe v, Maybe (HM.HashMap k (Trie k v)))
lookupPrefix ks' t = go ks' t
  where
    go kss@ (k : ks) t = case t of
      Branch mv hm -> case HM.lookup k hm >>= go ks of
        Nothing -> Just (kss, mv, Just hm)
        j -> j
      Tip v -> Just (kss, Just v, Nothing)
      Empty -> Nothing
    go _ t = case t of
      Branch mv hm -> Just ([], mv, Just hm)
      Tip v -> Just ([], Just v, Nothing)
      _ -> Nothing

-- * Modification

insert :: C k => [k] -> v -> Trie k v -> Trie k v
insert (k : ks) v t = case t of
  Branch v' hm -> Branch v' (HM.insertWith union k (insert ks v Empty) hm)
    where
  Tip v' -> Branch (Just v') (HM.singleton k (insert ks v Empty))
  Empty -> Branch Nothing (HM.singleton k (insert ks v Empty))
insert _ v t = case t of
  Branch _ hm -> Branch (Just v) hm
  _ -> Tip v

-- | Merge tries by prefering values from the right when prefixes match.
union :: C k => Trie k v -> Trie k v -> Trie k v
union Empty t = t
union t Empty = t
union (Tip _) (Tip v) = Tip v
union (Tip v) (Branch Nothing hm) = Branch (Just v) hm
union (Tip _) t@ (Branch (Just _) _) = t
union (Branch Nothing hm) (Tip v) = Branch (Just v) hm
union (Branch (Just _) hm) (Tip v) = Branch (Just v) hm
union (Branch Nothing hm) (Branch Nothing hm') = Branch Nothing (HM.unionWith union hm hm')
union (Branch Nothing hm) (Branch (Just v) hm') = Branch (Just v) (HM.unionWith union hm hm')
union (Branch (Just v) hm) (Branch Nothing hm') = Branch (Just v) (HM.unionWith union hm hm')
union (Branch (Just v) hm) (Branch (Just v') hm') = Branch (Just v') (HM.unionWith union hm hm')

update = putStrLn "OK"
