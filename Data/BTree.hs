{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BTree
-- Copyright   :  (c) Highlander Paiva 2022
-- License     :  BSD-style (see the file libraries/LICENSE)
--
-- Maintainer  :  hvpaiva@icloud.com
-- Stability   :  experimental
-- Portability :  portable
--
-- The BTree type, and associated operations. This module allows you to use a
-- BTree to store a set of values, and to efficiently lookup values in the set.
-- The BTree is a balanced binary tree, which means that the tree is more efficient
-- than a conventional list.
-- All BTree operations are performed in O(log n) time.
--
-----------------------------------------------------------------------------

module Data.BTree (
             -- * BTree type
               BTree

             -- * Constructors
             , empty
             , singleton
             , fromList

             -- * Operations
             , insert
             , delete
             , search
             , member
             , height

             -- * Traversal
             , preorder
             , inorder
             , postorder
             , levels

             -- * Ascii Drawing
             , draw
) where

import           Control.Applicative ()
import           Data.Foldable       (Foldable (..), toList)
import qualified Data.List           as L
import           Data.Maybe          (isJust)
import           Data.Monoid         ()
import           Data.Traversable    ()
import           Data.Function       (on)

-- $setup
-- Allow the use of some BTree functions in doctests.
-- >>> import Data.BTree ( insert, delete, search, fromList )


infix  4 `member`

{-
-- just for testing
import Test.QuickCheck
-}

{-|

The 'BTree' type represents a tree using the b-tree algorithm.

A @'BTree' a@ is a self-balancing tree as its nodes are sorted in the inorder traversal. The
node is a set of elements pointing to its children, and a leaf has no children and nothing in itself.

This implementation uses a order 3 'BTree', this means:

    * /{one, two}/ elements per node and /{two, three}/ subtrees.

    * A leaf contains nothing.

    * Every leaf is equidistant from the root.

    * Subtrees must have the same height.

    * Data are ordered left to right.

@

             4              <-- (3) root
         \/      \\
        2       6           <-- (2) internal nodes
       \/ \\     / \\
      1   3   5   7         <-- (1) internal nodes
     \/ \\ \/ \\ \/ \\ \/ \\
    .  ..  ..  ..   .       <-- (0) leafs
@


The complexity of the operations:

+------------+---------------+---------------+
| Algorithm  |  Medium Case  |   Worst Case  |
+============+===============+===============+
| Search     | O(log n)      | O(log n)      |
+------------+---------------+---------------+
| Insert     | O(log n)      | O(log n)      |
+------------+---------------+---------------+
| Delete     | O(log n)      | O(log n)      |
+------------+---------------+---------------+

==== __Examples __

A @'BTree' 'Int'@ may be represented as:

>>> let t = fromList [1,2,3,4,5,6,7]
>>> t
fromList [1,2,3,4,5,6,7]

>>> let n = insert 8 t
>>> n
fromList [1,2,3,4,5,6,7,8]

>>> draw n
"(((. 1 .) 2 (. 3 .)) 4 ((. 5 .) 6 (. 7 . 8 .)))"

@since 1.0.0
-}
data BTree a where
  BTree :: Tree n a -> BTree a

-- | A Natural Number datatype.
--   It represents either a __Zero__ or the __Successor__ of another natural number.
--
--   @since 1.0.0
data Natural
  = Z
  | Succ Natural

-- | A 'BTree' Node.
--   The node can have either /two or tree branches/, having /one or two elements/ on it, respectively.
--   The @n@ represents the height of the Node and the @a@ the actual element.
--
--   @since 1.0.0
data Node n a
  = Subtree (Tree n a) a (Tree n a)
  | Subtree' (Tree n a) a (Tree n a) a (Tree n a)

-- | A 'BTree'. This is an internal implementation, clients will use @'BTree' a@ instead.
--
--   It can have a Leaf or a Branch.
--   A leaf is a 'BTree' with Zero in height and no elements.
--   A branch is a representation of a Node to another 'BTree' with a successor height.
--   The @n@ represents the height of the 'BTree' and the @a@ the actual elements on it.
--
--   @since 1.0.0
data Tree n a where
  Branch :: Node n a -> Tree ('Succ n) a
  Leaf :: Tree 'Z a

-- | The insertion Keep type.
--   It represents the insertion of an element into a 'BTree'.
--   The insertion can be done in two ways:
--   * Keep: The node keeps the element in itself.
--   * Push: Overflow occurs and the element is pushed to the parent.
--
--   Note: This is a function that gets a internal 'BTree' and converts to its
--   polimorphic representation of Tree.
--
--   The @t@ represents the polimorphic representation of Tree, the @n@ represents the height of the 'BTree'
--   and the @a@ the actual element.
--   This prevents the needs of the internal 'BTree' beeing parsed earlyer than when presented to the client.
--
--   @since 1.0.0
type Keep t n a = Tree n a -> t

-- | The insertion Push type.
--   It represents the insertion of an element into a 'BTree'.
--   The insertion can be done in two ways:
--   * Keep: The node keeps the element in itself.
--   * Push: Overflow occurs and the element is pushed to the parent.
--
--   Note: This is a function that gets a internal 'BTree' and converts to its
--   polimorphic representation of Tree.
--
--   The @t@ represents the polimorphic representation of Tree, the @n@ represents the height of the 'BTree'
--   and the @a@ the actual element.
--   This prevents the needs of the internal 'BTree' beeing parsed earlyer than when presented to the client.
--
--   @since 1.0.0
type Push t n a = Tree n a -> a -> Tree n a -> t

-- | Inserts an element into a 'BTree'.
--   Inserts the element into the 'BTree' in inorder fashion.
--
--   +------------+---------------+---------------+
--   | Algorithm  |  Medium Case  |   Worst Case  |
--   +============+===============+===============+
--   | Insert     | O(log n)      | O(log n)      |
--   +------------+---------------+---------------+
--
--   >>> let t = insert 1 empty
--   >>> t
--   fromList [1]
--   >>> draw t
--   "(. 1 .)"
--   >>> let n = insert 2 t
--   >>> n
--   fromList [1,2]
--   >>> draw n
--   "(. 1 . 2 .)"
--
--   @since 1.0.0
insert :: forall a. Ord a => a -> BTree a -> BTree a
insert x (BTree tree) = insert' tree BTree $ \a b c -> BTree (branch a b c)
    where
      insert' :: forall n t. Tree n a -> Keep t n a -> Push t n a -> t
      insert' Leaf = \_ push -> push Leaf x Leaf -- insert in a leaf is always a overflow, so it pushes.
      insert' (Branch n) = i n
        where
          i :: forall p m. ('Succ p ~ m) => Node p a -> Keep t m a -> Push t m a -> t
          i (Subtree' a b c d e) keep push = select' x b d xltb xeqb xbtw xeqd xgtd
            where
              _ = keep :: Tree m a -> t
              _ = push :: Tree m a -> a -> Tree m a -> t
              xltb = insert' a (\k -> keep (branch' k b c d e)) (\p q r -> push (branch p q r) b (branch c d e))
              xeqb = keep (branch' a x c d e)
              xbtw = insert' c (\k -> keep (branch' a b k d e)) (\p q r -> push (branch a b p) q (branch r d e))
              xeqd = keep (branch' a b c x e)
              xgtd = insert' e (keep . branch' a b c d) (\p q r -> push (branch a b c) d (branch p q r))
          i (Subtree a b c) keep _ = select x b xltb xeqb xgtb
            where
              xltb = insert' a (\k -> keep (branch k b c)) (\p q r -> keep (branch' p q r b c))
              xgtb = insert' c (keep . branch a b) (\p q r -> keep (branch' a b p q r))
              xeqb = keep (branch a x c)

-- | The deletion Pull type.
--   It represents the action of pull an element to an upper branch when underflow, and merging the nodes
--   if needed.
--
--   Note: This is a function that takes a Shrunk type and converts it to the
--   polimorphic representation of Tree, merging it nodes and pulling up the elements if underflow.
--
--   The @t@ represents the polimorphic representation of Tree, the @n@ represents the height of the 'BTree'
--   and the @a@ the actual element.
--   This prevents the needs of the internal 'BTree' beeing parsed earlyer than when presented to the client.
--
--   @since 1.0.0
type Pull t n a = Shrunk n a -> t

-- | The Shrunk type.
--   Represents the action when two nodes are merged into one. It's a "partial" type,
--   meaning that it's not a full representation of a merged Node, but the argument
--   for the Pull when the Node is fully merged.
--   The @n@ represents the height of the 'BTree' and the @a@ the actual element.
--
--   @since 1.0.0
data Shrunk (n :: Natural) a where
  H :: Tree n a -> Shrunk ('Succ n) a

-- | Delete an element from a 'BTree'.
--
--   +------------+---------------+---------------+
--   | Algorithm  |  Medium Case  |   Worst Case  |
--   +============+===============+===============+
--   | Delete     | O(log n)      | O(log n)      |
--   +------------+---------------+---------------+
--
--   >>> let t = fromList [1,2,3,4,5,6,7]
--   >>> let n = delete 3 t
--   >>> draw n
--   "((. 1 . 2 .) 4 (. 5 .) 6 (. 7 .))"
--
--   >>> n
--   fromList [1,2,4,5,6,7]
--
--   @since 1.0.0
delete :: forall a. Ord a => a -> BTree a -> BTree a
delete x (BTree tree) = find tree BTree shrink
  where
    shrink :: forall n. Shrunk n a -> BTree a
    shrink (H t) = BTree t

    find :: forall n t. Tree n a -> Keep t n a -> Pull t n a -> t
    find Leaf keep _ = keep Leaf
    find (Branch (Subtree a b c)) keep pull = select x b xltb xeqb xgtb
      where
        xltb, xeqb, xgtb :: t
        xltb = find a (\k -> keep (branch k b c)) (\p -> mrgl p b c)
        xgtb = find c (keep . branch a b) (mrg2r keep pull a b)
        xeqb = replace a (\k r -> keep (branch k r c)) (\p r -> mrgl p r c) (pull (H a))

        mrgl :: forall p. ('Succ p ~ n) => Shrunk p a -> a -> Tree p a -> t
        mrgl (H a') b' (Branch (Subtree c' d e)) = pull (H (branch' a' b' c' d e))
        mrgl (H a') b' (Branch (Subtree' c' d e f g)) = keep (branch (branch a' b' c') d (branch e f g))
    find (Branch (Subtree' a b c d e)) keep _ = select' x b d xltb xeqb xbtw xeqd xgtd
      where
        xltb = find a (\k -> keep (branch' k b c d e)) (\p -> mrgl p b c d e)
        xbtw = find c (\k -> keep (branch' a b k d e)) (\p -> mrgm a b p d e)
        xgtd = find e (keep . branch' a b c d) (mrg3r keep a b c d)
        xeqb = replace a (\k r -> keep (branch' k r c d e)) (\p r -> mrgl p r c d e) (keep (branch c d e))
        xeqd = replace c (\k r -> keep (branch' a b k r e)) (\p r -> mrgm a b p r e) (keep (branch a b c))

        mrgl (H a') b' (Branch (Subtree' c' d' e' f g)) h i = keep (branch' (branch a' b' c') d' (branch e' f g) h i)
        mrgl (H a') b' (Branch (Subtree c' d' e')) f g = keep (branch (branch' a' b' c' d' e') f g)

        mrgm a' b' (H c') d' (Branch (Subtree' e' f g h i)) = keep (branch' a' b' (branch c' d' e') f (branch g h i))
        mrgm a' b' (H c') d' (Branch (Subtree e' f g)) = keep (branch a' b' (branch' c' d' e' f g))

    replace :: forall n t. Tree n a -> Keep (a -> t) n a -> Pull (a -> t) n a -> t -> t
    replace Leaf _ _ leaf = leaf
    replace (Branch (Subtree a b c)) keep pull _
      = replace c (keep . branch a b) (mrg2r keep pull a b) (pull (H a) b)
    replace (Branch (Subtree' a b c d e)) keep _ _ =
      replace e (keep . branch' a b c d) (mrg3r keep a b c d) (keep (branch a b c) d)

    mrg2r :: forall p t. Keep t ('Succ p) a -> Pull t ('Succ p) a -> Tree p a -> a -> Shrunk p a -> t
    mrg2r _ pull (Branch (Subtree a b c)) d (H e) = pull (H (branch' a b c d e))
    mrg2r keep _ (Branch (Subtree' a b c d e)) f (H g) = keep (branch (branch a b c) d (branch e f g))

    mrg3r :: forall p t. Keep t ('Succ p) a -> Tree p a -> a -> Tree p a -> a -> Shrunk p a -> t
    mrg3r keep a b (Branch (Subtree' c d e f g)) h (H i) = keep (branch' a b (branch c d e) f (branch g h i))
    mrg3r keep a b (Branch (Subtree c d e)) f (H g) = keep (branch a b (branch' c d e f g))

-- | Search an element in a 'BTree'. It will return a [Maybe](https://hackage.haskell.org/package/base-4.16.2.0/docs/Prelude.html#t:Maybe).
--
--   +------------+---------------+---------------+
--   | Algorithm  |  Medium Case  |   Worst Case  |
--   +============+===============+===============+
--   | Search     | O(log n)      | O(log n)      |
--   +------------+---------------+---------------+
--
--   >>> let t = fromList [1,2,3,4,5]
--
--   >>> search 3 t
--   Just 3
--
--   >>> search 6 t
--   Nothing
--
--   @since 1.0.0
search :: forall a. Ord a => a -> BTree a -> Maybe a
search x (BTree tree) = search' tree
  where
    search' :: forall n. Tree n a -> Maybe a
    search' Leaf = Nothing
    search' (Branch (Subtree a b c)) = select x b xltb xeqb xgtb
      where
        xltb, xeqb, xgtb :: Maybe a
        xltb = search' a
        xeqb = Just x
        xgtb = search' c
    search' (Branch (Subtree' a b c d e)) = select' x b d xltb xeqb xbtw xeqd xgtd
      where
        xltb, xeqb, xbtw, xeqd, xgtd :: Maybe a
        xltb = search' a
        xeqb = Just x
        xbtw = search' c
        xeqd = Just x
        xgtd = search' e

-- | A empty 'BTree'. It consists of a single leaf.
--
-- It may be used to construct a new 'BTree' by inserting elements into it.
--
--   >>> let t = empty
--   >>> t
--   fromList []
--
--   >>> insert 1 t
--   fromList [1]
--
--   >>> insert 2 empty
--   fromList [2]
--
--   @since 1.0.0
empty :: BTree a
empty = BTree Leaf

-- | Creates a singleton 'BTree' from a single element.
--
--   >>> let t = singleton 1
--   >>> t
--   fromList [1]
--
--   @since 1.0.0
singleton :: Ord a => a -> BTree a
singleton x = insert x empty

-- | Creates a 'BTree' from a list.
--
--   >>> let t = fromList [1,2,3,4,5,6,7]
--   >>> t
--   fromList [1,2,3,4,5,6,7]
--
--   >>> draw t
--   "(((. 1 .) 2 (. 3 .)) 4 ((. 5 .) 6 (. 7 .)))"
--
--   @since 1.0.0
fromList :: Ord a => [a] -> BTree a
fromList = L.foldl' (flip insert) empty

-- | Checks if the element is in the 'BTree'.
--
--   >>> let t = fromList [1,2,3,4,5,6,7]
--
--   >>> 3 `member` t
--   True
--
--   >>> member 8 t
--   False
--
--   @since 1.0.0
member :: forall a. Ord a => a -> BTree a -> Bool
member x t = isJust $ search x t

-- | Return the 'BTree' as a list in a in-order traversal.
--
-- The __in-order__ traversal runs through the elements of the 'BTree' in the following order:
--
--      (1) The left subtree.
--
--      (2) The root element.
--
--      (3) The right subtree.
--
--   /@Note:@ The in-order traversal is the default traversal for the 'BTree', and is used to return as list./
--
--   A 'BTree' of @[1, 2, 3, 4, 5, 6, 7]@ is represented as:
--
-- @
--             4
--         \/      \\
--        2       6
--       \/ \\     \/ \\
--      1   3   5   7
--     \/ \\ \/ \\ \/ \\ \/ \\
--    .  ..  ..  ..   .
--
-- @
--
--   >>> inorder (fromList [1,2,3,4,5,6,7])
--   [1,2,3,4,5,6,7]
--
--   @since 1.0.0
inorder :: forall a. BTree a -> [a]
inorder (BTree tree) = pre tree
  where
    pre :: Tree n a -> [a]
    pre (Branch (Subtree a b c))      = pre a ++ [b] ++ pre c
    pre (Branch (Subtree' a b c d e)) = pre a ++ [b] ++ pre c ++ [d] ++ pre e
    pre Leaf                          = []

-- | Return the 'BTree' as a list in a post-order traversal.
--
-- The __post-order__ traversal runs through the elements of the 'BTree' in the following order:
--
--      (1) The left subtree.
--
--      (2) The right subtree.
--
--      (3) The root element.
--
--   A 'BTree' of @[1, 2, 3, 4, 5, 6, 7]@ is represented as:
--
-- @
--             4
--         \/      \\
--        2       6
--       \/ \\     \/ \\
--      1   3   5   7
--     \/ \\ \/ \\ \/ \\ \/ \\
--    .  ..  ..  ..   .
-- @
--
--   >>> postorder (fromList [1,2,3,4,5,6,7])
--   [1,3,2,5,7,6,4]
--
--   @since 1.0.0
postorder :: forall a. BTree a -> [a]
postorder (BTree tree) = pos tree
  where
    pos :: Tree n a -> [a]
    pos (Branch (Subtree a b c))      = pos a ++ pos c ++ [b]
    pos (Branch (Subtree' a b c d e)) = pos a ++ pos c ++ pos e ++ [b] ++ [d]
    pos Leaf                          = []

-- | Return the 'BTree' as a list in a pre-order traversal.
--
-- The __pre-order__ traversal runs through the elements of the 'BTree' in the following order:
--
--      (1) The root element.
--
--      (2) The left subtree.
--
--      (3) The right subtree.
--
--   A 'BTree' of @[1, 2, 3, 4, 5, 6, 7]@ is represented as:
--
-- @
--             4
--         \/      \\
--        2       6
--       \/ \\     \/ \\
--      1   3   5   7
--     \/ \\ \/ \\ \/ \\ \/ \\
--    .  ..  ..  ..   .
-- @
--
--   >>> preorder (fromList [1,2,3,4,5,6,7])
--   [4,2,1,3,6,5,7]
--
--   @since 1.0.0
preorder :: forall a. BTree a -> [a]
preorder (BTree tree) = ino tree
  where
    ino :: Tree n a -> [a]
    ino (Branch (Subtree a b c))      = [b] ++ ino a ++ ino c
    ino (Branch (Subtree' a b c d e)) = [b] ++ [d] ++ ino a ++ ino c ++ ino e
    ino Leaf                          = []

-- | The height of the 'BTree'.
--
-- Represents the number of levels in the 'BTree'.
--
-- @
--             4             <-- height 3
--         \/      \\
--        2       6          <-- height 2
--       \/ \\     \/ \\
--      1   3   5   7        <-- height 1
--     \/ \\ \/ \\ \/ \\ \/ \\
--    .  ..  ..  ..   .      <-- height 0
-- @
--
--   >>> height (fromList [1,2,3,4,5,6,7])
--   3
--
--   @since 1.0.0
height :: forall a. BTree a -> Int
height (BTree tree) = height' tree
  where
    height' :: Tree n a -> Int
    height' (Branch (Subtree a _ _))      = 1 + height' a        -- As thet are equidistant, the height of a subtree is always the same as the others.
    height' (Branch (Subtree' a _ _ _ _)) = 1 + height' a   -- So we only need to check it once.
    height' Leaf                          = 0

-- | Draws the 'BTree'.
--
-- The output is a string of the form:
--
--      * Each Leaf is represented by a \' . \'
--
--      * Each Node is represented as /(left, value, right)/ where left and right are the left and right subtrees.
--
--   So, the current list @[1, 2, 3, 4, 5, 6, 7]@ will be represented as:
--
--   @"(((. 1 .) 2 (. 3 .)) 4 ((. 5 .) 6 (. 7 .)))"@
--
--   Which is an inline representation of the 'BTree'.
--
-- ==== __Examples __
--
--   >>> draw (fromList [1,2,3,4,5,6,7])
--   "(((. 1 .) 2 (. 3 .)) 4 ((. 5 .) 6 (. 7 .)))"
--
--   @since 1.0.0
draw :: forall a. Show a => BTree a -> String
draw (BTree tree) = draw' tree
  where
    draw' :: Tree n a -> String
    draw' (Branch (Subtree a b c)) = "(" ++ draw' a ++ " " ++ show b ++ " " ++ draw' c ++ ")"
    draw' (Branch (Subtree' a b c d e)) = "(" ++ draw' a ++ " " ++ show b ++ " " ++ draw' c ++ " " ++ show d ++ " " ++ draw' e ++ ")"
    draw' Leaf = "."

-- | Return the 'BTree' as a list of lists, grouped by hights. 
-- 
-- @
--
--   a
--  / \\    => [[a], [b,c]]
-- b   c
-- @
-- 
--   >>> levels (fromList [1,2,3,4,5,6,7])
--   [[4],[2,6],[1,3,5,7]]
levels :: forall a. Ord a => BTree a -> [[a]]
levels (BTree tree) = map (map snd) $ L.groupBy ((==) `on` fst) $ L.sort $ levels' 0 tree
  where
    levels' :: Int -> Tree n a -> [(Int, a)]
    levels' _ Leaf = []
    levels' n (Branch (Subtree a b c)) = levels' (n + 1) a ++ [(n, b)] ++ levels' (n + 1) c
    levels' n (Branch (Subtree' a b c d e)) = levels' (n + 1) a ++ [(n, b)] ++ levels' (n + 1) c ++ [(n, d)] ++ levels' (n + 1) e
   

-- | @since 1.0.0
instance Foldable BTree where
  foldMap = foldm
    where
      foldm :: forall m a. Monoid m => (a -> m) -> BTree a -> m
      foldm f (BTree t) = fm t
        where
          fm :: forall n. Tree n a -> m
          fm (Branch (Subtree a b c))      = fm a <> f b <> fm c
          fm (Branch (Subtree' a b c d e)) = fm a <> f b <> fm c <> f d <> fm e
          fm Leaf                          = mempty

-- | @since 1.0.0
instance Show a => Show (BTree a) where
  showsPrec n t = showParen (n > 10) $ showString "fromList " . shows (toList t)

---- Internal Utility Functions ----------------------------------------------------------------------------------------

-- | Utility function to select the correct element given a comparison of two other elements.
select :: Ord a => a -> a -> p -> p -> p -> p
select x y lt eq gt = case compare x y of { LT -> lt; EQ -> eq; GT -> gt }

-- | Utility function to select the correct element given a comparison of three other elements.
select' :: Ord a => a -> a -> a -> p -> p -> p -> p -> p -> p
select' x y z xlty xeqy xbtw xeqz xgtz = select x y xlty xeqy (select x z xbtw xeqz xgtz)

-- | A branch constructor. Convenient method for creating a branch with a single element and two subtrees.
branch :: Tree n a -> a -> Tree n a -> Tree ('Succ n) a
branch a b c = Branch (Subtree a b c)

-- | A branch constructor. Convenient method for creating a branch with a two elements and three subtrees.
branch' :: Tree n a -> a -> Tree n a -> a -> Tree n a -> Tree ('Succ n) a
branch' a b c d e = Branch (Subtree' a b c d e)

t' :: BTree Int
t' = fromList [1,2,3,4,5,6,7]
