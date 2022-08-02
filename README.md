# BTree

The BTree type represents a tree using the b-tree algorithm.

A BTree is a self-balancing tree as its nodes are sorted in the inorder traversal.
The node is a set of elements pointing to its children, and a leaf has no children and nothing in itself.

This implementation uses an order 3 BTree, this means:

- {one, two} elements per node and {two, three} subtrees.
- A leaf contains nothing.
- Every leaf is equidistant from the root.
- Subtrees must have the same height.
- Data are ordered left to right.

```
           4               <-- (3) root
        /     \
       2       6           <-- (2) internal nodes
      / \     / \
     1   3   5   7         <-- (1) internal nodes
    / \ / \ / \ / \
   .  ..  ..  ..   .       <-- (0) leafs
```

The complexity of the main operations:

| Algorithm | Medium Case | Worst Case   |
|-----------|-------------|--------------|
| Insert    | O(log n)    | O(log n)     |
| Delete    | O(log n)    | O(log n)     |
| Search    | O(log n)    | O(log n)     |

## Instalation

```haskell
$> cabal install btree-algorithm
```

then you import the module:

```haskell
import Data.BTree
```

## Constructors

### empty :: BTree a #

An empty BTree. It consists of a single leaf.

It may be used to construct a new BTree by inserting elements into it.

```haskell
>>> let t = empty
>>> t
fromList []

>>> insert 1 t
fromList [1]

>>> insert 2 empty
fromList [2]
```

### singleton :: Ord a => a -> BTree a #

Creates a singleton BTree from a single element.

```haskell
>>> let t = singleton 1
>>> t
fromList [1]
```

### fromList :: Ord a => [a] -> BTree a #

Creates a BTree from a list.

```haskell
>>> let t = fromList [1,2,3,4,5,6,7]
>>> t
fromList [1,2,3,4,5,6,7]

>>> draw t
"(((. 1 .) 2 (. 3 .)) 4 ((. 5 .) 6 (. 7 .)))"
```

## Operations

### insert :: forall a. Ord a => a -> BTree a -> BTree a #

Inserts an element into a BTree. Inserts the element into the BTree in inorder fashion.

```haskell
>>> let t = insert 1 empty
>>> t
fromList [1]

>>> draw t
"(. 1 .)"

>>> let n = insert 2 t
>>> n
fromList [1,2]

>>> draw n
"(. 1 . 2 .)"
```

### delete :: forall a. Ord a => a -> BTree a -> BTree a #

Delete an element from a BTree.

```haskell
>>> let t = fromList [1,2,3,4,5,6,7]
>>> let n = delete 3 t
>>> draw n
"((. 1 . 2 .) 4 (. 5 .) 6 (. 7 .))"

>>> n
fromList [1,2,4,5,6,7]
```

### search :: forall a. Ord a => a -> BTree a -> Maybe a #

Search an element in a BTree. It will return a Maybe.

```haskell
>>> let t = fromList [1,2,3,4,5]
>>> search 3 t
Just 3

>>> search 6 t
Nothing
```

### height :: forall a. BTree a -> Int #

The height of the BTree.

Represents the number of levels in the BTree.

```
            4             <-- height 3
        /      \
       2       6          <-- height 2
      / \     / \
     1   3   5   7        <-- height 1
    / \ / \ / \ / \
   .  ..  ..  ..   .      <-- height 0
```

```haskell
>>> height (fromList [1,2,3,4,5,6,7])
3
```

## Traversal

### preorder :: forall a. BTree a -> [a] #

Return the BTree as a list in a **pre-order** traversal.

The pre-order traversal runs through the elements of the BTree in the following order:

1. The root element.
2. The left subtree.
3. The right subtree.

A BTree of `[1, 2, 3, 4, 5, 6, 7]` is represented as:

```
            4
        /      \
       2       6
      / \     / \
     1   3   5   7
    / \ / \ / \ / \
   .  ..  ..  ..   .
```

```haskell
>>> preorder (fromList [1,2,3,4,5,6,7])
[4,2,1,3,6,5,7]
```

### inorder :: forall a. BTree a -> [a] #

Return the BTree as a list in an **in-order** traversal.

The in-order traversal runs through the elements of the BTree in the following order:

1. The left subtree.
2. The root element.
3. The right subtree.

> Note: The in-order traversal is the default traversal for the BTree, and is used to return as list.

A BTree of `[1, 2, 3, 4, 5, 6, 7]` is represented as:

```
            4
        /      \
       2       6
      / \     / \
     1   3   5   7
    / \ / \ / \ / \
   .  ..  ..  ..   .
```

```haskell
>>> inorder (fromList [1,2,3,4,5,6,7])
[1,2,3,4,5,6,7]
```

### postorder :: forall a. BTree a -> [a] #

Return the BTree as a list in a **post-order** traversal.

The post-order traversal runs through the elements of the BTree in the following order:

1. The left subtree.
2. The right subtree.
3. The root element.

A BTree of `[1, 2, 3, 4, 5, 6, 7]` is represented as:

```
            4
        /      \
       2       6
      / \     / \
     1   3   5   7
    / \ / \ / \ / \
.  ..  ..  ..   .
```

```haskell
>>> postorder (fromList [1,2,3,4,5,6,7])
[1,3,2,5,7,6,4]
```

### levels :: forall a. Ord a => BTree a -> [[a]] #

Return the BTree as a list of lists, grouped by hights.

```
  a
 / \    => [[a], [b,c]]
b   c
```

```haskell
>>> levels (fromList [1,2,3,4,5,6,7])
[[4],[2,6],[1,3,5,7]]
```

## Ascii Drawing

### draw :: forall a. Show a => BTree a -> String #

Draws the BTree.

The output is a string of the form:

- Each Leaf is represented by a ' . '
- Each Node is represented as (left, value, right) where left and right are the left and right subtrees.

So, the current list `[1, 2, 3, 4, 5, 6, 7]` will be represented as:

`"(((. 1 .) 2 (. 3 .)) 4 ((. 5 .) 6 (. 7 .)))"`

Which is an inline representation of the BTree.

```haskell
>>> draw (fromList [1,2,3,4,5,6,7])
"((((. 1 .) 2 (. 3 .)) 4 ((. 5 .) 6 (. 7 .))))"
```
