module RedBlack where

data Color = Red | Black deriving (Eq, Show)

data RedBlackTree a = Empty
                    | Node { value :: a
                           , color :: Color
                           , left  :: RedBlackTree a
                           , right :: RedBlackTree a
                           } deriving (Eq, Show)

data Direction = L | R deriving (Eq, Show)

data RedBlackPath a = Path Direction a Color (RedBlackTree a)
                    deriving (Show)

type RedBlackZipper a = (RedBlackTree a, [RedBlackPath a])

singleton :: (Ord a) => a -> RedBlackTree a
singleton x = Node x Black Empty Empty

insert :: (Ord a) => a -> RedBlackTree a -> RedBlackTree a
insert x tree = fromZipper $ insert' x (tree, [])
  where insert' :: (Ord a) => a -> RedBlackZipper a -> RedBlackZipper a
        insert' x (Empty, []) = (singleton x, [])
        insert' x (Empty, path) = balance (Node x Red Empty Empty, path)
        insert' x (n@(Node v c l r), path)
          | x < v  = insert' x (l, (Path L v c r):path)
          | x == v = (n, path)
          | x > v  = insert' x (r, (Path R v c l):path)

-- balance a tree using algorithm adapted from:
-- http://en.wikipedia.org/wiki/Red_black_tree
balance :: (Ord a) => RedBlackZipper a -> RedBlackZipper a
-- Case 1: focus node is the root of the tree. Paint it black.
balance (Node v _ l r, []) = (Node v Black l r, [])
-- Case 2: focus node's parent is black. Tree is valid.
balance z@(n, (Path _ _ Black _):_) = z
-- Case 3: parent and uncle are red. Paint them both black and paint 
-- grandparent red. Then move focus to the grandparent and balance.
balance (n, (Path L pv Red po):(Path L gv gc (Node uv Red ul ur)):path) =
    balance (Node gv Red (Node pv Black n po) (Node uv Black ul ur), path)
balance (n, (Path L pv Red po):(Path R gv gc (Node uv Red ul ur)):path) =
    balance (Node gv Red (Node uv Black ul ur) (Node pv Black n po), path)
balance (n, (Path R pv Red po):(Path L gv gc (Node uv Red ul ur)):path) =
    balance (Node gv Red (Node pv Black po n) (Node uv Black ul ur), path)
balance (n, (Path R pv Red po):(Path R gv gc (Node uv Red ul ur)):path) =
    balance (Node gv Red (Node uv Black ul ur) (Node pv Black po n), path)
-- Case 4: parent is red, but uncle is black. Path to focus node is either L,R
-- or R,L
balance (Node nv nc nl nr, (Path R pv Red pl):(Path L gv gc u):path) =
  balance (Node pv Red pl nl, (Path L nv Red nr):(Path L gv gc u):path)
balance (Node nv nc nl nr, (Path L pv Red pr):(Path R gv gc u):path) =
  balance (Node pv Red nr pr, (Path R nv Red nl):(Path R gv gc u):path)
-- Case 5: parent is red, but uncle is black. Path to focus node is either L,L
-- or R,R
balance (Node nv nc nl nr, (Path L pv Red pr):(Path L gv gc u):path) =
  (Node nv nc nl nr, (Path L pv Black (Node gv Red pr u)):path)
balance (Node nv nc nl nr, (Path R pv Red pl):(Path R gv gc u):path) =
  (Node nv nc nl nr, (Path R pv Black (Node gv Red u pl)):path)
       
fromZipper :: RedBlackZipper a -> RedBlackTree a
fromZipper (n, []) = n
fromZipper (n, (Path L pv pc pr):path) = fromZipper (Node pv pc n pr, path)
fromZipper (n, (Path R pv pc pl):path) = fromZipper (Node pv pc pl n, path)

fromList :: (Ord a) => [a] -> RedBlackTree a
fromList = foldr insert Empty

depth :: RedBlackTree a -> Int
depth Empty = 0
depth (Node _ _ l r) = 1 + max (depth l) (depth r)

test :: (Ord a) => a -> RedBlackTree a -> Int
test _ Empty = 1
test x (Node v _ l r)
  | x < v  = 1 + test x l
  | x == v = 1
  | x > v  = 1 + test x r
