import System

data Tree = Leaf | Node Int Tree Tree

minN : Int
minN = 4

io : String -> Int -> Int -> String
io s n t = s ++ " of depth " ++ show n ++ "\t check: " ++ show t ++ "\n"

-- traverse the tree, counting up the nodes
check' : Bool -> Int -> Tree -> Int
check' b z Leaf         = z
check' b z (Node i l r) = check' (not b) (check' b (if b then z+i else z-i) l) r

check : Tree -> Int
check = check' True 0

-- build a tree
make : Int -> Int -> Tree
make i 0 = Node i Leaf Leaf
make i d = Node i (make (i2-1) d2) (make i2 d2)
  where i2 = 2*i
        d2 = d-1

-- allocate and check lots of trees
sumT : Int -> Int -> Int -> Int  
sumT d 0 t = t
sumT d i t = sumT d (i-1) ans
  where a = check (make i    d)
        b = check (make (-i) d)
        ans = a + b + t

-- generate many trees
depth : Int -> Int -> List (Int,Int,Int)
depth d m = if (d <= m)
              then let s = sumT d n 0
                       rest = depth (d+2) m
                   in ((2*n,d,s) :: rest)
              else []
  where n = m - d + minN -- bit (m - d + minN)

main : IO ()
main = do
    --n <- getArgs >>= readIO . head
    [_,a] <- getArgs
    let n = the Int (cast a)
    let maxN     = max (minN + 2) n
    let stretchN = maxN + 1
    -- stretch memory tree
    let c = check (make 0 stretchN)
    putStrLn $ io "stretch tree" stretchN c

    -- allocate a long lived tree
    let long = make 0 maxN

    -- allocate, walk, and deallocate many bottom-up binary trees
    let vs = depth minN maxN
    traverse (\((m,d,i)) => putStrLn $ io (show m ++ "\t trees") d i) vs

    -- confirm the the long-lived binary tree still exists
    putStrLn $ io "long lived tree" maxN (check long)
    
    return ()

{-
data Tree = Nil | Node !Int !Tree !Tree

minN = 4

io s n t = printf "%s of depth %d\t check: %d\n" s n t

main = do
    n <- getArgs >>= readIO . head
    let maxN     = max (minN + 2) n
        stretchN = maxN + 1
    -- stretch memory tree
    let c = {-# SCC "stretch" #-} check (make 0 stretchN)
    io "stretch tree" stretchN c

    -- allocate a long lived tree
    let !long    = make 0 maxN

    -- allocate, walk, and deallocate many bottom-up binary trees
    let vs = depth minN maxN
    mapM_ (\((m,d,i)) -> io (show m ++ "\t trees") d i) vs

    -- confirm the the long-lived binary tree still exists
    io "long lived tree" maxN (check long)

-- generate many trees
depth :: Int -> Int -> [(Int,Int,Int)]
depth d m
    | d <= m    = let 
      s = sumT d n 0
      rest = depth (d+2) m
      in s `par` ((2*n,d,s) : rest)
    | otherwise = []
  where n = bit (m - d + minN)

-- allocate and check lots of trees
sumT :: Int -> Int -> Int -> Int  
sumT d 0 t = t
sumT  d i t = a `par` b `par` sumT d (i-1) ans
  where a = check (make i    d)
        b = check (make (-i) d)
        ans = a + b + t

check = check' True 0

-- traverse the tree, counting up the nodes
check' :: Bool -> Int -> Tree -> Int
check' !b !z Nil          = z
check' b z (Node i l r)   = check' (not b) (check' b (if b then z+i else z-i) l) r

-- build a tree
make :: Int -> Int -> Tree
make i 0 = Node i Nil Nil
make i d = Node i (make (i2-1) d2) (make i2 d2)
  where i2 = 2*i; d2 = d-1

-}


