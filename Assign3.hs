{-
CPSC 449 Assignment 3
By: Evan Wheeler
ID# 30046173
-}

--Question 1
data Season = Fall | Winter | Summer | Spring
  deriving Show

data Month = January | February | March | April | May | June | July | August | September | October | November | December
  deriving Show

months :: Season -> (Month, Month, Month)
months Fall   = (October, November, December)
months Winter = (January, February, March)
months Spring = (April, May, June)
months Summer = (July, August, September)


--Question 2
data Form = And Form Form | Or Form Form | Not Form | Val Bool

eval :: Form -> Bool
eval (Val True)  = True
eval (Val False) = False
eval (And a b)   = (eval a) && (eval b)
eval (Or  a b)   = (eval a) || (eval b)
eval (Not x)     = not (eval x)


--Question 3
data NTree = Leaf Int | Node NTree Int NTree

collapse :: NTree -> [Int]
collapse (Leaf x)            = [x]
collapse (Node left x right) = collapse left ++ [x] ++ collapse right


--Question 4
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
 deriving Show

countLeaves :: PTree a -> Integer
countLeaves PLeaf                = 1
countLeaves (PNode x left right) = countLeaves left + countLeaves right


--Question 5
data Store = Empty | Join Int Store Store

maxStore :: Store -> Int
maxStore Empty               = 0
maxStore (Join x left right) = max (max x (maxStore left)) (max x (maxStore right))


--Question 6
data Expr = Num Integer | BinOp Op Expr Expr
  deriving (Eq,Show)

data Op = Add | Mul
  deriving (Eq,Show)

countOp :: Op -> Expr -> Int
countOp x (Num y) = 0
countOp x (BinOp y left right)
        | x == y    = 1 + (countOp x left) + (countOp x right)
        | otherwise = (countOp x left) + (countOp x right)


--Question 7
data Tree a = Nil | Value a (Tree a) (Tree a)
 deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Value x left right) = Value (f x) (mapTree f left) (mapTree f right)

countChars :: String -> Integer
countChars []     = 0
countChars (x:xs) = toInteger(length (x:xs))


--Question 8
foldTree :: (a -> a -> a) -> a -> Tree a -> a
foldTree _ x Nil = x
foldTree f x (Value n left right) = foldTree f (f n (foldTree f x right)) left


--Question 9
data Road = City String | Fork Road Road
  deriving (Show)

reachable :: String -> Road -> Bool
reachable destination (City x)
          | x == destination = True
          | otherwise        = False
reachable destination (Fork left right) = reachable destination left || reachable destination right


--Question 10
data LR = L | R

--insertRoad (new,lr) city old
insertRoad :: (Road,LR) -> String -> Road -> Road
insertRoad (newRoad, L) goalNode (City currentNode)
           | goalNode == currentNode = Fork newRoad (City currentNode)
           | otherwise               = (City currentNode)
insertRoad (newRoad, R) goalNode (City currentNode)
           | goalNode == currentNode = Fork (City currentNode) newRoad
           | otherwise               = (City currentNode)
insertRoad (newRoad, lr) goalNode (Fork left right) = Fork (insertRoad (newRoad, lr) goalNode left) (insertRoad (newRoad, lr) goalNode right)


--Additional Challenge
data Instruction = FORW Int | BACKW Int | LEFT | RIGHT

data Direction = N | S | E | W

{-
Takes in a set of instructions and returns where the car will end up
assuming that the car starts at (0,0) and is facing North.
-}
destination :: [Instruction] -> (Int,Int)
destination instructions = dest instructions N (0,0)

{-
A fuction that takes in a list of instructions, a starting direction, and a starting position
and returns the final position of the car.
-}
dest :: [Instruction] -> Direction -> (Int,Int) -> (Int, Int)
dest [] _ (currentX,currentY) = (currentX, currentY)
dest (x: xs) currentDirection (currentX, currentY) = dest xs newDirection (newX, newY)
     where (newDirection, newX, newY) = newPosition currentDirection x (currentX, currentY)

{-
A function that is given a current direction, a single instruction, and a current position
and returns a tuple with the new direction, x, and y of the car after the instruction.

Note: Could have used a guard for more readable code but pattern matching still works
-}
newPosition :: Direction -> Instruction -> (Int, Int) -> (Direction, Int, Int)
--North
newPosition N (FORW n) (x,y)  = (N, x, y + n)
newPosition N (BACKW n) (x,y) = (N, x, y - n)
newPosition N LEFT (x,y)      = (W, x, y)
newPosition N RIGHT (x,y)     = (E, x, y)
--East
newPosition E (FORW n) (x,y)  = (E, x + n, y)
newPosition E (BACKW n) (x,y) = (E, x - n, y)
newPosition E LEFT (x,y)      = (N, x, y)
newPosition E RIGHT (x,y)     = (S, x, y)
--South
newPosition S (FORW n) (x,y)  = (S, x, y - n)
newPosition S (BACKW n) (x,y) = (S, x, y + n)
newPosition S LEFT (x,y)      = (E, x, y)
newPosition S RIGHT (x,y)     = (W, x, y)
--West
newPosition W (FORW n) (x,y)  = (N, x - n, y)
newPosition W (BACKW n) (x,y) = (N, x + n, y)
newPosition W LEFT (x,y)      = (S, x, y)
newPosition W RIGHT (x,y)     = (N, x, y)



