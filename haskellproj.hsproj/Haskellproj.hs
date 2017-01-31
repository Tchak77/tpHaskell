data Op = Add | Sub | Mul | Div
data Expr = Val Int | App Op Expr Expr 

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  
instance Show Expr where
  show (Val x) = show x
  show (App op x y) = "(" ++ show x ++ show op ++ show y ++ ")"
  
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True -- always true
valid Sub x y = x > y -- do not allow for negative integers 
valid Mul _ _ = True -- always valid
valid Div x y = mod x y == 0 -- integer division only

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> Int
eval (Val n) = n 
eval (App Add x y) = eval x + eval y
eval (App Sub x y) = eval x - eval y
eval (App Mul x y) = eval x * eval y
eval (App Div x y) = quot (eval x) (eval y) -- Should not be used with mod a b != 0

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

makeExpr :: Expr -> Op -> Int -> Expr
makeExpr exp op n = App op exp (Val n) 

-- Just looking for this array with all values used
solutionSingleArray :: [Int] -> Int -> Maybe Expr
solutionSingleArray x n = Nothing --TODO

-- Looking for all the subsets arrays with all values used
solutionSubsets :: [[Int]] -> Int -> [Maybe Expr]
solutionSubsets [] n = [Nothing]
solutionSubsets [[]] n = [Nothing]
solutionSubsets (x:xs) n = solutionSingleArray x n : solutionSubsets xs n -- We add the first array answer then recursivly we add the others

-- Checking for the first Expr validating, else Nothing
checkForExpr :: [Maybe Expr] -> Maybe Expr
checkForExpr [] = Nothing
checkForExpr (x:xs) = case x of 
                      Nothing -> checkForExpr xs
                      Just value -> Just value

-- The end-to-end function
solution :: [Int] -> Int -> Maybe Expr
solution [] n = Nothing
