-- IMPORTED FROM DATA.LIST SOURCES
-- | The 'permutations' function returns the list of all permutations of the argument.
-- > permutations "abc" == ["abc","bac","cba","bca","cab","acb"]
permutations :: [a] -> [[a]]
permutations xs0 =  xs0 : perms xs0 []
  where
    perms [] _ = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave xs r = let (_,zs) = interleave' id xs r in zs
            interleave' _ [] r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)
-----------------------------------------------

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

--------

eval :: Expr -> Int
eval (Val n) = n 
eval (App Add x y) = eval x + eval y
eval (App Sub x y) = eval x - eval y
eval (App Mul x y) = eval x * eval y
eval (App Div x y) = quot (eval x) (eval y) -- Should not be used with mod a b != 0

-- Returns all the subsets of the array given
subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- Returns the permutations of all subsets given
permute :: [[Int]] -> [[Int]]
permute = concatMap permutations

-- Create an array with all ops
allOps :: [Op]
allOps = [Add,Sub,Mul,Div]

-- Create new expr
nextExpr :: Expr -> Expr -> [Expr]
nextExpr a b = [App o a b | o <- allOps]

-- Just looking for this array with all values used without permutations
solutionSingleArray :: [Int] -> [Expr]
solutionSingleArray [] = []
solutionSingleArray [x] = [(Val x)]
solutionSingleArray (x:xs) = [ exp | n <- solutionSingleArray xs, exp <- nextExpr (Val x) n] 

-- Looking for all the subsets arrays with all values used
-- For each array, calculate the Maybe Expr
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










