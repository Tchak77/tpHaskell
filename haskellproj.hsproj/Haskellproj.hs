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

-- Create new expressions with all op (does not care about valid or not)
nextExpr :: Expr -> Expr -> [Expr]
nextExpr a b = [App o a b | o <- allOps]

-- Just looking for this array with all values used without permutations
exprFromArray :: [Int] -> [Expr]
exprFromArray [] = []
exprFromArray [x] = [(Val x)]
exprFromArray (x:xs) = [ exp | n <- exprFromArray xs, exp <- nextExpr (Val x) n] 

-- Create all the possible exp from an array of int (with subsets and permutations)
constructAllExprFromArray :: [Int] -> [Expr]
constructAllExprFromArray [] = []
constructAllExprFromArray x = concatMap exprFromArray (permute (subsets x)) -- concatenation of all the expressions possible for every permutation over every subset.

-- Tells if an expression is valid recursivly
isValidExpr :: Expr -> Bool
isValidExpr (Val n) = True
isValidExpr (App op a b) = isValidExpr a && isValidExpr b && valid op (eval a) (eval b) -- If the 2 Expressions are valid, we can test the validity on this one

-- Filter on the only valid expressions
filterValid = filter isValidExpr

-- Returns the first Expression equals to the int given, or Nothing
getFirstExprEvaluated :: [Expr] -> Int -> Maybe Expr
getFirstExprEvaluated [] _ = Nothing
getFirstExprEvaluated (x:xs) n 
                           | eval x == n = Just x -- Here x MUST be evaluable (with filter) !
                           | otherwise = getFirstExprEvaluated xs n -- Else continue

-- The end-to-end function
solution :: [Int] -> Int -> Maybe Expr
solution [] n = Nothing
solution x n = getFirstExprEvaluated (filterValid (constructAllExprFromArray x)) n -- Gets the first expression evaluated with the int given on all the expressions validated over all the expressions possible












