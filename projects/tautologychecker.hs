-- p1= A ∧ ¬A
-- p2= (A ∧ B) ⇒ A
-- p3= A ⇒ (A ∧ B)
-- p4= (A ∧ (A ⇒ B)) ⇒ B

data Prop = Const Bool | Var Char | Not Prop | And Prop Prop | Imply Prop Prop deriving (Show)

a = Var 'A'
p1 = And a (Not a)

p2 = Imply (And (Var 'A') (Var 'B')) (Var 'B')

p3 = Imply (Var 'B') (And (Var 'A') (Var 'B'))

p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Assoc k v = [(k, v)]

-- Type Substfor representing Assoc list as list of tuples (Char,Bool)
type Subst = Assoc Char Bool


find :: Eq k => k -> Assoc k v -> v
find key obj = head [v | (k,v) <- obj, k == key]

eval :: Subst -> Prop -> Bool
eval keyd (Const b) = b
eval keyd (Var c) = find c keyd
eval keyd (Not p) = not (eval keyd p)
eval keyd (And pa pb) = (eval keyd pa) && (eval keyd pb)
eval keyd (Imply pa pb) = (eval keyd pa) <= (eval keyd pb)


rmdupsh :: Eq k => [k] -> [k] -> [k]
rmdupsh [] [] = []
rmdupsh inp [] = inp
rmdupsh setl (x:xs)
    | not (elem x setl) = rmdupsh (setl ++ [x]) xs
    | otherwise = rmdupsh setl xs

rmdups :: Eq k => [k] -> [k] 
rmdups = rmdupsh []

bools :: Int -> [[Bool]]
bools 1 = [[True], [False]]
bools n = (map (\x -> x ++ [True]) oa) ++ (map (\x -> x ++ [False]) oa)  where oa = bools (n - 1)


vars :: Prop -> [Char]
vars (Const b) = []
vars (Var ch) = [ch]
vars (Not ch) = vars ch
vars (And pa pb) = (vars pa) ++ (vars pb)
vars (Imply pa pb) = (vars pa) ++ (vars pb)



tutatologyCheck :: Prop -> Bool
tutatologyCheck prop = foldl (&&) True (map (\x -> eval x prop) [zip (rmdups (vars prop)) x | x <- bools (length (rmdups (vars prop)))])

