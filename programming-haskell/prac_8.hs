
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [ v | (k', v) <- t, k == k' ]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) =  rmdups (filter (/= x) xs)

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

type Subst = Assoc Char Bool 

eval :: Subst -> Prop -> Bool
eval _ (Const p)      = p
eval s (Var p)        = find p s
eval s (Not p)        = not (eval s p)
eval s (And p1 p2)    = eval s p1 && eval s p2
eval s (Imply p1 p2)  = eval s p1 <= eval s p2

vars :: Prop -> [Char]
vars (Const p)        = []
vars (Var p)          = [p]
vars (Not p)          = vars p
vars (And p1 p2)      = vars p1 ++ vars p2
vars (Imply p1 p2)    = vars p1 ++ vars p2

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)

substs :: Prop -> [Subst]
substs p = map (zip n) (bools ( length n))
  where n = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval x p | x <- substs p] 


data Expr = Val Int | Add Expr Expr
value :: Expr -> Int
value (Val n)  = n
value (Add x y) = value x + value y

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval2 :: Expr -> Cont -> Int 
eval2 (Val n) c = exec c n 
eval2 (Add x y) c = eval2 x (EVAL y : c) 

exec :: Cont -> Int -> Int 
exec [] n = n 
exec (EVAL y : c) n = eval2 y (ADD n : c) 
exec (ADD n : c) m = exec c (n+m) 

value2 :: Expr -> Int 
value2 e = eval2 e [] 


data Nat = Zero | Succ Nat deriving Show


nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = (Succ (int2nat (n-1)))   

add :: Nat -> Nat -> Nat
add n Zero = n
add n (Succ x) = add (Succ n) x 


mult :: Nat -> Nat -> Nat
mult _ Zero = Zero
mult Zero _ = Zero
mult m (Succ Zero) = m
mult m n = 


