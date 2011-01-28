--CSCI 555, Functional Programming, Fall 2010
--Assignment 4

--J.T. Lomenick
--2 November 2010

-------------------------------------------------------------------
-- Exercise #6 in notes pg 89

--data type for natural numbers
data Nat = Zero | Succ Nat
		deriving Show

--intToNat takes a nonnegative Int and returns the equivalent Nat
intToNat :: Int   -> Nat 
intToNat 0 = Zero
intToNat x
	| x < 0 = error ("Invalid Natural Number")
	|otherwise =  Succ (intToNat (x-1))

--natToInt takes a Nat and returns the equivalent value of type Int
natToInt :: Nat  -> Int
natToInt Zero = 0
natToInt (Succ x) = 1 + natToInt x

--addNat takes two Nats and returns their product as a Nat
addNat :: Nat -> Nat -> Nat
addNat Zero x = x
addNat x Zero = x
addNat (Succ x) (Succ y) = addNat x (Succ (Succ y)) 

--multNat takes two Nates and returns their product as a Nat
multNat :: Nat -> Nat -> Nat
multNat _ Zero = Zero
multNat Zero _ = Zero
multNat x (Succ Zero) = x
multNat (Succ Zero) y = y
multNat (Succ x) (Succ y) = addNat (Succ y) (multNat x (Succ y))

--compNat takes two Nats and returns the value -1 if the first is less
--than the second, 0 if they are equals, and 1 if the first is greater 
--than the second
compNat :: Nat -> Nat -> Int
compNat Zero Zero = 0
compNat Zero _ = -1
compNat _ Zero = 1
compNat (Succ x)(Succ y) = compNat x y 
 
-----------------------------------------------------------------------------
--Exercise 14.27 pg 256-257, Thompson textbook
data Expr = Lit Int | Op Ops Expr Expr | If BExpr Expr Expr
		deriving Show
data Ops = Add | Sub | Mul | Div | Mod
		deriving Show
data BExpr = BoolLit Bool | And BExpr BExpr | Not BExpr | Equal Expr Expr | 
					Greater Expr Expr
		deriving Show
--eval takes an expression and evaluates the result
eval :: Expr -> Int
eval (Lit n )		= n
eval (Op Add e1 e2)  = (eval e1) + (eval e2)
eval (Op Sub e1 e2)	= (eval e1) - (eval e2)
eval (Op Mul e1 e2)  = (eval e1) * (eval e2)
eval (Op Div e1 e2)
	|(eval e2)== 0 = error "ERROR: Divide by 0"
	|otherwise =  (eval e1) `div` (eval e2)
eval (Op Mod e1 e2)  = (eval e1) `mod` (eval e2)
eval (If bexpr e1 e2)
	|bEval(bexpr) == True  = eval e1
	|bEval(bexpr) == False = eval e2

--bEval takes a boolean expression and evaluates the result
bEval :: BExpr -> Bool
bEval (BoolLit b)       = b
bEval (And be1 be2)     = (bEval be1) && (bEval be2)
bEval (Not be1)         = not(bEval be1)
bEval (Equal e1 e2)   = (eval e1) == (eval e2)
bEval (Greater e1 e2) = (eval e1) > (eval e2) 			

---SIZE------------------------------------------------------------- 
--size counts the number of operators in an expression
size :: Expr -> Int
size (Op _ e1 e2) = 1 + (size e1) + (size e2)
size (Lit n) = 0
size (If bexpr e1 e2) = (sizeb bexpr) + (size e1) + (size e2)
		where
			sizeb (Greater e1 e2) = (size e1) + (size e2)
			sizeb (Equal e1 e2)   = (size e1) + (size e2)
			sizeb (And be1 be2)   = (sizeb be1) + (sizeb be2)
			sizeb (Not be1)       = (sizeb be1)
			sizeb (BoolLit b)     = 0
------------------------------------------------------------------------
--TESTING


--Test natToInt
test_natToInt =
	do
		print "TESTING natToInt ....."
		print "Should print 0 - 5"
		print (natToInt (Zero))
		print (natToInt (Succ Zero))
		print (natToInt (Succ(Succ Zero)))
		print (natToInt (Succ(Succ(Succ Zero))))
		print (natToInt (Succ(Succ(Succ(Succ Zero)))))
		print (natToInt (Succ(Succ(Succ(Succ(Succ Zero))))))
		putStrLn "\n" 

test_intToNat =
	do
		print "TESTING intToNat ....."
		print "Should print 0-5 in Nat"
		print (intToNat 0)
		print (intToNat 1)
		print (intToNat 2)
		print (intToNat 3)
		print (intToNat 4)
		print (intToNat 5)
		putStrLn "\n"
--test addNat
test_addNat =
	do
		print "TESTING addNat ......"
		print "0+0"
		print (addNat (intToNat 0)(intToNat 0))
		print "0+1"
		print (addNat (intToNat 0)(intToNat 1))
		print "3+0"
		print (addNat (intToNat 3)(intToNat 0))
		print "3+2"
		print (addNat (intToNat 3)(intToNat 2))
		print "2+4"
		print (addNat (intToNat 2)(intToNat 4))
		putStrLn "\n"

--test multNat
test_multNat =
	do
		print "Testing multNat ....."
		print "1*0"
		print (multNat (intToNat 1)(intToNat 0))
		print "0*3"
		print (multNat (intToNat 0)(intToNat 3))
		print "3*1"
		print (multNat (intToNat 3)(intToNat 1))
		print "1*2"
		print (multNat (intToNat 1)(intToNat 2))
		print "3*2"
		print (multNat (intToNat 3)(intToNat 2))
		print "2*2"
		print (multNat (intToNat 2)(intToNat 2))
		print "2*4"
		print (multNat (intToNat 2)(intToNat 4))
		putStrLn "\n"

--test compNat
test_compNat =
	do
		print "Testing compNat ....."
		print "comp 0 0"
		print (compNat (intToNat 0)(intToNat 0))
		print "comp 1 2"
		print (compNat (intToNat 1)(intToNat 2))
		print "comp 4 2"
		print(compNat (intToNat 4)(intToNat 2))
		print "comp 3 3"
		print (compNat (intToNat 3)(intToNat 3))
		putStrLn "\n"

--test eval
test_eval =
	do
		print "Testing eval ....."
		print "eval (Lit 2)"
		print (eval (Lit 2))
		print "eval (Op Add (Lit 2)(Lit 3))"
		print (eval (Op Add (Lit 2)(Lit 3)))
		print "eval (Op Div (Lit 3)(Lit 0))"
		--print (eval (Op Div (Lit 3)(Lit 0)))
		print "eval (Op Div (Lit 6)(Lit 2))"
		print (eval (Op Div (Lit 6)(Lit 2)))
		print "eval (Op Div (Lit 5)(Lit 2))"
		print (eval (Op Div (Lit 5)(Lit 2)))
		print "eval (If (BoolLit True) (Op Mul (Lit 3)(Lit 2)) (Op Sub (Lit 3)(Lit 2)))"
		print (eval (If (BoolLit True) (Op Mul (Lit 3)(Lit 2)) (Op Sub (Lit 3)(Lit 2))))
		print "eval (If (Not(BoolLit True)) (Op Mul (Lit 3)(Lit 2)) (Op Sub (Lit 3)(Lit 2)))"
		print (eval (If (Not(BoolLit True)) (Op Mul (Lit 3)(Lit 2)) (Op Sub (Lit 3)(Lit 2))))
		print "eval (If ((And (BoolLit True)(BoolLit True))) (Op Mul (Lit 3)(Lit 2)) (Op Sub (Lit 3)(Lit 2)))"
		print (eval (If (And (BoolLit True)(BoolLit True)) (Op Mul (Lit 3)(Lit 2)) (Op Sub (Lit 3)(Lit 2))))
		print "eval (If (( Equal (Op Add (Lit 2)(Lit 2)) (Op Sub (Lit 6)(Lit 2)))) (Op Div (Op Add (Lit 3)(Lit 1))(Lit 2)) (Op Sub (Lit 6)(Lit 1))"
		print (eval (If (( Equal (Op Add (Lit 2)(Lit 2)) (Op Sub (Lit 6)(Lit 2)))) (Op Div (Op Add (Lit 3)(Lit 1))(Lit 2)) (Op Sub (Lit 6)(Lit 1))))
		print "eval (If (( Equal (Op Add (Lit 1)(Lit 2)) (Op Sub (Lit 6)(Lit 2)))) (Op Div (Op Add (Lit 3)(Lit 1))(Lit 2)) (Op Sub (Lit 6)(Lit 1))"
		print (eval (If (( Equal (Op Add (Lit 1)(Lit 2)) (Op Sub (Lit 6)(Lit 2)))) (Op Div (Op Add (Lit 3)(Lit 1))(Lit 2)) (Op Sub (Lit 6)(Lit 1))))
		print "eval (If (( Greater (Op Add (Lit 6)(Lit 2)) (Op Sub (Lit 6)(Lit 2)))) (Op Div (Op Add (Lit 3)(Lit 1))(Lit 2)) (Op Sub (Lit 6)(Lit 1))"
		print (eval (If (( Greater (Op Add (Lit 6)(Lit 2)) (Op Sub (Lit 6)(Lit 2))))(Op Div (Op Add (Lit 3)(Lit 1))(Lit 2))(Op Sub (Lit 6)(Lit 1))))
		print "eval (If (( Greater (Op Add (Lit 1)(Lit 2)) (Op Sub (Lit 6)(Lit 2)))) (Op Div (Op Add (Lit 3)(Lit 1))(Lit 2)) (Op Sub (Lit 6)(Lit 1))"
		print(eval (If (( Greater (Op Add (Lit 1)(Lit 2))(Op Sub (Lit 6)(Lit 2)))) (Op Div (Op Add (Lit 3)(Lit 1))(Lit 2)) (Op Sub (Lit 6)(Lit 1))))
		putStrLn "\n"

--test size
test_size =
	do
		print "TESTING size"
		print "size (Lit 1)"
		print (size (Lit 1))
		print "size (Op Add (Lit 1)(Lit 2))"
		print(size (Op Add(Lit 1)(Lit 2)))
		print "size (Op(Add (Op Sub (Lit 6)(Lit 2)) (Lit 3 )"
		print(size (Op Add (Op Sub (Lit 6)(Lit 2)) (Lit 3)))
		print "size (If (( Greater (Op Add (Lit 1)(Lit 2)) (Op Sub (Lit 6)(Lit 2)))) (Op Div (Op Add (Lit 3)(Lit 1))(Lit 2)) (Op Sub (Lit 6)(Lit 1))"
		print(size (If (( Greater (Op Add (Lit 1)(Lit 2)) (Op Sub (Lit 6)(Lit 2)))) (Op Div (Op Add (Lit 3)(Lit 1))(Lit 2)) (Op Sub (Lit 6)(Lit 1))))
		putStrLn "\n"
--test all
test_all =
	do
		test_intToNat
		test_natToInt
		test_addNat
		test_multNat
		test_compNat
		test_eval
		test_size









	
