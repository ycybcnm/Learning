------------------------------------------------------
---- 	student1: Chenyun Yu 933-652-132			--
----	student2: Jiechen Tan 933-505-491			--
----	student3: Xinwei Li	933-666-663				--
----	student4: Jiang Xu	933-309-688				--
----	student5: Yuanhao Zuo 933-643-293			--
------------------------------------------------------

myexample = [LD 3,DUP,ADD,DUP,MULT]::Prog
mybadexample = [LD 3,ADD,DUP,MULT]::Prog
mystack =[3,4] :: Stack

myexample2 = [ADD,LD 3,DUP,ADD,DUP,MULT,LD 3,LD 38, DEF "cxk" [ADD,ADD], CALL "cxk",DEF "nmsl" [DUP,DUP,ADD], CALL "nmsl"]::Prog
mybadexample2 = [ADD,LD 3,DUP,ADD,DUP,MULT,LD 3,LD 38, DEF "cxk" [ADD,ADD], CALL "cxk",DEF "nmsl" [DUP,DUP,ADD,ADD,ADD,ADD], CALL "nmsl"]::Prog
mymacros = []
myd = (mystack, mymacros)


-- E1 and some of E2
type Prog = [Cmd]

data Cmd = LD Int
		 | ADD
		 | MULT
		 | DUP
		 | DEF String Prog
		 | CALL String
		 deriving Show

type Stack = [Int]

-- I don't use Maybe type, because Maybe just can print a "Nothing" when something wrong.
type D = Stack -> Stack

sem :: Prog -> D
sem [] 		 st = st
sem (op:ops) st = sem ops (semCmd op st)

semCmd :: Cmd -> D
semCmd (LD x) st = x:st
semCmd ADD 	  st = if (length st) >= 2 
				   then ((st!!0) + (st!!1)) :(drop 2 st)
				   else error("STACK TOO SHORT: Need at least two values")

semCmd MULT   st = if (length st) >= 2 
				   then ((st!!0) * (st!!1)) :(drop 2 st)
				   else error("STACK TOO SHORT: Need at least two values")

semCmd DUP    st = if (length st) >= 1 
				   then (st!!0):st 
				   else error("STACK TOO SHORT: Need at least one value")

-- E2 
-- a)
{- Type Cmd has been defined in the first question, so I just use comment on here.
type Prog = [Cmd]

data Cmd = LD Int
		 | ADD
		 | MULT
		 | DUP
		 | DEF String Prog
		 | CALL String
		 deriving Show

type Stack = [Int]
-}

-- b)
-- I don't use Maybe type, because Maybe just can print a "Nothing" when something wrong.
type Macros = [(String,Prog)]
type DNew = (Stack, Macros) -> (Stack, Macros)

-- c) 
sem2 :: Prog -> DNew
sem2 [] 	  sam = sam
sem2 (op:ops) sam = sem2 ops (semCmd2 op sam)

findM :: String -> Macros -> (String, Prog)
findM str []     = error("CAN NOT FIND " ++ str ++ " IN MACROS LIST")
findM str (m:ms) =  if str == (fst m) 
					then m 
					else (findM str ms) 


semCmd2 :: Cmd -> DNew
semCmd2 (LD x) 		 (st, ms) = (x:st, ms)
semCmd2 ADD 		 (st, ms) = if (length st) >= 2 
								then (((st!!0) + (st!!1)):(drop 2 st), ms)
								else error("STACK TOO SHORT: Need at least two values")

semCmd2 MULT		 (st, ms) = if (length st) >= 2 
								then (((st!!0) * (st!!1)):(drop 2 st), ms)
								else error("STACK TOO SHORT: Need at least two values")

semCmd2 DUP 		 (st, ms) = if (length st) >= 1 
								then ((st!!0):st, ms)
								else error("STACK TOO SHORT: Need at least one value")
semCmd2 (DEF str pg) (st, ms) = (st, ((str, pg):ms))
semCmd2 (CALL str)	 (st, ms) = sem2 (snd (findM str ms)) (st, ms)


-- E3 
data CmdML = Pen Mode
		   | MoveTo Int Int
		   | Seq CmdML CmdML 
		   deriving Show

data Mode = Up 
		  | Down 
		  deriving (Eq, Show)
		  
type StateML = (Mode, Int, Int)

type Line = (Int, Int, Int, Int)
type Lines = [Line]

semS :: CmdML -> StateML -> (StateML,Lines)
semS (Pen m) 	   (_, i1, i2)			   = ((m, i1, i2), [])
semS (MoveTo p1 p2)(m, i1, i2) | m == Down = ((m, p1, p2),[(i1,i2,p1,p2)])
							   | m == Up   = ((m, p1, p2),[])
semS (Seq c1 c2)   s1					   =  semS c2 (fst(semS c1 s1))

sem' :: CmdML -> Lines
sem' c = snd (semS c (Up,0,0))
