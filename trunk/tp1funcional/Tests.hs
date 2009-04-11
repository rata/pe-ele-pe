
import Dict
import Lenguaje
import Parser
import ByValue
import ByName
import Test.HUnit

def :: String -> String -> (FuncId, FuncDef)
def proto body = d (parser proto) (parser body)
  where d (Call f args) body = (f, FuncDef (map unVar args) body)
        unVar (Var x) = x

definiciones :: ProgramDef
definiciones = makeDict [

	def "fib(x)"
        	" ifzero x then             \
	        \   1                       \
	        \ else ifzero x-1 then      \
	        \   1                       \
	        \ else                      \
	        \   fib(x - 1) + fib(x - 2) ",

	def "fact(x)"
	        " ifzero x then     \
	        \   1               \
	        \ else              \
        	\   x * fact(x - 1) ",

	def "colgate(x)" 
		"4 + colgate(x)",

	def "primero(x,y)"
		"x",

	def "suma(x,y)"
		"x + y",

	def "leticia(x,y,z)"
		"let x=4 in x + suma(x,z) * y",

	def "sellama(x)"
		" ifzero x  	\
		\ then 3 	\
		\ else 		\
		\ 2",
	
	def "varLibre(y)"
		"y + y" ,
	def "llamaA(x)"
		"ifzero x	\
		\ then 0	\
		\ else 1 + llamaB(x-1) ",
	def "llamaB(x)"
		"1 * llamaA(x)"
  ]

main0 = parser "7"
main1 = parser "fact(5)"
main2 = parser "fib(4)"
main3 = parser "primero(4,colgate(5))"
main4 = parser "colgate(5)"
main5 = parser "suma(7,primero(4,colgate(5)))"
main6 = parser "ifzero 1 then \
		\ 4 else 3"
main7 = parser "let x=2 in suma(x,x)"
main8 = parser "let x=2 in let x=3 in suma(x,x)"
main9 = parser "let x=5 in leticia(1,2,3)"
main10 = parser "let x=5 in leticia(1,x,3)"
main11 = parser "sellama(2)"
main12 = parser "suma(2,3)"
main13 = parser "let y=3 in varLibre(2)"
main14 = parser "( fact(4) * 5 ) + 30"
main15 = parser "llamaA(8)"

testBV = (runTestTT.test) testCBV
testBN = (runTestTT.test) testCBN

testcases :: [(ProgramDef, Exp, Int)]
testcases =  [	  ( definiciones, main0,  7)
		, ( definiciones, main1,  120)
		, ( definiciones, main2  , 5)
		, ( definiciones, main6 , 3) 
		, ( definiciones, main7 , 4)
		, ( definiciones, main8 , 6)
		, ( definiciones, main9 , 18)
		, ( definiciones, main10,  39)
		, ( definiciones, main11, 2)
		, ( definiciones, main12, 5)
		, ( definiciones, main13, 4)
		, ( definiciones, main14, 150)
		, ( definiciones, main15, 8)
--Casos que se cuelgan
		, ( definiciones, main5, 11)
		, ( definiciones, main3, 4) 
		, ( definiciones, main4, 4)
	 	 ]

testCBV = map (\(a,b,c) -> (eval a b ~=? c) ) testcases
--test = eval' prog exp env
testCBN = map (\(a,b,c) -> (reduce a b ~=? c) ) testcases
