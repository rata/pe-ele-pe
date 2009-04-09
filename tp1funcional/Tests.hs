
import Dict
import Lenguaje
import Parser
import ByValue
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
	"x + y"
  ]

main1 = parser "fact(5)"
main2 = parser "fib(4)"
main3 = parser "primero(4,colgate(5))"
main4 = parser "colgate(5)"
main5 = parser "suma(7,primero(4,colgate(5)))"
main6 = parser "ifzero 0 then \
		\ 4 else colgate(3)"


testBV = runTestTT test1
test1 = test [eval definiciones main1  ~=? 120
		, eval definiciones main2  ~=? 5 
--		, eval definiciones main3  ~=? 4 
--		, eval definiciones main4  ~=? 4 
--		, eval definiciones main5  ~=? 4
		, eval definiciones main6  ~=? 4  
		 ]

--test = eval' prog exp env

