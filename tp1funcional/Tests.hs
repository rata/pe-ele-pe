
import Dict
import Lenguaje
import Parser

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
        \   x * fact(x - 1) "
  ]

main1 = parser "fact(5)"

--test1 = evaluar definiciones main1

