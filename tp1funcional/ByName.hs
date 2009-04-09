--module ByName(reduce) where

import Lenguaje
import Dict
import Maybe

-- Una sustitución mapea variables a expresiones.
type Substitution = Dict VarId Exp

-- *** Ejercicio 5 ***

-- Extiende el dominio de la sustitución para que se
-- pueda aplicar a cualquier expresión.
--substitute :: Exp -> Substitution -> Exp
--substitute e1 s = foldExp (\v -> Const v) gv gbinOp gifZ gLet gCall e1  s

gv :: VarId -> Substitution -> Exp
gv a s	| isNothing v = Var a
	| otherwise = fromJust v
		where v = lookupDict s a

gbinOp :: Op -> (Substitution -> Exp) -> (Substitution -> Exp) -> Substitution -> Exp
gbinOp o e1 e2 s = BinOp o (e1 s) (e2 s)

gifZ :: (Substitution -> Exp) -> (Substitution -> Exp) -> (Substitution -> Exp) -> Substitution -> Exp
gifZ e1 e2 e3 s = IfZero (e1 s) (e2 s) (e3 s)

gLet :: VarId -> (Substitution -> Exp) -> (Substitution -> Exp) -> Substitution -> Exp
gLet v e1 e2 s = Let v (e1 s) (e2 (removeDict s v))

gCall :: FuncId -> [(Substitution -> Exp)] -> Substitution -> Exp
gCall f es _ = Call f (map (\e -> (e emptyDict)) es )

-- *** Ejercicio 6 ***

-- Reduce la expresión un paso. Si la expresión está en
-- forma normal, devuelve Done.
-- Si la expresión reduce en un paso a exp', devuelve
-- (ReducesTo exp').

data Result exp = ReducesTo exp | Done
  deriving Show

--reduceOneStep :: ProgramDef -> Exp -> Result Exp


-- *** Ejercicio 7 ***

-- Reduce la expresión a forma normal, aplicando
-- reduceOneStep tantas veces como sea necesario.
--reduce :: ProgramDef -> Exp -> Value

