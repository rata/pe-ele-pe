module ByName(reduce) where

import Lenguaje
import Dict
import Maybe

-- Una sustitución mapea variables a expresiones.
type Substitution = Dict VarId Exp

-- *** Ejercicio 5 ***

-- Extiende el dominio de la sustitución para que se
-- pueda aplicar a cualquier expresión.
substitute :: Exp -> Substitution -> Exp
substitute e1 s = foldExp (\v _ -> Const v) gv gbinOp gifZ gLet gCall e1 s

gv :: VarId -> Substitution -> Exp
gv a s	| isNothing e = Var a
	| otherwise = fromJust e
		where e = lookupDict s a

gbinOp :: Op -> (Substitution -> Exp) -> (Substitution -> Exp) -> Substitution -> Exp
gbinOp o e1 e2 s = BinOp o (e1 s) (e2 s)

gifZ :: (Substitution -> Exp) -> (Substitution -> Exp) -> (Substitution -> Exp) -> Substitution -> Exp
gifZ e1 e2 e3 s = IfZero (e1 s) (e2 s) (e3 s)

gLet :: VarId -> (Substitution -> Exp) -> (Substitution -> Exp) -> Substitution -> Exp
gLet v e1 e2 s = Let v (e1 s) (e2 (removeDict s v))

gCall :: FuncId -> [(Substitution -> Exp)] -> Substitution -> Exp
gCall f es s = Call f (map (\e -> e s ) es )

-- *** Ejercicio 6 ***

-- Reduce la expresión un paso. Si la expresión está en
-- forma normal, devuelve Done.
-- Si la expresión reduce en un paso a exp', devuelve
-- (ReducesTo exp').

data Result exp = ReducesTo exp | Done
  deriving Show

reduceOneStep :: ProgramDef -> Exp -> Result Exp
reduceOneStep p e = snd (reduceOneStep' p e)

--rBinOp op (ReducesTo x) saraza = ReducesTo (BinOp op x saraza)
--rBinOp op (Done x) saraza = BinOp op x saraza
 
reduceOneStep' :: ProgramDef -> Exp -> (Exp, Result Exp)
reduceOneStep' p e = foldExp (\v -> (Const v, Done)) (\v -> (Var v, Done)) rBinOp rifZ rLet (rCall p) e

rBinOp :: Op -> (Exp, Result Exp) -> (Exp, Result Exp) -> (Exp, Result Exp)
rBinOp op (e1, ReducesTo x)  (e2, _) = ((BinOp op e1 e2), ReducesTo (BinOp op x e2))
rBinOp op (e1, Done)  (e2, ReducesTo x) = ((BinOp op e1 e2), ReducesTo (BinOp op e1 x))
rBinOp op (Const e1, Done)  (Const e2, Done) = ((BinOp op (Const e1) (Const e2)), ReducesTo (Const ((op2Func op) e1 e2)))

rifZ :: (Exp, Result Exp) -> (Exp, Result Exp) -> (Exp, Result Exp) -> (Exp, Result Exp)
rifZ (e1, ReducesTo x) (e2, _) (e3, _) = (IfZero e1 e2 e3, ReducesTo (IfZero x e2 e3))
rifZ ((Const 0), Done) (e2, _) (e3, _) = (IfZero (Const 0) e2 e3, ReducesTo e2)
rifZ ((Const x), Done) (e2, _) (e3, _) = (IfZero (Const x) e2 e3, ReducesTo e3)


rLet :: VarId -> (Exp, Result Exp) -> (Exp, Result Exp) -> (Exp, Result Exp)
rLet v (e1, _) (e2, _) = (Let v e1 e2, ReducesTo (substitute e2 (makeDict [(v,e1)] )))

rCall :: ProgramDef -> FuncId -> [(Exp, Result Exp)] -> (Exp, Result Exp)
rCall p f xs = (Call f params, g)
	where   g = ReducesTo (substitute e s)
		e = getExp defFunc
		s = makeDict (zip (getParms defFunc) params)
--		s = foldr (\t d -> extendDict d (fst t) (snd t)) emptyDict (zip (getParms defFunc) params)
		defFunc = fromJust (lookupDict p f)
		params = fst (unzip xs)

-- *** Ejercicio 7 ***

-- Reduce la expresión a forma normal, aplicando
-- reduceOneStep tantas veces como sea necesario.
reduce :: ProgramDef -> Exp -> Value
reduce p e = getValue (res2Exp (last (tk)))
	where	tk = takeWhile g (iterate rOs (ReducesTo e))
		rOs re = reduceOneStep p (res2Exp re)
		g (ReducesTo _) = True
		g _             = False
		getValue (Const x) = x
				
res2Exp :: Result Exp -> Exp
res2Exp (ReducesTo e) = e

