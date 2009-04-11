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
substitute exp subs = foldExp (\v _ -> Const v ) sVar sBinOp sIfZ sLet sCall exp subs

sVar :: VarId -> Substitution -> Exp
sVar a s | isNothing e = Var a
	 | otherwise = fromJust e
		where e = lookupDict s a

sBinOp :: Op -> (Substitution -> Exp) -> (Substitution -> Exp) -> Substitution -> Exp
sBinOp o e1 e2 s = BinOp o (e1 s) (e2 s)

sIfZ :: (Substitution -> Exp) -> (Substitution -> Exp) -> (Substitution -> Exp) -> Substitution -> Exp
sIfZ e1 e2 e3 s = IfZero (e1 s) (e2 s) (e3 s)

sLet :: VarId -> (Substitution -> Exp) -> (Substitution -> Exp) -> Substitution -> Exp
sLet v e1 e2 s = Let v (e1 s) (e2 (removeDict s v))

sCall :: FuncId -> [(Substitution -> Exp)] -> Substitution -> Exp
sCall f es s = Call f (map (\e -> e s ) es )

-- *** Ejercicio 6 ***

-- Reduce la expresión un paso. Si la expresión está en
-- forma normal, devuelve Done.
-- Si la expresión reduce en un paso a exp', devuelve
-- (ReducesTo exp').

data Result exp = ReducesTo exp | Done
  deriving Show

reduceOneStep :: ProgramDef -> Exp -> Result Exp
reduceOneStep p e = snd (reduceOneStep' p e)

reduceOneStep' :: ProgramDef -> Exp -> (Exp, Result Exp)
reduceOneStep' p e = foldExp (\v -> (Const v, Done)) (\v -> (Var v, Done)) rBinOp rIfZ rLet (rCall p) e

rBinOp :: Op -> (Exp, Result Exp) -> (Exp, Result Exp) -> (Exp, Result Exp)
rBinOp op (e1, ReducesTo x)  (e2, _) = (BinOp op e1 e2, ReducesTo (BinOp op x e2))
rBinOp op (e1, Done)  (e2, ReducesTo x) = (BinOp op e1 e2, ReducesTo (BinOp op e1 x))
rBinOp op (Const e1, Done)  (Const e2, Done) = (BinOp op (Const e1) (Const e2), ReducesTo (Const (op2Func op e1 e2)))

rIfZ :: (Exp, Result Exp) -> (Exp, Result Exp) -> (Exp, Result Exp) -> (Exp, Result Exp)
rIfZ (e1, ReducesTo x) (e2, _) (e3, _) = (IfZero e1 e2 e3, ReducesTo (IfZero x e2 e3))
rIfZ (Const 0, Done) (e2, _) (e3, _) = (IfZero (Const 0) e2 e3, ReducesTo e2)
rIfZ (Const x, Done) (e2, _) (e3, _) = (IfZero (Const x) e2 e3, ReducesTo e3)


rLet :: VarId -> (Exp, Result Exp) -> (Exp, Result Exp) -> (Exp, Result Exp)
rLet v (e1, _) (e2, _) = (Let v e1 e2, ReducesTo (substitute e2 (makeDict [(v,e1)] )))

rCall :: ProgramDef -> FuncId -> [(Exp, Result Exp)] -> (Exp, Result Exp)
rCall p f xs = (Call f params, g)
	where   g = ReducesTo (substitute e s)
		e = getExp defFunc
		s = makeDict (zip (getParms defFunc) params)
		defFunc = fromJust (lookupDict p f)
		params = fst (unzip xs)

-- *** Ejercicio 7 ***

-- Reduce la expresión a forma normal, aplicando
-- reduceOneStep tantas veces como sea necesario.
reduce :: ProgramDef -> Exp -> Value
reduce p e = (getValue.res2Exp.last) reducciones
	where	reducciones = takeWhile faltaReducir (iterate reduceResult (ReducesTo e))
		reduceResult re = reduceOneStep p (res2Exp re)
		faltaReducir (ReducesTo _) = True
		faltaReducir _             = False
		getValue (Const x) = x
				
res2Exp :: Result Exp -> Exp
res2Exp (ReducesTo e) = e

