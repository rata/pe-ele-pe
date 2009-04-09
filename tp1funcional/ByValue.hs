module ByValue(eval) where

import Maybe
import Lenguaje
import Dict

-- Un entorno es un mapeo de variables a valores.
type Environment = Dict VarId Value 

-- *** Ejercicios 3 y 4 ***

-- Evalúa una expresión y devuelve el valor.
eval :: ProgramDef -> Exp -> Value
eval p e = eval' p e emptyDict

eval' :: ProgramDef -> Exp -> Environment -> Value
eval' p e en = (foldExp const getVar binOp ifZ fLet  (call p) e ) en


getVar :: VarId -> Environment -> Value
getVar v = \en -> fromJust (lookupDict en v)
-- fromJust.(flip lookupDict) v

--binOp :: Op -> (Environment -> Value) -> (Environment -> Value) -> (Environment -> Value)
--binOp op a b = binOp' op (seq b a) b
binOp :: Op -> (Environment -> Value) -> (Environment -> Value) -> Environment -> Value
--binOp Add f g en = (+) (f en) (g en)
--binOp Mul f g en = (*) (f en) (g en)
--binOp Sub f g en = (-) (f en) (g en)
binOp Add f g = \en -> forceBV (+) (f en) (g en)
binOp Mul f g = \en -> forceBV (*) (f en) (g en)
binOp Sub f g = \en -> forceBV (-) (f en) (g en)
--Hay que forzarlo a calcular los dos sumandos y luego sumar


forceBV :: (a -> b -> c) -> a -> b -> c
forceBV f a b = f (seq b a) b

ifZ :: (Environment -> Value) -> (Environment -> Value) -> (Environment -> Value) -> Environment -> Value
ifZ v1 v2 v3 en = ifZ' (v1 en) (v2 en) (v3 en)

ifZ' :: Value -> a -> a -> a
ifZ' 0 v2 _  = v2
ifZ' _ _  v3 = v3 

fLet :: VarId -> (Environment -> Value) -> (Environment -> Value) -> Environment -> Value
fLet var f1 f2 en = f2 (extendDictByV en var (f1 en))

call :: ProgramDef -> FuncId -> [(Environment -> Value)] -> Environment -> Value
call p f fs en = eval' p (getExp defFunc) nen
	where 	nen = foldr (\t d -> extendDictByV d (fst t) (snd t)) emptyDict (zip (getParms defFunc) vs)
		vs = map (\a -> a en ) fs
		defFunc = fromJust (lookupDict p f)

extendDictByV :: Eq a => (Dict a b) -> a -> b -> (Dict a b)
extendDictByV d k v = seq v (extendDict d k v)

-- type ProgramDef = Dict FuncId FuncDef
-- data FuncDef = FuncDef [VarId] Exp

getExp :: FuncDef -> Exp
getExp (FuncDef _ exp) = exp
getParms :: FuncDef -> [VarId]
getParms (FuncDef ls _ ) = ls

