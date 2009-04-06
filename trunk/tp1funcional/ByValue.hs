--module ByValue(eval) where

import Maybe
import Lenguaje
import Dict

-- Un entorno es un mapeo de variables a valores.
type Environment = Dict VarId Value 

-- *** Ejercicios 3 y 4 ***

-- type ProgramDef = Dict FuncId FuncDef
-- data FuncDef = FuncDef [VarId] Exp

-- Evalúa una expresión y devuelve el valor.
--eval :: ProgramDef -> Exp -> Value

eval' :: ProgramDef -> Environment -> Exp -> Value
eval' p en e = foldExp id (getVar en) binOp ifZ (fLet p en)  (call p en) e



-- Asigna cero a las variables que no estan definidas
getVar :: Environment -> VarId -> Value
getVar e v | a /= Nothing = fromJust a
		where a = (lookupDict e v)

binOp :: Op -> Value -> Value -> Value
binOp op a b = binOp' op (seq b a) b
binOp' :: Op -> (Value -> Value -> Value)
binOp' Add = (+)
binOp' Mul = (*)
binOp' Sub = (-)

ifZ :: Value -> Value -> Value -> Value
ifZ v1 v2 v3 = ifZ' (seq v3 (seq v2 v1)) v2 v3

ifZ' :: Value -> Value -> Value -> Value
ifZ' 0 v2 _  = v2
ifZ' _ _ v3 = v3 

fLet :: ProgramDef -> Environment -> VarId -> Value -> Exp -> Value
fLet p en var v1 e1 = eval' p (extendDict en var v1) e1

call :: ProgramDef -> Environment -> FuncId -> [Value] ->Value
call p en f vs e = eval' p nen (getExp (fromJust (lookupDict f p)))
	where nen = foldr (\ t d -> extendDict d (fst t) (snd t)) en (toTupla (getParms (fromJust (lookupDict f p))) vs) 
--	      defFunc = fromJust (lookupDict f p)

getExp (FuncDef _ exp) = exp
getParms (FuncDef ls _ ) = ls
toTupla [] [] = []
toTupla (x:xs) (y:ys) = (x,y):(toTupla xs ys)

colgate = colgate

test1 e colgate = e
