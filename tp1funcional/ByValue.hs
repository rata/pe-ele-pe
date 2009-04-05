--module ByValue(eval) where

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
--eval' p en (Let v e1 e2) = foldExp id (getVar (extendDict en v e1)) binOp ifZ fLet call e
eval' p en e = foldExp id (getVar en) binOp ifZ fLet call e



getVar :: Enviroment -> VarId -> Value
getVar e v = maybe 0 id (lookupDict e v)
-- Asigna cero a las variables que no estan definidas

binOp :: Op -> Value -> Value -> Value
binOp op a b = binOp' op seq(b a) b
binOp' :: Op -> (Value -> Value -> Value)
binOp' op | Add = (+)
 	  | Mul = (*)
 	  | Sub = (-)

ifZ :: Value -> Value -> Value
ifZ v1 v2 v3 = ifZ' seq ( v3 (seq v2 v1)) v2 v3

ifZ' :: Value -> Value -> Value
ifZ' 0 v2 _  = v2
ifZ' _ _ v3 = v3 

--fLet :: VarId -> Value -> (Environment -> Environment)
--fLet var v1 = (\e -> extendDict e var v1)

call
colgate = colgate
