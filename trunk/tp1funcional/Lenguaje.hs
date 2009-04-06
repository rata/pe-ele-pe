module Lenguaje where

import Dict

type Value = Int
type FuncId = String
type VarId = String

-- Una expresión del lenguaje es un valor del tipo de datos Exp.

data Op = Add | Sub | Mul
  deriving Show

data Exp = Const Value
         | Var VarId
         | BinOp Op Exp Exp
         | IfZero Exp Exp Exp
         | Let VarId Exp Exp
         | Call FuncId [Exp]
         deriving Show

-- Un programa es un diccionario de nombres de funciones a
-- sus definiciones.
type ProgramDef = Dict FuncId FuncDef

-- Una función se define mediante los nombres de los
-- parámetros y el cuerpo.
data FuncDef = FuncDef [VarId] Exp
  deriving Show

-- *** Ejercicio 2 ***

foldExp :: (Value -> b) -> (VarId -> b ) -> (Op -> b -> b -> b) -> ( b -> b -> b -> b) -> (VarId -> b -> Exp -> b) -> (FuncId  -> [b] -> b) -> Exp -> b
foldExp fC _ _ _ _ _       (Const v)         = fC v
foldExp _  fV _ _ _ _      (Var v)           = fV v
foldExp fC fV fB fI fL fCa (BinOp o e1 e2)   = fB o (foldExp fC fV fB fI fL fCa e1) (foldExp fC fV fB fI fL fCa e2)
foldExp fC fV fB fI fL fCa (IfZero e1 e2 e3) = fI (foldExp fC fV fB fI fL fCa e1) (foldExp fC fV fB fI fL fCa e2) (foldExp fC fV fB fI fL fCa e3)
foldExp fC fV fB fI fL fCa (Let v e1 e2)     = fL v (foldExp fC fV fB fI fL fCa e1) e2
foldExp fC fV fB fI fL fCa (Call f e1s)      = fCa f (map (foldExp fC fV fB fI fL fCa) e1s)

