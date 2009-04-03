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

-- foldExp :: ???


