module ByValue(eval) where

import Lenguaje
import Dict

-- Un entorno es un mapeo de variables a valores.
type Environment = Dict VarId Value 

-- *** Ejercicios 3 y 4 ***

-- Evalúa una expresión y devuelve el valor.
eval :: ProgramDef -> Exp -> Value


