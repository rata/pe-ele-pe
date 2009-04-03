module ByName(reduce) where

import Lenguaje
import Dict

-- Una sustitución mapea variables a expresiones.
type Substitution = Dict VarId Exp

-- *** Ejercicio 5 ***

-- Extiende el dominio de la sustitución para que se
-- pueda aplicar a cualquier expresión.
substitute :: Substitution -> Exp -> Exp


-- *** Ejercicio 6 ***

-- Reduce la expresión un paso. Si la expresión está en
-- forma normal, devuelve Done.
-- Si la expresión reduce en un paso a exp', devuelve
-- (ReducesTo exp').

data Result exp = ReducesTo exp | Done
  deriving Show

reduceOneStep :: ProgramDef -> Exp -> Result Exp


-- *** Ejercicio 7 ***

-- Reduce la expresión a forma normal, aplicando
-- reduceOneStep tantas veces como sea necesario.
reduce :: ProgramDef -> Exp -> Value

