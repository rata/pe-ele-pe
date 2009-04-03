module Dict(Dict, emptyDict, extendDict, lookupDict, removeDict, makeDict) where

-- Implementación de diccionarios usando funciones.

data Dict key val = DD (key -> Maybe val)

-- *** Ejercicio 1 ***

emptyDict :: Dict key val

extendDict :: Eq key => Dict key val -> key -> val -> Dict key val

lookupDict :: Dict key val -> key -> Maybe val

removeDict :: Eq key => Dict key val -> key -> Dict key val

makeDict :: Eq key => [(key, val)] -> Dict key val

