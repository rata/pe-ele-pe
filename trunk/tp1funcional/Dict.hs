module Dict(Dict, emptyDict, extendDict, lookupDict, removeDict, makeDict) where

-- Implementación de diccionarios usando funciones.

data Dict key val = DD (key -> Maybe val)

-- *** Ejercicio 1 ***

emptyDict :: Dict key val
emptyDict = DD (\_ -> Nothing)

extendDict :: Eq key => Dict key val -> key -> val -> Dict key val
extendDict (DD f) nk nv = DD g
	where   g k | k == nk   = Just nv
		    | otherwise = f k

lookupDict :: Dict key val -> key -> Maybe val
lookupDict (DD f) k = f k

removeDict :: Eq key => Dict key val -> key -> Dict key val
removeDict (DD f) nk = DD g
	where   g k | k == nk    = Nothing
		    | otherwise  = f k

makeDict :: Eq key => [(key, val)] -> Dict key val
makeDict xs = DD (\x -> (foldr (f x) Nothing xs ))
		where f _ _ (Just a) = Just a
		      f x k Nothing | (fst k) == x = Just (snd(k))
		      	            | otherwise    = Nothing

--makeDict :: Eq key => [(key, val)] -> Dict key val
--makeDict xs = DD f
--	where f x | empty ts  = Nothing
--	          | otherwise = Just (snd(head(ts)))
--		where ts = filter (\t -> (fst(t) == x)) xs

--empty :: [a] -> Bool
--empty [] = True
--empty _  = False
