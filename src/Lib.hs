{-
Nombre: dubreucq, agustin
Legajo: 999999-9
-}

module Lib where
import Text.Show.Functions

----------------------------------------------
-- Código base provisto en el enunciado
----------------------------------------------

maximoSegun :: Ord a => (b -> a) -> [b] -> b
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: Ord a => (p -> a) -> p -> p -> p
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- Descomentar luego de definir los tipos Rol y Participante
-- de acuerdo al modelo elegido en el punto 1:

{-
data Desafio = Desafio {
    rolesDisponibles :: [Rol],
    pruebaASuperar :: Participante -> Bool
  }
-}

----------------------------------------------
-- Definí tus tipos de datos y funciones aquí
-- abajo, indicando a qué punto pertenecen
----------------------------------------------

