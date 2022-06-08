{-
Nombre: dubreucq, agustin
Legajo: 178247-2
-}

module Lib where
import Text.Show.Functions
import Data.Fixed (HasResolution)

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

data Participante = UnParticipante {
  nombre :: String,
  experiencia :: Int,
  inteligencia :: Int,
  destreza :: Int,
  rol :: Rol
} deriving Show

data Arma = UnArma {
  valorCombate :: Int,
  experienciaMinima :: Int
} deriving Show

type Rol = Participante -> Int

indeterminado :: Participante -> Int
indeterminado participante = destreza participante + inteligencia participante

soporte :: Participante -> Int
soporte participante = experiencia participante + inteligencia participante*7

primeraLinea :: Arma -> Participante -> Int
primeraLinea arma participante = (destreza participante + potenciaArma participante arma) * div (experiencia participante) 100


potenciaArma :: Participante -> Arma -> Int
potenciaArma participante arma
  |experienciaMinima arma <= experiencia participante = valorCombate arma
  | otherwise = div (valorCombate arma) 2

participante = UnParticipante "participante" 1000 20 12 indeterminado

poder :: Participante -> Int
poder participante = experiencia participante * rol participante participante

{-------------------------------punto 2--------------------------------------}
elegirNuevoRol :: Participante -> [Rol] -> Participante
elegirNuevoRol participante roles = cambiarRol participante (maximoSegun ($participante) roles)

cambiarRol :: Participante -> Rol -> Participante
cambiarRol participante rol = participante{rol = rol}

rolesEjemplo = [indeterminado, soporte, primeraLinea arma]
arma = UnArma 20 750

maestroDeArmas :: [Arma] -> Rol 
maestroDeArmas armas participante = sum . map (potenciaArma participante) . take 3 . filter (puedeUsarArma participante) $ armas

puedeUsarArma :: Participante -> Arma -> Bool
puedeUsarArma participante arma = experienciaMinima arma <= experiencia participante

{-3 d) es posible gracias a que solo tomamos los 3 primeros(que cumplan la condicion) por lo que gracias a la evalucacion perezosa
va a ser posible dar un resultado-} 

{-------------------------------punto 3--------------------------------------}
participanteSeEncuentra :: Participante -> [Participante] -> Bool
participanteSeEncuentra participante = elem (nombre participante) . map nombre

recompensa :: [Participante] -> [Participante] -> Int
recompensa todos ganadores = experiencia . inteligenciaYDestrezaMasAltos . filter (`participanteSeEncuentra` ganadores) $ todos

inteligenciaYDestrezaMasAltos :: [Participante] -> Participante
inteligenciaYDestrezaMasAltos = maximoSegun valorMasAltoEntreInteligenciaYDestreza 

valorMasAltoEntreInteligenciaYDestreza :: Participante -> Int
valorMasAltoEntreInteligenciaYDestreza participante = max (destreza participante) (inteligencia participante)
{-------------------------------punto 4--------------------------------------}










