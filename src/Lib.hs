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


data Desafio = Desafio {
    rolesDisponibles :: [Rol],
    pruebaASuperar :: Participante -> Bool
  }


----------------------------------------------
-- Definí tus tipos de datos y funciones aquí
-- abajo, indicando a qué punto pertenecen
----------------------------------------------


{---------------------------------------------punto 1-------------------------------------------------}
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

indeterminado :: Rol
indeterminado participante = destreza participante + inteligencia participante

soporte :: Rol
soporte participante = experiencia participante + inteligencia participante*7

primeraLinea :: Arma -> Rol
primeraLinea arma participante = (destreza participante + potenciaArma participante arma) * div (experiencia participante) 100


potenciaArma :: Participante -> Arma -> Int
potenciaArma participante arma
  |experienciaMinima arma <= experiencia participante = valorCombate arma
  | otherwise = div (valorCombate arma) 2

participante = UnParticipante "participante" 1000 20 12 indeterminado

poder :: Participante -> Int
poder participante = experiencia participante * aptitudParaSuRol participante

aptitudParaSuRol :: Participante -> Int
aptitudParaSuRol = rol participante

{---------------------------------------------punto 2-------------------------------------------------}
elegirNuevoRol :: Participante -> [Rol] -> Participante
elegirNuevoRol participante roles = cambiarRol participante (maximoSegun ($participante) roles)

cambiarRol :: Participante -> Rol -> Participante
cambiarRol participante rol = participante{rol = rol}

maestroDeArmas :: [Arma] -> Rol 
maestroDeArmas armas participante = sum . map (potenciaArma participante) . take 3 . filter (puedeUsarArma participante) $ armas

puedeUsarArma :: Participante -> Arma -> Bool
puedeUsarArma participante arma = experienciaMinima arma <= experiencia participante

{-2 b) poder (elegirNuevoRol participante [indeterminado, soporte, primeraLinea (UnArma 20 750)])-}

{-2 d) es posible gracias a que solo tomamos los 3 primeros(que cumplan la condicion) por lo que gracias a la evalucacion perezosa
va a ser posible dar un resultado-} 

{---------------------------------------------punto 3-------------------------------------------------}
participanteSeEncuentra :: Participante -> [Participante] -> Bool
participanteSeEncuentra participante = elem (nombre participante) . map nombre

recompensa :: [Participante] -> [Participante] -> Int
recompensa todos ganadores = experiencia . inteligenciaYDestrezaMasAltos . filter (not . (`participanteSeEncuentra` ganadores)) $ todos

inteligenciaYDestrezaMasAltos :: [Participante] -> Participante
inteligenciaYDestrezaMasAltos = maximoSegun valorMasAltoEntreInteligenciaYDestreza 

valorMasAltoEntreInteligenciaYDestreza :: Participante -> Int
valorMasAltoEntreInteligenciaYDestreza participante = max (destreza participante) (inteligencia participante)

{---------------------------------------------punto 4-------------------------------------------------}
--encararDesafio :: [Participante] -> Desafio -> [Participante]
--encararDesafio participantes desafio = map (incrementarExperiencia (recompensa participantes (ganadores participantes desafio))) (ganadores participantes desafio)

encararDesafio :: [Participante] -> Desafio -> [Participante]
encararDesafio participantes desafio = incrementarExperienciaATodos (ganadores desafio participantes) (recompensa participantes (ganadores desafio participantes))

incrementarExperienciaATodos:: [Participante] -> Int -> [Participante]
incrementarExperienciaATodos participantes valor = map (incrementarExperiencia valor) participantes

ganadores :: Desafio -> [Participante] -> [Participante]
ganadores desafio = filter (pruebaASuperar desafio) . map (`elegirNuevoRol` rolesDisponibles desafio) 

incrementarExperiencia :: Int -> Participante -> Participante
incrementarExperiencia valor participante = participante {experiencia = experiencia participante + valor}

{---------------------------------------------punto 5-------------------------------------------------}
type Torneo = [Desafio]
jugarTorneo :: [Participante] -> Torneo -> [Participante]
--jugarTorneo participantes [] = participantes
--jugarTorneo participantes (d1:ds) = jugarTorneo (encararDesafio participantes d1) ds

jugarTorneo = foldl encararDesafio 

{--------------------------------------------------Ejemplos para Pruebas------------------------------------------}

{-
part1 = UnParticipante "agus" 750 20 12 indeterminado 
part2 = UnParticipante "lucas" 850 30 12 indeterminado 
part3 = UnParticipante "maxi" 500 15 12 indeterminado 
part4 = UnParticipante "colo" 1000 20 23 (primeraLinea arma3) 

rolesEjemplo = [indeterminado, soporte, primeraLinea arma1]
arma1 = UnArma 20 750
arma2 = UnArma 30 800
arma3 = UnArma 25 900
arma4 = UnArma 30 1000

desafio1 = Desafio rolesEjemplo ((>500). experiencia)
-}









