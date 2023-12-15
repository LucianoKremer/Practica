import Text.Show.Functions
import Data.List
import Data.Char

f1 (ns, nc, nf) = (ns + 1, nc + 2, nf + 3)
f2 (ns, nc, nf) = (ns, nc, nf + 5)
f3 (ns, nc, nf) = (ns, nc, nf - 3)

sinRepetidos [] = []
sinRepetidos (x:xs) = x : filter (/= x) (sinRepetidos xs)

misPociones = [felixFelices, multijugos]

invertir3 (a, b, c) = (c, b, a)

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c


maxSegun f x y | f x > f y = x
               | otherwise = y

maximoSegun f xs = foldl1 (maxSegun f) xs

data Persona = Persona {
    nombre :: String,
    niveles :: Niveles
} deriving(Show, Eq)

type Niveles = (Suerte, Poder, Fuerza)
type Suerte = Int
type Poder = Int
type Fuerza = Int

data Pocion = Pocion {
    nombrePocion :: String,
    ingredientes :: [Ingrediente]
} deriving(Show)

data Ingrediente = Ingrediente {
    nombreIngrediente :: String,
    efectos :: [Efecto],
    gramos :: Int
} deriving(Show)

type Efecto = (Niveles -> Niveles)

--------------
-- punto 01 --
--------------

harry :: Persona
harry = Persona "Harry Potter" (11, 5, 4)

ron :: Persona
ron = Persona "Ron Weasley" (6, 4, 6)

hermione :: Persona
hermione = Persona "Hermione Granger" (12, 8, 2)

--------------
-- punto 02 --
--------------

felixFelices :: Pocion
felixFelices = Pocion "FelixFelices" [(escarabajosMachacados 52), (ojoDeTigreSucio 2)]

type Formula = Int -> Ingrediente

escarabajosMachacados :: Formula
escarabajosMachacados = Ingrediente "escarabajosMachacados" [f1, f2] -- aca irian los gramos

ojoDeTigreSucio :: Formula
ojoDeTigreSucio = Ingrediente "ojoDeTigreSucion" [f3]

multijugos :: Pocion
multijugos = Pocion "Multijugos" [(cuernoBicornioEnPolvo 10), (sanguijuelasHormonales 54)]

cuernoBicornioEnPolvo :: Formula
cuernoBicornioEnPolvo = Ingrediente "cuernoBicornioEnPolvo" [invertir3, f1, f2]

sanguijuelasHormonales :: Formula
sanguijuelasHormonales = Ingrediente "sanguijuelasHormonales" [duplicarNiveles, f3]

duplicarNiveles :: Niveles -> Niveles
duplicarNiveles (a, b, c) = (a*2, b*2, c*2)

--------------
-- punto 03 --
--------------

sumaNiveles :: Niveles -> Int
sumaNiveles (a, b, c) = a + b + c

minSegun f x y | f x < f y = x
               | otherwise = y

minimoSegun f xs = foldl1 (minSegun f) xs

diferenciaNiveles :: Niveles -> Int
diferenciaNiveles (a, b, c) = abs ( maximoSegun id [a,b,c] - minimoSegun id [a,b,c] )

--------------
-- punto 04 --
--------------

sumaNivelesPersona :: Persona -> Int
sumaNivelesPersona = sumaNiveles . niveles

diferenciaNivelesPersona :: Persona -> Int
diferenciaNivelesPersona = diferenciaNiveles . niveles

--------------
-- punto 05 --
--------------

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion  = concat . map (efectos) . ingredientes

--------------
-- punto 06 --
--------------

pocionesHeavies :: [Pocion] -> [String]
pocionesHeavies = map nombrePocion . filter esExtensa

esExtensa :: Pocion -> Bool
esExtensa = (>= 4) . length . efectosDePocion

--------------
-- punto 07 --
--------------

incluyeA :: Eq a => [a] -> [a] -> Bool
incluyeA listaIncluida listaIncluyente = all (\elemento -> elem elemento listaIncluyente) (listaIncluida)

--------------
-- punto 08 --
--------------

esPocionMagica :: Pocion -> Bool
esPocionMagica unaPocion = unIngredienteTieneTodasSusVocales unaPocion && pidenCantidadParTodosSusIngredientes unaPocion

pidenCantidadParTodosSusIngredientes :: Pocion -> Bool
pidenCantidadParTodosSusIngredientes = all (even . gramos)  . ingredientes

unIngredienteTieneTodasSusVocales :: Pocion -> Bool
unIngredienteTieneTodasSusVocales = any (tieneTodasLasVocales . nombreIngrediente) . ingredientes

tieneTodasLasVocales :: String -> Bool
tieneTodasLasVocales = incluyeA "aeiou"

--------------
-- punto 09 --
--------------

mapNiveles :: ((Int, Int, Int) -> (Int, Int, Int)) -> Persona -> Persona
mapNiveles funcion unaPersona = unaPersona {niveles = funcion $ niveles unaPersona}

tomarPocion :: Pocion -> Persona -> Persona
tomarPocion unaPocion unaPersona = foldr (mapNiveles) unaPersona (efectosDePocion unaPocion)


--------------
-- punto 10 --
--------------

esAntidoto :: Persona -> Pocion -> Pocion -> Bool
esAntidoto unaPersona unaPocion otraPocion = (== unaPersona) . tomarPocion otraPocion . tomarPocion unaPocion $ unaPersona

--------------
-- punto 11 --
--------------

personaMasAfectada :: Ord a => (Persona -> a) -> Pocion -> [Persona] -> Persona
personaMasAfectada unaPonderacion unaPocion unasPersonas = maximoSegun unaPonderacion . map (tomarPocion unaPocion) $ unasPersonas

--------------
-- punto 12 --
--------------

type Consulta = Pocion -> [Persona] -> Persona

ponderacionSegunSumaDeNiveles :: Consulta
ponderacionSegunSumaDeNiveles = personaMasAfectada sumaNivelesPersona

ponderacionSegunPromedioDeNiveles :: Consulta
ponderacionSegunPromedioDeNiveles = personaMasAfectada promedioEnteroNivelesPersona

promedioEnteroNivelesPersona :: Persona -> Int
promedioEnteroNivelesPersona = ( (flip div) 3 ) . sumaNivelesPersona

ponderacionSegunFuerzaFisica :: Consulta
ponderacionSegunFuerzaFisica = personaMasAfectada fuerzaFisica

fuerzaFisica :: Persona -> Int
fuerzaFisica  = trd3 . niveles

ponderacionSegunDiferenciaDeNiveles :: Consulta
ponderacionSegunDiferenciaDeNiveles = personaMasAfectada diferenciaNivelesPersona

--------------
-- punto 13 --
--------------

superPocion :: [Ingrediente] -> Pocion
superPocion = (Pocion "superPocion") . generarIngredientesAlternadosDeFormaIncremental

generarIngredientesAlternadosDeFormaIncremental :: [Ingrediente] -> [Ingrediente]
generarIngredientesAlternadosDeFormaIncremental = concatMap (iterate modificarIngredientes ) 
