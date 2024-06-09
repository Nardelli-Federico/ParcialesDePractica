data Guantelete = Guantelete {
    material :: String,
    gemas    :: [Gema]
}
type Gema = Personaje -> Personaje

data Personaje = Personaje {
    nombre :: String,
    edad   :: Int,
    habilidades :: [Habilidad],
    energia :: Int,
    planeta :: String
}

type Habilidad = String

type Universo = [Personaje]

puedeUsarse :: Guantelete -> Bool
puedeUsarse unGuantelete = material unGuantelete == "uru" && length ( gemas unGuantelete ) == 6

reducirUniverso unUniverso = take ( div 2 . length $ unUniverso) unUniverso

chasquidoUniverso :: Guantelete -> ([a] -> [a]) -> [a] -> [a]
chasquidoUniverso unGuantelete unUniverso 
    |puedeUsarse unGuantelete = reducirUniverso
    |otherwise = unUniverso

-- 2) 
aptoPendex :: Universo -> Bool
aptoPendex  = any $ ( <= 45 ) . edad

energiaTotal = sum . map energia . filter ( ( > 1 ) . length . habilidades )


-- 3)

laMente :: Int -> Gema
laMente energiaRestada unPersonaje = quitarEnergia energiaRestada unPersonaje

elPoder :: Gema
elPoder unPersonaje 
    |muchasHabilidades unPersonaje = quitarHabilidades . dejarSeco $ unPersonaje
    |otherwise = dejarSeco unPersonaje

muchasHabilidades = ( >= 2 ) . length . habilidades
dejarSeco unPersonaje = quitarEnergia (energia unPersonaje) unPersonaje

quitarHabilidades unPersonaje = unPersonaje {habilidades = []}

elEspacio :: Gema
elEspacio unPersonaje = unPersonaje {planeta = "bolivia"}

elAlma :: Habilidad -> Gema
elAlma habilidadAQuitar unPersonaje 
    |poseeHabilidad habilidadAQuitar unPersonaje = quitarHabilidad habilidadAQuitar $ quitarEnergia 10 unPersonaje
    |otherwise = quitarEnergia 10 unPersonaje

poseeHabilidad :: Habilidad -> Personaje -> Bool
poseeHabilidad habilidadAQuitar = any ( == habilidadAQuitar) . habilidades 

quitarHabilidad :: Habilidad -> Personaje -> Personaje
quitarHabilidad habilidadAQuitar unPersonaje  = unPersonaje {habilidades = filter (habilidadAQuitar /= ) (habilidades unPersonaje) }

elTiempo :: Gema
elTiempo unPersonaje
    | (< 18) . edad . reducirMitadEdad $ unPersonaje = quitarEnergia 50 $ unPersonaje {edad = 18}
    |otherwise = quitarEnergia 50 unPersonaje

reducirMitadEdad unPersonaje = unPersonaje {edad = div (edad unPersonaje ) 2}

laGemaLoca :: Gema -> Gema
laGemaLoca gemaUsada = gemaUsada . gemaUsada

quitarEnergia :: Int -> Gema
quitarEnergia energiaRetirada unPersonaje = unPersonaje {energia = energia unPersonaje - energiaRetirada}

