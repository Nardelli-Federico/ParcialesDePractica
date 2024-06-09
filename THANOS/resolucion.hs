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

chasquidoUniverso unGuantelete unUniverso 
    |puedeUsarse unGuantelete = reducirUniverso
    |otherwise = unUniverso


