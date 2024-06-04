data Persona = Persona {
    edad         :: Int,
    items        :: [Item],
    experiencia  :: Int
}
type Item = String

data Criatura = Criatura {
    especie      :: Especie,
    peligrosidad :: Int,
    debilidad    :: Debilidad
}

data Fantasma = Fantasma {
    categoria     :: Int,
    asunto        :: String,
}

data Especie   = Siempredetras | Gnomo | Fantasma

type Debilidad = String