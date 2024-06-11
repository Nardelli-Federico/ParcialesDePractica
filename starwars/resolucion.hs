data Nave = Nave {
    nombre      :: String,
    durabilidad :: Int,
    escudo      :: Int,
    ataque      :: Int,
    poder       :: Poder
} 

type Poder = Nave -> Nave

modificarAtaque :: (Int -> Int -> Int) -> Int -> Poder
modificarAtaque operacion valor unaNave = unaNave {ataque = operacion ( ataque unaNave ) valor}

modificarDurabilidad :: (Int -> Int -> Int) -> Int -> Poder
modificarDurabilidad operacion valor unaNave = unaNave {durabilidad = operacion ( durabilidad unaNave ) valor}


movimientoTurbo :: Poder
movimientoTurbo = modificarAtaque ( + ) 25 

reparacionEmergencia :: Poder
reparacionEmergencia = modificarAtaque ( - ) 40 . modificarDurabilidad ( + ) 50

movimientoSuperTurbo :: Poder
movimientoSuperTurbo = movimientoTurbo . movimientoTurbo . movimientoTurbo . modificarDurabilidad ( - ) 45

modificarEscudo :: Poder
modificarEscudo unaNave = unaNave {escudo = escudo unaNave + 100}

detonacion :: Poder
detonacion unaNave = modificarDurabilidad ( - ) ( durabilidad unaNave ) . reparacionEmergencia $ unaNave

superReparcionEmergencia :: Poder
superReparcionEmergencia = modificarEscudo. reparacionEmergencia 

tie = Nave {
    nombre      = "TIE Fighter",
    durabilidad = 200,
    escudo      = 100,
    ataque      = 50,
    poder       = movimientoTurbo
}
xwing = Nave {
    nombre      = "X Wing",
    durabilidad = 300,
    escudo      = 150,
    ataque      = 100,
    poder       = reparacionEmergencia
}
darth = Nave {
    nombre      = "Nave De Darth Vader",
    durabilidad = 500,
    escudo      = 300,
    ataque      = 200,
    poder       = movimientoSuperTurbo
}
falcon = Nave {
    nombre      = "Millennium Falcon",
    durabilidad = 1000,
    escudo      = 500,
    ataque      = 50,
    poder       = superReparcionEmergencia
}
bondifornite = Nave {
    nombre      = "Bondi de Fornite",
    durabilidad = 1000,
    escudo      = 0,
    ataque      = 0,
    poder       = detonacion
}

--2)

durabilidadTotal :: [Nave] -> Int
durabilidadTotal = foldr ( ( + ) . durabilidad ) 0

--3)

daño :: Nave -> Nave -> Int 
daño defensora agresora 
    | escudo defensora >= ataque agresora = 0
    | ataque agresora - escudo defensora > durabilidad defensora = durabilidad defensora
    | otherwise = ataque agresora - escudo defensora 

confrontacion :: Nave -> Poder
confrontacion defensora agresora = modificarDurabilidad ( - ) (daño ( aplicarPoder defensora ) ( aplicarPoder agresora )) defensora

aplicarPoder :: Nave -> Nave 
aplicarPoder unaNave =  poder unaNave unaNave

--4)

fueraDeCombate :: Nave -> Bool
fueraDeCombate = ( ==0 ) . durabilidad

--5)

type Estrategia = Nave -> Bool

naveDebil :: Estrategia
naveDebil = ( < 200 ) . escudo

navePeligrosa :: Int -> Estrategia
navePeligrosa valor = ( < valor ) . ataque

naveFueraDeCombate :: Nave -> Estrategia 
naveFueraDeCombate defensora  = ( == 0 ) . durabilidad . confrontacion defensora 

naveNoTanque :: Estrategia
naveNoTanque = ( < 1000 ) . escudo

mision :: [Nave] -> Nave -> Estrategia -> [Nave]
mision unaFlota unaNave unaEstrategia = map (  flip ( ataquePosible unaNave unaEstrategia )  unaNave ) unaFlota

ataquePosible :: Nave -> Estrategia -> Nave -> ( Nave-> Nave-> Nave)
ataquePosible unaNave unaEstrategia naveDeFlota 
    | unaEstrategia naveDeFlota = confrontacion
    |otherwise = const



---6)