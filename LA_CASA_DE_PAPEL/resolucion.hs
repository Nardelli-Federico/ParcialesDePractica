--1) -----------------------------------------------

data Ladron = Ladron {
    nombre        :: String,
    habilidades   :: [Habilidades],
    armas          :: [Arma]
}
type Habilidades = String

data Arma =
    Pistola {
        cantidad :: Int
    } 
    |Metralleta {
        cantidadM :: Int,
        balas     :: Int
    }


data Rehen = Rehen {
    nombreR       :: String,
    complot       :: Int,
    miedo         :: Int,
    plan          :: [Plan]
}
type Plan = String

tokio = Ladron {
    nombre        = "tokio",
    habilidades   = ["trabajo psicologico", "entrar en moto"],
    arma          = [Pistola 2, Metralleta 1 40 ]
}

profesor = Ladron {
    nombre        = "profesor",
    habilidades   = ["disfrazarse de linyera","disfrazarse de payaso","estar siempre un paso adelante"],
    arma          = []
}
pablo = Rehen {
    nombreR       = "pablo",
    complot       = 40,
    miedo         = 30,
    plan          = ["esconderse"]
}
arturito = Rehen {
    nombreR       = "arturito",
    complot       = 70,
    miedo         = 50,
    plan          = ["esconderse", "atacar con pablo"]
}


--2) -----------------------------------------------

ladronInteligente unLadron = ( > 2) . length $ habilidades unLadron

--3) -----------------------------------------------


adquirirArma unLadron nuevaArma = -- SOlo pueden adquirir pistolas y metralladoras

--4) -----------------------------------------------   
--intimidadRehen unLadron unRehen metodo = 