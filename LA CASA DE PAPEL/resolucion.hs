--1) -----------------------------------------------

data Ladron = Ladron {
    nombre        :: String,
    habilidades   :: [Habilidades],
    pistola       :: Int,
    ametralladora :: Int
}
type Habilidades = String

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
    pistola       = 2,
    ametralladora = 1
}

profesor = Ladron {
    nombre        = "profesor",
    habilidades   = ["disfrazarse de linyera","disfrazarse de payaso","estar siempre un paso adelante"],
    pistola       = 0,
    ametralladora = 0
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

adquirirArma unLadron nuevaArma 
    |nuevaArma == "pistola"       = unLadron { pistola = pistola unLadron + 1}
    |nuevaArma == "ametralladora" = unLadron { ametralladora = ametralladora unLadron +1}

--4) -----------------------------------------------   
intimidadRehen unLadron unRehen = 