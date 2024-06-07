data Persona = Persona {
    edad         :: Int,
    items        :: [Item],
    experiencia  :: Int
}
type Item = String

data Criatura =
    Gnomo {
        cantidad      :: Int
    }
    |SiempreDetras deriving (Eq, Ord)

type Debilidad = String

--2)----------------------------------------

venceA unaPersona unaCriatura 
    | unaCriatura == Gnomo = head ( items unaPersona ) == "barredor de ojas"
    |otherwise = False

experienciaGanada 

enfrentamiento :: t1 -> t2 -> p
enfrentamiento unaPersona unaCriatura 
    | unaPersona `venceA` unaCriatura = Persona {experiencia = sumarExperiencia unaPersona ( experienciaGanada unaCriatura ) }
    | otherwise                       = Persona {experiencia = experiencia unaPersona +1}




