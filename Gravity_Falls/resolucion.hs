data Persona = Persona {
    edad         :: Int,
    items        :: [Item],
    experiencia  :: Int
}
type Item = String

data Criatura =
    Fantasma {
        categoria     :: Int,
        asunto        :: String 
    }
    |Gnomo {
        cantidad      :: Int
    }
    |SiempreDetras deriving (Eq, Ord)

type Debilidad = String

--2)----------------------------------------

{-enfrentamiento unaPersona unaCriatura 
    |unaCriatura == Fantasma
-}
type cumpleAsuntoPendiente :: Persona -> Criatura -> Bool

