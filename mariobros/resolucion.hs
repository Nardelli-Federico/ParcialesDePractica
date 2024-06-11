import qualified Control.Applicative as String
data Plomero     = Plomero {
    nombre       :: String,
    herramientas :: [Herramienta],
    reparaciones :: [Reparacion],
    dinero       :: Float
}

data Reparacion = Reparacion {
    descripcion  :: String,
    requisito    :: Requisito
}

type Requisito = Plomero -> Bool

data Herramienta = Herramienta {
    denominacion :: String,
    precio       :: Float,
    empuñadura   :: Empuñadura
}

data Empuñadura = Hierro | Madera | Goma | Plastico deriving(Eq)

llaveInglesa = Herramienta {
    denominacion = "Llave Inglesa",
    precio = 200.0,
    empuñadura = Hierro
}
martillo = Herramienta {
    denominacion = "Martillo",
    precio = 20.0,
    empuñadura = Madera
}

mario = Plomero {
    nombre       = "Mario",
    herramientas = [ llaveInglesa , martillo ],
    reparaciones = [],
    dinero       = 1200.0
}
wamario = Plomero {
    nombre       = "Wario",
    herramientas = tenerInfinitasHerramientas llaveInglesa,
    reparaciones = [],
    dinero       = 0.5
}

tenerInfinitasHerramientas :: Herramienta -> [Herramienta]
tenerInfinitasHerramientas herramientaConocida = [aumentarPrecio herramientaConocida precio | precio <- [0..]]

aumentarPrecio :: Herramienta -> Float -> Herramienta
aumentarPrecio herramienta valorNuevo = herramienta {precio = precio herramienta + valorNuevo}

--2)

--a)

herramientaDenominacion :: Plomero -> String -> Bool
herramientaDenominacion unPlomero unaDenominacion = any ( ( == unaDenominacion ) . denominacion) ( herramientas unPlomero)

--b) 

esMalvado :: Plomero -> Bool
esMalvado unPlomero = ( ( == "Wa") . take 2 ) ( nombre unPlomero)

--c)
puedeComprarHerramienta :: Plomero -> Herramienta -> Bool
puedeComprarHerramienta unPlomero unaHerramienta = precio unaHerramienta <= dinero unPlomero


--3)

empuñaduraDeHerramienta :: Empuñadura -> Herramienta -> Bool
empuñaduraDeHerramienta unaEmpuñadura = ( == unaEmpuñadura) . empuñadura

hierroCostoso :: Herramienta -> Bool
hierroCostoso unaHerramienta= ( ( > 1000 ) . precio $ unaHerramienta  ) && empuñaduraDeHerramienta Hierro unaHerramienta

empuñaduraMilPesos :: Herramienta -> Bool
empuñaduraMilPesos (Herramienta _ 1000 _) = True

martilloVersatil :: Herramienta -> Bool
martilloVersatil unaHerramienta= ( ( == "Martillo" ) . denominacion $ unaHerramienta  ) && ( empuñaduraDeHerramienta Madera unaHerramienta || empuñaduraDeHerramienta Goma unaHerramienta )

herramientaBuena :: Herramienta -> Bool
herramientaBuena unaHerramienta = empuñaduraMilPesos unaHerramienta || martilloVersatil unaHerramienta

--4) 

comprarHerramienta :: Herramienta -> Plomero -> Plomero
comprarHerramienta unaHerramienta unPlomero
    |puedeComprarHerramienta unPlomero unaHerramienta = unPlomero { herramientas = unaHerramienta : herramientas unPlomero, dinero = dinero unPlomero - precio unaHerramienta }
    |otherwise = unPlomero



-- 5) 
poseerLlaveInglesa :: Herramienta -> Requisito
poseerLlaveInglesa unPlomero = any "Llave Inglesa" ( denominacion . herramientas $ unPlomero)

filtracionAgua = Reparacion {
    descripcion = "Fuga de agua en una cañeria ",
    requisito = poseerLlaveInglesa 
}






