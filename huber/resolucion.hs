data Chofer = Chofer {
    nombre       :: String,
    kilometraje  :: Int,
    viajes       :: [Viaje],
    condicion    :: Condicion
}
type Condicion = Viaje -> Bool

data Viaje = Viaje {
    fecha :: (Int,Int,Int),
    cliente :: Cliente,
    costo :: Int
}

data Cliente = Cliente {
    nombreCliente :: String,
    vivienda      :: String
}

--2)
tomaCualquiera :: Condicion
tomaCualquiera _ = True 

tomaCaros :: Condicion
tomaCaros = ( > 200 ) . costo 

tomaNombreLargos :: Int -> Condicion
tomaNombreLargos cantidad = ( > cantidad) . length . nombreCliente . cliente 

noVillas :: String -> Condicion
noVillas villa = ( /= villa ).vivienda . cliente


--3) 

lucas = Cliente {
    nombreCliente = "Lucas",
    vivienda = "Victoria"
}

daniel = Chofer {
    nombre= "Daniel",
    kilometraje = 23500,
    viajes = [ Viaje (20, 4, 2017 ) lucas 150],
    condicion = noVillas "Olivos" 
}

alejandra = Chofer "Alejandra" 180000 [] ( tomaCualquiera )


puedeTomar :: Viaje -> Chofer -> Bool
puedeTomar unViaje unChofer = condicion unChofer $ unViaje

liquidacion unChofer = foldr ( ( + ) . costo) 0 ( viajes unChofer )

viajePosible unViaje unosChoferes = any ( condicion unViaje ) condicion unosChoferes
{--
realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje unViaje unosChoferes --}

