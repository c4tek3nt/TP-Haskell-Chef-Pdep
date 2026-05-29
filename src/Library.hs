module Library where
import PdePreludat

--Parte A
data Participante = UnParticipante {
    nombre :: String,
    trucos :: [ListaTrucos],
    especialidad :: Plato
}deriving (Show, Eq)

type ListaTrucos = Plato -> Plato

data Plato = UnPlato {
    dificultad :: Number,
    componentes :: [ListaComponentes]
}deriving (Show, Eq)

type ListaComponentes = (Ingrediente, PesoEnGramos)
type Ingrediente = String
type PesoEnGramos = Number

endulzar :: Number -> Plato -> Plato
endulzar gramos plato = agregarComponente ("azucar", gramos) plato

agregarComponente :: ListaComponentes -> Plato -> Plato
agregarComponente componente plato = plato {componentes = componente : componentes plato}

salar :: Number -> Plato -> Plato
salar gramos plato = agregarComponente ("sal", gramos) plato

darSabor :: Number -> Number -> Plato -> Plato
darSabor gramosSal gramosAzucar plato = endulzar gramosAzucar (salar gramosSal plato)

duplicarPorcion :: Plato -> Plato
duplicarPorcion plato = plato {componentes = map duplicarCantidad (componentes plato)}

duplicarCantidad :: ListaComponentes -> ListaComponentes
duplicarCantidad (ingrediente, gramos) = (ingrediente, gramos * 2)

simplificar :: Plato -> Plato
simplificar plato
    | esComplejo plato = plato {dificultad = 5, componentes = filter (not . menosDe10Gramos) (componentes plato)}
    | otherwise = plato

menosDe10Gramos :: ListaComponentes -> Bool
menosDe10Gramos (_, gramos) = gramos < 10

esVegano :: Plato -> Bool
esVegano plato = not (tieneIngrediente "carne" plato || tieneIngrediente "huevo" plato || tieneIngrediente "leche" plato)

tieneIngrediente :: Ingrediente -> Plato -> Bool
tieneIngrediente ingrediente plato = any ((== ingrediente) . fst) (componentes plato)

esSinTacc :: Plato -> Bool
esSinTacc plato = not (tieneIngrediente "harina" plato)

esComplejo :: Plato -> Bool
esComplejo plato = length (componentes plato) > 5 && dificultad plato > 7

noAptoHipertension :: Plato -> Bool
noAptoHipertension plato = gramosIngrediente "sal" plato > 2

gramosIngrediente :: Ingrediente -> Plato -> Number
gramosIngrediente ingrediente plato = sum (map snd (filter ((== ingrediente) . fst) (componentes plato)))

--Parte B
platoPepe :: Plato
platoPepe = UnPlato {
    dificultad = 8,
    componentes = [("carne", 100), ("harina", 50), ("sal", 1), ("tomate", 20), ("queso", 30), ("pimienta", 5)]
}

pepeRonccino :: Participante
pepeRonccino = UnParticipante {
    nombre = "Pepe Ronccino",
    trucos = [darSabor 2 5, simplificar, duplicarPorcion],
    especialidad = platoPepe
}

--Parte C
cocinar :: Participante -> Plato
cocinar participante = aplicarTrucos (trucos participante) (especialidad participante)

aplicarTrucos :: [Truco] -> Plato -> Plato
aplicarTrucos listaTrucos plato = foldl aplicarTruco plato listaTrucos

aplicarTruco :: Plato -> Truco -> Plato
aplicarTruco plato truco = truco plato

pesoTotal :: Plato -> Number
pesoTotal plato = sum (map snd (componentes plato))

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = dificultad unPlato > dificultad otroPlato && pesoTotal unPlato < pesoTotal otroPlato

participanteEstrella :: [Participante] -> Participante
participanteEstrella participantes =
    foldl1 mejorParticipante participantes

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante unParticipante otroParticipante
    | cocinar unParticipante `esMejorQue` cocinar otroParticipante = unParticipante
    | otherwise = otroParticipante