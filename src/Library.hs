module Library where
import PdePreludat

--Parte A
data Participante = unParticipante {
    nombre :: String,
    trucos :: [ListaTrucos],
    especialidad :: Plato
}deriving (Show, Eq)

type ListaTrucos = Plato -> Plato

data Plato = unPlato {
    dificultad :: Number,
    componentes :: [ListaComponentes]
}deriving (Show, Eq)

type ListaComponentes = (Ingredientes, PesoEnGramos)
type Ingredientes = String
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