module ParcialesDeLocura where
import MiniGolfitoResuelto (maximoSegun)
import LambdaProp (Depto(barrio))
data Investigador = Investigador{
    nombre:: String,
    cordura :: Int,
    items :: [Item],
    sucesosEvitados :: [String]
}deriving (Show, Eq)

data Item = Item {
    nombreItem:: String,
    valor :: Int
}deriving (Show, Eq)

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b 
    |f a > f b = a
    |otherwise = b

deltaSegun ponderacion transformacion valor=
    abs((ponderacion.transformacion)valor - ponderacion valor)

---------------
--1)a)
enloquece :: Investigador -> Int -> Investigador
enloquece investigador locura = investigador {cordura = cordura investigador - locura}
--1)b)
enloqueceSegunItem :: Investigador -> Item -> Investigador
enloqueceSegunItem investigador item = investigador {cordura= cordura (enloquece investigador (valor item)), items = item : items investigador }
    --Quiero incorporar un item a la lista de items, se me ocurre un map, tal vez no es lo mas eficiente, mal, es agregarle el elemento nuevo como cabeza con (:)
-----------------------------------
--2)

--tieneItemConNombreIndicado :: Item -> [Investigador] -> Bool
--tieneItemConNombreIndicado item investigadores = (tieneEseItem item ). (filter items investigadores)




---necesito la lista de items de la lista de investigadores



--Toma la lista de items de un investigador
tieneEseItem :: Item -> Investigador -> Bool
tieneEseItem item investigador = elem item (items investigador)

itemsDePrueba1 =[Item "perros" 5, Item "gatos" 6]
itemsDePrueba2 =[Item "mandril" 12, Item "conejo" 8]
itemsDePrueba3 =[Item "pato" 1, Item "elefante" 9]

investigador1= Investigador {
    nombre="Julian",
    cordura = 8,
    items = itemsDePrueba1,
    sucesosEvitados =[]
}


investigador2= Investigador {
    nombre="Juliana",
    cordura = 10,
    items = itemsDePrueba2,
    sucesosEvitados =[]
}

investigador3= Investigador {
    nombre="Pedro",
    cordura = 36,
    items = itemsDePrueba3,
    sucesosEvitados =[]
}
perro = Item "perros" 6

investigadore =[investigador1, investigador2, investigador3]

--Chris, filter analiza si se cumple