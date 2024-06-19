module HarryPotter where


data Persona = Persona {
  nombrePersona :: String,
  suerte :: Int,
  inteligencia :: Int,
  fuerza :: Int
} deriving (Show, Eq)

data Pocion = Pocion {
  nombrePocion :: String,
  ingredientes :: [Ingrediente]
}

type Efecto = Persona -> Persona

data Ingrediente = Ingrediente {
  nombreIngrediente :: String,
  efectos :: [Efecto]
}

nombresDeIngredientesProhibidos = [
 "sangre de unicornio",
 "veneno de basilisco",
 "patas de cabra",
 "efedrina"]

maximoSegun :: Ord b => (a -> b) -> [a] -> a
maximoSegun _ [ x ] = x
maximoSegun  f ( x : y : xs)
  | f x > f y = maximoSegun f (x:xs)
  | otherwise = maximoSegun f (y:xs)


-- 1)Dada una persona definir las siguientes funciones para cuantificar sus niveles de suerte,
-- inteligencia y fuerza sin repetir código:
-- a)sumaDeNiveles que suma todos sus niveles.
-- b)diferenciaDeNiveles es la diferencia entre el nivel más alto y más bajo.
-- c)nivelesMayoresA n, que indica la cantidad de niveles de la persona que están por encima del valor dado.


--a)
sumaDeNiveles :: Persona ->  Int 
sumaDeNiveles persona = suerte persona + inteligencia persona + fuerza persona   



--b)
diferenciaDeNiveles :: Persona -> Int
diferenciaDeNiveles persona = maximoNivel persona - minimoNivel persona


--FUNCIONES AUXILIARES PARA EL PUNTO B
--Funcion Auxiliar q me de los niveles para usar en el punto b, me permite hacerla mas generica
niveles :: Persona -> [Int]
niveles persona = [suerte persona, inteligencia persona, fuerza persona]

maximoNivel :: Persona -> Int
maximoNivel = maximum . niveles 
--maximoNivel' persona = maximum (niveles persona)
minimoNivel :: Persona -> Int
minimoNivel = minimum . niveles 
--minimoNivel' persona = minimum (niveles persona)

--C)
nivelesMayoresA :: Int -> Persona -> Int
nivelesMayoresA  valor = length . filter (valor <) .  niveles  
--Quiero q me recorra la lista de niveles y me diga cuantos son mayores a un valor q le doy 
--entonces le hago un filter q verifique los mayores a "valor" y a esa lista filtrada le hago un length



--2)Definir la función efectosDePocion que dada una poción devuelve una lista 
--con los efectos de todos sus ingredientes.

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion = concat . map efectos . ingredientes

--efectosDePocion'' pocion  = (concat . map efectos . ingredientes) pocion

--efectosDePocion' =  map efectos . ingredientes 

--Esta funcion me devolveria una lista de listas, efectosDePocion :: Pocion -> [[Efecto]], y yo lo que quiero es una lista sola, entonces uso concat

--Efecto :: Persona -> Persona me devuelve una persona modificada en su fuerza, energia, suerte, etc

--Pocion tiene un lista de ingredientes y a su vez ingrediente tiene una lista de efectos que a su vez es una funcion de persona en persona 

--entonces me debe devolver una lista de persona->persona 


---------------------------------------------

-- 3) Dada una lista de pociones, consultar:
-- a)Los nombres de las pociones hardcore, que son las que tienen al menos 4 efectos.
-- b)La cantidad de pociones prohibidas, que son aquellas que tienen algún ingrediente 
-- cuyo nombre figura en la lista de ingredientes prohibidos.
-- c)Si son todas dulces, lo cual ocurre cuando todas las pociones
-- de la lista tienen algún ingrediente llamado “azúcar”.

--a)
--funcion auxiliar
esPocionHardcore ::Pocion -> Bool
esPocionHardcore = (>=4).length.ingredientes
--pocionesHardcore :: [Pocion] -> [String]
--ingredientesPocionHardcore :: Pocion -> Bool
--ingredientesPocionHardcore = 
pocionesHardcore :: [Pocion] -> [String]
pocionesHardcore = map nombrePocion . filter esPocionHardcore

--verifico si la lista de efectos de una pocion es mayor igual a 4
--despues recorro la lista de pociones y me quedo con el nombre

--b)
cantPocionProhibida :: [Pocion] -> Int
cantPocionProhibida = length. filter esPocionProhibida 
esPocionProhibida :: Pocion-> Bool
esPocionProhibida = any (flip elem nombresDeIngredientesProhibidos . nombreIngrediente ) . ingredientes

--Tengo dos listas y debo recorrerlas y compararlas, esto es complejo, entonces
--Me hago una funcion auxiliar que me lleve a UNA pocion para compararla con los ELEMENTOS de la lista prohibia
--en la funcion auxiliar espocionProhibida, planteo q si algun elemento de la lista coincide con los elementos de la pocion, para eso uso any 
--voy a recibir los ingredientes de la pocion q es una lista, me quedo con nombreIngrediente para preguntarle a elem si coincide con los elementos de la lista
--de ingredientesProhibidos, le puse flip porq el primer parametro q tiene q recibir el any es el del nombre del ingrediente pero me quedo como segundo,
-- esa va a ser la condicion de mi any
--NOTAS, a tener en cuenta que cuando queres poner ==, siempre fue mejor poner elem, tenela presente, y otra cosa
-- siempre q se te complica asi, llevalo a funciones q manejen un solo elemento de esa lista, para usar las funciones auxiliares

--c)c)Si son todas dulces, lo cual ocurre cuando todas las pociones
-- de la lista tienen algún ingrediente llamado “azúcar”.

todasPocionesDulces :: [Pocion] -> Bool
todasPocionesDulces = all pocionDulce

pocionDulce :: Pocion -> Bool
pocionDulce =  elem "azucar" . map nombreIngrediente  . ingredientes
--pocionDulce'= any (("azucar"==) . nombreIngrediente) . ingredientes)

--pocionDulce va a recibir la lista con los nombreIngredientes de la lista ingredientes y va a re


--4)Definir la función tomarPocion que recibe una poción y una persona, y devuelve como quedaría la persona 
-- después de tomar la poción. Cuando una persona toma una poción, se aplican todos los efectos de esta última, en orden.

tomarPocion :: Pocion -> Persona -> Persona
tomarPocion pocion personaInicial = foldl (\persona efecto -> efecto persona) personaInicial  (efectosDePocion  pocion)
--tomarPocion' pocion personaInicial = (foldl (\persona efecto -> efecto persona) personaInicial . efectosDePocion ) pocion

--La funcion lambda, es una funcion reductora, personaInicial es la semilla, efectosDePocion es la lista de pociones
--Explicacion de la funcion lambda: dada una persona y un efecto, le aplico ese efecto (que es a su vez una funcion q va de persona en persona) a la persona


--5)Definir la función esAntidotoDe que recibe dos pociones y 
--  una persona, y dice si tomar la segunda poción revierte los
--  cambios que se producen en la persona al tomar la primera.
esAntidotoDe :: Pocion -> Pocion -> Persona -> Bool
esAntidotoDe pocion antidoto persona = ( (==persona) .tomarPocion antidoto . tomarPocion pocion) persona

--necesito aplicar la pocion1 a la persona, puede ser un foldl
-- si al aplicar la segunda pocion, la persona vuelve a los valores de la primera, entonces funciona
--Como ya tengo una funcion que aplica el foldl, q es tomarPocion, entonces la uso
-- al tomar la pocion me retorna una nueva persona, que va a tomar el antidoto, y si esa persona es igual a la del principio, entonces es verdadero
----------------------------------------------------------------
--6)Definir la función personaMasAfectada que recibe una poción, una función cuantificadora 
-- (es decir, una función que dada una persona retorna un número)
-- y una lista de personas, y devuelve a la persona de la lista que hace máxima el valor del cuantificador. 
-- Mostrar un ejemplo de uso utilizando los cuantificadores definidos en el punto 1.

personaMasAfectada :: Pocion -> (Persona -> Int) -> [Persona] -> Persona
personaMasAfectada pocion f  = maximoSegun (f . tomarPocion pocion)

ejemploDeUso = personaMasAfectada (Pocion "Placebo" [] ) (nivelesMayoresA 10)[]


-- data Persona = Persona {
--   nombrePersona :: String,
--   suerte :: Int,
--   inteligencia :: Int,
--   fuerza :: Int
-- } deriving (Show, Eq)

-- data Pocion = Pocion {
--   nombrePocion :: String,
--   ingredientes :: [Ingrediente]
-- }

-- type Efecto = Persona -> Persona

-- data Ingrediente = Ingrediente {
--   nombreIngrediente :: String,
--   efectos :: [Efecto]
-- }