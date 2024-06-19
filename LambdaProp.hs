module LambdaProp where
--import qualified Data.ByteString as criterio

type Barrio = String
type Mail = String
type Requisito = Depto -> Bool
type Busqueda = [Requisito]

data Depto = Depto {
 ambientes :: Int,
 superficie :: Int,
 precio :: Int,
 barrio :: Barrio
} deriving (Show, Eq)

data Persona = Persona {
   mail :: Mail,
   busquedas :: [Busqueda]
}
ordenarSegun ::(a -> a -> Bool) -> [a] ->[a]
ordenarSegun _ [] = [] --me dice q recibe como segundo parametro una lista vacia y devuelve otra lista vacia
ordenarSegun criterio (x:xs) = --criterio es un parametro, (x:xs) es una lista q se recibe por parametro con una cabeza y una cola
 (ordenarSegun criterio . filter (not . criterio x)) xs ++
 [x] ++
 (ordenarSegun criterio . filter (criterio x)) xs
between :: Ord a => a -> a -> a -> Bool
between cotaInferior cotaSuperior valor =
 valor <= cotaSuperior && valor >= cotaInferior

deptosDeEjemplo = [
 Depto 3 80 7500 "Palermo",
 Depto 1 45 3500 "Villa Urquiza",
 Depto 2 50 5000 "Palermo",
 Depto 1 45 5500 "Recoleta"]

ejemploString = ["pepe", "Juanita", "Antonito","Marcoantonio" ]

-- Definir las funciones mayor y menor que reciban una función y dos valores, y retorna true si 
-- el resultado de evaluar esa función sobre el primer valor es mayor o menor que el resultado de 
-- evaluarlo sobre el segundo valor respectivamente.



mayor :: Ord b => (a -> b) -> a -> a -> Bool
mayor f x y = f x > f y

menor :: Ord b => (a -> b) -> a -> a -> Bool
menor f x y = f x < f y
-- Mostrar un ejemplo de cómo se usaría una de estas funciones para ordenar 
-- una lista de strings en base a su longitud usando ordenarSegun.

ejemploDeOrdenarSegun = ordenarSegun (mayor length) ejemploString 

--2)a)ubicadoEn que dada una lista de barrios que le interesan al usuario,
-- retorne verdadero si el departamento se encuentra en alguno de los barrios de la lista.
ubicadoEn :: [Barrio] -> Requisito
ubicadoEn lista = (`elem` lista ) . barrio --elem me dice si algun elemento de la lista coincide con el barrio de mi depto, aplicando de manera infija elem para q reciba primero la lista y luego composicion para que reciba lo que me da la funcion barrio de mi data depto
--ubicadoEn' lista departamento= any (== barrio departamento) lista --Esta fue mi idea

--2)b)cumpleRango que a partir de una función y dos números, indique si
--el valor retornado por la función al ser aplicada con el departamento se encuentra entre los dos valores indicados.

cumpleRango::Ord a=> (Depto -> a) -> a -> a -> (Depto -> Bool)
cumpleRango f valor1 valor2 =  between valor1 valor2 . f
--cumpleRango' f valor1 valor2 departamento = (between valor1 valor2 . f) departamento --f es la funcion precio aplicada con el depto

--precio Departamento es una funcion que me devuelve un int cumpliria con (Depto->a)
--Valor1 y Valor2 son dos valores que tambien tendrian q ser del tipo int
--Between es una funcion que necesita una cotaSuperior una cotaInferior y un valor 
--El valor es la funcion precio aplicada parcialmente con un Depto
--El problema esta en que es el tercer parametro en Between mientras que en mi funcion seria el primero
--Si uso flip no me cambia en nada 

--3)a)Definir la función cumpleBusqueda que se cumple si todos los requisitos de una búsqueda se verifican para un departamento dado.
cumpleBusqueda :: Depto -> Busqueda -> Bool
cumpleBusqueda depto = all (\requisito -> requisito depto)  --cada requisito va a verificar con el depto 

-- all recibe una funcion y una lista, verifica que todos los elementos de la lista cumplen con la condicion de la funcion
-- busqueda = [Requisito], Requisito = Depto->Bool, entonces busqueda es una lista de funciones q van de Depto->Bool
--Necesito hacer una funcion q me verifique

   --Tengo una lista de requisitos, q cada requisito debe coincidir con cada item del depto
   --osea los requisitos van a ser un Depto (seria mi depto ideal)
   --osea podria recibir dos deptos, uno seria el ideal y el otro el que hay, ahora los deptos son equiparables

--3)b) Definir la función buscar que a partir de una búsqueda, un criterio de ordenamiento y una lista de 
-- departamentos retorne todos aquellos que cumplen con la búsqueda ordenados en base al criterio recibido

buscar :: Busqueda -> (Depto -> Depto -> Bool) -> [Depto] -> [Depto]

buscar busqueda criterio = ordenarSegun criterio . filter (`cumpleBusqueda` busqueda)

--Filter me va a dar una lista de departamentos filtradas por el cumpleBusqueda
--Esa lista de filter la voy a ordenar segun criterio


--3)c) Mostrar un ejemplo de uso de buscar para obtener los departamentos de ejemplo, ordenado por mayor superficie, que cumplan con:
-- Encontrarse en Recoleta o Palermo
-- Ser de 1 o 2 ambientes
-- Alquilarse a menos de $6000 por mes

ejemploDeBuscar = buscar [ ubicadoEn ["Palermo", "Recoleta"], cumpleRango ambientes 1 2 , cumpleRango precio 0 6000] (mayor superficie) deptosDeEjemplo

--4)Definir la función mailsDePersonasInteresadas que a partir de un departamento y una lista de personas retorne 
--los mails de las personas que tienen alguna búsqueda que se cumpla para el departamento dado.

mailsDePersonasInteresadas :: Depto -> ([Persona] -> [Mail])
mailsDePersonasInteresadas departamento = map mail. filter (any (cumpleBusqueda departamento) . busquedas) 
--CumpleBusqueda va a recibir departamento y una busqueda, esa busqueda es una lista de requisitos y un requisito es una funcion de (depto->Bool)
--como busquedas es una funcion de persona que me retorna una lista, entonces le aplico el any (ya q busquedas es un nuevo conjunto)
--Como quiero chequear si algunos de esos elementos de busqueda cunmple con los de departamento, uso any y a su vez, cuando cumple, lo filtro con filter y me da una nueva lista
--a esa lista se la mando a map y la funcion a aplicar es mail


-- data Persona = Persona {
--    mail :: Mail,
--    busquedas :: [Busqueda]
-- }