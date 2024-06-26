module MiniGolfito where


-- Modelo inicial

data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Float,
  precisionJugador :: Float
} deriving (Eq, Show)

-- Jugadores de ejemplo

bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Float,
  precision :: Float,
  altura :: Float
} deriving (Eq, Show)

type Puntos = Float

-- Funciones útiles

between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--1)a)Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro
--que se compone por velocidad, precisión y altura.
--OSEA ES UNA FUNCION

--paloMadera :: Palos

--I)El putter genera un tiro con velocidad igual a 10, 
--el doble de la precisión recibida y altura 0.
putter :: Habilidad -> Tiro 
putter habilidad = UnTiro 10 (2* precisionJugador habilidad) 0


--II) La madera genera uno de velocidad igual a 100, 
--altura igual a 5 y la mitad de la precisión
madera :: Habilidad -> Tiro
madera habilidad = UnTiro 100 (precisionJugador habilidad / 2) 5

--III)Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de
-- velocidad igual a la fuerza multiplicada por n, la precisión dividida por n y una altura de n-3
-- (con mínimo 0). Modelarlos de la forma más genérica posible
hierro :: Float -> Habilidad -> Tiro
hierro n habilidad = UnTiro (fuerzaJugador habilidad * n) (precisionJugador habilidad / n) ((n/0 - 3) `max` 0)
type Palo = Habilidad -> Tiro
--1)B)  b. Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.
palosJuego = [putter, madera] ++ map hierro [1..10]

-------------------------------------
-- 2) Definir la función golpe que dados una persona y un palo, 
-- obtiene el tiro resultante de usar ese palo con
-- las habilidades de la persona.

golpe ::Jugador -> Palo -> Tiro
golpe jugador palo = (palo . habilidad) jugador --No se puede hacer point free en esta
--golpe' :: Palo -> Jugador ->  Tiro
--golpe' palo   = palo . habilidad 


--necesito las habilidades del jugador, la funcion me 
--va a devolver el tiro modificado, usando la info del palo y las habilidades
-------------------------------------------


--3) 3. Loque nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder
--superarlo, cómo se ve afectado dicho tiro por el obstáculo. En principio necesitamos representar los
-- siguientes obstáculos:
--a)Untúnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo,
--independientemente de la velocidad del tiro. Al salir del túnel la velocidad del tiro se duplica, la
--precisión pasa a ser 100 y la altura 0.

type Obstaculo = Tiro -> Tiro



--Necesito una funcion con los efectos q produce el obstaculo
--Otra funcion que me de el ok de si pasa o no el tiro el obstaculo
--Otra funcion con guardas en la cual pueda poner el tipo de obstaculo y aplico las funciones anteriores 

--Funcion q me dice si paso o no el obstaculo del tunel
pasaraTunel :: Tiro -> Bool
pasaraTunel tiro = 90 < precision tiro && 0 == altura tiro
--pasaraTunel tiro = (90<=).precision tiro && (0==). altura tiro 

efectoTunel :: Tiro -> Tiro
efectoTunel tiro = UnTiro 100 0 (velocidad tiro * 2)

tunel = pasaObstaculo pasaraTunel efectoTunel

noPasoObstaculo = UnTiro 0 0 0


----------------------
--b)Unalaguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 
--metros. Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una
--altura equivalente a la altura original dividida por el largo de la laguna.




pasaraLaguna :: Tiro -> Bool
pasaraLaguna tiro = 81 < velocidad tiro && (between 1 5 . altura) tiro
efectoLaguna :: Float -> Tiro -> Tiro
efectoLaguna mtsLaguna tiro  = UnTiro (precision tiro)  (altura tiro / mtsLaguna) (velocidad tiro)



--Otra Solucion mas linda
-- efectoLaguna mtsLaguna tiro  = UnTiro {
--   precision = precision tiro,
--   altura = altura tiro/ mtsLaguna,
--   velocidad = velocidad tiro
--   }

laguna :: Float -> Tiro -> Tiro
laguna valor = pasaObstaculo pasaraLaguna (efectoLaguna valor)
---------------------------------------------------------
--c) Unhoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una
--  precisión mayor a 95. Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes
--  en 0.

pasaraHoyo :: Tiro -> Bool
pasaraHoyo tiro = precision tiro > 96 && 0 == altura tiro  && (between 5 20 . velocidad) tiro

efectoHoyo :: Tiro -> Tiro
efectoHoyo _ = noPasoObstaculo
hoyo:: Tiro -> Tiro
hoyo = pasaObstaculo pasaraHoyo  efectoHoyo 
--------------
--Esta funcion auxiliar va a validar si se pasa el obstaculo y me va a arrojar como quedo el tiro modificado
-- LA voy a usar en la funcion de cada OBSTACULO
--pasaObstaculo :: Tiro -> Bool -> Obstaculo -> Tiro -> Tiro (nose porq, pasandole estos parametros no me agarra)

pasaObstaculo pasara efecto tiro
  | pasara tiro = efecto tiro
  | otherwise = noPasoObstaculo

tiroPrueba = UnTiro 82 82 4


---------------------------------------------------------
--PUNTO 4
---------------------------------------------------------

--A)Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven
-- para superarlo.

--palosUtiles :: Obstaculo -> Jugador -> [Palo] 

palosUtiles obstaculo jugador palos = filter (paloSirveParaSuperar obstaculo jugador ) palos

--Quiero filtrar, pero me surge el problema de que filter necesita una respuesta booleana
--ME ARMO UNA FUNCION AUXILIAR QUE ME RETORNE UNA RESPUESTA BOOLEANA
paloSirveParaSuperar :: Obstaculo -> Jugador ->  Palo -> Bool
paloSirveParaSuperar obstaculo jugador palo = (( noPasoObstaculo /= ).obstaculo . (golpe jugador)) palo
--tengo la funcion golpe que dado un jugador y un palo me devuelve un tiro
--------------------------------------------
--B)Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden
--  superar.
--  Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles
--  con rampita seguidos de un hoyo, el resultado sería 2 ya que la velocidad al salir del segundo
--  túnel es de 40, por ende no supera el hoyo.
--  BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función
--  takeWhile :: (a-> Bool)-> [a]-> [a]quepodríaser de utilidad.



-- data Tiro = UnTiro {
--   velocidad :: Float,
--   precision :: Float,
--   altura :: Float
-- } deriving (Eq, Show)

-- data Jugador = UnJugador {
--   nombre :: String,
--   padre :: String,
--   habilidad :: Habilidad
-- } deriving (Eq, Show)