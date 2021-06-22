module Library where
import PdePreludat

data Equipo = Equipo {
    jugadores :: [Jugador],
    entrenador :: Entrenador,
    pais :: String
} deriving (Show, Eq)

data Jugador = Jugador {
    nombreJugador :: String,
    paisClub :: String,
    cotizacion :: Number
} deriving (Show, Eq)

data Entrenador = Entrenador {
    nombreEntrenador :: String,
    aniosExp :: Number
} deriving (Show, Eq)

doble x = x +x 

argentina = Equipo [ (Jugador "messi" "espania" 200000000), (Jugador "aguero" "inglaterra" 70000000), (Jugador "di maria" "francia" 60000000), (Jugador "montiel" "argentina" 12000000),( Jugador "paredes" "francia" 40000000) ] ( Entrenador "scaloni" 2) "argentina"

brasil = Equipo [ (Jugador "neymar" "francia" 150000000),(Jugador "jesus" "inglaterra" 90000000),(Jugador "militao" "espania" 40000000),(Jugador "dani alves" "brasil" 20000000) ] (Entrenador "tite" 22) "brasil"

uruguay = Equipo [ (Jugador "suarez" "espania" 60000000),(Jugador "cavani" "inglaterra" 70000000),(Jugador "sanchez" "francia" 5000000),(Jugador "perez" "italia" 25000000 ) ] (Entrenador "tabarez" 35) "uruguay"

----------------------------------
----------- EQUIPOS --------------
----------------------------------


-- Obtener la cantidad de jugadores que juegan fuera de su país.

--juegaFueraDelPais :: String -> Jugador -> Bool
--juegaFueraDelPais pais jugador = pais /= paisClub jugador

jugadoresFueraDeSuPais :: Equipo -> Number
jugadoresFueraDeSuPais equipo = cantidadJugadores equipo - cantidadJugadoresEnPais (pais equipo) equipo


cantidadJugadores = length.jugadores
--jugadoresFueraDeSuPais equipo = length ( filter ((pais equipo==).paisClub ) ( jugadores equipo ) )

-- Calcular la suma de la cotizacion de sus jugadores

cotizacionEquipo :: Equipo -> Number
--cotizacionEquipo equipo = sum ( map cotizacion ( jugadores equipo ) )
--cotizacionEquipo equipo = (sum.map cotizacion.jugadores) equipo 
cotizacionEquipo  = sum.map cotizacion.jugadores 

-- Averiguar si es marquetinero, que es cuando su entrenador tiene más de 20 años de experiencia. 

esMarquetinero :: Equipo -> Bool
esMarquetinero equipo = (aniosExp (entrenador equipo) ) > 20

-- Cambiar de entrenador

cambiarEntrenador :: Equipo -> Entrenador -> Equipo
cambiarEntrenador equipo entrenador = equipo { entrenador = entrenador }

-- Agregar un jugador al equipo

agregarJugador :: Equipo -> Jugador -> Equipo
agregarJugador equipo jugador = equipo { jugadores = jugador:jugadores equipo}

-- Quitar al último jugador en agregarse al equipo

quitarUltimoJugador :: Equipo -> Equipo
quitarUltimoJugador equipo = equipo { jugadores = tail (jugadores equipo) }


----------------------------------
----------- PARTIDOS -------------
----------------------------------

-- Gana el de mayor cotización total
equipoConMayorCotizacion :: Equipo -> Equipo -> Equipo
equipoConMayorCotizacion = ganadorDelPartido cotizacionEquipo
--equipoConMayorCotizacion equipo1 equipo2
--    | cotizacionEquipo equipo1 > cotizacionEquipo equipo2 = equipo1
--    | otherwise = equipo2

-- Gana el que tiene más jugadores que juegan en un país dado

cantidadJugadoresEnPais :: String -> Equipo ->  Number
cantidadJugadoresEnPais pais (Equipo jugadores _ _)  = length ( filter ((pais==).paisClub) jugadores )

masJugadoresEnPais :: String -> Equipo -> Equipo -> Equipo
--masJugadoresEnPais pais equipo1 equipo2
--    | (cantidadJugadoresEnPais equipo1 pais) > (cantidadJugadoresEnPais equipo2 pais) = equipo1
--    | otherwise = equipo2
masJugadoresEnPais pais e1 e2 = ganadorDelPartido (cantidadJugadoresEnPais pais) e1 e2

-- Gana el que tiene al entrenador más experimentado

entrenadorMasExperimentado :: Equipo -> Equipo -> Equipo
entrenadorMasExperimentado e1 e2 = ganadorDelPartido (aniosExp.entrenador) e1 e2
--entrenadorMasExperimentado equipo1 equipo2
--    | ( aniosExp ( entrenador equipo1 ) ) > ( aniosExp ( entrenador equipo2 ) ) = equipo1
--    | otherwise = equipo2

-- Inventar una nueva forma de ganar un partido
-- Gana el que tiene mejor promedio de cotizacion

promedioDeCotizacion :: Equipo -> Number
promedioDeCotizacion equipo = sum ( map cotizacion (jugadores equipo) ) / length ( jugadores equipo)

mejorPromedioDeCotizacion :: Equipo -> Equipo -> Equipo
mejorPromedioDeCotizacion e1 e2 = ganadorDelPartido promedioDeCotizacion e1 e2
--mejorPromedioDeCotizacion equipo1 equipo2
--    | promedioDeCotizacion equipo1 > promedioDeCotizacion equipo2 = equipo1
--    | otherwise = equipo2

-- Funcion de orden superior
--ganadorDelPartido :: ( Equipo -> Number) -> Equipo -> Equipo -> Equipo
ganadorDelPartido :: Ord a => ( Equipo -> a) -> Equipo -> Equipo -> Equipo
ganadorDelPartido f equipo1 equipo2  
    | f equipo1 > f equipo2 = equipo1
    | otherwise = equipo2

----------------------------------
----------- EL TORNEO ------------
----------------------------------

type Estrategia = Equipo -> Equipo -> Equipo 
-- Teniendo una lista de equipos, juega el primero contra el segundo, el que gana juega contra el que sigue y así sucesivamente.
-- El que gana el último partido es el ganador del torneo.

potrero :: [Equipo] -> ( Equipo -> Equipo -> Equipo ) -> Equipo
--potrero (equipo:equipos) estrategia = foldl estrategia equipo equipos
potrero equipos estrategia = foldl1 estrategia equipos

-- Hay un equipo del que se sospecha que cuenta con "ayuda". En caso que le gane a todos los demás equipos participantes,
-- se considera campeón. En caso contrario, resulta campeón el primero que le gane. 

listaSinUno :: [Equipo] -> Equipo -> [Equipo]
listaSinUno equipos candidato = filter ( /= candidato )  equipos

leGanaElPartido ::  ( Equipo -> Equipo -> Equipo ) -> Equipo -> Equipo  -> Bool
leGanaElPartido estrategia equipo1 equipo2 = estrategia equipo1 equipo2 == equipo1

leGanaAlCandidato :: Estrategia -> Equipo -> [Equipo]  -> Equipo
leGanaAlCandidato   estrategia  candidato equipos = head ( filter ( not.leGanaElPartido estrategia candidato ) equipos ) 

candidato :: ( Equipo -> Equipo -> Equipo )  -> Equipo -> [Equipo] ->  Equipo
candidato estrategia  ayudado equipos 
    | leGanaATodos estrategia  equipos ayudado = ayudado
    | otherwise =  leGanaAlCandidato estrategia ayudado equipos

leGanaATodos :: Estrategia ->  [Equipo] -> Equipo -> Bool
leGanaATodos estrategia equipos equipo= all ( leGanaElPartido estrategia equipo )  (listaSinUno equipos equipo)


-- No se sabe de antemano quién, pero si hubiera algún equipo que le gana a todos los restantes, sale campeón.
-- En caso contrario el campeón es cualquiera de los equipos participantes. 

invicto ::  Estrategia -> [Equipo] -> Equipo
invicto estrategia equipos 
    | any (leGanaATodos estrategia equipos) equipos = head (filter (leGanaATodos estrategia equipos) equipos )
    | otherwise = head equipos


-- Estan las rondas típicas: final, semifinal, cuartos, etc. Juegan siempre de a dos y el que pierde queda eliminado.
-- (preveer qué sucede si la cantidad de equipos no es exactamente potencia de 2) 

-- cantidadValida :: [Equipo] -> Bool
-- cantidadValida equipos = ( length equipos )

--eliminacionDirecta :: Estrategia -> [Equipo] -> Equipo

eliminacionDirecta estrategia [equipo] = equipo
eliminacionDirecta estrategia (local:(visitante:[])) = estrategia local visitante
eliminacionDirecta estrategia equipos =  estrategia (eliminacionDirecta estrategia (primerMitad equipos))  (eliminacionDirecta estrategia (segundaMitad equipos) )

primerMitad lista = take ( div (length lista)  2 )  lista
segundaMitad lista = drop ( div (length lista) 2 )  lista




----------------------------------
----------- PREGUNTAS ------------
----------------------------------

-- 1)
-- Averiguar que pasaría si todos los equipos participantes pierden al ultimo jugador en llegar.
-- ¿puede cambiar el ganador de un torneo? en caso afirmativo, mostrar un ejemplo donde suceda.
-- En caso contrario explicar por qué no.

-- Respuesta:

-- CASO AFIRMATIVO
-- Sí, puede cambiar el ganandor de un torneo ya que si, por ejemplo, mi estrategia es "Cuántos jugadores juegan en Italia" 
-- en el siguiente ejemplo vemos cómo perjudica a la selección de uruguay.

-- Primero lo evaluamos normal y sale campeón Uruguay:
-- potrero [argentina,uruguay,brasil] (masJugadoresEnPais "italia")

-- Segundo le quitamos el último jugador a Uruguay y sale campeón Brasil:
-- potrero [argentina,( quitarUltimoJugador uruguay ),brasil] (masJugadoresEnPais "italia")

-- CASO NEGATIVO
-- En este ejemplo que vamos a mostrar no tendrá efecto en el campeón el hecho de quitar un jugador.
-- Evaluamos un nuevo torneo pero con la estrategia del entrenador más experimentado.

-- Primero se evalúa normal y sale campeón Uruguay por la experiencia del maestro Tabarez:
-- potrero [argentina, uruguay ,brasil] entrenadorMasExperimentado

-- Y ahora probamos sacando el último jugador y no tiene impacto, nuevamente sale Uruguay por el mismo motivo:
-- potrero [argentina,( quitarUltimoJugador uruguay ),brasil] entrenadorMasExperimentado

-- 2)
-- Justificar. ¿que pasaría si un equipo tiene infinitos jugadores?

-- Respuesta:

-- En el caso que se requiera utilizar el elemento del final de la lista ésto quedará en un loop infinito.
-- De no requerir recorrerla en su totalidad se podrá trabajar normalmente con esa lista,
-- por ejemplo un take 'n' o cualquier funcion que tomo un subconjunto finito de esa lista, 
-- gracias a la evaluación diferida.
-- si hiciera un map, ( u otras similares) obtendrio un nuevo equipo, con tambien una lista infinita de jugadores.


