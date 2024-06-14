module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

aproboAlumno :: Number -> Bool
aproboAlumno nota = nota >= 6


data ColorPrimario = Rojo | Amarillo | Azul

recargoPorColor :: ColorPrimario -> Number
recargoPorColor Rojo = 50
recargoPorColor _ = 20



-- La granja parcial 
-- Animales sueltos
laPasoMal :: Animal -> Bool
laPasoMal = any((>30).diasrecuperacion). visitaMedicas
-- laPasoMal animal = (any((>30).diasrecuperacion). visitaMedicas) animal reducida queda como arriba
-- any ((>30).diasrecuperacion) $ visitaMedica animal

data Animal= Animal{
     nombre :: String, 
     tipoanimal :: String,
     peso :: Number,
     edad :: Number,
     estaEnfermo :: Bool,
     visitaMedicas :: [VisitaMedica] 
}

data VisitaMedica = VisitaMedica {
    diasrecuperacion :: Number,
    costo :: Number 
}

nombreFalopa :: Animal -> Bool
nombreFalopa  =  (=='i').last . nombre
-- nombreFalopa animal = ( (=='i').last . nombre) animal formato sin reducir

-- Punto 2:
type Actividad = Animal -> Animal 

engorde :: Number -> Actividad
engorde kilos = modificarPeso  ((min 5.(* 0.5)) kilos)

-- engorde kilos animal = modificarPeso  ((min 5.(* 0.5)) kilos) animal reducir
-- primera opción utilizada antes de hacer "modificarPeso"
-- engorde kilos animal = animal {
--       peso = peso animal +  (min 5.(* 0.5)) kilos
--            (5 'min' (kilos/2))
--             min 5 (kilos/2)
--    }

revisacion :: VisitaMedica -> Actividad
revisacion visita animal 
   | estaEnfermo animal = (engorde 2 animal) {
         -- peso = (peso. engorde 2) animal Utilizamos la función anterior tener en cuenta repeticiones no validas en el parcial
         visitaMedicas = visitaMedicas animal ++ [visita]
         -- = visita: visitasMedicas animal otra opción con cabeza y cola
   } 
   | otherwise = animal

modificarPeso :: Number -> Actividad
modificarPeso kilos animal = animal {
    peso = peso animal + kilos 
}


festejoCumple :: Actividad
festejoCumple animal = modificarPeso (-1) animal {
    edad = edad animal +1
}

chequeoDePeso :: Number -> Actividad
chequeoDePeso kilos animal = animal {
   estaEnfermo = peso animal <= kilos
}

-- Punto 3
type Proceso = [Actividad]   

aplicarProceso :: Proceso -> Animal -> Animal
aplicarProceso actividades animal = foldr ($) animal actividades 

-- aplicar proceso  foldr (     $ ) semilla lista 

-- Opción más pro
-- aplicarProceso :: Animal -> Proceso -> Animal
-- aplicarProceso foldr ($)  

-- Punto 4
-- Funcion auxiliar
mejoraSustentable :: Actividad -> Animal -> Bool
mejoraSustentable actividad animal = (peso.actividad) animal > peso animal && (peso.actividad) animal < ((+3). peso) animal

mejora :: Proceso -> Animal -> Bool
mejora [] _ = True 
mejora (actividad:actividades) animal = 
    mejoraSustentable actividad animal && mejora actividades animal

-- Punto 5
-- a) lazy evaluation, converge, diverge 
-- take3 . filter tieneNombreFalopa
-- b) 
-- 

