{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Library where
import PdePreludat

doble nro = nro *2

data Personaje = Personaje {nombre :: String, experiencia :: Number, fuerza :: Number, elemento :: Elemento} deriving Show

type Elemento = Number -> Number

nivel :: Personaje -> Number
nivel (Personaje _ experiencia _ _ ) = ceiling (experiencia ^ 2/(experiencia + 1))

capacidad :: Personaje -> Number
capacidad (Personaje _ _ fuerza elemento ) = elemento fuerza

espadaOxidada = (1.2*)
katanaFilosa = (10+).(0.9*)
sableLambdico cm = ((1+cm/100)*)
redParadigmatica = sqrt
baculoDuplicador x= x* 2
espadaMaldita = espadaOxidada.sableLambdico 89


ana = Personaje "ana" 20 50 espadaOxidada
julio = Personaje "julio" 15 70 espadaMaldita

type Alquimista = Personaje -> Personaje

aprendiz :: Alquimista
aprendiz personaje = alterarElemento (2*) personaje

alterarElemento :: Elemento -> Alquimista
alterarElemento f personaje = personaje {elemento = f.elemento personaje}

maestro :: Number -> Alquimista
maestro anios personaje = (alterarElemento (coeficienteAntiguedad anios).aprendiz ) personaje

coeficienteAntiguedad 0 = id
coeficienteAntiguedad anios = (*1.1). coeficienteAntiguedad (anios - 1) 

estafador :: Alquimista
estafador personaje = personaje {elemento = id}

inventado :: Alquimista
inventado personaje = personaje {elemento = (\x -> x * 5)}

capacidadesSuperioresA valor personaje alquimistas =  filter (capacidadSuperiorA valor personaje) alquimistas

capacidadSuperiorA :: Number -> Personaje -> Alquimista -> Bool
capacidadSuperiorA valor personaje alquimista =  (>valor).capacidad.alquimista $ personaje

convienenATodos :: [Alquimista] -> Personaje -> Bool
convienenATodos alquimistas personaje = all (capacidadSuperiorA (capacidad personaje) personaje) alquimistas

data Monstruo = Monstruo { especie :: String, resistencia :: Number,  habilidades :: [Habilidad]} deriving Show
type Habilidad = (String, String)
descripcion = fst
tipo = snd

esAgresivo :: Monstruo -> Bool
esAgresivo monstruo = (mayoriaHabilidadesOfensivas.habilidades) monstruo && (not.especieInofensiva.especie) monstruo && ((>0).resistencia)monstruo


mayoriaHabilidadesOfensivas :: [Habilidad] -> Bool
mayoriaHabilidadesOfensivas habilidades = (length.filter(esOfensiva.tipo)) habilidades > div (length habilidades) 2

esOfensiva :: String -> Bool
esOfensiva "magia" = True
esOfensiva "fisica" = True
esOfensiva _ = False

especieInofensiva :: String -> Bool
especieInofensiva especie = elem especie ["animal", "chocobo"]

gana :: Personaje -> Monstruo -> Bool
gana personaje monstruo = capacidad personaje > resistencia monstruo

pelearConTodos :: Personaje -> [Monstruo] -> Personaje
pelearConTodos personaje monstruos = foldl pelear personaje monstruos

pelear :: Personaje -> Monstruo -> Personaje
pelear personaje monstruo | gana personaje monstruo = modificarExperiencia 100 personaje
                          | otherwise = alterarElemento (0.9*) (modificarExperiencia (-50) personaje)


modificarExperiencia :: Number ->Personaje -> Personaje
modificarExperiencia valor personaje = personaje {experiencia = experiencia personaje + valor}

hayInvensible :: Personaje -> Alquimista -> [Monstruo] -> Bool
hayInvensible personaje alquimistra monstruos = not.all (gana (alquimista personaje)) $ monstruos