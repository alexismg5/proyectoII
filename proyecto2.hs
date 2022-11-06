data Carrera = Matematica | Fisica | Computacion | Astronomia deriving (Show, Eq)

--Defin ́ı la siguiente funci ́on, usando pattern matching:
-- titulo :: Carrera -> String
--que devuelve el nombre completo de la carrera en forma de string.
--Por ejemplo, para el
--constructor Matematica, debe devolver ”Licenciatura en Matem ́atica”.

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en matematica"
titulo Fisica = "Licenciatura en fisica"
titulo Computacion = "Licenciatura en computacion"
titulo Astronomia = "Licenciatura en astronomia"


data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si deriving (Show, Eq, Ord)

cifradoAmericano :: NotaBasica -> Char

cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'


--Ejercicio 2
minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

minimoElemento' :: (Ord a, Bounded a) => [a] -> a
minimoElemento' [] = minBound
minimoElemento' (x:xs) = min x (minimoElemento' xs)

--Ingreso es un sinonimo de tipo
type Ingreso = Int

-- Cargo Area son tipos enumerados
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Show, Eq)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Show, Eq)

--Persona es un tipo algebraico
data Persona = Decano
             | Docente Cargo
             | NoDocente Area
             | Estudiante Carrera Ingreso deriving (Show, Eq)

--b) El tipo de Docente es Cargo

cuantos_doc:: [Persona] ->Cargo -> Int
cuantos_doc xs c = length(filter(== Docente c) xs)


-- Ejercicio 5
data Alteracion = Bemol | Sostenido | Natural
data NotaMusical = Nota NotaBasica Alteracion

instance Eq NotaMusical where
    (Nota a b) == (Nota c d) = sonidoCromatico(Nota a b) == sonidoCromatico(Nota c d) 

instance Ord NotaMusical where
    (Nota a b) <= (Nota c d) = sonidoCromatico(Nota a b) <= sonidoCromatico(Nota c d)

sonido :: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota nb a)  =  case a of
  Sostenido -> (sonido nb + 1)
  Bemol -> (sonido nb - 1)
  Natural -> sonido nb

-- Ejercicio 6
dividir :: Int -> Int -> Maybe Int
dividir x 0 = Nothing
dividir x y = Just (x `div` y)

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento xs = Just (xs!!0)

-- Ejercicio 7

data Cola = VaciaC | Encolada Persona Cola deriving (Show,Eq)

atender:: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada a c) = Just c

encolar:: Persona -> Cola -> Cola
encolar a VaciaC = Encolada a VaciaC
encolar a (Encolada c cs)= (Encolada c (encolar a cs))

final:: a -> [a] -> [a]
final n [] = [n]
final n (x:xs) = (x:(final n xs))

busca :: Cola -> Cargo -> Maybe Persona
busca (VaciaC) c = Nothing
busca  (Encolada a as) c | a== (Docente c) = Just a | otherwise = (busca as c)

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving Show
type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

la_long:: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b (la)) = 2 + (la_long (la))

la_concat:: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia la2 = la2
la_concat la (Vacia) = la
la_concat (Nodo a b (la)) la2 = (Nodo a b (la_concat la la2))

la_agregar:: ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar la a b = (Nodo a b(la))

la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo a b (la)) = (a,b):(la_pares la)

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia c = Nothing
la_busca (Nodo a b (la)) c | a==c = Just b | otherwise = (la_busca la c)

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar x Vacia = Vacia
la_borrar x (Nodo a b (la)) | x==a = (la_borrar x la) | otherwise = (Nodo a b (la_borrar x la))