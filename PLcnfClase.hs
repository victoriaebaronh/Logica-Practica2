module PLcnfClase (PL, PLvar(..), Literal(..)
        ,Head(..), Body(..)
        ,ClausulaHorn, ClausulaHornVar(..)
        ,cH1, cH2, cH3, cH4
        ,FormulaHorn, FormulaHornVar(..)
        ,fH2,fH1
        ,CNF(..), Clausula(..)
        ,satisfaceA, sigma0
        ,sonLitComplentarios
        ,esClausulaValida)
-- Propositional logic. Sintaxis 
--mcb
where
-- import Data.Set as S 
-- import Data.List as L
--
-- ARREGLO provisional para la ausencia del modulo Variables:
-- import Variables (Variable) -- Variables y Literares (que usaremos despues)
type Variable=String  -- ARREGLO provisional para la ausencia del modulo Variables:
--
--Formulas de la PL -------------------------------------------------
--Un tipo de datos, PLvar, para formulas de la PL:
data PLvar v = -- v es un parametro para el tipo de variables
        -- Casos base:
          Bot                       -- Constructor para bottom
        | Top                       -- Constructor para top
        | Var v                     -- Constructor de variables
        -- Casos recursivos:
        | Imp (PLvar v) (PLvar v)   -- Constructor de implicaciones
        | Dis (PLvar v) (PLvar v)   -- Constructor de disyunciones
        | Con (PLvar v) (PLvar v)   -- Constructor de conjunciones
        | Neg (PLvar v)             -- Constructor de negaciones
        | Syss (PLvar v) (PLvar v)
        deriving (Eq,Show)
--
type PL= PLvar Variable  -- PL con tipo de variables Variable (ver arriba). Variable esta definido como String.
--

-- UN INTENTO, en 2 minutos para definir en Haskell la semántica de la PL definida en las notas (Def. 1.4)

type Valuacion = Variable -> Int  -- OTRO arreglo temporal.

-- Puse True solo para compilar:
--satisfaceA :: Valuacion -> PL -> Bool
--satisfaceA _ _= True  
-- LO CORRECTO se parece a:

satisfaceA :: Valuacion -> PL -> Bool
satisfaceA sigma phi =  -- σ |= ϕ, σ satisface a ϕ Def. 1.4 de las notas
    case phi of -- segun la estrutura de phi (Ver la Def. 1.4 de las notas):
        -- Casos base:
        Bot         -> False        -- Si phi=Bot, sigma no satisface a phi
        Top         -> True         -- Si phi=Top, sigma satisface a phi 
        Var x       -> sigma x == 1 -- Si phi=x in Var, sigma satisface a phi sii sigma(x)=1
        -- Casos recursivos:
        Imp alfa beta   -> not(satisfaceA sigma alfa)   -- sigma no|= alfa
                            ||                          --   or
                           (satisfaceA sigma beta)      -- sigma |= beta
        Dis alfa beta   -> (satisfaceA sigma alfa)      -- sigma |= alfa
                            ||                          --   or
                           (satisfaceA sigma beta)      -- sigma |= beta
        Con alfa beta   -> (satisfaceA sigma alfa)      
                            &&                         
                           (satisfaceA sigma beta)
        Neg alfa        -> not(satisfaceA sigma alfa)
        
--

--Tests:
sigma0 ::Valuacion
sigma0 v -- guarded definitions, el guardia es la barra vertical |
    | v=="x"    = 0  -- sigma0 v=0 si v="x"
    | v=="y"    = 1  -- sigma0 v=1 si v="y"
    | otherwise = 0  -- sigma0 v=0 en cualquier otro caso


---------------------------------------------------------------------
-- CNF

-- Un literal positivo es: una variable.
--        Un literal negativo es: la negación de una variable.
--        Un literal es: un literal positivo, o un literal negativo.

-- 1. Un literal es: 
--          una variable (literal positivo)
--          o la negación de una variable (literal negativo).
data Literal v = -- v es un parametro para el tipo de variables
      Lpos {value :: v}    -- literal positivo,  va
    | Lneg {value :: v}    -- literal negativo, ¬v
    deriving (Eq,Show)
    
-- 2. Una cláusula es la disyunción de (una lista de) literales.
data Clausula v = -- v es un parametro para el tipo de variables
    OR [Literal v]  -- Disyunción de una lista de literales. OR es un constructor
    deriving (Eq,Show)
    
-- Una fórmula en CNF (Forma normal de conjunciones) es:
--            la conjunción de (una lista de) cláusulas.
data CNF v = -- v es un parametro para el tipo de variables
    Cand [Clausula v] -- Conjuncion de una lista de clausulas. Cand es constructor
    deriving (Eq,Show)
    
-- Comentamos brevemente que: Una ventaja de las formas normales es:
--      algunos procedimientos (Validez para CNF) son eficientes (polinomiales).

--------------------------------------------------------------------------------
-- HOY, otra forma "normal" para PL: Formulas de Horn:
--
--     7. Una cláusula de Horn es una cláusula con A LO MÁS un literal positivo.
-- Es decir una cláusula de Horn tiene: 
--      cero o un literal positivo, llamado "Cabeza".
--        <Head>(Var) ::= [] | [l] 
--                        con l in <LiteralPositivo>(Var).
--        <ClausulaHorn>(Var) ::= OR (<Head>(Var) ++ [<LiteralNegativo>(Var)])
--                             con Var (un conjunto no vacío de "variables").
--                             donde ++ es: concatenación de listas
--     8. Una fórmula de Horn es:
--             la conjunción de (una lista de) cláusulas de Horn.
--        <FormulaHorn>(Var) ::= AND [<ClausulaHorn>(Var)]
--                             con Var (un conjunto no vacío de "variables").
-- En Haskell:
--
-- INICIO de un implementacion (formalización) en Haskell de Fórmulas de Horn
-- Cabeza de una clausula de Horn (el único posible literal positivo)
data Head v = -- v es un parametro para el tipo de variables
              Hbot      -- cero literales positivos, cabeza vacía
            | Hpos v    -- un literal positivo, cabeza no vacía
            deriving (Eq,Show)
-- Cuerpo de una clausula de Horn (la lista de literales negativos)
data Body v = Body [v]  -- el cuerpo se construye con 
                        -- la lista de variables representa los literales negativos.
            deriving (Eq,Show)
-- Una cláusula de Horn es una cláusula con A LO MÁS un literal positivo.
data ClausulaHornVar v = -- v es un parametro para el tipo de variables
    Chorn               -- constructor de clausulas de Horn
        (Head v         -- cabeza de la clausula de Horn (posible literal positivo)
        , Body v)       -- cuerpo de la clausula de Horn (los literales negativos).
    deriving (Eq,Show)
type ClausulaHorn = ClausulaHornVar String

-- Formulas de Horn
-- Conjuncion de una lista de clausulas de Horn.
data FormulaHornVar v =
    Hand                    -- Conjuncion de 
        [ClausulaHornVar v]    --      una lista de clausulas de Horn.
    deriving (Eq,Show)
type FormulaHorn = FormulaHornVar String

-- FIN de un implementacion (formalización) en Haskell de Fórmulas de Horn
-- Ejemplos:
--
cH1 :: ClausulaHornVar String
-- cH1= v1 or ¬v2 or ¬v3   disyunción de literales con <=1 literales positivos)
-- cH1= (v2 and v3) -> v1  (v2 y v3) implica v1
-- cH1= v1 <- (v2 and v3)  v1 si (v2 y v3)
--
-- cH1  = v1 or ¬v2 or ¬v3
--      = v1 or  (¬v2 or ¬v3)   Agrupando los literales negativos
--      = v1 or ¬(v2 and v3)    Usando ¬(p and q) = ¬p or ¬q.
--      = ¬(v2 and v3) or v1    Ordenando
--      = (v2 and v3) -> v1     Usando (p -> q) = ¬p or q
--      = v1 <- (v2 and v3)     Con implicacion al revés, "v1 si (v2 and v3)"
--
cH1 = Chorn             -- clausula de Horn con:
    (Hpos "v1",         --      cabeza v1
    Body ["v2","v3"])   --      cuerpo [¬v2,¬v3]
    

cH2 :: ClausulaHornVar String
-- cH2= bottom or ¬v4 or ¬v5    disyunción de literales con <=1 literales positivos)
-- cH2= (v4 and v5) -> bottom   (v4 y v5) implica bottom
-- cH2=  <- (v4 and v5)         bottom si (v4 y v5)
-- Obseevacion: alpha -> bottom     Forma que usa para expresar ¬alpha
cH2 = Chorn             -- clausula de Horn con:
    (Hbot,              --      cabeza vacía, cero literales positivos
    Body ["v4","v5"])   --      cuerpo [¬v4,¬v5], dos literales negativos

--
-- cH2b = v1             -- un literal positivo y cero literales negativos.
-- cH2b = And[] -> v1
-- cH2b = True -> v1
-- v1                   -- un hecho (fact)
cH2b = Chorn             -- clausula de Horn con:
    (Hpos "v1",         --      cabeza v1, un literal positivo
    Body [])            --      cuerpo [], cero literales negativos

--
-- cH2X = bottom or Or[]
-- cH2X = bottom or bottom
-- cH2X = bottom 

cH2X = Chorn             -- clausula de Horn con:
    (Hbot,              --      cabeza vacía, cero literales positivos
    Body [])            --      cuerpo [], cero literales negativos


cH3 :: ClausulaHornVar String
-- cH1= v3 or ¬v2 or ¬v3   disyunción de literales con <=1 literales positivos)
-- cH1= (v2 and v3) -> v3  (v2 y v3) implica v3
-- cH1= v3 <- (v2 and v3)  v3 si (v2 y v3)
cH3 = Chorn             -- clausula de Horn con:
    (Hpos "v3",         --      cabeza v3
    Body ["v2","v3"])   --      cuerpo [¬v2,¬v3]
--
--
cH4 = Chorn             -- clausula de Horn con:
    (Hpos "v4",         --      cabeza v4
    Body ["v4","v3"])   --      cuerpo [¬v4,¬v3]


fH1 :: FormulaHorn
fH1 = Hand      -- AND de  
    [cH1, cH2]  --      una lista de clausulas de Horn

fH2 :: FormulaHorn
fH2 = Hand      -- AND de  
    [cH3, cH4]  --      una lista de clausulas de Horn


-- INTUITIVAMENTE:
-- Formula de Horn  = conjuncion de clausulas (de Horn, a lo mas un lit pos)
-- CNF              = conjuncion de clausulas (sin restricciones)
-- ¿Las fórmulas de Horn estan contenidas en CNF? Sí.
-- Sabemos que Validez para formulas en CNF es eficiente (costo polinomial)
-- Por lo tanto, podemos decidir Validez para Formulas de Horn
--      en forma eficiente (costo polinomial).
-- ADEMAS!: podemos decidir Satisfactibilidad para Formulas de Horn
-- en forma eficiente (tiempo polinomial)
-- La proxima clase (el 29 de septiembre).

-- Clase 27 de septiembre de 2022:
esClausulaHornValida :: ClausulaHorn -> Bool
-- Para que una clausula de Horn tenga un par de literales complementarios,
-- como el único posible literal positivo es la cabeza,
-- basta revisar que el complemento de la cabeza esta en el cuerpo.

esClausulaHornValida (Chorn (head, Body litNegList)) = 
--(esClausulaHornValida cH) regresa True si cH tiene literales complementarios.
-- Pero cH tiene A LO MAS un literal positivo.
-- Basta checar que el posible literal positivo de cH 
--                  tambien esta en el cuerpo de cH (lit negativos)
    case head of
        Hpos v  -> v `elem` litNegList -- ¿v in lista de vars p lit negativos?
        Hbot    -> False 
-- TESTS:
-- esClausulaHornValida cH1
-- esClausulaHornValida cH2
-- esClausulaHornValida cH3
-- esClausulaHornValida cH4


esFormulaHornvalida :: FormulaHorn -> Bool
esFormulaHornvalida (Hand listaDeClausulasHorn) = 
    and [esClausulaHornValida cH | cH <- listaDeClausulasHorn]
--     case listaDeClausulasHorn of
--         []      -> True
--         c : l'  -> esClausulaHornValida c && (esFormulaHornvalida (Hand l'))
-- La función and de Haskell sí funciona (creo que confundí los ejemplos)
-- TESTS:
-- esFormulaHornvalida fH1
-- esFormulaHornvalida fH2
-- FIN Clase 27 de septiembre de 2022.
 
----------------------------------------------------------------------
-- Parentesis para definir formulas en forma normal de disyunciones (DNF)
-- Disjunction Normal Form 
-- Formulas en DNF
data Termino v =
    AND [Literal v]  -- Un Termino es una conjuncion de literales 
    deriving (Eq,Show)
    
data DNF v = 
    Cor [Termino v]  -- Una Disyunción de Términos
    deriving (Eq, Show)
    
-----------------------------------------------------------------------
    
    
-- Algoritmo polinomial para determinar si una formula en CNF es válida:

-- dos literales son complementarios:
sonLitComplentarios :: (Eq v) => (Literal v) -> (Literal v) -> Bool
sonLitComplentarios l1 l2 =
    case l1 of
        Lpos x     -> case l2 of
                            Lpos _  -> False
                            Lneg y  -> (x==y)
        Lneg x     -> case l2 of
                            Lpos y  -> (x==y)
                            Lneg _  -> False
                            
esClausulaValida :: (Eq v) => (Clausula v) -> Bool
esClausulaValida (OR c) =
    case c of
        []          -> False -- OR []=false por convención (ver las notas)
        [_]         -> False -- OR [l]=l por convención, y un literal l puede hacerse falso (ver notas) 
        l1:(l2:c')  -> (sonLitComplentarios l1 l2)
                        || (esClausulaValida (OR (l1:c'))) --quitando l2
                        || (esClausulaValida (OR (l2:c'))) --quitando l1
-- Devuelve el complemento de una literal
complemento :: (Eq v) => Literal v -> Literal v
complemento (Lpos v) = Lneg v
complemento (Lneg v) = Lpos v

--Nos dice si una literal esta en una clausula
elementoDe :: (Eq v) => Literal v -> [Literal v] -> Bool
elementoDe _ [] = False
elementoDe l (x:xs) = if l==x then True else (elementoDe l xs)

tieneLiteralesComplementarias :: (Eq v) => [Literal v] -> Bool
tieneLiteralesComplementarias [] = False
tieneLiteralesComplementarias [l] = False
-- tieneLiteralesComplementarias (x:xs) = if (elementoDe (complemento x) xs) then True else tieneLiteralesComplementarias xs

tieneLiteralesComplementarias (x:xs) = if ((complemento x) `elem` xs) then True else tieneLiteralesComplementarias xs

esClausulaValida2 :: (Eq v) => (Clausula v) -> Bool
esClausulaValida2 (OR l) = tieneLiteralesComplementarias l

esFormulaCNFvalida :: (Eq v) => CNF v -> Bool
esFormulaCNFvalida (Cand l) = case l of
                                []     -> True
                                (x:xs) -> (esClausulaValida2 x) && esFormulaCNFvalida (Cand xs)

                                        
esTerminoSatisfacible :: (Eq v) => Termino v -> Bool
esTerminoSatisfacible (AND l) = not (tieneLiteralesComplementarias l)

esFormulaDNFSatisfacible :: (Eq v) => DNF v -> Bool
esFormulaDNFSatisfacible  (Cor l) = case l of
                                []     -> False
                                (x:xs) -> (esTerminoSatisfacible x) || esFormulaDNFSatisfacible(Cor xs)


{-l1= Lpos "v1"
l1'=Lneg "v1"
 l2= Lneg "v2"
 l2'= Lpos "v2"
 l3= Lpos "v3"
 l4= Lneg "v4"
 t1 = AND [l1,l2,l3]
 t2 = AND [l2,l2',l4]
 t3 = AND [l1, l1',l4]
 dnf = Cor [t1,t2,t3]-}

