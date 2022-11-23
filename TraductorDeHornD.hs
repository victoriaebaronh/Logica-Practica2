module TraductorDeHornD
where

import PLcnfClase

{-

-}


traduceHeadAPL :: Head String -> PL 
traduceHeadAPL Hbot = Bot 
traduceHeadAPL (Hpos v) = Var v

traduceBodyAPL1:: Body String -> PL 
traduceBodyAPL1 (Body []) = Bot 
traduceBodyAPL1 (Body (x:xs)) = Dis (Neg (Var x)) (traduceBodyAPL1 (Body xs))


{- Lo malo de este implementacion es que perdimos los valores de positivo o negativo
de las variables. 
-}
traduceCHornAPL1 :: ClausulaHorn -> PL 
traduceCHornAPL1 (Chorn (x,y)) = Dis (traduceHeadAPL x) (traduceBodyAPL1 y)



traduceBodyAPL2:: Body String -> PL 
traduceBodyAPL2 (Body []) = Top
traduceBodyAPL2 (Body (x:xs)) = Con (Var x) (traduceBodyAPL2 (Body xs))



traduceCHornAPL2 :: ClausulaHorn -> PL 
traduceCHornAPL2 (Chorn (x,y)) = Imp (traduceBodyAPL2 y) (traduceHeadAPL x) 


traduceFormulaHornAPL :: FormulaHorn -> PL 
traduceFormulaHornAPL (Hand []) = Top
traduceFormulaHornAPL (Hand (x:xs)) = Con ( traduceCHornAPL2 x) (traduceFormulaHornAPL (Hand xs))

--Funciones Auxiliares de Azucar Sintactica
neg :: PL -> PL
neg = Neg

disj :: PL -> PL -> PL
disj = Dis

conj :: PL -> PL -> PL
conj = Con

-- Funcion que transforma una formula a su forma normal negativa
aNNF :: PL -> PL
aNNF expr@(Var _) = expr
aNNF expr@(Neg (Var _)) = expr
aNNF (Neg (Neg phi)) = aNNF phi
aNNF (Con exp1 exp2) = aNNF exp1 `conj` aNNF exp2
aNNF (Neg (Con exp1 exp2))  = aNNF $ neg exp1 `disj` neg exp2 --DeMorgan
aNNF (Dis exp1 exp2) = aNNF exp1 `disj` aNNF exp2
aNNF (Neg (Dis exp1 exp2))   = aNNF $ neg exp1 `conj` neg exp2 --DeMorgan 2
aNNF (Imp exp1 exp2)              = aNNF $ neg exp1 `disj` exp2
aNNF (Neg (Imp exp1 exp2))   = aNNF $ exp1 `conj` neg exp2

aNNF (Syss exp1 exp2)  =  let a = exp1 `conj` exp2
                              b = neg exp1 `conj` neg exp2
                            in aNNF $ a `disj` b
aNNF (Neg (Syss exp1 exp2)) =   let a = exp1 `disj` exp2
                                    b = neg exp1 `disj` neg exp2
                                in aNNF $ a `conj` b

phi = (Neg (Syss (Con (Neg (Var "p")) (Imp (Var "p") (Var "q")))(Var "p")))

--Funcion que transforma una formula en forma normal negativa a forma normal conjuntiva
aCNF :: PL -> PL 
aCNF e = aCNF' $ aNNF e

aCNF' :: PL -> PL
aCNF' (Con e1 e2) = aCNF' e1 `conj` aCNF' e2
aCNF' (Dis e1 e2) = aCNF' e1 `dist` aCNF' e2
aCNF' e = e

dist :: PL -> PL -> PL
dist (Con e11 e12) e2 = (e11 `dist` e2) `conj` (e12 `dist` e2)
dist e1 (Con e21 e22) = (e1 `dist` e21) `conj` (e1 `dist` e22)
dist e1 e2            = e1 `disj` e2



funcionPuntoLibre :: Int -> Int 
funcionPuntoLibre = (+ 2) 


funcionPuntoLibre2 :: String -> String 
funcionPuntoLibre2 = (" desde punto Libre"++) 


{-
data CNF v = -- v es un parametro para el tipo de variables
    Cand [Clausula v] -- Conjuncion de una lista de clausulas. Cand es constructor
    deriving (Eq,Show)

Recibe una PL que ya se supone que esta en CNF. Regresa una formula del tipo CNF 

Ejemplo:

>>>Recibe
Con (
    Con
        (Dis
            (Neg (Var "p")) (Var "p"))
            
            (Dis (Dis 
                (Neg (Var "p")) (Var "q")) (Var "p"))) 

    (Con 
        (Dis (Dis 
            (Var "p") (Var "p")) (Neg (Var "p"))) 
        
        (Dis (Dis 
        (Var "p") (Neg (Var "q"))) (Neg (Var "p"))))

>>>>Regresa
Cand [
    OR [Lneg {value = "p"},Lpos {value = "p"}],
    OR [Lneg {value = "p"},Lpos {value = "q"},Lpos {value = "p"}],
    OR [Lpos {value = "p"},Lpos {value = "p"},Lneg {value = "p"}],
    OR [Lpos {value = "p"},Lneg {value = "q"},Lneg {value = "p"}]]


Hint: Usen plACNF'
-}
plACNF :: PL -> CNF String
plACNF a = error "Falta implementar"


{-
Recibe una PL y regresa una lista de Clasulas.
Hint: Necesita la ayuda de la funcion auxiliar creaClausula en algunos casos.
-}
plACNF' :: PL -> [Clausula String]
plACNF' a = error "Falta implementar"



{-

-- 2. Una cláusula es la disyunción de (una lista de) literales.
data Clausula v = -- v es un parametro para el tipo de variables
    OR [Literal v]  -- Disyunción de una lista de literales. OR es un constructor
    deriving (Eq,Show)

Recibe una PL y regresa una Clausula que es la representacion con el Data Clausula.
Hint: Necesita la ayuda de la funcion creaLit.
-}


creaClausula :: PL -> Clausula String
creaClausula a = error "Falta implementar"

{--
Recibe una PL y regresa una lista de literales.

-- 1. Un literal es: 
--          una variable (literal positivo)
--          o la negación de una variable (literal negativo).
data Literal v = -- v es un parametro para el tipo de variables
      Lpos 
    | Lneg 
    deriving (Eq,Show)

Recibe una PL y regresa una lista de literales.
Hint: Ya no necesita ninguna llamada a alguna funcion auxiliar
-}
creaLit :: PL -> [Literal String]
creaLit a =
    case a of 
        Bot -> [ ]
        Top -> [ ]
        Var v -> [Lpos v]
        Imp v b  -> (creaLit a) ++ (creaLit b)
        Dis v b -> (creaLit a) ++ (creaLit b)
        Con v b -> (creaLit a) ++ (creaLit b) 
        Neg v -> creaLit v    
        Syss v b -> (creaLit v) ++ (creaLit b)



{-

data Head v = 
              Hbot      
            | Hpos v    
            deriving (Eq,Show)
data Body v = Body [v]  
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


Recibe una CNF String y la devuelve como formulaHorn.
Por ejemplo:
>>>Recibe:

Cand [OR [Lneg "a1",Lpos "a2"],
      OR [Lpos "b1"]]



>>>Regresa:
Hand [Chorn (Hpos "a2",Body ["a1, "]),Chorn (Hpos "b1",Body [])]

(Las comas o elementos que separen los strings en el body se quedan a su 
discrecion.)
Hint: Necesitan de makeHorn
-}
cnfAHorn :: CNF String -> FormulaHorn
cnfAHorn (Cand c)= Hand d
                 where d = map makeHorn c



{-
data Head v = -- v es un parametro para el tipo de variables
              Hbot      -- cero literales positivos, cabeza vacía
            | Hpos v    -- un literal positivo, cabeza no vacía
            deriving (Eq,Show)
-- Cuerpo de una clausula de Horn (la lista de literales negativos)
data Body v = Body [v]  -- el cuerpo se construye con 
                        -- la lista de variables representa los literales negativos.

Chorn               -- constructor de clausulas de Horn
        (Head v         -- cabeza de la clausula de Horn (posible literal positivo)
        , Body v)       -- cuerpo de la clausula de Horn (los literales negativos).

Recibe una Clausula String y devuelve una clausulaHorn.
Recuerden que una clausulaHorn no puede tener mas de una literal positiva por clausula.
Hint: Usen esPositiva.
-}
makeHorn :: Clausula String -> ClausulaHorn 
makeHorn (OR [])     = Chorn (Hbot, Body [])
makeHorn (OR c)
  | nLitPositivos > 1  = error "--Una Clausula de Horn tiene a lo mas un literal positivo.--"
  | nLitPositivos == 1 = Chorn (Hpos cabeza, Body cuerpo)
  | nLitPositivos == 0 = Chorn (Hbot, Body cuerpo)
  where litPositivos  = literalesPositivas c
        litNegativos  = filter (`notElem` litPositivos) c
        nLitPositivos = length litPositivos
        cabeza        = elementoEnLiteral (head litPositivos)
        cuerpo        = map elementoEnLiteral litNegativos 
        

-- Obtiene una lista de literales positivas de una lista de literales
literalesPositivas :: [Literal String] -> [Literal String]
literalesPositivas c = filter (esPositiva) (c)

elementoEnLiteral :: Literal String -> String
elementoEnLiteral (Lpos s) = s
elementoEnLiteral (Lneg s) = s     

{-
Solamente devuelve si es positiva o negativa una Literal.
-}
esPositiva :: Literal String -> Bool
esPositiva (Lpos a) = True 
esPositiva (Lneg a) = False

prueba1 :: PL 
prueba1 = (Con (Dis (Neg (Var "a1")) (Var "a2")) (Var "b1"))
prueba2 :: PL 
prueba2 = Syss (Neg (Con (Var "a1") (Neg (Var "a2"))) ) (Dis (Syss (Neg (Var "b1")) (Var "b2")) (Con (Var "b3") (Neg (Var "b4"))))

prueba3 :: PL 
prueba3 = Con (Dis (Neg (Var "a1")) (Var "a2")) (Dis (Con (Neg (Var "b1")) (Neg (Var "b2"))) (Var "c1") )



c :: CNF String
c= Cand[OR[Lpos "v", Lneg "c", Lneg "d", Lneg "e"], OR[Lneg "q", Lneg "w", Lpos "x", Lneg "z"],OR[Lneg "a", Lneg "p", Lneg "r", Lneg "f"]]


clausula :: Clausula String
clausula = OR[Lpos "v", Lneg "c", Lneg "d", Lneg "e"] 