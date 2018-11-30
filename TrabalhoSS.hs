import Estado


data AExp = Num Int
     |Var String
     |Som AExp AExp
     |Sub AExp AExp
     |Mul AExp AExp
  deriving(Show)

data BExp = TRUE
     | FALSE
     | Not BExp
     | And BExp BExp
     | Or  BExp BExp
     | Ig  AExp AExp
     | MIg AExp AExp -- Menor ou igual
   deriving(Show)

data CExp =    While BExp CExp
     | Repeat CExp BExp
     | For AExp AExp AExp CExp
     | If BExp CExp CExp
     | Seq CExp CExp
     | Atrib AExp AExp
     | Atrib2 AExp AExp AExp AExp
     | Skip
   deriving(Show)                



aSmallStep :: (AExp,Estado) -> (AExp,Estado)
aSmallStep (Var x,s) = (Num (procuraVar s x),s)
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                 in (Som (Num x) ef,s)
aSmallStep (Som e1 e2,s)  = let (ef,_) = aSmallStep (e1, s)
                            in (Som ef e2,s)
--aSmallStep (Sub e1 e2,s)  =
aSmallStep (Sub (Num x) (Num y), s) = (Num (x+y), s)
aSmallStep (Sub (Num x) e2, s) = let (ef,_) = aSmallStep (e2, s)
                                 in (Sub (Num x) ef,s)
aSmallStep (Sub e1 e2,s)  = let (ef,_) = aSmallStep(e1, s)
                            in (Sub ef e2, s)
--aSmallStep (Mul e1 e2,s)  = 
aSmallStep (Mul (Num x) (Num y), s) = (Num (x*y), s)
aSmallStep (Mul (Num x) e2, s) = let (ef,_) = aSmallStep (e2, s)
                                 in (Mul (Num x) ef,s)
aSmallStep (Mul e1 e2,s)  = let (ef,_) = aSmallStep(e1, s)
                            in (Mul ef e2, s)


interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False



bSmallStep :: (BExp,Estado) -> (BExp,Estado)
bSmallStep (Not FALSE,s)      = (TRUE,s)
bSmallStep (Not TRUE,s)       = (FALSE, s)
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s)
                        in (Not bn ,sn)
bSmallStep (And TRUE b2,s)  = (b2,s)
bSmallStep (And FALSE b2,s) = (FALSE,s)
bSmallStep (And b1 b2,s)    = let (bn,sn) = bSmallStep (b1,s)
                              in (And bn b2,sn)
-- IG
--bSmallStep (Ig e1 e2,s )  = 
bSmallStep (Ig (Num n1) (Num n2), s)
  | ( n1 == n2 ) = (TRUE, s)
  | otherwise    = (FALSE, s)

bSmallStep (Ig (Num n1) e2, s) = let (en, _) = aSmallStep (e2,s)
                                 in (Ig (Num n1) en, s)
bSmallStep (Ig e1 e2, s) = let (en,_) = aSmallStep (e1,s)
                                 in (Ig en e2, s)

-- MIG - Menor ou igual
-- bSmallStep (Ig e1 e2,s )  =
bSmallStep (MIg (Num n1) (Num n2), s)
  | ( n1 <= n2 ) = (TRUE, s)
  | otherwise    = (FALSE, s)

bSmallStep (MIg (Num n1) e2, s) = let (en, _) = aSmallStep (e2,s)
                                 in (MIg (Num n1) en, s)
bSmallStep (MIg e1 e2, s) = let (en,_) = aSmallStep (e1,s)
                                 in (MIg en e2, s)


-- OR
--bSmallStep (Or b1 b2,s )  =
bSmallStep (Or TRUE b2,s)  = (TRUE,s)
bSmallStep (Or FALSE b2,s) = (b2,s)
bSmallStep (Or b1 b2,s)    = let (bn,_) = bSmallStep (b1,s)
                              in (Or bn b2,s)
							  
interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE = True
isFinalB FALSE = True
isFinalB x = False


cSmallStep :: (CExp,Estado) -> (CExp,Estado)

-- IF
-- cSmallStep (If c1 c2, s) =
cSmallStep (If TRUE c1 c2, s) = cSmallStep (c1, s)
cSmallStep (If FALSE c1 c2, s) = cSmallStep (c2, s)
cSmallStep (If b c1 c2,s) = let (bn, _) = bSmallStep (b, s)
                            in (If bn c1 c2, s)
-- SEQ
-- cSmallStep (Seq c1 c2, s) =
cSmallStep (Seq Skip c2, s) = cSmallStep (c2, s)
cSmallStep (Seq c1 c2,s)  = let (cn, sn) = cSmallStep (c1, s)
                            in (Seq cn c2, sn)

-- ATRIB
--cSmallStep (Atrib (Var x) e,s) =
cSmallStep (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)

cSmallStep (Atrib (Var x) e, s) = let (en, _) = aSmallStep (e, s)
                                  in (Atrib (Var x) en, s)

-- WHILE
-- cSmallStep (While b c, s)
cSmallStep (While b c, s) = (If b (Seq c ( While b c ) ) Skip, s)

-- REPEAT UNTIL
-- cSmallStep (Repeat c b, s)
cSmallStep (Repeat c b, s) = (Seq c (If b Skip (Repeat c b)), s)

-- FOR
-- cSmallStep (For x e1 e2 c, s)
cSmallStep (For (Var x) e1 e2 c, s) = (Seq (Atrib (Var x) e1) ( If (MIg e1 e2) (Seq c (For (Var x) (Som e1 (Num 1)) e2 c) ) Skip ), s)

-- DUPLA ATRIBUICAO
-- cSmallStep (Atrib2 (Var x1) (Var x2) e1 e2, s)
cSmallStep (Atrib2 (Var x1) (Var x2) e1 e2, s) = (Seq (Atrib (Var x1) e1) (Atrib (Var x2) e2), s)


-- INTERPRETADOR COMANDOS
-- interpretC :: (CExp,Estado) -> (CExp,Estado)
-- interpretC (c,s) = ?
interpretC :: (CExp, Estado) -> (CExp, Estado) 
interpretC (c,s) = if isFinalC c then (c,s) else interpretC (cSmallStep (c,s))

-- isFinalC :: CExp -> Bool
isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC x = False


meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])

-- EXEMPLOS ADICIONADOS
exemplo3 :: CExp
exemplo3 = If ( Ig (Som (Num 1) (Num 2)) (Num 3) )  ( Atrib (Var "x") (Num 4) ) Skip

exemplo4 :: CExp
exemplo4 = While  (Ig (Var "x") (Num 3))  (Atrib (Var "x") (Num 1))

exemplo5 :: CExp
exemplo5 = For (Var "x") (Num 0) (Num 10) (Atrib (Var "y") (Som (Var "y") (Num 1) ) ) 
