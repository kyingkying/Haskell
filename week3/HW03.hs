module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st s n = (\x -> if x == s then n else st x)

empty :: State
empty = (\string -> 0)

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st expr = case expr of
                  Var x -> st x
                  Val n -> n
                  Op e1 bop e2 -> case bop of
                                    Plus -> n1 + n2
                                    Minus -> n1 - n2
                                    Times -> n1 * n2
                                    Divide -> n1 `div` n2
                                    Gt -> if n1 > n2 then 1 else 0
                                    Ge -> if n1 >= n2 then 1 else 0
                                    Lt -> if n1 < n2 then 1 else 0
                                    Le -> if n1 <= n2 then 1 else 0
                                    Eql -> if n1 == n2 then 1 else 0
                                  where n1 = evalE st e1
                                        n2 = evalE st e2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar stmt = case stmt of
                 Assign s expr -> DAssign s expr
                 Incr s -> DAssign s (Op (Var s) Plus (Val 1))
                 If expr stmt1 stmt2 -> DIf expr (desugar stmt1) (desugar stmt2)
                 While expr stmt1 -> DWhile expr (desugar stmt1)
                 For initstmt expr updatestmt bodystmt -> DSequence (desugar initstmt) (DWhile expr (DSequence (desugar bodystmt) (desugar updatestmt)))
                 Sequence stmt1 stmt2 -> DSequence (desugar stmt1) (desugar stmt2)
                 Skip -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st stmt = case stmt of
                       DAssign s expr -> extend st s (evalE st expr)
                       DIf expr stmt1 stmt2 -> if evalE st expr == 1 then evalSimple st stmt1 else evalSimple st stmt2
                       DWhile expr stmt1 -> if evalE st expr == 1 then evalSimple (evalSimple st stmt1) (DWhile expr stmt1) else st
                       DSequence stmt1 stmt2 -> evalSimple (evalSimple st stmt1) stmt2
                       DSkip -> st

run :: State -> Statement -> State
run st stmt = evalSimple st (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
