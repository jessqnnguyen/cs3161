module MinHS.Evaluator where
import qualified MinHS.Env as E
import MinHS.Syntax as S
import MinHS.Pretty
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type VEnv = E.Env Value

data Value = I Integer
           | B Bool
           | Nil
           | Cons Integer Value
           -- Others as needed
           deriving (Show)

instance PP.Pretty Value where
  pretty (I i) = numeric $ i
  pretty (B b) = datacon $ show b
  pretty (Nil) = datacon "Nil"
  pretty (Cons x v) = PP.parens (datacon "Cons" PP.<+> numeric x PP.<+> PP.pretty v)
  pretty _ = undefined -- should not ever be used

evaluate :: Program -> Value
evaluate [Bind _ _ _ e] = evalE E.empty e
evaluate bs = evalE E.empty (Let bs (Var "main"))                

evalOneArgPrimOp :: Op -> Value -> Value 
evalOneArgPrimOp Neg (I x) = I (-x)
evalOneArgPrimOp Null Nil = B True
evalOneArgPrimOp Null (Cons x xs) = B False 
evalOneArgPrimOp Head (Cons x xs) = I x 
evalOneArgPrimOp Tail (Cons x xs) = xs
evalOneArgPrimOp op v = error $ "Op: " ++ (show op) ++ " " ++ (show v) ++ " failed"

evalTwoArgsPrimOp :: Op -> Value -> Value -> Value 
evalTwoArgsPrimOp Add (I x) (I y) = I $ x + y
evalTwoArgsPrimOp Sub (I x) (I y) = I $ x - y
evalTwoArgsPrimOp Mul (I x) (I y) = I $ x * y
evalTwoArgsPrimOp Quot (I x) (I y)
  | y /= 0 = I $ x `quot` y
  | y == 0 = error $ "Cannot divide by 0"
evalTwoArgsPrimOp Rem (I x) (I y) = I $ x `mod` y 
evalTwoArgsPrimOp Gt (I x) (I y) = B (x > y)
evalTwoArgsPrimOp Ge (I x) (I y) = B (x >= y)
evalTwoArgsPrimOp Lt (I x) (I y) = B (x < y)
evalTwoArgsPrimOp Le (I x) (I y) = B (x <= y)
evalTwoArgsPrimOp Eq (I x) (I y) = B (x == y)
evalTwoArgsPrimOp Ne (I x) (I y) = B (x /= y)
evalTwoArgsPrimOp Eq (B b1) (B b2) = B (b1 == b2)
evalTwoArgsPrimOp Ne (B b1) (B b2) = B (b1 /= b2)
evalTwoArgsPrimOp op v1 v2 = error $ "Op: " ++ (show op) ++ " " ++ (show v1) ++ " " ++ (show v2) ++ "is not possible"

evalE :: VEnv -> Exp -> Value
evalE g (Var id) 
  = case (E.lookup g id) of Just a -> a
                            Nothing -> error ("Variable " ++ (show id) ++ " out of scope")
evalE _ (Num x) = I x 
evalE _ (Con c) 
  = case c of "True" -> B True
              "False" -> B False
              "Nil" -> Nil
evalE g (App (App (Con "Cons") x) xs) = Cons n $ evalE g xs
  where 
    I n = evalE g x 
evalE g (App (Prim op) x) = evalOneArgPrimOp op (evalE g x)            
evalE g (App (App (Prim op) e1) e2)
  = case op of Add -> evalTwoArgsPrimOp (Add) (evalE g e1) (evalE g e2)
               Sub -> evalTwoArgsPrimOp (Sub) (evalE g e1) (evalE g e2)
               Mul -> evalTwoArgsPrimOp (Mul) (evalE g e1) (evalE g e2)
               Quot -> evalTwoArgsPrimOp (Quot) (evalE g e1) (evalE g e2)
               Rem -> evalTwoArgsPrimOp (Rem) (evalE g e1) (evalE g e2)
               Gt -> evalTwoArgsPrimOp (Gt) (evalE g e1) (evalE g e2)
               Ge -> evalTwoArgsPrimOp (Ge) (evalE g e1) (evalE g e2)
               Lt -> evalTwoArgsPrimOp (Lt) (evalE g e1) (evalE g e2)
               Le -> evalTwoArgsPrimOp (Le) (evalE g e1) (evalE g e2)
               Eq -> evalTwoArgsPrimOp (Eq) (evalE g e1) (evalE g e2)
               Ne -> evalTwoArgsPrimOp (Ne) (evalE g e1) (evalE g e2) 
evalE g (If e1 e2 e3)
   = case e1' of B True  -> evalE g e2 
                 B False -> evalE g e3
   where 
     e1' = evalE g e1
evalE g e = error "Implement me!"
