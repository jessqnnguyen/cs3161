module MinHS.TyInfer where

import qualified MinHS.Env as E
import MinHS.Syntax
import MinHS.Subst
import MinHS.TCMonad

import Data.Monoid (Monoid (..), (<>))
import Data.Foldable (foldMap)
import Data.List (nub, union, (\\))

primOpType :: Op -> QType
primOpType Gt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ge   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Lt   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Le   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Eq   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Ne   = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Bool)
primOpType Neg  = Ty $ Base Int `Arrow` Base Int
primOpType Fst  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "a"
primOpType Snd  = Forall "a" $ Forall "b" $ Ty $ (TypeVar "a" `Prod` TypeVar "b") `Arrow` TypeVar "b"
primOpType _    = Ty $ Base Int `Arrow` (Base Int `Arrow` Base Int)

constType :: Id -> Maybe QType
constType "True"  = Just $ Ty $ Base Bool
constType "False" = Just $ Ty $ Base Bool
constType "()"    = Just $ Ty $ Base Unit
constType "Pair"  = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "a" `Arrow` (TypeVar "b" `Arrow` (TypeVar "a" `Prod` TypeVar "b"))
constType "Inl"   = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "a" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
constType "Inr"   = Just
                  $ Forall "a"
                  $ Forall "b"
                  $ Ty
                  $ TypeVar "b" `Arrow` (TypeVar "a" `Sum` TypeVar "b")
constType _       = Nothing

type Gamma = E.Env QType

initialGamma :: Gamma
initialGamma = E.empty

tv :: Type -> [Id]
tv = tv'
 where
   tv' (TypeVar x) = [x]
   tv' (Prod  a b) = tv a `union` tv b
   tv' (Sum   a b) = tv a `union` tv b
   tv' (Arrow a b) = tv a `union` tv b
   tv' (Base c   ) = []

tvQ :: QType -> [Id]
tvQ (Forall x t) = filter (/= x) $ tvQ t
tvQ (Ty t) = tv t

tvGamma :: Gamma -> [Id]
tvGamma = nub . foldMap tvQ

infer :: Program -> Either TypeError Program
infer program = do (p',tau, s) <- runTC $ inferProgram initialGamma program
                   return p'

unquantify :: QType -> TC Type
{-
Normally this implementation would be possible:

unquantify (Ty t) = return t
unquantify (Forall x t) = do x' <- fresh
                             unquantify (substQType (x =:x') t)

However as our "fresh" names are not checked for collisions with names bound in the type
we avoid capture entirely by first replacing each bound
variable with a guaranteed non-colliding variable with a numeric name,
and then substituting those numeric names for our normal fresh variables
-}

unquantify = unquantify' 0 emptySubst
unquantify' :: Int -> Subst -> QType -> TC Type
unquantify' i s (Ty t) = return $ substitute s t
unquantify' i s (Forall x t) = do x' <- fresh
                                  unquantify' (i + 1)
                                              ((show i =: x') <> s)
                                              (substQType (x =:TypeVar (show i)) t)
                                              
unifySumProductArrowTypes :: (Type, Type) -> (Type, Type) -> TC Subst 
unifySumProductArrowTypes (t11, t12) (t21, t22) = do 
  s <- unify t11 t21
  let result1 = substitute s t12 
      result2 = substitute s t22  
  s' <- unify result1 result2
  return (s <> s')

unify :: Type -> Type -> TC Subst
unify (TypeVar x) (TypeVar y) 
  | x == y = return emptySubst
  | otherwise = return $ x =: (TypeVar y)
unify t1@(Base bt1) t2@(Base bt2)
  | bt1 == bt2 = return emptySubst
  | otherwise = typeError $ TypeMismatch t1 t2 
unify (Prod t11 t12) (Prod t21 t22) = unifySumProductArrowTypes (t11, t22) (t21, t22)
unify (Sum t11 t12) (Sum t21 t22) = unifySumProductArrowTypes (t11, t22) (t21, t22)
unify (Arrow t11 t12) (Arrow t21 t22) = unifySumProductArrowTypes (t11, t22) (t21, t22)
unify (TypeVar v) t 
  | occursIn v t = typeError $ OccursCheckFailed v t
  | otherwise = return (v =: t)
unify t (TypeVar v)
  | occursIn v t = typeError $ OccursCheckFailed v t
  | otherwise = return (v =: t)
unify t1 t2 = typeError $ TypeMismatch t1 t2

occursIn :: Id -> Type -> Bool 
occursIn id (Base x) = False 
occursIn id (TypeVar x)
  | id == x = True
  | otherwise = False
occursIn id (Arrow x y) = (occursIn id x) || (occursIn id y)
occursIn id (Prod x y) = (occursIn id x) || (occursIn id y)
occursIn id (Sum x y) = (occursIn id x) || (occursIn id y)

generalise :: Gamma -> Type -> QType
generalise g t = error "implement me"

inferProgram :: Gamma -> Program -> TC (Program, Type, Subst)
inferProgram env bs = error "implement me! don't forget to run the result substitution on the"
                            "entire expression using allTypes from Syntax.hs"

inferExp :: Gamma -> Exp -> TC (Exp, Type, Subst)
inferExp _ e@(Num int) = return (e, (Base Int), emptySubst)
inferExp g e@(Var x)
  = case E.lookup g x of 
      Nothing -> typeError $ NoSuchVariable x 
      Just qt -> do 
        qtFresh <- unquantify qt
        return (e, qtFresh, emptySubst)
inferExp _ e@(Prim op) = do
  eFresh <- unquantify $ primOpType op
  return (e, eFresh, emptySubst)
inferExp _ e@(Con id) = do 
  let t = constType id in 
    case t of 
      Just qt -> do 
          eFresh <- unquantify qt
          return (e, eFresh, emptySubst)
      Nothing -> typeError $ NoSuchConstructor id
inferExp g (App e1 e2) = do 
  (e1', t1, s1) <- inferExp g e1
  let g1 = (substGamma s1 g)
  (e2', t2, s2) <- inferExp g1 e2
  alpha <- fresh 
  u <- unify (substitute s2 t1) (Arrow t2 alpha)
  return ((App e1' e2'), (substitute u alpha), (s1 <> s2 <> u))
inferExp g (If e e1 e2) = do 
  (e', t, s) <- inferExp g e -- t <-> tau
  let g1 = (substGamma s g) -- g1 <-> T, add substitution to the environment
  u1 <- unify t (Base Bool) -- u1 <-> U
  let g2 = (substGamma u1 g1) -- g2 <-> UT
  (e1', t1, s1) <- inferExp g2 e1  -- t <-> tau1 
  let g3 = (substGamma s1 g2) 
  (e2', t2, s2) <- inferExp g3 e2 
  u2 <- unify (substitute s2 t1) (t2)
  let finalType = substitute u2 t2 
  let finalSub = (s <> s1 <> s2 <> u1 <> u2)
  return ((If e' e1' e2'), finalType, finalSub)
inferExp g (Case e [Alt "Inl" [x] e1, Alt "Inr" [y] e2]) = do
  (e', t, s) <- inferExp g e
  al  <- fresh
  let g0 =  E.add g (x,(Ty al))
  let g1 = (substGamma s g0)
  (e1', t1, s1) <- inferExp g1 e1
  ar <- fresh
  let g2 =  E.add g (y,(Ty ar))
  let g3 = (substGamma s1 g2)
  (e2', t2, s2) <- inferExp g3 e2
  let p1 = substitute s2 (substitute s1 (substitute s (Sum al ar)))
  let p2 = substitute s2 (substitute s1 t)
  u <- unify p1 p2
  let p3 = substitute u (substitute s2 t1)
  let p4 = substitute u t2 
  u' <- unify p3 p4
  let outputExp = (Case e' [Alt "Inl" [x] e1', Alt "Inr" [y] e2'])
  let outputType = substitute u' (substitute u t2)
  let outputSub = u' <> u <>  s2 <> s1 <> s 
  return (outputExp, outputType, outputSub)
 
inferExp g (Case e _) = typeError MalformedAlternatives

