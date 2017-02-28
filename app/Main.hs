module Main (main) where


--- * imports --------------------------------------------------------------------------------------------------------

import           Control.Monad.State.Strict
import           Data.Monoid                      ((<>))
import qualified Data.Set                         as S
import           System.Environment               (getArgs, getProgName)

import qualified Data.Rewriting.Rule              as R

import           GUBS                             as G (Constraint (..))
import qualified GUBS                             as G
import qualified GUBS.Term                        as GT


import           Tct.Trs
import           Tct.Trs.Config                   (parserIO)
import qualified Tct.Trs.Data.Problem             as Prob
import qualified Tct.Trs.Data.Rules               as RS (funs, toList)
import qualified Tct.Trs.Data.Signature           as Sig
import           Tct.Trs.Data.Symbol              (fun, var)
import           Tct.Trs.Encoding.UsablePositions (usableArgsWhereApplicable, usablePositionsOf)


--- * encode ---------------------------------------------------------------------------------------------------------
-- construct a ConstraintSystem from a Trs problem

canonicalVar' :: Int -> V
canonicalVar' = var . ("_x" <>) . show

canonicalVar :: Int -> G.Term f V
canonicalVar = GT.Var . canonicalVar'

canonicalFun' :: Int -> F
canonicalFun' = fun . ("_f" <>) . show

fresh :: State ([G.Term F V],[F]) (G.Term F V)
fresh = do
  (cs,f:fs) <- get
  let c = G.Fun f []
  put (c:cs,fs)
  return c

withFreshFuns :: S.Set F -> State ([G.Term F v], [F]) a -> (a, [G.Term F v])
withFreshFuns funs m = fst <$> runState m ([], funs')
  where funs' = filter (`S.notMember` funs) $ canonicalFun' `fmap` [1..]


canonicalTerm :: (f,Int) -> G.Term f V
canonicalTerm (f,ar)= G.Fun f $ canonicalVar `fmap` [1..ar]

encodeTerm :: R.Term f v -> GT.Term f v
encodeTerm (R.Fun f ts) = GT.Fun f (encodeTerm `fmap` ts)
encodeTerm (R.Var v)    = GT.Var v

-- | Constraint encoding for Polynomial Interpretations, where
-- montonicity: f(x1,...,xn)  >= sum (upos(f))
-- restricted:  x1,...,xn + k >= f(x1,...,xn)
--
-- Any:
-- l_i >= r_i + k_i
-- sum(k_i) >= 0
encode :: Problem F V -> G.ConstraintSystem F V
encode prob =
  orientStrs strs
  <> orientWeakly `fmap` wtrs
  <> monotone `fmap` fs
  <> stronglyLinear cs

  where

  strs = RS.toList $ Prob.strictComponents prob
  wtrs = RS.toList $ Prob.weakComponents prob
  upos = usablePositionsOf $ usableArgsWhereApplicable prob (Prob.isDTProblem prob) True
  sig  = Prob.signature prob
  fs   = Sig.elems sig
  cs   = Sig.elems $ Sig.restrictSignature sig (Sig.constructors sig)
  funs = RS.funs (Prob.allComponents prob)

  orientStrs rs = orientStrictly `fmap` rs

  orientStrictly  R.Rule{R.lhs=l,R.rhs=r} = encodeTerm l :>=: encodeTerm r G..+ GT.Const 1
  orientWeakly    R.Rule{R.lhs=l,R.rhs=r} = encodeTerm l :>=: encodeTerm r

  monotone (f,i)        = canonicalTerm (f,i) :>=: G.sumA (canonicalVar `fmap` upos f)
  stronglyLinear xs     = fst $ withFreshFuns funs (forM xs stronglyLinearM)
  stronglyLinearM (f,i) = fresh >>= \v -> return $
                            G.sumA (canonicalVar `fmap` [1..i]) G..+ v :>=: canonicalTerm (f,i)


--- * main -----------------------------------------------------------------------------------------------------------

type Constraints = G.ConstraintSystem F V

fromFile :: FilePath -> IO (Either String Trs)
fromFile = parserIO

toRCI :: Trs -> Trs
toRCI = Prob.toInnermost . Prob.toRC

trs2cs :: Trs -> Constraints
trs2cs = encode

pp :: Constraints -> String
pp = show . G.prettySexp

main :: IO ()
main = do
  p  <- getProgName
  let usage = p <> " file.trs|file.xml"
  as <- getArgs
  if null as
  then print usage
  else
    case head as  of
      "--help" -> print usage
      fp       -> do
        csE <- fmap (pp . trs2cs . toRCI) <$> fromFile fp
        putStrLn $ either id id csE

