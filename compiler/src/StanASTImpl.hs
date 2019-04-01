{-
Copyright 2019 Adobe. All rights reserved. This file is licensed to you under
the Apache License, Version 2.0 (the "License"); you may not use this file
except in compliance with the License. You may obtain a copy of the License at
http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
REPRESENTATIONS OF ANY KIND, either express or implied. See the License for the
specific language governing permissions and limitations under the License.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module StanASTImpl where

import Data.Char (isSpace)
import qualified Data.Graph as G
import Data.List (intercalate)
import Unique (sortUniq)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.QQ
import Text.Printf (printf)

import Misc
import Number
import BasicExpr
import SSM

data StanAST = StanAST
  { stdata   :: [BoundedTypedVar]
  , stxdata  :: [(BoundedTypedVar, Expr)]
  , stparms  :: [BoundedTypedVar]
  , stxparms :: [(BoundedTypedVar, Expr)]
  , stmodel  :: StanModelBlock
  }
  deriving (Eq, Show)

type BoundedTypedVar = (VarName, (TypeS, Bounds))
type TypedVar = (VarName, TypeS)

data Type a = Type (ElemType a) [a]
  deriving (Eq, Show)

type TypeS = Type Expr -- type with shape

data ElemType a
  = IntType | RealType
  | VectorType a | RowVectorType a | MatrixType a a
  deriving (Eq, Show)

data StanModelBlock = StanModelBlock
  { medefs :: [(TypedVar, Expr)]
  , medraws :: [(VarName, DistrExpr)]
  , messm :: SSM
  }
  deriving (Eq, Show)

data DistrExpr = DistrExpr DistrName [Expr] Bounds
  deriving (Eq, Show)

distrExpr :: DistrName -> [Expr] -> DistrExpr
distrExpr dname args = DistrExpr dname args nobounds

matZ' :: SSM -> Expr
matZ' ssm = Apply "to_matrix" [vecZ ssm]

vecH :: SSM -> Expr
vecH ssm = Apply "rep_vector" [scalH ssm, litI 1]

compileStanAST :: StanAST -> String
compileStanAST x =
  concatMap (++ "\n") compiledLines
  where
    compiledLines = wrap "functions" fctsblock ++
                    wrap "data" datablock ++
                    wrap "transformed data" xdatablock ++
                    wrap "parameters" parmsblock ++
                    wrap "transformed parameters" xparmsblock ++
                    wrap "model" (modelblock $ stmodel x)
    fctsblock = intercalate [""] $ function_defs $ used_fcts x
    datablock = "int<lower = 1> N;" :
                "vector[N] y;" :
                (map stan_var_decl $ stdata x)
    xdatablock = "matrix[1, N] yy = to_matrix(y');" :
                 map stan_var_def (stxdata x)
    parmsblock = map stan_var_decl $ stparms x
    xparmsblock = map stan_var_def $ stxparms x

modelblock :: StanModelBlock -> [String]
modelblock x = defs ++ draws ++ [ssmdraw]
  where
    defs = map def2str $ medefs x
    draws = map draw2str $ medraws x
    ssmdraw =
      printf "yy ~ %s;" $ decode $ distrExpr "gaussian_dlm_obs" $
      map ($ (messm x)) [matZ', matT, vecH, matQ, veca0, matP0 ]
    def2str (vt, e) = stan_var_def (vt2vbt vt, e)
    draw2str (v, de) = printf "%s ~ %s;" (vcode v) (decode de)

vt2vbt :: TypedVar -> BoundedTypedVar
vt2vbt (v, Type et shape) = (v, (Type et shape, nobounds))

decode :: DistrExpr -> String
decode (DistrExpr dname args bnds) =
  if Set.member dname stanDistrs
    then fctcode dname 0 args ++ bndscode
    else error $ "Unknown Stan distribution name: " ++ dname
  where
    bndscode = case bnds of
      Bounds Nothing   Nothing   -> ""
      Bounds (Just e)  Nothing   -> printf " T[%s, ]" (ecode e)
      Bounds Nothing   (Just e)  -> printf " T[, %s]" (ecode e)
      Bounds (Just e1) (Just e2) -> printf " T[%s, %s]" (ecode e1) (ecode e2)

stanDistrs :: Set DistrName
stanDistrs = Set.fromList
  [ "normal", "exp_mod_normal", "skew_normal", "student_t", "cauchy"
  , "double_exponential", "logistic", "gumbel", "lognormal", "chi_square"
  , "inv_chi_square", "scaled_inv_chi_square", "exponential", "gamma"
  , "inv_gamma", "weibull", "frechet", "rayleigh", "wiener", "pareto"
  , "pareto_type_2", "beta", "uniform", "multi_normal", "multi_normal_prec"
  , "multi_normal_cholesky", "multi_student_t", "gaussian_dlm_obs"
  ]

topo_sort_map :: Ord k => Map k ([k], a) -> [k] -> [(k, a)]
-- The map m defines a graph with values attached to every node. In particular,
-- the map entry (v, (out, x)) means that v is a vertex of the graph, with
-- associated value x, and there is an outgoing graph edge from v to each
-- element of out.
-- RETURNS: A list of pairs (v, x), where v is a graph node reachable
-- from some member of initial, x is the value associated with v in the graph,
-- and the list is topologically sorted, that is, if there is an edge from
-- v to v' in the graph, then (v, _) precedes (v', _) in the list.
--
topo_sort_map m initial =
  map vinfo sorted_reachable_vertices
  where
    edge_for (key, (successors, value)) = (value, key, successors)
    (graph, nodeFromVertex, vertexFromKey) =
      G.graphFromEdges $ map edge_for $ Map.toList m
    sorted_vertices = G.topSort graph
    reachable_vertices =
      Set.fromList $ concatMap (G.reachable graph) $
      catMaybes $ map vertexFromKey initial
    sorted_reachable_vertices =
      filter (flip Set.member reachable_vertices) sorted_vertices
    vinfo i = (v, x) where (x, v, _) = nodeFromVertex i

function_defs :: [FctName] -> [[String]]
function_defs fctnames =
  map snd $ reverse $ topo_sort_map extra_fcts_map fctnames

used_fcts :: StanAST -> [FctName]
used_fcts x =
  sortUniq $ concatMap expr2ufs $
  edata ++ exdata ++ eparms ++ exparms ++ emodel
  where
    expr2ufs e = case e of
      Var _ -> []
      Lit _ -> []
      Apply f args -> f : concatMap expr2ufs args
    edata = vbts2exprs $ stdata x
    exdata = vbtes2exprs $ stxdata x
    eparms = vbts2exprs $ stparms x
    exparms = vbtes2exprs $ stxparms x
    emodel = model2exprs $ stmodel x
    vbts2exprs = concatMap (bt2exprs . snd)
    bt2exprs (Type et shape, bnds) =
      et2exprs et ++ b2exprs bnds ++ shape
    et2exprs et = case et of
      IntType -> []
      RealType -> []
      VectorType e -> [e]
      RowVectorType e -> [e]
      MatrixType e1 e2 -> [e1, e2]
    b2exprs (Bounds lo hi) = maybeToList lo ++ maybeToList hi
    vbtes2exprs = concatMap vbte2exprs
    vbte2exprs ((_, bt), e) = e : bt2exprs bt
    model2exprs me =
      concatMap medef2exprs (medefs me) ++
      concatMap medraw2exprs (medraws me) ++
      ssm2exprs (messm me)
    medef2exprs (vt, e) = e : vt2exprs vt
    medraw2exprs (_, de) = de2exprs de
    ssm2exprs ssm = map ($ ssm) [matZ', vecH, matT, matQ, veca0, matP0]
    vt2exprs (_, Type et shape) = et2exprs et ++ shape
    de2exprs (DistrExpr dfct args bnds) = Apply dfct args : b2exprs bnds

wrap :: String -> [String] -> [String]
wrap block_name src_lines =
  [ block_name ++ " {" ] ++ map indent src_lines ++ [ "}" ]
  where indent = ("  " ++)

stan_var_decl :: BoundedTypedVar -> String
stan_var_decl x  = stan_vd x ++ ";"

stan_var_def :: (BoundedTypedVar, Expr) -> String
stan_var_def (x, e) = stan_vd x ++ printf " = %s;" (ecode e)

stan_vd :: BoundedTypedVar -> String
stan_vd (varname, (Type elemtype sizes, bnds)) =
  printf "%s %s%s" (etcode elemtype bnds) (vcode varname) (shapecode sizes)

etcode :: ElemType Expr -> Bounds -> String
etcode IntType bnds = "int" ++ bcode bnds
etcode RealType bnds = "real" ++ bcode bnds
etcode (VectorType len) bnds =
  printf "vector%s[%s]" (bcode bnds) (ecode len)
etcode (RowVectorType len) bnds =
  printf "row_vector%s[%s]" (bcode bnds) (ecode len)
etcode (MatrixType nrow ncol) bnds =
  printf "matrix%s[%s, %s]" (bcode bnds) (ecode nrow) (ecode ncol)

bcode :: Bounds -> String
bcode (Bounds Nothing   Nothing)   = ""
bcode (Bounds (Just lo) Nothing)   = "<lower = " ++ ecodebnd lo ++ ">"
bcode (Bounds Nothing   (Just hi)) = "<upper = " ++ ecodebnd hi ++ ">"
bcode (Bounds (Just lo) (Just hi)) =
  "<lower = " ++ ecodebnd lo ++ ", upper = " ++ ecodebnd hi ++ ">"

ecodebnd :: Expr -> String
ecodebnd = ecodep 50

shapecode :: [Expr] -> String
shapecode [] = ""
shapecode sizes = "[" ++ commasep (map ecode sizes) ++ "]"

ecode :: Expr -> String
ecode = ecodep maxBound

ecodep :: Int -> Expr -> String -- ecode with context precedence
ecodep _ (Var v) = vcode v
ecodep _ (Lit (IntVal n)) = litcode n
ecodep _ (Lit (RealVal x)) = litcode x
ecodep n (Apply fctname args) =
  case Map.lookup fctname stanEnv of
    Just f  -> f n args
    Nothing -> error $ "Unknown Stan function name: " ++ fctname

litcode :: (Ord a, Num a, Show a) => a -> String
litcode x
  | x < 0     = "(" ++ show x ++ ")"  -- Just to be safe...
  | otherwise = show x

vcode :: VarName -> String
vcode v = v ++ "_"  -- Ensures no clash with any Stan reserved name.

stanEnv :: Map FctName (Int -> [Expr] -> String)
stanEnv = Map.fromList $
  [ ("{}", \_ args -> printf "{%s}" $ argscode args)
  , ("[]", \_ (hd:tl) -> printf "%s[%s]" (ecodep 0 hd) (argscode tl))
  , ("'", \_ [a] -> ecodep 0 a ++ "'")
  , ("!", unopcode 10 "!") 
  , ("negate", unopcode 10 "-")
  ]
  ++
  map (keyval1 rbinopcode)
  [ ("^", 5) ]
  ++
  map (keyval1 lbinopcode)
  [ (".*", 20), ("./", 20), ("\\", 30), ("*", 40), ("/", 40), ("%", 40)
  , ("+", 50), ("-", 50), ("<", 60), ("<=", 60), (">", 60), (">=", 60)
  , ("==", 70), ("!=", 70), ("&&", 80), ("||", 90)
  ]
  ++
  map (keyval fctcode)
  [ "pi", "e", "sqrt2", "log2", "log10" -- 0-arg functions
  , "min", "max", "fmin", "fmax", "abs", "fabs", "floor", "ceil", "round"
  , "trunc", "sqrt", "cbrt"
  , "square", "exp", "exp2", "log", "log2", "log10", "inv", "inv_sqrt"
  , "inv_square", "cos", "sin", "tan", "acos", "asin", "atan", "cosh", "sinh"
  , "tanh", "acosh", "asinh", "atanh", "logit", "inv_logit", "inv_cloglog"
  , "erf", "erfc", "Phi", "inv_Phi", "Phi_approx", "tgamma", "lgamma"
  , "digamma", "trigamma", "expm1", "log1p", "log1m", "log1p_exp", "log1m_exp"
  , "log_inv_logit", "log1m_inv_logit", "int_step", "step", "fdim", "fmod"
  , "hypot", "atan2", "binary_log_loss", "owens_t", "inc_beta", "lbeta"
  , "lmgamma", "gamma_p", "gamma_q", "choose", "bessel_first_kind"
  , "bessel_second_kind", "modified_bessel_first_kind"
  , "modified_bessel_second_kind", "falling_factorial", "lchoose"
  , "log_falling_factorial", "rising_factorial", "log_rising_factorial"
  , "fma", "lmultiply", "log_diff_exp", "log_mix", "log_sum_exp", "dims"
  , "size", "num_elements", "rows", "cols", "rep_array", "rep_vector"
  , "rep_row_vector", "rep_matrix", "diag_matrix", "append_col", "append_row"
  , "to_matrix", "to_vector", "to_row_vector", "to_array_1d", "diagonal"
  , "col", "row", "sub_col", "sub_row", "head", "tail", "segment", "sum"
  , "prod", "log_sum_exp", "mean", "variance", "sd", "diag_pre_multiply"
  , "diag_post_multiply", "tcrossprod", "crossprod"
  , "multiply_lower_tri_self_transpose", "dot_product", "columns_dot_product"
  , "rows_dot_product", "dot_self", "columns_dot_self", "rows_dot_self"
  , "distance", "squared_distance", "quad_form", "quad_form_diag"
  , "quad_form_sym", "trace_quad_form", "softmax", "log_softmax"
  , "cumulative_sum", "cov_exp_quad", "mdivide_left_tri_low"
  , "mdivide_right_tri_low", "mdivide_left_spd", "mdivide_right_spd"
  , "matrix_exp", "trace", "determinant", "log_determinant", "inverse"
  , "inverse_spd", "eigenvalues_sym", "eigenvectors_sym", "qr_Q", "qr_R"
  , "cholesky_decompose", "singular_values", "sort_asc", "sort_desc"
  , "sort_indices_asc", "sort_indices_desc", "rank"    
  ]
  ++
  map (keyval fctcode) (Map.keys extra_fcts_map)
  where
    keyval f name = (name, f name)
    keyval1 f x = (fst x, f x)

fctcode :: FctName -> Int -> [Expr] -> String
fctcode fctname _ args = printf "%s(%s)" fctname $ argscode args

argscode :: [Expr] -> String
argscode args = commasep $ map ecode args

paren :: Bool -> String -> String
paren False str = str
paren True str = "(" ++ str ++ ")"

unopcode :: Int -> FctName -> Int -> [Expr] -> String
unopcode prec op = \n [a] ->
  paren (n < prec) $ op ++ ecodep 0 a

-- left-associative binary operator
lbinopcode :: (FctName, Int) -> Int -> [Expr] -> String
lbinopcode (name, p) =
  let sep = printf " %s " name
      pm1 = p - 1
  in \n (hd:tl) ->
       paren (n < p) $ intercalate sep $ (ecodep p hd) : map (ecodep pm1) tl

-- right-associative binary operator
rbinopcode :: (FctName, Int) -> Int -> [Expr] -> String
rbinopcode (name, p) =
  let sep = printf " %s " name
      pm1 = p - 1
  in \n-> (\(hd:tl)->
             paren (n < p) $ intercalate sep $ reverse $
            (ecodep p hd) : map (ecodep pm1) tl
          ) . reverse

extra_fcts_map :: Map FctName ([FctName], [String])
extra_fcts_map = Map.fromList
  [ entry "scale_matrices" [] [s|
      matrix[] scale_matrices(real mult, matrix [] matrices) {
        int N = size(matrices);
        if (N == 0)
          return matrices;
        {
          int nr = rows(matrices[1]);
          int nc = cols(matrices[1]);
          matrix[nr, nc] result[N];
          for (i in 1:N)
            result[i] = mult * matrices[i];
          return result;
        }
      }
    |]
  , entry "sum_nrows" [] [s|
      int sum_nrows(matrix [] M) {
        int n = 0;
        int N = size(M);
        for (i in 1:N)
          n += rows(M[i]);
        return n;
      }
    |]
  , entry "sum_ncols" [] [s|
      int sum_ncols(matrix [] M) {
        int n = 0;
        int N = size(M);
        for (i in 1:N)
          n += cols(M[i]);
        return n;
      }
    |]
  , entry "block_diag" ["sum_nrows", "sum_ncols"] [s|
      matrix block_diag(matrix [] M) {
        int nr = sum_nrows(M);
        int nc = sum_ncols(M);
        matrix[nr, nc] result = rep_matrix(0, nr, nc);
        int N = size(M);
        int i = 0;
        int j = 0;
        for (k in 1:N) {
          int nrk = rows(M[k]);
          int nck = cols(M[k]);
          result[(i+1):(i+nrk), (j+1):(j+nck)] = M[k];
          i += nrk;
          j += nck;
        }
        return result;
      }
    |]
  , entry "sum_lengths" [] [s|
      int sum_lengths(vector [] va) {
        int n = 0;
        for (i in 1:size(va))
          n += num_elements(va[i]);
        return n;
      }
    |]
  , entry "vec_append" ["sum_lengths"] [s|
      vector vec_append(vector [] va) {
        int n = size(va);
        if (n == 0) return rep_vector(0.0, 0);
        if (n == 1) return va[1];
        {
          int sz = sum_lengths(va);
          vector[sz] v;
          int bi;
          int ei = 0;
          for (k in 1:n) {
            bi = ei + 1;
            ei += num_elements(va[k]);
            v[bi:ei] = va[k];
          }
          return v;
        }
      }
    |]
  , entry "polynomial" [] [s|
      real polynomial(real x, real[] coeffs) {
        real y = 0;
        real xpow = 1;
        for (i in 1:size(coeffs)) {
          y += xpow * coeffs[i];
          xpow *= x;
        }
        return (y);
      }                            
    |]
  , entry "exponential_mt_rate" ["polynomial"] [s|
      real exponential_mt_rate(real normalized_mean) {
        // This would be simpler if recursion were allowed...
        real mu;
        real sign;
        real res0;
        if (normalized_mean <= 0)
          reject("exponential_mt_rate: parameter must be positive, but is ",
                 normalized_mean, ".");
        if (normalized_mean >= 1)
          reject("exponential_mt_rate: parameter must be less than 1, but is ",
                 normalized_mean, ".");
        if (normalized_mean > 0.5) {
          mu = 1.0 - normalized_mean;
          sign = -1.0;
        }
        else {
          mu = normalized_mean;
          sign = 1.0;
        }
        if (mu < 4.54545451755986457121e-02)
          res0 = inv(mu);
        else if (mu < 2.10988429519736647721e-01)
          res0 = polynomial(2.10988429519736647721e-01 - mu,
                            { 9.49448609949615329739e-01,
                              1.04866432774455753396e+00,
                              -6.42959435928104205971e+00,
                              3.79762444624978590113e+00,
                              6.11881450074166792774e+01,
                              -1.48309894287500156906e+02,
                              -2.97228418317228170054e+03,
                              6.97728216979455610272e+04,
                              -4.46472170428893645294e+05,
                              8.96230566675862530246e+05 }) / mu;
        else
          res0 = polynomial(0.5 - mu,
                            { -3.36262872290467608517e-06,
                              1.20012123407418513921e+01,
                              -1.06585476744743931632e-01,
                              3.27768615844976523022e+01,
                              -7.77323727482908424236e+01,
                              9.86330022949583849368e+02,
                              -5.95443311922654356749e+03,
                              2.45388908776985881559e+04,
                              -5.40960590256227224017e+04,
                              5.49423597728985769209e+04 });
        return sign * res0;
      }
    |]
  , entry "blocks4" [] [s|
      matrix blocks4(matrix A, matrix B, matrix C, matrix D) {
        int m1 = rows(A);
        int n1 = cols(A);
        int m2 = rows(D);
        int n2 = cols(D);
        if (rows(B) != m1)
          reject("A and B must have same number of rows in blocks()");
        if (cols(B) != n2)
          reject("B and D must have same number of columns in blocks()");
        if (rows(C) != m2)
          reject("C and D must have same number of rows in blocks()");
        if (cols(C) != n1)
          reject("A and C must have same number of columns in blocks()");
        {
          matrix[m1 + m2, n1 + n2] M;
          // A B
          // C D
          M[1:m1, 1:n1] = A;
          M[1:m1, (n1 + 1):(n1 + n2)] = B;
          M[(m1 + 1):(m1 + m2), 1:n1] = C;
          M[(m1 + 1):(m1 + m2), (n1 + 1):(n1 + n2)] = D;

          return M;
        }
      }
    |]
  ]
  where
    entry name dependencies def = (name, (dependencies, normlines def))

-- Convert a string into a list of lines, normalizing the result by
-- removing leading and trailing all-whitespace lines, replacing any
-- remaining all-whitespace lines with empty lines, then un-indenting
-- by the max amount that can be applied to all nonempty lines.
normlines :: String -> [String]
normlines str = stripnulls $ map (drop n) strs'
  where n = minimum $ (maxBound::Int) : indents
        indents = map (length . takeWhile isSpace) $ filter (not .null) strs'
        stripnulls xs = reverse $ dropWhile null $ reverse $ dropWhile null xs
        strs' = map (\x->if all isSpace x then "" else x) $ lines str

extra_fcts_stanprog :: String
extra_fcts_stanprog = fctsblock_string ++ rest
  where
    fctsblock_string =
      concatMap (++ "\n") $ wrap "functions" $ intercalate [""] $
      function_defs $ Map.keys extra_fcts_map
    rest = [s|
data {
  int<lower = 1> N;
  vector[N] y;
}
parameters {
  real<lower=0> sigma;
}
model {
  sigma ~ normal(0, 1) T[0, ];
  y ~ normal(0, sigma);
}
|]
