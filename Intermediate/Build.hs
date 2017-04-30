{-# LANGUAGE GADTs, KindSignatures, DataKinds, RankNTypes, FlexibleInstances #-}

module Intermediate.Build(fromAST) where

import Data.Int (Int32)
import Data.List (foldl', uncons)
import Data.Maybe (fromMaybe)
import Control.Monad (void)

import Intermediate
import FormatString
import qualified AST
import qualified Data.Map.Strict as Map

newtype IntermediateBuilder a = IntermediateBuilder ([Function], a)

instance Functor IntermediateBuilder where
  fmap f ma = IntermediateBuilder (fs, f a) where
    IntermediateBuilder (fs, a) = ma

instance Applicative IntermediateBuilder where
  pure a = IntermediateBuilder ([], a)
  (<*>) mf ma = IntermediateBuilder (fs ++ fs', f a) where
    IntermediateBuilder (fs, f) = mf
    IntermediateBuilder (fs', a) = ma

instance Monad IntermediateBuilder where
  (>>=) ma f = IntermediateBuilder (fs ++ fs', b) where
    IntermediateBuilder (fs, a) = ma
    IntermediateBuilder (fs', b) = f a

runIntermediateBuilder :: IntermediateBuilder a -> ([Function], a)
runIntermediateBuilder ma = (fs, a) where
  IntermediateBuilder (fs, a) = ma

finalizeFunction :: IntermediateBuilder Function -> IntermediateBuilder ()
finalizeFunction ma = IntermediateBuilder (f : fs, ()) where
  IntermediateBuilder (fs, f) = ma

data FunctionEntry = FunctionEntry { uid :: String, resultType :: AST.Type }
data Variable = Variable { variableType :: AST.Type, path :: [Int32] }

type VEnv = Map.Map AST.Ident Variable
type FEnv = Map.Map AST.Ident FunctionEntry
data Env =
  Env
  { functionEntries :: FEnv
  , functionCounter :: Int
  , variables :: VEnv }

insertVariable :: AST.Ident -> Variable -> Env -> Env
insertVariable name variable env = env' where
  env' = env { variables = variables' }
  variables' = Map.insert name variable $ variables env

insertPattern :: AST.Pattern -> AST.Type -> Env -> Env
insertPattern pattern valueType env = insertPattern_ pattern valueType [] env where
  insertPattern_ :: AST.Pattern -> AST.Type -> [Int32] -> Env -> Env
  insertPattern_ pattern valueType path env = case pattern of
    AST.PatternVariable name            -> insertVariable name Variable { variableType = valueType, path = path } env
    AST.PatternMutableVariable name     -> insertVariable name Variable { variableType = valueType, path = path } env
    AST.PatternIgnore                   -> env
    AST.PatternTuple pattern'           ->
      let AST.Tuple types = valueType in
      let subpatterns = map (\(p, t, i) -> insertPattern_ p t (i : path)) $ zip3 pattern' types [0..] in
      foldl' (.) id subpatterns env

variableNamesFromPattern :: AST.Pattern -> [AST.Ident]
variableNamesFromPattern pattern = case pattern of
  AST.PatternVariable name            -> [name]
  AST.PatternMutableVariable name     -> [name]
  AST.PatternIgnore                   -> []
  AST.PatternTuple pattern'           -> concat $ map variableNamesFromPattern pattern'

fromAST :: AST.Program -> Program
fromAST program = Program { functions = fs, mainUid = u } where
  (fs, env) = runIntermediateBuilder $ fromProgram program
  u = uid $ findFunction env "main"

enumerate :: [a] -> [(a, Int)]
enumerate = flip zip [0..]

addFunctionEntry :: Env -> AST.FunctionDeclaration -> Env
addFunctionEntry env function = env { functionEntries = Map.insert (AST.name function) entry (functionEntries env) } where
  entry = FunctionEntry { uid = format "%0#%1" [AST.name function, show $ functionCounter env], resultType = AST.resultType function }

findFunction :: Env -> AST.Ident -> FunctionEntry
findFunction env name =
  fromMaybe
  (error $ format "Internal interpreter error: function \"%0\" not found." [name])
  (Map.lookup name $ functionEntries env)

findVariable :: Env -> AST.Ident -> Variable
findVariable env name =
  fromMaybe
  (error $ format "Internal interpreter error: variable \"%0\" not found." [name])
  (Map.lookup name $ variables env)

insertFunctions :: Env -> [AST.FunctionDeclaration] -> IntermediateBuilder Env
insertFunctions env functions =
  let env' = foldl' addFunctionEntry env functions in do
    mapM_ (finalizeFunction . fromFunction env' { functionCounter = 0} ) functions
    return env'

fromProgram :: AST.Program -> IntermediateBuilder Env
fromProgram program =
  let initialEnv = Env Map.empty 0 Map.empty in
  insertFunctions initialEnv (AST.functions program)

fromFunction :: Env -> AST.FunctionDeclaration -> IntermediateBuilder Function
fromFunction env function = do
  let parameters' = map (\funParam -> insertPattern (AST.pattern funParam) (AST.valueType funParam)) $ AST.parameters function
  env <- return $ env { variables = Map.empty }
  env <- return $ (foldl' (.) id parameters') env
  expr <- fromBlock env (AST.body function)

  return Function { name = uid $ findFunction env $ AST.name function, body = expr}

type HeteromorphicExpr = Either (Expr 'LValue) (Expr 'RValue)

instance TypeOf HeteromorphicExpr where
  typeOf = either typeOf typeOf

buildRValueExpr :: Either (Expr 'LValue) (Expr 'RValue) -> Expr 'RValue
buildRValueExpr = either Materialize id

getRValueExpr :: Either (Expr 'LValue) (Expr 'RValue) -> Expr 'RValue
getRValueExpr = either (error "Trying to get Expr 'RValue from Left $ Expr 'LValue.") id

fromWhateverExpr :: (forall a . Expr a -> b) -> HeteromorphicExpr -> b
fromWhateverExpr f = either f f

fromBlock :: Env -> AST.Block -> IntermediateBuilder (Expr 'RValue)
fromBlock env block@(AST.Block stmts expr) =
  let functions = AST.getFunctionsFromBlock block in
  fromBlockSuffix env stmts expr

fromBlockSuffix :: Env -> [AST.Stmt] -> AST.Expr -> IntermediateBuilder (Expr 'RValue)
fromBlockSuffix env stmts expr = case uncons stmts of
  Nothing -> fromExpr env expr >>= return.buildRValueExpr
  Just (stmt, stmts) -> do
    suffix <- fromBlockSuffix env stmts expr
    fromStmt env stmt suffix

fromStmt :: Env -> AST.Stmt -> (Expr 'RValue) -> IntermediateBuilder (Expr 'RValue)
fromStmt env stmt suffix = case stmt of
  AST.FunDeclStmt funDecl                     -> undefined
  AST.If expr block                           -> undefined
  AST.Stmt expr                               -> do
    expr <- fromExpr env expr
    return $ fromWhateverExpr Sequence expr suffix
  AST.StrictStmt expr                         -> undefined
  AST.Loop block                              -> undefined
  AST.While expr block                        -> do
    expr <- fromExpr env expr
    block <- fromBlock env block
    let loop = Loop (fromWhateverExpr While expr) block
    return $ Sequence loop suffix
  AST.IterableForLoop ident expr block        -> undefined
  AST.RangeForLoop ident expr1 expr2 block    -> undefined
  AST.Break                                   -> undefined
  AST.Continue                                -> undefined
  AST.LetStmt pattern valueType expr'          -> do
    expr' <- fromExpr env expr'
    expr' <- return $ buildRValueExpr expr'
    env <- return $ insertPattern pattern (typeOf expr') env
    names <- return $ variableNamesFromPattern pattern
    bindings <- return $ map (\n -> (n, path $ findVariable env n)) names
    return $ BindVariables bindings expr' suffix

fromExpr :: Env -> AST.Expr -> IntermediateBuilder HeteromorphicExpr
fromExpr env expr = case expr of
  AST.BinaryOperator  expr1 expr2 kind    -> undefined
  AST.UnaryOperator   expr kind           -> undefined
  AST.Identifier      ident               -> return $ Left $ Identifier (variableType $ findVariable env ident) ident
  AST.Equal expr1 expr2                   -> undefined
  AST.NotEqual expr1 expr2                -> do
    expr1 <- fromExpr env expr1
    expr2 <- fromExpr env expr2
    expr1 <- return $ buildRValueExpr expr1
    expr2 <- return $ buildRValueExpr expr2
    return $ Right $ FunctionCall AST.Bool "$not" [Equal expr1 expr2]
  AST.Assign expr1 expr2                  -> undefined
  AST.ArrayLookup expr1 expr2             -> undefined
  AST.Dereference expr                    -> undefined
  AST.Borrow expr                         -> undefined
  AST.MutableBorrow expr                  -> undefined
  AST.LiteralExpr     literal             -> return $ Right $ Literal literal
  AST.FunctionCall    ident exprs         -> do
    exprs <- mapM (fromExpr env) exprs
    exprs <- return $ map buildRValueExpr exprs
    let function = findFunction env ident
    return $ Right $ FunctionCall (resultType function) (uid function) exprs
  AST.TupleLookup     expr n              -> undefined
  AST.ArrayElements   exprs               -> undefined
  AST.ArrayRepeat     expr integer        -> undefined
  AST.ArrayRange      begin end           -> undefined
  AST.TupleConstruct  exprs               -> do
    exprs <- mapM (fromExpr env) exprs
    exprs <- return $ map buildRValueExpr exprs
    return $ Right $ Tuple exprs
  AST.BlockExpr       block               -> undefined
  AST.IfElse          expr block1 block2  -> undefined
