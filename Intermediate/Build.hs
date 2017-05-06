-- Jakub Staroń, 2017

{-# LANGUAGE GADTs, DataKinds, RankNTypes, FlexibleInstances #-}

module Intermediate.Build (fromAST) where

import Data.Int (Int32)
import Data.List (foldl', uncons)
import Data.Maybe (fromMaybe)
import Control.Exception.Base (assert)
import Control.Monad (void)
import Control.Monad.Writer.Strict (Writer, runWriter, tell)

import Debug.Trace (trace)

import Intermediate
import FormatString (format)
import qualified AST
import qualified Data.Map.Strict as Map

-- Intermediate monad definition
type IntermediateBuilder = Writer [Function]

runIntermediateBuilder :: IntermediateBuilder a -> (a, [Function])
runIntermediateBuilder = runWriter

finalizeFunction :: IntermediateBuilder Function -> IntermediateBuilder ()
finalizeFunction = (=<<) (\a -> tell [a])
-- Intermediate monad definition end

-- Enviroment definition
data FunctionEntry =
  FunctionEntry
  { uid :: String
  , resultType :: AST.Type }

data Variable =
  Variable
  { variableType :: AST.Type
  , path :: [Int32] }

type VEnv = Map.Map AST.Ident Variable
type FEnv = Map.Map AST.Ident FunctionEntry
data Env =
  Env
  { functionEntries :: FEnv
  , functionCounter :: Int
  , variables :: VEnv }
-- Enviroment definition end

-- Initial enviroments
initialFEnv :: FEnv
initialFEnv = Map.fromList [("readI32", readI32), ("writeI32", writeI32)] where
  readI32 = FunctionEntry "readI32" AST.I32
  writeI32 = FunctionEntry "writeI32" AST.unit

initialVEnv :: VEnv
initialVEnv = Map.empty

initialEnv :: Env
initialEnv = Env initialFEnv 0 initialVEnv
-- Initial enviroments end

insertVariable :: AST.Ident -> Variable -> Env -> Env
insertVariable name variable env = env' where
  env' = env { variables = variables' }
  variables' = Map.insert name variable $ variables env

insertPattern :: AST.Pattern -> AST.Type -> Env -> Env
insertPattern pattern valueType = insertPattern_ pattern valueType [] where
  insertPattern_ :: AST.Pattern -> AST.Type -> [Int32] -> Env -> Env
  insertPattern_ pattern valueType path = case pattern of
    AST.PatternVariable name            -> insertVariable name Variable { variableType = valueType, path = path }
    AST.PatternMutableVariable name     -> insertVariable name Variable { variableType = valueType, path = path }
    AST.PatternIgnore                   -> id
    AST.PatternTuple pattern'           ->
      let AST.Tuple types = valueType in
      let subpatterns = map (\(p, t, i) -> insertPattern_ p t (i : path)) $ zip3 pattern' types [0..] in
      foldl' (.) id subpatterns

variableNamesFromPattern :: AST.Pattern -> [AST.Ident]
variableNamesFromPattern pattern = case pattern of
  AST.PatternVariable name            -> [name]
  AST.PatternMutableVariable name     -> [name]
  AST.PatternIgnore                   -> []
  AST.PatternTuple pattern'           -> concatMap variableNamesFromPattern pattern'

fromAST :: AST.Program -> Program
fromAST program = Program { functions = fs, mainUid = u } where
  (env, fs) = runIntermediateBuilder $ fromProgram program
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
fromProgram = insertFunctions initialEnv . AST.functions

fromFunction :: Env -> AST.FunctionDeclaration -> IntermediateBuilder Function
fromFunction env function = do
  let bindingsAsTuple = AST.PatternTuple $ map AST.pattern $ AST.parameters function
  let parametersAsTuple = AST.Tuple $ map AST.valueType $ AST.parameters function
  let bindingsInserter = insertPattern bindingsAsTuple parametersAsTuple
  -- Clean variables environment for compiling new function.
  env <- return $ env { variables = initialVEnv }
  -- Insert parameter bindings
  env <- return $ bindingsInserter env
  body <- fromBlock env (AST.body function)

  assert (typeOf body == AST.resultType function) $ return Function { name = uid $ findFunction env $ AST.name function, body = body, parameter = parametersAsTuple, bindings = Map.toList $ path <$> variables env }

-- Heteromorphic expression for handling Expr a.
type HeteromorphicExpr = Either (Expr 'LValue) (Expr 'RValue)

instance TypeOf HeteromorphicExpr where
  typeOf = either typeOf typeOf

buildRValueExpr :: Either (Expr 'LValue) (Expr 'RValue) -> Expr 'RValue
buildRValueExpr = either Materialize id

getRValueExpr :: Either (Expr 'LValue) (Expr 'RValue) -> Expr 'RValue
getRValueExpr = either (error "Trying to get Expr 'RValue from Left $ Expr 'LValue.") id

getLValueExpr :: Either (Expr 'LValue) (Expr 'RValue) -> Expr 'LValue
getLValueExpr = either id $ error "Trying to get Expr 'LValue from Right $ Expr 'RValue."

fromWhateverExpr :: (forall a . Expr a -> b) -> HeteromorphicExpr -> b
fromWhateverExpr f = either f f

fromWhateverExpr_ :: (forall a . Expr a -> Expr a) -> HeteromorphicExpr -> HeteromorphicExpr
fromWhateverExpr_ f (Left e) = Left $ f e
fromWhateverExpr_ f (Right e) = Right $ f e

class ToHeteromorphic a where
  toHeteromorphic :: a -> HeteromorphicExpr

instance ToHeteromorphic (Expr 'LValue) where
  toHeteromorphic = Left

instance ToHeteromorphic (Expr 'RValue) where
  toHeteromorphic = Right
-- End of heteromorphic expression definition.

fromBlock :: Env -> AST.Block -> IntermediateBuilder (Expr 'RValue)
fromBlock env block@(AST.Block stmts expr) =
  let functions = AST.getFunctionsFromBlock block in
  fromBlockSuffix env stmts expr

fromBlockSuffix :: Env -> [AST.Stmt] -> AST.Expr -> IntermediateBuilder (Expr 'RValue)
fromBlockSuffix env stmts expr = case uncons stmts of
  Nothing -> buildRValueExpr <$> fromExpr env expr
  Just (stmt, stmts) -> fromStmt env stmt (stmts, expr)

fromStmt :: Env -> AST.Stmt -> ([AST.Stmt], AST.Expr) -> IntermediateBuilder (Expr 'RValue)
fromStmt env stmt suffix =
  (uncurry $ fromBlockSuffix env) suffix >>= \s -> let compiledSuffix = s in
  case stmt of
    AST.FunDeclStmt funDecl                     -> return compiledSuffix
    AST.If condExpr block                       -> do
      condExpr <- fromExpr env condExpr
      block <- fromBlock env block
      let if_ = fromWhateverExpr If condExpr block
      return $ Sequence if_ compiledSuffix
    AST.Stmt expr                               -> do
      expr <- fromExpr env expr
      return $ fromWhateverExpr Sequence expr compiledSuffix
    AST.StrictStmt expr                         -> undefined
    AST.Loop block                              -> do
      block <- fromBlock env block
      let loop = Loop Forever block
      return $ Sequence loop compiledSuffix
    AST.While condExpr block                    -> do
      condExpr <- fromExpr env condExpr
      block <- fromBlock env block
      let loop = Loop (fromWhateverExpr While condExpr) block
      return $ Sequence loop compiledSuffix
    AST.IterableForLoop name initExpr block     -> undefined
    AST.RangeForLoop name beginExpr endExpr block       -> do
      beginExpr <- buildRValueExpr <$> fromExpr env beginExpr
      endExpr <- buildRValueExpr <$> fromExpr env endExpr
      env <- return $ insertVariable name Variable { variableType = AST.I32, path = [] } env
      block <- fromBlock env block
      let loop = Loop (ForRange name beginExpr endExpr) block
      return $ Sequence loop compiledSuffix
    AST.Break                                   ->
      return $ Sequence (FlowControl Break) compiledSuffix
    AST.Continue                                ->
      return $ Sequence (FlowControl Continue) compiledSuffix
    AST.LetStmt pattern valueType initExpr      -> do
      initExpr <- fromExpr env initExpr
      initExpr <- return $ buildRValueExpr initExpr
      env <- return $ insertPattern pattern (typeOf initExpr) env
      compiledSuffix <- (uncurry $ fromBlockSuffix env) suffix
      let names = variableNamesFromPattern pattern
      let bindings = map (\n -> (n, path $ findVariable env n)) names
      return $ BindVariables bindings initExpr compiledSuffix

fromExpr :: Env -> AST.Expr -> IntermediateBuilder HeteromorphicExpr
fromExpr env expr = case expr of
  AST.BinaryOperator lhs rhs kind         -> do
    lhs <- buildRValueExpr <$> fromExpr env lhs
    rhs <- buildRValueExpr <$> fromExpr env rhs
    let (valueType, name) = binaryOperatorInfo kind
    return $ toHeteromorphic $ FunctionCall valueType name $ Tuple [lhs, rhs]
  AST.UnaryOperator   expr kind           -> do
    expr <- buildRValueExpr <$> fromExpr env expr
    let (valueType, name) = unaryOperatorInfo kind
    return $ toHeteromorphic $ FunctionCall valueType name $ Tuple [expr]
  AST.Identifier      name                -> return $ Left $ Identifier (variableType $ findVariable env name) name
  AST.Equal lhs rhs                       -> do
    lhs <- fromExpr env lhs
    rhs <- fromExpr env rhs
    return $ toHeteromorphic $ fromWhateverExpr (fromWhateverExpr Equal lhs) rhs
  AST.NotEqual lhs rhs                    -> do
    lhs <- buildRValueExpr <$> fromExpr env lhs
    rhs <- buildRValueExpr <$> fromExpr env rhs
    return $ toHeteromorphic $ FunctionCall AST.Bool "$not" $ Tuple [Equal lhs rhs]
  AST.Assign lhs rhs                      -> do
    lhs <- fromExpr env lhs
    rhs <- fromExpr env rhs
    lhs <- return $ getLValueExpr lhs
    return $ toHeteromorphic $ fromWhateverExpr (Assign lhs) rhs
  AST.ArrayLookup arrayExpr indexExpr     -> do
    arrayExpr <- fromExpr env arrayExpr
    indexExpr <- fromExpr env indexExpr
    return $ fromWhateverExpr_ (fromWhateverExpr (flip ArrayLookup) indexExpr) arrayExpr
  AST.Dereference expr                    -> undefined
  AST.Borrow expr                         -> undefined
  AST.MutableBorrow expr                  -> undefined
  AST.LiteralExpr     literal             -> return $ Right $ Literal literal
  AST.FunctionCall    ident exprs         -> do
    exprs <- mapM (fromExpr env) exprs
    exprs <- return $ map buildRValueExpr exprs
    let function = findFunction env ident
    return $ toHeteromorphic $ FunctionCall (resultType function) (uid function) $ Tuple exprs
  AST.TupleLookup     tupleExpr count     -> undefined
  AST.ArrayElements   exprs               -> undefined
  AST.ArrayRepeat     initExpr count      -> do
    initExpr <- buildRValueExpr <$> fromExpr env initExpr
    return $ toHeteromorphic $ Array $ Repeat initExpr count
  AST.ArrayRange      begin end           -> undefined
  AST.TupleConstruct  exprs               -> do
    exprs <- mapM (fromExpr env) exprs
    exprs <- return $ map buildRValueExpr exprs
    return $ toHeteromorphic $ Tuple exprs
  AST.BlockExpr       block               -> undefined
  AST.IfElse          condExpr trueBlock falseBlock -> do
    condExpr <- fromExpr env condExpr
    trueBlock <- fromBlock env trueBlock
    falseBlock <- fromBlock env falseBlock
    return $ toHeteromorphic $ fromWhateverExpr IfElse condExpr trueBlock falseBlock

-- Result types and replacement function names for build-in operators.
binaryOperatorInfo :: AST.BinaryOperatorKind -> (AST.Type, AST.Ident)
binaryOperatorInfo kind = case kind of
  AST.Or        -> (AST.Bool, "$or")
  AST.And       -> (AST.Bool, "$and")
  AST.Less      -> (AST.Bool, "$less")
  AST.Add       -> (AST.I32,  "$add")
  AST.Subtract  -> (AST.I32,  "$subtract")
  AST.Multiply  -> (AST.I32,  "$multiply")
  AST.Divide    -> (AST.I32,  "$divide")
  AST.Modulo    -> (AST.I32,  "$modulo")

unaryOperatorInfo :: AST.UnaryOperatorKind -> (AST.Type, AST.Ident)
unaryOperatorInfo AST.Not    = (AST.Bool, "$not")
unaryOperatorInfo AST.Negate = (AST.I32,  "$negate")
