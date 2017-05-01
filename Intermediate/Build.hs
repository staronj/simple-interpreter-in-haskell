{-# LANGUAGE GADTs, KindSignatures, DataKinds, RankNTypes, FlexibleInstances #-}

module Intermediate.Build(fromAST) where

import Data.Int (Int32)
import Data.List (foldl', uncons)
import Data.Maybe (fromMaybe)
import Control.Exception.Base (assert)
import Control.Monad (void)

import Debug.Trace (trace)

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

initialEnv :: Env
initialEnv = Env functionEntries functionCounter variables where
  functionEntries = Map.fromList [("readI32", readI32), ("writeI32", writeI32)]
  readI32 = FunctionEntry "readI32" AST.I32
  writeI32 = FunctionEntry "writeI32" AST.unit
  functionCounter = 0
  variables = Map.empty

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
fromProgram program = insertFunctions initialEnv (AST.functions program)

fromFunction :: Env -> AST.FunctionDeclaration -> IntermediateBuilder Function
fromFunction env function = do
  let bindingsAsTuple = AST.PatternTuple $ map AST.pattern $ AST.parameters function
  let parametersAsTuple = AST.Tuple $ map AST.valueType $ AST.parameters function
  let bindingsInserter = insertPattern bindingsAsTuple parametersAsTuple
  env <- return $ env { variables = Map.empty }
  env <- return $ bindingsInserter env
  expr <- fromBlock env (AST.body function)


  trace (show expr) $ assert (typeOf expr == AST.resultType function) $ return Function { name = uid $ findFunction env $ AST.name function, body = expr, parameter = parametersAsTuple, bindings = Map.toList $ fmap path $ variables env }

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

fromBlock :: Env -> AST.Block -> IntermediateBuilder (Expr 'RValue)
fromBlock env block@(AST.Block stmts expr) =
  let functions = AST.getFunctionsFromBlock block in
  fromBlockSuffix env stmts expr

fromBlockSuffix :: Env -> [AST.Stmt] -> AST.Expr -> IntermediateBuilder (Expr 'RValue)
fromBlockSuffix env stmts expr = case uncons stmts of
  Nothing -> fromExpr env expr >>= return.buildRValueExpr
  Just (stmt, stmts) -> fromStmt env stmt (stmts, expr)

fromStmt :: Env -> AST.Stmt -> ([AST.Stmt], AST.Expr) -> IntermediateBuilder (Expr 'RValue)
fromStmt env stmt suffix =
  (uncurry $ fromBlockSuffix env) suffix >>= \s -> let compiledSuffix = s in
  case stmt of
    AST.FunDeclStmt funDecl                     -> undefined
    AST.If expr block                           -> undefined
    AST.Stmt expr                               -> do
      expr <- fromExpr env expr
      return $ fromWhateverExpr Sequence expr compiledSuffix
    AST.StrictStmt expr                         -> undefined
    AST.Loop block                              -> undefined
    AST.While expr block                        -> do
      expr <- fromExpr env expr
      block <- fromBlock env block
      let loop = Loop (fromWhateverExpr While expr) block
      return $ Sequence loop compiledSuffix
    AST.IterableForLoop ident expr block        -> undefined
    AST.RangeForLoop ident expr1 expr2 block    -> do
      expr1 <- fmap buildRValueExpr $ fromExpr env expr1
      expr2 <- fmap buildRValueExpr $ fromExpr env expr2
      env <- return $ insertVariable ident Variable { variableType = AST.I32, path = [] } env
      block <- fromBlock env block
      let loop = Loop (ForRange ident expr1 expr2) block
      return $ Sequence loop compiledSuffix
    AST.Break                                   -> undefined
    AST.Continue                                -> undefined
    AST.LetStmt pattern valueType expr'          -> do
      expr' <- fromExpr env expr'
      expr' <- return $ buildRValueExpr expr'
      env <- return $ insertPattern pattern (typeOf expr') env
      compiledSuffix <- (uncurry $ fromBlockSuffix env) suffix
      names <- return $ variableNamesFromPattern pattern
      bindings <- return $ map (\n -> (n, path $ findVariable env n)) names
      return $ BindVariables bindings expr' compiledSuffix

fromExpr :: Env -> AST.Expr -> IntermediateBuilder HeteromorphicExpr
fromExpr env expr = case expr of
  AST.BinaryOperator  expr1 expr2 kind    -> do
    expr1 <- fromExpr env expr1
    expr2 <- fromExpr env expr2
    expr1 <- return $ buildRValueExpr expr1
    expr2 <- return $ buildRValueExpr expr2
    let (valueType, name) = binaryOperatorInfo kind
    return $ Right $ FunctionCall valueType name [expr1, expr2]
  AST.UnaryOperator   expr kind           -> do
    expr <- fmap buildRValueExpr $ fromExpr env expr
    let (valueType, name) = unaryOperatorInfo kind
    return $ Right $ FunctionCall valueType name [expr]
  AST.Identifier      ident               -> return $ Left $ Identifier (variableType $ findVariable env ident) ident
  AST.Equal expr1 expr2                   -> do
    expr1 <- fromExpr env expr1
    expr2 <- fromExpr env expr2
    return $ Right $ fromWhateverExpr (fromWhateverExpr Equal expr1) expr2
  AST.NotEqual expr1 expr2                -> do
    expr1 <- fromExpr env expr1
    expr2 <- fromExpr env expr2
    expr1 <- return $ buildRValueExpr expr1
    expr2 <- return $ buildRValueExpr expr2
    return $ Right $ FunctionCall AST.Bool "$not" [Equal expr1 expr2]
  AST.Assign expr1 expr2                  -> do
    expr1 <- fromExpr env expr1
    expr2 <- fromExpr env expr2
    expr1 <- return $ getLValueExpr expr1
    return $ Right $ fromWhateverExpr (Assign expr1) expr2
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
  AST.IfElse          expr block1 block2  -> do
    expr <- fromExpr env expr
    block1 <- fromBlock env block1
    block2 <- fromBlock env block2
    return $ Right $ fromWhateverExpr IfElse expr block1 block2

binaryOperatorInfo :: AST.BinaryOperatorKind -> (AST.Type, AST.Ident)
binaryOperatorInfo kind = case kind of
  AST.Or        -> (AST.Bool, "$or")
  AST.And       -> (AST.Bool, "$and")
  AST.Less      -> (AST.Bool, "$less")
  AST.Add       -> (AST.I32, "$add")
  AST.Subtract  -> (AST.I32, "$subtract")
  AST.Multiply  -> (AST.I32, "$multiply")
  AST.Divide    -> (AST.I32, "$divide")
  AST.Modulo    -> (AST.I32, "$modulo")

unaryOperatorInfo :: AST.UnaryOperatorKind -> (AST.Type, AST.Ident)
unaryOperatorInfo AST.Not = (AST.Bool, "$not")
unaryOperatorInfo AST.Negate = (AST.I32, "$negate")
