-- Jakub Staroń, 2017
{-# LANGUAGE RankNTypes, TypeSynonymInstances, GADTs #-}

module Compile (compile, execute) where

import Data.Int
import Data.List
import Control.Monad
import Control.Monad.Cont
import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as MapStrict

import qualified AST
import qualified Intermediate as Imd
import RList
import FormatString

type Input = [Int32]
type Output = RList Int32

newtype MemoryAddress = MemoryAddress Int32 deriving (Eq, Ord)
newtype MemoryOffset = MemoryOffset Int32
newtype Memory = Memory (MapStrict.Map MemoryAddress Int32)

liftMemoryOffset :: (Int32 -> Int32) -> (MemoryOffset -> MemoryOffset)
liftMemoryOffset f (MemoryOffset a) = MemoryOffset $ f a
lift2MemoryOffset :: (Int32 -> Int32 -> Int32) -> (MemoryOffset -> MemoryOffset -> MemoryOffset)
lift2MemoryOffset f (MemoryOffset a) (MemoryOffset b) = MemoryOffset $ f a b

instance Num MemoryOffset where
  negate      = liftMemoryOffset negate
  (+)         = lift2MemoryOffset (+)
  (*)         = lift2MemoryOffset (*)
  fromInteger = MemoryOffset . fromInteger
  abs         = liftMemoryOffset abs
  signum      = liftMemoryOffset signum


(|+) :: MemoryAddress -> MemoryOffset -> MemoryAddress
(|+) (MemoryAddress address) (MemoryOffset offset) = MemoryAddress (address + offset)


data State = State { input :: Input, output :: Output, memory :: Memory, callPointer :: MemoryAddress }

type Interruption value = Either (String, State) value
type Continuation = State -> Interruption (State, MemoryAddress)

newtype Variable = Variable { offset :: MemoryOffset }
newtype Function = Function { body :: Continuation }

type FEnv = Map.Map AST.Ident Function
type VEnv = Map.Map AST.Ident Variable
data Env = Env {functions :: FEnv, variables :: VEnv}

type Program = Continuation

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

memoryRead :: MemoryAddress -> State -> Interruption Int32
memoryRead address state = return $
  let Memory m = memory state in
    case MapStrict.lookup address m of
      Nothing -> error "Internal interpreter error: value not found under given adress."
      Just v -> v

memoryWrite :: MemoryAddress -> Int32 -> State -> Interruption State
memoryWrite address value state = return $
  state { memory = m' } where
    Memory m = memory state
    m' = Memory $ MapStrict.insert address value m
{-
liftMemory :: (Memory -> Memory) -> (State -> Interruption a State)
liftMemory f state = let m = (memory state) in Right $ state { memory = f m }
-}
sizeOf ::  AST.Type -> MemoryOffset
sizeOf valueType = MemoryOffset $ sizeOf_ valueType where
  sizeOf_ ::  AST.Type -> Int32
  sizeOf_ valueType = case valueType of
    AST.Tuple types             -> sum $ map sizeOf_ types
    AST.Array valueType count   -> (sizeOf_ valueType) * count
    _                           -> 1

memoryCopy :: MemoryOffset -> MemoryAddress -> MemoryAddress -> State -> Interruption State
memoryCopy size from to state = undefined

findFunction :: AST.Ident -> Env -> Function
findFunction ident env = case Map.lookup ident $ functions env of
  Nothing -> error $ format "Internal interpreter error: function \"%0\" not found." [ident]
  Just f -> f

initialFEnv :: FEnv
initialFEnv = Map.fromList [
    ("readI32", Function readI32)
  , ("writeI32", Function writeI32)
  , ("$or", Function or_)
  , ("$and", Function and_)
  , ("$less", Function less_)
  , ("$add", Function add_)
  , ("$subtract", Function subtract_)
  , ("$multiply", Function multiply_)
  , ("$divide", Function divide_)
  , ("$modulo", Function modulo_)
  , ("$negate", Function negate_)
  , ("$not", Function not_)
  ] where
  readI32 :: Continuation
  readI32 state = do
    let callAddress = callPointer state
    let value : input' = input state
    state <- return $ state { input = input' }
    state <- memoryWrite callAddress value state
    return (state, callAddress)
  writeI32 :: Continuation
  writeI32 state = do
    let callAddress = callPointer state
    value <- memoryRead callAddress state
    state <- return $ state { output = (output state :> value) }
    return (state, callAddress)
  or_ state = do
    let callAddress = callPointer state
    let argumentsAddress = callAddress |+ sizeOf AST.Bool
    v1 <- memoryRead argumentsAddress state
    v2 <- memoryRead (argumentsAddress |+ sizeOf AST.Bool) state
    state <- memoryWrite callAddress (v1 * v2) state
    return (state, callAddress)
  and_ state = undefined
  less_ state = undefined
  add_ state = undefined
  subtract_ state = undefined
  multiply_ state = undefined
  divide_ state = undefined
  modulo_ state = undefined
  negate_ state = undefined
  not_ state = undefined

initialVEnv :: VEnv
initialVEnv = Map.empty

compile program = body $ findFunction "main" env where
  env = Env fenv initialVEnv
  fenv = foldl' insertFunction initialFEnv $ Imd.functions program where
    insertFunction :: FEnv -> Imd.Function -> FEnv
    insertFunction fenv' function = Map.insert (Imd.name function) (Function $ compileExpr env (MemoryOffset 0) (Imd.body function)) fenv'

compileExpr :: Env -> MemoryOffset -> Imd.Expr a -> Continuation
compileExpr env offset expr state = case expr of
  Imd.FunctionCall resultType ident exprs -> do
    let callAddress = callPointer state
    let heapPointer = callAddress |+ offset
    let fn = body $ findFunction ident env
    let offsets = scanl (+) (sizeOf resultType) $ map (sizeOf . Imd.typeOf) exprs
    exprs <- return $ map (uncurry $ compileExpr env) (zip offsets exprs)
    (state, _) <- foldM (\(state, _) expr -> expr state) (state, undefined) exprs
    (state, _) <- fn state
    return (state, heapPointer)

  Imd.TupleLookup   t _ _     -> undefined
  Imd.ArrayLookup   t _ _     -> undefined
  Imd.Assign        _ _       -> undefined
  Imd.Equal         _ _       -> undefined
  Imd.Dereference   t _       -> undefined
  Imd.Borrow        t _       -> undefined
  Imd.Identifier    t _       -> undefined
  Imd.Literal literal -> do
    let callAddress = callPointer state
    let heapPointer = callAddress |+ offset
    let value = case literal of {
      AST.LiteralI32 n -> n;
      AST.LiteralBool True -> 1;
      AST.LiteralBool False -> 0;
    }
    state <- memoryWrite heapPointer value state
    return (state, heapPointer)

  Imd.Array         c         -> undefined
  Imd.Tuple         t _       -> undefined
  Imd.IfElse        t _ _ _   -> undefined
  Imd.Materialize   t         -> undefined
  Imd.If            _ _       -> undefined
  Imd.Loop          _ _       -> undefined
  Imd.FlowControl   _         -> undefined
  Imd.BindVariables _ _       -> undefined
  Imd.Sequence      _ e       -> undefined

{-
compileFunction :: AST.FunctionDeclaration -> FEnv -> Function
compileFunction function fenv =
    let venv = initialVEnv in
    let resultType = AST.resultType function in
    let parameters = map AST.valueType (AST.parameters function) in
    let cont = do
        (valueType, cont) <- compileBlock (AST.body function) (Env fenv venv)
        unless (valueType == resultType) $ Left $ format "Function \"%0\": could not match type \"%1\" with result type \"%2\"." [AST.name function, show valueType, show resultType]
        return cont in
    Function cont parameters resultType

compileBlock :: AST.Block -> Env -> Either String (AST.Type, Continuation)
compileBlock (AST.Block stmts expr) env =
    let functionDeclarations = [funDecl | AST.FunDeclStmt funDecl <- stmts] in
    let fenv = insertFunctions functionDeclarations (functions env) in
    let env' = env {functions = fenv} in
    do
        (env'', offset, cont1) <- compileStmtSequence stmts env'
        (valueType, cont2) <- compileExpr expr env''
        let cont memoryAddress state = do
            state <- cont1 memoryAddress state
            cont2 (memoryAddress + offset) state
        return (valueType, cont)

compileStmtSequence :: [AST.Stmt] -> Env -> Either String (Env, Int32, Continuation)
compileStmtSequence [] env = Right (env, 0, identityContinuation)
compileStmtSequence (stmt : stmts) env = do
    (env', offset, cont1) <- compileStmt stmt env
    (env'', offset', cont2) <- compileStmtSequence stmts env'
    let cont memoryAddress state = do
            state <- cont1 memoryAddress state
            cont2 (memoryAddress + offset) state
    return (env'', offset + offset', cont)

compileStmt :: AST.Stmt -> Env -> Either String (Env, Int32, Continuation)
compileStmt stmt env = case stmt of
    AST.FunDeclStmt _       -> return (env, 0, identityContinuation)
    AST.Stmt expr           -> do
        (valueType, expr) <- compileExpr expr env
        return (env, 0, expr)
    _                       -> error "Not implemented."

compileExpr :: AST.Expr -> Env -> Either String (AST.Type, Continuation)
compileExpr expr env = case expr of
    AST.LiteralExpr literal         -> do
        case literal of
            AST.LiteralI32 n    -> return (AST.I32, \address -> liftMemory $ memoryWrite address n)
            AST.LiteralBool b   -> undefined
    AST.FunctionCall ident exprs   -> do
        function <- findFunction ident (functions env)
        exprs <- mapM (flip compileExpr env) exprs
        assertFunctionArguments function ident $ map fst exprs
        let (Right fun_cont) = body function
        let cont memoryAddress state = do
            state <- (snd $ head exprs) memoryAddress state
            fun_cont memoryAddress state
        return (resultType function, cont)
    AST.TupleConstruct exprs        -> return (AST.unit, identityContinuation)
    _                               -> error "Not implemented."
-}

execute :: Program -> Input -> Either (String, Output) Output
execute program input =
  let emptyMemory = Memory Map.empty in
  let state = State input Nil emptyMemory (MemoryAddress 0) in
  case program  state of
    Right (state, _) -> (memory state) `seq` Right $ output state
    Left (message, state) -> (memory state) `seq` Left $ (message, output state)
