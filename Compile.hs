-- Jakub Staroń, 2017
{-# LANGUAGE RankNTypes, TypeSynonymInstances, GADTs #-}

module Compile (compile, execute) where

import Data.Int
import Data.List
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Cont
import Control.Exception.Base (assert)

import Debug.Trace (trace)

import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as MapStrict

import qualified AST
import qualified Intermediate as Imd
import Intermediate (typeOf)
import RList
import FormatString

type Input = [Int32]
type Output = RList Int32

newtype MemoryAddress = MemoryAddress Int32 deriving (Eq, Ord, Show)
newtype MemoryOffset = MemoryOffset Int32 deriving (Eq, Ord, Show)
newtype Memory = Memory (MapStrict.Map MemoryAddress Int32) deriving (Show)

liftMemoryOffset :: (Int32 -> Int32) -> (MemoryOffset -> MemoryOffset)
liftMemoryOffset f (MemoryOffset a) = MemoryOffset $ f a
lift2MemoryOffset :: (Int32 -> Int32 -> Int32) -> (MemoryOffset -> MemoryOffset -> MemoryOffset)
lift2MemoryOffset f (MemoryOffset a) (MemoryOffset b) = MemoryOffset $ f a b

instance Enum MemoryAddress where
  toEnum = MemoryAddress . fromIntegral
  fromEnum (MemoryAddress n) = fromIntegral n

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

newtype Variable = Variable { variableOffset :: MemoryOffset } deriving (Show)
newtype Function = Function { body :: Continuation }

type FEnv = Map.Map AST.Ident Function
type VEnv = Map.Map AST.Ident Variable
data Env = Env {functions :: FEnv, variables :: VEnv}

type Program = Continuation

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

-- Close-open range (cause' Haskell [a..b] sucks.)
range :: (Enum a, Ord a) => a -> a -> [a]
range begin end = takeWhile (< end) [begin ..]

memoryRead :: MemoryAddress -> State -> Int32
memoryRead address state =
  let Memory m = memory state in
    fromMaybe
    (error $ format "Internal interpreter error: value not found under given adress.\nAddress: %0, call pointer: %1." [show address, show $ callPointer state])
    (MapStrict.lookup address m)

memoryWrite :: MemoryAddress -> Int32 -> State -> State
memoryWrite address value state =
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
    AST.Array valueType count   -> sizeOf_ valueType * count
    _                           -> 1

memoryCopy :: MemoryOffset -> MemoryAddress -> MemoryAddress -> State -> State
memoryCopy size from to state =
  assert ( ((from |+ size) <= to) || ((to |+ size) <= from) )
  foldl' (flip ($)) state $ zipWith cellCopy (range from $ from |+ size) (range to $ to |+ size) where
    cellCopy :: MemoryAddress -> MemoryAddress -> State -> State
    cellCopy from to env = memoryWrite to (memoryRead from env) env

memoryEqual :: MemoryOffset -> MemoryAddress -> MemoryAddress -> State -> Bool
--memoryEqual size first second state | trace (format "memoryEqual %0 %1 %2" [show size, show first, show second]) False = undefined
memoryEqual size first second state = all (uncurry (==)) pairs where
  pairs = zip (readSequence first) (readSequence second)
  readSequence :: MemoryAddress -> [Int32]
  readSequence addr = map (`memoryRead` state) $ range addr $ addr |+ size

findFunction :: AST.Ident -> Env -> Function
findFunction ident env =
  fromMaybe
  (error $ format "Internal interpreter error: function \"%0\" not found." [ident])
  (Map.lookup ident $ functions env)

findVariable :: AST.Ident -> Env -> Variable
findVariable ident env =
    fromMaybe
    (error $ format "Internal interpreter error: variable \"%0\" not found.\nVariables: %1" [ident, show $ variables env])
    (Map.lookup ident $ variables env)

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
    state <- return $ memoryWrite callAddress value state
    return (state, callAddress)
  writeI32 :: Continuation
  writeI32 state = do
    let callAddress = callPointer state
    let value = memoryRead callAddress state
    state <- return $ state { output = output state :> value }
    return (state, callAddress)
  or_ = buildBinaryOperatorFunction (\a b -> if a == 1 || b == 1 then 1 else 0)
  and_ = buildBinaryOperatorFunction (*)
  less_ = buildBinaryOperatorFunction (\a b -> if a < b then 1 else 0)
  add_ = buildBinaryOperatorFunction (+)
  subtract_ = buildBinaryOperatorFunction (-)
  multiply_ = buildBinaryOperatorFunction (*)
  divide_ = buildDivisionLikeOperator quot
  modulo_ = buildDivisionLikeOperator mod
  negate_ = buildUnaryOperatorFunction negate
  not_ = buildUnaryOperatorFunction (1-)
  buildBinaryOperatorFunction :: (Int32 -> Int32 -> Int32) -> Continuation
  buildBinaryOperatorFunction f state = do
    let callAddress = callPointer state
    let argumentsAddress = callAddress |+ 1
    let v1 = memoryRead argumentsAddress state
    let v2 = memoryRead (argumentsAddress |+ 1) state
    trace (format "Tu binary operator function, v1 = %0, v2 = %1" [show v1, show v2]) (return ())
    state <- return $ memoryWrite callAddress (f v1 v2) state
    return (state, callAddress)
  buildDivisionLikeOperator :: (Int32 -> Int32 -> Int32) -> Continuation
  buildDivisionLikeOperator f state = do
    let callAddress = callPointer state
    let argumentsAddress = callAddress |+ 1
    let v1 = memoryRead argumentsAddress state
    let v2 = memoryRead (argumentsAddress |+ 1) state
    trace (format "Tu division like operator function, v1 = %0, v2 = %1" [show v1, show v2]) (return ())
    when (v2 == 0) $ Left ("Runtime error: divide by 0.", state)
    when (v1 == minBound && v2 == -1) $ Left ("Runtime error: divide minint by -1.", state)
    state <- return $ memoryWrite callAddress (f v1 v2) state
    return (state, callAddress)
  buildUnaryOperatorFunction :: (Int32 -> Int32) -> Continuation
  buildUnaryOperatorFunction f state = do
    let callAddress = callPointer state
    let argumentsAddress = callAddress |+ 1
    let v = memoryRead argumentsAddress state
    trace (format "Tu unary operator function, v = %0" [show v]) (return ())
    state <- return $ memoryWrite callAddress (f v) state
    return (state, callAddress)
initialVEnv :: VEnv
initialVEnv = Map.empty

insertVariable :: AST.Ident -> Variable -> Env -> Env
insertVariable name variable env = env' where
  env' = env { variables = variables' }
  variables' = Map.insert name variable $ variables env

compile program = body $ findFunction (Imd.mainUid program) env where
  env = Env fenv initialVEnv
  fenv = foldl' insertFunction initialFEnv $ Imd.functions program where
    insertFunction :: FEnv -> Imd.Function -> FEnv
    insertFunction fenv' function = Map.insert (Imd.name function) (compileFunction env function) fenv'

compileFunction :: Env -> Imd.Function -> Function
compileFunction env function = Function cont where
  resultType = typeOf $ Imd.body function
  resultSize = sizeOf resultType
  parameterSize = sizeOf $ Imd.parameter function
  cont :: Continuation
  cont state = do
    -- Clean variables bindings when calling function!
    env <- return $ env { variables = initialVEnv }
    -- Bind arguments. Initializer is placed in memory under offset resultSize
    env <- return $ bindVariables (Imd.bindings function) (Imd.parameter function) resultSize env
    -- Compile expression with offset = sizeOf resultType and copy
    -- result of the expression later to left space
    trace (format "Hello from function '%0', resultSize: %2, variable bindings: %1" [Imd.name function, show $ variables env, show resultSize]) (return ())
    expr <- return $ compileExpr env (resultSize + parameterSize) (Imd.body function)
    (state, resultAddress) <- expr state
    state <- return $ memoryCopy resultSize resultAddress (callPointer state) state
    return (state, callPointer state)

compileExpr :: Env -> MemoryOffset -> Imd.Expr a -> Continuation
compileExpr env offset expr state =
  -- Offset - size of already occupied stack memory for this function call, statically computed
  -- Call address - begin of stack for this function call, passed in State (dynamically computed)
  let callAddress = callPointer state in
  -- Heap pointer - addres of begining of free memory.
  let heapPointer = callAddress |+ offset in
  case expr of
    Imd.FunctionCall resultType ident exprs -> do
      let fn = body $ findFunction ident env
      let offsets = scanl (+) (offset + sizeOf resultType) $ map (sizeOf . Imd.typeOf) exprs
      exprs <- return $ zipWith (compileExpr env) offsets exprs
      (state, _) <- foldM (\(state, _) expr -> expr state) (state, undefined) exprs
      trace (format "Call of function '%0', call on memory address: %1.\nMemory in the moment of call: %2" [ident, show heapPointer, show $ memory state]) (return ())
      (state, _) <- fn $ state { callPointer = heapPointer }
      state <- return $ state { callPointer = callAddress }
      _ <- trace (format "Back from function '%0'.\nMemory after call: %1" [ident, show $ memory state]) (return ())
      -- Returns new state and pointer to newly created value (probably anyway ignored)
      return (state, heapPointer)

    Imd.TupleLookup   _ _       -> undefined
    Imd.ArrayLookup   _ _       -> undefined
    Imd.Assign        expr1 expr2 -> do
      let assignType = typeOf expr2
      let assignSize = sizeOf assignType
      expr1 <- return $ compileExpr env offset expr1
      expr2 <- return $ compileExpr env offset expr2
      (state, writeAddress) <- expr1 state
      (state, readAddress) <- expr2 state
      state <- return $ memoryCopy assignSize readAddress writeAddress state
      -- Returning heap pointer because result type is ().
      return (state, heapPointer)
    Imd.Equal         expr1 expr2 -> do
      assert (sizeOf (typeOf expr1) == sizeOf (typeOf expr2)) $ return ()
      let sizeOfValue = sizeOf $ typeOf expr1
      expr1 <- return $ compileExpr env offset expr1
      expr2 <- return $ compileExpr env (sizeOfValue + offset) expr2
      (state, valueAddress1) <- expr1 state
      (state, valueAddress2) <- expr2 state
      let equal = memoryEqual sizeOfValue valueAddress1 valueAddress2 state
      trace (format "Tu equal, wpisuje wynik porownania (%0) na adres %1." [show equal, show heapPointer]) (return ())
      state <- return $ memoryWrite heapPointer (if equal then 1 else 0) state
      return (state, heapPointer)
    Imd.Dereference   t _       -> undefined
    Imd.Borrow        t _       -> undefined
    Imd.Identifier    _ name    -> do
      let variable = findVariable name env
      return (state, callAddress |+ variableOffset variable)
    Imd.Literal literal -> do
      let value = case literal of {
        AST.LiteralI32 n -> n;
        AST.LiteralBool True -> 1;
        AST.LiteralBool False -> 0;
      }
      state <- return $ memoryWrite heapPointer value state
      return (state, heapPointer)

    Imd.Array         c         -> undefined
    Imd.Tuple         exprs     -> do
      let tupleType = typeOf expr
      let offsets = scanl (+) offset $ map (sizeOf . Imd.typeOf) exprs
      exprs <- return $ zipWith (compileExpr env) offsets exprs
      (state, _) <- foldM (\(state, _) expr -> expr state) (state, undefined) exprs
      return (state, heapPointer)
    Imd.IfElse        expr1 block1 block2     -> do
      expr1 <- return $ compileExpr env offset expr1
      block1 <- return $ compileExpr env offset block1
      block2 <- return $ compileExpr env offset block2
      (state, conditionAddress) <- expr1 state
      let condition = memoryRead conditionAddress state
      assert (condition == 0 || condition == 1) $ return ()
      if condition == 1 then
        block1 state
      else
        block2 state
    Imd.Materialize   expr      -> do
      let typeOfExpr = typeOf expr
      let sizeOfValue = sizeOf typeOfExpr
      expr <- return $ compileExpr env (offset + sizeOfValue) expr
      (state, valueAddress) <- expr state
      trace (format "Materializing 'LValue' of type %0, copying from address %1 to address %2." [show typeOfExpr, show valueAddress, show heapPointer]) (return ())
      state <- return $ memoryCopy sizeOfValue valueAddress heapPointer state
      return (state, heapPointer)
    Imd.If            _ _       -> undefined
    Imd.Loop          loopConstructor block       ->
      case loopConstructor of
        Imd.ForRange variableName beginExpr endExpr -> do
          let forVariableOffset = offset + 2 * sizeOf AST.I32
          beginExpr <- return $ compileExpr env offset beginExpr
          endExpr <- return $ compileExpr env (offset + sizeOf AST.I32) endExpr

          env <- return $ insertVariable variableName (Variable forVariableOffset) env
          block <- return $ compileExpr env (offset + 3 * sizeOf AST.I32) block
          (state, beginVariableAddress) <- beginExpr state
          (state, endVariableAddress) <- endExpr state
          let beginVariable = memoryRead beginVariableAddress state
          let endVariable = memoryRead endVariableAddress state
          unless (beginVariable <= endVariable) $ Left (format "Runtime exception: invalid range (%0, %1) in for-range." [show beginVariable, show endVariable], state)
          let forVariableAddress = callPointer state |+ forVariableOffset
          let forRuns = map (\x -> fmap fst . block . (memoryWrite forVariableAddress x)) $ range beginVariable endVariable
          state <- foldl' (>>=) (return state) forRuns
          return (state, heapPointer)
        Imd.While conditionExpr -> do
          conditionExpr <- return $ compileExpr env offset conditionExpr
          block <- return $ compileExpr env (offset + sizeOf AST.Bool) block
          let loopRun = block >=> return . fst >=> conditionExpr
          let ifConditionThenCall f (state, conditionAddress) = if memoryRead conditionAddress state == 1 then f state else return state
          let loop = loopRun >=> ifConditionThenCall loop
          state <- (conditionExpr >=> ifConditionThenCall loop) state
          return (state, heapPointer)
        _ -> undefined
    Imd.FlowControl   _         -> undefined
    Imd.BindVariables bindings initializer suffix -> do
      let initializerType = typeOf initializer
      let dataSize = sizeOf initializerType
      initializer <- return $ compileExpr env offset initializer
      env <- return $ bindVariables bindings initializerType offset env
      suffix <- return $ compileExpr env (offset + dataSize) suffix
      (state, _) <- initializer state
      suffix state
    Imd.Sequence      expr1 expr2 -> do
      expr1 <- return $ compileExpr env offset expr1
      expr2 <- return $ compileExpr env offset expr2
      (state, _) <- expr1 state
      expr2 state

bindVariables :: [(AST.Ident, [Int32])] -> AST.Type -> MemoryOffset -> Env -> Env
bindVariables bindings initializerType offset = foldl' (.) id variableBindings where
    variableBindings = map (\(n, p) -> insertVariable n $ Variable $ offset + offsetInTuple initializerType p) bindings

offsetInTuple :: AST.Type -> [Int32] -> MemoryOffset
offsetInTuple _ [] = MemoryOffset 0
offsetInTuple (AST.Tuple types) (n : ns) =
  let sizes = map sizeOf types in
  let nn = fromIntegral n in
  sum (take nn sizes) + offsetInTuple (types !! nn) ns


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
    Right (state, _) -> memory state `seq` Right $ output state
    Left (message, state) -> memory state `seq` Left (message, output state)
