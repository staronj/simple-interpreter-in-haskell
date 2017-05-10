-- Jakub Staroń, 2017

{-# LANGUAGE RankNTypes, TypeSynonymInstances, GADTs #-}

module Compile (compile, execute) where

import Data.Int (Int32)
import Data.List (foldl', uncons)
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Exception.Base (assert)

import qualified Debug.Trace as Debug


import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as MapStrict

import qualified AST
import qualified Intermediate as Imd
import Intermediate (typeOf)
import RList
import FormatString

-- Debugging
debugMode :: Bool
debugMode = False

trace :: String -> a -> a
trace message a = if debugMode then Debug.trace message a else a
-- Debugging end

type Input = [Int32]
type Output = RList Int32

newtype MemoryAddress = MemoryAddress Int32 deriving (Eq, Ord, Show)
newtype MemoryOffset = MemoryOffset Int32 deriving (Eq, Ord, Show)
newtype Memory = Memory (MapStrict.Map MemoryAddress Int32) deriving (Show)

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

-- Group of helper functions for defining Num for Memory offset
liftMemoryOffset :: (Int32 -> Int32) -> (MemoryOffset -> MemoryOffset)
liftMemoryOffset f (MemoryOffset a) = MemoryOffset $ f a
lift2MemoryOffset :: (Int32 -> Int32 -> Int32) -> (MemoryOffset -> MemoryOffset -> MemoryOffset)
lift2MemoryOffset f (MemoryOffset a) (MemoryOffset b) = MemoryOffset $ f a b
-- End of group

(|+) :: MemoryAddress -> MemoryOffset -> MemoryAddress
(|+) (MemoryAddress address) (MemoryOffset offset) = MemoryAddress (address + offset)

data State =
  State
  { input :: Input
  , output :: Output
  , memory :: Memory
  , callPointer :: MemoryAddress }

type Interruption value = Either (String, State) value
type Continuation = State -> Interruption (State, MemoryAddress)

newtype Variable = Variable { variableOffset :: MemoryOffset } deriving (Show)
newtype Function = Function { body :: Continuation }

type FEnv = Map.Map AST.Ident Function
type VEnv = Map.Map AST.Ident Variable
data Env = Env {functions :: FEnv, variables :: VEnv}

type Program = Continuation

-- Close-open range (cause' Haskell [a..b] sucks.)
range :: (Enum a, Ord a) => a -> a -> [a]
range begin end = takeWhile (< end) [begin ..]

lazyFixpoint :: Monad m => (a -> m a) -> (a -> m a)
lazyFixpoint f a = f a >>= lazyFixpoint f

readMemory :: MemoryAddress -> State -> Int32
readMemory address state =
  let Memory m = memory state in
    fromMaybe
    (error $ format "Internal interpreter error: value not found under given adress.\nAddress: %0, call pointer: %1." [show address, show $ callPointer state])
    (MapStrict.lookup address m)

writeMemory :: MemoryAddress -> Int32 -> State -> State
writeMemory address value _ | trace (format "writeMemory '%0' '%1'" [show address, show value]) False = undefined
writeMemory address value state =
  state { memory = m' } where
    Memory m = memory state
    m' = Memory $ MapStrict.insert address value m

sizeOf ::  AST.Type -> MemoryOffset
sizeOf = MemoryOffset . sizeOf_ where
  sizeOf_ ::  AST.Type -> Int32
  sizeOf_ (AST.Tuple types)           = sum $ map sizeOf_ types
  sizeOf_ (AST.Array valueType count) = sizeOf_ valueType * count
  sizeOf_ _                           = 1

copyMemory :: MemoryOffset -> MemoryAddress -> MemoryAddress -> State -> State
copyMemory size from to _ | trace (format "copyMemory '%0' '%1' '%2'" [show size, show from, show to]) False = undefined
copyMemory size from to state =
  assert ( ((from |+ size) <= to) || ((to |+ size) <= from) )
  foldl' (flip ($)) state $ zipWith cellCopy (range from $ from |+ size) (range to $ to |+ size) where
    cellCopy :: MemoryAddress -> MemoryAddress -> State -> State
    cellCopy from to env = writeMemory to (readMemory from env) env

copyValue :: AST.Type -> MemoryAddress -> MemoryAddress -> State -> State
copyValue valueType = copyMemory $ sizeOf valueType

equalMemory :: MemoryOffset -> MemoryAddress -> MemoryAddress -> State -> Bool
--equalMemory size first second state | trace (format "equalMemory %0 %1 %2" [show size, show first, show second]) False = undefined
equalMemory size first second state = all (uncurry (==)) pairs where
  pairs = zip (readSequence first) (readSequence second)
  readSequence :: MemoryAddress -> [Int32]
  readSequence addr = map (`readMemory` state) $ range addr $ addr |+ size

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
  readI32 state =
    let callAddress = callPointer state in do
      (value, input') <- fromMaybe (throwError ("Runtime error: end of input.", state)) (fmap return $ uncons $ input state)
      let state' = state { input = input' }
      let state'' = writeMemory callAddress value state'
      return (state'', callAddress)
  writeI32 :: Continuation
  writeI32 state =
    let callAddress = callPointer state in
    let value = readMemory callAddress state in
    let state' =  state { output = output state :> value } in
    return (state', callAddress)
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
  buildBinaryOperatorFunction f state =
    let callAddress = callPointer state in
    let v1 = readMemory callAddress state in
    let v2 = readMemory (callAddress |+ 1) state in
    let state' = writeMemory callAddress (f v1 v2) state in
    trace (format "Tu binary operator function, v1 = %0, v2 = %1" [show v1, show v2]) $
    return (state', callAddress)
  buildDivisionLikeOperator :: (Int32 -> Int32 -> Int32) -> Continuation
  buildDivisionLikeOperator f state =
    let callAddress = callPointer state in
    let v1 = readMemory callAddress state in
    let v2 = readMemory (callAddress |+ 1) state in do
      trace (format "Tu division like operator function, v1 = %0, v2 = %1" [show v1, show v2]) (return ())
      when (v2 == 0) $ Left ("Runtime error: divide by 0.", state)
      when (v1 == minBound && v2 == -1) $ Left ("Runtime error: divide minint by -1.", state)
      let state' = writeMemory callAddress (f v1 v2) state
      return (state', callAddress)
  buildUnaryOperatorFunction :: (Int32 -> Int32) -> Continuation
  buildUnaryOperatorFunction f state =
    let callAddress = callPointer state in
    let v = readMemory callAddress state in
    let state' = writeMemory callAddress (f v) state in
    trace (format "Tu unary operator function, v = %0" [show v]) $
    return (state', callAddress)
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
compileFunction env function = Function { body = body } where
  parameterSize = sizeOf $ Imd.parameter function
  -- Clean variables bindings when calling function!
  env' = env { variables = initialVEnv }
  -- Bind arguments. Arguments are placed under callAddress so that offser is 0.
  env'' = bindVariables (Imd.bindings function) (Imd.parameter function) (MemoryOffset 0) env'
  -- Compile expression with offset = parameterSize
  body = compileExpr env'' parameterSize (Imd.body function)

compileExpr :: Env -> MemoryOffset -> Imd.Expr a -> Continuation
compileExpr env offset expr state =
  -- Offset - size of already occupied stack memory for this function call, statically computed
  -- Call address - begin of stack for this function call, passed in State (dynamically computed)
  let callAddress = callPointer state in
  -- Stack pointer - addres of begining of free memory.
  let stackPointer = callAddress |+ offset in
  case expr of
    Imd.FunctionCall resultType name argumentExpr ->
      let functionBody = body $ findFunction name env in
      let argumentType = typeOf argumentExpr in
      let argumentSize = sizeOf argumentType in
      -- Argument expression will execute on stack from (offset + argumentSize)
      -- and we will copy result of this expression back to offset
      let argumentExpr' = compileExpr env (offset + argumentSize) argumentExpr in do
        -- First, execute argument expr
        (state, argumentAddress) <- argumentExpr' state
        -- Then copy argument back to function call address
        state <- return $ copyMemory argumentSize argumentAddress stackPointer state
        trace (format "Calling '%0'. CallPointer: %1. %2" [name, show stackPointer, show $ memory state]) (return ())
        -- Call function with updated callPointer
        (state, resultAddress) <- functionBody $ state { callPointer = stackPointer }
        trace (format "Back from '%0'. Memory: %1" [name, show $ memory state]) (return ())
        -- Recover old call address
        state <- return $ state { callPointer = callAddress }
        return (state, resultAddress)
    Imd.TupleLookup   _ _       -> undefined
    Imd.ArrayLookup   arrayExpr indexExpr ->
      let arrayType = typeOf arrayExpr in
      let AST.Array elementType arrayLength = arrayType in
      let elementSize = sizeOf elementType in
      let arraySize = sizeOf arrayType in
      let indexSize = sizeOf AST.I32 in
      -- Index and array exprs are compiled in the same offset.
      -- It's ok because we first execute index expr, load its value
      -- and and after that execute array expr.
      let indexCont = compileExpr env offset indexExpr in
      let arrayCont = compileExpr env offset arrayExpr in do
        -- First, execute index expr and load index from memory.
        (state, indexAddress) <- indexCont state
        let index = readMemory indexAddress state
        -- Then, execute array expr.
        (state, arrayAddress) <- arrayCont state
        unless (index < arrayLength) $ throwError (format "Runtime error: array lookup out of bounds - lookup at position %0 in array of type %1." [show index, show arrayType], state)
        return (state, arrayAddress |+ (MemoryOffset index * elementSize))
    Imd.Assign        lhs rhs ->
      let lhsType = typeOf lhs in
      let rhsType = typeOf rhs in
      let lhsSize = sizeOf lhsType in
      let rhsSize = sizeOf rhsType in
      -- We will first execute rhs on offset = (offset + rhs size)
      -- then copy coputed value into offset, then execute lhs on
      -- offset = (offset + rhs size) and then copy values.
      let rhsCont = compileExpr env (offset + rhsSize) rhs in
      let lhsCont = compileExpr env (offset + rhsSize) lhs in do
        (state, rhsAddress) <- rhsCont state
        state <- return $ copyMemory rhsSize rhsAddress stackPointer state
        rhsAddress <- return stackPointer
        (state, lhsAddress) <- lhsCont state
        trace (format "Assign: writing %0 cells from %1 to %2." [show rhsSize, show rhsAddress, show lhsAddress]) (return ())
        state <- return $ copyMemory rhsSize rhsAddress lhsAddress state
        return (state, stackPointer)
    Imd.Equal         lhs rhs ->
      let lhsType = typeOf lhs in
      let rhsType = typeOf rhs in
      let lhsSize = sizeOf lhsType in
      let rhsSize = sizeOf rhsType in
      -- First, execute lhs on offset = (offset + lhsSize),
      -- copy value to offset, excute rhs on offset = (offset + lhsSize)
      -- and compare values. Store result on offset.
      let lhsCont = compileExpr env (offset + lhsSize) lhs in
      let rhsCont = compileExpr env (offset + lhsSize) rhs in do
        assert (lhsSize == rhsSize) $ return ()
        (state, lhsAddress) <- lhsCont state
        state <- return $ copyMemory lhsSize lhsAddress stackPointer state
        lhsAddress <- return stackPointer
        (state, rhsAddress) <- rhsCont state
        let equal = equalMemory lhsSize lhsAddress rhsAddress state
        trace (format "Tu equal, wpisuje wynik porownania (%0) na adres %1." [show equal, show stackPointer]) (return ())
        state <- return $ writeMemory stackPointer (if equal then 1 else 0) state
        return (state, stackPointer)
    Imd.Dereference   _ _       -> error "Not implemented."
    Imd.Borrow        _ _       -> error "Not implemented."
    Imd.Identifier    _ name    ->
      -- Just return the static offset of variable in function.
      let variable = findVariable name env in
      return (state, callAddress |+ variableOffset variable)
    Imd.Literal literal ->
      -- Just place the value of literal on stack.
      let value = case literal of {
        AST.LiteralI32 n -> n;
        AST.LiteralBool True -> 1;
        AST.LiteralBool False -> 0;
      } in
      let state' = writeMemory stackPointer value state in
      return (state', stackPointer)
    Imd.Array         c         -> case c of
      Imd.Repeat valueExpr count ->
        let valueType = typeOf valueExpr in
        let valueSize = sizeOf valueType in
        let arrayType = typeOf expr in
        let arraySize = sizeOf arrayType in
        -- We will execute valueExpr once, on offset = (offset + arraySize)
        -- and then copy resulting value count times to create an array.
        let valueCont = compileExpr env (offset + arraySize) valueExpr in
        do
          (state, valueAddress) <- valueCont state
          let copyAddresses = map (\i -> stackPointer |+ (MemoryOffset i * valueSize)) $ range 0 count
          let actions = map (copyMemory valueSize valueAddress) copyAddresses
          state <- return $ foldl' (.) id actions state
          return (state, stackPointer)
      _ -> error "Not implemented."
    Imd.Tuple         valueExprs     ->
      let tupleType = typeOf expr in
      let tupleSize = sizeOf tupleType in
      -- We will execute every valueExpr on offset = (offset + tupleSize)
      -- and copy that value to corresponding place in memory.
      let valueConts = map (compileExpr env (offset + tupleSize)) valueExprs in
      let valueSizes = map (sizeOf . typeOf) valueExprs in
      -- Value offsets - offsets for each value in resulting tuple.
      let valueOffsets = scanl (+) offset valueSizes in do
        trace (format "Constructing tuple %0." [show tupleType]) (return ())
        copyActions <- return $ map copyMemory valueSizes
        -- We know where to copy but don't know from where
        -- Let's flip arguments, provide 'to' and left 'from' for later.
        copyActions <- return $ map flip copyActions
        let valueAddresses = map (callAddress |+) valueOffsets
        copyActions <- return $ zipWith ($) copyActions valueAddresses
        -- Copy action now want MemoryAddress -> State, transform it
        -- to take (State, MemoryAddress)
        copyActions <- return $ map (uncurry . flip) copyActions
        -- And please return state in monadic context.
        copyActions <- return $ map (return . ) copyActions
        let actions = zipWith (>=>) valueConts copyActions
        let action = foldl' (>=>) return actions
        state <- action state
        return (state, stackPointer)
    Imd.IfElse conditionExpr trueBlock falseBlock ->
      -- Execute conditon on offset = offset, load result
      -- and execute one of blocks on offset = offset.
      let conditionCont = compileExpr env offset conditionExpr in
      let trueCont = compileExpr env offset trueBlock in
      let falseCont = compileExpr env offset falseBlock in do
        (state, conditionAddress) <- conditionCont state
        let condition = readMemory conditionAddress state
        assert (condition == 0 || condition == 1) $ return ()
        if condition == 1 then
          trueCont state
        else
          falseCont state
    Imd.Materialize valueExpr ->
      let valueType = typeOf valueExpr in
      let valueSize = sizeOf valueType in
      -- Execute valueExpr on offset = (offset + valueSize) and
      -- then copy to offset.
      let valueCont = compileExpr env (offset + valueSize) valueExpr in do
        (state, valueAddress) <- valueCont state
        trace (format "Materializing 'LValue' of type %0, copying from address %1 to address %2." [show valueType, show valueAddress, show stackPointer]) (return ())
        state <- return $ copyMemory valueSize valueAddress stackPointer state
        return (state, stackPointer)
    Imd.If conditionExpr block ->
      -- Execute conditionExpr on offset = offset, load result
      -- and then conditionally execute block. Return stackPointer as
      -- result address.
      let conditionCont = compileExpr env offset conditionExpr in
      let blockCont = compileExpr env offset block in
      let blockCont' = fmap fst . blockCont in
      let conditionCheck = conditionCont >=> (\(state, conditionAddress) -> return (state, readMemory conditionAddress state == 1)) in
      let ifConditionThenCall (state, condition) = if condition then blockCont' state else return state in do
        state <- conditionCheck state >>= ifConditionThenCall
        return (state, stackPointer)
    Imd.Loop          loopConstructor block       ->
      case loopConstructor of
        Imd.ForRange variableName beginExpr endExpr ->
          -- Run beginExpr, load value, run endExpr, load value
          -- then run block right number of times. BeginExpr and endExpr
          -- have offset = offset and block have offset = (offset + sizeOf AST.I32)
          -- because loop variable is stroed under under offset.
          let beginCont = compileExpr env offset beginExpr in
          let endCont = compileExpr env offset endExpr in
          let forVariableOffset = offset in
          -- Block must be compiled with enviroment containing for variable.
          let blockEnv = insertVariable variableName Variable { variableOffset = forVariableOffset } env in
          let blockCont = compileExpr blockEnv (offset + sizeOf AST.I32) block in do
            (state, beginAddress) <- beginCont state
            let begin = readMemory beginAddress state
            (state, endAddress) <- endCont state
            let end = readMemory beginAddress state
            unless (begin <= end) $ throwError (format "Runtime exception: invalid range (%0, %1) in for-range." [show begin, show end], state)
            let forVariableAddress = callAddress |+ forVariableOffset
            let writeActions = map (writeMemory forVariableAddress) $ range begin end
            let actions = map (\f -> fmap fst . blockCont . f) writeActions
            let action = foldl' (>=>) return actions
            state <- action state
            return (state, stackPointer)
        Imd.ForEach name initExpr ->
          -- First, we call initExpr on offset = offset + sizeOf initExpr.
          -- Next, we copy initExpr to offset.
          -- Next, we call block specific number of times with different
          -- enviroments and offset = offset + sizeOf initExpr.
          let initType = typeOf initExpr in
          let initSize = sizeOf initType in
          let (AST.Array elementType elementsCount) = initType in
          let elementSize = sizeOf elementType in
          let initCont = compileExpr env (offset + initSize) initExpr in
          -- Block must be compiled with enviroment containing for variable.
          let forVariableOffsets = map (\i -> offset + MemoryOffset i * elementSize) $ range 0 elementsCount in
          let blockEnvs = map
                            (\offset -> insertVariable name (Variable offset) env)
                            forVariableOffsets in
          let blockConts = map (\env' -> compileExpr env' (offset + initSize) block) blockEnvs in
          let blockConts' = map (>=> (return . fst)) blockConts in
          let blockCont = foldl' (>=>) return blockConts' in do
            (state, arrayAddress) <- initCont state
            state <- return $ copyMemory initSize arrayAddress stackPointer state
            state <- blockCont state
            return (state, stackPointer)
        Imd.While conditionExpr ->
          -- Alternately run contirionExpr and block on offset = offset
          let conditionCont = compileExpr env offset conditionExpr in
          let blockCont = compileExpr env offset block in
          let conditionCheck = conditionCont >=> (\(state, conditionAddress) -> return (state, readMemory conditionAddress state == 1)) in
          let loopIteration = blockCont >=> return . fst >=> conditionCheck in
          let ifConditionThenCall f (state, condition) = if condition then f state else return state in
          let loop = loopIteration >=> ifConditionThenCall loop in do
            state <- (conditionCheck >=> ifConditionThenCall loop) state
            return (state, stackPointer)
        Imd.Forever ->
          -- Just run block forever (till break) on offset = offset
          let blockCont = compileExpr env offset block in
          let loopIteration = blockCont >=> return . fst in
          do
            state <- lazyFixpoint loopIteration state
            return (state, stackPointer)
    Imd.FlowControl   _         -> undefined
    Imd.BindVariables bindings initExpr suffixExpr ->
      -- Run initExpr on offset = (offset + sizeOf initExpr), copy
      -- resulting variable to  offset, run suffixExpr with
      -- offset = (offset + sizeOf initExpr) and return its result.
      let initType = typeOf initExpr in
      let initSize = sizeOf initType in
      let initCont = compileExpr env (offset + initSize) initExpr in
      -- SuffixExpr must be compiled with modified enviroment.
      -- We create bindings on offset = offset.
      let suffixEnv = bindVariables bindings initType offset env in
      let suffixCont = compileExpr suffixEnv (offset + initSize) suffixExpr in do
        (state, initAddress) <- initCont state
        state <- return $ copyMemory initSize initAddress stackPointer state
        initAddress <- return stackPointer
        suffixCont state
    Imd.Sequence firstExpr secondExpr ->
      -- firstExpr and secondExpr are not related thus we will
      -- run both on offset = offset.
      let firstCont = compileExpr env offset firstExpr in
      let secondCont = compileExpr env offset secondExpr in
      fmap fst (firstCont state) >>= secondCont

bindVariables :: [(AST.Ident, [Int32])] -> AST.Type -> MemoryOffset -> Env -> Env
bindVariables bindings initializerType offset = foldl' (.) id variableBindings where
    variableBindings = map (\(n, p) -> insertVariable n $ Variable $ offset + offsetInTuple initializerType p) bindings

offsetInTuple :: AST.Type -> [Int32] -> MemoryOffset
offsetInTuple _ [] = MemoryOffset 0
offsetInTuple (AST.Tuple types) (n : ns) =
  let sizes = map sizeOf types in
  let nn = fromIntegral n in
  sum (take nn sizes) + offsetInTuple (types !! nn) ns
offsetInTuple _ _ = undefined

execute :: Program -> Input -> Either (String, Output) Output
execute program input =
  let emptyMemory = Memory Map.empty in
  let state = State input Nil emptyMemory (MemoryAddress 0) in
  case program  state of
    Right (state, _) -> Right $ output state
    Left (message, state) -> Left (message, output state)
