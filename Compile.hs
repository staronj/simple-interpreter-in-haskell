-- Jakub Staroń, 2017
{-# LANGUAGE RankNTypes, TypeSynonymInstances #-}

module Compile (compile, execute) where

import Data.Int
import Data.List
import Control.Monad
import Control.Monad.Cont
import qualified Data.Map.Lazy as Map
import qualified Data.Map.Strict as MapStrict

import qualified AST
import RList
import FormatString

type Input = [Int32]
type Output = RList Int32

type Interruption err val = Either (err, val) val


mapInterrupt :: (a -> b) -> Interruption c a -> Interruption c b
mapInterrupt f (Left (err, v)) = Left (err, f v)
mapInterrupt f (Right v) = Right (f v)

type MemoryAddress = Int32
type Memory = MapStrict.Map MemoryAddress Int32

data State = State {input :: Input, output :: Output, memory :: Memory}

type Continuation = MemoryAddress -> State -> Interruption String State

data Variable = Variable { address :: MemoryAddress, valueType :: AST.Type, mutable :: Bool }
type VEnv = Map.Map AST.Ident Variable

data Function = Function { body :: Either String Continuation, parameters :: [AST.Type], resultType :: AST.Type}
type FEnv = Map.Map AST.Ident Function

data Env = Env {functions :: FEnv, variables :: VEnv}

type Program = Continuation

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

memoryRead :: MemoryAddress -> Memory -> Int32
memoryRead address memory =
    case MapStrict.lookup address memory of
        Nothing -> error "Internal interpreter error: value not found under given adress."
        Just v -> v

memoryWrite :: MemoryAddress -> Int32 -> Memory -> Memory
memoryWrite address value memory = MapStrict.insert address value memory

liftMemory :: (Memory -> Memory) -> (State -> Interruption a State)
liftMemory f state = let m = (memory state) in Right $ state { memory = f m }

sizeOf ::  AST.Type -> Int32
sizeOf valueType = case valueType of
    AST.Tuple types             -> sum $ map sizeOf types
    AST.Array valueType count   -> (sizeOf valueType) * count
    _                           -> 1

memoryCopy :: AST.Type -> MemoryAddress -> MemoryAddress -> Memory -> Memory
memoryCopy valueType from to memory = case valueType of
    AST.Tuple types             -> memory' where
        offsets = init $ scanl (+) 0 $ map sizeOf types
        actions = map (\(offset, valueType') -> memoryCopy valueType' (from + offset) (to + offset)) $ zip offsets types
        memory' = (foldl (.) id actions) memory

    AST.Array valueType' count  -> memory' where
        elementSize = sizeOf valueType'
        actions = map (\i -> memoryCopy valueType' (from + i * elementSize) (to + i * elementSize)) [0 .. count-1]
        memory' = (foldl (.) id actions) memory

    _                           -> memory' where
        value = memoryRead from memory
        memory' = memoryWrite to value memory

findFunction :: AST.Ident -> FEnv -> Either String Function
findFunction ident fenv = maybeToEither (format "Function \"%0\" not defined." [ident]) $ Map.lookup ident fenv

assertFunctionArguments :: Function -> AST.Ident -> [AST.Type] -> Either String ()
assertFunctionArguments function ident types = do
    let types_length = length types
    let parameters_length = length $ parameters function
    unless (types_length == parameters_length) $ Left $ format "Function \"%0\": expected %1 arguments, got %2." [ident, show parameters_length, show types_length]
    let equal t1 t2 = unless (t1 == t2) $ Left $ format "Could not match \"%0\" with \"%1\"." [show t1, show t2]
    zipWithM_ equal types $ parameters function


initialFEnv :: FEnv
initialFEnv = Map.fromList [("readI32", readI32_f), ("writeI32", writeI32_f)] where
    readI32_f = Function { body = Right readI32, parameters = [], resultType = AST.I32 } where
        readI32 :: Continuation
        readI32 address state =
            Right $ state { input = input', memory = memory' } where
                value : input' = input state
                memory' = memoryWrite address value (memory state)
    writeI32_f = Function { body = Right writeI32, parameters = [AST.I32], resultType = AST.unit } where
        writeI32 :: Continuation
        writeI32 address state =
            Right $ state { output = (out :> value) } where
                out = output state
                value = memoryRead address (memory state)

initialVEnv :: VEnv
initialVEnv = Map.empty

identityContinuation :: Continuation
identityContinuation memoryAddress state = Right state

typeOf :: AST.Expr -> Either String AST.Type
typeOf expr = case expr of
    AST.BinaryOperator  expr1 expr2 kind    -> undefined
    AST.UnaryOperator   expr kind           -> undefined
    AST.Identifier      ident               -> undefined
    AST.LiteralExpr     literal             -> undefined
    AST.FunctionCall    ident exprs         -> undefined
    AST.TupleLookup     expr index          -> undefined
    AST.ArrayElements   exprs               -> undefined
    AST.ArrayRepeat     expr count          -> do
        valueType <- typeOf expr
        return $ AST.Array valueType count
    AST.ArrayRange      begin end           -> do
        unless (begin < end) $ Left $ format "Expected nonempty range, got [%0..%1]" [show begin, show end]
        return $ AST.Array AST.I32 (end - begin)
    AST.TupleConstruct  exprs               -> do
        types <- mapM typeOf exprs
        return $ AST.Tuple types
    AST.BlockExpr       (AST.Block _ expr)  -> typeOf expr
    AST.IfElse          expr (AST.Block _ expr1) (AST.Block _ expr2)  -> do
        type1 <- typeOf expr1
        type2 <- typeOf expr2
        unless (type1 == type2) $ Left $ format "\"if-else\": could not match \"%0\" with \"%1\"" [show type1, show type2]
        return type1


compile :: AST.Program -> Either String Program
compile ast =
    let functionDeclarations = AST.functions ast in
    let fenv = insertFunctions functionDeclarations initialFEnv in
    do
        sequence_ $ fmap body fenv
        main <- findFunction "main" fenv
        unless (parameters main == []) $ Left "\"main\" function must take no arguments."
        unless (resultType main == AST.unit) $ Left "\"main\" function must return '()' type."
        body main

insertFunctions :: [AST.FunctionDeclaration] -> FEnv -> FEnv
insertFunctions functionDeclarations fenv =
    let fenv' = foldl' addFun fenv functionDeclarations where
        addFun :: FEnv -> AST.FunctionDeclaration -> FEnv
        addFun fenv function = Map.insert (AST.name function) (compileFunction function fenv') fenv in
    fenv'

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

execute :: Program -> Input -> Interruption String Output
execute program input =
    let memory = Map.empty in
    let state = State input Nil memory in
    mapInterrupt (\( State _ output memory) -> memory `seq` output) (program 0 state)
