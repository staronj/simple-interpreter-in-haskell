-- Jakub Staroń, 2017
{-# LANGUAGE RankNTypes, TypeSynonymInstances #-}

module Compile (compile, execute) where

import Data.Int
import Data.List
import Control.Monad
import Control.Monad.Cont
import qualified Data.Map.Lazy as Map

import qualified AST
import RList

type Input = [Int32]
type Output = RList Int32

type Interruption err val = Either (err, val) val


mapInterrupt :: (a -> b) -> Interruption c a -> Interruption c b
mapInterrupt f (Left (err, v)) = Left (err, f v)
mapInterrupt f (Right v) = Right (f v)

type MemoryAddress = Int32

type Memory = Map.Map MemoryAddress Int32
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
    case Map.lookup address memory of
        Nothing -> error "Internal interpreter error: value not found under given adress"
        Just v -> v

memoryWrite :: MemoryAddress -> Int32 -> Memory -> Memory
memoryWrite address value memory = Map.insert address value memory

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
typeOf expr = undefined


compile :: AST.Program -> Either String Program
compile ast =
    let fenv = initialFEnv in
    let functionDeclarations = AST.functions ast in
    let fenv' = insertFunctions functionDeclarations fenv in
    do
        sequence_ $ fmap body fenv'
        main <- maybeToEither "main function not found" $ Map.lookup "main" fenv'
        unless (parameters main == []) (Left "main function should not take any arguments")
        unless (resultType main == AST.unit) (Left "main function should return '()' type")
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
    let cont = compileBlockDummy (AST.body function) (Env fenv venv) in
    Function cont parameters resultType

compileBlockDummy :: AST.Block -> Env -> Either String Continuation
compileBlockDummy (AST.Block stmts expr) env =
    do
        readF <- maybeToEither "Could not find readI32" $ Map.lookup "readI32" (functions env)
        writeF <- maybeToEither "Could not find writeI32" $ Map.lookup "writeI32" (functions env)
        readF <- body readF
        writeF <- body writeF
        let cont address state = do
                state' <- readF address state
                writeF address state'
        return cont

compileBlock :: AST.Block -> Env -> Either String Continuation
compileBlock (AST.Block stmts expr) env =
    let functionDeclarations = [funDecl | AST.FunDeclStmt funDecl <- stmts] in
    let fenv = insertFunctions functionDeclarations (functions env) in
    let env = env {functions = fenv} in
    do
        sequence_ $ fmap body (functions env)
        (env', offset, cont1) <- compileStmtSequence stmts env
        cont2 <- compileExpr expr env'
        let cont memoryAddress state = do
            state <- cont1 memoryAddress state
            cont2 (memoryAddress + offset) state
        return cont

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
compileStmt stmt env = undefined

compileExpr :: AST.Expr -> Env -> Either String Continuation
compileExpr expr env = undefined

execute :: Program -> Input -> Interruption String Output
execute program input =
    let memory = Map.empty in
    let state = State input Nil memory in
    mapInterrupt (\( State _ output _) -> output) (program 0 state)
