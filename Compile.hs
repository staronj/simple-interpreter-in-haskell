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

data Env = Env {fEnv :: FEnv, vEnv :: VEnv}

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


compile :: AST.Program -> Either String Program
compile ast =
    let fEnv = initialFEnv in
    let functions = AST.functions ast in
    let fEnv' = insertFunctions functions fEnv in
    do
        sequence_ $ fmap body fEnv'
        main <- maybeToEither "main function not found" $ Map.lookup "main" fEnv'
        unless (parameters main == []) (Left "main function should not take any arguments")
        unless (resultType main == AST.unit) (Left "main function should return '()' type")
        body main

insertFunctions :: [AST.FunctionDeclaration] -> FEnv -> FEnv
insertFunctions functions fEnv =
    let fEnv' = foldl' addFun fEnv functions where
        addFun :: FEnv -> AST.FunctionDeclaration -> FEnv
        addFun fenv function = Map.insert (AST.name function) (compileFunction fEnv' function) fenv in
    fEnv'

compileFunction :: FEnv -> AST.FunctionDeclaration -> Function
compileFunction fenv function =
    let venv = initialVEnv in
    let resultType = AST.resultType function in
    let parameters = map AST.valueType (AST.parameters function) in
    let cont = compileBlockDummy (Env fenv venv) (AST.body function) in
    Function cont parameters resultType

compileBlockDummy :: Env -> AST.Block -> Either String Continuation
compileBlockDummy env (AST.Block stmts expr) =
    do
        readF <- maybeToEither "Could not find readI32" $ Map.lookup "readI32" (fEnv env)
        writeF <- maybeToEither "Could not find writeI32" $ Map.lookup "writeI32" (fEnv env)
        readF <- body readF
        writeF <- body writeF
        let cont address state = do
                state' <- readF address state
                writeF address state'
        return cont

compileBlock :: Env -> AST.Block -> Either String Continuation
compileBlock env (AST.Block stmts expr) =
    let functionDeclarations = [funDecl | AST.FunDeclStmt funDecl <- stmts] in
    let fenv = insertFunctions functionDeclarations (fEnv env) in
    let env = env {fEnv = fenv} in
    do
        sequence_ $ fmap body (fEnv env)
        undefined

compileStmt :: Env -> AST.Stmt -> Either String (Env, Continuation)
compileStmt env stmt = undefined

compileExpr :: Env -> AST.Expr -> Either String Continuation
compileExpr env expr = undefined

execute :: Program -> Input -> Interruption String Output
execute program input =
    let memory = Map.empty in
    let state = State input Nil memory in
    mapInterrupt (\( State _ output _) -> output) (program 0 state)
