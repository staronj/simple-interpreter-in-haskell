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

data Function = Function { body :: Continuation, parameters :: [AST.Type], resultType :: AST.Type}
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


initialFEnv :: FEnv
initialFEnv = Map.fromList [("readI32", readI32_f), ("writeI32", writeI32_f)] where
    readI32_f = Function { body = readI32, parameters = [], resultType = AST.I32 } where
        readI32 :: Continuation
        readI32 address state =
            Right $ state { input = input', memory = memory' } where
                value : input' = input state
                memory' = memoryWrite address value (memory state)
    writeI32_f = Function { body = writeI32, parameters = [AST.I32], resultType = AST.unit } where
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
    do
        fEnv' <- insertFunctions functions fEnv
        main <- maybeToEither "main function not found" $ Map.lookup "main" fEnv'
        unless (parameters main == []) (Left "main function should not take any arguments")
        unless (resultType main == AST.unit) (Left "main function should return '()' type")
        return $ (body main)


insertFunctions :: [AST.FunctionDeclaration] -> FEnv -> Either String FEnv
insertFunctions functions fEnv =
    let fEnv' = foldl' addFun fEnv functions where
        addFun :: FEnv -> AST.FunctionDeclaration -> FEnv
        addFun fenv function = Map.insert ident (compileFunction fEnv' function) fenv where
            ident = AST.name function in
    Right fEnv'

compileFunction :: FEnv -> AST.FunctionDeclaration -> Function
compileFunction fenv function =
    let venv = initialVEnv in
    let resultType = AST.resultType function in
    let parameters = map AST.valueType (AST.parameters function) in
    let cont = compileBlockDummy (Env fenv venv) (AST.body function) in
    Function cont parameters resultType

compileBlockDummy :: Env -> AST.Block -> Continuation
compileBlockDummy env (AST.Block stmts expr) =
    let (Just readF) = Map.lookup "readI32" (fEnv env) in
    let (Just writeF) = Map.lookup "writeI32" (fEnv env) in
    let cont address state = do
            state' <- (body readF) address state
            (body writeF) address state' in
    cont

compileBlock :: Env -> AST.Block -> Either String Continuation
compileBlock env (AST.Block stmts expr) =
    let functionDeclarations = [funDecl | AST.FunDeclStmt funDecl <- stmts] in
    undefined

compileStmt :: Env -> AST.Stmt -> Continuation
compileStmt env stmt address = undefined

compileExpr :: Env -> AST.Expr -> Continuation
compileExpr env expr address = undefined

execute :: Program -> Input -> Interruption String Output
execute program input =
    let memory = Map.empty in
    let state = State input Nil memory in
    mapInterrupt (\( State _ output _) -> output) (program 0 state)
