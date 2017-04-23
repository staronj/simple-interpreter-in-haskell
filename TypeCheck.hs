-- Jakub Staroń, 2017

module TypeCheck (typeCheck) where

import Data.Int
import Data.List
import Control.Monad
import qualified Data.Map.Strict as Map

import qualified AST
import FormatString

data Variable = Variable { varType :: AST.Type, mutable :: Bool }
type VEnv = Map.Map AST.Ident Variable

data Function = Function { parameters :: [AST.Type], resultType :: AST.Type}
type FEnv = Map.Map AST.Ident Function

data Env = Env {functions :: FEnv, variables :: VEnv, path :: AST.Ident }

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

findFunction :: AST.Ident -> Env -> Either String Function
findFunction ident env = maybeToEither (format "cannot find function `%0` in this scope" [ident]) $ Map.lookup ident $ functions env

findVariable :: AST.Ident -> Env -> Either String Variable
findVariable ident env = maybeToEither (format "cannot find value `%0` in this scope" [ident]) $ Map.lookup ident $ variables env

insertFunction :: AST.Ident -> Function -> Env -> Env
insertFunction ident function env = env' where
    env' = env { functions = functions' }
    functions' = Map.insert ident function (functions env)

insertVariable :: AST.Ident -> Variable -> Env -> Env
insertVariable ident variable env = env' where
    env' = env { variables = variables' }
    variables' = Map.insert ident variable (variables env)


insertPattern :: AST.Pattern -> AST.Type -> Env -> Either String Env
insertPattern pattern valueType env = case pattern of
    AST.PatternVariable name            -> return $ insertVariable name (Variable valueType False) env
    AST.PatternMutableVariable name     -> return $ insertVariable name (Variable valueType True) env
    AST.PatternIgnore                   -> return env
    AST.PatternTuple pattern'           ->
        case valueType of
            AST.Tuple types -> do
                let pattern_length = length pattern'
                let types_length = length types
                unless (pattern_length == types_length) $ Left $ format "could not match tuple of %0 elements with tuple of %1 elements" [show types_length, show pattern_length]
                let pairs = map (uncurry insertPattern) $ zip pattern' types
                (foldl' (>=>) return pairs) env
            _               -> Left $ format "could not match \"%0\" with tuple" [show valueType]

pushSubpath :: AST.Ident -> AST.Ident -> Env -> Env
pushSubpath nodeType name env = env { path = format "%0:%1@%2" [nodeType, name, path env]}

assertTypesEqual :: Env -> AST.Type -> AST.Type -> Either String ()
assertTypesEqual env type1 type2 =
    unless (type1 == type2) $ Left $ format "could not match type \"%0\" with type \"%1\"\nat %2" [show type1, show type2, path env]

assertMutable :: Variable -> Either String ()
assertMutable variable = unless (mutable variable) $ Left $ "expected mutable variable"

assertIsArray :: AST.Type -> Either String ()
assertIsArray valueType = case valueType of
    AST.Array _ _   -> return ()
    _               -> Left $ format "can not index a value of type \"%0\"" [show valueType]


assertFunctionArguments :: Env -> Function -> AST.Ident -> [AST.Type] -> Either String ()
assertFunctionArguments env function ident types = do
    let types_length = length types
    let parameters_length = length $ parameters function
    unless (types_length == parameters_length) $ Left $ format "Function \"%0\": expected %1 arguments, got %2." [ident, show parameters_length, show types_length]
    zipWithM_ (assertTypesEqual env) types $ parameters function

initialEnv :: Env
initialEnv = Env functions variables "#" where
    functions = Map.fromList [("readI32", readI32), ("writeI32", writeI32)]
    readI32 = Function { parameters = [], resultType = AST.I32 }
    writeI32 = Function { parameters = [AST.I32], resultType = AST.unit }
    variables = Map.empty

typeCheck :: AST.Program -> Either String ()
typeCheck ast =
    let functionDeclarations = AST.functions ast in
    let env = insertFunctions functionDeclarations initialEnv in
    do
        sequence_ $ map (flip typeCheckFunction env) functionDeclarations
        main <- findFunction "main" env
        unless (parameters main == []) $ Left "\"main\" function must take no arguments"
        unless (resultType main == AST.unit) $ Left "\"main\" function must return '()' type"
        return ()

insertFunctions :: [AST.FunctionDeclaration] -> Env -> Env
insertFunctions functions env =
    foldl' (flip insertASTFunction) env functions where
        insertASTFunction :: AST.FunctionDeclaration -> Env -> Env
        insertASTFunction function = insertFunction ident function' where
            ident = AST.name function
            function' = Function parameters resultType
            parameters = map AST.valueType (AST.parameters function)
            resultType = AST.resultType function

typeCheckFunction :: AST.FunctionDeclaration -> Env -> Either String ()
typeCheckFunction fun env = do
        env <- return $ pushSubpath "function" (AST.name fun) env
        let parameters' = map (\funParam -> insertPattern (AST.pattern funParam) (AST.valueType funParam)) $ AST.parameters fun
        env' <- (foldl' (>=>) return parameters') env
        blockType <- typeCheckBlock (AST.body fun) env'
        let resultType = AST.resultType fun
        assertTypesEqual env blockType resultType

typeCheckBlock :: AST.Block -> Env -> Either String AST.Type
typeCheckBlock (AST.Block stmts expr) env = do
        env <- return $ insertFunctions [funDecl | AST.FunDeclStmt funDecl <- stmts] env
        env <- return $ pushSubpath "block" "0" env
        stmts <- return $ map typeCheckStmt stmts
        stmts <- return $ zipWith (\i s -> s . pushSubpath "stmt" (show i)) [0..] stmts
        env <- (foldl' (>=>) return stmts) env
        liftM varType $ typeCheckExpr expr env

typeCheckStmt :: AST.Stmt -> Env -> Either String Env
typeCheckStmt stmt env = case stmt of
    AST.FunDeclStmt funDecl                     -> typeCheckFunction funDecl env >> return env
    AST.If expr block                           -> do
        exprType <- liftM varType $ typeCheckExpr expr env
        assertTypesEqual env exprType AST.Bool
        blockType <- typeCheckBlock block env
        assertTypesEqual env blockType AST.unit
        return env
    AST.Stmt expr                               -> typeCheckExpr expr env >> return env
    AST.StrictStmt expr                         -> undefined
    AST.Loop block                              -> undefined
    AST.While expr block                        -> do
        exprType <- liftM varType $ typeCheckExpr expr env
        blockType <- typeCheckBlock block env
        assertTypesEqual env exprType AST.Bool
        assertTypesEqual env blockType AST.unit
        return env
    AST.IterableForLoop ident expr block        -> undefined
    AST.RangeForLoop ident expr1 expr2 block    -> do
        exprType1 <- liftM varType $ typeCheckExpr expr1 env
        exprType2 <- liftM varType $ typeCheckExpr expr2 env
        let env' = insertVariable ident (Variable { varType = AST.I32, mutable = False }) env
        blockType <- typeCheckBlock block env'
        assertTypesEqual env exprType1 AST.I32
        assertTypesEqual env exprType2 AST.I32
        assertTypesEqual env blockType AST.unit
        return env
    AST.Break                                   -> undefined
    AST.Continue                                -> undefined
    AST.LetStmt pattern valueType expr          -> do
        exprType <- liftM varType $ typeCheckExpr expr env
        case valueType of
            Nothing -> return ()
            Just valueType -> assertTypesEqual env valueType exprType
        insertPattern pattern exprType env


typeCheckExpr :: AST.Expr -> Env -> Either String Variable
typeCheckExpr expr env = case expr of
    AST.BinaryOperator  expr1 expr2 kind    -> do
        exprVar1 <- typeCheckExpr expr1 env
        exprVar2 <- typeCheckExpr expr2 env
        let exprType1 = varType exprVar1
        let exprType2 = varType exprVar2
        case kind of
            _ | kind `elem` [AST.Or, AST.And] -> do
                assertTypesEqual env exprType1 AST.Bool
                assertTypesEqual env exprType2 AST.Bool
                return $ Variable AST.Bool False
            _ | kind `elem` [AST.Equal, AST.NotEqual] -> do
                assertTypesEqual env exprType1 exprType2
                return $ Variable AST.Bool False
            AST.Less -> do
                assertTypesEqual env exprType1 AST.I32
                assertTypesEqual env exprType2 AST.I32
                return $ Variable AST.Bool False
            _ | kind `elem` [AST.Add, AST.Subtract, AST.Multiply, AST.Divide, AST.Modulo] -> do
                assertTypesEqual env exprType1 AST.I32
                assertTypesEqual env exprType2 AST.I32
                return $ Variable AST.I32 False
            AST.Assign -> do
                assertMutable exprVar1
                assertTypesEqual env exprType2 exprType1
                return $ Variable AST.unit False
            AST.ArrayLookup -> do
                assertIsArray exprType1
                assertTypesEqual env exprType2 AST.I32
                let AST.Array valueType _ = exprType1
                return $ exprVar1 { varType = valueType }
    AST.UnaryOperator   expr kind           -> undefined
    AST.Identifier      ident               -> findVariable ident env
    AST.LiteralExpr     literal             -> do
        case literal of
            AST.LiteralI32 _    -> return $ Variable AST.I32 False
            AST.LiteralBool _   -> return $ Variable AST.Bool False
    AST.FunctionCall    ident exprs         -> do
        variables <- mapM (flip typeCheckExpr env) exprs
        let types = map varType variables
        function <- findFunction ident env
        assertFunctionArguments env function ident types
        return $ Variable (resultType function) False
    AST.TupleLookup     expr integer        -> undefined
    AST.ArrayElements   exprs               -> undefined
    AST.ArrayRepeat     expr integer        -> do
        exprType <- liftM varType $ typeCheckExpr expr env
        return $ Variable (AST.Array exprType integer) False
    AST.ArrayRange      integer1 integer2   -> undefined
    AST.TupleConstruct  exprs               -> do
        variables <- mapM (flip typeCheckExpr env) exprs
        let types = map varType variables
        return $ Variable (AST.Tuple types) False
    AST.BlockExpr       block               -> undefined
    AST.IfElse          expr block1 block2  -> undefined