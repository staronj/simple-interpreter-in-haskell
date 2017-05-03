-- Jakub Staroń, 2017

module TypeCheck (typeCheck) where

import Data.List (foldl', intercalate)
import Control.Monad
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified AST
import FormatString (format)

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate xs = either Just (const Nothing) $ foldM insert Set.empty xs where
  insert :: Ord a => Set.Set a -> a -> Either a (Set.Set a)
  insert s x = if Set.member x s then Left x else Right $ Set.insert x s

data Variable =
  Variable
  { varType :: AST.Type
  , mutable :: Bool }

data Function =
  Function
  { parameters :: [AST.Type]
  , resultType :: AST.Type}

type VEnv = Map.Map AST.Ident Variable
type FEnv = Map.Map AST.Ident Function

newtype Path = Path [(String, String)]

instance Show Path where
  show (Path path) = intercalate "@" elems where
    elems = map (\(a, b) -> format "%0:%1" [a, b]) path

data Env =
  Env
  { functions :: FEnv
  , variables :: VEnv
  , path :: Path
  , inLoop :: Bool }

newtype TypeCheckedProgram = TypeCheckedProgram AST.Program

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

-- Messages
functionNotFoundMessage :: Env -> AST.Ident -> String
functionNotFoundMessage env name = format "cannot find function `%0` in scope\n%1" [name, show $ path env]

variableNotFoundMessage :: Env -> AST.Ident -> String
variableNotFoundMessage env name = format "cannot find value `%0` in scope\n%1" [name, show $ path env]

tupleLengthNotMatchMessage :: (Num a, Show a) => Env -> a -> a -> String
tupleLengthNotMatchMessage env first second = format "could not match tuple of %0 elements with tuple of %1 elements in scope\n%2" [show first, show second, show $ path env]

couldNotMatchTypeWithTupleMessage :: Env -> AST.Type -> String
couldNotMatchTypeWithTupleMessage env valueType = format "could not match '%0' with tuple in scope\n%1" [show valueType, show $ path env]

couldNotMatchTypesMessage :: Env -> AST.Type -> AST.Type -> String
couldNotMatchTypesMessage env type1 type2 = format "could not match type '%0' with type '%1'\nat %2" [show type1, show type2, show $ path env]

expectedMutableVariableMessage :: Env -> String
expectedMutableVariableMessage env = format "expected mutable variable\nat%0" [show $ path env]

expectedArrayMessage :: Env -> AST.Type -> String
expectedArrayMessage env valueType = format "expected array, not a value of type '%0'\nat%1" [show valueType, show $ path env]

expectedTupleMessage :: Env -> AST.Type -> String
expectedTupleMessage env valueType = format "expected tuple, not a value of type '%0'\nat%1" [show valueType, show $ path env]

invalidNumberOfArgumentsMessage :: (Num a, Show a) => Env -> AST.Ident -> a -> a -> String
invalidNumberOfArgumentsMessage env name expected got = format "function '%0': expected %1 arguments, got %2\at%3" [name, show expected, show got, show $ path env]

mainFunctionMustTakeNoArgumentsMessage :: String
mainFunctionMustTakeNoArgumentsMessage = "'main' function must take no arguments"

mainFunctionMustReturnUnitMessage :: String
mainFunctionMustReturnUnitMessage = "'main' function must return '()' type"

duplicatedFunctionIdentifierInScopeMessage :: Env -> AST.Ident -> String
duplicatedFunctionIdentifierInScopeMessage env name =  format "duplicated definitions of function '%0' at\n%1" [name, show $ path env]

outOfBoundsTupleLookupMessage :: (Num a, Show a) => Env -> AST.Type -> a -> String
outOfBoundsTupleLookupMessage env valueType n = format "attempted out-of-bounds tuple index `%0` on type `%1` at\n%2" [show n, show valueType, show $ path env]

arrayFromInvalidRangeMessage :: (Num a, Show a) => Env -> a -> a -> String
arrayFromInvalidRangeMessage env begin end = format "trying to create array from invalid range `(%0, %1)`" [show begin, show end, show $ path env]

controlFlowStmtOutsideOfLoopMessage :: Env -> String
controlFlowStmtOutsideOfLoopMessage env = format "usage of control flow instruction oustide of loop at \n%0" [show $ path env]

-- Messages End

findFunction :: AST.Ident -> Env -> Either String Function
findFunction name env = maybeToEither message $ Map.lookup name $ functions env where
  message = functionNotFoundMessage env name

findVariable :: AST.Ident -> Env -> Either String Variable
findVariable name env = maybeToEither message $ Map.lookup name $ variables env where
  message = variableNotFoundMessage env name

insertFunction :: AST.Ident -> Function -> Env -> Env
insertFunction name function env = env' where
  env' = env { functions = functions' }
  functions' = Map.insert name function $ functions env

insertVariable :: AST.Ident -> Variable -> Env -> Env
insertVariable name variable env = env' where
  env' = env { variables = variables' }
  variables' = Map.insert name variable $ variables env

insertPattern :: AST.Pattern -> AST.Type -> Env -> Either String Env
insertPattern pattern valueType env = case pattern of
  AST.PatternVariable name            -> return $ insertVariable name (Variable valueType False) env
  AST.PatternMutableVariable name     -> return $ insertVariable name (Variable valueType True) env
  AST.PatternIgnore                   -> return env
  AST.PatternTuple pattern'           -> case valueType of
    AST.Tuple types ->
      let patternLength = length pattern' in
      let typesLength = length types in
      do
        unless (patternLength == typesLength) $ throwError $ tupleLengthNotMatchMessage env typesLength patternLength
        let pairs = zipWith insertPattern pattern' types
        foldl' (>=>) return pairs env
    _               -> throwError $ couldNotMatchTypeWithTupleMessage env valueType

pushSubpath :: String -> String -> Env -> Env
pushSubpath nodeType name env = env { path = Path $ (nodeType, name) : oldPath} where
  Path oldPath = path env

setSubpath :: String -> String -> Env -> Env
setSubpath nodeType name env = env { path = Path $ (nodeType, name) : oldPath} where
  Path (_ : oldPath) = path env

-- Assertions begin

assertTypesEqual :: Env -> AST.Type -> AST.Type -> Either String ()
assertTypesEqual env type1 type2 =
  unless (type1 == type2) $ throwError $ couldNotMatchTypesMessage env type1 type2

assertMutable :: Env -> Variable -> Either String ()
assertMutable env variable = unless (mutable variable) $ throwError $ expectedMutableVariableMessage env

assertIsArray :: Env -> AST.Type -> Either String ()
assertIsArray env valueType = case valueType of
    AST.Array {}  -> return ()
    _             -> throwError $ expectedArrayMessage env valueType

assertIsTuple :: Env -> AST.Type -> Either String ()
assertIsTuple env valueType = case valueType of
    AST.Tuple {}  -> return ()
    _             -> throwError $ expectedTupleMessage env valueType

assertFunctionArguments :: Env -> Function -> AST.Ident -> [AST.Type] -> Either String ()
assertFunctionArguments env function ident types =
    let typesLength = length types in
    let parametersLength = length $ parameters function in
    do
      unless (typesLength == parametersLength) $ throwError $ invalidNumberOfArgumentsMessage env ident parametersLength typesLength
      zipWithM_ (assertTypesEqual env) types $ parameters function

assertInLoop :: Env -> Either String ()
assertInLoop env = unless (inLoop env) $ throwError $ controlFlowStmtOutsideOfLoopMessage env
-- Assertions end

initialEnv :: Env
initialEnv =
  Env
  { functions = functions
  , variables = variables
  , path = Path []
  , inLoop = False } where
    functions = Map.fromList [("readI32", readI32), ("writeI32", writeI32)]
    variables = Map.empty
    readI32 = Function { parameters = [], resultType = AST.I32 }
    writeI32 = Function { parameters = [AST.I32], resultType = AST.unit }

typeCheck :: AST.Program -> Either String AST.Program
typeCheck ast =
    let functionDeclarations = AST.functions ast in
    do
        env <- insertFunctions functionDeclarations initialEnv
        mapM_ (`typeCheckFunction` env) functionDeclarations
        main <- findFunction "main" env
        unless (null $ parameters main) $ throwError mainFunctionMustTakeNoArgumentsMessage
        unless (resultType main == AST.unit) $ throwError mainFunctionMustReturnUnitMessage
        return ast

insertFunctions :: [AST.FunctionDeclaration] -> Env -> Either String Env
insertFunctions functions env =
    let duplicateName = findDuplicate $ map AST.name functions in
    do
        maybe (return ()) (throwError . duplicatedFunctionIdentifierInScopeMessage env) duplicateName
        return $ foldl' (flip insertASTFunction) env functions
        where
          insertASTFunction :: AST.FunctionDeclaration -> Env -> Env
          insertASTFunction function = insertFunction name function' where
              name = AST.name function
              function' = Function parameters resultType
              parameters = map AST.valueType (AST.parameters function)
              resultType = AST.resultType function

typeCheckFunction :: AST.FunctionDeclaration -> Env -> Either String ()
typeCheckFunction fun env = do
        env <- return $ pushSubpath "function" (AST.name fun) env
        let parameters' = map (\funParam -> insertPattern (AST.pattern funParam) (AST.valueType funParam)) $ AST.parameters fun
        env' <- foldl' (>=>) return parameters' env
        blockType <- typeCheckBlock (AST.body fun) env'
        let resultType = AST.resultType fun
        assertTypesEqual env blockType resultType

typeCheckBlock :: AST.Block -> Env -> Either String AST.Type
typeCheckBlock block@(AST.Block stmts expr) env = do
        env <- insertFunctions (AST.getFunctionsFromBlock block) env
        env <- return $ pushSubpath "block" "0" env
        env <- return $ pushSubpath "" "" env
        stmts <- return $ map typeCheckStmt stmts
        stmts <- return $ zipWith (\i s -> s . setSubpath "stmt" (show i)) [0..] stmts
        env <- foldl' (>=>) return stmts env
        varType <$> typeCheckExpr expr env

typeCheckStmt :: AST.Stmt -> Env -> Either String Env
typeCheckStmt stmt env = case stmt of
    AST.FunDeclStmt funDecl                     -> typeCheckFunction funDecl env >> return env
    AST.If expr block                           -> do
        exprType <- varType <$> typeCheckExpr expr env
        assertTypesEqual env exprType AST.Bool
        blockType <- typeCheckBlock block env
        assertTypesEqual env blockType AST.unit
        return env
    AST.Stmt expr                               -> typeCheckExpr expr env >> return env
    AST.StrictStmt expr                         -> do
        exprType <- varType <$> typeCheckExpr expr env
        assertTypesEqual env exprType AST.unit
        return env
    AST.Loop block                              -> do
        let env' = env { inLoop = True }
        blockType <- typeCheckBlock block env'
        assertTypesEqual env blockType AST.unit
        return env
    AST.While expr block                        -> do
        exprType <- varType <$> typeCheckExpr expr env
        let env' = env { inLoop = True }
        blockType <- typeCheckBlock block env'
        assertTypesEqual env exprType AST.Bool
        assertTypesEqual env blockType AST.unit
        return env
    AST.IterableForLoop ident expr block        -> do
        exprType <- varType <$> typeCheckExpr expr env
        assertIsArray env exprType
        let AST.Array valueType _ = exprType
        let env' = insertVariable ident Variable { varType = valueType, mutable = False } env
        let env'' = env' { inLoop = True }
        blockType <- typeCheckBlock block env''
        assertTypesEqual env blockType AST.unit
        return env
    AST.RangeForLoop ident expr1 expr2 block    -> do
        exprType1 <- varType <$> typeCheckExpr expr1 env
        exprType2 <- varType <$> typeCheckExpr expr2 env
        let env' = insertVariable ident Variable { varType = AST.I32, mutable = False } env
        let env'' = env' { inLoop = True }
        blockType <- typeCheckBlock block env''
        assertTypesEqual env exprType1 AST.I32
        assertTypesEqual env exprType2 AST.I32
        assertTypesEqual env blockType AST.unit
        return env
    AST.Break                                   -> do
        assertInLoop env
        return env
    AST.Continue                                -> do
        assertInLoop env
        return env
    AST.LetStmt pattern valueType expr          -> do
        exprType <- varType <$> typeCheckExpr expr env
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
            AST.Less -> do
                assertTypesEqual env exprType1 AST.I32
                assertTypesEqual env exprType2 AST.I32
                return $ Variable AST.Bool False
            _ | kind `elem` [AST.Add, AST.Subtract, AST.Multiply, AST.Divide, AST.Modulo] -> do
                assertTypesEqual env exprType1 AST.I32
                assertTypesEqual env exprType2 AST.I32
                return $ Variable AST.I32 False
    AST.UnaryOperator   expr kind           -> do
        exprVar <- typeCheckExpr expr env
        let exprType = varType exprVar
        case kind of
            AST.Negate -> do
                assertTypesEqual env exprType AST.I32
                return $ Variable AST.I32 False
            AST.Not -> do
                assertTypesEqual env exprType AST.Bool
                return $ Variable AST.Bool False

    AST.Identifier      ident               -> findVariable ident env
    AST.Equal expr1 expr2                   -> do
      exprType1 <- varType <$> typeCheckExpr expr1 env
      exprType2 <- varType <$> typeCheckExpr expr2 env
      assertTypesEqual env exprType2 exprType1
      return $ Variable AST.Bool False
    AST.NotEqual expr1 expr2                   -> do
        exprType1 <- varType <$> typeCheckExpr expr1 env
        exprType2 <- varType <$> typeCheckExpr expr2 env
        assertTypesEqual env exprType2 exprType1
        return $ Variable AST.Bool False
    AST.Assign expr1 expr2 -> do
        exprVar1 <- typeCheckExpr expr1 env
        exprVar2 <- typeCheckExpr expr2 env
        let exprType1 = varType exprVar1
        let exprType2 = varType exprVar2
        assertMutable env exprVar1
        assertTypesEqual env exprType2 exprType1
        return $ Variable AST.unit False
    AST.ArrayLookup expr1 expr2 -> do
        exprVar1 <- typeCheckExpr expr1 env
        exprVar2 <- typeCheckExpr expr2 env
        let exprType1 = varType exprVar1
        let exprType2 = varType exprVar2
        assertIsArray env exprType1
        assertTypesEqual env exprType2 AST.I32
        let AST.Array valueType _ = exprType1
        return $ exprVar1 { varType = valueType }
    AST.Dereference expr -> error "Not implemented."
    AST.Borrow expr -> error "Not implemented."
    AST.MutableBorrow expr -> error "Not implemented."
    AST.LiteralExpr     literal             ->
        case literal of
            AST.LiteralI32 _    -> return $ Variable AST.I32 False
            AST.LiteralBool _   -> return $ Variable AST.Bool False
    AST.FunctionCall    ident exprs         -> do
        variables <- mapM (`typeCheckExpr` env) exprs
        let types = map varType variables
        function <- findFunction ident env
        assertFunctionArguments env function ident types
        return $ Variable (resultType function) False
    AST.TupleLookup     expr n        -> do
      exprVar <- typeCheckExpr expr env
      let exprType = varType exprVar
      assertIsTuple env exprType
      let AST.Tuple types = exprType
      unless (length types > fromIntegral n) $ throwError $ outOfBoundsTupleLookupMessage env exprType n
      return $ exprVar { varType = types !! fromIntegral n }
    AST.ArrayElements   exprs               -> do
        exprs <- mapM (`typeCheckExpr` env) exprs
        exprs <- return $ map varType exprs
        when (null exprs) $ error "Internal interpreter error: empty exprs list in AST.ArrayElements."
        let commonType = head exprs
        mapM_ (assertTypesEqual env commonType) exprs
        return $ Variable (AST.Array commonType $ fromIntegral $ length exprs) False
    AST.ArrayRepeat     expr integer        -> do
        exprType <- varType <$> typeCheckExpr expr env
        return $ Variable (AST.Array exprType integer) False
    AST.ArrayRange      begin end           -> do
        unless (begin < end) $ throwError $ arrayFromInvalidRangeMessage env begin end
        return $ Variable (AST.Array AST.I32 (end - begin)) False
    AST.TupleConstruct  exprs               -> do
        variables <- mapM (`typeCheckExpr` env) exprs
        let types = map varType variables
        return $ Variable (AST.Tuple types) False
    AST.BlockExpr       block               -> do
        blockType <- typeCheckBlock block env
        return $ Variable blockType False
    AST.IfElse          expr block1 block2  -> do
        exprType <- varType <$> typeCheckExpr expr env
        blockType1 <- typeCheckBlock block1 env
        blockType2 <- typeCheckBlock block2 env
        assertTypesEqual env exprType AST.Bool
        assertTypesEqual env blockType1 blockType2
        return $ Variable blockType1 False
