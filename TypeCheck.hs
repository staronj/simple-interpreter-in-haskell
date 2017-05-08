-- Jakub Staroń, 2017

module TypeCheck (typeCheck) where

import Data.List (foldl', intercalate)
import Control.Monad
import Control.Monad.Except (throwError)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified AST
import FormatString (format)

-- Information about variable needed
-- for type check.
data Variable =
  Variable
  { varType :: AST.Type
  , mutable :: Bool }

-- Informations about function needed
-- for type check.
data Function =
  Function
  { parameters :: [AST.Type]
  , resultType :: AST.Type}

type VEnv = Map.Map AST.Ident Variable
type FEnv = Map.Map AST.Ident Function

-- Sequence of pairs (kind, identifier) guiding to
-- AST node. Used for error messages.
newtype Path = Path [(String, String)]

instance Show Path where
  show (Path path) = intercalate " @ " elems where
    elems = map (\(a, b) -> format "%0: %1" [a, b]) path

data Env =
  Env
  { functions :: FEnv
  , variables :: VEnv
  , path :: Path
  , inLoop :: Bool }

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate = either Just (const Nothing) . foldM insert Set.empty where
  insert :: Ord a => Set.Set a -> a -> Either a (Set.Set a)
  insert s x = if Set.member x s then Left x else Right $ Set.insert x s

-- Messages
functionNotFoundMessage :: AST.Ident -> Env -> String
functionNotFoundMessage name env = format "cannot find function `%0` in scope\n%1" [name, show $ path env]

variableNotFoundMessage :: AST.Ident -> Env -> String
variableNotFoundMessage name env = format "cannot find value `%0` in scope\n%1" [name, show $ path env]

tupleLengthNotMatchMessage :: (Num a, Show a) => a -> a -> Env -> String
tupleLengthNotMatchMessage first second env = format "could not match tuple of %0 elements with tuple of %1 elements in scope\n%2" [show first, show second, show $ path env]

couldNotMatchTypeWithTupleMessage :: AST.Type -> Env -> String
couldNotMatchTypeWithTupleMessage valueType env = format "could not match '%0' with tuple in scope\n%1" [show valueType, show $ path env]

couldNotMatchTypesMessage :: AST.Type -> AST.Type -> Env -> String
couldNotMatchTypesMessage type1 type2 env = format "could not match type '%0' with type '%1'\nat %2" [show type1, show type2, show $ path env]

expectedMutableVariableMessage :: Env -> String
expectedMutableVariableMessage env = format "expected mutable variable\nat%0" [show $ path env]

expectedArrayMessage :: AST.Type -> Env -> String
expectedArrayMessage valueType env = format "expected array, not a value of type '%0'\nat%1" [show valueType, show $ path env]

expectedTupleMessage :: AST.Type -> Env -> String
expectedTupleMessage valueType env = format "expected tuple, not a value of type '%0'\nat%1" [show valueType, show $ path env]

invalidNumberOfArgumentsMessage :: (Num a, Show a) => AST.Ident -> a -> a -> Env -> String
invalidNumberOfArgumentsMessage name expected got env = format "function '%0': expected %1 arguments, got %2\at%3" [name, show expected, show got, show $ path env]

mainFunctionMustTakeNoArgumentsMessage :: String
mainFunctionMustTakeNoArgumentsMessage = "'main' function must take no arguments"

mainFunctionMustReturnUnitMessage :: String
mainFunctionMustReturnUnitMessage = "'main' function must return '()' type"

duplicatedFunctionIdentifierInScopeMessage :: AST.Ident -> Env -> String
duplicatedFunctionIdentifierInScopeMessage name env =  format "duplicated definitions of function '%0' at\n%1" [name, show $ path env]

duplicatedBindingNameInPatternMessage :: AST.Ident -> Env -> String
duplicatedBindingNameInPatternMessage name env =  format "duplicated name '%0' in pattern at\n%1" [name, show $ path env]

outOfBoundsTupleLookupMessage :: (Num a, Show a) => AST.Type -> a -> Env -> String
outOfBoundsTupleLookupMessage valueType n env = format "attempted out-of-bounds tuple index `%0` on type `%1` at\n%2" [show n, show valueType, show $ path env]

arrayFromInvalidRangeMessage :: (Num a, Show a) => a -> a -> Env -> String
arrayFromInvalidRangeMessage begin end env = format "trying to create array from invalid range `(%0, %1)`" [show begin, show end, show $ path env]

controlFlowStmtOutsideOfLoopMessage :: Env -> String
controlFlowStmtOutsideOfLoopMessage env = format "usage of control flow instruction oustide of loop at \n%0" [show $ path env]

-- Messages End

findFunction :: AST.Ident -> Env -> Either String Function
findFunction name env = maybeToEither message $ Map.lookup name $ functions env where
  message = functionNotFoundMessage name env

findVariable :: AST.Ident -> Env -> Either String Variable
findVariable name env = maybeToEither message $ Map.lookup name $ variables env where
  message = variableNotFoundMessage name env

insertFunction :: AST.Ident -> Function -> Env -> Env
insertFunction name function env = env' where
  env' = env { functions = functions' }
  functions' = Map.insert name function $ functions env

insertVariable :: AST.Ident -> Variable -> Env -> Env
insertVariable name variable env = env' where
  env' = env { variables = variables' }
  variables' = Map.insert name variable $ variables env

markInLoop :: Env -> Env
markInLoop env = env { inLoop = True }

insertPattern :: AST.Pattern -> AST.Type -> Env -> Either String Env
insertPattern pattern valueType env = do
  let duplicateName = findDuplicate $ namesInPattern pattern
  maybe (return ()) (throwError . flip duplicatedBindingNameInPatternMessage env) duplicateName
  insertPattern' pattern valueType env
  where
  namesInPattern :: AST.Pattern -> [AST.Ident]
  namesInPattern pattern = case pattern of
    AST.PatternVariable name            -> [name]
    AST.PatternMutableVariable name     -> [name]
    AST.PatternIgnore                   -> []
    AST.PatternTuple pattern'           -> concat $ map namesInPattern pattern'
  insertPattern' :: AST.Pattern -> AST.Type -> Env -> Either String Env
  insertPattern' pattern valueType env = case pattern of
    AST.PatternVariable name            -> return $ insertVariable name (Variable valueType False) env
    AST.PatternMutableVariable name     -> return $ insertVariable name (Variable valueType True) env
    AST.PatternIgnore                   -> return env
    AST.PatternTuple pattern'           -> case valueType of
      AST.Tuple types ->
        let patternLength = length pattern' in
        let typesLength = length types in
        do
          unless (patternLength == typesLength) $ throwError $ tupleLengthNotMatchMessage typesLength patternLength env
          let pairs = zipWith insertPattern' pattern' types
          foldl' (>=>) return pairs env
      _ -> throwError $ couldNotMatchTypeWithTupleMessage valueType env

pushSubpath :: String -> String -> Env -> Env
pushSubpath nodeType name env = env { path = Path $ (nodeType, name) : oldPath} where
  Path oldPath = path env

setSubpath :: String -> String -> Env -> Env
setSubpath nodeType name env = env { path = Path $ (nodeType, name) : oldPath} where
  Path (_ : oldPath) = path env

-- Assertions begin

assertTypesEqual :: AST.Type -> AST.Type -> Env -> Either String ()
assertTypesEqual type1 type2 =
  unless (type1 == type2) . throwError . couldNotMatchTypesMessage type1 type2

assertMutable :: Variable -> Env -> Either String ()
assertMutable variable = unless (mutable variable) . throwError . expectedMutableVariableMessage

assertIsArray :: AST.Type -> Env -> Either String ()
assertIsArray valueType env = case valueType of
    AST.Array {}  -> return ()
    _             -> throwError $ expectedArrayMessage valueType env

assertIsTuple :: AST.Type -> Env -> Either String ()
assertIsTuple valueType env = case valueType of
    AST.Tuple {}  -> return ()
    _             -> throwError $ expectedTupleMessage valueType env

assertFunctionArguments :: Function -> AST.Ident -> [AST.Type] -> Env -> Either String ()
assertFunctionArguments function ident types env =
    let typesLength = length types in
    let parametersLength = length $ parameters function in
    do
      unless (typesLength == parametersLength) $ throwError $ invalidNumberOfArgumentsMessage ident parametersLength typesLength env
      let typesEqual = zipWith assertTypesEqual types $ parameters function
      mapM_ ($ env) typesEqual

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
        maybe (return ()) (throwError . flip duplicatedFunctionIdentifierInScopeMessage env) duplicateName
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
        let bindingsAsTuple = AST.PatternTuple $ map AST.pattern $ AST.parameters fun
        let parametersAsTuple = AST.Tuple $ map AST.valueType $ AST.parameters fun
        env' <- insertPattern bindingsAsTuple parametersAsTuple env
        blockType <- typeCheckBlock (AST.body fun) env'
        let resultType = AST.resultType fun
        assertTypesEqual blockType resultType env

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
    AST.If condExpr block                       -> do
        condType <- varType <$> typeCheckExpr condExpr env
        assertTypesEqual condType AST.Bool env
        blockType <- typeCheckBlock block env
        assertTypesEqual blockType AST.unit env
        return env
    AST.Stmt expr'                              -> typeCheckExpr expr' env >> return env
    AST.StrictStmt expr'                        -> do
        exprType <- varType <$> typeCheckExpr expr' env
        assertTypesEqual exprType AST.unit env
        return env
    AST.Loop block                              -> do
        blockType <- typeCheckBlock block $ markInLoop env
        assertTypesEqual blockType AST.unit env
        return env
    AST.While condExpr block                     -> do
        condType <- varType <$> typeCheckExpr condExpr env
        blockType <- typeCheckBlock block $ markInLoop env
        assertTypesEqual condType AST.Bool env
        assertTypesEqual blockType AST.unit env
        return env
    AST.IterableForLoop name iterExpr block        -> do
        iterType <- varType <$> typeCheckExpr iterExpr env
        assertIsArray iterType env
        let AST.Array valueType _ = iterType
        let env' = insertVariable name Variable { varType = valueType, mutable = False } env
        blockType <- typeCheckBlock block $ markInLoop env'
        assertTypesEqual blockType AST.unit env
        return env
    AST.RangeForLoop name beginExpr endExpr block -> do
        beginType <- varType <$> typeCheckExpr beginExpr env
        endType <- varType <$> typeCheckExpr endExpr env
        let env' = insertVariable name Variable { varType = AST.I32, mutable = False } env
        blockType <- typeCheckBlock block $ markInLoop env'
        assertTypesEqual beginType AST.I32 env
        assertTypesEqual endType AST.I32 env
        assertTypesEqual blockType AST.unit env
        return env
    AST.Break                                   -> do
        assertInLoop env
        return env
    AST.Continue                                -> do
        assertInLoop env
        return env
    AST.LetStmt pattern valueType initExpr      -> do
        initType <- varType <$> typeCheckExpr initExpr env
        maybe (return ()) (flip (assertTypesEqual initType) env) valueType
        insertPattern pattern initType env


typeCheckExpr :: AST.Expr -> Env -> Either String Variable
typeCheckExpr expr env = case expr of
    AST.BinaryOperator lhs rhs kind -> do
        lhsVar <- typeCheckExpr lhs env
        rhsVar <- typeCheckExpr rhs env
        let lhsType = varType lhsVar
        let rhsType = varType rhsVar
        case kind of
            _ | kind `elem` [AST.Or, AST.And] -> do
                assertTypesEqual lhsType AST.Bool env
                assertTypesEqual rhsType AST.Bool env
                return $ Variable AST.Bool False
            AST.Less -> do
                assertTypesEqual lhsType AST.I32 env
                assertTypesEqual rhsType AST.I32 env
                return $ Variable AST.Bool False
            _ | kind `elem` [AST.Add, AST.Subtract, AST.Multiply, AST.Divide, AST.Modulo] -> do
                assertTypesEqual lhsType AST.I32 env
                assertTypesEqual rhsType AST.I32 env
                return $ Variable AST.I32 False
    AST.UnaryOperator expr' kind -> do
        exprVar <- typeCheckExpr expr' env
        let exprType = varType exprVar
        case kind of
            AST.Negate -> do
                assertTypesEqual exprType AST.I32 env
                return $ Variable AST.I32 False
            AST.Not -> do
                assertTypesEqual exprType AST.Bool env
                return $ Variable AST.Bool False

    AST.Identifier name -> findVariable name env
    AST.Equal lhs rhs -> do
      lhsType <- varType <$> typeCheckExpr lhs env
      rhsType <- varType <$> typeCheckExpr rhs env
      assertTypesEqual rhsType lhsType env
      return $ Variable AST.Bool False
    AST.NotEqual lhs rhs -> do
        lhsType <- varType <$> typeCheckExpr lhs env
        rhsType <- varType <$> typeCheckExpr rhs env
        assertTypesEqual rhsType lhsType env
        return $ Variable AST.Bool False
    AST.Assign lhs rhs -> do
        lhsVar <- typeCheckExpr lhs env
        rhsVar <- typeCheckExpr rhs env
        let lhsType = varType lhsVar
        let rhsType = varType rhsVar
        assertMutable lhsVar env
        assertTypesEqual rhsType lhsType env
        return $ Variable AST.unit False
    AST.ArrayLookup arrayExpr indexExpr -> do
        arrayVar <- typeCheckExpr arrayExpr env
        indexVar <- typeCheckExpr indexExpr env
        let arrayType = varType arrayVar
        let indexType = varType indexVar
        assertIsArray arrayType env
        assertTypesEqual indexType AST.I32 env
        let AST.Array valueType _ = arrayType
        return $ arrayVar { varType = valueType }
    AST.Dereference expr' -> error "Not implemented."
    AST.Borrow expr' -> error "Not implemented."
    AST.MutableBorrow expr' -> error "Not implemented."
    AST.LiteralExpr literal ->
        case literal of
            AST.LiteralI32 _    -> return $ Variable AST.I32 False
            AST.LiteralBool _   -> return $ Variable AST.Bool False
    AST.FunctionCall name exprs -> do
        variables <- mapM (`typeCheckExpr` env) exprs
        let types = map varType variables
        function <- findFunction name env
        assertFunctionArguments function name types env
        return $ Variable (resultType function) False
    AST.TupleLookup tupleExpr index -> do
      tupleVar <- typeCheckExpr tupleExpr env
      let tupleType = varType tupleVar
      assertIsTuple tupleType env
      let AST.Tuple types = tupleType
      unless (length types > fromIntegral index) $ throwError $ outOfBoundsTupleLookupMessage tupleType index env
      return $ tupleVar { varType = types !! fromIntegral index }
    AST.ArrayElements   exprs               -> do
        exprs <- mapM (`typeCheckExpr` env) exprs
        exprs <- return $ map varType exprs
        when (null exprs) $ error "Internal interpreter error: empty exprs list in AST.ArrayElements."
        let commonType = head exprs
        mapM_ (flip (assertTypesEqual commonType) env) exprs
        return $ Variable (AST.Array commonType $ fromIntegral $ length exprs) False
    AST.ArrayRepeat initExpr count -> do
        initType <- varType <$> typeCheckExpr initExpr env
        return $ Variable (AST.Array initType count) False
    AST.ArrayRange begin end -> do
        unless (begin < end) $ throwError $ arrayFromInvalidRangeMessage begin end env
        return $ Variable (AST.Array AST.I32 (end - begin)) False
    AST.TupleConstruct  exprs -> do
        variables <- mapM (`typeCheckExpr` env) exprs
        let types = map varType variables
        return $ Variable (AST.Tuple types) False
    AST.BlockExpr block -> do
        blockType <- typeCheckBlock block env
        return $ Variable blockType False
    AST.IfElse condExpr trueBlock falseBlock  -> do
        condType <- varType <$> typeCheckExpr condExpr env
        trueBlockType <- typeCheckBlock trueBlock env
        falseBlockType <- typeCheckBlock falseBlock env
        assertTypesEqual condType AST.Bool env
        assertTypesEqual trueBlockType falseBlockType env
        return $ Variable trueBlockType False
