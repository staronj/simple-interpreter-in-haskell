-- Jakub Staroń, 2017

{-# LANGUAGE GADTs #-}

module AST.Build (buildAST) where

import AST

import ErrM
import qualified AbsGrammar as Abs
import qualified ParGrammar as Par
import Control.Monad.Except (throwError)


buildAST :: String -> Either String Program
buildAST str = case Par.pProgram $ Par.myLexer str of
  Ok tree -> return $ buildProgram tree
  Bad err -> throwError err


buildProgram :: Abs.Program -> Program
buildProgram program = Program { functions = map buildFunDecl list } where
  Abs.Program list = program


buildFunDecl :: Abs.FunDecl -> FunctionDeclaration
buildFunDecl funDecl = case funDecl of
  Abs.FunDecl (Abs.Ident name) parameterList block                   -> FunctionDeclaration name (buildParameters parameterList) unit (buildBlock block)
  Abs.FunDeclType (Abs.Ident name) parameterList returnType block    -> FunctionDeclaration name (buildParameters parameterList) (buildType returnType) (buildBlock block)
  where
    buildParameter :: Abs.Parameter -> FunctionParameter
    buildParameter (Abs.Parameter pattern valueType) = FunctionParameter (buildPattern pattern) (buildType valueType)
    buildParameters :: Abs.SepParameterList -> [FunctionParameter]
    buildParameters parameters = case parameters of
      Abs.SepPNil         -> []
      Abs.SepPOne p       -> [buildParameter p]
      Abs.SepPMore p ps   -> buildParameter p : buildParameters ps
      Abs.SepPHead p      -> [buildParameter p]
      Abs.SepPTail ps p   -> buildParameters ps ++ [buildParameter p]


buildBlock :: Abs.Block -> Block
buildBlock block = case block of
  -- Temporary (?) workaround for buggy grammar:
  Abs.Block [Abs.IfElseStmt ifElse] -> Block [] (buildExpr $ Abs.IfElseExpr ifElse)
  Abs.Block stmts                   -> Block (map buildStmt stmts) unitExpr
  Abs.BlockWithValue stmts expr     -> Block (map buildStmt stmts) (buildExpr expr)


buildExpr :: Abs.Expr -> Expr
buildExpr expr = case expr of
  Abs.Or lhs rhs                               -> BinaryOperator (buildExpr lhs) (buildExpr rhs) Or
  Abs.And lhs rhs                              -> BinaryOperator (buildExpr lhs) (buildExpr rhs) And
  Abs.Less lhs rhs                             -> BinaryOperator (buildExpr lhs) (buildExpr rhs) Less
  Abs.Add lhs rhs                              -> BinaryOperator (buildExpr lhs) (buildExpr rhs) Add
  Abs.Subtract lhs rhs                         -> BinaryOperator (buildExpr lhs) (buildExpr rhs) Subtract
  Abs.Multiply lhs rhs                         -> BinaryOperator (buildExpr lhs) (buildExpr rhs) Multiply
  Abs.Divide lhs rhs                           -> BinaryOperator (buildExpr lhs) (buildExpr rhs) Divide
  Abs.Modulo lhs rhs                           -> BinaryOperator (buildExpr lhs) (buildExpr rhs) Modulo
  Abs.Negate value                             -> UnaryOperator (buildExpr value) Negate
  Abs.Not value                                -> UnaryOperator (buildExpr value) Not
  Abs.Assign lhs rhs                           -> Assign (buildExpr lhs) (buildExpr rhs)
  Abs.Equal lhs rhs                            -> Equal (buildExpr lhs) (buildExpr rhs)
  Abs.NotEqual lhs rhs                         -> NotEqual (buildExpr lhs) (buildExpr rhs)
  Abs.Dereference value                        -> Dereference (buildExpr value)
  Abs.Borrow value                             -> Borrow (buildExpr value)
  Abs.MutableBorrow value                      -> MutableBorrow (buildExpr value)
  Abs.LiteralExpr literal                      -> LiteralExpr (buildLiteral literal)
  Abs.ExprIdent (Abs.Ident name)               -> Identifier name
  Abs.FunctionCall (Abs.Ident name) sepExprList       -> FunctionCall name $ map buildExpr (buildSepExprList sepExprList)
  Abs.ArrayLookup array index                  -> ArrayLookup (buildExpr array) (buildExpr index)
  Abs.TupleLookup tuple index                  -> TupleLookup (buildExpr tuple) (fromIntegral index)
  Abs.IfElseExpr (Abs.IfElse cond trueBlock falseBlock) -> IfElse (buildExpr cond) (buildBlock trueBlock) (buildBlock falseBlock)
  Abs.BlockExpr block                              -> BlockExpr (buildBlock block)
  Abs.ArrayElements markExprList               -> ArrayElements $ map buildExpr (buildMarkExprList markExprList)
  Abs.ArrayRepeat value count                  -> ArrayRepeat (buildExpr value) (fromIntegral count)
  Abs.ArrayRange begin end                     -> ArrayRange  (fromIntegral begin) (fromIntegral end)
  Abs.TupleConstruct markExprList              -> TupleConstruct $ map buildExpr (buildMarkExprList markExprList)
  where
    buildSepExprList :: Abs.SepExprList -> [Abs.Expr]
    buildSepExprList exprs = case exprs of
      Abs.SepExprNil        -> []
      Abs.SepExprOne e      -> [e]
      Abs.SepExprMore e es  -> e : buildSepExprList es
      Abs.SepExprHead e     -> [e]
      Abs.SepExprTail es e  -> buildSepExprList es ++ [e]
    buildMarkExprList :: Abs.MarkExprList -> [Abs.Expr]
    buildMarkExprList exprs = case exprs of
      Abs.MarkExprNil        -> []
      Abs.MarkExprOne e      -> [e]
      Abs.MarkExprMore e es  -> e : buildMarkExprList es
      Abs.MarkExprHead e     -> [e]
      Abs.MarkExprTail es e  -> buildMarkExprList es ++ [e]


buildStmt :: Abs.Stmt -> Stmt
buildStmt stmt = case stmt of
  Abs.FunDeclStmt funDecl                              -> FunDeclStmt (buildFunDecl funDecl)
  Abs.Stmt expr                                        -> Stmt (buildExpr expr)
  Abs.Break                                            -> Break
  Abs.Continue                                         -> Continue
  Abs.If (Abs.IfStmt cond block)                       -> If (buildExpr cond) (buildBlock block)
  Abs.IfElseStmt (Abs.IfElse cond trueBlock falseBlock) -> StrictStmt $ IfElse (buildExpr cond) (buildBlock trueBlock) (buildBlock falseBlock)
  Abs.Loop block                                       -> Loop (buildBlock block)
  Abs.While cond block                                 -> While (buildExpr cond) (buildBlock block)
  Abs.IterableForLoop (Abs.Ident name) iterable block  -> IterableForLoop name (buildExpr iterable) (buildBlock block)
  Abs.RangeForLoop (Abs.Ident name) begin end block    -> RangeForLoop name (buildExpr begin) (buildExpr end) (buildBlock block)
  Abs.LetStmtStrict pattern valueType expr             -> LetStmt (buildPattern pattern) (Just $ buildType valueType) (buildExpr expr)
  Abs.LetStmt pattern expr                             -> LetStmt (buildPattern pattern) Nothing (buildExpr expr)
  Abs.BlockStmt block                                  -> StrictStmt $ BlockExpr $ buildBlock block


buildLiteral :: Abs.Literal -> Literal
buildLiteral literal = case literal of
  Abs.LiteralI32 n                  -> LiteralI32 $ fromIntegral n
  Abs.LiteralBool (Abs.Boolean b)   -> LiteralBool $ b == "true"


buildPattern :: Abs.Pattern -> Pattern
buildPattern pattern = case pattern of
  Abs.PatternVariable (Abs.Ident name)        -> PatternVariable name
  Abs.PatternMutableVariable (Abs.Ident name) -> PatternMutableVariable name
  Abs.PatternIgnore                           -> PatternIgnore
  Abs.PatternTuple patterns                   -> PatternTuple $ map buildPattern (buildList patterns)
  where
    buildList :: Abs.MarkPatternList -> [Abs.Pattern]
    buildList patterns = case patterns of
      Abs.MarkPatternNil        -> []
      Abs.MarkPatternOne p      -> [p]
      Abs.MarkPatternMore p ps  -> p : buildList ps
      Abs.MarkPatternHead p     -> [p]
      Abs.MarkPatternTail ps p  -> buildList ps ++ [p]


buildType :: Abs.Type -> Type
buildType type' = case type' of
  Abs.Bool                    -> Bool
  Abs.I32                     -> I32
  Abs.Reference type''        -> Reference $ buildType type''
  Abs.MutableReference type'' -> MutableReference $ buildType type''
  Abs.Array type'' n          -> Array (buildType type'') (fromIntegral n)
  Abs.Tuple types             -> Tuple $ map buildType $ buildList types
  where
    buildList :: Abs.MarkTypeList -> [Abs.Type]
    buildList types = case types of
      Abs.MarkTNil        -> []
      Abs.MarkTOne t      -> [t]
      Abs.MarkTMore t ts  -> t : buildList ts
      Abs.MarkTHead t     -> [t]
      Abs.MarkTTail ts t  -> buildList ts ++ [t]
