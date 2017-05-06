-- Jakub Staroń, 2017

module AST.Print (prettyPrint) where

import Data.Tree (Tree(..), drawTree)
import FormatString (format)
import AST


-- Print AST to Tree
prettyPrint :: Program -> String
prettyPrint = drawTree . programToTree

-- Shortcut for Node sth []
leaf :: String -> Tree String
leaf = flip Node []

programToTree :: Program -> Tree String
programToTree = Node "program" . map funDeclToTree . AST.functions

funDeclToTree :: FunctionDeclaration -> Tree String
funDeclToTree function =
  Node label [parametersNode, returnTypeNode, blockToTree $ body function] where
    label = format "declaration of function '%0'" [name function]
    parametersNode = Node "parameters: " $ map parameterToTree $ parameters function
    returnTypeNode = leaf $ format "return type '%0'" [show $ resultType function]

parameterToTree :: FunctionParameter -> Tree String
parameterToTree parameter =
  Node "parameter" [patternNode, typeNode] where
    patternNode = patternToTree $ pattern parameter
    typeNode = leaf $ format "type '%0'" [show $ valueType parameter]

blockToTree :: Block -> Tree String
blockToTree (Block stmts expr) = Node "block" $ map stmtToTree stmts ++ [exprToTree expr]

stmtToTree :: Stmt -> Tree String
stmtToTree stmt = case stmt of
  FunDeclStmt funDecl                     -> funDeclToTree funDecl
  If condExpr block                       -> Node "if" [exprToTree condExpr, blockToTree block]
  Stmt expr                               -> Node "stmt" [exprToTree expr]
  StrictStmt expr                         -> Node "strict stmt" [exprToTree expr]
  Loop block                              -> Node "loop" [blockToTree block]
  While condExpr block                    -> Node "while" [exprToTree condExpr, blockToTree block]
  IterableForLoop name expr block         -> Node "for in iterable" [leaf $ format "variable: '%0'" [name], exprToTree expr, blockToTree block]
  RangeForLoop name begin end block       -> Node "for in range" [leaf $ format "variable: '%0'" [name], exprToTree begin, exprToTree end, blockToTree block]
  Break                                   -> leaf "break"
  Continue                                -> leaf "continue"
  LetStmt pattern Nothing expr            -> Node "let binding" [patternToTree pattern, exprToTree expr]
  LetStmt pattern (Just valueType) expr   -> Node "let binding" [patternToTree pattern, leaf $ format "type '%0'" [show valueType], exprToTree expr]

exprToTree :: Expr -> Tree String
exprToTree expr = case expr of
  BinaryOperator  lhs rhs kind        -> Node "binary operator"   [leaf $ show kind, exprToTree lhs, exprToTree rhs]
  UnaryOperator   expr kind           -> Node "unary operator"    [leaf $ show kind, exprToTree expr]
  Equal           lhs rhs             -> Node "equal"             [exprToTree lhs, exprToTree rhs]
  NotEqual        lhs rhs             -> Node "not equal"         [exprToTree lhs, exprToTree rhs]
  Assign          lhs rhs             -> Node "assign"            [exprToTree lhs, exprToTree rhs]
  ArrayLookup     array index         -> Node "array lookup"      [exprToTree array, exprToTree index]
  Identifier      name                -> Node "identifier"        [leaf name]
  LiteralExpr     literal             -> Node "literal expr"      [leaf $ show literal]
  FunctionCall    name exprs          -> Node "function call"     $ Node name [] : map exprToTree exprs
  TupleLookup     expr integer        -> Node "tuple lookup"      [exprToTree expr, leaf $ show integer]
  ArrayElements   exprs               -> Node "array elements"    $ map exprToTree exprs
  ArrayRepeat     expr count          -> Node "array repeat"      [exprToTree expr, leaf $ show count]
  ArrayRange      begin end           -> Node "array range"       [leaf $ show begin, leaf $ show end]
  TupleConstruct  exprs               -> Node "tuple construct"   $ map exprToTree exprs
  BlockExpr       block               -> Node "block expression"  [blockToTree block]
  IfElse          condExpr trueBlock falseBlock -> Node "if else" [exprToTree condExpr, blockToTree trueBlock, blockToTree falseBlock]


patternToTree :: Pattern -> Tree String
patternToTree pattern = case pattern of
  PatternVariable ident            -> leaf ident
  PatternMutableVariable ident     -> leaf $ format "mut %0" [ident]
  PatternIgnore                    -> leaf "ignore"
  PatternTuple ps                  -> Node "tuple pattern" $ map patternToTree ps
