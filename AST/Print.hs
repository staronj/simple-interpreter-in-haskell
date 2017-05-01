module AST.Print (prettyPrint) where

import Data.Tree (Tree(..), drawTree)
import FormatString
import AST


-- Print AST to Tree
prettyPrint :: Program -> String
prettyPrint = drawTree . programToTree

programToTree :: Program -> Tree String
programToTree (Program funDecls) =
  Node "program" nodes where
    nodes = map funDeclToTree funDecls

funDeclToTree :: FunctionDeclaration -> Tree String
funDeclToTree function =
  Node label [parametersNode, returnTypeNode, blockToTree $ body function] where
    label = format "declaration of function '%0'" [name function]
    parametersNode = Node "parameters: " $ map parameterToTree $ parameters function
    returnTypeNode = Node label' [] where
      label' = format "return type '%0'" [show $ resultType function]

parameterToTree :: FunctionParameter -> Tree String
parameterToTree parameter =
  Node "parameter" [patternNode, typeNode] where
    patternNode = patternToTree (pattern parameter)
    typeNode = Node label [] where
      label = format "type '%0'" [show $ valueType parameter]

blockToTree :: Block -> Tree String
blockToTree (Block stmts expr) = Node "block" $ (map stmtToTree stmts) ++ [exprToTree expr]

stmtToTree :: Stmt -> Tree String
stmtToTree stmt = case stmt of
  FunDeclStmt funDecl                     -> funDeclToTree funDecl
  If expr block                           -> Node "if" [exprToTree expr, blockToTree block]
  Stmt expr                               -> Node "Stmt" [exprToTree expr]
  StrictStmt expr                         -> Node "StrictStmt" [exprToTree expr]
  Loop block                              -> Node "loop" [blockToTree block]
  While expr block                        -> Node "while" [exprToTree expr, blockToTree block]
  IterableForLoop ident expr block        -> Node "for in iterable" [Node (format "variable: '%0'" [ident]) [], exprToTree expr, blockToTree block]
  RangeForLoop ident expr1 expr2 block    -> Node "for in range" [Node (format "variable: '%0'" [ident]) [], exprToTree expr1, exprToTree expr2, blockToTree block]
  Break                                   -> Node "break" []
  Continue                                -> Node "continue" []
  LetStmt pattern Nothing expr            -> Node "let binding" [patternToTree pattern, exprToTree expr]
  LetStmt pattern (Just valueType) expr   -> Node "let binding" [patternToTree pattern, Node (format "type '%0'" [show valueType]) [], exprToTree expr]

exprToTree :: Expr -> Tree String
exprToTree expr = case expr of
  BinaryOperator  expr1 expr2 kind    -> Node "Binary operator"   [Node (show kind) [], exprToTree expr1, exprToTree expr2]
  UnaryOperator   expr kind           -> Node "Unary operator"    [Node (show kind) [], exprToTree expr]
  Identifier      ident               -> Node "Identifier"        [Node ident []]
  LiteralExpr     literal             -> Node "Literal expr"      [Node (show literal) []]
  FunctionCall    ident exprs         -> Node "Function call"     $ Node ident [] : map exprToTree exprs
  TupleLookup     expr integer        -> Node "Tuple lookup"      [exprToTree expr, Node (show integer) []]
  ArrayElements   exprs               -> Node "Array elements"    $ map exprToTree exprs
  ArrayRepeat     expr integer        -> Node "Array repeat"      [exprToTree expr, Node (show integer) []]
  ArrayRange      integer1 integer2   -> Node "Array range"       [Node (show integer1) [], Node (show integer2) []]
  TupleConstruct  exprs               -> Node "Tuple construct"   $ map exprToTree exprs
  BlockExpr       block               -> Node "Block expression"  [blockToTree block]
  IfElse          expr block1 block2  -> Node "If else"           [exprToTree expr, blockToTree block1, blockToTree block2]


patternToTree :: Pattern -> Tree String
patternToTree pattern = case pattern of
  PatternVariable ident            -> Node ident []
  PatternMutableVariable ident     -> Node ("mut " ++ ident) []
  PatternIgnore                    -> Node "ignore" []
  PatternTuple ps                  -> Node "tuple pattern" $ map patternToTree ps
