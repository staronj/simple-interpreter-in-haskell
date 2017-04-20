-- Jakub Staroń, 2017

{-# LANGUAGE GADTs #-}

module AST where

import Data.List
import Data.Tree
import qualified AbsGrammar as Abs
import qualified ParGrammar as Par
import FormatString
import ErrM

-- Identifier type
type Ident = String

-- Rust data type
data Type =
    I32
  | Bool
  | Tuple [Type]
  | Reference Type
  | MutableReference Type
  | Array Type Integer
  deriving (Eq)

instance Show Type where
    show I32 =                      "i32"
    show Bool =                     "bool"
    show (Tuple []) =               "()"
    show (Tuple [t]) =              format "(%0,)" [show t]
    show (Tuple list) =             format "(%0)" [intercalate "," $ map show list]
    show (Reference t) =            format "&%0" [show t]
    show (MutableReference t) =     format "&mut %0" [show t]
    show (Array t n) =              format "[%0; %1]" [show t, show n]

-- Unit (void) type
unit :: Type
unit = Tuple []

-- One and only one value of unit type.
unitExpr :: Expr
unitExpr = TupleConstruct []

-- Let Pattern
data Pattern =
    PatternVariable Ident
  | PatternMutableVariable Ident
  | PatternIgnore
  | PatternTuple [Pattern]
  deriving (Eq)

-- Literal
data Literal =
    LiteralI32 Integer
  | LiteralBool Bool
  deriving (Eq)

instance Show Literal where
    show (LiteralI32 n) =   format "literal %0" [show n]
    show (LiteralBool b) =  format "literal %0" [show b]

data BinaryOperatorKind =
    Or
  | And
  | Equal
  | NotEqual
  | Less
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Assign
  | ArrayLookup
  deriving (Eq, Show)

data UnaryOperatorKind =
    Negate
  | Dereference
  | Not
  | Borrow
  | MutableBorrow
  deriving (Eq, Show)

-- Expr
data Expr =
    BinaryOperator  Expr BinaryOperatorKind Expr
  | UnaryOperator   UnaryOperatorKind Expr
  | Identifier      Ident
  | LiteralExpr     Literal

  | FunctionCall    Ident [Expr]
  | TupleLookup     Expr Integer

  | ArrayElements   [Expr]
  | ArrayRepeat     Expr Integer
  | ArrayRange      Integer Integer
  | TupleConstruct  [Expr]
  | BlockExpr       Block

  | IfElse          Expr Block Block
  deriving (Eq)

-- Statement
data Stmt =
    FunDeclStmt FunctionDeclaration
  | If Expr Block
  | Stmt Expr                              -- Expects any type
  | StrictStmt Expr                        -- Expects unit type
  | Loop Block
  | While Expr Block
  | IterableForLoop Ident Expr Block
  | RangeForLoop Ident Expr Expr Block
  | Break
  | Continue
  | LetStmt Pattern (Maybe Type) Expr
  deriving (Eq)

-- Block
data Block = Block [Stmt] Expr
    deriving (Eq)

-- Function Parameter
data FunctionParameter =
    FunctionParameter
    { pattern :: Pattern
    , valueType :: Type}
    deriving (Eq)

-- Function declaration
data FunctionDeclaration =
  FunctionDeclaration
  { name :: Ident
  , parameters :: [FunctionParameter]
  , resultType :: Type
  , body :: Block }
    deriving (Eq)

-- Program
data Program =
  Program
  { functions :: [FunctionDeclaration] }

buildAST :: String -> Either String Program
buildAST str = let tokens = Par.myLexer str in
    case Par.pProgram tokens of
        Ok tree -> return $ buildProgram tree
        Bad err -> Left err

buildProgram :: Abs.Program -> Program
buildProgram program = case program of
    Abs.Program list -> Program $ map buildFunDecl list

buildFunDecl :: Abs.FunDecl -> FunctionDeclaration
buildFunDecl funDecl = case funDecl of
    Abs.FunDecl (Abs.Ident ident) parameterList block                   -> FunctionDeclaration ident (buildParameters parameterList) unit (buildBlock block)
    Abs.FunDeclType (Abs.Ident ident) parameterList returnType block    -> FunctionDeclaration ident (buildParameters parameterList) (buildType returnType) (buildBlock block)
    where
        buildParameter :: Abs.Parameter -> FunctionParameter
        buildParameter (Abs.Parameter pattern valueType) = FunctionParameter (buildPattern pattern) (buildType valueType)
        buildParameters :: Abs.SepParameterList -> [FunctionParameter]
        buildParameters parameters = case parameters of
                Abs.SepPNil         -> []
                Abs.SepPOne p       -> [buildParameter p]
                Abs.SepPMore p ps   -> (buildParameter p) : (buildParameters ps)
                Abs.SepPHead p      -> [buildParameter p]
                Abs.SepPTail ps p   -> (buildParameters ps) ++ [buildParameter p]

buildBlock :: Abs.Block -> Block
buildBlock block = case block of
    Abs.Block stmts               -> Block (map buildStmt stmts) unitExpr
    Abs.BlockWithValue stmts expr -> Block (map buildStmt stmts) (buildExpr expr)

buildExpr :: Abs.Expr -> Expr
buildExpr expr = case expr of
    Abs.Assign expr1 expr2                       -> BinaryOperator (buildExpr expr1) Assign (buildExpr expr2)
    Abs.Or expr1 expr2                           -> BinaryOperator (buildExpr expr1) Or (buildExpr expr2)
    Abs.And expr1 expr2                          -> BinaryOperator (buildExpr expr1) And (buildExpr expr2)
    Abs.Equal expr1 expr2                        -> BinaryOperator (buildExpr expr1) Equal (buildExpr expr2)
    Abs.NotEqual expr1 expr2                     -> BinaryOperator (buildExpr expr1) NotEqual (buildExpr expr2)
    Abs.Less expr1 expr2                         -> BinaryOperator (buildExpr expr1) Less (buildExpr expr2)
    Abs.Add expr1 expr2                          -> BinaryOperator (buildExpr expr1) Add (buildExpr expr2)
    Abs.Subtract expr1 expr2                     -> BinaryOperator (buildExpr expr1) Subtract (buildExpr expr2)
    Abs.Multiply expr1 expr2                     -> BinaryOperator (buildExpr expr1) Multiply (buildExpr expr2)
    Abs.Divide expr1 expr2                       -> BinaryOperator (buildExpr expr1) Divide (buildExpr expr2)
    Abs.Modulo expr1 expr2                       -> BinaryOperator (buildExpr expr1) Modulo (buildExpr expr2)
    Abs.Negate expr                              -> UnaryOperator Negate (buildExpr expr)
    Abs.Dereference expr                         -> UnaryOperator Dereference (buildExpr expr)
    Abs.Not expr                                 -> UnaryOperator Not (buildExpr expr)
    Abs.Borrow expr                              -> UnaryOperator Borrow (buildExpr expr)
    Abs.MutableBorrow expr                       -> UnaryOperator MutableBorrow (buildExpr expr)
    Abs.LiteralExpr literal                      -> LiteralExpr (buildLiteral literal)
    Abs.ExprIdent (Abs.Ident ident)              -> Identifier ident
    Abs.FunctionCall (Abs.Ident ident) sepExprList       -> FunctionCall ident $ map buildExpr (buildSepExprList sepExprList)
    Abs.ArrayLookup expr1 expr2                  -> BinaryOperator (buildExpr expr1) ArrayLookup (buildExpr expr2)
    Abs.TupleLookup expr integer                 -> TupleLookup (buildExpr expr) integer
    Abs.IfElseExpr (Abs.IfElse expr1 block1 block2)  -> IfElse (buildExpr expr1) (buildBlock block1) (buildBlock block2)
    Abs.BlockExpr block                              -> BlockExpr (buildBlock block)
    Abs.ArrayElements markExprList               -> ArrayElements $ map buildExpr (buildMarkExprList markExprList)
    Abs.ArrayRepeat expr integer                 -> ArrayRepeat (buildExpr expr) integer
    Abs.ArrayRange integer1 integer2             -> ArrayRange  integer1 integer2
    Abs.TupleConstruct markExprList              -> TupleConstruct $ map buildExpr (buildMarkExprList markExprList)
    where
        buildSepExprList :: Abs.SepExprList -> [Abs.Expr]
        buildSepExprList exprs = case exprs of
            Abs.SepExprNil        -> []
            Abs.SepExprOne e      -> [e]
            Abs.SepExprMore e es  -> e : (buildSepExprList es)
            Abs.SepExprHead e     -> [e]
            Abs.SepExprTail es e  -> (buildSepExprList es) ++ [e]
        buildMarkExprList :: Abs.MarkExprList -> [Abs.Expr]
        buildMarkExprList exprs = case exprs of
            Abs.MarkExprNil        -> []
            Abs.MarkExprOne e      -> [e]
            Abs.MarkExprMore e es  -> e : (buildMarkExprList es)
            Abs.MarkExprHead e     -> [e]
            Abs.MarkExprTail es e  -> (buildMarkExprList es) ++ [e]

buildStmt :: Abs.Stmt -> Stmt
buildStmt stmt = case stmt of
    Abs.FunDeclStmt funDecl                              -> FunDeclStmt (buildFunDecl funDecl)
    Abs.Stmt expr                                        -> Stmt (buildExpr expr)
    Abs.Break                                            -> Break
    Abs.Continue                                         -> Continue
    Abs.If (Abs.IfStmt expr block)                       -> If (buildExpr expr) (buildBlock block)
    Abs.IfElseStmt (Abs.IfElse expr block1 block2)       -> StrictStmt $ IfElse (buildExpr expr) (buildBlock block1) (buildBlock block2)
    Abs.Loop block                                       -> Loop (buildBlock block)
    Abs.While expr block                                 -> While (buildExpr expr) (buildBlock block)
    Abs.IterableForLoop (Abs.Ident ident) expr block     -> IterableForLoop ident (buildExpr expr) (buildBlock block)
    Abs.RangeForLoop (Abs.Ident ident) expr1 expr2 block -> RangeForLoop ident (buildExpr expr1) (buildExpr expr2) (buildBlock block)
    Abs.LetStmtStrict pattern valueType expr             -> LetStmt (buildPattern pattern) (Just (buildType valueType)) (buildExpr expr)
    Abs.LetStmt pattern expr                             -> LetStmt (buildPattern pattern) Nothing (buildExpr expr)
    Abs.BlockStmt block                                  -> StrictStmt $ BlockExpr $ buildBlock block


buildLiteral :: Abs.Literal -> Literal
buildLiteral literal = case literal of
    Abs.LiteralI32 n -> LiteralI32 n
    Abs.LiteralBool (Abs.Boolean b) -> LiteralBool (b == "true")


buildPattern :: Abs.Pattern -> Pattern
buildPattern pattern = case pattern of
    Abs.PatternVariable (Abs.Ident ident)         -> PatternVariable ident
    Abs.PatternMutableVariable (Abs.Ident ident)  -> PatternMutableVariable ident
    Abs.PatternIgnore                             -> PatternIgnore
    Abs.PatternTuple patterns                     -> PatternTuple $ map buildPattern (buildList patterns)
    where
        buildList :: Abs.MarkPatternList -> [Abs.Pattern]
        buildList patterns = case patterns of
            Abs.MarkPatternNil        -> []
            Abs.MarkPatternOne p      -> [p]
            Abs.MarkPatternMore p ps  -> p : (buildList ps)
            Abs.MarkPatternHead p     -> [p]
            Abs.MarkPatternTail ps p  -> (buildList ps) ++ [p]


buildType :: Abs.Type -> Type
buildType type' = case type' of
    Abs.Bool            -> Bool
    Abs.I32             -> I32
    Abs.Reference type''      -> Reference (buildType type'')
    Abs.MutableReference type''   -> MutableReference (buildType type'')
    Abs.Array type'' n  -> Array (buildType type'') n
    Abs.Tuple types     -> Tuple $ map buildType $ buildList types
    where
        buildList :: Abs.MarkTypeList -> [Abs.Type]
        buildList types = case types of
            Abs.MarkTNil        -> []
            Abs.MarkTOne t      -> [t]
            Abs.MarkTMore t ts  -> t : (buildList ts)
            Abs.MarkTHead t     -> [t]
            Abs.MarkTTail ts t  -> (buildList ts) ++ [t]

-- Print AST to Tree
prettyPrint :: Program -> String
prettyPrint program = drawTree $ programToTree program

programToTree :: Program -> Tree String
programToTree (Program funDecls) = Node "program" nodes where
    nodes = map funDeclToTree funDecls

funDeclToTree :: FunctionDeclaration -> Tree String
funDeclToTree (FunctionDeclaration ident parameters returnType block) =
    Node (format "declaration of function \"%0\"" [ident]) [parametersNode, returnTypeNode, blockToTree block] where
        parametersNode = Node "parameters: " $ map parameterToTree parameters
        returnTypeNode = Node (format "return type \"%0\"" [show returnType]) []

parameterToTree :: FunctionParameter -> Tree String
parameterToTree parameter = Node "parameter" [patternNode, typeNode] where
  patternNode = patternToTree (pattern parameter)
  typeNode = Node (format "type \"%0\"" [show $ valueType parameter]) []

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
    IterableForLoop ident expr block        -> Node "for in iterable" [Node (format "variable: \"%0\"" [ident]) [], exprToTree expr, blockToTree block]
    RangeForLoop ident expr1 expr2 block    -> Node "for in range" [Node (format "variable: \"%0\"" [ident]) [], exprToTree expr1, exprToTree expr2, blockToTree block]
    Break                                   -> Node "break" []
    Continue                                -> Node "continue" []
    LetStmt pattern Nothing expr            -> Node "let binding" [patternToTree pattern, exprToTree expr]
    LetStmt pattern (Just valueType) expr   -> Node "let binding" [patternToTree pattern, Node (format "type \"%0\"" [show valueType]) [], exprToTree expr]

exprToTree :: Expr -> Tree String
exprToTree expr = case expr of
    BinaryOperator  expr1 kind expr2    -> Node "Binary operator"   [Node (show kind) [], exprToTree expr1, exprToTree expr2]
    UnaryOperator   kind expr           -> Node "Unary operator"    [Node (show kind) [], exprToTree expr]
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