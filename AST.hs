{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}

module AST where

import qualified AbsGrammar as Abs;
import qualified ParGrammar as Par;
import ErrM;
import Data.List;
import Data.Tree;

-- Identifier type
type Ident = String;

-- Rust data type
data Type =
    I32 |
    Bool |
    Tuple [Type] |
    Reference Type |
    MutReference Type |
    Array Type Integer
    deriving (Eq);

instance Show Type where
    show I32 = "i32"
    show Bool = "bool"
    show (Tuple []) = "()"
    show (Tuple [t]) = "(" ++ (show t) ++  ",)"
    show (Tuple list) = "(" ++ (intercalate "," $ map show list) ++  ")"
    show (Reference t) = "&" ++ (show t)
    show (MutReference t) = "&mut " ++ (show t)
    show (Array t n) = "[" ++ (show t) ++ "; " ++ (show n) ++  "]"
;

unit :: Type;
unit = Tuple [];

unitExpr :: Expr;
unitExpr = TupleConstruct [];

-- Function Parameter
data FunctionParameter = FunctionParameter { ident :: Ident, valueType :: Type};

instance Show FunctionParameter where
    show (FunctionParameter ident valueType) = ident ++ " of type " ++ (show valueType)
;

-- Let Pattern
data LetPattern =
    LetPatternIdent Ident           |
    LetPatternMutableIdent Ident    |
    LetPatternIgnore                |
    LetPatternTuple [LetPattern]
;

instance Show LetPattern where
    show (LetPatternIdent ident) = "ident: " ++ ident
    show (LetPatternMutableIdent ident) = "mutable ident: " ++ ident
    show LetPatternIgnore = "_"
    show (LetPatternTuple []) = "()"
    show (LetPatternTuple [p]) = "(" ++ (show p) ++ ",)"
    show (LetPatternTuple ps) = "(" ++ (intercalate "," (map show ps) ) ++ ",)"
;

-- Literal
data Literal =
    LiteralI32 Integer |
    LiteralBool Bool;

instance Show Literal where
    show (LiteralI32 n) = "Literal " ++ show n
    show (LiteralBool b) = "Literal " ++ show b
;

-- Expr
data Expr =
    Or              Expr Expr        |
    And             Expr Expr        |
    Equal           Expr Expr        |
    NotEqual        Expr Expr        |
    Less            Expr Expr        |
    Add             Expr Expr        |
    Substract       Expr Expr        |
    Multiply        Expr Expr        |
    Divide          Expr Expr        |
    Modulo          Expr Expr        |
    Assign          Expr Expr        |
    Negate          Expr             |
    Dereference     Expr             |
    Not             Expr             |
    Borrow          Expr             |
    MutableBorrow   Expr             |
    Identifier      Ident            |
    LiteralExpr     Literal          |

    FunctionCall    Ident [Expr]     |
    ArrayLookup     Expr Expr        |
    TupleLookup     Expr Integer     |

    ArrayElements   [Expr]           |
    ArrayRepeat     Expr Integer     |
    ArrayRange      Integer Integer  |
    TupleConstruct  [Expr]           |
    BlockExpr       Block            |

    IfElse          Expr Block Block
;

-- Statement
data Stmt =
    FunDeclStmt FunctionDeclaration     |
    If Expr Block                       |
    Stmt Expr                           |   -- Expects any type
    StrictStmt Expr                     |   -- Expects unit type
    Loop Block                          |
    While Expr Block                    |
    IterableForLoop Ident Expr Block    |
    RangeForLoop Ident Expr Expr Block  |
    Break                               |
    Continue                            |
    LetStmt LetPattern (Maybe Type) Expr
;


-- Block
data Block = Block [Stmt] Expr;

-- Function declaration
data FunctionDeclaration = FunctionDeclaration Ident [FunctionParameter] Type Block;

-- Program
data Program = Program [FunctionDeclaration];

buildAST :: String -> Either String Program;
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
        buildParameter (Abs.Parameter (Abs.Ident ident) valueType) = FunctionParameter ident (buildType valueType)
        buildParameters :: Abs.SepParameterList -> [FunctionParameter]
        buildParameters parameters = case parameters of
                Abs.SepPNil         -> []
                Abs.SepPOne p       -> [buildParameter p]
                Abs.SepPMore p ps   -> (buildParameter p) : (buildParameters ps)
                Abs.SepPHead p      -> [buildParameter p]
                Abs.SepPTail ps p   -> (buildParameters ps) ++ [buildParameter p]

buildBlock :: Abs.Block -> Block;
buildBlock block = case block of
    Abs.BlockStmt stmts         -> Block (map buildStmt stmts) unitExpr
    Abs.BlockExpr stmts expr    -> Block (map buildStmt stmts) (buildExpr expr)

buildExpr :: Abs.Expr -> Expr
buildExpr expr = case expr of
    Abs.ExprAssign expr1 expr2 ->                       Assign (buildExpr expr1) (buildExpr expr2)
    Abs.ExprOr expr1 expr2 ->                           Or (buildExpr expr1) (buildExpr expr2)
    Abs.ExprAnd expr1 expr2 ->                          And (buildExpr expr1) (buildExpr expr2)
    Abs.ExprEq expr1 expr2 ->                           Equal (buildExpr expr1) (buildExpr expr2)
    Abs.ExprNotEq expr1 expr2 ->                        NotEqual (buildExpr expr1) (buildExpr expr2)
    Abs.ExprLess expr1 expr2 ->                         Less (buildExpr expr1) (buildExpr expr2)
    Abs.ExprAdd expr1 expr2 ->                          Add (buildExpr expr1) (buildExpr expr2)
    Abs.ExprSub expr1 expr2 ->                          Substract (buildExpr expr1) (buildExpr expr2)
    Abs.ExprMul expr1 expr2 ->                          Multiply (buildExpr expr1) (buildExpr expr2)
    Abs.ExprDiv expr1 expr2 ->                          Divide (buildExpr expr1) (buildExpr expr2)
    Abs.ExprMod expr1 expr2 ->                          Modulo (buildExpr expr1) (buildExpr expr2)
    Abs.ExprNeg expr ->                                 Negate (buildExpr expr)
    Abs.ExprDeref expr ->                               Dereference (buildExpr expr)
    Abs.ExprNot expr ->                                 Not (buildExpr expr)
    Abs.ExprBorrow expr ->                              Borrow (buildExpr expr)
    Abs.ExprMutBorrow expr ->                           MutableBorrow (buildExpr expr)
    Abs.ExprLiteral literal ->                          LiteralExpr (buildLiteral literal)
    Abs.ExprIdent (Abs.Ident ident) ->                  Identifier ident
    Abs.ExprCall (Abs.Ident ident) sepExprList ->       FunctionCall ident $ map buildExpr (buildSepExprList sepExprList)
    Abs.ExprArrayLookup expr1 expr2 ->                  ArrayLookup (buildExpr expr1) (buildExpr expr2)
    Abs.ExprTupleLookup expr integer ->                 TupleLookup (buildExpr expr) integer
    Abs.ExprIfElse (Abs.IfElse expr1 block1 block2) ->  IfElse (buildExpr expr1) (buildBlock block1) (buildBlock block2)
    Abs.ExprBlock block ->                              BlockExpr (buildBlock block)
    Abs.ExprArrayElems markExprList ->                  ArrayElements $ map buildExpr (buildMarkExprList markExprList)
    Abs.ExprArrayRepeat expr integer ->                 ArrayRepeat (buildExpr expr) integer
    Abs.ExprArrayRange integer1 integer2 ->             ArrayRange  integer1 integer2
    Abs.ExprTuple markExprList ->                       TupleConstruct $ map buildExpr (buildMarkExprList markExprList)
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
    Abs.StmtFnDecl funDecl ->                               FunDeclStmt (buildFunDecl funDecl)
    Abs.StmtExpr expr ->                                    Stmt (buildExpr expr)
    Abs.StmtBreak ->                                        Break
    Abs.StmtContinue ->                                     Continue
    Abs.StmtIf (Abs.IfStmt expr block) ->                   If (buildExpr expr) (buildBlock block)
    Abs.StmtIfElse (Abs.IfElse expr block1 block2) ->       StrictStmt $ IfElse (buildExpr expr) (buildBlock block1) (buildBlock block2)
    Abs.StmtLoop block ->                                   Loop (buildBlock block)
    Abs.StmtWhile expr block ->                             While (buildExpr expr) (buildBlock block)
    Abs.StmtForIterable (Abs.Ident ident) expr block ->     IterableForLoop ident (buildExpr expr) (buildBlock block)
    Abs.StmtForRange (Abs.Ident ident) expr1 expr2 block -> RangeForLoop ident (buildExpr expr1) (buildExpr expr2) (buildBlock block)
    Abs.StmtLetType letPattern valueType expr ->            LetStmt (buildLetPattern letPattern) (Just (buildType valueType)) (buildExpr expr)
    Abs.StmtLet letPattern expr ->                          LetStmt (buildLetPattern letPattern) Nothing (buildExpr expr)
;

buildLiteral :: Abs.Literal -> Literal
buildLiteral literal = case literal of
    Abs.LiteralI32 n -> LiteralI32 n
    Abs.LiteralBool (Abs.Boolean b) -> LiteralBool (b == "true")
;

buildLetPattern :: Abs.LetPattern -> LetPattern
buildLetPattern pattern = case pattern of
    Abs.PatternIdent (Abs.Ident ident)          -> LetPatternIdent ident
    Abs.PatternMutIdent (Abs.Ident ident)       -> LetPatternMutableIdent ident
    Abs.PatternIgnore                           -> LetPatternIgnore
    Abs.PatternTuple patterns                   -> LetPatternTuple $ map buildLetPattern (buildList patterns)
    where
        buildList :: Abs.MarkLetPatternList -> [Abs.LetPattern]
        buildList patterns = case patterns of
            Abs.MarkPatternNil        -> []
            Abs.MarkPatternOne p      -> [p]
            Abs.MarkPatternMore p ps  -> p : (buildList ps)
            Abs.MarkPatternHead p     -> [p]
            Abs.MarkPatternTail ps p  -> (buildList ps) ++ [p]
;

buildType :: Abs.Type -> Type;
buildType type' = case type' of
    Abs.TypeBool            -> Bool
    Abs.TypeI32             -> I32
    Abs.TypeRef type''      -> Reference (buildType type'')
    Abs.TypeMutRef type''   -> MutReference (buildType type'')
    Abs.TypeArray type'' n  -> Array (buildType type'') n
    Abs.TypeTuple types     -> Tuple $ map buildType $ buildList types
    where
        buildList :: Abs.MarkTypeList -> [Abs.Type]
        buildList types = case types of
            Abs.MarkTNil        -> []
            Abs.MarkTOne t      -> [t]
            Abs.MarkTMore t ts  -> t : (buildList ts)
            Abs.MarkTHead t     -> [t]
            Abs.MarkTTail ts t  -> (buildList ts) ++ [t]
;

-- Print AST to Tree
prettyPrint :: Program -> String;
prettyPrint program = drawTree $ programToTree program;

programToTree :: Program -> Tree String;
programToTree (Program funDecls) = Node "program" nodes where
    nodes = map funDeclToTree funDecls;

funDeclToTree :: FunctionDeclaration -> Tree String;
funDeclToTree (FunctionDeclaration ident parameters returnType block) =
    Node ("declaration of function \"" ++ ident ++ "\"") [parametersNode, returnTypeNode, blockToTree block] where
        parametersNode = Node "parameters: " $ map (\s -> Node (show s) []) parameters
        returnTypeNode = Node ("return type \"" ++ (show returnType) ++ "\"") []

blockToTree :: Block -> Tree String;
blockToTree (Block stmts expr) = Node "block" $ (map stmtToTree stmts) ++ [exprToTree expr]

stmtToTree :: Stmt -> Tree String
stmtToTree stmt = case stmt of
    FunDeclStmt funDecl                     -> funDeclToTree funDecl
    If expr block                           -> Node "if" [exprToTree expr, blockToTree block]
    Stmt expr                               -> Node "Stmt (ignoring result)" [exprToTree expr]
    StrictStmt expr                         -> Node "StrictStmt (result must be ())" [exprToTree expr]
    Loop block                              -> Node "loop" [blockToTree block]
    While expr block                        -> Node "while" [exprToTree expr, blockToTree block]
    IterableForLoop ident expr block        -> Node "for in iterable" [Node ("variable: " ++ ident) [], exprToTree expr, blockToTree block]
    RangeForLoop ident expr1 expr2 block    -> Node "for in range" [Node ("variable: " ++ ident) [], exprToTree expr1, exprToTree expr2, blockToTree block]
    Break                                   -> Node "break" []
    Continue                                -> Node "continue" []
    LetStmt letPattern Nothing expr           -> Node "let binding" [Node ("pattern: " ++ (show letPattern)) [], exprToTree expr]
    LetStmt letPattern (Just valueType) expr  -> Node "let binding" [Node ("pattern: " ++ (show letPattern)) [], Node ("type " ++ (show valueType)) [], exprToTree expr]

exprToTree :: Expr -> Tree String
exprToTree expr = Node "expr" []

