{-# LANGUAGE GADTs, KindSignatures, DataKinds, Rank2Types #-}

module AST where

import qualified AbsGrammar as Abs;
import qualified ParGrammar as Par;
import qualified ErrM as ErrM;
import Control.Monad.Except;
import Data.List;

type Ident = String;

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
   

data FunctionParameter = FunctionParameter { ident :: Ident, valueType :: Type};

data LetPattern = 
    LetPatternIdent Ident |
    LetPatternIgnore      |
    LetPatternTuple [LetPattern];

data Literal = 
    I32Literal Integer | 
    BoolLiteral Bool;

data KindTag = ExprTag | StmtTag ;

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
    Assign          Expr Expr        |
    Negate          Expr             |
    Dereference     Expr             |
    Not             Expr             |
    Borrow          Expr             |
    MutableBorrow   Expr             |
    Identifier      Ident            |

    FunctionCall    Ident [Expr]     |
    ArrayLookup     Expr Expr        |
    TupleLookup     Expr Literal     |

    ArrayElements   [Expr]           |
    ArrayRepeat     Expr Literal     |
    ArrayRange      Literal Literal  |
    TupleConstruct  [Expr]           |

    IfElse          Expr Block Block
;

instance Show Expr where
    show _ = "expr"

-- Statement
data Stmt = 
    If Expr Block                       |
    Stmt Expr                           |   -- Expects any type
    StrictStmt Expr                     |   -- Expects unit type
    Loop Block                          |
    While Block                         |
    IterableForLoop Ident Expr Block    |
    RangeForLoop Ident Expr Expr Block  |
    Break                               |
    Continue                            |
    LetStmt LetPattern (Maybe Type) Expr    

instance Show Stmt where
    show _ = "stmt"

-- Block
data Block = Block [Stmt] Expr;
instance Show Block where
    show (Block stmts expr) = "Block {" ++ (show stmts) ++ "; " ++ (show expr) ++ "}"

-- Function declaration
data FunctionDeclaration = FunctionDeclaration Ident [FunctionParameter] Type Block;
instance Show FunctionDeclaration where
    show (FunctionDeclaration ident paramteres resultType block) = "fn " ++ ident ++ " -> " ++ (show resultType)

-- Program
data Program = Program [FunctionDeclaration];
instance Show Program where
    show (Program functions) = intercalate "\n" $ map show functions
;

buildAST :: String -> Except String Program;
buildAST str = let tokens = Par.myLexer str in case Par.pProgram tokens of
       ErrM.Bad msg  -> do throwError msg
       ErrM.Ok  tree -> do return $ buildProgram tree

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
buildExpr = undefined

buildStmt :: Abs.Stmt -> Stmt
buildStmt stmt = undefined;

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

