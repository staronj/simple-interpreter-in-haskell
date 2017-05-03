-- Jakub Staroń, 2017


module AST where

import Data.Int (Int32)
import Data.List (intercalate)
import FormatString (format)

-- Identifier type
type Ident = String

-- Rust data type
data Type =
    I32
  | Bool
  | Tuple [Type]
  | Reference Type
  | MutableReference Type
  | Array Type Int32
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

-- Pattern
data Pattern =
    PatternVariable Ident
  | PatternMutableVariable Ident
  | PatternIgnore
  | PatternTuple [Pattern]
  deriving (Eq)

-- Literal
data Literal =
    LiteralI32 Int32
  | LiteralBool Bool
  deriving (Eq)

instance Show Literal where
    show (LiteralI32 n) =   format "literal '%0'" [show n]
    show (LiteralBool b) =  format "literal '%0'" [show b]

-- Type of binary operator.
data BinaryOperatorKind =
    Or
  | And
  | Less
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  deriving (Eq, Show)

-- Type of unary operator.
data UnaryOperatorKind =
    Negate
  | Not
  deriving (Eq, Show)

-- Expression
data Expr =
    BinaryOperator  Expr Expr BinaryOperatorKind
  | UnaryOperator   Expr UnaryOperatorKind
  | Equal           Expr Expr
  | NotEqual        Expr Expr
  | Assign          Expr Expr
  | ArrayLookup     Expr Expr
  | Dereference     Expr
  | Borrow          Expr
  | MutableBorrow   Expr
  | Identifier      Ident
  | LiteralExpr     Literal

  | FunctionCall    Ident [Expr]
  | TupleLookup     Expr Int32

  | ArrayElements   [Expr]
  | ArrayRepeat     Expr Int32
  | ArrayRange      Int32 Int32
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
  | LetStmt Pattern (Maybe Type) Expr       -- Let stmt may or may not have
  deriving (Eq)                             -- specified variable type

-- Block
data Block = Block [Stmt] Expr
    deriving (Eq)

-- Function Parameter
data FunctionParameter =
    FunctionParameter
    { pattern :: Pattern
    , valueType :: Type }
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
newtype Program =
  Program
  { functions :: [FunctionDeclaration] }


getFunctionsFromBlock :: Block -> [FunctionDeclaration]
getFunctionsFromBlock block = [funDecl | AST.FunDeclStmt funDecl <- stmts] where
  Block stmts _ = block
