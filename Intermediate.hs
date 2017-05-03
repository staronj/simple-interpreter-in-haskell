{-# LANGUAGE GADTs, KindSignatures, DataKinds, RankNTypes, FlexibleInstances #-}

module Intermediate where

import Data.Int (Int32)
import qualified AST(Ident, Type(..), unit, Literal(..))

data ExprKind = LValue | RValue

data Expr :: ExprKind -> * where
  FunctionCall ::   AST.Type -> AST.Ident -> Expr 'RValue -> Expr 'RValue
  TupleLookup ::    Expr a -> Int32 -> Expr a
  ArrayLookup ::    Expr a -> Expr b -> Expr a

  Equal ::          Expr a -> Expr b -> Expr 'RValue
  Assign ::         Expr 'LValue -> Expr a -> Expr 'RValue
  Dereference ::    AST.Type -> Expr a -> Expr 'LValue
  Borrow ::         AST.Type -> Expr 'LValue -> Expr 'RValue

  Identifier ::     AST.Type -> AST.Ident -> Expr 'LValue
  Literal ::        AST.Literal -> Expr 'RValue
  Array ::          ArrayConstructor -> Expr 'RValue
  Tuple ::          [Expr 'RValue] -> Expr 'RValue
  IfElse ::         Expr a -> Expr 'RValue -> Expr 'RValue -> Expr 'RValue

  -- Takes LValue and pushes it on the stack
  Materialize ::    Expr 'LValue -> Expr 'RValue

  -- Stmts
  If ::             Expr a -> Expr 'RValue -> Expr 'RValue
  Loop ::           LoopConstructor -> Expr 'RValue -> Expr 'RValue

  -- Flow control instructions: break and continue
  FlowControl ::    FlowControlType -> Expr 'RValue

  -- Variable binding - mapping Ident -> position in tuple
  BindVariables ::  [(AST.Ident, [Int32])] -> Expr 'RValue -> Expr 'RValue -> Expr 'RValue

  -- Evaluates both expressions and returns value of second
  Sequence ::       Expr a -> Expr b -> Expr b

instance Show (Expr a) where
  show expr = case expr of
    FunctionCall  t _ _     -> "FunctionCall"
    TupleLookup   e n       -> "TupleLookup"
    ArrayLookup   e _       -> "ArrayLookup"
    Assign        _ _       -> "Assign"
    Equal         _ _       -> "Equal"
    Dereference   t _       -> "Dereference"
    Borrow        t _       -> "Borrow"
    Identifier    t _       -> "Identifier"
    Literal       l         -> "Literal"
    Array         c         -> "Array"
    Tuple         es        -> "Tuple"
    IfElse        _ e _     -> "IfElse"
    Materialize   t         -> "Materialize"
    If            _ _       -> "If"
    Loop          _ _       -> "Loop"
    FlowControl   _         -> "FlowControl"
    BindVariables _ _ _     -> "BindVariables"
    Sequence      _ e       -> "Sequence"

data LoopConstructor :: * where
  Forever ::    LoopConstructor
  While ::      Expr a -> LoopConstructor
  ForEach ::    AST.Ident -> Expr a -> LoopConstructor
  ForRange ::   AST.Ident -> Expr 'RValue -> Expr 'RValue -> LoopConstructor

data ArrayConstructor :: * where
  Elements :: AST.Type -> [Expr 'RValue] -> ArrayConstructor
  Repeat ::   Expr a -> Int32 -> ArrayConstructor
  Range ::    Int32 -> Int32 -> ArrayConstructor

data FlowControlType = Break | Continue;

class TypeOf a where
  typeOf :: a -> AST.Type

instance TypeOf (Expr e) where
  typeOf expr = case expr of
    FunctionCall  t _ _     -> t
    TupleLookup   e n       -> let AST.Tuple ts = typeOf e in ts !! fromIntegral n
    ArrayLookup   e _       -> let AST.Array t _ = typeOf e in t
    Assign        {}        -> AST.unit
    Equal         {}        -> AST.Bool
    Dereference   t _       -> t
    Borrow        t _       -> t
    Identifier    t _       -> t
    Literal       l         -> typeOf l
    Array         c         -> typeOf c
    Tuple         es        -> AST.Tuple $ map typeOf es
    IfElse        _ e _     -> typeOf e
    Materialize   t         -> typeOf t
    If            {}        -> AST.unit
    Loop          {}        -> AST.unit
    FlowControl   {}        -> AST.unit
    BindVariables _ _ e     -> typeOf e
    Sequence      _ e       -> typeOf e

instance TypeOf ArrayConstructor where
  typeOf array = case array of
    Elements  t _ -> t
    Repeat    e n -> AST.Array (typeOf e) n
    Range     b e -> AST.Array AST.I32 (e - b)

instance TypeOf AST.Literal where
  typeOf literal = case literal of
    AST.LiteralI32  _ -> AST.I32
    AST.LiteralBool _ -> AST.Bool

data Function =
  Function
  { name :: AST.Ident
  , bindings :: [(AST.Ident, [Int32])]
  , parameter :: AST.Type
  , body :: Expr 'RValue }

data Program =
  Program
  { functions :: [Function]
  , mainUid :: String }
