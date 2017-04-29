{-# LANGUAGE GADTs, KindSignatures, DataKinds, ExistentialQuantification, TypeOperators #-}

module Intermediate where

import Data.Int
import qualified AST

data ExprKind = RValue | LValue

-- Every expr have type now?
data Expr :: ExprKind -> * where
  FunctionCall ::   AST.Type -> AST.Ident -> [Expr 'RValue] -> Expr 'RValue
  TupleLookup ::    AST.Type -> Expr a -> Int32 -> Expr a
  ArrayLookup ::    AST.Type -> Expr a -> Expr b -> Expr a

  Equal ::          Expr a -> Expr b -> Expr 'RValue
  Assign ::         Expr 'LValue -> Expr a -> Expr 'RValue
  Dereference ::    AST.Type -> Expr a -> Expr 'LValue
  Borrow ::         AST.Type -> Expr 'LValue -> Expr 'RValue

  Identifier ::     AST.Type -> AST.Ident -> Expr 'LValue
  Literal ::        AST.Literal -> Expr 'RValue
  Array ::          ArrayConstructor -> Expr 'RValue
  Tuple ::          AST.Type -> [Expr 'RValue] -> Expr 'RValue
  IfElse ::         AST.Type -> Expr a -> Expr 'RValue -> Expr 'RValue -> Expr 'RValue

  -- Takes LValue and pushes it on the stack
  Materialize ::    Expr 'LValue -> Expr 'RValue

  -- Stmts
  If ::             Expr a -> Expr 'RValue -> Expr 'RValue
  Loop ::           LoopConstructor -> Expr 'RValue -> Expr 'RValue

  -- Flow control instructions: break and continue
  FlowControl ::    FlowControlType -> Expr 'RValue
  BindVariables ::  [(AST.Ident, [Int32])] -> Expr 'RValue -> Expr 'RValue

  -- Evaluates both expressions and returns value of second
  Sequence ::       Expr a -> Expr b -> Expr b

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
    TupleLookup   t _ _     -> t
    ArrayLookup   t _ _     -> t
    Assign        _ _       -> AST.unit
    Equal         _ _       -> AST.Bool
    Dereference   t _       -> t
    Borrow        t _       -> t
    Identifier    t _       -> t
    Literal       l         -> typeOf l
    Array         c         -> typeOf c
    Tuple         t _       -> t
    IfElse        t _ _ _   -> t
    Materialize   t         -> typeOf t
    If            _ _       -> AST.unit
    Loop          _ _       -> AST.unit
    FlowControl   _         -> AST.unit
    BindVariables _ _       -> AST.unit
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

data Function = Function { name :: AST.Ident, body :: Expr 'RValue }

newtype Program = Program {  functions :: [Function] }
