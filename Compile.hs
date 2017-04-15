-- Jakub Staroń, 2017

module Compile (compile) where

import qualified Data.Map.Strict as Map
import qualified AST
import DiffList
import Data.Int

type Input = [Int32]
type Output = DiffList Int32

type Program = Input -> Either (String, Output) Output


type Store = ()
type VEnv = Map.Map AST.Ident ()
type FEnv = Map.Map AST.Ident ()

type State = (Input, Output, Store)


compile :: AST.Program -> Either String Program
compile program =
    Right $ \_ -> Right Nil
