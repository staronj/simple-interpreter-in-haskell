-- Jakub Staroń, 2017

module Main where

import Control.Monad
import Data.Either

import FormatString
import AST
import TypeCheck

left :: Either a b -> a
left e = case e of
  Left a -> a
  Right _ -> undefined

right :: Either a b -> b
right e = case e of
  Left _ -> undefined
  Right b -> b

test :: Bool -> String -> String -> IO ()
test shouldTypeCheck message code = do
  let ast = AST.buildAST code
  when (isLeft ast) $ fail $ format "Test \"%0\" failed to parse, error is\n%1" [message, left ast]
  let typeChecked = typeCheck $ right ast
  case typeChecked of
    Left err -> when shouldTypeCheck $ fail $ format "Test \"%0\" should type-check but it don't. Error is \"%1\"." [message, err]
    Right _ -> unless shouldTypeCheck $ fail $ format "Test \"%0\" should not type-check but it does." [message]

tests :: [IO()]

tests =
 [
  test True "empty-main"
    "fn main() { }",

  test False "main-wrong-number-of-arguments"
    "fn main(a : i32) { }",

  test False "main-wrong-declared-return-type"
    "fn main() -> i32 { 42 }",

  test False "main-wrong-return-type"
    "fn main() { 42 }",

  test False "main-not-declared"
    "fn foo() { }",

  test False "main-not-declared-ex"
    "fn foo() { fn main() { } }",

  test True "function-declared"
    "fn foo() { } fn main() { foo(); }",

  test False "function-not-declared"
    "fn main() { foo(); }",

  test False "if-wrong-block-type"
    "fn main() { if true { 42 } }",

  test False "if-wrong-condition-type"
    "fn main() { if 42 { } }",

  test True "if-minimal-example"
    "fn main() { if true { } }",

  test True "if-else-minimal-example"
    "fn main() { if true { } else { } }",

  test True "if-else-values-minimal-example"
    "fn main() { if true { 42 } else { 31 }; }",

  test True "if-else-values-in-block"
    "fn main() { { if true { 42 } else { 31 } }; }",

  test False "if-else-wrong-condition-type"
    "fn main() { if 42 { } else { } }",

  test False "if-else-incompatible-types"
    "fn main() { { if true { true } else { 42 } }; }",

  test True "let-minimal-example"
    "fn main() { let a = true; }",

  test True "let-minimal-example-with-type"
    "fn main() { let a : bool = true; }",

  test False "let-inconsistent-types"
    "fn main() { let a : bool = 42; }",

  test True "let-ignore-result"
    "fn main() { let _ = 42; }",

  test True "let-match-tuple"
    "fn main() { let (a, b) = (1, 2); }",

  test True "let-match-tuple-to-single"
    "fn main() { let tuple = (1, 2); }",

  test False "let-match-single-to-tuple"
    "fn main() { let (a, b) = 42; }",

  test False "let-match-inconsistent-tuple-sizes"
    "fn main() { let (a, b) = (1, 2, 3); }",

  test True "or-minimal-example"
    "fn main() { true || false; }",

  test False "or-wrong-types"
    "fn main() { true || 42; }",

  test True "equal-minimal-example"
    "fn main() { true == false; }",

  test False "equal-wrong-types"
    "fn main() { true == 42; }",

  test True "equal-tuples"
    "fn main() { (1, 2, 3) == (4, 5, 6); }",

  test False "equal-tuples-inconsistent-sizes"
    "fn main() { (1, 2, 3) == (4, 5); }",

  test True "equal-arrays"
    "fn main() { [1, 2, 3] == [4, 5, 6]; }",

  test False "equal-arrays-inconsistent-sizes"
    "fn main() { [1, 2, 3] == [4, 5]; }",

  test True "array-repeat-type"
    "fn main() { let arr : [i32; 10] = [1 + 1; 10]; }",

  test True "array-repeat-type"
    "fn main() { let arr : [i32; 10] = [1 + 1; 10]; }",

  test False "array-repeat-type-inconsistent-sizes"
    "fn main() { let arr : [i32; 10] = [1 + 1; 100]; }",

  test True "use-variable-minimal-example"
    "fn main() { let a = 42; a + 1; }",

  test False "use-variable-wrong-type"
    "fn main() { let a = true; a + 1; }",

  test False "use-undeclared-variable"
    "fn main() { let a = 42; b + 1; }",

  test True "names-shadowing"
    "fn main() { let a = 42; { let a = true; a || false; } a + 1; }",

  test True "assign-minimal-example"
    "fn main() { let mut a = 0; a = 1; }",

  test False "assign-to-not-mutable"
    "fn main() { let a = 0; a = 1; }",

  test True "assign-to-array-element"
    "fn main() { let mut a = [1, 2, 3]; a[0] = 1; }",

  test False "assign-to-not-mutable-array-element"
    "fn main() { let a = [1, 2, 3]; a[0] = 1; }",

  test True "assign-to-array"
    "fn main() { let mut a = [1, 2, 3]; a = [0, 1, 2]; }",

  test False "assign-to-not-mutable-array"
    "fn main() { let a = [1, 2, 3]; a = [0, 1, 2]; }",

  test True "assign-to-tuple-element"
    "fn main() { let mut a = (1, 2, 3); a.0 = 1; }",

  test False "assign-to-not-mutable-tuple-element"
    "fn main() { let a = (1, 2, 3); a.0 = 1; }",

  test True "assign-to-tuple"
    "fn main() { let mut a = (1, 2, 3); a = (0, 1, 2); }",

  test False "assign-to-not-mutable-tuple"
    "fn main() { let a = (1, 2, 3); a = (0, 1, 2); }",

  test False "tuple-out-of-bound-lookup"
    "fn main() { let a = (1, 2, 3); a.3; }",

  test True "array-range-minimal-example"
    "fn main() { let a = [0..10]; }",

  test False "array-range-empty"
    "fn main() { let a = [0..0]; }",

  test True "loop-minimal-example"
    "fn main() { loop { } }",

  test False "loop-wrong-block-type"
    "fn main() { loop { 42 } }",

  test True "while-minimal-example"
    "fn main() { while true { } }",

  test False "while-wrong-type"
    "fn main() { while 42 { } }",

  test False "while-wrong-block-type"
    "fn main() { while true { 42 } }",

  test True "range-for-loop-minimal-example"
    "fn main() { for x in 1..2 { } }",

  test False "range-for-loop-wrong-type"
    "fn main() { for x in 1..true { } }",

  test False "range-for-loop-wrong-block-type"
    "fn main() { for x in 1..2 { 42 } }",

  test True "iterable-for-loop-minimal-example"
    "fn main() { for x in [1, 2, 3] { } }",

  test False "iterable-for-loop-wrong-block-type"
    "fn main() { for x in [1, 2, 3] { 42 } }",

  test True "negation-minimal-example"
    "fn main() { -1; }",

  test False "negation-wrong-type"
    "fn main() { -true; }",

  test True "not-minimal-example"
    "fn main() { !true; }",

  test False "not-wrong-type"
    "fn main() { !42; }"
 ]

main :: IO ()
main = sequence_ tests
