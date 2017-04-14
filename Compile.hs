import qualified AST;

type Input = [String]
type Output = [String] -- Diff list

type Program = Input -> Either String Output



compile :: AST.Program -> Program
compile program = undefined
