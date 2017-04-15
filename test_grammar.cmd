bnfc -m --haskell-gadt ../grammar.bnfc && happy -gca ParGrammar.y && alex -g LexGrammar.x && ghc --make GrammarUnitTests.hs -o GrammarUnitTests && GrammarUnitTests.exe
