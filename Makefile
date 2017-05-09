# Jakub Staroń, 2017

SHELL=sh
COMPILER=ghc
FLAGS=-outputdir obj -iparser
MKDIR_COMMAND = mkdir

DIRECTORIES = obj parser
PARSER_FILES = parser/AbsGrammar.hs parser/ComposOp.hs parser/ErrM.hs parser/LexGrammar.hs parser/ParGrammar.hs parser/PrintGrammar.hs parser/SkelGrammar.hs

SOURCES = *.hs Intermediate/*.hs AST/*.hs

all: directories interpreter

%: %.hs $(PARSER_FILES) $(SOURCES)
	$(COMPILER) $(FLAGS) --make $< -o $@

interpreter: Main.hs $(PARSER_FILES) $(SOURCES)
	$(COMPILER) $(FLAGS) --make Main.hs -o interpreter

$(PARSER_FILES): grammar.cf
	bnfc --haskell-gadt --outputdir=obj grammar.cf 
	happy -gca obj/ParGrammar.y
	alex -g obj/LexGrammar.x
	mv obj/*.hs parser/

${DIRECTORIES}:
	${MKDIR_COMMAND} ${DIRECTORIES}  || true

.PHONY: directories
directories: ${OUT_DIR}

.PHONY: test_grammar
test_grammar: GrammarUnitTests
	./GrammarUnitTests

.PHONY: test_type_checking
test_type_checking: TypeCheckUnitTests
	./TypeCheckUnitTests

.PHONY: tests
tests: test_grammar test_type_checking

.PHONY: clean
clean:
	rm -f $(PARSER_FILES) parser/TestGrammar.hs obj/*.hi obj/*.o obj/*.x obj/*.y obj/*.bak obj/**/*.hi obj/**/*.o Main interpreter GrammarUnitTests TypeCheckUnitTests
