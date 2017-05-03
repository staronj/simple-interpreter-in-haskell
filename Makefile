# Jakub Staroń, 2017

SHELL=sh
COMPILER=ghc
FLAGS=-outputdir obj -iparser
MKDIR_COMMAND = mkdir

DIRECTORIES = obj parser
PARSER_FILES = parser/AbsGrammar.hs parser/ComposOp.hs parser/ErrM.hs parser/LexGrammar.hs parser/ParGrammar.hs parser/PrintGrammar.hs parser/SkelGrammar.hs

SOURCES = *.hs Intermediate/*.hs AST/*.hs

all: directories interpreter.exe

test_grammar: GrammarUnitTests.exe
	./GrammarUnitTests.exe

test_type_checking: TypeCheckUnitTests.exe
	./TypeCheckUnitTests.exe

%.exe: %.hs $(PARSER_FILES) $(SOURCES)
	$(COMPILER) $(FLAGS) --make $< -o $@

interpreter.exe: Main.hs $(PARSER_FILES) $(SOURCES)
	$(COMPILER) $(FLAGS) --make Main.hs -o interpreter.exe

$(PARSER_FILES): grammar.bnfc
	bnfc -m --haskell-gadt grammar.bnfc  --outputdir=obj
	happy -gca obj/ParGrammar.y
	alex -g obj/LexGrammar.x
	mv obj/*.hs parser/

${DIRECTORIES}:
	${MKDIR_COMMAND} ${DIRECTORIES}  || true

.PHONY: directories
directories: ${OUT_DIR}

.PHONY: clean
clean:
	rm -f $(PARSER_FILES) parser/TestGrammar.hs obj/*.hi obj/*.o obj/*.x obj/*.y obj/Makefile Main.exe interpreter.exe GrammarUnitTests.exe TypeCheckUnitTests.exe
