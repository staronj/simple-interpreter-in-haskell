#### Building
To build the interpreter please run ```make```.

Please use up to date versions of ghc and bnfc. The code is written and working under bnfc 2.8.1 and ghc 8.0.2.
Versions of bnfc and ghc installed on 'students' are outdated and may not accept this code.

#### Usage
To interpret source file *foo.rc* please type ```interpreter foo.rc```.

Example:
```
interpreter examples/hello_world.rc
# output:
42
```

To dump ast of given source file please type ```interpreted --ast-dump foo.rc```

#### Tests
Unit tests for grammar can be found in file *GrammarUnitTests.hs*. Unit tests for type checking phase can be fount in file *TypeCheckUnitTests.hs*. Please consider unit tests located in that file as part of code examples, especially *should-fail* tests - examples of mailformed programs.

To run the tests please run ```make tests```.

#### Todo
* Implement constructing array from listed elements.
* Implement constructing array from range.
* Fix problem with interleaving streams (output appears only after whole program executes)
* Allow pattern instead of name in IterableForLoop.
* Implement break and continue in compilation part.
* Implement typecheck for Borrowing, MutableBorrowing and Dereference.
* Implement Intermediate representation building for above.
* Implement compilation part of above.
