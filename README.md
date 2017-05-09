#### Building
To build the interpreter please run ```make```.

Please use current versions of ghc and bnfc. The code is written and working under bnfc 2.8.1 and ghc 8.0.2.

#### Usage
To interpret source file *foo.rc* please type ```interpreter foo.rc```.

Example:
```
interpreter examples/hello_world.rc
# output:
42
```

#### Tests
Unit tests for grammar can be found in file *GrammarUnitTests.hs*. Unit tests for type checking phase can be fount in file *TypeCheckUnitTests.hs*. Please consider unit tests located in that file as part of code examples, especially *should-fail* tests - examples of mailformed programs.

To run the tests please run ```make tests```.
