# ML Interpreter

This is a interpreter of a subset of Ocaml written in Ocaml as my final project for Implementations of Programming Languages class at Kyoto University (IoPL - Spring 2020). I developed the interpreter with multiple extentions like invalid input detection, comment support, multivariable declaration, recursion, dynamic typechecking and so on.

if you are intrested in it and looking for more details about it, please refer to [IoPL-Materials](https://kuis-isle3sw.github.io/IoPLMaterials/) (written in Japanese).

</br>

## Environment Setup for Ocaml

In order to run this interpreter, you need first to install [OPAM](https://opam.ocaml.org/) and initialize some settings.

```
// install OPAM (MacOS)
brew install gpatch
brew install opam

// initial setup for OPAM
opam init -y --disable-sandboxing
opam switch list-available ocaml-base-compiler
opam switch create X.XX.X
eval $(opam env)

```

You will also need [Ocaml](http://ocaml.org/) and [Menhir](http://gallium.inria.fr/~fpottier/menhir/):

```
opam install depext
opam install user-setup
opam depext menhir dune ounit
opam install menhir dune ounit tuareg
opam user-setup install
```

</br>

## Build and Test

- `dune build`: Build
- `dune exec miniml`: Invoke the interpreter
- `dune runtest`: Run tests

You may need to run the command `eval $(opam env)` before running the above commands.

## Features

### Expression

- Arithmetic calculations (integer type): `+`,`-`,`*`,`/`
- Logic calculations (boolean type): `&&`,`||`
- Infix operator: `(+)`,`(-)`,`(*)`,`(/)`,`(&&)`,`(||)`
- if ... then ... else statement
- Comments: `(*...*)`
- let statement
  - `let ... in ...`
  - `let ... and ...`
- Function (higher-ordered function supported):
  - Static binding: `fun arguments -> body`
  - Dynamic binding: `dfun arguments -> body`
- Shorthand expression of multi-variable function:
  - `fun x1 ... xn -> ...`
  - `let f x1 ... xn = ...`
- Recursive function: `let rec arguments = ...`
- Quit interpreter: `quit` (`command + C` is supported as well)

### Type Inference

- Integer
- Boolean
- Function
- Variant
- `let`-polymorphism
- `let rec`-polymorphism

### Error Messages

- Invalid expression
  Any expression: that is not defined in [Expression](#expression) will cause an error.
- Type error
  Any expression: that is inferenced by [Type Inference](#type-inference) with an invalid type will cause an type error.

After showing error message, it will be automatically back to the prompt front so that users won't need to reinvoke the interpreter.
