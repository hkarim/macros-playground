macros-playground
=================

A playground for Scala macros, including a useless embedded Lisp like syntax to quickly create macros

## Usage

To see the example, run:

    cd macros-playground
    sbt
    project playground
    compile
    runMain org.thinkmeta.smp.playground.MacrosPlayground

## Useless Lisp Syntax

Currently the language supports the following features:
- Module definitions including nested modules
- Function definitions
- Value definitions
- Lambdas
- Limited support for pattern matching
- Instantiating Scala classes and calling methods

### Examples

- The factorial function can be written as:
```
    (defun factorial:int [n:int]
      (if (le n 1)
        1
        (* n (factorial (- n 1)))))
```

- Here is a longer example, showing some of the language features:
```
    module par {
      using scala::concurrent::_
      using ExecutionContext::Implicits::global
      using scala::util::Failure
      using scala::util::Success
      using scala::concurrent::duration::_

      (defun run
        (val futureList
          (future
            (.
              (Range 1 100)
              (map { x → Math.random } ))))
        (val result ( futureList.map { x -> x.sum } ))
        (result.onComplete {
            x →
            (match x
              ((Success v) → (println v))
              ((Failure m) → (println m)))
          })
        (Await.result result (. 4 seconds))
      )
    }
```





