package org.thinkmeta.smp.playground

import org.thinkmeta.smp.core.{HostedLisp, ASTShow}

/**
 * @author Hossam Karim
 */
object MacrosPlayground extends App {

  import language.reflectiveCalls

  val m1 = ASTShow.showModule {
    """
    module A {
      using scala::math::_
      module B {
        using A::*
        (defun f [x:int] (plus x 1))
      }
    }
    """
  }

  println(m1)

  val HelloWorld = HostedLisp.module {
    """
    module HelloWorld {
      (defun hello "Hello World from Lisp from Scala from JVM")
    }
    """
  }

  println(HelloWorld.hello)


  val compiled = HostedLisp.module {
    """
    module HostedLispInScala {
      (defun f:int [i:int j:int] (+ i j))
      (defun g:int [i:int j:int] (f i j))
      (defun h "hello")
    }
    """
  }

  println {
    s"""
      ${compiled.f(1,2)}
      ${compiled.g(1,2)}
      ${compiled.h()}
    """
  }

  val mA = HostedLisp.module {
    """
    module A {
      using scala::math::_
      (defun hello "Hello from A")
      (defun calc [a:double] (sin a))
      module B {
        (defun f [x:int] (+ x 1))
        (defun hello "Hello from B")
      }
    }
    """
  }

  println {
    s"""
    ${mA.calc(0)}
    ${mA.B.f(1)}
    """
  }

  val fact = HostedLisp.function {
    """
      (defun factorial:int [n:int]
         (if (le n 1)
            1
            (* n (factorial (- n 1)))))
    """
  }

  println(s"${fact.factorial(10)}")

  val lambdas = HostedLisp.module {
    """
    module ds {
      using Lists::_
      (val x (new StringBuilder "5"))
      (val option (Some 1))
      (defun showOption (option.map {o → (+ o 1)}))
      (val l (List 1 2 3 4 5))
      (defun show0 (map { x:int → (+ x 1) } l))
      (defun show1 (l.reduce {x:int y:int → (+ x y)}))
      (defun show2 l.length.toString)
    }
    """
  }

  println(s"${lambdas.showOption}")
  println(s"${lambdas.show0}")
  println(s"${lambdas.show1}")
  println(s"${lambdas.show2}")


  val chaining = HostedLisp.module {
    """
    module chaining {
      (defun show1
        (. (List 1 2 3)
           (map {x → (+ x 1)})
           (reduce {x y → (+ x y)})))

      (defun show2
        (val sb (new StringBuilder))
        (. sb (append "a")
              (append "b")
              (append "c")))

      (defun show3
        (. "1" toInt))

      (defun show4
        (val fn {x:int → (* x x)})
        (val l (. 1 (to 10)))
        (. l (map fn)))

      (defun show5
        (val fn {x:int → (* x x)})
        (val l (. 1 (to 10)))
        (l.map fn))

      (defun run
        (println show1)
        (println show2)
        (println show3)
        (println show4)
        (println show5))
    }
    """
  }

  chaining.run


  val patterns = HostedLisp.module {
    """
    module patterns {
      (defun show1
        (val t (Tuple3  1 "a" 2))
        (match t
          ((Tuple3 1 b c) if (gt c 3) → "first")
          ((Tuple3 1 b c) → "second")
          ((Tuple3 a b c) → "third")
        )
      )
    }
    """
  }

  println(s"${patterns.show1}")

  val par = HostedLisp.module {
    """
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
    """
  }

  par.run

}
