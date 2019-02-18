package org.hablapps.wecode19
package code

import org.scalatest._

import code.ConventionalApproach.Modular.{IO => ConventionalIO}
import code.monads.`I-FunctionalAPIs`.{Monad, IO}
import code.monads.`II-Instantiations`.{StateIO, StateMonad, IOState}

/**
 * Write a program that reads from  standard input and says whether it's
 * even or ordd.
 *
 * For instance:
 *
 * scala> runWriteANumber
 * << type "8" >>
 * 8 is even
 *
 * scala> runWriteANumber
 * << type "5" >>
 * 5 is odd
 */
object Program1 extends Program1
class Program1 extends FunSpec with Matchers{

  // Helper method
  def evenOdd(n: String): String =
    if (n.toInt % 2 == 0)
      s"$n es un número par"
    else
      s"$n es un número impar"


  // Impure version, to be used as a specification

  object Conventional{

    def writeANumber(io: ConventionalIO): Unit = {
      val num = io.read
      io.write(evenOdd(num))
    }

  }

  // Declarative version, to be implemented
  object Declarative{

    def writeANumber[F[_]](io: IO[F], m: Monad[F]): F[Unit] =
      m.flatMap(io.read()){ num =>
        m.flatMap(io.write(evenOdd(num))){ _ =>
          m.returns(())
        }
      }
  }

  describe("WriteANumber"){
    it("should leer el número 3 y decir que es impar"){
      val ios = IOState("3" :: Nil, Nil)
      Declarative.writeANumber(StateIO, StateMonad)(ios) shouldBe
        (IOState(Nil, "3 es un número impar" :: Nil), ())
    }

    it("should leer el número 4 y decir que es par"){
      val ios = IOState("4" :: Nil, Nil)
      Declarative.writeANumber(StateIO, StateMonad)(ios) shouldBe
        (IOState(Nil, "4 es un número par" :: Nil), ())
    }
  }
}

/**
 * Same as in `Program1` but now the program asks politely
 * for a number to the user.
 *
 * For instance:
 *
 *   scala> runWriteANumber2
 *   Please, type a number:
 *   << type 8 >>
 *   8 is even
 */
class Program2 extends FunSpec with Matchers{

  // Impure version

  object Conventional{
    import Program1.Conventional.writeANumber

    def writeANumberBis(io: ConventionalIO): Unit = {
      io.write("Introduce un número por favor:")
      writeANumber(io)
    }
  }

  // Declarative version
  object Declarative{
    import Program1.Declarative.writeANumber

    def writeANumberBis[F[_]](io: IO[F], m: Monad[F]): F[Unit] =
      m.flatMap(io.write("Introduce un número por favor:")){ _ =>
        m.flatMap(writeANumber[F](io, m)){ _ =>
          m.returns(())
        }
      }
  }

  describe("WriteANumber2"){
    it("should leer el número 3 y decir que es impar"){
      val ios = IOState("3" :: Nil, Nil)
      Declarative.writeANumberBis(StateIO, StateMonad)(ios) shouldBe
        (IOState(Nil, "3 es un número impar" :: "Introduce un número por favor:" :: Nil), ())
    }

    it("should leer el número 4 y decir que es par"){
      val ios = IOState("4" :: Nil, Nil)
      Declarative.writeANumberBis(StateIO, StateMonad)(ios) shouldBe
        (IOState(Nil, "4 es un número par" :: "Introduce un número por favor:" :: Nil), ())
    }
  }


}

/**
 * Write a program that reads lines continously until it's read
 * the word "exit"
 *
 * For instance:
 * scala> readUntilExit
 *   <<type "hi" and enter>>
 *   <<type "bye" and enter>>
 *   <<type "exit" and enter>>
 */
class Program3 extends FunSpec with Matchers{

  // Impure version

  object Conventional{

    def readUntilExit(io: ConventionalIO): Unit = {
      val msg = io.read()
      if (msg == "exit") ()
      else readUntilExit(io)
    }

  }

  // Declarative version
  object Declarative{

    def readUntilExit[F[_]](io: IO[F], m: Monad[F]): F[Unit] =
      m.flatMap(io.read){ msg =>
        m.flatMap(if (msg == "exit") m.returns(()) else readUntilExit[F](io, m)){ _ =>
          m.returns(())
        }
      }
  }

  describe("ReadUntilExit"){
    it("should leer cadenas de texto hasta encontrar `exit`"){
      val ios = IOState("uno" :: "dos" :: "tres" :: "exit" :: Nil, Nil)
      Declarative.readUntilExit(StateIO, StateMonad)(ios) shouldBe
        (IOState(Nil, Nil), ())
    }

    it("should parar de leer cuando encuentra un `exit`"){
      val ios = IOState("uno" :: "dos" :: "tres" :: "exit" :: "otro" :: Nil, Nil)
      Declarative.readUntilExit(StateIO, StateMonad)(ios) shouldBe
        (IOState("otro" :: Nil, Nil), ())
    }
  }
}

/**
 * Same as in Program3 but now the program echoes user input.
 *
 * For instance:
 *
 * scala> readUntilExit2
 *   <<type "hi" and enter>>
 *   hi
 *   <<type "bye" and enter>>
 *   bye
 *   <<type "exit" and enter>>
 */
class Program4 extends FunSpec with Matchers{

  // Impure version

  object Conventional{

    def readUntilExit(io: ConventionalIO): Unit = {
      val msg = io.read
      io.write(msg)
      if (msg == "exit") ()
      else readUntilExit(io)
    }
  }

  // Declarative version
  object Declarative{

    def readUntilExit[F[_]](io: IO[F], m: Monad[F]): F[Unit] =
      m.flatMap(io.read){ msg =>
        m.flatMap(io.write(msg)){ _ =>
          m.flatMap(if (msg == "exit") m.returns(())
                    else readUntilExit[F](io, m)){ _ =>
            m.returns()
          }
        }
      }
  }

  describe("ReadUntilExit2"){
    it("should leer cadenas de texto hasta encontrar `exit`"){
      val ios = IOState("uno" :: "dos" :: "tres" :: "exit" :: Nil, Nil)
      Declarative.readUntilExit(StateIO, StateMonad)(ios) shouldBe
        (IOState(Nil, "exit" :: "tres" :: "dos" :: "uno" :: Nil), ())
    }

    it("should parar de leer cuando encuentra un `exit`"){
      val ios = IOState("uno" :: "dos" :: "tres" :: "exit" :: "otro" :: Nil, Nil)
      Declarative.readUntilExit(StateIO, StateMonad)(ios) shouldBe
        (IOState("otro" :: Nil, "exit" :: "tres" :: "dos" :: "uno" :: Nil), ())
    }
  }
}

