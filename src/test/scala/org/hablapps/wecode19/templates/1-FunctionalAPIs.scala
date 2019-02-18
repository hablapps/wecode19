package org.hablapps.wecode19
package templates

import scala.concurrent.{Await, Future, ExecutionContext, duration}
import ExecutionContext.Implicits.global, duration._
import scala.io.StdIn.readLine
import org.scalatest._

object `I-FunctionalAPIs`{

  // Functional API

  // Particular APIs for particular computation types can now be
  // defined in a modular way

  // Asynchronous computations

  trait AsyncIO{
    def read(): Future[String]
    def write(msg: String): Future[Unit]
  }

  // State-transformations

  case class IOState(reads: List[String], writes: List[String])

  trait StateIO{
    def read(): IOState => (IOState, String)
    def write(msg: String): IOState => (IOState, Unit)
  }

  // Synchronous transformations

  trait SyncIO{
    def read(): String
    def write(msg: String): Unit
  }

}


object `II-Instantiations`{
  import `I-FunctionalAPIs`._

  // Asynchronous computation

  // object AsyncIO extends IO{
  //   def read() = ??? // Whatever
  //   def write(msg: String) = ??? // Whatever
  // }

  // Console-based interpretation of IO Programs

  // object ConsoleIO extends IO{
  //   def read() = readLine()
  //   def write(msg: String) = println(msg)
  // }

  // State transformations

  // object StateIO extends IO{
  //   def read(): IOState => (IOState, String) = ???
  //   def write(msg: String): IOState => (IOState, Unit) = ???
  // }
}

object `III-Logic`{
  import `I-FunctionalAPIs`._

  // def echo()(io: IO): String = {
  //   val msg: String = io.read()
  //   io.write(msg)
  //   msg
  // }
}

object `IV-Composition`{
  // import `I-FunctionalAPIs`.IOState
  // import `II-Instantiations`.{ConsoleIO, AsyncIO, StateIO}
  // import `III-Logic`.echo

  // def consoleEcho(): String =
  //   echo()(ConsoleIO)

  // def stateEcho(): IOState => (IOState, String) = ???

  // def asynEcho(): Future[String] = ???
}
