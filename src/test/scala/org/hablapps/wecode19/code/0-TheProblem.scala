package org.hablapps.wecode19
package code

import scala.io.StdIn.readLine
import org.scalatest._

object ConventionalApproach{

  object Monolithic{

    def echo(): String = {
      val msg: String = readLine()
      println(msg)
      msg
    }
  }

  object Modular{

    // API

    trait IO{
      def read(): String
      def write(msg: String): Unit
    }

    // PROGRAM OVER API

    def echo()(io: IO): String = {
      val msg: String = io.read()
      io.write(msg)
      msg
    }

    // API INSTANCE

    object ConsoleIO extends IO{
      def read() = readLine
      def write(msg: String) = println(msg)
    }

    // COMPOSITION

    def consoleEcho(): String =
      echo()(ConsoleIO)

  }

  object ModularWithMultipleAPIs{

    // API

    trait Log{
      def log(level: Log.Level, msg: String): Unit
      def warn(msg: String): Unit = log(Log.WARN,msg)
      def debug(msg: String): Unit = log(Log.DEBUG,msg)
      def trace(msg: String): Unit = log(Log.TRACE,msg)
    }

    object Log{
      sealed abstract class Level
      case object WARN extends Level
      case object DEBUG extends Level
      case object TRACE extends Level
    }

    // API INSTANCE

    object ConsoleLog extends Log{
      def log(level: Log.Level, msg: String) =
        println(s"$level: $msg")
    }

    // PROGRAM OVER APIs

    import Modular.IO

    def echoWithLog()(io: IO, log: Log): String = {
      val msg: String = io.read()
      log.trace(s"read $msg")
      io.write(msg)
      log.trace(s"written $msg")
      msg
    }

    // COMPOSITION

    import Modular.ConsoleIO
    def consoleEchoWithLog(): String =
      echoWithLog()(ConsoleIO, ConsoleLog)
  }

  object ButNotGeneral{
    import scala.concurrent.{Await, Future, ExecutionContext, duration}
    import ExecutionContext.Implicits.global, duration._

    // API INSTANCE (WRONG)

    object WrongAsyncIO extends Modular.IO{

      def read(): String = {
        val futureMsg: Future[String] = ???
        Await.result(futureMsg, 1 second)
      }
      def write(msg: String): Unit = ???
    }

    // ASYNCHRONOUS API

    trait IO{
      def read(): Future[String]
      def write(msg: String): Future[Unit]
    }

    // API INSTANCE

    object AsyncIO extends IO{
      def read(): Future[String] = Future(scala.io.StdIn.readLine())
      def write(msg: String): Future[Unit] = Future(println(msg))
    }

    // PROGRAM OVER API

    def echo()(io: IO): Future[String] =
      io.read().flatMap{ msg =>
        io.write(msg).flatMap{ _ =>
          Future.successful(msg)
        }
      }
  }

  object AnotherExample{

    import java.io.FileDescriptor

    // FILE IO

    case class IOFileState(inFD: FileDescriptor, outFD: FileDescriptor)

    trait IO{
      def read()            : IOFileState => String
      def write(msg: String): IOFileState => Unit
    }
  }

  object YetAnotherOne{

    // SIMULATED IO FOR UNIT TESTING

    case class IOState(reads: List[String], writes: List[String])

    trait IO{
      def read()            : IOState => (IOState, String)
      def write(msg: String): IOState => (IOState, Unit)
    }
  }
}
