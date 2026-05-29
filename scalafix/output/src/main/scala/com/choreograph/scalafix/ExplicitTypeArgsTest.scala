package com.choreograph.scalafix
// format: off
import scala.concurrent.Future
import java.time.Instant
import scala.collection.immutable.TreeSet
import scala.collection.mutable.Buffer
import scala.concurrent.duration.FiniteDuration
import scala.util.{ Either, Success, Try }

object ExplicitTypeArgsTest {
  // Type from another package that needs import
  // Future is imported, but ExecutionContext is not
  val future1 = Option(1).map[Future[Int]](x => Future.successful(x))

  // Type from scala.util that needs import
  val opt1 = Option("123").map[Try[Int]](s => scala.util.Try(s.toInt))

  // Type from java.time that needs import
  val opt2 = Option(1L).map[Instant](millis => java.time.Instant.ofEpochMilli(millis))

  // Nested types requiring multiple imports
  val opt3 = Option("data").map[Try[Instant]](_ => scala.util.Try(java.time.Instant.now()))

  // Generic type with type parameter needing import
  import scala.collection.mutable
  val opt4 = Option(List(1, 2, 3)).map[Buffer[Int]](lst => mutable.Buffer.from(lst))

  // Type already imported - should not add duplicate import
  val future2 = Option(42).map[Future[Int]](x => Future.successful(x * 2))

  // Fully qualified type in different package
  object other {
    case class CustomType(value: Int)
  }
  val opt5 = Option(1).map(x => other.CustomType(x))

  // Either type from scala.util
  val opt6 = Option("test").map[Either[String, String]](s => scala.util.Right(s): scala.util.Either[String, String])

  // Type from same package - should not need import
  case class LocalType(data: String)
  val opt7 = Option("hello").map(s => LocalType(s))

  // Multiple type parameters with imports
  val opt8 = Option(1).map[Success[Try[Int]]](x => scala.util.Success(scala.util.Try(x)))

  // Type with nested package path
  val opt9 = Option(Seq(1, 2, 3)).map[TreeSet[Int]](seq => scala.collection.immutable.TreeSet.from(seq))

  // Type from scala.concurrent.duration
  val opt10 = Option(5).map[FiniteDuration](n => scala.concurrent.duration.Duration(n, scala.concurrent.duration.SECONDS))

  // Type alias
  type CustomOption[T] = Option[T]
  val opt11 = Option(1).map[Option[Int]](x => Some(x): CustomOption[Int])
}
