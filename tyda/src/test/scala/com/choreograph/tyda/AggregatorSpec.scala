package com.choreograph.tyda

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.ObjectStreamClass

import scala.util.Using

import org.scalatest.funsuite.AnyFunSuite

object AggregatorSpec {
  def assertDoesNotCapture(value: Serializable, forbiddenClasses: Class[?]*): Unit = {
    val bytes = Using.resource(ByteArrayOutputStream()) { baos =>
      Using.resource(ObjectOutputStream(baos))(_.writeObject(value))
      baos.toByteArray
    }
    Using.resource(ByteArrayInputStream(bytes)) { bais =>
      val ois = new ObjectInputStream(bais) {
        override def resolveClass(desc: ObjectStreamClass): Class[?] = {
          val cls = super.resolveClass(desc)
          forbiddenClasses.foreach { forbidden =>
            assert(
              !forbidden.isAssignableFrom(cls),
              s"${forbidden.getSimpleName} subtype found in serialized value: ${desc.getName}"
            )
          }
          cls
        }
      }
      ois.readObject(): Unit
    }
  }
}

class AggregatorSpec extends AnyFunSuite {
  import AggregatorSpec.*

  def testAggregatorDoesNotCaptureCodec[P <: PrimitiveAggregate[?, ?]: TypeName](agg: P): Unit =
    test(s"Aggregator for ${TypeName.name[P]} does not serialize any Codec") {
      assertDoesNotCapture(
        PrimitiveAggregateEvaluation.aggregatorAndIntermediateCodec(agg).aggregator,
        classOf[Codec[?]]
      )
    }

  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.Collect[Long]())
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.Collect[Option[Long]]())
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.Collect[(a: Int, b: String)]())
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.Count[Long]())
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.CountSome[Long]())
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.BoolAnd())
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.BoolOr())
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.Min[Long](Comparable.long))
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.Max[Long](Comparable.long))
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.MinBy[Long, Long](Comparable.long))
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.MaxBy[Long, Long](Comparable.long))
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.Reduce[Long](_ + _))
  testAggregatorDoesNotCaptureCodec(PrimitiveAggregate.Sum(SumMagnet[Long]))
}
