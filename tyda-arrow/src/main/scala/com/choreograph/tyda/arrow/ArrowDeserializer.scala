package com.choreograph.tyda.arrow

import java.io.ByteArrayInputStream

import scala.collection.AbstractIterator
import scala.collection.Factory
import scala.util.Using

import org.apache.arrow.memory.BufferAllocator
import org.apache.arrow.vector.BigIntVector
import org.apache.arrow.vector.BitVector
import org.apache.arrow.vector.DateDayVector
import org.apache.arrow.vector.DecimalVector
import org.apache.arrow.vector.DurationVector
import org.apache.arrow.vector.FieldVector
import org.apache.arrow.vector.Float4Vector
import org.apache.arrow.vector.Float8Vector
import org.apache.arrow.vector.IntVector
import org.apache.arrow.vector.SmallIntVector
import org.apache.arrow.vector.TimeStampMicroTZVector
import org.apache.arrow.vector.TinyIntVector
import org.apache.arrow.vector.VarBinaryVector
import org.apache.arrow.vector.VarCharVector
import org.apache.arrow.vector.VectorSchemaRoot
import org.apache.arrow.vector.complex.ListVector
import org.apache.arrow.vector.complex.MapVector
import org.apache.arrow.vector.complex.StructVector
import org.apache.arrow.vector.ipc.ArrowReader
import org.apache.arrow.vector.ipc.ArrowStreamReader
import org.apache.arrow.vector.util.Text

import com.choreograph.tyda.Codec
import com.choreograph.tyda.Date
import com.choreograph.tyda.Decimal
import com.choreograph.tyda.Duration
import com.choreograph.tyda.Timestamp

private[tyda] object ArrowDeserializer {
  private type Deserializer[T] = Int => T

  def fromIpcBytes[T: Codec](bytes: Array[Byte], allocator: BufferAllocator)[R](use: Iterator[T] => R): R = {
    val in = ByteArrayInputStream(bytes)
    val reader = ArrowStreamReader(in, allocator)
    Using.resource(reader)(reader => use(fromReader[T](reader)))
  }

  def fromVector[T: Codec](vector: FieldVector): Iterator[T] = {
    val deserializer = create(Codec[T], vector)
    (0 until vector.getValueCount).iterator.map(deserializer)
  }

  private def fromReader[T: Codec](reader: ArrowReader): Iterator[T] = {
    val root = reader.getVectorSchemaRoot()
    val deserializer = create(Codec[T], root)
    new AbstractIterator[T] {
      var index = 0
      var eos = false
      def hasNext: Boolean = {
        while (!eos && index >= root.getRowCount) {
          index = 0
          eos = !reader.loadNextBatch()
        }
        index < root.getRowCount
      }

      def next: T = {
        if !hasNext then Iterator.empty.next
        index += 1
        deserializer(index - 1)
      }
    }
  }

  private def support[T](vector: FieldVector)(supported: PartialFunction[FieldVector, Deserializer[T]]) =
    supported.applyOrElse(
      vector,
      _ => throw new IllegalArgumentException(s"Unsupported vector type: ${vector.getClass.getSimpleName}")
    )

  private def create[T](codec: Codec[T], root: VectorSchemaRoot): Deserializer[T] =
    codec match {
      case p @ Codec.Product(_, _, _) => product(p, root.getVector)
      case Codec.FromInjection(inj, to) =>
        val inner = create(to, root)
        index => inj.invert(inner(index))
      case codec => create[T](codec, root.getVector(0))
    }

  private def create[T](codec: Codec[T], vector: FieldVector): Deserializer[T] =
    codec match {
      case Codec.Boolean => support(vector) { case v: BitVector => index => v.get(index) > 0 }
      case Codec.Byte => support(vector) { case v: TinyIntVector => v.get }
      case Codec.Short => support(vector) { case v: SmallIntVector => index => v.get(index) }
      case Codec.Int => support(vector) { case v: IntVector => v.get }
      case Codec.Long => support(vector) { case v: BigIntVector => v.get }
      case Codec.Float => support(vector) { case v: Float4Vector => v.get }
      case Codec.Double => support(vector) { case v: Float8Vector => v.get }
      case Codec.String => support(vector) { case v: VarCharVector => index => Text.decode(v.get(index)) }
      case decimal @ Codec.Decimal(_, _) => support(vector) { case v: DecimalVector =>
          index =>
            val value = BigDecimal(v.getObject(index))
            Decimal(using decimal.valid)(value).getOrElse {
              throw new RuntimeException(s"Unable to represent $value as ${decimal.valid}")
            }
        }
      case Codec.Date => support(vector) { case v: DateDayVector => index => Date.fromDays(v.get(index)) }
      case Codec.TimestampMicros => support(vector) { case v: TimeStampMicroTZVector =>
          index => Timestamp.fromMicros(v.get(index))
        }
      case Codec.DurationMicros => support(vector) { case v: DurationVector =>
          index => Duration.fromMicros(DurationVector.get(v.getDataBuffer, index))
        }
      case Codec.Bytes => support(vector) { case v: VarBinaryVector => v.get }
      case Codec.Option(innerCodec @ Codec.Option(_)) => support(vector) { case v: StructVector =>
          val inner = create(innerCodec, v.getChild("value"))
          index => Option.when(!v.isNull(index))(inner(index))
        }
      case Codec.Option(innerCodec) =>
        val inner = create(innerCodec, vector)
        index => Option.when(!vector.isNull(index))(inner(index))
      case Codec.Seq(element: Codec[t]) => iterable(create(element, _), summon[Factory[t, Vector[t]]], vector)
      case map @ Codec.Map(_, _) => mapDeserializer(map, vector)
      case p @ Codec.Product(_, _, _) => support(vector) { case v: StructVector => product(p, v.getChild) }
      case Codec.FromInjection(inj, innerCodec) =>
        val inner = create(innerCodec, vector)
        index => inj.invert(inner(index))
    }

  private def iterable[T, C <: Iterable[T]](
      createElement: FieldVector => Deserializer[T],
      factory: Factory[T, C],
      vector: FieldVector
  ): Deserializer[C] =
    support(vector) { case v: ListVector =>
      val element = createElement(v.getDataVector)
      index => {
        val start = v.getElementStartIndex(index)
        val end = v.getElementEndIndex(index)
        factory.fromSpecific((start until end).iterator.map(element))
      }
    }

  private def mapDeserializer[K, V](map: Codec.Map[K, V], vector: FieldVector): Deserializer[Map[K, V]] =
    iterable(
      support(_) { case v: StructVector =>
        val key = create(map.key, v.getChild(MapVector.KEY_NAME))
        val value = create(map.value, v.getChild(MapVector.VALUE_NAME))
        index => (key(index), value(index))
      },
      summon,
      vector
    )

  private def product[T](codec: Codec.Product[T], getFieldVector: String => FieldVector): Deserializer[T] = {
    val fieldDeserializer = codec.fields.mapK([t] => f => create(f.codec, getFieldVector(f.name)))
    index => fieldDeserializer.construct([t] => _(index))
  }
}
