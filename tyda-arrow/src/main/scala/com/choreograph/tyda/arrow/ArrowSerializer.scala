package com.choreograph.tyda.arrow

import java.io.ByteArrayOutputStream

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
import org.apache.arrow.vector.ipc.ArrowStreamWriter
import org.apache.arrow.vector.util.Text

import com.choreograph.tyda.Codec

private[tyda] object ArrowSerializer {
  private type Serializer[T] = Function2[Int, T, Unit]

  def toIpcBytes[T: Codec](data: Seq[T], allocator: BufferAllocator): Array[Byte] =
    Using.resource(VectorSchemaRoot.create(CodecToArrowSchema.convert[T], allocator)) { root =>
      root.allocateNew()
      val serializer = create[T](Codec[T], root)
      root.setRowCount(data.size)
      data.zipWithIndex.foreach { case (value, index) => serializer(index, value) }
      // For collections like Seq, we only know the size of nested data vectors after serialization.
      // Arrow doesn't automatically update value counts during writes, so we call setRowCount again
      // to synchronize all vector metadata based on the actual serialized data.
      root.setRowCount(data.size)

      val out = new ByteArrayOutputStream()
      Using.resource(new ArrowStreamWriter(root, null, out)) { writer =>
        writer.start()
        writer.writeBatch()
        writer.end()
        out.toByteArray
      }
    }

  def toVector[T: Codec](it: Iterator[T], allocator: BufferAllocator): FieldVector = {
    val vector = CodecToArrowSchema.field("value", nullable = false, Codec[T]).createVector(allocator)
    val knownSize = it.knownSize
    if knownSize != -1 then vector.setInitialCapacity(knownSize)
    vector.allocateNew()
    val serializer = create(Codec[T], vector)
    val lastIndex = it
      .zipWithIndex
      .map { case (value, index) =>
        serializer(index, value)
        index
      }
      .fold(-1)((_, index) => index)
    vector.setValueCount(lastIndex + 1)
    vector
  }

  private def support[T](vector: FieldVector)(supported: PartialFunction[FieldVector, Serializer[T]]) =
    supported.applyOrElse(
      vector,
      _ => throw new IllegalArgumentException(s"Unsupported vector type: ${vector.getClass.getSimpleName}")
    )

  private def create[T](codec: Codec[T], root: VectorSchemaRoot): Serializer[T] =
    codec match {
      case p @ Codec.Product(_, _, _) => product(p, root.getVector)
      case Codec.FromInjection(inj, innerCodec) =>
        val inner = create(innerCodec, root)
        (index, value) => inner(index, inj(value))
      case _ => create[T](codec, root.getVector(0))
    }

  private def create[T](codec: Codec[T], vector: FieldVector): Serializer[T] =
    codec match {
      case Codec.Boolean => support(vector) { case v: BitVector =>
          (index, value) => v.setSafe(index, if (value) 1 else 0)
        }
      case Codec.Byte => support(vector) { case v: TinyIntVector => v.setSafe }
      case Codec.Short => support(vector) { case v: SmallIntVector => v.setSafe }
      case Codec.Int => support(vector) { case v: IntVector => v.setSafe }
      case Codec.Long => support(vector) { case v: BigIntVector => v.setSafe }
      case Codec.Float => support(vector) { case v: Float4Vector => v.setSafe }
      case Codec.Double => support(vector) { case v: Float8Vector => v.setSafe }
      case Codec.String => support(vector) { case v: VarCharVector =>
          (index, value) =>
            val bytes = Text.encode(value)
            v.setSafe(index, bytes, 0, bytes.limit())
        }
      case Codec.Decimal(_, _) => support(vector) { case v: DecimalVector =>
          (index, value) => v.setSafe(index, value.toBigDecimal.bigDecimal)
        }
      case Codec.Date => support(vector) { case v: DateDayVector =>
          (index, value) => v.setSafe(index, value.daysSinceEpoch)
        }
      case Codec.TimestampMicros => support(vector) { case v: TimeStampMicroTZVector =>
          (index, value) => v.setSafe(index, value.toMicros)
        }
      case Codec.DurationMicros => support(vector) { case v: DurationVector =>
          (index, value) => v.setSafe(index, value.toMicros)
        }
      case Codec.Bytes => support(vector) { case v: VarBinaryVector => v.setSafe }
      case Codec.Option(innerCodec @ Codec.Option(_)) => support(vector) { case v: StructVector =>
          val inner = create(innerCodec, v.getChild("value"))
          (index, value) =>
            value match {
              case Some(value) =>
                v.setIndexDefined(index)
                inner(index, value)
              case None => v.setNull(index)
            }
        }
      case Codec.Option(innerCodec) =>
        val inner = create(innerCodec, vector)
        (index, value) =>
          value match {
            case Some(v) => inner(index, v)
            case None => vector.setNull(index)
          }
      case Codec.Seq(element) => iterable(create(element, _), vector)
      case map @ Codec.Map(_, _) => mapSerializer(map, vector)
      case p @ Codec.Product(_, _, _) => support(vector) { case v: StructVector =>
          val serializeFields = product(p, v.getChild)
          (index, value) => {
            v.setIndexDefined(index)
            serializeFields(index, value)
          }
        }
      case Codec.FromInjection(inj, innerCodec) =>
        val inner = create(innerCodec, vector)
        (index, value) => inner(index, inj(value))
    }

  private def iterable[T, C <: Iterable[T]](
      createElement: FieldVector => Serializer[T],
      vector: FieldVector
  ): Serializer[C] =
    support(vector) { case v: ListVector =>
      val element = createElement(v.getDataVector)
      (index, value) => {
        val startIndex = v.startNewValue(index)
        val it = value.iterator
        var elementIndex = startIndex
        while (it.hasNext) {
          element(elementIndex, it.next())
          elementIndex += 1
        }
        v.endValue(index, elementIndex - startIndex)
      }
    }

  private def mapSerializer[K, V](map: Codec.Map[K, V], vector: FieldVector): Serializer[Map[K, V]] =
    iterable(
      support(_) { case v: StructVector =>
        val keySerializer = create(map.key, v.getChild(MapVector.KEY_NAME))
        val valueSerializer = create(map.value, v.getChild(MapVector.VALUE_NAME))
        (index, kv) => {
          keySerializer(index, kv._1)
          valueSerializer(index, kv._2)
        }
      },
      vector
    )

  private def product[T](codec: Codec.Product[T], getFieldVector: String => FieldVector): Serializer[T] = {
    val fieldSerializers = codec.fields.mapK([t] => f => create(f.codec, getFieldVector(f.name)))
    (index, value) =>
      fieldSerializers.foldLeft(value)(())([t] => (_, fieldSer, fieldValue) => fieldSer(index, fieldValue))
  }
}
