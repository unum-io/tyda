package com.choreograph.tyda.table

import scala.collection.Factory
import scala.collection.mutable
import scala.deriving.Mirror

import shapeless3.deriving.K0

import com.choreograph.tyda.Date
import com.choreograph.tyda.TypeName

/** Allows modification and collection of sources and sinks inside GADTs.
  *
  * The name Traversal is based on
  * https://github.com/optics-dev/Monocle/blob/master/docs/src/main/mdoc/optics/traversal.md
  */
trait SourceSinkTraversal[Args] {
  def mapSources(f: SourceSinkTraversal.SourceMapper): Args => Args
  def mapSinks(f: SourceSinkTraversal.SinkMapper): Args => Args

  final def sources(args: Args): List[SourceAndModelName[?]] = {
    val buffer = mutable.ArrayBuffer.empty[SourceAndModelName[?]]
    val _ = mapSources { [m, p <: Partitioner] => (s: Source[m, p], name: TypeName[m]) =>
      buffer.append(SourceAndModelName(s, name))
      s
    }(args)
    buffer.toList
  }

  final def sinks(args: Args): List[SinkAndModelName[?]] = {
    val buffer = mutable.ArrayBuffer.empty[SinkAndModelName[?]]
    val _ = mapSinks { [m, p <: Partitioner] => (s: Sink[m, p], name: TypeName[m]) =>
      buffer.append(SinkAndModelName(s, name))
      s
    }(args)
    buffer.toList
  }
}

object SourceSinkTraversal {
  type SourceMapper = [m, p <: Partitioner] => (Source[m, p], TypeName[m]) => Source[m, p]
  type SinkMapper = [m, p <: Partitioner] => (Sink[m, p], TypeName[m]) => Sink[m, p]

  def apply[Args: SourceSinkTraversal]: SourceSinkTraversal[Args] = summon

  /** Implementation for types that do not contain sources or sinks. */
  private final class Empty[Args] extends SourceSinkTraversal[Args] {
    def mapSources(f: SourceMapper): Args => Args = identity
    def mapSinks(f: SinkMapper): Args => Args = identity
  }

  def empty[Args]: SourceSinkTraversal[Args] = new Empty[Args]

  given SourceSinkTraversal[String] = Empty[String]
  given SourceSinkTraversal[Boolean] = Empty[Boolean]
  given SourceSinkTraversal[Float] = Empty[Float]
  given SourceSinkTraversal[Int] = Empty[Int]
  given SourceSinkTraversal[Long] = Empty[Long]
  given SourceSinkTraversal[Date] = Empty[Date]

  given sink[M: TypeName, P <: Partitioner]: SourceSinkTraversal[Sink[M, P]] with {
    def mapSources(f: SourceMapper): Sink[M, P] => Sink[M, P] = identity
    def mapSinks(f: SinkMapper): Sink[M, P] => Sink[M, P] = (s: Sink[M, P]) => f(s, TypeName[M])
  }

  given source[M: TypeName, P <: Partitioner]: SourceSinkTraversal[Source[M, P]] with {
    def mapSources(f: SourceMapper): Source[M, P] => Source[M, P] = (s: Source[M, P]) => f(s, TypeName[M])
    def mapSinks(f: SinkMapper): Source[M, P] => Source[M, P] = identity
  }

  given option[T: SourceSinkTraversal as inner]: SourceSinkTraversal[Option[T]] with {
    def mapSources(f: SourceMapper): Option[T] => Option[T] = (arg: Option[T]) => arg.map(inner.mapSources(f))
    def mapSinks(f: SinkMapper): Option[T] => Option[T] = (arg: Option[T]) => arg.map(inner.mapSinks(f))
  }

  given iterable[T: SourceSinkTraversal as inner, C <: Iterable[T]](using
      factory: Factory[T, C]
  ): SourceSinkTraversal[C] with {
    def mapSources(f: SourceMapper): C => C = (args: C) => factory.fromSpecific(args.map(inner.mapSources(f)))
    def mapSinks(f: SinkMapper): C => C = (args: C) => factory.fromSpecific(args.map(inner.mapSinks(f)))
  }

  given product[T](using inst: K0.ProductInstances[SourceSinkTraversal, T]): SourceSinkTraversal[T] with {
    def mapSources(f: SourceMapper): T => T =
      args => inst.map(args)([t] => (traversal, value) => traversal.mapSources(f)(value))
    def mapSinks(f: SinkMapper): T => T =
      args => inst.map(args)([t] => (traversal, value) => traversal.mapSinks(f)(value))
  }

  def sum[T](using inst: K0.CoproductInstances[SourceSinkTraversal, T]): SourceSinkTraversal[T] =
    new SourceSinkTraversal[T] {
      def mapSources(f: SourceMapper): T => T =
        args => inst.fold(args)([t <: T] => (traversal, value) => traversal.mapSources(f)(value))
      def mapSinks(f: SinkMapper): T => T =
        args => inst.fold(args)([t <: T] => (traversal, value) => traversal.mapSinks(f)(value))
    }

  inline def derived[T](using m: Mirror.Of[T]): SourceSinkTraversal[T] =
    inline m match {
      case given Mirror.SumOf[T] => sum
      case given Mirror.ProductOf[T] => product
    }
}
