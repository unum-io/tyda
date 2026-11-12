package com.choreograph.tyda

import scala.reflect.ClassTag

import shapeless3.deriving.Ap
import shapeless3.deriving.Complete
import shapeless3.deriving.K0
import shapeless3.deriving.MapF
import shapeless3.deriving.Pure
import shapeless3.deriving.TailRecM

import com.choreograph.tyda.shapeless3extras.State
import com.choreograph.tyda.shapeless3extras.tupleInstances

/** A generic, type-safe API for traversing and transforming trees described by
  * higher-kinded generalized algebraic data types (GADTs).
  *
  * The `Base[_]` type parameter represents the root type of the tree (e.g.
  * `Expr[_]`), while `Node` represents either 1) a particular node type within
  * that tree (e.g. `Expr.Add[T]` or `Seq[T]` for some given T) or 2) a
  * Container which is not part of the tree.
  *
  * Traversal is controlled by the [[TreeApi.Control]] ADT, which allows:
  *   - Continuing traversal (`Continue`)
  *   - Skipping children or parent nodes (`Skip`)
  *   - Stopping traversal early (`Stop`)
  *
  * For example
  * ```scala
  * import com.choreograph.tyda.TreeApi
  * import com.choreograph.tyda.TreeApi.Control
  *
  * enum Expr[T] {
  *   case Literal(value: T)
  *   case Add(lhs: Expr[T], rhs: Expr[T])
  * }
  *
  * // Automatically derive TreeApi for Expr
  * given api[T]: TreeApi[Expr, Expr] = {
  *   given [T]: TreeApi[Expr.Literal[T], Expr] = TreeApi.leaf
  *   TreeApi.coproduct
  * }
  *
  * val expr: Expr[Int] = Expr.Add(Expr.Literal(1), Expr.Literal(2))
  *
  * // Count the number of Literal nodes
  * val count = api.fold(expr)(0)([t] =>
  *   (acc, node) =>
  *     node match {
  *       case Expr.Literal(_) => Continue(acc + 1)
  *       case _ => Continue(acc)
  *     }
  * )
  * ```
  */
private[tyda] sealed trait TreeApi[Node, Base[_]] {
  import TreeApi.{StopOrContinue, Control, Continue, Skip, Stop}

  final type FoldVisitor[Acc] = [t] => (Acc, TreeApi[t, Base], t) => Control[Acc]
  final type TransformVisitor[F[_]] = [t] => (TreeApi[t, Base], t) => F[t]

  /** Fold over the direct children of the node by visiting them with the
    * supplied visitor.
    */
  protected def foldVisitChildren[Acc](acc: Acc, value: Node, visitor: FoldVisitor[Acc]): Control[Acc]

  /** Fold the current node. For all cases but `Node[_] =:= `Base[_]` this
    * should be a noop and will just return a `Continue(acc)`.
    */
  protected def foldNode[Acc](acc: Acc, value: Node, f: [t] => (Acc, Base[t]) => Control[Acc]): Control[Acc]

  /** Map over the direct children of the node by visiting them with the
    * supplied visitor.
    */
  protected def mapVisitChildren[F[_]: {Pure, MapF, Ap}](value: Node, visitor: TransformVisitor[F]): F[Node]

  /** Map the current node. For all cases but `Node[_] =:= `Base[_]` this should
    * be a noop and will just return `Continue(value)`.
    */
  protected def mapNode[F[_]: Pure as pure](value: Node, f: [t] => Base[t] => F[Base[t]]): F[Node]

  /** Checks whether any node in the tree satisfies the given predicate.
    *
    * Traversal is done in pre-order and stopped as a matching node is found.
    */
  final def exists(value: Node, f: [t] => Base[t] => Boolean): Boolean =
    fold(value)(acc = false)([t] => (acc, base) => if f(base) then Stop(true) else Continue(acc))

  /** Checks whether all nodes in the tree satisfy the given predicate.
    *
    * Traversal is done in pre-order and stopped as a non-matching node is
    * found.
    */
  final def forall(value: Node, f: [t] => Base[t] => Boolean): Boolean = !exists(value, [t] => !f(_))

  /** Collects values from all nodes in the tree for which the given partial
    * function is defined.
    */
  final def collect[B](value: Node, f: [t] => Base[t] => Option[B]): Seq[B] =
    fold(value)(Vector.empty[B])([t] =>
      (acc, base) => f(base).map(b => Continue(acc :+ b)).getOrElse(Continue(acc))
    )

  final def count(value: Node, f: [t] => Base[t] => Boolean): Long =
    fold(value)(0L)([t] => (acc, base) => if f(base) then Continue(acc + 1) else Continue(acc))

  /** Fold over the entire tree pre-order traversal. */
  final def fold[Acc](value: Node)(acc: Acc)(f: [t] => (Acc, Base[t]) => Control[Acc]): Acc = {
    lazy val visitor: FoldVisitor[Acc] = [t] =>
      (acc, treeApi, value) =>
        treeApi.foldNode(acc, value, f) match {
          case stop @ Stop(_) => stop
          case Skip(acc) => Continue(acc)
          case Continue(acc) => treeApi.foldVisitChildren(acc, value, visitor)
        }
    visitor(acc, this, value).value
  }

  /** Fold over the direct children of a node.
    */
  final def foldChildren[Acc](value: Node)(acc: Acc)(f: [t] => (Acc, Base[t]) => Control[Acc]): Acc = {
    lazy val visitor: FoldVisitor[Acc] = [t] =>
      (acc, treeApi, value) =>
        if treeApi.isInstanceOf[TreeApi.Container[?, ?]] then treeApi.foldVisitChildren(acc, value, visitor)
        else treeApi.foldNode(acc, value, f)
    this.foldVisitChildren(acc, value, visitor).value
  }

  /** Transform all nodes using post-order traversal. */
  final def transformUp(value: Node, f: [t] => Base[t] => StopOrContinue[Base[t]]): Node = {
    lazy val visitor: TransformVisitor[Control] = [t] =>
      (treeApi, value) =>
        treeApi.mapVisitChildren(value, visitor) match {
          case Continue(value) => treeApi.mapNode[Control](value, f)
          case stop @ Stop(_) => stop
          case Skip(_) => unreachable("Should not be reached because f returns StopOrContinue")
        }
    visitor(this, value).value
  }

  /** Transform all nodes using pre-order traversal. */
  final def transformDown(value: Node, f: [t] => Base[t] => Control[Base[t]]): Node = {
    lazy val visitor: TransformVisitor[Control] = [t] =>
      (treeApi, value) =>
        treeApi.mapNode(value, f) match {
          case stop @ Stop(_) => stop
          case Skip(value) => Continue(value)
          case Continue(value) => treeApi.mapVisitChildren(value, visitor)
        }
    visitor(this, value).value
  }

  /** Transform all nodes using post-order traversal, accumulating state through
    * the traversal.
    */
  final def transformAccumulateUp[Acc](initial: Acc, value: Node)(
      f: [t] => (Acc, Base[t]) => StopOrContinue[(Acc, Base[t])]
  ): (Acc, Node) = {
    lazy val visitor: TransformVisitor[State.For[Control, Acc]] = [t] =>
      (treeApi, value) => {
        val visitChildren = treeApi.mapVisitChildren[State.For[Control, Acc]](value, visitor)
        (s0: Acc) =>
          visitChildren.run(s0) match {
            case Continue(s1, value) =>
              treeApi.mapNode[State.For[Control, Acc]](value, [t] => base => f(_, base)).run(s1)
            case stop @ Stop(_) => stop
            case Skip(_) => unreachable("Should not be reached because f returns StopOrContinue")
          }
      }
    visitor(this, value).run(initial).value
  }

  /** Transform all nodes using pre-order traversal, accumulating state through
    * the traversal.
    */
  final def transformAccumulateDown[Acc](initial: Acc, value: Node)(
      f: [t] => (Acc, Base[t]) => Control[(Acc, Base[t])]
  ): (Acc, Node) = {
    lazy val visitor: TransformVisitor[State.For[Control, Acc]] = [t] =>
      (treeApi, value) =>
        (s0: Acc) =>
          treeApi.mapNode[State.For[Control, Acc]](value, [t] => base => f(_, base)).run(s0) match {
            case stop @ Stop(_) => stop
            case Skip(s1, value) => Continue(s1, value)
            case Continue(s1, value) =>
              treeApi.mapVisitChildren[State.For[Control, Acc]](value, visitor).run(s1)
          }
    visitor(this, value).run(initial).value
  }
}

private object TreeApi {
  type For[Base[_]] = [Node] =>> TreeApi[Node, Base]

  /** Stop the recursion as soon as possible.
    *
    * For folds this is immediately after the current node. For transforms it
    * only after visiting all sibling nodes. This is due to implementation
    * limitations and might be improved in the future.
    */
  final case class Stop[+T](value: T)

  /** For pre-order traversal this will skip traversing into children nodes.
    *
    * For post-order traversal only Stop and Continue is supported, this is
    * ensured using the alias StopOrContinue. This is done because Skip does not
    * have a clear meaning in post-order traversal. When someone has a good use
    * case and example of what the behavior should be then it should be
    * implemented.
    */
  final case class Skip[+T](value: T)

  /** Continue the traversal.
    */
  final case class Continue[+T](value: T)

  type Control[+T] = Continue[T] | Skip[T] | Stop[T]
  type StopOrContinue[+T] = Continue[T] | Stop[T]

  extension [T](c: Control[T]) {
    def value: T =
      c match {
        case Stop(v) => v
        case Skip(v) => v
        case Continue(v) => v
      }

    def map[U](f: T => U): Control[U] =
      c match {
        case Stop(v) => Stop(f(v))
        case Skip(v) => Skip(f(v))
        case Continue(v) => Continue(f(v))
      }

    def flatMap[U](f: T => Control[U]): Control[U] =
      c match {
        case Stop(v) => Stop(f(v).value)
        case Skip(v) => f(v) match {
            case stop @ Stop(_) => stop
            case skip @ Skip(_) => skip
            case Continue(u) => Skip(u)
          }
        case Continue(v) => f(v)
      }
  }

  given pure: Pure[Control] = [a] => (a: a) => Continue(a)
  given map: MapF[Control] = [a, b] => (fa: Control[a], f: a => b) => fa.map(f)
  given ap: Ap[Control] = [a, b] => (ff: Control[a => b], fa: Control[a]) => ff.flatMap(f => fa.map(f))
  given tailRecM: TailRecM[Control] = { [a, b] => (a: a, f: a => Control[Either[a, b]]) =>
    @annotation.tailrec
    def loop(a: a, seenStop: Boolean, seenSkip: Boolean): Control[b] = {
      val next = f(a)
      val nextIsStop = seenStop || next.isInstanceOf[Stop[?]]
      val nextIsSkip = seenSkip || next.isInstanceOf[Skip[?]]
      next.value match {
        case Left(nextA) => loop(nextA, nextIsStop, nextIsSkip)
        case Right(b) => if nextIsStop then Stop(b) else if nextIsSkip then Skip(b) else Continue(b)
      }
    }
    loop(a, seenStop = false, seenSkip = false)
  }

  extension [F[_]: {Pure as pure, MapF as map, Ap as ap}, T](seq: Seq[F[T]]) {
    private def sequence: F[Seq[T]] =
      seq.foldRight(pure(Vector.empty[T]))((f, acc) => ap(map(f, (v: T) => (seq: Seq[T]) => v +: seq), acc))
  }

  extension [F[_]: {Pure as pure, MapF as map, Ap as ap}, T: ClassTag](array: IArray[F[T]]) {
    private def sequence: F[IArray[T]] = map(array.toSeq.sequence, IArray.from(_))
  }

  /** Trait for `Node` is not `Base[_]` and therefore is not a tree node itself
    * but only a container of other `Base` nodes.
    */
  sealed trait Container[Node, Base[_]] extends TreeApi[Node, Base] {
    final def foldNode[Acc](acc: Acc, value: Node, f: [t] => (Acc, Base[t]) => Control[Acc]): Control[Acc] =
      Continue(acc)
    final def mapNode[F[_]: Pure as pure](value: Node, f: [t] => Base[t] => F[Base[t]]): F[Node] = pure(value)
  }

  /** A container that has no children.
    */
  final case class Leaf[Node, Base[_]]() extends Container[Node, Base] {
    def foldVisitChildren[Acc](acc: Acc, value: Node, visitor: FoldVisitor[Acc]): Control[Acc] = Continue(acc)
    def mapVisitChildren[F[_]: {Pure as pure, MapF, Ap}](value: Node, visitor: TransformVisitor[F]): F[Node] =
      pure(value)
  }

  /** For some cases where the provided givens can derive a `TreeApi` this can
    * be used to specify that a node is a leaf that has no children. For example
    * for
    * ```
    * enum Expr[T] {
    *   case Literal(v: T)
    * }
    * ```
    * here `Expr.Literal` has no children that are of type `Expr[_]`, but the
    * derivation can not know that because of the value of type parameter T. So
    * the user must specify that this is a leaf node. By providing
    * ```
    * given TreeApi[Expr.Literal, Expr] = TreeApi.leaf
    * ```
    */
  def leaf[Node, Base[_]]: TreeApi[Node, Base] = Leaf()

  /** This makes it possible to automatically traverse `Tuple.Map[T, Node]` for some `T <: Tuple`.
    *
    * For example
    * ```
    * enum Expr[T] {
    *   case MakeTuple[T <: Tuple](values: Tuple.Map[Tuple, Expr]) extends Expr[T]
    * }
    */
  def mappedTuple[T <: Tuple, T2, Node[_], Base[_]](
      api: [t] => () => TreeApi[Node[t], Base]
  ): TreeApi[Tuple.Map[T, Node], Base] =
    new Container[Tuple.Map[T, Node], Base] {
      def foldVisitChildren[Acc](
          acc: Acc,
          value: Tuple.Map[T, Node],
          visitor: FoldVisitor[Acc]
      ): Control[Acc] =
        tupleInstances(value).foldLeft0(Continue(acc))([t] =>
          (controlAcc: Control[Acc], v) =>
            controlAcc match {
              case stop @ Stop(_) => stop
              case other => other.flatMap(visitor(_, api[t](), v))
            }
        )
      def mapVisitChildren[F[_]: {Pure, MapF as map, Ap}](
          value: Tuple.Map[T, Node],
          visitor: TransformVisitor[F]
      ): F[Tuple.Map[T, Node]] =
        // TYPE SAFETY: The each element of value is a subtype of Node
        val result = value.toIArray.map(v => visitor(api(), v.asInstanceOf)).sequence
        // TYPE SAFETY: The visitor does not change the element types.
        map(result, Tuple.fromIArray(_).asInstanceOf[Tuple.Map[T, Node]])
    }

  /** This makes it automatically to traverse GDATS contains seqs.
    *
    * For example
    * ```
    * enum Expr[T] {
    *   case MakeArray(values: Set[Expr[T])
    * }
    * ```
    */
  given seq[T, Base[_]](using api: TreeApi[T, Base]): TreeApi[Seq[T], Base] =
    new Container[Seq[T], Base] {
      def foldVisitChildren[Acc](acc: Acc, value: Seq[T], visitor: FoldVisitor[Acc]): Control[Acc] =
        value.foldLeft(Continue(acc)) { (controlAcc: Control[Acc], v) =>
          controlAcc match {
            case stop @ Stop(_) => stop
            case other => other.flatMap(visitor(_, api, v))
          }
        }

      def mapVisitChildren[F[_]: {Pure, MapF, Ap}](value: Seq[T], visitor: TransformVisitor[F]): F[Seq[T]] =
        value.map(v => visitor(api, v)).sequence
    }

  /** This makes it automatically to traverse GADTs that contain options.
    *
    * For example
    * ```
    * enum Expr[T] {
    *   case MaybeValue(value: Option[Expr[T]])
    * }
    * ```
    */
  given option[T: TreeApi.For[Base], Base[_]]: TreeApi[Option[T], Base] = coproductContainer

  /** This handles all concrete sub types of Node. For example
    * ```
    * enum Expr[T] {
    *   case Add(l: Expr[T], Expr[T])
    * }
    * ```
    * This would provide `TreeApi[Expr.Add, Expr]`.
    */
  given product[Node, Base[_]](using
      instances: K0.ProductInstances[TreeApi.For[Base], Node]
  ): TreeApi[Node, Base] =
    new Container[Node, Base] {
      def foldVisitChildren[Acc](acc: Acc, value: Node, visitor: FoldVisitor[Acc]): Control[Acc] =
        instances.foldLeft(value)(Continue(acc))([t] =>
          (controlAcc: Control[Acc], treeApi, v) =>
            controlAcc match {
              case stop @ Stop(_) => Complete(stop)
              case other => other.flatMap(visitor(_, treeApi, v))
            }
        )
      def mapVisitChildren[F[_]: {Pure as pure, MapF as map, Ap as ap}](
          value: Node,
          visitor: TransformVisitor[F]
      ): F[Node] = instances.traverse[F](value)(map)(pure)(ap)([t] => visitor(_, _))
    }

  /** Allows derivation for sum types that are not `Base[_]` but do contain
    * `Base[_]` values. For example
    * ```
    * enum Expr[T] {
    *   case Literal(v: T)
    * }
    * enum Dataset[T] {
    *   case FromSeq(value: Seq[T])
    *   case Filter(input: Dataset[T], p: Expr[Boolean])
    * }
    * ```
    * This provides `TreeApi[Dataset, Expr]` that allows traversing a `Dataset`
    * while visiting `Expr` nodes. The case where `Node[_] =:= Base[_]` is
    * handled by [[coproduct]] instead.
    */
  def coproductContainer[Node, Base[_]](using
      instances: K0.CoproductInstances[TreeApi.For[Base], Node]
  ): TreeApi[Node, Base] =
    new Container[Node, Base] {
      def foldVisitChildren[Acc](acc: Acc, value: Node, visitor: FoldVisitor[Acc]): Control[Acc] =
        instances.fold[Control[Acc]](value)([t <: Node] =>
          (treeApi, v) => treeApi.foldVisitChildren(acc, v, visitor)
        )
      def mapVisitChildren[F[_]: {Pure as pure, MapF as map, Ap as ap}](
          value: Node,
          visitor: TransformVisitor[F]
      ): F[Node] = instances.traverse[F](value)(map)(pure)(ap)([t] => _.mapVisitChildren(_, visitor))
    }

  /** Derives a TreeApi for sum type that is the same as the base type. This is
    * responsible for actually visiting the nodes in foldNode/mapNode apis.
    */
  def coproduct[T, Base[_]](using
      instances: K0.CoproductInstances[TreeApi.For[Base], Base[T]]
  ): TreeApi[Base[T], Base] =
    new TreeApi[Base[T], Base] {
      def foldVisitChildren[Acc](acc: Acc, value: Base[T], visitor: FoldVisitor[Acc]): Control[Acc] =
        instances.fold[Control[Acc]](value)([t <: Base[T]] =>
          (treeApi, v) => treeApi.foldVisitChildren(acc, v, visitor)
        )
      def foldNode[Acc](acc: Acc, value: Base[T], f: [t] => (Acc, Base[t]) => Control[Acc]): Control[Acc] =
        f(acc, value)
      def mapVisitChildren[F[_]: {Pure as pure, MapF as map, Ap as ap}](
          value: Base[T],
          visitor: TransformVisitor[F]
      ): F[Base[T]] = instances.traverse[F](value)(map)(pure)(ap)([t] => _.mapVisitChildren(_, visitor))
      def mapNode[F[_]: Pure](value: Base[T], f: [t] => Base[t] => F[Base[t]]): F[Base[T]] = f(value)
    }
}
