package freskog.aoc.utils

import scala.annotation.tailrec

object Zipper {
  case class Lens[A, B](get: A => B, set: (A, B) => A)

  case class Navigate[Tree, Node, Leaf](
    leftL: Lens[Node, Tree],
    rightL: Lens[Node, Tree],
    asNode: Tree => Option[Node],
    asLeaf: Tree => Option[Leaf]
  )
  def apply[Tree, Node, Leaf](t: Tree, nav: Navigate[Tree, Node, Leaf]): Zipper[Tree, Node, Leaf] =
    new Zipper[Tree, Node, Leaf] {
      override val navigate: Navigate[Tree, Node, Leaf] = nav
      override def parents: List[Node]                  = Nil
      override def current: Tree                        = t
      override def path: List[Path]                     = Nil
    }
}

sealed trait Path
object Path                    {
  case object Left  extends Path
  case object Right extends Path
}
trait Zipper[Tree, Node, Leaf] { self =>
  import Zipper._

  def newZipper(newParents: List[Node], newCurrent: Tree, newPath: List[Path]): Zipper[Tree, Node, Leaf] =
    new Zipper[Tree, Node, Leaf] {
      override val navigate: Navigate[Tree, Node, Leaf] = self.navigate
      override val parents: List[Node]                  = newParents
      override val current: Tree                        = newCurrent
      override val path: List[Path]                     = newPath
    }

  def navigate: Navigate[Tree, Node, Leaf]
  def parents: List[Node]
  def current: Tree
  def path: List[Path]

  def up(implicit ev: Node <:< Tree): Option[Zipper[Tree, Node, Leaf]] =
    parents.headOption.map(p => newZipper(parents.tail, p, path.tail))

  def right: Option[Zipper[Tree, Node, Leaf]] =
    navigate.asNode(current).map(n => newZipper(n :: parents, navigate.rightL.get(n), Path.Right :: path))

  def left: Option[Zipper[Tree, Node, Leaf]] =
    navigate.asNode(current).map(n => newZipper(n :: parents, navigate.leftL.get(n), Path.Left :: path))

  def isNode: Boolean =
    navigate.asNode(current).fold(false)(_ => true)

  def isLeaf: Boolean =
    !isNode

  def isRightChild: Boolean =
    path.headOption.contains(Path.Right)

  def isLeftChild: Boolean =
    path.headOption.contains(Path.Left)

  def isRoot: Boolean =
    parents.isEmpty

  def next(implicit ev: Node <:< Tree): Option[Zipper[Tree, Node, Leaf]] = {
    def findLeftAncestor(value: Zipper[Tree, Node, Leaf]): Option[Zipper[Tree, Node, Leaf]] =
      if (value.isLeftChild) Some(value)
      else value.up.flatMap(findLeftAncestor)
    left.orElse(findLeftAncestor(self).flatMap(_.up.flatMap(_.right)))
  }

  def prev(implicit ev: Node <:< Tree): Option[Zipper[Tree, Node, Leaf]] = {
    def findBottomRight(z: Zipper[Tree, Node, Leaf]): Option[Zipper[Tree, Node, Leaf]] =
      if (z.isLeaf) Some(z) else z.right.flatMap(findBottomRight)

    if (path.headOption.contains(Path.Right))
      self.up.flatMap(_.left.flatMap(findBottomRight))
    else up
  }

  def prevNode(implicit ev: Node <:< Tree): Option[Zipper[Tree, Node, Leaf]] =
    prevWhere(_.isNode)

  def nextNode(implicit ev: Node <:< Tree): Option[Zipper[Tree, Node, Leaf]] =
    nextWhere(_.isNode)

  def prevLeaf(implicit ev: Node <:< Tree): Option[Zipper[Tree, Node, Leaf]] =
    prevWhere(_.isLeaf)

  def nextLeaf(implicit ev: Node <:< Tree): Option[Zipper[Tree, Node, Leaf]] =
    nextWhere(_.isLeaf)

  @tailrec
  final def prevWhere(p: Zipper[Tree, Node, Leaf] => Boolean)(implicit ev: Node <:< Tree): Option[Zipper[Tree, Node, Leaf]] =
    prev match {
      case Some(zipper) =>
        if (p(zipper)) Some(zipper) else zipper.prevWhere(p)
      case None         => None
    }

  def root(implicit ev: Node <:< Tree): Zipper[Tree, Node, Leaf] =
    newZipper(Nil, parents.lastOption.fold[Tree](self.current)(ev(_)), Nil)

  @tailrec
  final def nextWhere(p: Zipper[Tree, Node, Leaf] => Boolean)(implicit ev: Node <:< Tree): Option[Zipper[Tree, Node, Leaf]] =
    next match {
      case Some(zipper) => if (p(zipper)) Some(zipper) else zipper.nextWhere(p)
      case None         => None
    }

  @tailrec
  final def transform(
    f: PartialFunction[Zipper[Tree, Node, Leaf], Zipper[Tree, Node, Leaf]]
  )(implicit ev: Node <:< Tree): Zipper[Tree, Node, Leaf] = {
    val transformed = f.lift(self).orElse(self.next)
    transformed match {
      case Some(nextToTransform) =>
        nextToTransform.transform(f)
      case None                  =>
        self.root
    }
  }

  def set(t: Tree)(implicit ev: Node <:< Tree): Zipper[Tree, Node, Leaf] =
    newZipper(
      parents
        .zip(path)
        .foldLeft(List.empty[Node]) {
          case (Nil, (parent, Path.Right)) => List(navigate.rightL.set(parent, t))
          case (Nil, (parent, Path.Left))  => List(navigate.leftL.set(parent, t))
          case (acc, (parent, Path.Left))  => navigate.leftL.set(parent, acc.head) :: acc
          case (acc, (parent, Path.Right)) => navigate.rightL.set(parent, acc.head) :: acc
        }
        .reverse,
      t,
      path
    )

}
