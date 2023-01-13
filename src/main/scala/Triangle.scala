import cats.Applicative
import model.Node
import cats.implicits._

import scala.util.Try

trait Triangle[F[_]] {
  implicit protected val A: Applicative[F]

  def find(structure: Map[Int, List[Int]], startAtTriangleLevel: Int = 0): F[List[Int]] = {
    def findPath(currentTriangleLevel: Int, currentPath: List[Int], starterIndex: Int): List[Int] = {
      val nextTriangleLevel = currentTriangleLevel + 1
      structure
        .get(nextTriangleLevel)
        .flatMap { upperRow =>
          // analyse upper left
          val leftIndex = starterIndex - 1
          val upperLeft = Try(upperRow(leftIndex)).map(Node(_, leftIndex)).toOption
          // analyse upper right
          val rightIndex = starterIndex
          val upperRight = Try(upperRow(rightIndex)).map(Node(_, rightIndex)).toOption

          val upperPath =
            (upperLeft, upperRight) match {
              case (Some(leftNode), Some(rightNode)) => Option(findBetterPath(currentTriangleLevel, leftNode, rightNode))
              case (None, Some(rightNode)) => Option(rightNode)
              case (Some(leftNode), None) => Option(leftNode)
              case (None, None) => none[Node]
            }

          upperPath.map(node => findPath(nextTriangleLevel, currentPath :+ node.value, node.index))
        }.getOrElse(currentPath)
    }

    def findBetterPath(currentTriangleLevel: Int, leftNode: Node, rightNode: Node): Node = {
      val leftPath = findPath(currentTriangleLevel + 1, leftNode.value :: Nil, leftNode.index)
      val rightPath = findPath(currentTriangleLevel + 1, rightNode.value :: Nil, rightNode.index)

      if (leftPath.sum <= rightPath.sum) leftNode else rightNode
    }

    structure
      .get(startAtTriangleLevel)
      .map(row => row.zipWithIndex.map { case (value, index) => findPath(startAtTriangleLevel, value :: Nil, index) })
      .map(_.map(row => (row, row.sum)).minBy(_._2)._1)
      .getOrElse(List.empty[Int])
      .pure[F]
  }
}

final case class ShortestTrianglePath[F[_]]()(implicit protected val A: Applicative[F]) extends Triangle[F]