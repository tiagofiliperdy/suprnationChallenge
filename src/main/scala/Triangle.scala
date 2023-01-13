import cats.MonadError
import model.{Node, TriangleModel}
import cats.implicits._

import scala.util.Try

trait Triangle[F[_]] {
  implicit protected val ME: MonadError[F, String]

  def find(structure: Map[Int, List[Int]]): F[List[Int]] = {
    def updateModel(node: Node, modelData: TriangleModel, nextTriangleLevel: Int): TriangleModel =
      modelData
        .copy(
          currentPath = node.value :: Nil,
          currentTriangleLevel = nextTriangleLevel,
          starterIndex = node.index
        )
    def recursionSafely(modelData: TriangleModel): F[TriangleModel]  = {
      ME.tailRecM(modelData) { currentData =>
        val nextTriangleLevel = currentData.currentTriangleLevel - 1
        structure.get(nextTriangleLevel)
          .traverse { upperRow =>
            // analyse upper left
            val leftIndex = currentData.starterIndex - 1
            val upperLeft = Try(upperRow(leftIndex)).map(Node(_, leftIndex)).toOption
            // analyse upper right
            val rightIndex = currentData.starterIndex
            val upperRight = Try(upperRow(rightIndex)).map(Node(_, rightIndex)).toOption

            (upperLeft, upperRight) match {
              case (Some(leftNode), Some(rightNode)) =>
                val leftPathModel = recursionSafely(
                  currentData
                    .copy(
                      currentPath = leftNode.value :: Nil,
                      currentTriangleLevel = nextTriangleLevel,
                      starterIndex = leftNode.index
                    )
                )
                val rightPathModel = recursionSafely(
                  currentData
                    .copy(
                      currentPath = rightNode.value :: Nil,
                      currentTriangleLevel = nextTriangleLevel,
                      starterIndex = rightNode.index
                    )
                )

                for {
                  left <- leftPathModel
                  right <- rightPathModel
                } yield {
                  val betterNode =
                    if (left.currentPath.sum <= right.currentPath.sum) leftNode else rightNode
                  currentData
                    .copy(
                      currentPath = currentData.currentPath :+ betterNode.value,
                      currentTriangleLevel = nextTriangleLevel,
                      starterIndex = betterNode.index
                    ).asLeft[TriangleModel]
                }
              case (None, Some(rightNode)) =>
                currentData
                  .copy(
                    currentPath = currentData.currentPath :+ rightNode.value,
                    currentTriangleLevel = nextTriangleLevel,
                    starterIndex = rightNode.index
                  ).asLeft[TriangleModel].pure[F]
              case (Some(leftNode), None) =>
                currentData
                  .copy(
                    currentPath = currentData.currentPath :+ leftNode.value,
                    currentTriangleLevel = nextTriangleLevel,
                    starterIndex = leftNode.index
                  ).asLeft[TriangleModel].pure[F]
              case (None, None) => currentData.asRight[TriangleModel].pure[F]
            }
          }.map(_.getOrElse(currentData.asRight[TriangleModel]))
      }
    }

    structure
      .get(structure.keys.size - 1)
      .traverse(row => row.zipWithIndex.traverse { case (value, index) =>
        val x =
          recursionSafely(TriangleModel(value :: Nil, structure.keys.size - 1, index))
        println(x)
        x
      })
      .map(_.map(_.map(model => (model.currentPath, model.currentPath.sum)).minBy(_._2)._1))
      .map(_.getOrElse(List.empty[Int]))
  }
}

class ShortestTrianglePath[F[_]]()(implicit protected val ME: MonadError[F, String]) extends Triangle[F]