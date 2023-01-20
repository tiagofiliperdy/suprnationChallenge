import cats.Applicative
import model.Node
import cats.implicits._

trait Triangle[F[_]] {
  implicit protected val A: Applicative[F]

  def find(structure: List[List[Node]]): F[List[Int]] = {
    val allPaths =
      structure.foldLeft(List.empty[List[Node]]) { (acc, triangleRow) =>
        val solutions =
          if(acc.isEmpty) acc :+ triangleRow
          else acc

        val maybeNextRow: Option[List[Node]] = structure.drop(triangleRow.size).headOption
        val allSolutions = solutions.flatMap { solution =>
          triangleRow.find(_ == solution.head).map { node =>
            maybeNextRow.flatMap { nextRow =>
              val maybeLeft: Option[Node] = nextRow.find(_.index == node.index)
              val maybeRight: Option[Node] = nextRow.find(_.index == (node.index + 1))

              (maybeLeft, maybeRight)
                .mapN((left, right) => List(left +: solution, right +: solution))
            }.getOrElse(solutions) // end of triangle
          }.getOrElse(List.empty[List[Node]]) // logic error
        }

        allSolutions
          .groupBy(_.head.index)
          .values
          .map(_.minBy(_.map(_.value).sum))
          .toList
      }

    allPaths.map(_.map(_.value)).map(lists => (lists, lists.sum)).minBy(_._2)._1.pure[F]
  }
}

class ShortestTrianglePath[F[_]]()(implicit protected val A: Applicative[F]) extends Triangle[F]