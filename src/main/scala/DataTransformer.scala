import cats.implicits._
import input.Reader
import cats.MonadError
import model.Node

import scala.util.{Failure, Success, Try}

class DataTransformer[F[_]](reader: Reader[F])(implicit ME: MonadError[F, String]) {
  import DataTransformer._

  def readData: F[List[List[Node]]] = {

    def toInt(str: String): F[Int] = Try(str.toInt) match {
      case Failure(_) => ME.raiseError[Int]("File doesn't contain only numbers!")
      case Success(value) => value.pure[F]
    }

    def aggregateResults(line: String, modelData: ModelData): F[ModelData] =
      line
        .split(" ")
        .toList
        .traverse(toInt)
        .map(_.zipWithIndex.map {case (a, b) => Node(a, b) })
        .map(modelData.append)

    def safeRecursive(modelData: ModelData): F[ModelData] =
      ME.tailRecM(modelData) { currentData =>
        reader.readLine().flatMap {
          case Some(line) => aggregateResults(line, currentData).map(_.asLeft[ModelData])
          case None => currentData.asRight[ModelData].pure[F]
        }
      }

    for {
      line <- reader.readLine()
      data <- line match {
        case Some(lineContent) =>
          val initialModelData = ModelData.empty
          aggregateResults(lineContent, initialModelData).flatMap(safeRecursive)
        case None => ME.raiseError("File doesn't have content!")
      }
    } yield data.structure
  }

}

object DataTransformer {

  case class ModelData(structure: List[List[Node]]) {
    def append(row: List[Node]): ModelData = ModelData(structure :+ row)
  }

  object ModelData {
    def empty: ModelData = ModelData(List.empty[List[Node]])
  }

}