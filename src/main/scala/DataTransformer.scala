import cats.implicits._
import input.Reader
import cats.MonadError

import scala.util.{Failure, Success, Try}

class DataTransformer[F[_]](reader: Reader[F])(implicit ME: MonadError[F, String]) {

  def readData: F[Map[Int, List[Int]]] = {

    def toInt(str: String): F[Int] = Try(str.toInt) match {
      case Failure(_) => ME.raiseError[Int]("File doesn't contain only numbers!")
      case Success(value) => value.pure[F]
    }

    def aggregateResults(line: String, modelData: ModelData): F[ModelData] =
      line
        .split(" ")
        .toList
        .traverse(toInt)
        .map(modelData.iteration -> _)
        .map(keyValue => modelData.copy(structure = modelData.structure + keyValue, iteration = modelData.iteration + 1))

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

final case class ModelData(structure: Map[Int, List[Int]], iteration: Int)
object ModelData {
  def empty: ModelData = ModelData(Map.empty[Int, List[Int]], 0)
}