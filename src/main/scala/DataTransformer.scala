import cats.implicits._
import input.Reader
import cats.MonadError
import model.TransformerData

import scala.util.{Failure, Success, Try}

class DataTransformer[F[_]](reader: Reader[F])(implicit ME: MonadError[F, String]) {

  def readData: F[Map[Int, List[Int]]] = {

    def toInt(str: String): F[Int] = Try(str.toInt) match {
      case Failure(_) => ME.raiseError[Int]("File doesn't contain only numbers!")
      case Success(value) => value.pure[F]
    }

    def aggregateResults(line: String, modelData: TransformerData): F[TransformerData] =
      line
        .split(" ")
        .toList
        .traverse(toInt)
        .map(modelData.iteration -> _)
        .map(keyValue => modelData.copy(structure = modelData.structure + keyValue, iteration = modelData.iteration + 1))

    def safeRecursive(modelData: TransformerData): F[TransformerData] =
      ME.tailRecM(modelData) { currentData =>
        reader.readLine().flatMap {
          case Some(line) => aggregateResults(line, currentData).map(_.asLeft[TransformerData])
          case None => currentData.asRight[TransformerData].pure[F]
        }
      }

    for {
      line <- reader.readLine()
      data <- line match {
        case Some(lineContent) =>
          val initialModelData = TransformerData.empty
          aggregateResults(lineContent, initialModelData).flatMap(safeRecursive)
        case None => ME.raiseError("File doesn't have content!")
      }
    } yield data.structure
  }

}

