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

    def recursive(line: String, currentData: Map[Int, List[Int]], iteration: Int = 0): F[Map[Int, List[Int]]] = {
      val structureUpdated =
        line
          .split(" ")
          .toList
          .traverse(toInt)
          .map(iteration -> _)
          .map(currentData + _)

      for {
        newStructure <- structureUpdated
        newLine <- reader.readLine()
        result <- newLine match {
          case Some(newLineContent) => recursive(newLineContent, newStructure, iteration + 1)
          case None => structureUpdated
        }
      } yield result
    }

    for {
      line <- reader.readLine()
      data <- line match {
        case Some(lineContent) => recursive(lineContent, Map.empty)
        case None => ME.raiseError("File doesn't have content!")
      }
    } yield data
  }

}
