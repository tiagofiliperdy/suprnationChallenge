package input

import cats.Applicative
import cats.implicits._

import scala.io.{BufferedSource, Source, StdIn}

sealed trait Reader[F[_]] {
  def readLine(): F[Option[String]]
}

class InputReader[F[_]: Applicative]() extends Reader[F] {

  override def readLine(): F[Option[String]] =
    StdIn.readLine() match {
      case null => none[String].pure[F]
      case line => line.some.pure[F]
    }
}

class ReaderFromFile[F[_]: Applicative](fileName: String) extends Reader[F] {
  private val bufferedSource: BufferedSource = Source.fromFile(fileName)
  private val iterator: Iterator[String] = bufferedSource.getLines()

  override def readLine(): F[Option[String]] =
    if(iterator.hasNext) iterator.next().some.pure[F]
    else {
      //considered EOF
      bufferedSource.close()
      none[String].pure[F]
    }
}