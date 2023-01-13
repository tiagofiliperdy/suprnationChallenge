import cats.implicits._
import input.ReaderFromFile

object Main extends App {
  type F[A] = Either[String, A]

  val reader: ReaderFromFile[F] = new ReaderFromFile[F]("data_small.txt")
  val dataTransformer = new DataTransformer[F](reader)
}
