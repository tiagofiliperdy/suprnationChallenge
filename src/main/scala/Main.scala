
import cats.implicits._
import input.{InputReader, Reader, ReaderFromFile}

object Main extends App {
  import cats.implicits.catsStdInstancesForEither
  type F[A] = Either[String, A]

  val reader: Reader[F] = new ReaderFromFile[F]("data_smaller.txt")
  val dataTransformer = new DataTransformer[F](reader)

  for {
    dataTransformed <- dataTransformer.readData
    shortestPath <- new ShortestTrianglePath()(cats.implicits.catsStdInstancesForEither).find(dataTransformed)
  } yield {
    println(s"Shortest path -> ${shortestPath.mkString(" + ")} = ${shortestPath.sum}")
    shortestPath
  }.void
}
