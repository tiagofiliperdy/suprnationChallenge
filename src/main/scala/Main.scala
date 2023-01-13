import input.ReaderFromFile

object Main extends App {
  type F[A] = Either[String, A]

  val reader = new ReaderFromFile[F]("data_small.txt")

  println(reader.readLine())
  println(reader.readLine())
  println(reader.readLine())
  println(reader.readLine())

}
