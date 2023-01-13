import input.ReaderFromFile
import org.scalatest.funsuite.AnyFunSuite

class DataTransformerSpec extends AnyFunSuite {
  type F[A] = Either[String, A]

  test("DataTransformer - should transform data correctly") {
    val reader = new ReaderFromFile[F]("data_small.txt")
    val dataTransformer = new DataTransformer[F](reader)

    val result = dataTransformer.readData

    assert(result.isRight)
    assert(result.map(_.get(0)) == Right(Some(List(1))))
    assert(result.map(_.get(18)) == Right(Some((1 to 19).toList)))
    assert(result.map(_.get(49)) == Right(Some((1 to 50).toList)))
    assert(result.map(_.get(50)) == Right(None))
  }

  test("DataTransformer - should fail on empty file") {
    val reader = new ReaderFromFile[F]("data_empty.txt")
    val dataTransformer = new DataTransformer[F](reader)

    val result = dataTransformer.readData

    assert(result == Left("File doesn't have content!"))
  }

  test("DataTransformer - should fail on malformed file") {
    val reader = new ReaderFromFile[F]("data_malformed.txt")
    val dataTransformer = new DataTransformer[F](reader)

    val result = dataTransformer.readData

    assert(result == Left("File doesn't contain only numbers!"))
  }
}
