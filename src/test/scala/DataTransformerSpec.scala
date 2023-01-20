import input.ReaderFromFile
import org.scalatest.funsuite.AnyFunSuite

class DataTransformerSpec extends AnyFunSuite {
  type F[A] = Either[String, A]

  test("DataTransformer - should transform data correctly") {
    val reader = new ReaderFromFile[F]("data_small.txt")
    val dataTransformer = new DataTransformer[F](reader)

    val result = dataTransformer.readData

    assert(result.isRight)
    assert(result.map(_.head.map(_.value)) == Right(List(1)))
    assert(result.map(_.drop(18).head.map(_.value)) == Right((1 to 19).toList))
    assert(result.map(_.drop(49).head.map(_.value)) == Right((1 to 50).toList))
    assert(result.map(_.drop(50).headOption) == Right(None))
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

  test("DataTransformer - should be stack safe") {
    val reader = new ReaderFromFile[F]("data_big.txt")
    val dataTransformer = new DataTransformer[F](reader)

    val result = dataTransformer.readData

    assert(result.map(_.size) == Right(2000))
  }
}
