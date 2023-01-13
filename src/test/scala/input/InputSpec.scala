package input

import org.scalatest.funsuite.AnyFunSuite
import cats.implicits._

import scala.util.Random

class InputSpec extends AnyFunSuite {
  type F[A] = Either[String, A]

  test("ReaderFromFile - should read lines") {
    val reader = new ReaderFromFile[F]("data_small.txt")
    val randomInt = Random.between(1, 50)

    val allElements = (1 to randomInt).toList
    val results: List[F[Option[String]]] = allElements.map(_ => reader.readLine())
    val expectedReads =
      allElements
        .foldLeft(List.empty[String])((acc, elem) => acc ++ List((1 to elem).mkString(" ")))
        .map(str => Right(Some(str)))

    assert(expectedReads.size == randomInt)
    assert(results.size == randomInt)
    expectedReads
      .zip(results)
      .foreach { case (expected, result) => assert(expected == result)}
  }

}
