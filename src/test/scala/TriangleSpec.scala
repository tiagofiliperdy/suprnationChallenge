
import org.scalatest.funsuite.AnyFunSuite

class TriangleSpec extends AnyFunSuite {
  type F[A] = Either[String, A]
  implicit val f = cats.implicits.catsStdInstancesForEither

  test("Shortest - Simple triangle") {
    val triangle: Map[Int, List[Int]] =
      Map(
        3 ->        List(7),
        2 ->      List(6, 3),
        1 ->    List(3, 8, 5),
        0 -> List(11, 2, 10, 9)
      )

    val expectedPath = List(2, 3, 6, 7)
    val shortestPathTriangle = new ShortestTrianglePath()
    val resultPath = shortestPathTriangle.find(triangle)
    assert(resultPath == Right(expectedPath))
  }

  test("Shortest - Repeated values triangle") {
    val triangle: Map[Int, List[Int]] =
      Map(
        3 ->        List(7),
        2 ->      List(6, 3),
        1 ->    List(3, 3, 5),
        0 -> List(11, 2, 10, 9)
      )

    val expectedPath = List(2, 3, 3, 7)
    val shortestPathTriangle = new ShortestTrianglePath()
    val resultPath = shortestPathTriangle.find(triangle)
    assert(resultPath == Right(expectedPath))
  }

  test("Shortest - No obvious path triangle") {
    val triangle: Map[Int, List[Int]] =
      Map(
        5 ->              List(1),
        4 ->            List(5, 8),
        3 ->          List(2, 3, 2),
        2 ->        List(9, 8, 5, 5),
        1 ->      List(5, 1, 3, 3, 5),
        0 ->    List(6, 5, 3, 2, 10, 11)
      )

    val expectedPath = List(2, 3, 5, 3, 5, 1)
    val shortestPathTriangle = new ShortestTrianglePath()
    val resultPath = shortestPathTriangle.find(triangle)
    assert(resultPath == Right(expectedPath))
  }

  test("Shortest - Start On level 1 of triangle") {
    val triangle: Map[Int, List[Int]] =
      Map(
        5 ->              List(1),
        4 ->            List(5, 8),
        3 ->          List(2, 3, 2),
        2 ->        List(9, 8, 5, 5),
        1 ->      List(5, 2, 3, 3, 5),
        0 ->    List(6, 5, 3, 2, 10, 11)
      )

    val expectedPath = List(3, 5, 3, 5, 1)
    val shortestPathTriangle = new ShortestTrianglePath()
    val resultPath = shortestPathTriangle.find(triangle, startAtTriangleLevel = 1)
    assert(resultPath == Right(expectedPath))
  }
}