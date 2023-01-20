
import org.scalatest.funsuite.AnyFunSuite

class TriangleSpec extends AnyFunSuite {
  type F[A] = Either[String, A]
  implicit val f = cats.implicits.catsStdInstancesForEither

  test("Shortest - Simple triangle") {
    val triangle: Map[Int, List[Int]] =
      Map(
        0 ->        List(7),
        1 ->      List(6, 3),
        2 ->    List(3, 8, 5),
        3 -> List(11, 2, 10, 9)
      )

    val expectedPath = List(2, 3, 6, 7)
    val shortestPathTriangle = new ShortestTrianglePath()
//    val resultPath = shortestPathTriangle.find(triangle)
//    assert(resultPath == Right(expectedPath))
  }

  test("Shortest - Repeated values triangle") {
    val triangle: Map[Int, List[Int]] =
      Map(
        0 ->        List(7),
        1 ->      List(6, 3),
        2 ->    List(3, 3, 5),
        3 -> List(11, 2, 10, 9)
      )

    val expectedPath = List(2, 3, 3, 7)
    val shortestPathTriangle = new ShortestTrianglePath()
//    val resultPath = shortestPathTriangle.find(triangle)
//    assert(resultPath == Right(expectedPath))
  }

  test("Shortest - No obvious path triangle") {
    val triangle: Map[Int, List[Int]] =
      Map(
        0 ->              List(1),
        1 ->            List(5, 8),
        2 ->          List(2, 3, 2),
        3 ->        List(9, 8, 5, 5),
        4 ->      List(5, 1, 3, 3, 5),
        5 ->    List(6, 5, 3, 2, 10, 11)
      )

    val expectedPath = List(2, 3, 5, 3, 5, 1)
    val shortestPathTriangle = new ShortestTrianglePath()
//    val resultPath = shortestPathTriangle.find(triangle)
//    assert(resultPath == Right(expectedPath))
  }
}