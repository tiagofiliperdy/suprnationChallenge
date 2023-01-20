
import model.Node
import org.scalatest.funsuite.AnyFunSuite

class TriangleSpec extends AnyFunSuite {
  type F[A] = Either[String, A]
  implicit val f = cats.implicits.catsStdInstancesForEither

  test("Shortest - Simple triangle") {
    val triangle: List[List[Node]] =
      List(
        List(Node(7, 0)),
        List(Node(6, 0), Node(3, 1)),
        List(Node(3, 0), Node(8, 1), Node(5, 2)),
        List(Node(11, 0), Node(2, 1), Node(10, 2), Node(9, 3))
      )

    val expectedPath = List(2, 3, 6, 7)
    val shortestPathTriangle = new ShortestTrianglePath()
    val resultPath = shortestPathTriangle.find(triangle)
    assert(resultPath == Right(expectedPath))
  }

  test("Shortest - Repeated values triangle") {
    val triangle: List[List[Node]] =
      List(
        List(Node(7,0)),
        List(Node(6, 0), Node(3, 1)),
        List(Node(3, 0), Node(3, 1), Node(5, 2)),
        List(Node(11, 0), Node(2, 1), Node(10, 2), Node(9, 3))
      )

    val expectedPath = List(2, 3, 3, 7)
    val shortestPathTriangle = new ShortestTrianglePath()
    val resultPath = shortestPathTriangle.find(triangle)
    assert(resultPath == Right(expectedPath))
  }

  test("Shortest - No obvious path triangle") {
    val triangle: List[List[Node]] =
      List(
        List(Node(1, 0)),
        List(Node(5, 0), Node(8, 1)),
        List(Node(2, 0), Node(3, 1), Node(2, 2)),
        List(Node(9, 0), Node(8, 1), Node(5, 2), Node(5, 3)),
        List(Node(5, 0), Node(1, 1), Node(3, 2), Node(3, 3), Node(5, 4)),
        List(Node(6, 0), Node(5, 1), Node(3, 2), Node(2, 3), Node(10, 4), Node(11, 5))
      )

    val expectedPath = List(2, 3, 5, 3, 5, 1)
    val shortestPathTriangle = new ShortestTrianglePath()
    val resultPath = shortestPathTriangle.find(triangle)
    assert(resultPath == Right(expectedPath))
  }
}