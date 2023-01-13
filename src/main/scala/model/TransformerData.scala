package model

final case class TransformerData(structure: Map[Int, List[Int]], iteration: Int)

object TransformerData {
  def empty: TransformerData = TransformerData(Map.empty[Int, List[Int]], 0)
}