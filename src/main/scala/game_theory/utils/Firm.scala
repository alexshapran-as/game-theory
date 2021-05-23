package game_theory.utils

import game_theory.MSA

case class Firm(
                   index: Int,
                   expense: Double,
                   initDemand: Double,
                   productsCosts: List[Double] = List(),
                   demands: List[Double] = List(),
                   benefits: List[Double] = List(),
                   cost: Option[Double] = None,
                   best: Boolean = false
               ) {
    def toMap: MSA = Map(
        "index" -> index,
        "expense" -> expense,
        "productsCosts" -> productsCosts,
        "demands" -> demands,
        "benefits" -> benefits,
        "cost" -> cost.getOrElse(""),
        "best" -> best
    )
}
