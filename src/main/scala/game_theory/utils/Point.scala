package game_theory.utils

case class Point(x: Double, y: Double, z: Double) {
    def toMap = Map("x" -> x, "y" -> y, "z" -> z)

    def notIntersectWith(otherPoints: List[Point], epsilon: Double) = {
        otherPoints.foldLeft(true) { case (notIntersect, otherPoint) =>
            val distance = math.sqrt(
                math.pow(otherPoint.x - this.x,2.0) +
                    math.pow(otherPoint.y - this.y,2.0) +
                    math.pow(otherPoint.z - this.z,2.0)
            )
            if (distance <= 2.0 * epsilon) false else notIntersect
        }
    }
}