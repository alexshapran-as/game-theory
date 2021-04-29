package game_theory

import game_theory.utils.{TreeNode, Utils}

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

object TestSphere2Cube {

    def main(args: Array[String]): Unit = {
        val pointsCount = 500
        val cubeDimensions = 40
        val epsilon = 1
        searchGame(pointsCount, cubeDimensions, epsilon)
    }

    private def planeIntersectionParameterHandler(denominator: Double, radius: Double): Double = {
        val parameter = radius / denominator
        if (parameter < 0.0) -parameter else parameter
    }

    private def planeIntersectionParameter(radius: Double, point: Point): Option[Double] = {
        (point.x - radius, point.y - radius, point.z - radius) match {
            case (0.0, 0.0, 0.0) => None
            case (denominatorX, 0.0, 0.0) => Some(planeIntersectionParameterHandler(denominatorX, radius))
            case (0.0, denominatorY, 0.0) => Some(planeIntersectionParameterHandler(denominatorY, radius))
            case (0.0, 0.0, denominatorZ) => Some(planeIntersectionParameterHandler(denominatorZ, radius))
            case (0.0, denominatorY, denominatorZ) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorY, radius),
                    planeIntersectionParameterHandler(denominatorZ, radius)
                ).min)
            case (denominatorX, 0.0, denominatorZ) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorX, radius),
                    planeIntersectionParameterHandler(denominatorZ, radius)
                ).min)
            case (denominatorX, denominatorY, 0.0) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorX, radius),
                    planeIntersectionParameterHandler(denominatorY, radius)
                ).min)
            case (denominatorX, denominatorY, denominatorZ) =>
                Some(List(
                    planeIntersectionParameterHandler(denominatorX, radius),
                    planeIntersectionParameterHandler(denominatorY, radius),
                    planeIntersectionParameterHandler(denominatorZ, radius)
                ).min)
        }
    }

    private def projectPoint2Cube(radius: Double, point: Point) = {
        planeIntersectionParameter(radius, point).map { parameter =>
            val x = (point.x - radius) * parameter + radius
            val y = (point.y - radius) * parameter + radius
            val z = (point.z - radius) * parameter + radius
            Point(x, y, z)
        }
    }

    private def generateFirstGamerStrategyOnCube(pointsCount: Int, cubeDimensions: Double, epsilon: Double): List[Point] = {
        val radius = cubeDimensions / 2.0
        val pointsIndices = (0 until pointsCount).toList
        val points = pointsIndices.map { pointIndex =>
            val theta = (2.0 * math.Pi * pointIndex) / ((1 + math.pow(5.0, 0.5)) / 2.0)
            val phi = math.acos(1.0 - (2.0 * pointIndex / pointsCount))

            val x = radius * Math.cos(theta) * Math.sin(phi) + radius
            val y = radius * Math.sin(theta) * Math.sin(phi) + radius
            val z = radius * Math.cos(phi) + radius

            Point(Utils.roundWithScale(x, 10), Utils.roundWithScale(y, 10), Utils.roundWithScale(z, 10))
        }
        points.filter(point => point.notIntersectWith(points.filter(_ != point), epsilon))
            .flatMap { point =>
                val projectedPoint = projectPoint2Cube(radius, point)
                projectedPoint.map { case Point(x, y, z) =>
                    Point(Utils.roundWithScale(x, 10), Utils.roundWithScale(y, 10), Utils.roundWithScale(z, 10))
                }
            }
    }

    def searchGame(pointsCount: Int, cubeDimensions: Double, epsilon: Double) = {
        val gamesCount = 120
        val gamesCountForOnePlane = gamesCount / 6
        val firstGamerPoints = generateFirstGamerStrategyOnCube(pointsCount, cubeDimensions, epsilon)
        val secondGamerPointsWithWins = {
            (0 until gamesCountForOnePlane).toList.map(_ => Point(0.0, cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions * scala.util.Random.nextDouble())) ++
                (0 until gamesCountForOnePlane).toList.map(_ => Point(cubeDimensions * scala.util.Random.nextDouble(), 0.0, cubeDimensions * scala.util.Random.nextDouble())) ++
                (0 until gamesCountForOnePlane).toList.map(_ => Point(cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions * scala.util.Random.nextDouble(), 0.0)) ++
                (0 until gamesCountForOnePlane).toList.map(_ => Point(cubeDimensions, cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions * scala.util.Random.nextDouble())) ++
                (0 until gamesCountForOnePlane).toList.map(_ => Point(cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions, cubeDimensions * scala.util.Random.nextDouble())) ++
                (0 until gamesCountForOnePlane).toList.map(_ => Point(cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions * scala.util.Random.nextDouble(), cubeDimensions))
        }.map { secondPoint =>
            if (secondPoint.notIntersectWith(firstGamerPoints, epsilon)) (secondPoint, true)
            else (secondPoint, false)
        }
        val gameCost = Utils.roundWithScale(
            secondGamerPointsWithWins.count(_._2 == false).toDouble / secondGamerPointsWithWins.length,
            3
        )
        Map(
            "cubeDimensions" -> cubeDimensions,
            "firstGamerPoints" -> firstGamerPoints.map(_.toMap),
            "secondGamerPoints" -> secondGamerPointsWithWins.map(_._1.toMap),
            "gameCost" -> gameCost,
            "gamesCount" -> secondGamerPointsWithWins.length,
            "epsilon" -> epsilon
        )
    }

}