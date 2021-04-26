package game_theory.api

import akka.http.scaladsl.server.{Route, StandardRoute}
import game_theory.utils.{Matrix, Utils}

import scala.annotation.tailrec

object MonotoneIterationMethodApi extends HttpRouteUtils {

    def getRoute: Route =
        respondWithJsonContentType {
            post("monotone_iteration_method") {
                extractPostRequestAsClass(read[Array[Array[String]]]) { matrixStrategy =>
                    Matrix.fromArray(matrixStrategy) match {
                        case Right(matrixStrategyConverted) =>
                            monotoneIterationMethodProcessor(matrixStrategyConverted)
                        case Left(errorMessage) =>
                            complete(getErrorResponse(500, errorMessage))
                    }
                }
            }
        }

    private def findOptimalSolution(subMatrix: Matrix): Option[Array[Double]] = {
        val minMaxOptimalSolution = Utils.minmaxSolution(subMatrix)
        if (minMaxOptimalSolution.isDefined) {
            minMaxOptimalSolution.map(_._1)
        } else {
            val m: Array[Array[Double]] = (subMatrix.transpose.matrix.map { row =>
                Array(-1.0) ++ row.map(x => -x)
            }) :+ Array.fill(subMatrix.getMatrixRowsCount + 1)(-1.0).patch(0, Array(0.0), 1)
            val simplexMatrix = Matrix(m)
            val variables = (0 until subMatrix.getMatrixRowsCount).toArray
            val basicVariables = (variables.length until variables.length + subMatrix.getMatrixColumnsCount).toArray
            Utils.simplex(simplexMatrix, basicVariables, variables).map {
                case (matrixSolution, basicVariablesSolution, variablesSolution) =>
                    val solutionsIndices = basicVariablesSolution.zipWithIndex.filter(x => variables.contains(x._1))
                    val factor = 1.0 / matrixSolution.matrix.last.head
                    val basicSolution: Array[(Double, Int)] = matrixSolution.transpose.matrix.head.zipWithIndex
                        .filter(x => solutionsIndices.map(_._2).contains(x._2))
                        .zip(solutionsIndices)
                        .map(x => (x._1._1, x._2._1))
                    val zeroSolution: Array[(Double, Int)] = variablesSolution
                        .filter(x => variables.contains(x)).map(solutionIndex => (0.0, solutionIndex))
                    (basicSolution ++ zeroSolution).sortBy(_._2).map(_._1 * factor)
            }
        }
    }

    @tailrec
    private def monotoneIterationMethodHandler(matrix: Matrix,
                                               step: Int,
                                               vector: Array[Double],
                                               indices: Array[Int],
                                               alpha: Option[Double],
                                               xN: Array[Double],
                                               strategyLabel: String,
                                               gameCost: Double = 0.0): (Double, Array[Double]) = {
        if (alpha.nonEmpty &&  Utils.isNumberZero(alpha.get)) {
            (Utils.roundWithScale(gameCost, 3), xN.map(x => Utils.roundWithScale(x, 3)))
        } else {
            val gameCostLoss = vector.min
            val updatedIndices = (indices ++ vector.zipWithIndex.filter(_._1 == gameCostLoss).map(_._2)).distinct.sorted
            val matrixTransposed = matrix.transpose
            val subMatrix = matrix.copy(matrixTransposed.matrix.zipWithIndex.filter { case (_, columnIndex) =>
                updatedIndices.contains(columnIndex)
            }.map(_._1)).transpose
            val optimalSolution = findOptimalSolution(subMatrix).getOrElse(throw new Exception("Нет решения"))
            val c = matrix.matrix.zip(optimalSolution).map { case (row, solution) =>
                row.map(_ * solution)
            }.foldLeft(Array.fill(matrix.getMatrixRowsCount)(0.0)) { case (row, sum) =>
                sum.zip(row).map(x => x._1 + x._2)
            }
            val updatedAlpha = Utils.roundWithScale(
                findOptimalSolution(Matrix(Array(vector, c))).getOrElse(throw new Exception("Нет решения")).head,
                3
            )
            val (cost, updatedXN) = if (updatedAlpha != 0.0) {
                (
                    gameCostLoss,
                    xN.map(x => x * (1 - updatedAlpha)).zip(optimalSolution.map(x => x * updatedAlpha)).map(x => x._1 + x._2)
                )
            } else {
                (
                    gameCost,
                    xN
                )
            }
            val updatedVector = vector.map(x => x * (1 - updatedAlpha)).zip(c.map(x => x * updatedAlpha)).map(x => x._1 + x._2)
            game_theory.logger.info(s"Gamer ${strategyLabel}: GameCost = ${cost}, strategies = (${updatedXN.mkString(",")}), step=${step}")
            monotoneIterationMethodHandler(matrix, step + 1, updatedVector, updatedIndices, Some(updatedAlpha), updatedXN, strategyLabel, cost)
        }
    }

    private def monotoneIterationMethodProcessor(matrixStrategy: Matrix): StandardRoute = {
        val step = 0
        val x0 = Array.fill(matrixStrategy.getMatrixColumnsCount)(0.0).patch(0, Array(1.0), 1)
        val c0 = matrixStrategy.matrix(0)
        val resX = monotoneIterationMethodHandler(matrixStrategy, step + 1, c0, Array(), None, x0, "A")
        val resY = monotoneIterationMethodHandler(matrixStrategy.transpose, step + 1, c0, Array(), None, x0, "B")
        complete(getOkResponse(Map(
            "gameCost" -> resX._1,
            "strategiesX" -> resX._2,
            "strategiesY" -> resY._2
        )))
    }

}
