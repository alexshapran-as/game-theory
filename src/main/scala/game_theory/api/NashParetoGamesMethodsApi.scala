package game_theory.api

import akka.http.scaladsl.server.{Route, StandardRoute}
import game_theory.MSA
import game_theory.utils.{Matrix, Utils}

object NashParetoGamesMethodsApi extends HttpRouteUtils {
    def getRoute: Route =
        respondWithJsonContentType {
            post("nash_pareto") {
                extractPostRequestAsClass(read[Array[Array[Array[String]]]]) { matricesStrategyMSA =>
                    var exceptions = List.empty[String]
                    val matricesStrategy = matricesStrategyMSA.map(Matrix.fromArray).flatMap {
                        case Right(matrixStrategy) =>
                            Some(matrixStrategy)
                        case Left(errMsg) =>
                            exceptions = exceptions :+ errMsg
                            None
                    }
                    if (exceptions.isEmpty) {
                        nashPoretoProcessor(matricesStrategy)
                    } else {
                        complete(getErrorResponse(500, exceptions.mkString("\n")))
                    }
                }
            } ~
                post("gen_matrix") {
                    extractPostRequestAsClass(read[MSA]) { dimensionSettings =>
                        val matrixRowsCount = dimensionSettings("matrixRowsCount").toString.toInt
                        val matrixColumnsCount = dimensionSettings("matrixColumnsCount").toString.toInt
                        generateBiMatrixProcessor(matrixRowsCount, matrixColumnsCount)
                    }
                }
        }

    private def nashPoretoProcessor(matricesStrategy: Array[Matrix]): StandardRoute = {
        val matricesStrategyAndIsTransposed =
            Array((matricesStrategy.head.copy(matricesStrategy.head.matrix.transpose), true)) ++ matricesStrategy.tail.map((_, false))
        Utils.findNashEquilibrium(matricesStrategyAndIsTransposed) match {
            case Right(nashEquilibrium) =>
                Utils.findOptimalPareto(matricesStrategy) match {
                    case Right(optimalPareto) => complete(getOkResponse(nashEquilibrium ++ optimalPareto))
                    case Left(errorMessage) => complete(getErrorResponse(500, errorMessage))
                }
            case Left(errorMessage) =>
                complete(getErrorResponse(500, errorMessage))
        }
    }

    private def generateBiMatrix(matrixRowsCount: Int, matrixColumnsCount: Int): Array[Array[Int]] = {
        Array.fill(matrixRowsCount, matrixColumnsCount)(0.0) map(_.map(_ => scala.util.Random.nextInt(101) - 50))
    }

    private def generateBiMatrixProcessor(matrixRowsCount: Int, matrixColumnsCount: Int): StandardRoute = {
        val firstGamerMatrix = generateBiMatrix(matrixRowsCount, matrixColumnsCount)
        val secondGamerMatrix = generateBiMatrix(matrixRowsCount, matrixColumnsCount)
        val result = firstGamerMatrix zip secondGamerMatrix map { case (first, second) =>
            first zip second map { case (firstElem, secondElem) =>
                Map("first" -> firstElem, "second" -> secondElem)
            }
        }
        complete(getOkResponse(
            Map(
                "biMatrix" -> result,
                "firstMatrix" -> firstGamerMatrix,
                "secondMatrix" -> secondGamerMatrix
            )
        ))
    }

}
