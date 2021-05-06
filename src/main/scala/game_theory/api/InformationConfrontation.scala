package game_theory.api

import akka.http.scaladsl.server.{Route, StandardRoute}
import game_theory.MSA
import game_theory.utils.{Matrix, Utils}

object InformationConfrontation extends HttpRouteUtils {

    private var generatedTrustMatrix = Matrix(Array.empty[Array[Double]])

    def getRoute: Route =
        respondWithJsonContentType {
            post("gen_matrix") {
                extractPostRequestAsClass(read[MSA]) { request =>
                    val agentsCount = request("agentsCount").toString.toInt
                    generateTrustMatrixProcessor(agentsCount)
                }
            } ~
            post("compute_game") {
                extractPostRequestAsClass(read[MSA]) { request =>
                    val epsilon = Utils.convertExpressionToNumber(request("epsilon").toString)
                    val agentsMinOpinionInitial = request("agentsMinOpinionInitial").toString.toInt
                    val agentsMaxOpinionInitial = request("agentsMaxOpinionInitial").toString.toInt
                    val agentsInfluenceMinOpinionInitial = request("agentsInfluenceMinOpinionInitial").toString.toInt
                    val agentsInfluenceMaxOpinionInitial = request("agentsInfluenceMaxOpinionInitial").toString.toInt
                    val firstGamerAgentsInfluenceCount = request("firstGamerAgentsInfluenceCount").toString.toInt
                    val secondGamerAgentsInfluenceCount = request("secondGamerAgentsInfluenceCount").toString.toInt
                    computeInformationConfrontationProcessor(
                        epsilon,
                        agentsMinOpinionInitial, agentsMaxOpinionInitial,
                        agentsInfluenceMinOpinionInitial, agentsInfluenceMaxOpinionInitial,
                        firstGamerAgentsInfluenceCount, secondGamerAgentsInfluenceCount
                    )
                }
            }
        }

    private val stochasticRowSum = 1.0
    private val maxDigitsCount = 3
    private val minRandomValue = 1.0 / math.pow(10, maxDigitsCount)

    private def generateTrustMatrix(agentsCount: Int): Array[Array[Double]] = {
        generatedTrustMatrix = Matrix(Array.fill(agentsCount, agentsCount)(0.0).zipWithIndex.map { case (row, rowIndex) =>
            row.zipWithIndex.foldLeft((0.0, Array.empty[Double])) { case ((currentRowSum, newRow), (_, columnIndex)) =>
                val currentMaxRowSum = stochasticRowSum - currentRowSum
                val elem =
                    if (columnIndex == agentsCount - 1) currentMaxRowSum
                    else (math.random() * ((currentMaxRowSum - minRandomValue) / 2.0)) + minRandomValue
                (currentRowSum + elem, newRow :+ elem)
            }._2
        })
        generatedTrustMatrix.matrix
    }

    private def generateTrustMatrixProcessor(agentsCount: Int): StandardRoute = {
        val trustMatrix = generateTrustMatrix(agentsCount)
        complete(getOkResponse(Map("trustMatrix" -> trustMatrix)))
    }

    private def computeInformationConfrontationProcessor(
                                                            epsilon: Double,
                                                            agentsMinOpinionInitial: Int,
                                                            agentsMaxOpinionInitial: Int,
                                                            agentsInfluenceMinOpinionInitial: Int,
                                                            agentsInfluenceMaxOpinionInitial: Int,
                                                            firstGamerAgentsInfluenceCount: Int,
                                                            secondGamerAgentsInfluenceCount: Int
                                                        ): StandardRoute = {
        Utils.computeInformationConfrontation(
            generatedTrustMatrix,
            epsilon,
            agentsMinOpinionInitial, agentsMaxOpinionInitial,
            agentsInfluenceMinOpinionInitial, agentsInfluenceMaxOpinionInitial,
            firstGamerAgentsInfluenceCount, secondGamerAgentsInfluenceCount
        ) match {
            case Right(result) => complete(getOkResponse(result))
            case Left(errMsg) => complete(getErrorResponse(500, errMsg))
        }
    }

}
