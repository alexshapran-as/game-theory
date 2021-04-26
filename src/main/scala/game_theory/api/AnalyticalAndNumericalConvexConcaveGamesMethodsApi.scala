package game_theory.api

import akka.http.scaladsl.server.{Route, StandardRoute}
import game_theory.MSA
import game_theory.utils.{GameCorePolynomial, Utils}

object AnalyticalAndNumericalConvexConcaveGamesMethodsApi extends HttpRouteUtils {

    def getRoute: Route =
        respondWithJsonContentType {
            post("analytical_method") {
                extractPostRequestAsClass(read[Array[MSA]]) { matrixStrategyKernelMSA =>
                    GameCorePolynomial.fromArray(matrixStrategyKernelMSA) match {
                        case Right(matrixStrategyKernel) =>
                            analyticalMethodProcessor(matrixStrategyKernel)
                        case Left(errorMessage) =>
                            complete(getErrorResponse(500, errorMessage))
                    }
                }
            } ~
            post("numerical_method") {
                extractPostRequestAsClass(read[Array[MSA]]) { matrixStrategyKernelMSA =>
                    GameCorePolynomial.fromArray(matrixStrategyKernelMSA) match {
                        case Right(matrixStrategyKernel) =>
                            numericalMethodProcessor(matrixStrategyKernel)
                        case Left(errorMessage) =>
                            complete(getErrorResponse(500, errorMessage))
                    }
                }
            }
        }

    def analyticalMethodProcessor(matrixStrategyKernel: GameCorePolynomial): StandardRoute =
        Utils.analyticalConvexConcaveGamesMethod(matrixStrategyKernel) match {
            case Right(result) => complete(getOkResponse(result))
            case Left(errorMessage) => complete(getErrorResponse(500, errorMessage))
        }

    def numericalMethodProcessor(matrixStrategyKernel: GameCorePolynomial): StandardRoute = {
        game_theory.logger.info("\n" + "*"*10 + " NUMERICAL CONVEX CONCAVE METHOD " + "*"*10 + "\n")
        complete(getOkResponse(Utils.numericalConvexConcaveGamesMethod(matrixStrategyKernel)))
    }

}
