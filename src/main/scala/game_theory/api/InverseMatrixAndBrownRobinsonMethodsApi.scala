package game_theory.api

import akka.http.scaladsl.server.{Route, StandardRoute}
import game_theory.utils.{Matrix, Utils}

import java.io.File

object InverseMatrixAndBrownRobinsonMethodsApi extends HttpRouteUtils {

    def getRoute: Route =
        respondWithJsonContentType {
            post("inverse_matrix_method") {
                extractPostRequestAsClass(read[Array[Array[String]]]) { matrixStrategy =>
                    Matrix.fromArray(matrixStrategy) match {
                        case Right(matrixStrategyConverted) =>
                            inverseMatrixMethodProcessor(matrixStrategyConverted)
                        case Left(errorMessage) =>
                            complete(getErrorResponse(500, errorMessage))
                    }
                }
            } ~
            post("brown_robinson_method") {
                extractPostRequestAsClass(read[Array[Array[String]]]) { matrixStrategy =>
                   Matrix.fromArray(matrixStrategy) match {
                        case Right(matrixStrategyConverted) =>
                            brownRobinsonMethodProcessor(matrixStrategyConverted)
                        case Left(errorMessage) =>
                            complete(getErrorResponse(500, errorMessage))
                    }
                }
            } ~
            pathPrefix("by_file") {
                post("inverse_matrix_method") {
                    protectedStoreUploadedFile(fieldName = "file", destinationFunction = fInfo => new File(fInfo.fileName)) {
                        case (_, matrixStrategyFile) =>
                            val matrixStrategy = Utils.readXls(matrixStrategyFile.getAbsolutePath)
                            Utils.deleteFileOrDir(matrixStrategyFile.getAbsolutePath)
                           Matrix.fromArray(matrixStrategy) match {
                                case Right(matrixStrategyConverted) =>
                                    inverseMatrixMethodProcessor(matrixStrategyConverted)
                                case Left(errorMessage) =>
                                    complete(getErrorResponse(500, errorMessage))
                            }
                    }
                } ~
                post("brown_robinson_method") {
                    protectedStoreUploadedFile(fieldName = "file", destinationFunction = fInfo => new File(fInfo.fileName)) {
                        case (_, matrixStrategyFile) =>
                            val matrixStrategy = Utils.readXls(matrixStrategyFile.getAbsolutePath)
                            Utils.deleteFileOrDir(matrixStrategyFile.getAbsolutePath)
                           Matrix.fromArray(matrixStrategy) match {
                                case Right(matrixStrategyConverted) =>
                                    brownRobinsonMethodProcessor(matrixStrategyConverted)
                                case Left(errorMessage) =>
                                    complete(getErrorResponse(500, errorMessage))
                            }
                    }
                }
            }
        }

    private def inverseMatrixMethodProcessor(matrixStrategy: Matrix): StandardRoute =
        Utils.analyticalInverseMatrixMethod(matrixStrategy) match {
            case Right(result) => complete(getOkResponse(result))
            case Left(errorMessage) => complete(getErrorResponse(500, errorMessage))
        }

    private def brownRobinsonMethodProcessor(matrixStrategy: Matrix): StandardRoute =
        Utils.brownRobinson(matrixStrategy) match {
            case Right(result) => complete(getOkResponse(result))
            case Left(errorMessage) => complete(getErrorResponse(500, errorMessage))
        }

}
