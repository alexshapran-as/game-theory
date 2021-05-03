package game_theory.api

import akka.http.scaladsl.server.Route
import game_theory.MSA
import game_theory.utils.Utils

object SearchGamesApi extends HttpRouteUtils {
    def getRoute: Route =
        respondWithJsonContentType {
            post("search_game") {
                extractPostRequestAsClass(read[MSA]) { request =>
                        val pointsCount = Utils.convertExpressionToNumber(request("pointsCount").toString).toInt
                        val cubeDimensions = Utils.convertExpressionToNumber(request("cubeDimensions").toString)
                        val epsilon = Utils.convertExpressionToNumber(request("epsilon").toString)
                        complete(getOkResponse(Utils.searchGame(pointsCount, cubeDimensions, epsilon)))
                }
            }
        }
}
