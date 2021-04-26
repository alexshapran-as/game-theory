package game_theory.api

import akka.http.scaladsl.server.Route

object MainApi extends HttpRouteUtils {
    def getRoute: Route =
        logRequestResult(accessLogger)(handleExceptions(exceptionEventHandler) {
            pathPrefix("game_theory") {
                pathPrefix("api") {
                    get("web") {
                        getFromResource("web/game_theory.html")
                    } ~
                    pathPrefix("lab1") {
                        InverseMatrixAndBrownRobinsonMethodsApi.getRoute
                    } ~
                    pathPrefix("lab2") {
                        AnalyticalAndNumericalConvexConcaveGamesMethodsApi.getRoute
                    } ~
                    pathPrefix("lab3") {
                        NashParetoGamesMethodsApi.getRoute
                    } ~
                    pathPrefix("rk1") {
                        MonotoneIterationMethodApi.getRoute
                    } ~
                    pathPrefix("rk2") {
                        SearchGames.getRoute
                    }
                }
            } ~
            get("js" / Segment) { sourceName =>
                getFromResource(s"web/js/$sourceName")
            } ~
            get("css" / Segment) { sourceName =>
                getFromResource(s"web/css/$sourceName")
            }
        })
}
