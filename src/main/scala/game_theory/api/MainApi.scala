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
                    pathPrefix("lab4") {
                        PositionGamesApi.getRoute
                    } ~
                    pathPrefix("lab5") {
                        CooperativeGamesApi.getRoute
                    } ~
                    pathPrefix("rk1") {
                        MonotoneIterationMethodApi.getRoute
                    } ~
                    pathPrefix("rk2") {
                        SearchGamesApi.getRoute
                    }
                }
            } ~
            get("js" / Segments) { sourceNames =>
                getFromResource(s"web/js/${sourceNames.mkString("/")}")
            } ~
            get("css" / Segment) { sourceName =>
                getFromResource(s"web/css/$sourceName")
            }
        })
}
