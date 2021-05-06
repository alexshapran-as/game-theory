package game_theory.api

import akka.http.scaladsl.server.Route
import game_theory.MSA
import game_theory.utils.Utils

object CooperativeGamesApi extends HttpRouteUtils {

    def getRoute: Route =
        respondWithJsonContentType {
            post("generate_coalitions") {
                extractPostRequestAsClass(read[MSA]) { request =>
                    val gamersCount = request("gamersCount").toString.toInt
                    val coalitions = (0 to gamersCount).toList.flatMap { coalitionSize =>
                        (1 to gamersCount).toList.combinations(coalitionSize).toList
                    }
                    complete(getOkResponse(coalitions))
                }
            } ~
            post("compute_game") {
                extractPostRequestAsClass(read[MSA]) { request =>
                    val gamersCount = request("gamersCount").toString.toInt
                    val coalitionElementsPattern = "\\{(.*)\\}".r
                    val characteristicFunction = request("characteristicFunction").asInstanceOf[MSA].map {
                        case (coalition, characteristicFunctionValue) =>
                            coalitionElementsPattern
                                .findAllMatchIn(coalition)
                                .map(_.group(1).split(",").map(_.trim.toInt).toList).toList
                                .headOption.getOrElse(List.empty[Int]) ->
                            characteristicFunctionValue.toString.toDouble
                    }
                    val (superadditive, superadditiveCoalitions, notSuperadditiveCoalitions) = Utils.isSuperadditive(characteristicFunction)
                    val (convex, convexCoalitions, notConvexCoalitions) = Utils.isConvex(characteristicFunction)
                    val shapleyVector = Utils.computeShapleyVector(gamersCount, characteristicFunction).mapValues(Utils.roundWithScale(_, 2))
                    val (individualRationalization, badGamerIds) = Utils.isIndividualRationalizationConditionMet(shapleyVector, characteristicFunction)
                    val groupRationalization = Utils.isGroupRationalizationConditionMet(gamersCount, shapleyVector, characteristicFunction)
                    val result = Map(
                        "superadditive" -> Map(
                            "result" -> superadditive,
                            "superadditiveCoalitions" -> superadditiveCoalitions,
                            "notSuperadditiveCoalitions" -> notSuperadditiveCoalitions
                        ),
                        "convex" -> Map(
                            "result" -> convex,
                            "convexCoalitions" -> convexCoalitions,
                            "notConvexCoalitions" -> notConvexCoalitions
                        ),
                        "shapleyVector" -> shapleyVector,
                        "individualRationalization" -> Map(
                            "result" -> individualRationalization,
                            "badGamerIds" -> badGamerIds
                        ),
                        "groupRationalization" -> groupRationalization
                    )
                    complete(getOkResponse(result))
                }
            }
        }
}

