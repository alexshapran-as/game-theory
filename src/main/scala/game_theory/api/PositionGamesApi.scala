package game_theory.api

import akka.http.scaladsl.server.Route
import game_theory.MSA
import game_theory.utils.Utils

object PositionGamesApi extends HttpRouteUtils {

    def getRoute: Route =
        respondWithJsonContentType {
            post("generate_tree") {
                extractPostRequestAsClass(read[MSA]) { request =>
                    val treeDepth = request("treeDepth").toString.toInt
                    val gamersCount = request("gamersCount").toString.toInt
                    val strategiesCount = request("strategiesCount").toString.split(",").map(_.trim.toInt).toList
                    val winMin = request("winMin").toString.toInt
                    val winMax = request("winMax").toString.toInt
                    Utils.generateTree(treeDepth, gamersCount, strategiesCount, winMin, winMax)
                    complete(getOkResponse(Utils.getTree.values.map(_.toMap)))
                }
            } ~
            post("compute_game") {
                extractPostRequestAsClass(read[MSA]) { request =>
                    val gamersCount = request("gamersCount").toString.toInt
                    val root = Utils.getTree.find(_._2.root).getOrElse(throw new Exception("root not found"))._2
                    Utils.computePositionGame(root, gamersCount)
                    val bestLeaves = Utils.getTree.values.filter { node =>
                        node.leaf && node.best
                    }.toList
                    Utils.setColorForTree(bestLeaves.map(node => (node, "")))
                    Utils.setGamer(gamersCount)
                    complete(getOkResponse(Utils.getTree.values.map(_.toMap)))
                }
            }
        }
}
