package game_theory.api

import akka.http.scaladsl.server.Route
import game_theory.MSA
import game_theory.utils.{TreeNode, Utils}

object PositionGamesApi extends HttpRouteUtils {

    private val availableColors = List("#fa9292", "#4d7fa0", "#dbbf79", "#d179db", "#79dbbf")
    private val availableGamers = Map(0 -> "A", 1 -> "B", 2 -> "C", 3 -> "D", 4 -> "E")

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
                    Utils.setColorForTree(bestLeaves.map(node => (node, "")), availableColors)
                    Utils.setGamer(gamersCount, availableGamers)

                    complete(getOkResponse(Utils.getTree.values.map(_.toMap)))
                }
            } ~
            post("compute_nature_game") {
                extractPostRequestAsClass(read[MSA]) { request =>
                    val treeDepth = request("treeDepth").toString.toInt
                    val gamersCount = request("gamersCount").toString.toInt
                    val strategiesCount = request("strategiesCount").toString
                        .split(",").map(_.trim.toInt).zipWithIndex
                        .map { case (count, gamerId) => gamerId -> count }.toMap
                    val winMin = request("winMin").toString.toInt
                    val winMax = request("winMax").toString.toInt

                    val newAvailableColors = availableColors.tails.toList
                    val trees = (0 until gamersCount).map { gamerId =>
                        val newStrategiesCount = strategiesCount(gamerId) :: (strategiesCount - gamerId).values.toList
                        Utils.generateTree(treeDepth, gamersCount, newStrategiesCount, winMin, winMax)
                        val root = Utils.getTree.find(_._2.root).getOrElse(throw new Exception("root not found"))._2
                        Utils.computePositionGame(root, gamersCount)
                        val bestLeaves = Utils.getTree.values.filter { node =>
                            node.leaf && node.best
                        }.toList
                        Utils.setColorForTree(bestLeaves.map(node => (node, "")), newAvailableColors(gamerId))
                        val newAvailableGamers = (availableGamers - gamerId).values.zipWithIndex
                            .map { case (gamer, id) => id + 1 -> gamer }.toMap + (0 -> availableGamers(gamerId))
                        Utils.setGamer(gamersCount, newAvailableGamers)
                        Utils.getTree
                    }

                    var natureRoot = TreeNode(Utils.getId, List(), None, List(), 0, leaf = false, root = true, best = true)
                    val natureTree = trees.map { tree =>
                        val root = tree.find(_._2.root).getOrElse(throw new Exception("root not found"))._2
                        natureRoot = natureRoot.copy(
                            children = natureRoot.children :+ root.id,
                            wins = natureRoot.wins ++ root.wins
                        )
                        tree + (root.id -> root.copy(parent = Some(natureRoot.id), root = false))
                    }.foldLeft(Map.empty[String, TreeNode]) { case (currentNatureTree, tree) =>
                        currentNatureTree ++ tree
                    } + (natureRoot.id -> natureRoot)

                    complete(getOkResponse(natureTree.values.map(_.toMap)))
                }
            }
        }
}
