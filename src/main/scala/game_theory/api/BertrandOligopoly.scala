package game_theory.api

import akka.http.scaladsl.server.{Route, StandardRoute}
import game_theory.MSA
import game_theory.utils.{Firm, Utils}

import scala.annotation.tailrec

object BertrandOligopoly extends HttpRouteUtils {

    def getRoute: Route =
        respondWithJsonContentType {
            post("compute_game") {
                extractPostRequestAsClass(read[MSA]) { request =>
                    val initProductsCosts = request("initProductsCosts").asInstanceOf[MSA]
                        .map { case (firm, cost) => (firm.toInt, Utils.convertExpressionToNumber(cost.toString)) }
                        .toList.sortBy(_._1)
                    val firmsExpenses = request("firmsExpenses").asInstanceOf[MSA]
                        .map { case (firm, expense) => (firm.toInt, Utils.convertExpressionToNumber(expense.toString)) }
                    val initDemand = Utils.convertExpressionToNumber(request("initDemand").toString)
                    bertrandOligopolyProcessor(initProductsCosts, firmsExpenses, initDemand)
                }
            }
        }

    private val epsilon = 0.01
    private val zeroEpsilon = 0.05

    private val demand = (firm: Firm, firms: List[Firm]) => {
        val firstFirmCost = firm.productsCosts.last
        val secondFirmCost = firms.filter(_.index != firm.index).minBy(_.productsCosts.last).productsCosts.last
        if (firstFirmCost > secondFirmCost) {
            0.0
        } else if (firstFirmCost < secondFirmCost) {
            firm.initDemand - firstFirmCost
        } else {
            (firm.initDemand - firstFirmCost) / 2.0
        }
    }

    private val benefit = (firm: Firm, firms: List[Firm]) =>
        demand(firm, firms) * (firm.productsCosts.last - firm.expense)

    private val cost = (firm: Firm, firms: List[Firm]) => {
        val newCost = firms.filter(_.index != firm.index).minBy(_.productsCosts.last).productsCosts.last
        if (math.abs(firm.expense - newCost - epsilon) <= zeroEpsilon) firm.expense
        else newCost - epsilon
    }

    @tailrec
    private def bertrandOligopolyProcessorHandler(firms: List[Firm]): (Int, List[Firm]) =
        firms.filter(_.cost.isEmpty) match {
            case List() =>
                val bestFound = firms.exists(_.best)
                val iterations = firms.maxBy(_.productsCosts.length).productsCosts.length
                val updatedFirms = firms map { firm =>
                    val newFirm = if (firm.productsCosts.length < iterations) {
                        firm.copy(
                            productsCosts = firm.productsCosts :+ firm.productsCosts.last,
                            benefits = firm.benefits :+ benefit(firm, firms),
                            demands = firm.demands :+ demand(firm, firms)
                        )
                    } else {
                        firm
                    }
                    if (bestFound) {
                        newFirm.copy(
                            productsCosts = newFirm.productsCosts :+ newFirm.productsCosts.last,
                            benefits = newFirm.benefits :+ benefit(newFirm, firms),
                            demands = newFirm.demands :+ demand(newFirm, firms)
                        )
                    } else {
                        newFirm
                    }
                }
                (updatedFirms.maxBy(_.productsCosts.length).productsCosts.length, updatedFirms)
            case firmsInGame =>
                val firmThatGo = firmsInGame.maxBy(_.productsCosts.last)
                val updatedCost = cost(firmThatGo, firms)
                val firmWithNewCost = firmThatGo.copy(
                    productsCosts = firmThatGo.productsCosts :+ updatedCost,
                    cost = if (updatedCost == firmThatGo.expense || firmsInGame.length == 1) Some(updatedCost) else None,
                    best = if (updatedCost != firmThatGo.expense && firmsInGame.length == 1) true else false
                )
                val updatedFirm = firmWithNewCost.copy(
                    benefits = firmWithNewCost.benefits :+ benefit(firmWithNewCost, firms),
                    demands = firmWithNewCost.demands :+ demand(firmWithNewCost, firms)
                )
                val updatedFirms = firms map { firm => if (firm.index == updatedFirm.index) updatedFirm else firm }

                bertrandOligopolyProcessorHandler(updatedFirms)
        }

    private def bertrandOligopolyProcessor(
                                              initProductsCosts: List[(Int, Double)],
                                              firmsExpenses: Map[Int, Double],
                                              initDemand: Double
                                          ): StandardRoute = {
        val firms = initProductsCosts zip firmsExpenses map { case ((firmIndex, cost), (_, expense)) =>
            Firm(firmIndex, expense, initDemand, List(cost))
        }
        val initFirms = firms map { firm =>
            firm.copy(
                demands = List(demand(firm, firms)),
                benefits = List(benefit(firm, firms))
            )
        }
        val (iterations, updatedFirms) = bertrandOligopolyProcessorHandler(initFirms)
        complete(getOkResponse(Map("iterations" -> iterations, "firms" -> updatedFirms.map(_.toMap))))
    }
}
