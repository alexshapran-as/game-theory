package game_theory

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import game_theory.api.MainApi

object Boot {
    def main(args: Array[String]) {
        Config.loadConfigs()
        startAkkaHttp()
    }

    private def startAkkaHttp(): Unit = {
        val config = ConfigFactory.load()
        val actorSystem = ActorSystem.apply("GameTheoryActorSystem", config.getConfig("game_theory"))
        implicit val system: ActorSystem = actorSystem
        implicit val materializer: ActorMaterializer = ActorMaterializer()

        Http().bindAndHandle(MainApi.getRoute, Config.getApiHost, Config.getApiPort)
        game_theory.logger.info(s"Game Theory Web Page can be accessed by url: http://${Config.getApiHost}:${Config.getApiPort}/game_theory/api/web")
    }
}
