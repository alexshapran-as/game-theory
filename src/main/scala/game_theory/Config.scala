package game_theory

import ch.qos.logback.classic.util.ContextInitializer

import java.io.FileInputStream
import java.util.Properties

object Config {
    private val p = new Properties

    def loadConfigs(): Unit = {
        p.load(new FileInputStream("server.properties"))
        System.setProperty("config.file", "application.conf")
        System.setProperty(ContextInitializer.CONFIG_FILE_PROPERTY, "logback.xml")
    }

    def getApiHost = getStringParam("api_host", "localhost")

    def getApiPort = getStringParam("api_port", "9101").toInt

    private def getStringParam(paramName: String, defaultValue: String = ""): String = {
        val paramValue = p.getProperty(paramName)
        if (paramValue == null) defaultValue else paramValue.trim
    }
}
