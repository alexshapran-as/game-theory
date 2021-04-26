package game_theory.api

import akka.http.scaladsl.model.{ContentTypes, HttpRequest}
import akka.http.scaladsl.server.directives.{FileInfo, LoggingMagnet}
import akka.http.scaladsl.server.{Directive, _}
import game_theory.MSA
import game_theory.utils.Utils
import org.json4s.native.JsonMethods.parseOpt
import org.json4s.native.Serialization.write

import java.io.File
import scala.util.{Failure, Success, Try}

trait HttpRouteUtils extends Directives {
    implicit val formats = org.json4s.DefaultFormats

    // -------------------------------------------------------------------------------------------------------------------
    // Методы для обработки GET и POST запросов
    // -------------------------------------------------------------------------------------------------------------------

    def get[L](pathRoute: PathMatcher[L]): Directive[L] = Directives.get & path(pathRoute)

    def post[L](pathRoute: PathMatcher[L]): Directive[L] = Directives.post & path(pathRoute)

    // -------------------------------------------------------------------------------------------------------------------
    // Вспомогательные методы
    // -------------------------------------------------------------------------------------------------------------------

    def respondWithJsonContentType: Directive0 =
        mapResponse(response => response.mapEntity(entity => entity.withContentType(ContentTypes.`application/json`)))

    def read[T: Manifest](json: String): T = {
        Try(org.json4s.native.Serialization.read[T](json))
            .recover {
                case ex: Exception =>
                    game_theory.logger.error(s"Read json as class - exception", ex)
                    throw ex
            }
            .get
    }

    def extractPostRequestAsClass[T](f: String => T): Directive[Tuple1[T]] =
        entity(as[String]) map { json =>
            game_theory.logger.info("POST request: " + json)
            f(json)
        }

    val accessLogger: LoggingMagnet[HttpRequest => RouteResult => Unit] = LoggingMagnet { log =>
        request => response => game_theory.logger.info("Http Incoming Request: " + request)
    }

    def exceptionEventHandler: ExceptionHandler = {
        ExceptionHandler.apply { case e: Throwable =>
            extractUri { uri =>
                respondWithJsonContentType {
                    game_theory.logger.error(s"Request to $uri could not be handled normally case of exception", e)
                    e.printStackTrace()
                    complete(getErrorResponse(500, e.getMessage))
                }
            }
        }
    }

    def getOkResponse: String = {
        makeJson(Map("success" -> true))
    }

    def getOkResponse(data: Any): String = {
        makeJson(Map("success" -> true, "result" -> data))
    }

    def getErrorResponse(code: Int, description: String): String = {
        makeJson(Map("success" -> false, "error" -> Map("code" -> code, "description" -> description)))
    }

    def makeJson(obj: Map[String, Any]): String = write(obj)

    def makeJson(arr: Iterable[Any]): String = write(arr)

    def protectedStoreUploadedFile(fieldName: String, destinationFunction: FileInfo => File)(processorFunction: (FileInfo, File) => Route) =
        Directives.storeUploadedFile(fieldName, destinationFunction) { case (metadata, file) =>
            Try(processorFunction(metadata, file)) match {
                case Success(routeComplete) =>
                    routeComplete
                case Failure(e) =>
                    val filePath = file.getAbsolutePath
                    Utils.deleteFileOrDir(filePath)
                    throw new Exception("Uploaded file " + filePath + " was deleted cause of exception:\n" + e.getMessage, e.getCause)
            }
        }

    def protectedStoreUploadedFiles(fieldName: String, destinationFunction: FileInfo => File)(processorFunction: Seq[(FileInfo, File)] => Route) =
        Directives.storeUploadedFiles(fieldName, destinationFunction) { files =>
            Try(processorFunction(files)) match {
                case Success(routeComplete) =>
                    routeComplete
                case Failure(e) =>
                    val filesPaths = files.map(_._2.getAbsolutePath)
                    Utils.deleteFileOrDir(filesPaths)
                    throw new Exception("Uploaded files " + filesPaths.mkString(",") + " was deleted cause of exception:\n" + e.getMessage, e.getCause)
            }
        }
}