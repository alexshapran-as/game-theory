import org.slf4j.LoggerFactory

package object game_theory {
  type MSA = Map[String, Any]
  type MSS = Map[String, String]
  type Coalition = List[Int]

  val logger = LoggerFactory.getLogger(getClass)
}
