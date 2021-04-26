import sbt._

object Resolvers {
  val typeSafeRepo = Classpaths.typesafeReleases

  def commonResolvers = Seq(typeSafeRepo)
}
