package computerdatabase

import io.gatling.core.Predef._
import io.gatling.core.structure.ScenarioBuilder
import io.gatling.http.Predef._

import scala.concurrent.duration._
import scala.util.Random

class BasicSimulation extends Simulation {

  val random = new Random()

  val httpConf = http
    .baseURL("http://localhost:6533") // Here is the root for all relative URLs
    .userAgentHeader("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:16.0) Gecko/20100101 Firefox/16.0")

  val zoomLevels = (9 to 11)

  def zoomLevelFeeder(zoomLevel: Int): Feeder[Int] = {
    Iterator.continually(Map(
      ("x", random.nextInt(2 ^ zoomLevel - 1)),
      ("y", random.nextInt(2 ^ zoomLevel - 1))
    ))
  }

  val scns: Map[Int, ScenarioBuilder] = zoomLevels.map(i => (i, browseAtLevel(i, zoomLevelFeeder(i)))).toMap

  def browseAtLevel(zoomLevel: Int, zoomLevelFeeder: Feeder[Int]): ScenarioBuilder = {
    scenario("browse tiles at level " + zoomLevel) // A scenario is a chain of requests and pauses
      .repeat(10) {
      feed(zoomLevelFeeder)
        .exec(http("tile level " + zoomLevel)
          .get("/osm-intl/" + zoomLevel + "/${x}/${y}.png")
          .check(status.in(200 to 304)))
        .pause(5 seconds)
    }
  }

  def numUsersPerZoomLevel(zoomLevel: Int) = 2

  val populationBuilders = scns.map { case (zoomLevel, scn) =>
    val users = numUsersPerZoomLevel(zoomLevel)
    scn.inject(atOnceUsers(users)).protocols(httpConf)
  }.toList
  setUp(populationBuilders)
}
