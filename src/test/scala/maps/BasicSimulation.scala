package computerdatabase

import io.gatling.core.Predef._
import io.gatling.core.structure.ScenarioBuilder
import io.gatling.http.Predef._

import scala.collection.immutable.Range.Inclusive
import scala.concurrent.duration._
import scala.math.{max, round}
import scala.util.Random

class BasicSimulation extends Simulation {

  val random = new Random()

  val httpConf = http
    .baseURL("http://localhost:6533") // Here is the root for all relative URLs
    .userAgentHeader("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:16.0) Gecko/20100101 Firefox/16.0")

  val zoomLevels = (0 to 18)

  /**
    * Returns random coordinates for a given zoom level
    */
  def zoomLevelFeeder(zoomLevel: Int): Feeder[Int] = {
    Iterator.continually(Map(
      ("x", random.nextInt(max(2 ^ zoomLevel - 1, 1))),
      ("y", random.nextInt(max(2 ^ zoomLevel - 1, 1)))
    ))
  }

  val scns: Map[Int, ScenarioBuilder] = zoomLevels.map(i => (i, browseAtLevel(i, zoomLevelFeeder(i)))).toMap

  def browseAtLevel(zoomLevel: Int, zoomLevelFeeder: Feeder[Int]): ScenarioBuilder = {
    scenario("browse tiles at level " + zoomLevel) // A scenario is a chain of requests and pauses
      .repeat(5) {
      feed(zoomLevelFeeder)
        .exec(http("tile level " + zoomLevel)
          .get("/osm-intl/" + zoomLevel + "/${x}/${y}.png")
          .check(status.in(200, 304)))
        .pause(5 seconds)
    }
  }

  /**
    * Distribution of requests over each zoom level. The numbers are a rough
    * estimate coming from the metrics we have from the live cluster.
    * @param zoomLevel
    * @param loadFactor the number of users is multiplied by load factor,
    *                   which allows to increse load uniformly over all zoom
    *                   levels
    * @return the number of users for this zoom level and load factor
    */
  def numUsersPerZoomLevel(zoomLevel: Int, loadFactor: Int): Int = {
    val baseUsers = zoomLevel match {
      case 0 => 0.00000106698
      case 1 => 0.00000548734
      case 2 => 0.00002990599
      case 3 => 0.00008772881
      case 4 => 0.00026311784
      case 5 => 0.00081466388
      case 6 => 0.00308889101
      case 7 => 0.00543417916
      case 8 => 0.01190560233
      case 9 => 0.02608982167
      case 10 => 0.03485735273
      case 11 => 0.06350297054
      case 12 => 0.10425845684
      case 13 => 0.12712961196
      case 14 => 0.17717455135
      case 15 => 0.16486020333
      case 16 => 0.13326213106
      case 17 => 0.07754095233
      case 18 => 0.06969330484
      case _ => 0
    }
    max(round(baseUsers * loadFactor).toInt, 1)
  }

  val populationBuilders = scns.map { case (zoomLevel, scn) =>
    val users = numUsersPerZoomLevel(zoomLevel, 1000)
    scn.inject(
      rampUsers(users) over (30 seconds)
    ).protocols(httpConf)
  }.toList

  setUp(populationBuilders)
}
