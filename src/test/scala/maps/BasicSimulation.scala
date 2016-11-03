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

  val feeder09 = zoomLevelFeeder(9)
  val feeder10 = zoomLevelFeeder(10)
  val feeder11 = zoomLevelFeeder(11)

  def zoomLevelFeeder(zoomLevel: Int): Feeder[Int] = {
    Iterator.continually(Map(
      ("x", random.nextInt(2 ^ zoomLevel - 1)),
      ("y", random.nextInt(2 ^ zoomLevel - 1))
    ))
  }

  val scn09 = browseAtLevel(10, feeder09)
  val scn10 = browseAtLevel(10, feeder10)
  val scn11 = browseAtLevel(11, feeder11)

  def browseAtLevel(zoomLevel: Int, zoomLevelFeeder: Feeder[Int]): ScenarioBuilder = {
    scenario("browse tiles at level " + zoomLevel) // A scenario is a chain of requests and pauses
      .repeat(10) {
      feed(zoomLevelFeeder)
        .exec(http("tile level " + zoomLevel)
          .get("/osm-intl/" + zoomLevel + "/${x}/${y}.png")
          .check(status.in(200 to 304)))
    }
  }

  setUp(
    scn09.inject(atOnceUsers(1)).protocols(httpConf),
    scn10.inject(atOnceUsers(1)).protocols(httpConf),
    scn11.inject(atOnceUsers(2)).protocols(httpConf)
  )
}
