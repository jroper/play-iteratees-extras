package play.extras.iteratees

import scala.io.Source
import play.api.libs.iteratee._
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object JsonPerformanceTest extends App {

  val events = Source.fromInputStream(JsonPerformanceTest.getClass.getResourceAsStream("/events.json")).mkString.toCharArray.grouped(8192).toSeq

  def test = {
    Await.result(Enumerator.enumerate(events) |>>> JsonIteratees.jsSimpleArray, Duration.Inf)
  }
  def time(block : => Unit): Long = {
    val start = System.currentTimeMillis()
    block
    System.currentTimeMillis() - start
  }

  println("Warmup: " + time((1 to 10).foreach(_ => test)) + "ms")
  println("Run 1: " + time((1 to 10).foreach(_ => test)) + "ms")
  println("Run 2: " + time((1 to 10).foreach(_ => test)) + "ms")

}
