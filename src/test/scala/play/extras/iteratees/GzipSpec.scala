package play.extras.iteratees

import play.api.libs.iteratee.{Iteratee, Enumeratee, Enumerator}
import org.specs2.mutable.Specification

object GzipSpec extends Specification {

  "gzip" should {

    "gzip simple input" in {
      test("Hello world")
    }

    "gzip multiple inputs" in {
      test("Hello", " ", "world")
    }

    "gzip large repeating input" in {
      val bigString = Seq.fill(1000)("Hello world").mkString("")
      test(bigString)
    }

    "gzip multiple large repeating inputs" in {
      val bigString = Seq.fill(1000)("Hello world").mkString("")
      test(bigString, bigString, bigString)
    }

    "gzip large random input" in {
      test(scala.util.Random.nextString(10000))
    }

    "gzip multiple large random inputs" in {
      test(scala.util.Random.nextString(10000),
        scala.util.Random.nextString(10000),
        scala.util.Random.nextString(10000))
    }

    def test(values: String*) {
      import java.io._
      import java.util.zip._

      val valuesBytes = values.map(_.getBytes("utf-8"))

      val result: Array[Byte] = (Enumerator(valuesBytes:_*) &> Gzip.gzip() |>> consumeBytes).flatMap(_.run).await.get

      // Check that it exactly matches the gzip output stream
      val baos = new ByteArrayOutputStream()
      val os = new GZIPOutputStream(baos)
      valuesBytes.foreach(bytes => os.write(bytes))
      os.close()
      val baosResult = baos.toByteArray

      for (i <- 0 until result.length) {
        if (result(i) != baosResult(i)) {
          result(i) must_== baosResult(i)
        }
      }

      result must_== baos.toByteArray

      // Check that it can be unzipped
      val bais = new ByteArrayInputStream(result)
      val is = new GZIPInputStream(bais)
      val check: Array[Byte] = (Enumerator.fromStream(is) |>> consumeBytes).flatMap(_.run).await.get
      values.mkString("") must_== new String(check, "utf-8")
    }
  }

  "gunzip" should {

    "gunzip simple input" in {
      test("Hello world")
    }

    "gunzip simple input in small chunks" in {
      test("Hello world", gzip = Gzip.gzip(5))
    }

    "gunzip large repeating input" in {
      test(Seq.fill(1000)("Hello world").mkString(""))
    }

    "gunzip large repeating input in small chunks" in {
      test(Seq.fill(1000)("Hello world").mkString(""), gzip = Gzip.gzip(10))
    }

    "gunzip large random input" in {
      test(scala.util.Random.nextString(10000))
    }

    "gunzip large random input in small chunks" in {
      test(scala.util.Random.nextString(10000), gzip = Gzip.gzip(10))
    }

    def test(value: String, gunzip: Enumeratee[Array[Byte], Array[Byte]] = Gzip.gunzip(), gzip: Enumeratee[Array[Byte], Array[Byte]] = Gzip.gzip()) {
      val future = (Enumerator(value.getBytes("utf-8")) &> gzip &> gunzip |>> consumeBytes).flatMap(_.run)
      val result = new String(future.await.get, "utf-8")
      result must_== value
    }
  }

  val consumeBytes = Iteratee.fold[Array[Byte], Array[Byte]](Array.empty) { (collected, value) =>
    collected ++ value
  }
}
