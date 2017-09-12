package fi.oph.scalaschema

object SchemaFactoryPerformanceTester extends App {
  val f = SchemaFactory.default

  Timing.rounds("create schema", 1000) { round =>
    f.createSchema[List[List[TestClass]]]
  }
}

object Timing {
  def rounds(name: String, rounds: Int)(f: Int => Any): Unit = {
    f(0)
    timed(s"${rounds} rounds ${name}") {
      (1 to rounds).foreach(f)
    }
  }

  def timed[R](blockname: String, thresholdMs: Int = 50)(block: => R): R = {
    val timer = new Timer(blockname, thresholdMs)
    timer.complete(block)
  }

  class Timer(blockname: String, thresholdMs: Int) {
    private val t0 = System.nanoTime()
    private var completed = false

    def complete[T](result: T): T = {
      if (!completed) {
        completed = true
        val t1 = System.nanoTime()
        val time: Long = (t1 - t0) / 1000000
        if (time >= thresholdMs) println(s"$blockname took $time ms")
      }
      result
    }
  }
}