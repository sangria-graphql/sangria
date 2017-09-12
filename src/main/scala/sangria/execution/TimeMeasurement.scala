package sangria.execution

case class TimeMeasurement(startMs: Long, endMs: Long, durationNanos: Long)

object TimeMeasurement {
  def measure[T](fn: ⇒ T): (T, TimeMeasurement) = {
    val sw = StopWatch.start()
    val res = fn
    res → sw.stop
  }

  def empty = {
    val time = System.currentTimeMillis()

    TimeMeasurement(time, time, 0L)
  }
}

case class StopWatch(startTime: Long, startNanos: Long) {
  def stop: TimeMeasurement = {
    val endTime = System.currentTimeMillis()
    val endNanos = System.nanoTime()

    TimeMeasurement(startTime, endTime, endNanos - startNanos)
  }
}

object StopWatch {
  def start(): StopWatch = StopWatch(System.currentTimeMillis(), System.nanoTime())
}
