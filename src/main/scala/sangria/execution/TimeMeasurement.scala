package sangria.execution

case class TimeMeasurement(startMs: Long, endMs: Long, durationNanos: Long)

object TimeMeasurement {
  def measure[T](fn: ⇒ T): (T, TimeMeasurement) = {
    val startTime = System.currentTimeMillis()
    val start = System.nanoTime()

    val res = fn

    val end = System.nanoTime()
    val endTime = System.currentTimeMillis()

    res → TimeMeasurement(startTime, endTime, end - start)
  }

  def empty = {
    val time = System.currentTimeMillis()

    TimeMeasurement(time, time, 0L)
  }
}
