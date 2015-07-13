package sangria.execution

trait SangriaScheduler

object SangriaScheduler {
  implicit val stubScheduler = new SangriaScheduler {}
}
