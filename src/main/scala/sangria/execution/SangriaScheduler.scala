package sangria.execution

trait SangriaScheduler


// todo I think it actually not really needed
object SangriaScheduler {
  implicit val stubScheduler = new SangriaScheduler {}
}
