package sangria

import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._

case class Config(introspectionComplexity: Option[Double] = None)

object Config {

  import net.ceedubs.ficus.readers.ArbitraryTypeReader._

  def load(): Config = ConfigFactory.load().as[Config]("sangria")
}
