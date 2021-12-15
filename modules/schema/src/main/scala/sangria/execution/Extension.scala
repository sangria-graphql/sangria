package sangria.execution

import sangria.marshalling.InputUnmarshaller

case class Extension[In](data: In)(implicit val iu: InputUnmarshaller[In])
