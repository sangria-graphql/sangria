package sangria.util

import sangria.execution.{ErrorWithResolver, QueryAnalysisError}
import sangria.marshalling.{ResultMarshallerForType, ResultMarshaller}

import language.postfixOps

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait FutureResultSupport {
  implicit class FutureResult[T](f: Future[T]) {
    def await = Await.result(f, 2 seconds)

    def awaitAndRecoverQueryAnalysis(implicit m: ResultMarshallerForType[T]): T = Await.result(recoverQueryAnalysis, 2 seconds)

    def recoverQueryAnalysis(implicit m: ResultMarshallerForType[T]): Future[T] = f.recover {
      case analysisError: QueryAnalysisError ⇒ analysisError.resolveError(m.marshaller).asInstanceOf[T]
    }

    def awaitAndRecoverQueryAnalysisScala(implicit ev: T =:= Any) = Await.result(recoverQueryAnalysisScala, 2 seconds)

    def recoverQueryAnalysisScala(implicit ev: T =:= Any) = f.recover {
      case analysisError: ErrorWithResolver ⇒ analysisError.resolveError
    }
  }

}
