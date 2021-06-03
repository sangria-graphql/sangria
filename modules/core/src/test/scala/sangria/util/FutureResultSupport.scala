package sangria.util

import sangria.execution.{ErrorWithResolver, QueryAnalysisError}
import sangria.marshalling.ResultMarshallerForType

import language.postfixOps
import com.twitter.util.{Await, Future}

trait FutureResultSupport {
  implicit class FutureResult[T](f: Future[T]) {
    def await = Await.result(f)

    def awaitAndRecoverQueryAnalysis(implicit m: ResultMarshallerForType[T]): T =
      Await.result(recoverQueryAnalysis)

    def recoverQueryAnalysis(implicit m: ResultMarshallerForType[T]): Future[T] = f.handle {
      case analysisError: QueryAnalysisError =>
        analysisError.resolveError(m.marshaller).asInstanceOf[T]
    }

    def awaitAndRecoverQueryAnalysisScala(implicit ev: T =:= Any) =
      Await.result(recoverQueryAnalysisScala)

    def recoverQueryAnalysisScala(implicit ev: T =:= Any) = f.handle {
      case analysisError: ErrorWithResolver => analysisError.resolveError
    }
  }
}
