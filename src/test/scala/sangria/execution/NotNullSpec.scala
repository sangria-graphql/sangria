package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.AwaitSupport

import scala.concurrent.Future
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class NotNullSpec extends WordSpec with Matchers with AwaitSupport {
  trait TestSubject {
    def sync: Option[String]
    def nonNullSync: String
    def promise: Future[Option[String]]
    def nonNullPromise: Future[String]
    def nest: Option[TestSubject]
    def nonNullNest: TestSubject
    def promiseNest: Future[Option[TestSubject]]
    def nonNullPromiseNest: Future[TestSubject]
  }

  class ThrowingSubject extends TestSubject {
    def sync = throw new IllegalStateException("sync")
    def nonNullSync = throw new IllegalStateException("nonNullSync")
    def promise = Future.failed(new IllegalStateException("promise"))
    def nonNullPromise = Future.failed(new IllegalStateException("nonNullPromise"))
    def nest = Some(new ThrowingSubject)
    def nonNullNest = new ThrowingSubject
    def promiseNest = Future.successful(Some(new ThrowingSubject))
    def nonNullPromiseNest = Future.successful(new ThrowingSubject)
  }

  class NullingSubject extends TestSubject {
    def sync = None
    def nonNullSync = null
    def promise = Future.successful(None)
    def nonNullPromise = Future.successful(null)
    def nest = Some(new NullingSubject)
    def nonNullNest = new NullingSubject
    def promiseNest = Future.successful(Some(new NullingSubject))
    def nonNullPromiseNest = Future.successful(new NullingSubject)
  }

  val DataType: ObjectType[Unit, TestSubject] = ObjectType("DataType", () => List[Field[Unit, TestSubject]](
    Field("sync", OptionType(StringType), resolve = _.value.sync),
    Field("nonNullSync", StringType, resolve = _.value.nonNullSync),
    Field("promise", OptionType(StringType), resolve = _.value.promise),
    Field("nonNullPromise", StringType, resolve = _.value.nonNullPromise),
    Field("nest", OptionType(DataType), resolve = _.value.nest),
    Field("nonNullNest", DataType, resolve = _.value.nonNullNest),
    Field("promiseNest", OptionType(DataType), resolve = _.value.promiseNest),
    Field("nonNullPromiseNest", DataType, resolve = _.value.nonNullPromiseNest)))

  val TestSchema = Schema(DataType)

  def check(data: TestSubject, query: String, expected: Any) = {
    val Success(doc) = QueryParser.parse(query)

    val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node] = {
      case (m, e: IllegalStateException) => m.mapNode(Seq("message" -> m.stringNode(e.getMessage)))
    }

    Executor(TestSchema, data, exceptionHandler = exceptionHandler).execute(doc).await should be (expected)
  }

  def check(data: TestSubject, query: String, expectedData: Map[String, Any], expectedErrors: List[Map[String, Any]]) = {
    val Success(doc) = QueryParser.parse(query)

    val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node] = {
      case (m, e: IllegalStateException) => m.mapNode(Seq("message" -> m.stringNode(e.getMessage)))
    }

    val result = Executor(TestSchema, data, exceptionHandler = exceptionHandler).execute(doc).await.asInstanceOf[Map[String, Any]]

    result("data") should be (expectedData)

    val errors = result.get("errors").getOrElse(Nil).asInstanceOf[List[Any]]

    errors should have size(expectedErrors.size)

    expectedErrors foreach (expected => errors should contain (expected))
  }

  "Execute: handles non-nullable types" should {
    "nulls a nullable field that throws synchronously" in check(
      new ThrowingSubject,
      """
        query Q {
          sync
        }
      """,
      Map(
        "data" -> Map("sync" -> null),
        "errors" -> List(Map("message" -> "sync", "field" -> "sync", "locations" -> List(Map("line" -> 3, "column" -> 11))))))

    "nulls a nullable field that throws in a promise" in check(
      new ThrowingSubject,
      """
        query Q {
          promise
        }
      """,
      Map(
        "data" -> Map("promise" -> null),
        "errors" -> List(Map("message" -> "promise", "field" -> "promise", "locations" -> List(Map("line" -> 3, "column" -> 11))))))

    "nulls a synchronously returned object that contains a non-nullable field that throws synchronously" in check(
      new ThrowingSubject,
      """
        query Q {
          nest {
            nonNullSync
          }
        }
      """,
      Map(
        "data" -> Map("nest" -> null),
        "errors" -> List(Map("message" -> "nonNullSync", "field" -> "nest.nonNullSync", "locations" -> List(Map("line" -> 4, "column" -> 13))))))

    "nulls a synchronously returned object that contains a non-nullable field that throws in a promise" in check(
      new ThrowingSubject,
      """
        query Q {
          nest {
            nonNullPromise,
          }
        }
      """,
      Map(
        "data" -> Map("nest" -> null),
        "errors" -> List(Map("message" -> "nonNullPromise", "field" -> "nest.nonNullPromise", "locations" -> List(Map("line" -> 4, "column" -> 13))))))

    "nulls an object returned in a promise that contains a non-nullable field that throws synchronously" in check(
      new ThrowingSubject,
      """
        query Q {
          promiseNest {
            nonNullSync,
          }
        }
      """,
      Map(
        "data" -> Map("promiseNest" -> null),
        "errors" -> List(Map("message" -> "nonNullSync", "field" -> "promiseNest.nonNullSync", "locations" -> List(Map("line" -> 4, "column" -> 13))))))

    "nulls an object returned in a promise that contains a non-nullable field that throws in a promise" in check(
      new ThrowingSubject,
      """
        query Q {
          promiseNest {
            nonNullPromise,
          }
        }
      """,
      Map(
        "data" -> Map("promiseNest" -> null),
        "errors" -> List(Map("message" -> "nonNullPromise", "field" -> "promiseNest.nonNullPromise", "locations" -> List(Map("line" -> 4, "column" -> 13))))))
    
    "nulls a complex tree of nullable fields that throw" in check(
      new ThrowingSubject,
      """
        query Q {
          nest {
            sync
            promise
            nest {
              sync
              promise
            }
            promiseNest {
              sync
              promise
            }
          }
          promiseNest {
            sync
            promise
            nest {
              sync
              promise
            }
            promiseNest {
              sync
              promise
            }
          }
        }
      """,
      Map(
        "nest" -> Map(
          "sync" -> null,
          "promise" -> null,
          "nest" -> Map(
            "sync" -> null,
            "promise" -> null
          ),
          "promiseNest" -> Map(
            "sync" -> null,
            "promise" -> null
          )
        ),
        "promiseNest" -> Map(
          "sync" -> null,
          "promise" -> null,
          "nest" -> Map(
            "sync" -> null,
            "promise" -> null
          ),
          "promiseNest" -> Map(
            "sync" -> null,
            "promise" -> null
          )
        )
      ),
      List(
        Map("message" -> "sync", "field" -> "nest.sync", "locations" -> List(Map("line" -> 4, "column" -> 13))),
        Map("message" -> "sync", "field" -> "nest.nest.sync", "locations" -> List(Map("line" -> 7, "column" -> 15))),
        Map("message" -> "sync", "field" -> "nest.promiseNest.sync", "locations" -> List(Map("line" -> 11, "column" -> 15))),
        Map("message" -> "sync", "field" -> "promiseNest.sync", "locations" -> List(Map("line" -> 16, "column" -> 13))),
        Map("message" -> "sync", "field" -> "promiseNest.nest.sync", "locations" -> List(Map("line" -> 19, "column" -> 15))),
        Map("message" -> "sync", "field" -> "promiseNest.promiseNest.sync", "locations" -> List(Map("line" -> 23, "column" -> 15))),
        Map("message" -> "promise", "field" -> "nest.promise", "locations" -> List(Map("line" -> 5, "column" -> 13))),
        Map("message" -> "promise", "field" -> "nest.nest.promise", "locations" -> List(Map("line" -> 8, "column" -> 15))),
        Map("message" -> "promise", "field" -> "nest.promiseNest.promise", "locations" -> List(Map("line" -> 12, "column" -> 15))),
        Map("message" -> "promise", "field" -> "promiseNest.promise", "locations" -> List(Map("line" -> 17, "column" -> 13))),
        Map("message" -> "promise", "field" -> "promiseNest.nest.promise", "locations" -> List(Map("line" -> 20, "column" -> 15))),
        Map("message" -> "promise", "field" -> "promiseNest.promiseNest.promise", "locations" -> List(Map("line" -> 24, "column" -> 15)))
      ))

    "nulls the first nullable object after a field throws in a long chain of fields that are non-null" in check(
      new ThrowingSubject,
      """
        query Q {
          nest {
            nonNullNest {
              nonNullPromiseNest {
                nonNullNest {
                  nonNullPromiseNest {
                    nonNullSync
                  }
                }
              }
            }
          }
          promiseNest {
            nonNullNest {
              nonNullPromiseNest {
                nonNullNest {
                  nonNullPromiseNest {
                    nonNullSync
                  }
                }
              }
            }
          }
          anotherNest: nest {
            nonNullNest {
              nonNullPromiseNest {
                nonNullNest {
                  nonNullPromiseNest {
                    nonNullPromise
                  }
                }
              }
            }
          }
          anotherPromiseNest: promiseNest {
            nonNullNest {
              nonNullPromiseNest {
                nonNullNest {
                  nonNullPromiseNest {
                    nonNullPromise
                  }
                }
              }
            }
          }
        }
      """,
      Map(
        "nest" -> null,
        "promiseNest" -> null,
        "anotherNest" -> null,
        "anotherPromiseNest" -> null
      ),
      List(
        Map(
          "message" -> "nonNullSync",
          "field" -> "nest.nonNullNest.nonNullPromiseNest.nonNullNest.nonNullPromiseNest.nonNullSync",
          "locations" -> List(Map("line" -> 8, "column" -> 21))),
        Map(
          "message" -> "nonNullSync",
          "field" -> "promiseNest.nonNullNest.nonNullPromiseNest.nonNullNest.nonNullPromiseNest.nonNullSync",
          "locations" -> List(Map("line" -> 19, "column" -> 21))),
        Map(
          "message" -> "nonNullPromise",
          "field" -> "anotherNest.nonNullNest.nonNullPromiseNest.nonNullNest.nonNullPromiseNest.nonNullPromise",
          "locations" -> List(Map("line" -> 30, "column" -> 21))),
        Map(
          "message" -> "nonNullPromise",
          "field" -> "anotherPromiseNest.nonNullNest.nonNullPromiseNest.nonNullNest.nonNullPromiseNest.nonNullPromise",
          "locations" -> List(Map("line" -> 41, "column" -> 21)))
      ))

    "nulls a nullable field that synchronously returns null" in check(
      new NullingSubject,
      """
        query Q {
          sync
        }
      """,
      Map("data" -> Map("sync" -> null)))

    "nulls a nullable field that returns null in a promise" in check(
      new NullingSubject,
      """
        query Q {
          promise
        }
      """,
      Map("data" -> Map("promise" -> null)))

    "nulls a synchronously returned object that contains a non-nullable field that returns null synchronously" in check(
      new NullingSubject,
      """
        query Q {
          nest {
            nonNullSync
          }
        }
      """,
      Map(
        "data" -> Map("nest" -> null),
        "errors" -> List(Map(
          "message" -> "Cannot return null for non-nullable type",
          "field" -> "nest.nonNullSync",
          "locations" -> List(Map("line" -> 4, "column" -> 13))))))

    "nulls a synchronously returned object that contains a non-nullable field that returns null in a promise" in check(
      new NullingSubject,
      """
        query Q {
          nest {
            nonNullPromise,
          }
        }
      """,
      Map(
        "data" -> Map("nest" -> null),
        "errors" -> List(Map(
          "message" -> "Cannot return null for non-nullable type",
          "field" -> "nest.nonNullPromise",
          "locations" -> List(Map("line" -> 4, "column" -> 13))))))

    "nulls an object returned in a promise that contains a non-nullable field that returns null synchronously" in check(
      new NullingSubject,
      """
        query Q {
          promiseNest {
            nonNullSync,
          }
        }
      """,
      Map(
        "data" -> Map("promiseNest" -> null),
        "errors" -> List(Map(
          "message" -> "Cannot return null for non-nullable type",
          "field" -> "promiseNest.nonNullSync",
          "locations" -> List(Map("line" -> 4, "column" -> 13))))))

    "nulls an object returned in a promise that contains a non-nullable field that returns null ina a promise" in check(
      new NullingSubject,
      """
        query Q {
          promiseNest {
            nonNullPromise,
          }
        }
      """,
      Map(
        "data" -> Map("promiseNest" -> null),
        "errors" -> List(Map(
          "message" -> "Cannot return null for non-nullable type",
          "field" -> "promiseNest.nonNullPromise",
          "locations" -> List(Map("line" -> 4, "column" -> 13))))))

    "nulls a complex tree of nullable fields that return null" in check(
      new NullingSubject,
      """
        query Q {
          nest {
            sync
            promise
            nest {
              sync
              promise
            }
            promiseNest {
              sync
              promise
            }
          }
          promiseNest {
            sync
            promise
            nest {
              sync
              promise
            }
            promiseNest {
              sync
              promise
            }
          }
        }
      """,
      Map(
        "nest" -> Map(
          "sync" -> null,
          "promise" -> null,
          "nest" -> Map(
            "sync" -> null,
            "promise" -> null
          ),
          "promiseNest" -> Map(
            "sync" -> null,
            "promise" -> null
          )
        ),
        "promiseNest" -> Map(
          "sync" -> null,
          "promise" -> null,
          "nest" -> Map(
            "sync" -> null,
            "promise" -> null
          ),
          "promiseNest" -> Map(
            "sync" -> null,
            "promise" -> null
          )
        )
      ),
      Nil)

    "nulls the first nullable object after a field returns null in a long chain of fields that are non-null" in check(
      new NullingSubject,
      """
        query Q {
          nest {
            nonNullNest {
              nonNullPromiseNest {
                nonNullNest {
                  nonNullPromiseNest {
                    nonNullSync
                  }
                }
              }
            }
          }
          promiseNest {
            nonNullNest {
              nonNullPromiseNest {
                nonNullNest {
                  nonNullPromiseNest {
                    nonNullSync
                  }
                }
              }
            }
          }
          anotherNest: nest {
            nonNullNest {
              nonNullPromiseNest {
                nonNullNest {
                  nonNullPromiseNest {
                    nonNullPromise
                  }
                }
              }
            }
          }
          anotherPromiseNest: promiseNest {
            nonNullNest {
              nonNullPromiseNest {
                nonNullNest {
                  nonNullPromiseNest {
                    nonNullPromise
                  }
                }
              }
            }
          }
        }
      """,
      Map(
        "nest" -> null,
        "promiseNest" -> null,
        "anotherNest" -> null,
        "anotherPromiseNest" -> null
      ),
      List(
        Map(
          "message" -> "Cannot return null for non-nullable type",
          "field" -> "nest.nonNullNest.nonNullPromiseNest.nonNullNest.nonNullPromiseNest.nonNullSync",
          "locations" -> List(Map("line" -> 8, "column" -> 21))),
        Map(
          "message" -> "Cannot return null for non-nullable type",
          "field" -> "promiseNest.nonNullNest.nonNullPromiseNest.nonNullNest.nonNullPromiseNest.nonNullSync",
          "locations" -> List(Map("line" -> 19, "column" -> 21))),
        Map(
          "message" -> "Cannot return null for non-nullable type",
          "field" -> "anotherNest.nonNullNest.nonNullPromiseNest.nonNullNest.nonNullPromiseNest.nonNullPromise",
          "locations" -> List(Map("line" -> 30, "column" -> 21))),
        Map(
          "message" -> "Cannot return null for non-nullable type",
          "field" -> "anotherPromiseNest.nonNullNest.nonNullPromiseNest.nonNullNest.nonNullPromiseNest.nonNullPromise",
          "locations" -> List(Map("line" -> 41, "column" -> 21)))
      ))

    "nulls the top level if sync non-nullable field throws" in check(
      new ThrowingSubject,
      "query Q { nonNullSync }",
      Map(
        "data" -> null,
        "errors" -> List(Map("message" -> "nonNullSync", "field" -> "nonNullSync", "locations" -> List(Map("line" -> 1, "column" -> 11))))))

    "nulls the top level if async non-nullable field errors" in check(
      new ThrowingSubject,
      "query Q { nonNullPromise }",
      Map(
        "data" -> null,
        "errors" -> List(Map("message" -> "nonNullPromise", "field" -> "nonNullPromise", "locations" -> List(Map("line" -> 1, "column" -> 11))))))

    "nulls the top level if sync non-nullable field returns null" in check(
      new NullingSubject,
      "query Q { nonNullSync }",
      Map(
        "data" -> null,
        "errors" -> List(Map("message" -> "Cannot return null for non-nullable type", "field" -> "nonNullSync", "locations" -> List(Map("line" -> 1, "column" -> 11))))))

    "nulls the top level if async non-nullable field resolves null" in check(
      new NullingSubject,
      "query Q { nonNullPromise }",
      Map(
        "data" -> null,
        "errors" -> List(Map("message" -> "Cannot return null for non-nullable type", "field" -> "nonNullPromise", "locations" -> List(Map("line" -> 1, "column" -> 11))))))
  }
}
