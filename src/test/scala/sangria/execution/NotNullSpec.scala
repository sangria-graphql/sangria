package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.schema._
import sangria.util.{GraphQlSupport, FutureResultSupport}

import scala.concurrent.Future

class NotNullSpec extends WordSpec with Matchers with FutureResultSupport with GraphQlSupport {
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

  val DataType: ObjectType[Unit, TestSubject] = ObjectType("DataType", () ⇒ fields[Unit, TestSubject](
    Field("sync", OptionType(StringType), resolve = _.value.sync),
    Field("nonNullSync", StringType, resolve = _.value.nonNullSync),
    Field("promise", OptionType(StringType), resolve = _.value.promise),
    Field("nonNullPromise", StringType, resolve = _.value.nonNullPromise),
    Field("nest", OptionType(DataType), resolve = _.value.nest),
    Field("nonNullNest", DataType, resolve = _.value.nonNullNest),
    Field("promiseNest", OptionType(DataType), resolve = _.value.promiseNest),
    Field("nonNullPromiseNest", DataType, resolve = _.value.nonNullPromiseNest),
    Field("NaN", OptionType(FloatType), resolve = _ ⇒ Some(Double.NaN)),
    Field("Inf", OptionType(FloatType), resolve = _ ⇒ Some(Double.PositiveInfinity)),
    Field("nonNullNaN", FloatType, resolve = _ ⇒ Double.NaN),
    Field("nonNullInf", FloatType, resolve = _ ⇒ Double.PositiveInfinity)))

  val schema = Schema(DataType)

  "Execute: handles non-nullable types" should {
    "nulls a nullable field that throws synchronously" in check(
      new ThrowingSubject,
      """
        query Q {
          sync
        }
      """,
      Map(
        "data" → Map("sync" → null),
        "errors" → List(Map("message" → "sync", "path" → List("sync"), "locations" → List(Map("line" → 3, "column" → 11))))))

    "nulls a nullable field that throws in a promise" in check(
      new ThrowingSubject,
      """
        query Q {
          promise
        }
      """,
      Map(
        "data" → Map("promise" → null),
        "errors" → List(Map("message" → "promise", "path" → List("promise"), "locations" → List(Map("line" → 3, "column" → 11))))))

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
        "data" → Map("nest" → null),
        "errors" → List(Map("message" → "nonNullSync", "path" → List("nest", "nonNullSync"), "locations" → List(Map("line" → 4, "column" → 13))))))

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
        "data" → Map("nest" → null),
        "errors" → List(Map("message" → "nonNullPromise", "path" → List("nest", "nonNullPromise"), "locations" → List(Map("line" → 4, "column" → 13))))))

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
        "data" → Map("promiseNest" → null),
        "errors" → List(Map("message" → "nonNullSync", "path" → List("promiseNest", "nonNullSync"), "locations" → List(Map("line" → 4, "column" → 13))))))

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
        "data" → Map("promiseNest" → null),
        "errors" → List(Map("message" → "nonNullPromise", "path" → List("promiseNest", "nonNullPromise"), "locations" → List(Map("line" → 4, "column" → 13))))))
    
    "nulls a complex tree of nullable fields that throw" in checkErrors(
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
        "nest" → Map(
          "sync" → null,
          "promise" → null,
          "nest" → Map(
            "sync" → null,
            "promise" → null
          ),
          "promiseNest" → Map(
            "sync" → null,
            "promise" → null
          )
        ),
        "promiseNest" → Map(
          "sync" → null,
          "promise" → null,
          "nest" → Map(
            "sync" → null,
            "promise" → null
          ),
          "promiseNest" → Map(
            "sync" → null,
            "promise" → null
          )
        )
      ),
      List(
        Map("message" → "sync", "path" → List("nest", "sync"), "locations" → List(Map("line" → 4, "column" → 13))),
        Map("message" → "sync", "path" → List("nest", "nest", "sync"), "locations" → List(Map("line" → 7, "column" → 15))),
        Map("message" → "sync", "path" → List("nest", "promiseNest", "sync"), "locations" → List(Map("line" → 11, "column" → 15))),
        Map("message" → "sync", "path" → List("promiseNest", "sync"), "locations" → List(Map("line" → 16, "column" → 13))),
        Map("message" → "sync", "path" → List("promiseNest", "nest", "sync"), "locations" → List(Map("line" → 19, "column" → 15))),
        Map("message" → "sync", "path" → List("promiseNest", "promiseNest", "sync"), "locations" → List(Map("line" → 23, "column" → 15))),
        Map("message" → "promise", "path" → List("nest", "promise"), "locations" → List(Map("line" → 5, "column" → 13))),
        Map("message" → "promise", "path" → List("nest", "nest", "promise"), "locations" → List(Map("line" → 8, "column" → 15))),
        Map("message" → "promise", "path" → List("nest", "promiseNest", "promise"), "locations" → List(Map("line" → 12, "column" → 15))),
        Map("message" → "promise", "path" → List("promiseNest", "promise"), "locations" → List(Map("line" → 17, "column" → 13))),
        Map("message" → "promise", "path" → List("promiseNest", "nest", "promise"), "locations" → List(Map("line" → 20, "column" → 15))),
        Map("message" → "promise", "path" → List("promiseNest", "promiseNest", "promise"), "locations" → List(Map("line" → 24, "column" → 15)))
      ))

    "nulls the first nullable object after a field throws in a long chain of fields that are non-null" in checkErrors(
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
        "nest" → null,
        "promiseNest" → null,
        "anotherNest" → null,
        "anotherPromiseNest" → null
      ),
      List(
        Map(
          "message" → "nonNullSync",
          "path" → List("nest", "nonNullNest", "nonNullPromiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullSync"),
          "locations" → List(Map("line" → 8, "column" → 21))),
        Map(
          "message" → "nonNullSync",
          "path" → List("promiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullSync"),
          "locations" → List(Map("line" → 19, "column" → 21))),
        Map(
          "message" → "nonNullPromise",
          "path" → List("anotherNest", "nonNullNest", "nonNullPromiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullPromise"),
          "locations" → List(Map("line" → 30, "column" → 21))),
        Map(
          "message" → "nonNullPromise",
          "path" → List("anotherPromiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullPromise"),
          "locations" → List(Map("line" → 41, "column" → 21)))
      ))

    "nulls a nullable field that synchronously returns null" in check(
      new NullingSubject,
      """
        query Q {
          sync
        }
      """,
      Map("data" → Map("sync" → null)))

    "nulls a nullable field that returns null in a promise" in check(
      new NullingSubject,
      """
        query Q {
          promise
        }
      """,
      Map("data" → Map("promise" → null)))

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
        "data" → Map("nest" → null),
        "errors" → List(Map(
          "message" → "Cannot return null for non-nullable type",
          "path" → List("nest", "nonNullSync"),
          "locations" → List(Map("line" → 4, "column" → 13))))))

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
        "data" → Map("nest" → null),
        "errors" → List(Map(
          "message" → "Cannot return null for non-nullable type",
          "path" → List("nest", "nonNullPromise"),
          "locations" → List(Map("line" → 4, "column" → 13))))))

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
        "data" → Map("promiseNest" → null),
        "errors" → List(Map(
          "message" → "Cannot return null for non-nullable type",
          "path" → List("promiseNest", "nonNullSync"),
          "locations" → List(Map("line" → 4, "column" → 13))))))

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
        "data" → Map("promiseNest" → null),
        "errors" → List(Map(
          "message" → "Cannot return null for non-nullable type",
          "path" → List("promiseNest", "nonNullPromise"),
          "locations" → List(Map("line" → 4, "column" → 13))))))

    "nulls a complex tree of nullable fields that return null" in checkErrors(
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
        "nest" → Map(
          "sync" → null,
          "promise" → null,
          "nest" → Map(
            "sync" → null,
            "promise" → null
          ),
          "promiseNest" → Map(
            "sync" → null,
            "promise" → null
          )
        ),
        "promiseNest" → Map(
          "sync" → null,
          "promise" → null,
          "nest" → Map(
            "sync" → null,
            "promise" → null
          ),
          "promiseNest" → Map(
            "sync" → null,
            "promise" → null
          )
        )
      ),
      Nil)

    "nulls the first nullable object after a field returns null in a long chain of fields that are non-null" in checkErrors(
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
        "nest" → null,
        "promiseNest" → null,
        "anotherNest" → null,
        "anotherPromiseNest" → null
      ),
      List(
        Map(
          "message" → "Cannot return null for non-nullable type",
          "path" → List("nest", "nonNullNest", "nonNullPromiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullSync"),
          "locations" → List(Map("line" → 8, "column" → 21))),
        Map(
          "message" → "Cannot return null for non-nullable type",
          "path" → List("promiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullSync"),
          "locations" → List(Map("line" → 19, "column" → 21))),
        Map(
          "message" → "Cannot return null for non-nullable type",
          "path" → List("anotherNest", "nonNullNest", "nonNullPromiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullPromise"),
          "locations" → List(Map("line" → 30, "column" → 21))),
        Map(
          "message" → "Cannot return null for non-nullable type",
          "path" → List("anotherPromiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullNest", "nonNullPromiseNest", "nonNullPromise"),
          "locations" → List(Map("line" → 41, "column" → 21)))
      ))

    "nulls the top level if sync non-nullable field throws" in check(
      new ThrowingSubject,
      "query Q { nonNullSync }",
      Map(
        "data" → null,
        "errors" → List(Map("message" → "nonNullSync", "path" → List("nonNullSync"), "locations" → List(Map("line" → 1, "column" → 11))))))

    "nulls the top level if async non-nullable field errors" in check(
      new ThrowingSubject,
      "query Q { nonNullPromise }",
      Map(
        "data" → null,
        "errors" → List(Map("message" → "nonNullPromise", "path" → List("nonNullPromise"), "locations" → List(Map("line" → 1, "column" → 11))))))

    "nulls the top level if sync non-nullable field returns null" in check(
      new NullingSubject,
      "query Q { nonNullSync }",
      Map(
        "data" → null,
        "errors" → List(Map("message" → "Cannot return null for non-nullable type", "path" → List("nonNullSync"), "locations" → List(Map("line" → 1, "column" → 11))))))

    "nulls the top level if async non-nullable field resolves null" in check(
      new NullingSubject,
      "query Q { nonNullPromise }",
      Map(
        "data" → null,
        "errors" → List(Map("message" → "Cannot return null for non-nullable type", "path" → List("nonNullPromise"), "locations" → List(Map("line" → 1, "column" → 11))))))

    "nulls the top level if non-nullable field resolves NaN" in check(
      new NullingSubject,
      "query Q { nonNullNaN }",
      Map(
        "data" → null,
        "errors" → List(Map("message" → "Cannot return null for non-nullable type", "path" → List("nonNullNaN"), "locations" → List(Map("line" → 1, "column" → 11))))))

    "nulls the top level if non-nullable field resolves infinity" in check(
      new NullingSubject,
      "query Q { nonNullInf }",
      Map(
        "data" → null,
        "errors" → List(Map("message" → "Cannot return null for non-nullable type", "path" → List("nonNullInf"), "locations" → List(Map("line" → 1, "column" → 11))))))

    "treats infinity as `null`" in check(
      new NullingSubject,
      "query Q { Inf }",
      Map("data" → Map("Inf" → null)))

    "treats NaN as `null`" in check(
      new NullingSubject,
      "query Q { NaN }",
      Map("data" → Map("NaN" → null)))
  }
}
