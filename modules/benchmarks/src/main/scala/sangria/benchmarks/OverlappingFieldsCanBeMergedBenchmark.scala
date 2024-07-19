package sangria.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import sangria.ast.Document
import sangria.parser.QueryParser
import sangria.schema.Schema
import sangria.validation.rules
import sangria.validation.{QueryValidator, RuleBasedQueryValidator, Violation}

@State(Scope.Thread)
class OverlappingFieldsCanBeMergedBenchmark {

  val validator: QueryValidator = new RuleBasedQueryValidator(
    List(new rules.OverlappingFieldsCanBeMerged))

  val schema: Schema[_, _] =
    Schema.buildFromAst(
      QueryParser
        .parse("""
        type Query {
          viewer: Viewer
        }

        interface Abstract {
          field: Abstract
          leaf: Int
        }

        interface Abstract1 {
          field: Abstract
          leaf: Int
        }

        interface Abstract2 {
          field: Abstract
          leaf: Int
        }

        type Concrete1 implements Abstract1

        type Concrete2 implements Abstract2

        type Viewer {
          xingId: XingId
        }
        type XingId {
          firstName: String!
          lastName: String!
        }
        """)
        .get
    )

//  @Param(Array("10", "20", "30", "50", "80", "110"))
  @Param(Array("2", "10", "100"))
  var size: Int = _

  var overlapFrag: Document = _
  var overlapNoFrag: Document = _
  var noOverlapFrag: Document = _
  var noOverlapNoFrag: Document = _
  var repeatedFields: Document = _
  var deepAbstractConcrete: Document = _

  @Setup
  def setup(): Unit = {
    overlapFrag = makeQuery(size, overlapping = true, fragments = true)
    overlapNoFrag = makeQuery(size, overlapping = true, fragments = false)
    noOverlapFrag = makeQuery(size, overlapping = false, fragments = true)
    noOverlapNoFrag = makeQuery(size, overlapping = false, fragments = false)
    repeatedFields = makeRepeatedFieldsQuery(size)
    deepAbstractConcrete = makeDeepAbstractConcreteQuery(size)
  }

  @Benchmark
  def benchmarkRepeatedFields(bh: Blackhole): Unit =
    bh.consume(doValidate(validator, repeatedFields))

  @Benchmark
  def benchmarkOverlapFrag(bh: Blackhole): Unit =
    bh.consume(doValidate(validator, overlapFrag))

  @Benchmark
  def benchmarkOverlapNoFrag(bh: Blackhole): Unit =
    bh.consume(doValidate(validator, overlapNoFrag))

  @Benchmark
  def benchmarkNoOverlapFrag(bh: Blackhole): Unit =
    bh.consume(doValidate(validator, noOverlapFrag))

  @Benchmark
  def benchmarkNoOverlapNoFrag(bh: Blackhole): Unit =
    bh.consume(doValidate(validator, noOverlapNoFrag))

  @Benchmark
  def benchmarkDeepAbstractConcrete(bh: Blackhole): Unit =
    bh.consume(doValidate(validator, deepAbstractConcrete))

  private def doValidate(validator: QueryValidator, document: Document): Vector[Violation] = {
    val result = validator.validateQuery(schema, document, Map.empty, None)
    require(result.isEmpty)
    result
  }

  private def makeQuery(size: Int, overlapping: Boolean, fragments: Boolean) =
    if (fragments) makeQueryWithFragments(size, overlapping)
    else makeQueryWithoutFragments(size, overlapping)

  private def makeRepeatedFieldsQuery(size: Int) = {
    val b = new StringBuilder

    b.append("""
               | query testQuery {
               |   viewer {
               |     xingId {
             """.stripMargin)

    for (_ <- 1 to size)
      b.append("firstName\n")

    b.append("""
               |    }
               |  }
               |}
             """.stripMargin)

    QueryParser.parse(b.result()).get
  }

  private def makeQueryWithFragments(size: Int, overlapping: Boolean) = {
    val b = new StringBuilder

    for (i <- 1 to size) {
      if (overlapping) {
        b.append(s"""
                    |fragment mergeIdenticalFields$i on Query {
                    |   viewer {
                    |     xingId {
                    |       firstName
                    |       lastName
                    |     }
                    |   }
                    |}
        """.stripMargin)
      } else {
        b.append(s"""
                    |fragment mergeIdenticalFields$i on Query {
                    |   viewer$i {
                    |     xingId$i {
                    |       firstName$i
                    |       lastName$i
                    |     }
                    |   }
                    |}
        """.stripMargin)
      }

      b.append("\n\n")
    }

    b.append("query testQuery {")
    for (i <- 1 to size)
      b.append(s"...mergeIdenticalFields$i\n")
    b.append("}")

    QueryParser.parse(b.result()).get
  }

  private def makeQueryWithoutFragments(size: Int, overlapping: Boolean) = {
    val b = new StringBuilder

    b.append("query testQuery {")

    for (i <- 1 to size) {
      if (overlapping) {
        b.append("""
                   |viewer {
                   |  xingId {
                   |     firstName
                   |  }
                   |}
                   |
        """.stripMargin)
      } else {
        b.append(s"""
                    | viewer${1} {
                    |   xingId$i {
                    |     firstName$i
                    |   }
                    | }
        """.stripMargin)
      }

      b.append("\n\n")
    }

    b.append("}")

    QueryParser.parse(b.result()).get
  }

  private def makeDeepAbstractConcreteQuery(depth: Int) = {
    val q = new StringBuilder

    q.append("""
        |fragment multiply on Whatever {
        |   field {
        |     ... on Abstract1 { field { leaf } }
        |     ... on Abstract2 { field { leaf } }
        |     ... on Concrete1 { field { leaf } }
        |     ... on Concrete2 { field { leaf } }
        |   }
        |}
        |
        |query DeepAbstractConcrete {
        |
        |""".stripMargin)

    (1 to depth).foreach { _ =>
      q.append("field { ...multiply ")
    }

    (1 to depth).foreach { _ =>
      q.append(" }")
    }

    q.append("\n}")

    QueryParser.parse(q.result()).get
  }
}
