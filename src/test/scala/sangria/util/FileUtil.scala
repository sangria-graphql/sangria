package sangria.util

import java.io.File

import io.github.lukehutch.fastclasspathscanner.FastClasspathScanner
import io.github.lukehutch.fastclasspathscanner.matchprocessor.FileMatchContentsProcessorWithContext
import sangria.parser.QueryParser
import sangria.parser.DeliveryScheme.Throw

import scala.collection.immutable.VectorBuilder
import scala.io.Source

import net.jcazevedo.moultingyaml._


object FileUtil {
  def loadQuery(name: String) =
    Source.fromInputStream(this.getClass.getResourceAsStream(s"/queries/$name"), "UTF-8").mkString

  def loadYaml(name: String, root: String = "scenarios") =
    Source.fromInputStream(this.getClass.getResourceAsStream(s"/$root/$name"), "UTF-8").mkString.parseYaml

  def loadScenarios(path: String, root: String = "scenarios") = {
    val builder = new VectorBuilder[ScenarioFile]

    new FastClasspathScanner(root + "." + path).matchFilenameExtension("yaml", new FileMatchContentsProcessorWithContext {
      override def processMatch(file: File, relativePath: String, fileContents: Array[Byte]) = {
        builder += ScenarioFile(file.getName, relativePath, new String(fileContents, "UTF-8").parseYaml)
      }
    }).scan()

    builder.result()
  }

  def loadSchema(path: String) =
    QueryParser.parse(Source.fromInputStream(this.getClass.getResourceAsStream("/" + path), "UTF-8").mkString)

  case class ScenarioFile(fileName: String, path: String, scenario: YamlValue) {
    def folder = path.substring(0, path.lastIndexOf("/"))
  }
}
