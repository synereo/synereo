package com.biosimilarity.evaluator.util

import java.io._
import java.nio.file.{Path, Paths}

trait FileHelpers {

  def withFileReader[T](file: File)(fn: (FileReader) => T): T = {
    val fr: FileReader = new FileReader(file)
    try fn(fr)
    finally fr.close()
  }

  def withFileWriter[T](file: File)(fn: (FileWriter) => T): T = {
    val fw: FileWriter = new FileWriter(file)
    try fn(fw)
    finally fw.close()
  }

  def resourceStream(resourceName: String): InputStream = {
    val is: InputStream = new FileInputStream(resourcesDir.resolve(resourceName).toFile)
    require(is.ne(null), s"Resource $resourceName not found")
    is
  }

  def pwd: Path           = Paths.get(".").toAbsolutePath.normalize()
  def classLocation: Path = Paths.get(classOf[FileHelpers].getProtectionDomain.getCodeSource.getLocation.toURI)
  def resourcesDir: Path  = classLocation.getParent.getParent.resolve("resources")
  def testDir: Path       = classLocation.getParent.getParent.getParent.getParent.resolve("test-data")
}
