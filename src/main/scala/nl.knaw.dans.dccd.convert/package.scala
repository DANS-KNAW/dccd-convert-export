/**
 * Copyright (C) 2017 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans

import java.io.File
import java.nio.file.Path
import java.text.SimpleDateFormat

import org.apache.commons.csv.{ CSVFormat, CSVPrinter }

import scala.collection.mutable
import scala.collection.mutable.Map
import scala.io.Source
import scala.io.Source.fromString
import scala.util.{ Failure, Success, Try }
import scala.xml.parsing.ConstructingParser.fromSource
import scala.xml.{ NodeSeq, TopScope }

package object dccd {

  def getListOfSubDirectories(directoryName: String): Array[String] = {
    new File(directoryName)
      .listFiles
      .filter(_.isDirectory)
      .map(_.getName)
  }

  val csvFormat: CSVFormat = CSVFormat.RFC4180.withHeader(
    "DATASET",
    "DC_TITLE",
    "DCT_ALTERNATIVE",
    "DCX_CREATOR_TITLES",
    "DCX_CREATOR_INITIALS",
    "DCX_CREATOR_INSERTIONS",
    "DCX_CREATOR_SURNAME",
    "DCX_CREATOR_DAI",
    "DCX_CREATOR_ROLE",
    "DCX_CREATOR_ORGANIZATION",
    "DCX_CONTRIBUTOR_TITLES",
    "DCX_CONTRIBUTOR_INITIALS",
    "DCX_CONTRIBUTOR_INSERTIONS",
    "DCX_CONTRIBUTOR_SURNAME",
    "DCX_CONTRIBUTOR_DAI",
    "DCX_CONTRIBUTOR_ORGANIZATION",
    "DCX_CONTRIBUTOR_ROLE",
    "DDM_CREATED",
    "DCT_RIGHTSHOLDER",
    "DC_PUBLISHER",
    "DC_DESCRIPTION",
    "DC_SUBJECT_SCHEME",
    "DC_SUBJECT",
    "DCT_TEMPORAL_SCHEME",
    "DCT_TEMPORAL",
    "DCT_SPATIAL",
    "DCX_SPATIAL_SCHEME",
    "DCX_SPATIAL_X",
    "DCX_SPATIAL_Y",
    "DCX_SPATIAL_NORTH",
    "DCX_SPATIAL_SOUTH",
    "DCX_SPATIAL_EAST",
    "DCX_SPATIAL_WEST",
    "DC_IDENTIFIER_TYPE",
    "DC_IDENTIFIER",
    "DCX_RELATION_QUALIFIER",
    "DCX_RELATION_TITLE",
    "DCX_RELATION_LINK",
    "DC_TYPE",
    "DC_FORMAT",
    "DC_LANGUAGE",
    "DC_SOURCE",
    "DDM_ACCESSRIGHTS",
    "DDM_AVAILABLE",
    "DDM_AUDIENCE",
    "DEPOSITOR_ID",
    "SF_DOMAIN",
    "SF_USER",
    "SF_COLLECTION",
    "AV_SUBTITLES",
    "AV_FILE_PATH",
    "AV_SUBTITLES_LANGUAGE",
    "SF_PLAY_MODE",
    "DCT_DATE",
    "DCT_DATE_QUALIFIER",
    "FILE_PATH",
    "FILE_TITLE",
    "FILE_ACCESSIBILITY").withDelimiter(',')

  val out: Appendable = new StringBuffer()

  val printer: CSVPrinter = csvFormat.print(out)

  def parseNoWS(s: String): NodeSeq = fromSource(fromString(s), preserveWS = false).element(TopScope)

  val Format = new SimpleDateFormat("yyyy-MM-dd")
  var range: String = ""
  val valA: String = " AD"
  val valB: String = " BC"

  def CSVReader(absPath: String, delimiter: String): List[scala.Seq[String]] = {
    Source.fromFile(absPath).getLines().toList map (_.split("""\""" + delimiter).toSeq)
  }

  def mapDepositorIdActualEasyUserId(path: String): scala.collection.mutable.Map[String, String] = {
    var map: scala.collection.mutable.Map[String, String] = scala.collection.mutable.Map()
    CSVReader(path, ",").foreach(i => { map.put(i.head, i.apply(1)) })
    map
  }

  def directoryOfData(projectName: String, path: Path, relativePathOfData: String): String = {
    path.toString + "/" + projectName + relativePathOfData
  }

  def createListDcSubject(listDcS: List[String], candidateSeq: mutable.Seq[String]): List[String] = {
    var list = listDcS
    var candidates = candidateSeq
    for (i <- candidates) {
      if (i.nonEmpty)
        list = list ::: List(i)
    }
    list
  }

  def maxLengthOfList(multiValMap: Map[String, List[String]]): Int = {
    var maxLength: Int = 0
    for (k <- multiValMap.keys) {
      if (multiValMap(k).length > maxLength) {
        maxLength = multiValMap(k).length
      }
    }
    maxLength
  }



  implicit class TryExtensions2[T](val t: Try[T]) extends AnyVal {
    // TODO candidate for dans-scala-lib
    def unsafeGetOrThrow: T = {
      t match {
        case Success(value) => value
        case Failure(throwable) => throw throwable
      }
    }
  }
}

