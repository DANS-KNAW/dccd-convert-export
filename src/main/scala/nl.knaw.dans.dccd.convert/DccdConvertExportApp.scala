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
package nl.knaw.dans.dccd.convert

import java.io.{ File, FileWriter }
import java.nio.file.{ Path, Paths }
import java.text.SimpleDateFormat
import java.util.{ Calendar, Date, GregorianCalendar, Locale }

import org.apache.commons.csv.{ CSVFormat, CSVPrinter }

import scala.io.Source.fromString
import scala.util.Try
import scala.xml.parsing.ConstructingParser.fromSource
import scala.xml.{ Elem, NodeSeq, TopScope, XML }

//class DccdConvertExportApp(wiring: ApplicationWiring)   {
class DccdConvertExportApp(configuration: Configuration) {

  def getListOfSubDirectories(directoryName: String): Array[String] = {
    new File(directoryName)
      .listFiles
      .filter(_.isDirectory)
      .map(_.getName)
  }

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    }
    else {
      List[File]()
    }
  }

  def getPath(dir: String): Path = {
    Paths.get(configuration.properties.getString(dir))
  }

  val csvFormat: CSVFormat = CSVFormat.RFC4180.withHeader("DATASET", "DC_TITLE", "DEPOSITOR_ID", "DCX_CREATOR_ORGANIZATION", "DDM_CREATED", "DCT_RIGHTSHOLDER", "DC_DESCRIPTION", "DC_SUBJECT", "DCT_TEMPORAL", "DCX_SPATIAL_X", "DCX_SPATIAL_Y", "DC_IDENTIFIER", "DCX_RELATION_QUALIFIER", "DCX_RELATION_TITLE", "DCX_RELATION_LINK", "DCX_RELATION_QUALIFIER(2)", "DCX_RELATION_TITLE(2)", "DC_TYPE", "DC_LANGUAGE", "DDM_ACCESSRIGHTS", "DDM_AVAILABLE", "DDM_AUDIENCE").withDelimiter(',')
  val out: Appendable = new StringBuffer()
  val printer: CSVPrinter = csvFormat.print(out)

  val csvPrinterToFile = new CSVPrinter(new FileWriter("./data/dccdConvert" + ".csv"), csvFormat.withDelimiter(','))

  private def parse(s: String) = fromSource(fromString(s), preserveWS = true).element(TopScope)

  private def parseNoWS(s: String) = fromSource(fromString(s), preserveWS = false).element(TopScope)

  var dirr: String = _
  var dirr2: String = _
  var dirPath: Path = _
  var dirList: List[String] = _
  var fileList: List[File] = _
  var file2: File = _

  var metadata: Elem = _
  var parsedMetadata: NodeSeq = _
  var user: Elem = _
  var parsedUser: NodeSeq = _

  var dataset: String = _
  var dcTitle: String = _
  var depositorId: String = _
  var dcxCreatorOrganization: String = _

  val Format = new SimpleDateFormat("yyyy-MM-dd")

  var date: String = _
  var parsedDate: AnyRef = _
  var ddmCreated: String = _

  var dctRightsHolder: String = _
  var dcDescription: String = _
  var dcSubject: String = _
  var dcElementType: String = _
  var dcObjectType: String = _
  var dcTypeMetadata: String = _
  var dcCategory: String = _
  var dcTaxon: String = _

  var range: String = _

  var dctTemporal: String = _

  var dcxSpatialX: String = _
  var dcxSpatialY: String = _

  var dcIdentifierSid: String = _
  var dcIdentifierIdentifier: String = _

  var dcxRelationQualifier: String = _
  var dcxRelationTitle: String = _
  var dcxRelationLink: String = _

  var dcxRelationQualifier_2: String = _
  var dcxRelationTitle_2: String = _

  var dcType: String = _

  var language: String = _
  var isoDefault: Locale = _
  var languageISO3: String = _

  var ddmAccessRights: String = _
  var now: Date = _
  var ddmAvailable: String = _
  var ddmAudience: String = _


  def createCsvReportFromXmlFiles(dir: String): String = {
    dirPath = getPath(dir)
    dirList = getListOfSubDirectories(getPath(dir).toString).toList
    dirList.foreach { i =>
      dirr = dirPath.toString + "/" + i + "/administrative/project_metadata.xml"
      dirr2 = dirPath.toString + "/" + i + "/administrative/user.xml"
      println(dirr)
      metadata = XML.loadFile(dirr.toString)
      parsedMetadata = parseNoWS(metadata.toString())
      user = XML.loadFile(dirr2.toString)
      parsedUser = parseNoWS(user.toString())

      dataset = dirr
      dcTitle = (parsedMetadata \\ "title").text
      depositorId = (parsedUser \\ "id").text
      dcxCreatorOrganization = (parsedMetadata \\ "ownerOrganizationId").text

      date = (parsedMetadata \\ "stateChanged").text
      parsedDate = Format.parseObject(date)
      ddmCreated = Format.format(parsedDate)

      dctRightsHolder = dcxCreatorOrganization
      dcDescription = "Dendrochronological project"
      dcSubject = "Dendrochronology"
      dcElementType = "dccd_element_type: " + (parsedMetadata \\ "elementType").text
      dcObjectType = "dccd_object_type: " + (parsedMetadata \\ "objectType").text
      dcTypeMetadata = "dccd_type: " + (parsedMetadata \\ "type").text
      dcCategory = "dccd_category: " + (parsedMetadata \\ "category").text
      dcTaxon = "dccd_taxon: " + (parsedMetadata \\ "taxon").text

      def timeRange(parsedData: NodeSeq): String = {
        val parsedData = parsedMetadata
        val first = (parsedData \\ "firstYear").text
        val second = (parsedData \\ "lastYear").text
        if (first.contains("-").equals(false) && second.contains("-").equals(false)) {
          range = first + "-" + second + " AD"
        }
        if (first.contains("-") && second.contains("-")) {
          range = ((first.toInt - 1) * (-1)).toString + "-" + ((second.toInt - 1) * (-1)).toString + " BC"
        }
        if (first.contains("-") && second.contains("-").equals(false)) {
          range = ((first.toInt - 1) * (-1)).toString + " BC" + "-" + second + " AD"
        }
        range
      }

      dctTemporal = timeRange(parsedMetadata)

      dcxSpatialX = (parsedMetadata \\ "lat").text
      dcxSpatialY = (parsedMetadata \\ "lng").text

      dcIdentifierSid = (parsedMetadata \\ "sid").text + "\n"
      dcIdentifierIdentifier = (parsedMetadata \\ "identifier").text

      dcxRelationQualifier = " 'references' "
      dcxRelationTitle = "Digital Collaboratory for Cultural Dendrochronology (DCCD)"
      dcxRelationLink = "https://dendro.dans.knaw.nl/dccd/project/" + (parsedMetadata \\ "sid").text

      dcxRelationQualifier_2 = " 'IsFormatOf' "
      dcxRelationTitle_2 = (parsedMetadata \\ "sid").text

      dcType = "DATASET"

      language = (parsedMetadata \\ "language").text
      isoDefault = new Locale(language)
      languageISO3 = isoDefault.getISO3Language

      ddmAccessRights = "NO_ACCESS"
      now = Calendar.getInstance().getTime
      ddmAvailable = Format.format(now)

      ddmAudience = "D37000"

      printer.printRecord(dirr, dcTitle, depositorId, dcxCreatorOrganization, ddmCreated, dctRightsHolder, dcDescription, dcSubject, dctTemporal, dcxSpatialX, dcxSpatialY, dcIdentifierSid, dcxRelationQualifier, dcxRelationTitle, dcxRelationLink, dcxRelationQualifier_2, dcxRelationTitle_2, dcType, languageISO3, ddmAccessRights, ddmAvailable, ddmAudience)
      printer.printRecord("", "", "", "", "", "", "", dcElementType, "", "", "", dcIdentifierIdentifier, "", "", "", "", "", "", "", "", "", "")
      printer.printRecord("", "", "", "", "", "", "", dcObjectType, "", "", "", "", "", "", "", "", "", "", "", "", "", "")
      printer.printRecord("", "", "", "", "", "", "", dcTypeMetadata, "", "", "", "", "", "", "", "", "", "", "", "", "", "")
      printer.printRecord("", "", "", "", "", "", "", dcCategory, "", "", "", "", "", "", "", "", "", "", "", "", "", "")
      printer.printRecord("", "", "", "", "", "", "", dcTaxon, "", "", "", "", "", "", "", "", "", "", "", "", "", "")

      csvPrinterToFile.printRecord(dirr, dcTitle, depositorId, dcxCreatorOrganization, ddmCreated, dctRightsHolder, dcDescription, dcSubject, dctTemporal, dcxSpatialX, dcxSpatialY, dcIdentifierSid, dcxRelationQualifier, dcxRelationTitle, dcxRelationLink, dcxRelationQualifier_2, dcxRelationTitle_2, dcType, languageISO3, ddmAccessRights, ddmAvailable, ddmAudience)
      csvPrinterToFile.printRecord("", "", "", "", "", "", "", dcElementType, "", "", "", dcIdentifierIdentifier, "", "", "", "", "", "", "", "", "", "")
      csvPrinterToFile.printRecord("", "", "", "", "", "", "", dcObjectType, "", "", "", "", "", "", "", "", "", "", "", "", "", "")
      csvPrinterToFile.printRecord("", "", "", "", "", "", "", dcTypeMetadata, "", "", "", "", "", "", "", "", "", "", "", "", "", "")
      csvPrinterToFile.printRecord("", "", "", "", "", "", "", dcCategory, "", "", "", "", "", "", "", "", "", "", "", "", "", "")
      csvPrinterToFile.printRecord("", "", "", "", "", "", "", dcTaxon, "", "", "", "", "", "", "", "", "", "", "", "", "", "")


    }

    csvPrinterToFile.flush()
    printer.flush()
    printer.close()
    out.toString

  }

  def createFullReport(depositor: Option[String] = None): Try[String] = {
    createCsvReportFromXmlFiles("projects")
    System.out.print(out.toString)
    Try {
      "full report"
    }
  }

}
