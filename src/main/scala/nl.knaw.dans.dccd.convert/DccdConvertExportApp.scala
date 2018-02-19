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

import java.io._
import java.nio.file.{ Path, Paths }
import java.util.{ Calendar, GregorianCalendar, Locale }

import nl.knaw.dans.dccd.{ dataPath, _ }
import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVPrinter, ExtendedBufferedReader }

import scala.collection.mutable._
import scala.io.Source.fromInputStream
import scala.util.Try
import scala.xml.parsing.ConstructingParser.fromSource
import scala.xml.{ Elem, NodeSeq, XML }

class DccdConvertExportApp(configuration: Configuration)  {

  def getPath(dir: String): Path = {
        Paths.get(configuration.properties.getString(dir))
  }


  def directoryPath(dir: String, userDefinedDataPath:String): String = {
    val userPath: String = userDefinedDataPath
    val defaultPath : String = getPath(dir).toString
    var path = ""
    if(userPath != "None"){
       path = userPath
    }
    if(userPath == "None"){
       path = defaultPath
    }
    path
  }

  //TODO Currently the output is being saved into current directory. We can replace this by a specific directory if neccessary.
  val csvPrinterToFile = new CSVPrinter(new FileWriter("./instructions.csv"), csvFormat.withDelimiter(','))

  //TODO I need an actual data for "UserIdEasyMap.csv" file that will contain
  //TODO DepositorIds and corresponding actual Easy UserIds
  //TODO Ex: DepositorIdInUserXmlFile, CorrespondingEasyUserId
  //TODO     AnotherDepositorIdInUserXmlFile, CorrespondingEasyUserId
  //TODO Sequence of DepositorIds in the first column do not matter
  def UserIdMappingFilePath(dir: String, userDefinedDataPath:String ): String = {
    directoryPath(dir, userDefinedDataPath) + "/UserIdEasyMap.csv"
  }


  def directoryList(dir: String, userDefinedDataPath:String): List[String] = {
    getListOfSubDirectories(directoryPath(dir, userDefinedDataPath)).toList
  }

  def getMetadataAsXmlTreeElem(projectName: String, dir: String, userDefinedDataPath:String): Elem = {
    XML.loadFile(directoryOfData(projectName, directoryPath(dir, userDefinedDataPath), "/administrative/project_metadata.xml").toString)
  }

  def getUserAsXmlTreeElem(projectName: String, dir: String, userDefinedDataPath:String): Elem = {
    XML.loadFile(directoryOfData(projectName, directoryPath(dir, userDefinedDataPath), "/administrative/user.xml").toString)
  }

  def getParsedMetadata(projectName: String, dir: String, userDefinedDataPath:String): NodeSeq = {
    parseNoWS(getMetadataAsXmlTreeElem(projectName, dir, userDefinedDataPath).toString())
  }

  def getParsedUser(projectName: String, dir: String, userDefinedDataPath:String): NodeSeq = {
    parseNoWS(getUserAsXmlTreeElem(projectName, dir, userDefinedDataPath).toString())
  }

  def extractDcTitle(projectName: String, dir: String, userDefinedDataPath:String): String = {
    (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "title").text.trim
  }

  //TODO else case should be removed after getting the actual data for UserIdEasyMap
  def extractDepositorId(projectName: String, dir: String, userDefinedDataPath:String): String = {
    val depId: String = (getParsedUser(projectName, dir, userDefinedDataPath:String) \\ "id").text.trim
    var userId: String = depId
    val cmdDataPath: String = userDefinedDataPath
    if (mapDepositorIdActualEasyUserId(UserIdMappingFilePath(dir, cmdDataPath )).keySet.contains(depId)) {
      for ((k, v) <- mapDepositorIdActualEasyUserId(UserIdMappingFilePath(dir, cmdDataPath )))
        if (k == depId)
          userId = v
    }
    userId
  }

  def extractDataset(projectName: String): String = {
    projectName
  }

  def extractDcxCreatorOrganization(projectName: String, dir: String, userDefinedDataPath:String): String = {
    (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "ownerOrganizationId").text.trim
  }

  def extractDdmCreated(projectName: String, dir: String, userDefinedDataPath:String): String = {
    var date: String = (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "stateChanged").text.trim
    var parsedDate: AnyRef = Format.parseObject(date)
    Format.format(parsedDate)
  }

  def extractDctRightsHolder(projectName: String, dir: String, userDefinedDataPath:String): String = {
    extractDcxCreatorOrganization(projectName, dir, userDefinedDataPath)
  }

  def setDcDescription(): String = {
    "Dendrochronological project".trim
  }

  def setDcSubject(): String = {
    "Dendrochronology".trim
  }

  def extractListOfDcElementTypes(projectName: String, dir: String, userDefinedDataPath:String): List[String] = {
    var ListOfElementTypes: List[String] = List()
    val metadataXmlElem: Elem = getMetadataAsXmlTreeElem(projectName, dir, userDefinedDataPath)
    for (e <- metadataXmlElem.child) {
      if (e.label == "elementTypes") {
        for (ee <- e.child) {
          if ((ee \\ "elementType").text.nonEmpty) {
            ListOfElementTypes = ListOfElementTypes ::: List("dccd_element_type: " + (ee \\ "elementType").text)
          }
        }
      }
    }
    ListOfElementTypes
  }

  def extractListOfDcObjectTypes(projectName: String, dir: String, userDefinedDataPath:String): List[String] = {
    var ListOfObjectTypes: List[String] = List()
    val metadataXmlElem: Elem = getMetadataAsXmlTreeElem(projectName, dir, userDefinedDataPath:String)
    for (e <- metadataXmlElem.child) {
      if (e.label == "objectTypes") {
        for (ee <- e.child) {
          if ((ee \\ "objectType").text.nonEmpty) {
            ListOfObjectTypes = ListOfObjectTypes ::: List("dccd_object_type: " + (ee \\ "objectType").text)
          }
        }
      }
    }
    ListOfObjectTypes
  }


  def extractListOfDcTypesMetadata(projectName: String, dir: String, userDefinedDataPath:String): List[String] = {
    var ListOfDcTypes: List[String] = List()
    val metadataXmlElem: Elem = getMetadataAsXmlTreeElem(projectName, dir, userDefinedDataPath:String)
    for (e <- metadataXmlElem.child) {
      if (e.label == "types") {
        for (ee <- e.child) {
          if ((ee \\ "type").text.nonEmpty) {
            ListOfDcTypes = ListOfDcTypes ::: List("dccd_type: " + (ee \\ "type").text)
          }
        }
      }
    }
    ListOfDcTypes
  }

  def extractDcCategory(projectName: String, dir: String, userDefinedDataPath:String): String = {
    if ((getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "category").text.nonEmpty) {
      "dccd_category: " + (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "category").text.trim
    }
    else ""
  }

  def extractListOfDcTaxons(projectName: String, dir: String, userDefinedDataPath:String): List[String] = {
    var ListOfDcTaxons: List[String] = List()
    val metadataXmlElem: Elem = getMetadataAsXmlTreeElem(projectName, dir, userDefinedDataPath)
    for (e <- metadataXmlElem.child) {
      if (e.label == "taxons") {
        for (ee <- e.child) {
          if ((ee \\ "taxon").text.nonEmpty) {
            ListOfDcTaxons = ListOfDcTaxons ::: List("dccd_taxon: " + (ee \\ "taxon").text)
          }
        }
      }
    }
    ListOfDcTaxons
  }

  //TODO rearrange the locations of ".isEmpty" conditions
  def extractTimeRange(projectName: String, dir: String, userDefinedDataPath:String): String = {
    val first = (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "firstYear").text.trim
    val last = (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "lastYear").text.trim
    if (first.contains("-").equals(false) && last.contains("-").equals(false)) {
      if (first.nonEmpty && last.nonEmpty)
        range = first + "-" + last + valA
      //TODO think about the output format of the following conditions
      if (first.nonEmpty && last.isEmpty)
      //TODO should we replace "first" with "first + valA"?
        range = first + "-" + ""
      if (first.isEmpty && last.nonEmpty)
      //TODO should we replace "last" with "last + valA"?
        range = "" + "-" + last
    }
    if (first.contains("-") && last.contains("-")) {
      if (first.nonEmpty && last.nonEmpty)
        range = ((first.toInt - 1) * (-1)).toString + "-" + ((last.toInt - 1) * (-1)).toString + valB
      //TODO think about the output format of the following conditions
      if (first.nonEmpty && last.isEmpty)
      //TODO should we remove "+ valB"
        range = ((first.toInt - 1) * (-1)).toString + valB + "-" + ""
      if (first.isEmpty && last.nonEmpty)
      //TODO should we remove "+ valB"
        range = "" + "-" + ((last.toInt - 1) * (-1)).toString + valB
    }
    if (first.contains("-") && last.contains("-").equals(false)) {
      if (first.nonEmpty && last.nonEmpty)
        range = ((first.toInt - 1) * (-1)).toString + valB + "-" + last + valA
      //TODO think about the output format of the following conditions
      if (first.nonEmpty && last.isEmpty)
      //TODO should we remove "+ valB"
        range = ((first.toInt - 1) * (-1)).toString + valB + "-" + ""
      if (first.isEmpty && last.nonEmpty)
      //TODO should we remove "+ valA"
        range = "" + "-" + last + valA

    }
    range

  }

  def setDcxSpatialScheme(): String = {
    "degrees".trim
  }

  def extractDcxSpatialX(projectName: String, dir: String, userDefinedDataPath:String): String = {
    if ((getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "lat").text.nonEmpty) {
      (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "lat").text.trim
    }
    else ""
  }

  def extractDcxSpatialY(projectName: String, dir: String, userDefinedDataPath:String): String = {
    if ((getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "lng").text.nonEmpty) {
      (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "lng").text.trim
    }
    else ""
  }

  def extractDcIdentifierSid(projectName: String, dir: String, userDefinedDataPath:String): String = {
    if ((getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "sid").text.nonEmpty) {
      (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "sid").text.trim
    }
    else ""
  }

  def extractDcIdentifierIdentifier(projectName: String, dir: String, userDefinedDataPath:String): String = {
    if ((getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "identifier").text.nonEmpty) {
      (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "identifier").text.trim
    }
    else ""
  }

  def setDcxRelationQualifier(): String = {
    " references ".trim
  }

  def setDcxRelationTitle(): String = {
    "Digital Collaboratory for Cultural Dendrochronology (DCCD)".trim
  }

  def extractDcxRelationLink(projectName: String, dir: String, userDefinedDataPath:String): String = {
    if ((getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "sid").text.nonEmpty) {
      "https://dendro.dans.knaw.nl/dccd/project/" + (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "sid").text.trim
    }
    else ""
  }

  def setDcxRelationQualifier_2(): String = {
    " isFormatOf ".trim
  }

  def extractDcxRelationTitle_2(projectName: String, dir: String, userDefinedDataPath:String): String = {
    if ((getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "sid").text.nonEmpty) {
      (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "sid").text.trim
    }
    else ""
  }

  def setDcType(): String = {
    "Dataset".trim
  }

  def extractLanguage(projectName: String, dir: String, userDefinedDataPath:String): String = {
    if ((getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "language").text.nonEmpty) {
      (getParsedMetadata(projectName, dir, userDefinedDataPath) \\ "language").text.trim
    }
    else ""
  }

  def getDefaultIsoForLanguage(language: String): Locale = {
    new Locale(language)
  }

  def convertDefaultToISO3Language(defaultLanguage: Locale): String = {
    defaultLanguage.getISO3Language
  }

  def setDdmAccessRights(): String = {
    "NO_ACCESS".trim
  }

  def setDdmAvailable(): String = {
    Format.format(Calendar.getInstance().getTime)
  }

  def setDdmAudience(): String = {
    "D37000".trim
  }

  def createInfoPerProject(projectName: String, dir: String, userDefinedDataPath:String): Unit = {

    var dirList: List[String] = directoryList(dir, userDefinedDataPath)

    val parsedMetadata: NodeSeq = getParsedMetadata(projectName, dir, userDefinedDataPath)
    val parsedUser: NodeSeq = getParsedMetadata(projectName, dir, userDefinedDataPath)

    val dataset: String = extractDataset(projectName)
    val dcTitle: String = extractDcTitle(projectName, dir, userDefinedDataPath)
    val depositorId: String = extractDepositorId(projectName, dir, userDefinedDataPath)
    val dcxCreatorOrganization: String = extractDcxCreatorOrganization(projectName, dir, userDefinedDataPath)
    val ddmCreated: String = extractDdmCreated(projectName, dir, userDefinedDataPath)
    val dctRightsHolder: String = extractDctRightsHolder(projectName, dir, userDefinedDataPath)
    val dcDescription: String = setDcDescription()
    val dcSubject: String = setDcSubject()

    val dcElementTypes: List[String] = extractListOfDcElementTypes(projectName, dir, userDefinedDataPath)
    val dcObjectTypes: List[String] = extractListOfDcObjectTypes(projectName, dir, userDefinedDataPath)
    val dcTypeMetadata: List[String] = extractListOfDcTypesMetadata(projectName, dir, userDefinedDataPath)
    val dcCategory: String = extractDcCategory(projectName, dir, userDefinedDataPath)
    val dcTaxons: List[String] = extractListOfDcTaxons(projectName, dir, userDefinedDataPath)
    val dctTemporal: String = extractTimeRange(projectName, dir, userDefinedDataPath)
    val dcxSpatialScheme: String = setDcxSpatialScheme()
    val dcxSpatialX: String = extractDcxSpatialX(projectName, dir, userDefinedDataPath)
    val dcxSpatialY: String = extractDcxSpatialY(projectName, dir, userDefinedDataPath)
    val dcIdentifierSid: String = extractDcIdentifierSid(projectName, dir, userDefinedDataPath)
    val dcIdentifierIdentifier: String = extractDcIdentifierIdentifier(projectName, dir, userDefinedDataPath)
    val dcxRelationQualifier: String = setDcxRelationQualifier()
    val dcxRelationTitle: String = setDcxRelationTitle()
    val dcxRelationLink: String = extractDcxRelationLink(projectName, dir, userDefinedDataPath)
    val dcxRelationQualifier_2: String = setDcxRelationQualifier_2()
    val dcxRelationTitle_2: String = extractDcxRelationTitle_2(projectName, dir, userDefinedDataPath)
    val dcType: String = setDcType()
    val language: String = extractLanguage(projectName, dir, userDefinedDataPath)
    val isoDefault: Locale = getDefaultIsoForLanguage(language)
    val languageISO3: String = convertDefaultToISO3Language(isoDefault)
    val ddmAccessRights: String = setDdmAccessRights()
    val ddmAvailable: String = setDdmAvailable()
    val ddmAudience: String = setDdmAudience()

    val setOfCandidatesForDcSubject: Seq[String] = Seq(dcSubject) ++ dcElementTypes ++ dcObjectTypes ++ dcTypeMetadata ++ dcTaxons ++ Seq(dcCategory)
    val listDcSubject = createListDcSubject(List(), setOfCandidatesForDcSubject)

    var listDataset: List[String] = List()

    val listDcTitle: List[String] = List(dcTitle)
    val listDepositorId: List[String] = List(depositorId)
    val listDcxCreatorOrg: List[String] = List(dcxCreatorOrganization)
    val listDdmCreated: List[String] = List(ddmCreated)
    val listDctRightsHolder: List[String] = List(dctRightsHolder)
    val listDcDescription: List[String] = List(dcDescription)
    val listDctTemporal: List[String] = List(dctTemporal)
    val listDcxSpatialScheme: List[String] = List(dcxSpatialScheme)
    val listDcxSpatialX: List[String] = List(dcxSpatialX)
    val listDcxSpatialY: List[String] = List(dcxSpatialY)
    val listDcIdentifier: List[String] = List(dcIdentifierSid, dcIdentifierIdentifier)
    val listDcxRelationQualifier: List[String] = List(dcxRelationQualifier, dcxRelationQualifier_2)
    val listDcxRelationTitle: List[String] = List(dcxRelationTitle, dcxRelationTitle_2)
    val listDcxRelationLink: List[String] = List(dcxRelationLink)
    val listDcType: List[String] = List(dcType)
    val listDcLanguage: List[String] = List(languageISO3)
    val listDdmAccessRights: List[String] = List(ddmAccessRights)
    val listDdmAvailable: List[String] = List(ddmAvailable)
    val listDdmAudience: List[String] = List(ddmAudience)

    val listDctAlternative: List[String] = List()
    val listDcxCreatorTitles: List[String] = List()
    val listDcxCreatorInitials: List[String] = List()
    val listDcxCreatorInsertions: List[String] = List()
    val listDcxCreatorSurname: List[String] = List()
    val listDcxCreatorDai: List[String] = List()
    val listDcxCreatorRole: List[String] = List()
    val listDcxContributorTitles: List[String] = List()
    val listDcxContributorInitials: List[String] = List()
    val listDcxContributorInsertions: List[String] = List()
    val listDcxContributorSurname: List[String] = List()
    val listDcxContributorDai: List[String] = List()
    val listDcxContributorOrganization: List[String] = List()
    val listDcxContributorRole: List[String] = List()
    val listDcPublisher: List[String] = List()
    val listDcSubjectScheme: List[String] = List()
    val listTemporalScheme: List[String] = List()
    val listDctSpatial: List[String] = List()
    val listDctSpatialNorth: List[String] = List()
    val listDctSpatialSouth: List[String] = List()
    val listDctSpatialEast: List[String] = List()
    val listDctSpatialWest: List[String] = List()
    val listDcIdentifierType: List[String] = List()
    val listDcFormat: List[String] = List()
    val listDcSource: List[String] = List()
    val listSfDomain: List[String] = List()
    val listSfUser: List[String] = List()
    val listSfCollection: List[String] = List()
    val listAvSubtitles: List[String] = List()
    val listAvFilePath: List[String] = List()
    val listAvSubtitlesLanguage: List[String] = List()
    val listSfPlayMode: List[String] = List()
    val listDctDate: List[String] = List()
    val listDctDateQualifier: List[String] = List()
    val listFilePath: List[String] = List()
    val listFileTitle: List[String] = List()
    val listFileAccessibility: List[String] = List()

    var multiValueMap: Map[String, List[String]] = Map(
      "DATASET" -> listDataset,
      "DC_TITLE" -> listDcTitle,
      "DEPOSITOR_ID" -> listDepositorId,
      "DCX_CREATOR_ORGANIZATION" -> listDcxCreatorOrg,
      "DDM_CREATED" -> listDdmCreated,
      "DCT_RIGHTSHOLDER" -> listDctRightsHolder,
      "DC_DESCRIPTION" -> listDcDescription,
      "DC_SUBJECT" -> listDcSubject,
      "DCT_TEMPORAL" -> listDctTemporal,
      "DCX_SPATIAL_SCHEME" -> listDcxSpatialScheme,
      "DCX_SPATIAL_X" -> listDcxSpatialX,
      "DCX_SPATIAL_Y" -> listDcxSpatialY,
      "DC_IDENTIFIER" -> listDcIdentifier,
      "DCX_RELATION_QUALIFIER" -> listDcxRelationQualifier,
      "DCX_RELATION_TITLE" -> listDcxRelationTitle,
      "DCX_RELATION_LINK" -> listDcxRelationLink,
      "DC_TYPE" -> listDcType,
      "DC_LANGUAGE" -> listDcLanguage,
      "DDM_ACCESSRIGHTS" -> listDdmAccessRights,
      "DDM_AVAILABLE" -> listDdmAvailable,
      "DDM_AUDIENCE" -> listDdmAudience,
      //TODO The following columns do not contain any info.
      //TODO We should decide whether to add any info to these columns
      //TODO and which information to add if we need to fill in these columns
      "DCT_ALTERNATIVE" -> listDctAlternative,
      "DCX_CREATOR_TITLES" -> listDcxCreatorTitles,
      "DCX_CREATOR_INITIALS" -> listDcxCreatorInitials,
      "DCX_CREATOR_INSERTIONS" -> listDcxCreatorInsertions,
      "DCX_CREATOR_SURNAME" -> listDcxCreatorSurname,
      "DCX_CREATOR_DAI" -> listDcxCreatorDai,
      "DCX_CREATOR_ROLE" -> listDcxCreatorRole,
      "DCX_CONTRIBUTOR_TITLES" -> listDcxContributorTitles,
      "DCX_CONTRIBUTOR_INITIALS" -> listDcxContributorInitials,
      "DCX_CONTRIBUTOR_INSERTIONS" -> listDcxContributorInsertions,
      "DCX_CONTRIBUTOR_SURNAME" -> listDcxContributorSurname,
      "DCX_CONTRIBUTOR_DAI" -> listDcxContributorDai,
      "DCX_CONTRIBUTOR_ORGANIZATION" -> listDcxContributorOrganization,
      "DCX_CONTRIBUTOR_ROLE" -> listDcxContributorRole,
      "DC_PUBLISHER" -> listDcPublisher,
      "DC_SUBJECT_SCHEME" -> listDcSubjectScheme,
      "DCT_TEMPORAL_SCHEME" -> listTemporalScheme,
      "DCT_SPATIAL" -> listDctSpatial,
      "DCX_SPATIAL_NORTH" -> listDctSpatialNorth,
      "DCX_SPATIAL_SOUTH" -> listDctSpatialSouth,
      "DCX_SPATIAL_EAST" -> listDctSpatialEast,
      "DCX_SPATIAL_WEST" -> listDctSpatialWest,
      "DC_IDENTIFIER_TYPE" -> listDcIdentifierType,
      "DC_FORMAT" -> listDcFormat,
      "DC_SOURCE" -> listDcSource,
      "SF_DOMAIN" -> listSfDomain,
      "SF_USER" -> listSfUser,
      "SF_COLLECTION" -> listSfCollection,
      "AV_SUBTITLES" -> listAvSubtitles,
      "AV_FILE_PATH" -> listAvFilePath,
      "AV_SUBTITLES_LANGUAGE" -> listAvSubtitlesLanguage,
      "SF_PLAY_MODE" -> listSfPlayMode,
      "DCT_DATE" -> listDctDate,
      "DCT_DATE_QUALIFIER" -> listDctDateQualifier,
      "FILE_PATH" -> listFilePath,
      "FILE_TITLE" -> listFileTitle,
      "FILE_ACCESSIBILITY" -> listFileAccessibility

    )

    var maxListLength = maxLengthOfList(multiValueMap)

    var multiValueMapCsv: scala.collection.mutable.Map[String, List[String]] = multiValueMap

    for (j <- 0 until maxListLength) {
      for (k <- multiValueMap.keys) {
        if (k.contentEquals("DATASET")) {
          if (multiValueMap("DATASET").slice(j, j + 1).isEmpty) {
            var newList = multiValueMapCsv("DATASET").repr ++ List(dataset)
            multiValueMapCsv = multiValueMapCsv ++ scala.collection.mutable.Map("DATASET" -> newList)
          }
        }
        if (!k.contentEquals("DATASET")) {
          if (multiValueMap(k).slice(j, j + 1).isEmpty) {
            var newList = multiValueMapCsv(k).repr ++ List("")
            multiValueMapCsv = multiValueMapCsv ++ scala.collection.mutable.Map(k -> newList)
          }
        }
      }
    }

    for (j <- 0 until maxListLength) {
      //TODO printer.printRecord and csvPrinterToFile.printRecord didn't allow an iterable i like "aList.foreach(i => .." inside.
      //TODO thus I generated a comma seperated single string consisting of the elements I would like to print in a row of the csv
      //TODO and printed them to the associated row by using a CSVParser on the string I created

      var rowString = "\"\"" + ","
      if (multiValueMapCsv(csvFormat.getHeader.apply(0)).slice(j, j + 1).head.nonEmpty) {
        rowString = "\"" + multiValueMapCsv(csvFormat.getHeader.apply(0)).slice(j, j + 1).head + "\"" + ","
      }
      for (i <- 1 until csvFormat.getHeader.length) {
        if (multiValueMapCsv(csvFormat.getHeader.apply(i)).slice(j, j + 1).head.nonEmpty) {
          var a = "\"" + multiValueMapCsv(csvFormat.getHeader.apply(i)).slice(j, j + 1).head + "\"" + ","
          if (i == csvFormat.getHeader.length - 1)
            a = "\"" + multiValueMapCsv(csvFormat.getHeader.apply(i)).slice(j, j + 1).head + "\""
          rowString = rowString ++ a
        }
        if (multiValueMapCsv(csvFormat.getHeader.apply(i)).slice(j, j + 1).head.isEmpty) {
          var a = " " + ","
          if (i == csvFormat.getHeader.length - 1)
            a = "  "
          rowString = rowString ++ a
        }
      }

      var formatRecords: CSVFormat = CSVFormat.RFC4180.withDelimiter(',').withSkipHeaderRecord().withAllowMissingColumnNames()

      var records= CSVParser.parse(rowString, formatRecords)

      printer.printRecords(records)

      csvPrinterToFile.printRecords(CSVParser.parse(rowString, csvFormat))

    }

    csvPrinterToFile.flush()
    printer.flush()

  }


  def createCsvReportFromDccdExport(dir: String, userDefinedDataPath:String): String = {
    var dirList: List[String] = getListOfSubDirectories(directoryPath(dir, userDefinedDataPath)).toList
    dirList.foreach { i =>
      createInfoPerProject(i, dir, userDefinedDataPath)
    }
    printer.close()
    out.toString

  }

  def createFullReport(userDefinedDataPath:String): Try[String] = {
    createCsvReportFromDccdExport("projects", userDefinedDataPath)
    System.out.print(out.toString)
    Try {
      "full report"
    }
  }

}
