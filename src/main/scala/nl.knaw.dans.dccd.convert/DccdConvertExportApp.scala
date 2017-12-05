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

import org.apache.commons.csv.{ CSVFormat, CSVParser, CSVPrinter, ExtendedBufferedReader }

import scala.io.Source.fromString
import scala.util.Try
import scala.xml.parsing.ConstructingParser.fromSource
import scala.xml.{ Elem, NodeSeq, TopScope, XML }


//class DccdConvertExportApp(wiring: ApplicationWiring)  {
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

  /* Previous Columns
  val csvFormat: CSVFormat = CSVFormat.RFC4180.withHeader("DATASET", "DC_TITLE", "DEPOSITOR_ID", "DCX_CREATOR_ORGANIZATION", "DDM_CREATED", "DCT_RIGHTSHOLDER", "DC_DESCRIPTION", "DC_SUBJECT", "DCT_TEMPORAL",  "DCX_SPATIAL_SCHEME", "DCX_SPATIAL_X", "DCX_SPATIAL_Y", "DC_IDENTIFIER", "DCX_RELATION_QUALIFIER", "DCX_RELATION_TITLE", "DCX_RELATION_LINK", "DC_TYPE", "DC_LANGUAGE", "DDM_ACCESSRIGHTS", "DDM_AVAILABLE", "DDM_AUDIENCE").withDelimiter(',')
  */

  val csvFormat: CSVFormat = CSVFormat.RFC4180.withHeader(
    "DATASET",
    "DC_TITLE",
    //"DEPOSITOR_ID",
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

  val csvPrinterToFile = new CSVPrinter(new FileWriter("./data/dccdConvert" + ".csv"), csvFormat.withDelimiter(','))

  private def parse(s: String) = fromSource(fromString(s), preserveWS = true).element(TopScope)

  private def parseNoWS(s: String) = fromSource(fromString(s), preserveWS = false).element(TopScope)

  //Build reader instance
  //Read data.csv
  //Default seperator is comma
  //Default quote character is double quote
  //Start reading from line number 2 (line numbers start from zero)
  // val reader: CSVReader = new CSVReader(new FileReader("./data/UserIdEasyMap.csv"), ',' , '"' , 1)
  //var nextLine = null
  //while ( { (nextLine = reader.readNext) != null }) if (nextLine != null) { //Verifying the read data here
  //  System.out.println(util.Arrays.toString(nextLine))
  //}
  //println(reader.)



  val Format = new SimpleDateFormat("yyyy-MM-dd")
  var range: String = ""
  val valA: String = " AD"
  val valB: String = " BC"



  def createInfoPerProject(projectName:String, dir:String): Unit = {
    var dirPath: Path = getPath(dir)
    var dirList: List[String] = getListOfSubDirectories(getPath(dir).toString).toList
    var dirrMetadata: String = dirPath.toString + "/" + projectName + "/administrative/project_metadata.xml"
    var dirrUser: String = dirPath.toString + "/" + projectName + "/administrative/user.xml"
    var metadata: Elem = XML.loadFile(dirrMetadata.toString)
    var parsedMetadata: NodeSeq = parseNoWS(metadata.toString())
    var user: Elem = XML.loadFile(dirrUser.toString)
    var parsedUser: NodeSeq = parseNoWS(user.toString())

    var dataset: String = projectName.trim
    //var dataset: String = (parsedMetadata \\ "sid").text.stripPrefix("dccd:")

    var dcTitle: String = (parsedMetadata \\ "title").text.trim
    var depositorId: String = (parsedUser \\ "id").text.trim
    var dcxCreatorOrganization: String = (parsedMetadata \\ "ownerOrganizationId").text.trim

    var date: String = (parsedMetadata \\ "stateChanged").text.trim
    var parsedDate: AnyRef = Format.parseObject(date)
    var ddmCreated: String = Format.format(parsedDate)

    var dctRightsHolder: String = dcxCreatorOrganization
    var dcDescription: String = "Dendrochronological project".trim

    var dcSubject: String = "Dendrochronology".trim


    /* WE CAN USE STH LIKE THIS IF THERE ARE SEVERAL SUB-ELEMENTS OF AN ELEMENT IN THE XML FILE, FOR THE RELEVANT COLUMNS */
    /*for (e <- metadata.child) {
        if (e.label == "elementTypes"){
           for(ee <- e.child){
               println((ee \\ "elementType").text)
           }
        }
    }*/


    var dcElementType: String = ""
    if ((parsedMetadata \\ "elementType").text.nonEmpty) {
       dcElementType = "dccd_element_type: " + (parsedMetadata \\ "elementType").text.trim
    }

    var dcObjectType: String = ""
    if((parsedMetadata \\ "objectType").text.nonEmpty){
      dcObjectType = "dccd_object_type: " + (parsedMetadata \\ "objectType").text.trim
    }

    var dcTypeMetadata : String = ""
    if((parsedMetadata \\ "type").text.nonEmpty){
      dcTypeMetadata = "dccd_type: " + (parsedMetadata \\ "type").text.trim
    }

    var dcCategory : String = ""
    if((parsedMetadata \\ "category").text.nonEmpty){
      dcCategory = "dccd_category: " + (parsedMetadata \\ "category").text.trim
    }

    var dcTaxon : String = ""
    if((parsedMetadata \\ "taxon").text.nonEmpty){
      dcTaxon = "dccd_taxon: " + (parsedMetadata \\ "taxon").text.trim
    }


    def timeRange(parsedData: NodeSeq): String = {
      val parsedData = parsedMetadata
      val first = (parsedData \\ "firstYear").text.trim
      val last = (parsedData \\ "lastYear").text.trim
      if (first.contains("-").equals(false) && last.contains("-").equals(false)) {
        if (first.nonEmpty && last.nonEmpty)
          range = first + "-" + last + valA
        if (first.nonEmpty && last.isEmpty) //???????????
          range = first + "-" + "" //first + valA ?????
        if (first.isEmpty && last.nonEmpty) //????????????
          range = "" + "-" + last // + valA ??????????
      }
      if (first.contains("-") && last.contains("-")) {
        if (first.nonEmpty && last.nonEmpty)
          range = ((first.toInt - 1) * (-1)).toString + "-" + ((last.toInt - 1) * (-1)).toString + valB
        if (first.nonEmpty && last.isEmpty) //??????????????????????????????????????
          range = ((first.toInt - 1) * (-1)).toString + valB + "-" + ""
        if (first.isEmpty && last.nonEmpty) //??????????????????????????????????????
          range = "" + "-" + ((last.toInt - 1) * (-1)).toString + valB
      }
      if (first.contains("-") && last.contains("-").equals(false)) {
        if (first.nonEmpty && last.nonEmpty)
          range = ((first.toInt - 1) * (-1)).toString + valB + "-" + last + valA
        if (first.nonEmpty && last.isEmpty) //??????????????????????????????????????
          range = ((first.toInt - 1) * (-1)).toString + valB + "-" + ""
        if (first.isEmpty && last.nonEmpty) //??????????????????????????????????????
          range = "" + "-" + last + valA

      }
      range

    }

    var dctTemporal: String = timeRange(parsedMetadata)

    var dcxSpatialScheme: String = "degrees".trim

    var dcxSpatialX: String = ""
    if((parsedMetadata \\ "lat").text.nonEmpty)
       dcxSpatialX = (parsedMetadata \\ "lat").text.trim

    var dcxSpatialY: String = ""
    if((parsedMetadata \\ "lng").text.nonEmpty)
       dcxSpatialY = (parsedMetadata \\ "lng").text.trim

    var dcIdentifierSid: String =""
    if((parsedMetadata \\ "sid").text.nonEmpty)
        dcIdentifierSid = (parsedMetadata \\ "sid").text.trim

    var dcIdentifierIdentifier: String =""
    if((parsedMetadata \\ "identifier").text.nonEmpty)
        dcIdentifierIdentifier = (parsedMetadata \\ "identifier").text.trim

    var dcxRelationQualifier: String = " 'references' ".trim
    var dcxRelationTitle: String = "Digital Collaboratory for Cultural Dendrochronology (DCCD)".trim

    var dcxRelationLink: String =""
    if((parsedMetadata \\ "sid").text.nonEmpty)
        dcxRelationLink = "https://dendro.dans.knaw.nl/dccd/project/" + (parsedMetadata \\ "sid").text.trim

    var dcxRelationQualifier_2: String = " 'IsFormatOf' ".trim

    var dcxRelationTitle_2: String =""
    if((parsedMetadata \\ "sid").text.nonEmpty)
        dcxRelationTitle_2 = (parsedMetadata \\ "sid").text.trim

    var dcType: String = "Dataset".trim

    var language: String =""
    if((parsedMetadata \\ "language").text.nonEmpty)
        language = (parsedMetadata \\ "language").text.trim

    var isoDefault: Locale = new Locale(language)
    var languageISO3: String = isoDefault.getISO3Language

    var ddmAccessRights: String = "NO_ACCESS".trim
    var now: Date = Calendar.getInstance().getTime
    var ddmAvailable: String = Format.format(now)

    var ddmAudience: String = "D37000".trim


    var listDcSubject : List[String] = List()
    var listDataset   : List[String] = List()

    if(dcSubject.nonEmpty)
       listDcSubject = listDcSubject ::: List(dcSubject)
    if(dcElementType.nonEmpty)
       listDcSubject = listDcSubject ::: List(dcElementType)
    if(dcObjectType.nonEmpty)
       listDcSubject = listDcSubject ::: List(dcObjectType)
    if(dcTypeMetadata.nonEmpty)
       listDcSubject = listDcSubject ::: List(dcTypeMetadata)
    if(dcCategory.nonEmpty)
       listDcSubject = listDcSubject ::: List(dcCategory)
    if(dcTaxon.nonEmpty)
       listDcSubject = listDcSubject ::: List(dcTaxon)


    var listDcTitle : List[String] = List(dcTitle)
    var listDepositorId : List[String] = List(depositorId)
    var listDcxCreatorOrg : List[String] = List(dcxCreatorOrganization)
    var listDdmCreated : List[String] = List(ddmCreated)
    var listDctRightsHolder : List[String] = List(dctRightsHolder)
    var listDcDescription : List[String] = List(dcDescription)
    var listDctTemporal : List[String] = List(dctTemporal)
    var listDcxSpatialScheme: List[String] = List(dcxSpatialScheme)
    var listDcxSpatialX: List[String] = List(dcxSpatialX)
    var listDcxSpatialY: List[String] = List(dcxSpatialY)
    var listDcIdentifier: List[String] = List(dcIdentifierSid, dcIdentifierIdentifier)
    var listDcxRelationQualifier: List[String] = List(dcxRelationQualifier, dcxRelationQualifier_2)
    var listDcxRelationTitle: List[String] = List(dcxRelationTitle, dcxRelationTitle_2)
    var listDcxRelationLink: List[String] = List(dcxRelationLink)
    var listDcType: List[String] = List(dcType)
    var listDcLanguage: List[String] = List(languageISO3)
    var listDdmAccessRights: List[String] = List(ddmAccessRights)
    var listDdmAvailable: List[String] = List(ddmAvailable)
    var listDdmAudience: List[String] = List(ddmAudience)

    var listDctAlternative   : List[String] = List()
    var listDcxCreatorTitles : List[String] = List()
    var listDcxCreatorInitials: List[String] = List()
    var listDcxCreatorInsertions: List[String] = List()
    var listDcxCreatorSurname: List[String] = List()
    var listDcxCreatorDai: List[String] = List()
    var listDcxCreatorRole: List[String] = List()
    var listDcxContributorTitles: List[String] = List()
    var listDcxContributorInitials: List[String] = List()
    var listDcxContributorInsertions: List[String] = List()
    var listDcxContributorSurname: List[String] = List()
    var listDcxContributorDai: List[String] = List()
    var listDcxContributorOrganization: List[String] = List()
    var listDcxContributorRole: List[String] = List()
    var listDcPublisher: List[String] = List()
    var listDcSubjectScheme: List[String] = List()
    var listTemporalScheme: List[String] = List()
    var listDctSpatial: List[String] = List()
    var listDctSpatialNorth: List[String] = List()
    var listDctSpatialSouth: List[String] = List()
    var listDctSpatialEast: List[String] = List()
    var listDctSpatialWest: List[String] = List()
    var listDcIdentifierType: List[String] = List()
    var listDcFormat: List[String] = List()
    var listDcSource: List[String] = List()
    var listSfDomain: List[String] = List()
    var listSfUser: List[String] = List()
    var listSfCollection: List[String] = List()
    var listAvSubtitles: List[String] = List()
    var listAvFilePath: List[String] = List()
    var listAvSubtitlesLanguage: List[String] = List()
    var listSfPlayMode: List[String] = List()
    var listDctDate: List[String] = List()
    var listDctDateQualifier: List[String] = List()
    var listFilePath: List[String] = List()
    var listFileTitle: List[String] = List()
    var listFileAccessibility: List[String] = List()


    var multiValueMap: Map[String, List[String]] = Map(
      "DATASET" -> listDataset,
      "DC_TITLE" -> listDcTitle,
      "DEPOSITOR_ID" -> listDepositorId ,
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
       /* The following columns do not contain any data !!!*/
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
      "DCX_SPATIAL_NORTH"-> listDctSpatialNorth,
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
       "FILE_ACCESSIBILITY" ->listFileAccessibility

    )


    var maxListLength: Int = 0
    for (k <- multiValueMap.keys) {
      if (multiValueMap(k).length > maxListLength) {
        maxListLength = multiValueMap(k).length
      }
    }

    var multiValueMapCsv: Map[String, List[String]] = multiValueMap

    for(j <- 0 until maxListLength) {
      for(k <- multiValueMap.keys){
          if(k.contentEquals("DATASET")){
            if(multiValueMap("DATASET").slice(j,j+1).isEmpty) {
              var newList =  multiValueMapCsv("DATASET").repr ++ List(dataset)
              multiValueMapCsv = multiValueMapCsv ++  Map("DATASET" -> newList)
            }
          }
          if(!k.contentEquals("DATASET")){
             if(multiValueMap(k).slice(j,j+1).isEmpty){
               var newList =  multiValueMapCsv(k).repr ++ List("")
               multiValueMapCsv = multiValueMapCsv ++  Map(k -> newList)
            }
          }
      }
    }




    for(j <- 0 until maxListLength) {
      printer.printRecord(
        multiValueMapCsv("DATASET").slice(j,j+1).head,
        multiValueMapCsv("DC_TITLE").slice(j,j+1).head,
        multiValueMapCsv("DCT_ALTERNATIVE").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_TITLES").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_INITIALS").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_INSERTIONS").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_SURNAME").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_DAI").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_ROLE").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_ORGANIZATION").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_TITLES").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_INITIALS").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_INSERTIONS").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_SURNAME").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_DAI").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_ORGANIZATION").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_ROLE").slice(j,j+1).head,
        multiValueMapCsv("DDM_CREATED").slice(j,j+1).head,
        multiValueMapCsv("DCT_RIGHTSHOLDER").slice(j,j+1).head,
        multiValueMapCsv("DC_PUBLISHER").slice(j,j+1).head,
        multiValueMapCsv("DC_DESCRIPTION").slice(j,j+1).head,
        multiValueMapCsv("DC_SUBJECT_SCHEME").slice(j,j+1).head,
        multiValueMapCsv("DC_SUBJECT").slice(j,j+1).head,
        multiValueMapCsv("DCT_TEMPORAL_SCHEME").slice(j,j+1).head,
        multiValueMapCsv("DCT_TEMPORAL").slice(j,j+1).head,
        multiValueMapCsv("DCT_SPATIAL").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_SCHEME").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_X").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_Y").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_NORTH").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_SOUTH").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_EAST").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_WEST").slice(j,j+1).head,
        multiValueMapCsv("DC_IDENTIFIER_TYPE").slice(j,j+1).head,
        multiValueMapCsv("DC_IDENTIFIER").slice(j,j+1).head,
        multiValueMapCsv("DCX_RELATION_QUALIFIER").slice(j,j+1).head,
        multiValueMapCsv("DCX_RELATION_TITLE").slice(j,j+1).head,
        multiValueMapCsv("DCX_RELATION_LINK").slice(j,j+1).head,
        multiValueMapCsv("DC_TYPE").slice(j,j+1).head,
        multiValueMapCsv("DC_FORMAT").slice(j,j+1).head,
        multiValueMapCsv("DC_LANGUAGE").slice(j,j+1).head,
        multiValueMapCsv("DC_SOURCE").slice(j,j+1).head,
        multiValueMapCsv("DDM_ACCESSRIGHTS").slice(j,j+1).head,
        multiValueMapCsv("DDM_AVAILABLE").slice(j,j+1).head,
        multiValueMapCsv("DDM_AUDIENCE").slice(j,j+1).head,
        multiValueMapCsv("DEPOSITOR_ID").slice(j,j+1).head,
        multiValueMapCsv("SF_DOMAIN").slice(j,j+1).head,
        multiValueMapCsv("SF_USER").slice(j,j+1).head,
        multiValueMapCsv("SF_COLLECTION").slice(j,j+1).head,
        multiValueMapCsv("AV_SUBTITLES").slice(j,j+1).head,
        multiValueMapCsv("AV_FILE_PATH").slice(j,j+1).head,
        multiValueMapCsv("AV_SUBTITLES_LANGUAGE").slice(j,j+1).head,
        multiValueMapCsv("SF_PLAY_MODE").slice(j,j+1).head,
        multiValueMapCsv("DCT_DATE").slice(j,j+1).head,
        multiValueMapCsv("DCT_DATE_QUALIFIER").slice(j,j+1).head,
        multiValueMapCsv("FILE_PATH").slice(j,j+1).head,
        multiValueMapCsv("FILE_TITLE").slice(j,j+1).head,
        multiValueMapCsv("FILE_ACCESSIBILITY").slice(j,j+1).head
      )
      csvPrinterToFile.printRecord(
        multiValueMapCsv("DATASET").slice(j,j+1).head,
        multiValueMapCsv("DC_TITLE").slice(j,j+1).head,
        multiValueMapCsv("DCT_ALTERNATIVE").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_TITLES").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_INITIALS").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_INSERTIONS").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_SURNAME").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_DAI").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_ROLE").slice(j,j+1).head,
        multiValueMapCsv("DCX_CREATOR_ORGANIZATION").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_TITLES").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_INITIALS").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_INSERTIONS").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_SURNAME").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_DAI").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_ORGANIZATION").slice(j,j+1).head,
        multiValueMapCsv("DCX_CONTRIBUTOR_ROLE").slice(j,j+1).head,
        multiValueMapCsv("DDM_CREATED").slice(j,j+1).head,
        multiValueMapCsv("DCT_RIGHTSHOLDER").slice(j,j+1).head,
        multiValueMapCsv("DC_PUBLISHER").slice(j,j+1).head,
        multiValueMapCsv("DC_DESCRIPTION").slice(j,j+1).head,
        multiValueMapCsv("DC_SUBJECT_SCHEME").slice(j,j+1).head,
        multiValueMapCsv("DC_SUBJECT").slice(j,j+1).head,
        multiValueMapCsv("DCT_TEMPORAL_SCHEME").slice(j,j+1).head,
        multiValueMapCsv("DCT_TEMPORAL").slice(j,j+1).head,
        multiValueMapCsv("DCT_SPATIAL").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_SCHEME").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_X").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_Y").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_NORTH").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_SOUTH").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_EAST").slice(j,j+1).head,
        multiValueMapCsv("DCX_SPATIAL_WEST").slice(j,j+1).head,
        multiValueMapCsv("DC_IDENTIFIER_TYPE").slice(j,j+1).head,
        multiValueMapCsv("DC_IDENTIFIER").slice(j,j+1).head,
        multiValueMapCsv("DCX_RELATION_QUALIFIER").slice(j,j+1).head,
        multiValueMapCsv("DCX_RELATION_TITLE").slice(j,j+1).head,
        multiValueMapCsv("DCX_RELATION_LINK").slice(j,j+1).head,
        multiValueMapCsv("DC_TYPE").slice(j,j+1).head,
        multiValueMapCsv("DC_FORMAT").slice(j,j+1).head,
        multiValueMapCsv("DC_LANGUAGE").slice(j,j+1).head,
        multiValueMapCsv("DC_SOURCE").slice(j,j+1).head,
        multiValueMapCsv("DDM_ACCESSRIGHTS").slice(j,j+1).head,
        multiValueMapCsv("DDM_AVAILABLE").slice(j,j+1).head,
        multiValueMapCsv("DDM_AUDIENCE").slice(j,j+1).head,
        multiValueMapCsv("DEPOSITOR_ID").slice(j,j+1).head,
        multiValueMapCsv("SF_DOMAIN").slice(j,j+1).head,
        multiValueMapCsv("SF_USER").slice(j,j+1).head,
        multiValueMapCsv("SF_COLLECTION").slice(j,j+1).head,
        multiValueMapCsv("AV_SUBTITLES").slice(j,j+1).head,
        multiValueMapCsv("AV_FILE_PATH").slice(j,j+1).head,
        multiValueMapCsv("AV_SUBTITLES_LANGUAGE").slice(j,j+1).head,
        multiValueMapCsv("SF_PLAY_MODE").slice(j,j+1).head,
        multiValueMapCsv("DCT_DATE").slice(j,j+1).head,
        multiValueMapCsv("DCT_DATE_QUALIFIER").slice(j,j+1).head,
        multiValueMapCsv("FILE_PATH").slice(j,j+1).head,
        multiValueMapCsv("FILE_TITLE").slice(j,j+1).head,
        multiValueMapCsv("FILE_ACCESSIBILITY").slice(j,j+1).head
      )

    }







/*  PREVIOUS PRINTING FORMAT
    printer.printRecord( multiValueMap("DATASET").slice(1,2).head, dcTitle, depositorId, dcxCreatorOrganization, ddmCreated, dctRightsHolder, dcDescription,  dcSubject,     dctTemporal, dcxSpatialScheme, dcxSpatialX, dcxSpatialY, dcIdentifierSid,        dcxRelationQualifier,    dcxRelationTitle,   dcxRelationLink, dcType, languageISO3, ddmAccessRights, ddmAvailable, ddmAudience)
    printer.printRecord(dataset,    "",        "",         "",                    "",           "",             "",         dcElementType,     "",            "",                 "",        "",   dcIdentifierIdentifier,  dcxRelationQualifier_2,  dcxRelationTitle_2,       "",          "",        "",           "",               "",           "")
    printer.printRecord(dataset,    "",        "",         "",                    "",           "",             "",         dcObjectType,      "",            "",                 "",        "",          "",                        "",                  "",                  "",          "",        "",           "",               "",           "")
    printer.printRecord(dataset,    "",        "",         "",                    "",           "",             "",         dcTypeMetadata,    "",            "",                 "",        "",          "",                        "",                  "",                  "",          "",        "",           "",               "",           "")
    printer.printRecord(dataset,    "",        "",         "",                    "",           "",             "",         dcCategory,        "",            "",                 "",        "",          "",                        "",                  "",                  "",          "",        "",           "",               "",           "")
    printer.printRecord(dataset,    "",        "",         "",                    "",           "",             "",         dcTaxon,           "",            "",                 "",        "",          "",                        "",                  "",                  "",          "",        "",           "",               "",           "")

*/
/*
    csvPrinterToFile.printRecord(dataset, dcTitle, depositorId, dcxCreatorOrganization, ddmCreated, dctRightsHolder, dcDescription, dcSubject,   dctTemporal, dcxSpatialScheme, dcxSpatialX, dcxSpatialY,     dcIdentifierSid,     dcxRelationQualifier,    dcxRelationTitle,   dcxRelationLink, dcType, languageISO3, ddmAccessRights, ddmAvailable, ddmAudience)
    csvPrinterToFile.printRecord(dataset,   "",        "",                "",              "",           "",             "",      dcElementType,     "",          "",              "",            "",      dcIdentifierIdentifier, dcxRelationQualifier_2,  dcxRelationTitle_2,      "",            "",       "",           "",             "",          "")
    csvPrinterToFile.printRecord(dataset,   "",        "",                "",              "",           "",             "",      dcObjectType,      "",          "",              "",            "",              "",                   "",                    "",                  "",            "",       "",           "",             "",          "")
    csvPrinterToFile.printRecord(dataset,   "",        "",                "",              "",           "",             "",      dcTypeMetadata,    "",          "",              "",            "",              "",                   "",                    "",                  "",            "",       "",           "",             "",          "")
    csvPrinterToFile.printRecord(dataset,   "",        "",                "",              "",           "",             "",      dcCategory,        "",          "",              "",            "",              "",                   "",                    "",                  "",            "",       "",           "",             "",          "")
    csvPrinterToFile.printRecord(dataset,   "",        "",                "",              "",           "",             "",      dcTaxon,           "",          "",              "",            "",              "",                   "",                    "",                  "",            "",       "",           "",             "",          "")

    //csvPrinterToFile.printRecord(multiValueMap)
    */

    csvPrinterToFile.flush()
    printer.flush()

  }



  def createCsvReportFromDccdExport(dir: String): String = {
    var dirPath: Path = getPath(dir)
    var dirList: List[String] = getListOfSubDirectories(getPath(dir).toString).toList
    dirList.foreach { i =>
      createInfoPerProject(i, dir)
    }
    printer.close()
    out.toString

  }

  def createFullReport(depositor: Option[String] = None): Try[String] = {
    createCsvReportFromDccdExport("projects")
    System.out.print(out.toString)
    Try {
      "full report"
    }
  }

}
