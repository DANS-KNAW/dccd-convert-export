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

import java.util.GregorianCalendar

import nl.knaw.dans.dccd._
import nl.knaw.dans.lib.error._
import org.apache.commons.csv.{ CSVParser, ExtendedBufferedReader }
import org.scalatest.{ FlatSpec, Matchers, _ }

import scala.collection.mutable
import scala.io.Source
import scala.util.{ Failure, Success, Try }
import scala.xml.XML
import scala.xml.parsing.ConstructingParser.fromSource


class dccdSpec extends FlatSpec with Matchers with CustomMatchers with BeforeAndAfterAll {

  "getListOfSubDirectories(\"./src/test/resources/data/projects\")" should "be non-empty" in {
    getListOfSubDirectories("./src/test/resources/data/projects").isEmpty shouldBe false
  }

  it should "create an Array[String] containing the names of data files in the projects directory" in {
    var array: Array[String] = Array()
    array = getListOfSubDirectories("./src/test/resources/data/projects")
    //TODO The elements of the following array are sorted according to increasing nbrs considering the first two digits.
    //TODO These are the data files in my "./src/test/resources/data/projects" directory.
    //TODO I haven't shared these test files in my pull request
    //TODO I can share them if we decide to use these set of data files as test resources
    //TODO Otherwise I will change the following content.
    array shouldBe Array("dccd_1605", "dccd_2137", "dccd_2554", "dccd_2693", "dccd_3045", "dccd_3887", "dccd_5457", "dccd_6152", "dccd_6856", "dccd_93")
  }

  it should "throw NullPointerException if the projects directory is empty" in {
    def getListErrorCheck() = {
      //TODO projects4 is a file which does not exist in "./src/test/resources/data/"
      Try { getListOfSubDirectories("./src/test/resources/data/projects4") }
      match {
        case Failure(_) => throw new java.lang.NullPointerException("NullPointerException thrown")
        case Success(_) => "success"
      }
    }

    a[NullPointerException] should be thrownBy { getListErrorCheck() }
  }

  "getListOfSubDirectories(\"./src/test/resources/data/projects\").apply(0)" should
    "throw ArrayIndexOutOfBoundsException if the projects directory is empty" in {
    def getListErrorCheck() = {
      //TODO projects3 is an empty directory which I've created in "./src/test/resources/data/"
      //TODO I haven't shared it in my pull request
      Try { getListOfSubDirectories("./src/test/resources/data/projects3").apply(0) }
      match {
        case Failure(_) => throw new java.lang.ArrayIndexOutOfBoundsException("ArrayIndexOutOfBoundsException thrown")
        case Success(_) => "success"
      }
    }

    a[ArrayIndexOutOfBoundsException] should be thrownBy { getListErrorCheck() }
  }

  "Format.format" should "reformat the date 2011-02-10T15:28:42.096Z as 2011-02-10" in {
    Format.format(Format.parseObject("2011-02-10T15:28:42.096Z")) shouldBe "2011-02-10"
  }

  "parseNoWS" should "parse an xml file" in {
    def parse() = {
      (parseNoWS(XML.loadFile("./src/test/resources/data/projects/dccd_93/administrative/project_metadata.xml").toString()) \\ "abc").text
    }

    def parseErrorCheck() = {
      Try { parse() } match {
        case Failure(_) => "failure"
        case Success(_) => "success"
      }
    }

    parseErrorCheck() shouldBe "success"
  }


  "csvFormat.getHeaders().head" should "give the first header of the csv file" in {
    csvFormat.getHeader.head shouldBe ("DATASET")
  }

  "csvFormat.getHeaders().tail" should "contain the header of any column of the csv file" in {
    csvFormat.getHeader.tail should contain("DC_TITLE")
    csvFormat.getHeader.tail should contain("DC_LANGUAGE")
    csvFormat.getHeader.tail should contain("DCT_ALTERNATIVE")
    csvFormat.getHeader.tail should contain("DCX_CREATOR_TITLES")
    csvFormat.getHeader.tail should contain("DCX_CREATOR_INITIALS")
    csvFormat.getHeader.tail should contain("DCX_CREATOR_INSERTIONS")
    csvFormat.getHeader.tail should contain("DCX_CREATOR_SURNAME")
    csvFormat.getHeader.tail should contain("DCX_CREATOR_DAI")
    csvFormat.getHeader.tail should contain("DCX_CREATOR_ROLE")
    csvFormat.getHeader.tail should contain("DCX_CREATOR_ORGANIZATION")
    csvFormat.getHeader.tail should contain("DCX_CONTRIBUTOR_TITLES")
    csvFormat.getHeader.tail should contain("DCX_CONTRIBUTOR_INITIALS")
    csvFormat.getHeader.tail should contain("DCX_CONTRIBUTOR_INSERTIONS")
    csvFormat.getHeader.tail should contain("DCX_CONTRIBUTOR_SURNAME")
    csvFormat.getHeader.tail should contain("DCX_CONTRIBUTOR_DAI")
    csvFormat.getHeader.tail should contain("DCX_CONTRIBUTOR_ORGANIZATION")
    csvFormat.getHeader.tail should contain("DCX_CONTRIBUTOR_ROLE")
    csvFormat.getHeader.tail should contain("DDM_CREATED")
    csvFormat.getHeader.tail should contain("DCT_RIGHTSHOLDER")
    csvFormat.getHeader.tail should contain("DC_PUBLISHER")
    csvFormat.getHeader.tail should contain("DC_DESCRIPTION")
    csvFormat.getHeader.tail should contain("DC_SUBJECT_SCHEME")
    csvFormat.getHeader.tail should contain("DC_SUBJECT")
    csvFormat.getHeader.tail should contain("DCT_TEMPORAL_SCHEME")
    csvFormat.getHeader.tail should contain("DCT_TEMPORAL")
    csvFormat.getHeader.tail should contain("DCT_SPATIAL")
    csvFormat.getHeader.tail should contain("DCX_SPATIAL_SCHEME")
    csvFormat.getHeader.tail should contain("DCX_SPATIAL_X")
    csvFormat.getHeader.tail should contain("DCX_SPATIAL_Y")
    csvFormat.getHeader.tail should contain("DCX_SPATIAL_NORTH")
    csvFormat.getHeader.tail should contain("DCX_SPATIAL_SOUTH")
    csvFormat.getHeader.tail should contain("DCX_SPATIAL_EAST")
    csvFormat.getHeader.tail should contain("DCX_SPATIAL_WEST")
    csvFormat.getHeader.tail should contain("DC_IDENTIFIER_TYPE")
    csvFormat.getHeader.tail should contain("DC_IDENTIFIER")
    csvFormat.getHeader.tail should contain("DCX_RELATION_QUALIFIER")
    csvFormat.getHeader.tail should contain("DCX_RELATION_TITLE")
    csvFormat.getHeader.tail should contain("DCX_RELATION_LINK")
    csvFormat.getHeader.tail should contain("DC_TYPE")
    csvFormat.getHeader.tail should contain("DC_FORMAT")
    csvFormat.getHeader.tail should contain("DC_LANGUAGE")
    csvFormat.getHeader.tail should contain("DC_SOURCE")
    csvFormat.getHeader.tail should contain("DDM_ACCESSRIGHTS")
    csvFormat.getHeader.tail should contain("DDM_AVAILABLE")
    csvFormat.getHeader.tail should contain("DDM_AUDIENCE")
    csvFormat.getHeader.tail should contain("DEPOSITOR_ID")
    csvFormat.getHeader.tail should contain("SF_DOMAIN")
    csvFormat.getHeader.tail should contain("SF_USER")
    csvFormat.getHeader.tail should contain("SF_COLLECTION")
    csvFormat.getHeader.tail should contain("AV_SUBTITLES")
    csvFormat.getHeader.tail should contain("AV_FILE_PATH")
    csvFormat.getHeader.tail should contain("AV_SUBTITLES_LANGUAGE")
    csvFormat.getHeader.tail should contain("SF_PLAY_MODE")
    csvFormat.getHeader.tail should contain("DCT_DATE")
    csvFormat.getHeader.tail should contain("DCT_DATE_QUALIFIER")
    csvFormat.getHeader.tail should contain("FILE_PATH")
    csvFormat.getHeader.tail should contain("FILE_TITLE")
    csvFormat.getHeader.tail should contain("FILE_ACCESSIBILITY")
  }

  def CSVReader(absPath: String, delimiter: String): List[scala.Seq[String]] = {
    Source.fromFile(absPath).getLines().toList map (_.split("""\""" + delimiter).toSeq)
  }

  "CSVReader(\"./src/test/resources/data/UserIdEasyMap.csv\", \",\")" should
    "throw a NullPointer exception if the csv file does not exists" in {
    def getCsvFile() = {
      Try { CSVReader("./src/test/resources/data/UserIdEasyMap.csv", ",") }
      match {
        case Failure(_) => throw new java.lang.NullPointerException("NullPointerException thrown")
        case Success(_) => "success"
      }
    }

    if (getCsvFile() != "success")
      a[NullPointerException] should be thrownBy { getCsvFile() }

  }

  "CSVReader" should "read a csv file and provide a list of sequences where each line is a sequence having a size of 2" in {

    CSVReader("./src/test/resources/data/UserIdEasyMap.csv", ",").foreach(i => { i.head.nonEmpty shouldBe true })

    CSVReader("./src/test/resources/data/UserIdEasyMap.csv", ",").foreach(i => { i.apply(1).nonEmpty shouldBe true })

    CSVReader("./src/test/resources/data/UserIdEasyMap.csv", ",").foreach(i => { a[ArrayIndexOutOfBoundsException] shouldBe thrownBy { i.apply(2).nonEmpty } })

  }

  "mapDepositorIdActualEasyUserId" should
    "provide a map with keys corresponding to depositorIds and values to their associated actual easyUsers" in {

    mapDepositorIdActualEasyUserId("./src/test/resources/data/UserIdEasyMap.csv").keys.nonEmpty shouldBe true
    mapDepositorIdActualEasyUserId("./src/test/resources/data/UserIdEasyMap.csv")("DEPOSITOR_ID                   ").leftSideValue shouldBe "EASYuserId"

  }

  "createListDcSubject" should "create a list from a set of candidates" in {
    createListDcSubject(List(), mutable.Seq("a", "b", "c")) shouldBe List("a", "b", "c")
  }


}











