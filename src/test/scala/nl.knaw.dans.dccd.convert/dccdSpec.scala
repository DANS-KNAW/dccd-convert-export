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

import scala.xml.parsing.ConstructingParser.fromSource


class dccdSpec extends FlatSpec with Matchers with CustomMatchers with BeforeAndAfterAll {

  "getListOfSubDirectories" should "get list of subdirectories" in {
    getListOfSubDirectories("./src/test/resources/data/projects").isEmpty shouldBe false
  }

  "csvFormat.getHeaders().head" should "give the first header of the csv file" in {
    csvFormat.getHeader.head shouldBe ("DATASET")
  }

  "csvFormat.getHeaders().tail" should "contain the header of any column of the csv file" in {
    csvFormat.getHeader.tail should contain("DCT_ALTERNATIVE")
    csvFormat.getHeader.tail should contain("DC_TITLE")
    csvFormat.getHeader.tail should contain("DC_LANGUAGE")
  }


}











