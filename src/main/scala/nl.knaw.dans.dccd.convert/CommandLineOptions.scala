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

import nl.knaw.dans.dccd.{ dataPath, _ }
import org.rogach.scallop.{ ScallopConf, ScallopOption, Subcommand }


class CommandLineOptions(args: Array[String], configuration: Configuration) extends ScallopConf(args) {
  appendDefaultToDescription = true
  editBuilder(_.setHelpWidth(110))
  printedName = "dccd-convert-export"
  private val SUBCOMMAND_SEPARATOR = "---\n"
  val description: String = s"""Convert DCCD export to multi-deposit"""
  val synopsis: String =
    s"""
       |  $printedName dataPath [<the path of the data>]
       |  $printedName dataPath
       |
       |  Details:
       |
       |  $printedName dataPath [<the path of the data>]
       |   => Use this command to specify a path for the data.
       |
       |  $printedName dataPath
       |   => Use this command to use the default data path.
       |
       |   Use run.sh instead of $printedName if you are not on the VM
       |
       |   The default path is :
       |   > ./data/projects on your local machine
       |   > /vagrant/data/projects on the VM
       |
       |   To use the default path in any case, data must be saved into
       |   ./data/projects while dccd-convert-export is the current directory
      """.stripMargin

  version(s"$printedName v${ configuration.version }")
  banner(
    s"""
       |$description
       |
       |Usage:
       |
       |$synopsis
       |
       |Options:
       |""".stripMargin)
  //val url = opt[String]("someOption", noshort = true, descr = "Description of the option", default = app.someProperty)

  val data = new Subcommand("dataPath") {
    descr("takes dccd data from the given or default path and creates instructions.csv file")
    val dPath: ScallopOption[dataPath] = trailArg("the path of the data", required = false)

    footer(SUBCOMMAND_SEPARATOR)
  }
  addSubcommand(data)


  footer("")
}
