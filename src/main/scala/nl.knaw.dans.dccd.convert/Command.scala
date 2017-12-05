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

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.language.reflectiveCalls
import scala.util.{ Failure, Success, Try }

object Command extends App with DebugEnhancedLogging {
  type FeedBackMessage = String

  val configuration = Configuration()
  val commandLine: CommandLineOptions = new CommandLineOptions(args, configuration) {
    verify()
  }
  //val app = new DccdConvertExportApp(new ApplicationWiring(configuration))
  val app = new DccdConvertExportApp(configuration)


  val result: Try[FeedBackMessage] = commandLine.subcommand match {

    case Some(runService @ commandLine.runService) =>
      app.createFullReport()
      Try {"full report"} match {
        case Failure(_) => Try{"failure: Full report dccd-convert"}
        case Success(_) => Try{"success: Full report dccd-convert"}
      }
    case _ =>
      Try {""} match {
        case Failure(_) => Try{"failure: ?"}
        case Success(_) => Try{"unknown command"}
      }


      //???
  }

  result.map(msg => Console.err.println(s"OK: $msg")).doIfFailure {
    case t => Console.err.println(s"ERROR: ${ t.getMessage }")
  }
}
