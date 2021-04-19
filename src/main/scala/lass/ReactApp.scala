package lass

import japgolly.scalajs.react.ScalaComponent
import lass.ui.components.Repl
import lass.ui.components.Repl.{Backend, ReplState}
import org.scalajs.dom.document

import scala.collection.mutable.ListBuffer
import scala.scalajs.js.annotation.JSExport

object ReactApp {
   @JSExport
  def main(args: Array[String]): Unit = {
     import lass.Syntaxs._

     val commandParser = new FastParseCommandModule[Attempt]()
     val evaluation = new StateEvaluationModule[StateFulAttempt]()
     val interpreter = new UTLCInterpreterModule[StateFulAttempt](evaluation)
     val ReplApp = ScalaComponent.builder[Unit]
       .initialState(ReplState(Map(), new ListBuffer[Repl.ReplLine](), ""))
       .backend(new Backend(_, commandParser, interpreter))
       .renderBackend
       .build

     ReplApp().renderIntoDOM(document.body)
  }
}
