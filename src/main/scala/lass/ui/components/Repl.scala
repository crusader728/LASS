package lass.ui.components

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{BackendScope, ReactEventFromInput, ScalaFnComponent}
import lass.Syntaxs.{Attempt, Environment, StateFulAttempt}
import lass.{CommandParser, UTLCInterpreter}
import scalacss.Defaults._
import scalacss.ScalaCssReact._

import scala.collection.mutable.ListBuffer


object Repl {
  sealed abstract class ReplLine(val content: String) extends Product with Serializable
  case class OutputLine(l: String) extends ReplLine(l)
  case class ErrorLine(l: String) extends ReplLine(l)
  case class InputLine(l: String) extends ReplLine(l)

  object InputStyle extends StyleSheet.Inline {
    import dsl._
    val input = style(color(Color("#fff")),
      fontSize.inherit,
      fontWeight.bold,
      border.none,
      outline.none,
      flexGrow(1),
      backgroundColor.transparent,
      fontFamily.inherit,
      caretColor(Color("#f48fb1"))
    )

  }

  case class ReplState(m: Environment, lines: ListBuffer[ReplLine], current: String)

  val Outputs = ScalaFnComponent[Seq[ReplLine]] { props =>
    def renderLine(line: ReplLine) = {
      line match {
        case InputLine(l) => <.div(^.display.flex, ^.marginTop := "8px", <.div(^.color := "#f48fb1", ^.paddingRight := "8px", ">"), l)
        case ErrorLine(l) => <.div(^.color := "#d33", ^.marginTop := "8px", ^.whiteSpace.`pre-wrap`, l)
        case OutputLine(l) => <.div(^.color := "#ccc", ^.marginTop := "8px", ^.whiteSpace.`pre-wrap`, l)
      }
    }

    <.div(props map renderLine: _*)

  }

  class Backend($: BackendScope[Unit, ReplState],
                commandParser: CommandParser[Attempt],
                interpreter: UTLCInterpreter[StateFulAttempt]) {
    def onChange(e: ReactEventFromInput) = {
      val newValue = e.target.value
      $.modState(_.copy(current = newValue))
    }

    def handleSubmit(e: ReactEventFromInput) = {
      e.preventDefaultCB >>
        $.modState(s => {
          val env = s.m
          import cats.implicits._
          val attempt = for {
            command <- commandParser.readLine(s.current)
            (e, l) <- interpreter.executeLine(command).run(env)
          } yield (e, l)
          attempt match {
            case Left(error) => ReplState(s.m, s.lines.append(InputLine(s.current)).append(ErrorLine(error)), "")
            case Right((newEnv, l)) => ReplState(newEnv, s.lines.append(InputLine(s.current)).appendAll(l.split("\n").map(OutputLine)), "")
          }
        })
    }

    def render(replState: ReplState) = {
      <.div(^.fontFamily := "monospace", ^.fontWeight.bold, ^.color := "#fff", ^.fontSize := "25px",
        ^.backgroundColor := "#333", ^.borderRadius := "4px", ^.boxShadow := "0px 2px 2px rgba(0, 0, 0, 0.5)",
        ^.overflow.hidden,
        <.div(^.padding := "16px", ^.paddingTop := "2px", ^.height := "800px", ^.overflowY.auto,
          Outputs(replState.lines.toSeq),
          <.form(^.onSubmit ==> handleSubmit,
            <.div(^.display.flex, ^.alignItems.center, ^.marginTop := "8px",
              <.div(^.color := "#f48fb1", ^.paddingRight := "8px", ">"),
              <.input(InputStyle.input, ^.size := 100, ^.fontSize := "25px", ^.onChange ==> onChange, ^.value := replState.current)
            )
          )
        )
      )
    }
  }


}
