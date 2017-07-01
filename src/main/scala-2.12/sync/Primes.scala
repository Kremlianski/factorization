package sync

import scala.annotation.tailrec
import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.{Element, document, window}
import org.scalajs.dom.raw.{Event, HTMLInputElement}
import scala.util.{Failure, Success, Try}

object Primes {

  private val calculate = document.getElementById("calculate")
  private val target = document.getElementById("target")
  private val numbers = document.getElementById("number")

  sealed trait Action
  case class PrintResult(n: Vector[Long], t: Long) extends Action

  case class State(
                    nextNumber: Long,
                    primes: Vector[Long],
                    target: Long,
                    isFinished: Boolean = false,
                    Error: Option[String] = None
                  )

  private def makeString(n: Long): String =
    n.toString.reverse.grouped(3).reduceLeft { (x, s) =>
      if (x.length >= 3) x + "," + s
      else x
    }.reverse

  private def fillElement(target: Element)(text: String) = target.innerHTML = text

  private val fillEl = fillElement(target)(_)

  private def doAction(action: Action) = action match {

    case PrintResult(n, t) if t == 1 =>
      val s = n.map(makeString).mkString("final:&nbsp;&nbsp;&nbsp;", " * ", " = " + makeString(n.product))
      fillEl(s)

    case _ =>
  }

  def main(): Unit = {

    calculate.addEventListener("click",
      (_: Event) => {

        val number = Try(numbers.asInstanceOf[HTMLInputElement].value.toLong) match {
          case Success(n) => n
          case Failure(_) => 0
        }

        if (number > 1) {

          target.innerHTML = ""
          val initialState = State(1, Vector.empty[Long], number)

          implicit val dispatcher: (Action) => Unit = doAction

          window.setTimeout(() => doCalculation(initialState), 100)

        } else {
          target.innerHTML = "You must enter a number > 1"
        }
      }
    )
  }

  @tailrec
  private def doDivide(state: State)(implicit dispatcher: (Action) => Unit): State = {
    state match {

      case State(n, p, t, _, _) if t % n == 0 =>

        val target = t / n
        val primes = p :+ n

        dispatcher(PrintResult(primes, target))
        doDivide(state.copy(primes = primes, target = target))

      case _ => state
    }
  }

  private def newState(s: State)(implicit dispatcher: (Action) => Unit): State =
    s match {

      case State(n, _, t, _, _) if t % n == 0 => doDivide(s)
      case State(_, _, t, _, _) if t > 1 => s
      case _ => s.copy(isFinished = true)
    }



  @tailrec
  private def doCalculation(state: State)(implicit dispatcher: (Action) => Unit): Unit = {
    state match {
      case State(_, _, _, true, _) =>
      case State(_, _, _, _, Some(_)) =>
        doCalculation(state.copy(isFinished = true))
      case State(n, _, t, _, _) if n >= t =>
        doCalculation(state.copy(isFinished = true))
      case State(n, _, _, _, _) =>
        doCalculation(newState(state.copy(nextNumber = n + 1)))
    }
  }
}

