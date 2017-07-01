package async

import scala.annotation.tailrec
import scala.scalajs.js.JSApp
import org.scalajs.dom
import monix.eval.Task
import dom.{Element, document}
import org.scalajs.dom.raw.{Event, HTMLElement, HTMLInputElement}
import scala.util.{Failure, Success, Try}
import monix.execution.Scheduler.Implicits.global


object Primes extends JSApp {


  private val calculate = document.getElementById("calculate")
  private val stop = document.getElementById("stop")
  private val target = document.getElementById("target")
  private val numbers = document.getElementById("number")

  private val onCancel = () => {

    numbers.asInstanceOf[HTMLElement].style.display = ""
    calculate.asInstanceOf[HTMLElement].style.display = ""
    stop.asInstanceOf[HTMLElement].style.display = "none"

  }


  sealed trait Action
  case class PrintResult(n: Vector[Long], t: Long) extends Action
  case class Start(initial: Long) extends Action


  case class CalculationState(var flag: Boolean)

  case class State(
                    nextNumber: Long,
                    primes: Vector[Long],
                    target: Long,
                    isFinished: Boolean = false,
                    Error: Option[String] = None
                  )

  private def makeString(n: Long): String = n.toString.reverse.grouped(3).reduceLeft { (x, s) =>
    if (x.length >= 3) x + "," + s
    else x
  }.reverse

  private def fillElement(target: Element)(text: String) = target.innerHTML = text

  private val fillEl = fillElement(target)(_)

  private def doAction(action: Action) = action match {
    case Start(i) =>

      fillEl("1 * " + makeString(i))
    case PrintResult(n, t) =>
      val s = if (t != 1) n.map(makeString).mkString("1 * ", " * ", " * " + makeString(t))
      else n.map(makeString).mkString("final:&nbsp;&nbsp;&nbsp;", " * ", " = " + makeString(n.product))
      fillEl(s)
  }

  def main(): Unit = {

    stop.asInstanceOf[HTMLElement].style.display = "none"

    calculate.addEventListener("click",
      (_: Event) => {

        val number = Try(numbers.asInstanceOf[HTMLInputElement].value.toLong) match {
          case Success(n) => n
          case Failure(_) => 0
        }

        if (number > 1) {

          stop.asInstanceOf[HTMLElement].style.display = ""
          numbers.asInstanceOf[HTMLElement].style.display = "none"
          calculate.asInstanceOf[HTMLElement].style.display = "none"

          implicit val mutableState: CalculationState = CalculationState(false)

          val initialState = State(1, Vector.empty[Long], number)

          implicit val dispatcher: Dispatcher[Action] = new Dispatcher[Action](Start(initialState.target))

          dispatcher.subscribe(doAction, onCancel)

          val stopClickHandler: Event => Unit = (_: Event) => {

            mutableState.flag = true
          }
          stop.addEventListener("click", (e: Event) => {
            stopClickHandler(e)
            stop.removeEventListener("click", stopClickHandler)
          })

          doCalculation(initialState).runAsync

        } else {
          target.innerHTML = "You must enter a number > 1"
        }
      }
    )

  }

  @tailrec
  private def doDivide(state: State)(implicit dispatcher: Dispatcher[Action]): State = {
    state match {

      case State(n, p, t, _, _) if t % n == 0 =>

        val target = t / n
        val primes = p :+ n

        dispatcher.dispatch(PrintResult(primes, target))
        doDivide(state.copy(primes = primes, target = target))

      case _ => state
    }
  }

  private def newState(s: State)(implicit dispatcher: Dispatcher[Action]): State =
    s match {

      case State(n, _, t, _, _) if t % n == 0 => doDivide(s)
      case State(_, _, t, _, _) if t > 1 => s
      case _ => s.copy(isFinished = true)
    }


  private def doCalculation(state: State)(implicit dispatcher: Dispatcher[Action],
                                          flag: CalculationState): Task[Unit] = {
    Task.eval(state) flatMap {

      case _ if flag.flag => Task.now {
        dispatcher.cancel()
      }
      case State(_, _, _, true, _) =>
        Task.now {
          dispatcher.cancel()
        }

      case State(_, _, _, _, Some(_)) =>
        doCalculation(state.copy(isFinished = true))

      case State(n, _, t, _, _) if n >= t => doCalculation(state.copy(isFinished = true))
      case State(n, _, _, _, _) =>

        doCalculation(newState(state.copy(nextNumber = n + 1)))
    }
  }
}
