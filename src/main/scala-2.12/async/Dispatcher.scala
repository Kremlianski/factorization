package async

import monix.execution.{Ack, Cancelable, Scheduler}
import monix.execution.Ack.Continue
import monix.reactive.Observer
import monix.reactive.subjects.BehaviorSubject
import monix.execution.ExecutionModel.AlwaysAsyncExecution
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, MILLISECONDS}


class Dispatcher [Action](val initialState: Action){

  implicit val scheduler = Scheduler(executionModel=AlwaysAsyncExecution)

  private val stream: BehaviorSubject[Action] = BehaviorSubject.apply(initialState)

  def dispatch(s: Action): Unit = stream.onNext(s)
  def cancel():Unit = stream.onComplete


  def observer(f:Action => Unit, c: () => Unit)= new Observer[Action] {
    def onNext(s: Action): Future[Ack] = {

      f(s)
      Continue

    }

    def onError(ex: Throwable): Unit = {
      ex.printStackTrace()
    }

    def onComplete(): Unit =  c()

  }

  def subscribe(doAction: Action=>Unit, onCancel:() => Unit): Cancelable =
    stream.sample(Duration(10, MILLISECONDS))
      .delayOnNext(Duration(500, MILLISECONDS))
      .subscribe(observer(doAction, onCancel))

}
