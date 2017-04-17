package nl.hugo

import org.scalatest.{ Alerter, Informer }
import org.scalatest.exceptions.TestPendingException

package object redbook {

  type Warn = String => Unit

  object Warn {

    // does not warn on evaluation inefficiency
    case object Silent extends Warn {
      override def apply(s: String): Unit = ()
    }

    // informs on evaluation inefficiency
    def apply(info: => Informer): Warn = new Warn {
      override def apply(s: String): Unit =
        Option(s).foreach(info(_))
    }

    // alerts on evaluation inefficiency and optionally makes a test pending
    def apply(alert: => Alerter, pending: Boolean = false): Warn = new Warn {
      override def apply(s: String): Unit = {
        Option(s).foreach(alert(_, None))
        if (pending) throw new TestPendingException
      }
    }
  }
}