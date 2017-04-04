package nl.hugo.redbook.ch7

import java.util.concurrent.{ ExecutorService, ThreadPoolExecutor }

object ExecutorServiceDecorator {

  implicit class ExectorServiceOps(val es: ExecutorService) extends AnyVal {
    def completedTaskCount: Long =
      es.asInstanceOf[ThreadPoolExecutor].getCompletedTaskCount
  }
}
