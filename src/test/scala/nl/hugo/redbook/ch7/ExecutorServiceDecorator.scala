package nl.hugo.redbook.ch7

import java.util.concurrent.{ ExecutorService, ThreadPoolExecutor }

object ExecutorServiceDecorator {

  implicit class ExectorServiceOps(es: ExecutorService) {
    def completedTaskCount: Long =
      es.asInstanceOf[ThreadPoolExecutor].getCompletedTaskCount
  }
}
