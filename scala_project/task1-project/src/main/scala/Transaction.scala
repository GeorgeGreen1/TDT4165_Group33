import exceptions._
import scala.collection.mutable.Queue

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

    //creating a new queue for the transactions
    val q = new Queue[Transaction]

    // Remove and return the first element from the queue
    def pop: Transaction = {
        val next = q.dequeue
        println("pop")
        return next
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = {
        return (q.size == 0)
    }

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = {
        q += t
        println("push")
        println(q)
    }

    // Return the first element from the queue without removing it
    def peek: Transaction = {
        return q.head
    }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = {
        return q.iterator
    }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {

      def doTransaction() = {
          from withdraw amount
          to deposit amount
      }

      if (from.uid < to.uid) from synchronized {
          to synchronized {
            doTransaction
          }
      } else to synchronized {
          from synchronized {
            doTransaction
          }
      }

      // Extend this method to satisfy requirements.

    }
}
