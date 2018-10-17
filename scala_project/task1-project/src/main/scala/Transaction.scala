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
        this.synchronized{
            val next = q.dequeue
            println("pop")
            return next
        }
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = {
        this.synchronized{
          return (q.size == 0)
        }
    }

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = {
        this.synchronized{
            q += t
            println("Push --> Added tx: " + t)
        }
    }

    // Return the first element from the queue without removing it
    def peek: Transaction = {
        this.synchronized{
            return q.head
        }
    }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = {
        this.synchronized{
            return q.iterator
        }
    }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttempts: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {

      var attemptsLeft = allowedAttempts;
      var over = false;
      var success = false;

      def doTransaction() = {
          from withdraw amount
          to deposit amount 
      }
      def tryTransaction() = {
        if (from.uid < to.uid) from synchronized {
            to synchronized {
                doTransaction
            }
        } else to synchronized {
            from synchronized {
                doTransaction
            }
        }
      }
      while ((attemptsLeft>0)&&(!over)){
          try {
              tryTransaction;
              over = true;
              success = true;
          }
          catch{
              case e: IllegalAmountException => attemptsLeft-=1;
              case e: NoSufficientFundsException => attemptsLeft-=1;
          }
          if (attemptsLeft == 0){
              over = true;
          }
      }
      if (!success){
          status = TransactionStatus.FAILED;
      }
      else {
          status = TransactionStatus.SUCCESS;
      }

      processedTransactions.push(this)
    }
}
