 import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = new uid(0);
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = 0

    class uid (var id: Integer){
        def incrementRet(): Integer = {
            this.synchronized{
            id += 1;
            return id
            }
        }
    }

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
      transactionsQueue push new Transaction(
        transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
    }

    // Hint: use a counter
    def generateAccountId: Int = uid.incrementRet();

    private def processTransactions: Unit = {
        println("processing transactions")
        val it = transactionsQueue.iterator
        while (it.hasNext){
            var trans = transactionsQueue.pop
            trans.run
        }
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
