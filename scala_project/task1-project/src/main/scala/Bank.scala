 import scala.concurrent.forkjoin.ForkJoinPool
 import scala.concurrent._

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = new uid(0);
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = ExecutionContext.global
    private val processor = Main.thread(processTransactions);

    class uid (var id: Integer){
        def incrementRet(): Integer = {
            this.synchronized{
            id += 1;
            return id
            }
        }
    }

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        this.synchronized{
            transactionsQueue push new Transaction(
                transactionsQueue, processedTransactions, from, to, amount, allowedAttempts);
            println("Peekers: " + transactionsQueue.peek);
        }
    }

    // Hint: use a counter 
    def generateAccountId: Int = uid.incrementRet();

    def execute(body: => Unit) = executorContext.execute(
        new Runnable {def run() = body}
    )


    private def processTransactions: Unit = {
        while(true){
            // println("TX are in queue at beginning of loop: " + !transactionsQueue.isEmpty)
            val it = transactionsQueue.iterator
            // println("TX queue iterator has next: " + it.hasNext)
            while (it.hasNext){
                    println("Next Process Found!");
                    val trans = it.next;
                    println("Nummeret er: " + trans)
                    transactionsQueue.pop;
                    execute(trans.run);
                    // println("But does it still have next? " + it.hasNext);
                    // println("Or is it empty here?: " + transactionsQueue.isEmpty)
            }
        }
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance) 
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
