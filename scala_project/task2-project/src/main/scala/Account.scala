import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

    private var transactions = HashMap[String, Transaction]()

    class Balance(var amount: Double) {

        // Current value of the Balance
        private var bal = amount;

        // Get value of Balance
        def getBalance() : Double = {
            return bal;
        }

        // Add amount from current balance
        def add(amount : Double) = {
            this.synchronized{
                bal = bal + amount;
            }
        }

        // Substract from current balance
        def subtract(amount : Double) = {
            this.synchronized{
                bal = bal - amount;
            }
        }

        // Return if the balance is superior to the asked
        // amount
        def hasSufficientFunds(amount : Double) : Boolean = {
            this.synchronized{
                return (bal >= amount);
            }
        }
    }

    val balance = new Balance(initialBalance)

    def getFullAddress: String = {
        bankId + accountId
    }

    def getTransactions: List[Transaction] = {
        // Should return a list of all Transaction-objects stored in transactions
        transactions.values.toList;
    }

    def isTxComplete(tx:(String,Transaction)): Boolean = {
        tx._2.isCompleted;
    }

    def allTransactionsCompleted: Boolean = {
        // Should return whether all Transaction-objects in transactions are completed
        transactions.forall(isTxComplete);
    }

    def withdraw(amount: Double): Unit= {
        if (amount < 0) {
            throw new IllegalAmountException();
        }
        else if (balance.hasSufficientFunds(amount)) {
            balance.subtract(amount);
        }
        else {
            throw new NoSufficientFundsException();
        }
    }
    def deposit(amount: Double): Unit = {
        if (amount < 0) {
            throw new IllegalAmountException();
        }
        balance.add(amount);
    }
    def getBalanceAmount: Double = {
        return balance.getBalance();
    }

    def sendTransactionToBank(t: Transaction): Unit = {
        // Should send a message containing t to the bank of this account
        transactions+=(t.id->t)
        BankManager.findBank(bankId) ! t;
    }

    def transferTo(accountNumber: String, amount: Double): Transaction = {

        val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

        if (reserveTransaction(t)) {
            try {
                withdraw(amount)
                sendTransactionToBank(t)

            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }
        }

        t

    }

    def reserveTransaction(t: Transaction): Boolean = {
      if (!transactions.contains(t.id)) {
        transactions += (t.id -> t)
        return true
      }
      false
    }

    override def receive = {
		case IdentifyActor => sender ! this

		case TransactionRequestReceipt(to, transactionId, transaction) => {
			// Process receipt
            if (transaction.isCompleted){
                val tx = transactions(transactionId);
                if (transaction.isSuccessful){
                    tx.status = TransactionStatus.SUCCESS;
                }
                else{
                    tx.status = TransactionStatus.FAILED;
                    deposit(tx.amount);
                }
            }
		}

		case BalanceRequest => sender ! getBalanceAmount; // Should return current balance

		case t: Transaction => {
			// Handle incoming transaction
			val fromAccountId = t.from;
            val transactionId = t.id;
            val amount = t.amount;
            var status = TransactionStatus.PENDING;
            try{
                deposit(amount);
                status = TransactionStatus.SUCCESS;
            }
            catch{
                case e: IllegalAmountException => status = TransactionStatus.FAILED;
            }
            val receipt = TransactionRequestReceipt(fromAccountId,transactionId,new Transaction(from="0",to="0",amount=0,status=status));
            BankManager.findBank(bankId) ! receipt;
		}

		case msg => ???
    }


}
