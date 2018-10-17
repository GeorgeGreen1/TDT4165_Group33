    import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(amount : Double) {

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
    val uid = this.synchronized { 
        var x = bank.generateAccountId;
        println("Generated ID: " + x);
        x;
        }

    def withdraw(amount: Double): Unit = {
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

    def transferTo(account: Account, amount: Double) = {
        println(amount + " kroner vil ga inn " + account.uid)
        bank addTransactionToQueue(this, account, amount);
    }
}
