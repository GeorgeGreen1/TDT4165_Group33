    import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {
        val bal = amount;
        def setBal(var entry: Double): Unit = {
            bal = entry;
        }
        def getBal(): Double = {
            return bal;
	    // test for git pushing
        }
    }

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId

    def withdraw(amount: Double): Unit = balance.setBal(balance.getBal-amount);
    def deposit(amount: Double): Unit = balance.setBal(balance.getBal+amount);
    def getBalanceAmount: Double = return balance.getBal

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}
