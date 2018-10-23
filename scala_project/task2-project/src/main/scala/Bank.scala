import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

    val accountCounter = new AtomicInteger(1000)

    def createAccount(initialBalance: Double): ActorRef = {
        // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
        BankManager.createAccount(accountCounter.incrementAndGet().toString,bankId,initialBalance);
    }

    def findAccount(accountId: String): Option[ActorRef] = {
        // Use BankManager to look up an account with ID accountId
        try {
            Some(BankManager.findAccount(bankId,accountId))
        }
        catch{
            case e: NoSuchElementException => None;
        }
    }

    def findOtherBank(bankId: String): Option[ActorRef] = {
        // Use BankManager to look up a different bank with ID bankId
        try {
            Some(BankManager.findBank(bankId))
        }
        catch{
            case e: NoSuchElementException => None;
        }
    }

    override def receive = {
        case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance); // Create a new account
        case GetAccountRequest(id) => findAccount(id); // Return account
        case IdentifyActor => sender ! this
        case t: Transaction => processTransaction(t)

        case t: TransactionRequestReceipt => {
            val isInternal = t.toAccountNumber.substring(0,4) == bankId;
            val toBankId = if (isInternal) bankId else t.toAccountNumber.substring(0, 4)
            val toAccountId = t.toAccountNumber.substring(4)
            if (isInternal){
                val accId = findAccount(toAccountId);
                if (!(accId.isEmpty)){
                    accId.get ! t;
                }
            }
            else{
                val otherBank = findOtherBank(toBankId);
                if (!(otherBank.isEmpty)){
                    otherBank.get ! t;
                }
            }
        }

        case msg => ???
    }

    def processTransaction(t: Transaction): Unit = {
        implicit val timeout = new Timeout(5 seconds)
        val isInternal = t.to.length <= 4 || (t.to.substring(0, 4) == bankId)
        val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
        val toAccountId = t.to.takeRight(4);
        val transactionStatus = t.status
        val fromBankId = t.from.substring(0,4)
        // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
        // HINT: Make use of the variables that have been defined above.
        if (transactionStatus == TransactionStatus.FAILED){
            findAccount(t.from).get ! new TransactionRequestReceipt(t.from,t.id,new Transaction(from="0",to="0",amount=0,status=TransactionStatus.FAILED));
        }
        
        if (isInternal){
            val accId = findAccount(toAccountId);
            if (!(accId.isEmpty)){
                accId.get ! t;
            }
            else {
                if (fromBankId==bankId){
                    findAccount(t.from.takeRight(4)).get ! new TransactionRequestReceipt(t.from,t.id,new Transaction(from="0",to="0",amount=0,status=TransactionStatus.FAILED));
                }
                else {
                    findOtherBank(fromBankId).get ! new TransactionRequestReceipt(t.from,t.id,new Transaction(from="0",to="0",amount=0,status=TransactionStatus.FAILED));
                }
            }
        }
        else{
            val otherBank = findOtherBank(toBankId);
            if (!(otherBank.isEmpty)){
                otherBank.get ! t;
            }
            else{
                findAccount(t.from.takeRight(4)).get ! new TransactionRequestReceipt(t.from,t.id,new Transaction(from="0",to="0",amount=0,status=TransactionStatus.FAILED));
            }
        }
    }
}