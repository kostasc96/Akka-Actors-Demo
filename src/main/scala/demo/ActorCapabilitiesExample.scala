package demo

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import demo.ActorCapabilitiesExample.Person.LiveTheLife

object ActorCapabilitiesExample extends App {

  /**
   * 1. a A count actor
   *  - Increment
   *  - Decrement
   *  - Print
   *
   *
   *  2. a Bank account as an actor
   *  receives
   *  - Deposit an amount
   *  - Withdraw an amount
   *  - Statement
   *  replies with:
   *  - Success/Failure
   *
   *  interact with some other kind of actor
   **/

  val actorSystem = ActorSystem("firstDemoActorSystem")

  // DOMAIN of the counter
    // companion object
  object Counter {
    // messages that actor supports
    case object Increment
    case object Decrement
    case object Print
  }
  class Counter extends Actor{
    import Counter._

    var count = 0

    override def receive: Receive = {
        case Increment => count += 1
        case Decrement => count -= 1
        case Print => println(s"[counter] my current count is $count")
    }
  }

  import Counter._
  val counter = actorSystem.actorOf(Props[Counter], "myCoynter")

  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print

  /**
   * bank account
  **/

  object BankAccount{
    case class Deposit(amount: Int)
    case class Withdraw(amount: Int)
    case object Statement
    case class TransactionSuccess(message: String)
    case class TransactionFailure(message: String)
  }
  class BankAccount extends Actor {
    import BankAccount._

    var funds = 0

    override def receive: Receive = {
      case Deposit(amount) =>
        if(amount < 0) sender() ! TransactionFailure("Invalid deposit amount")
        else {
          funds += amount
          sender() ! TransactionSuccess(s"Successfully deposited $amount")
        }
      case Withdraw(amount) =>
        if(amount < 0) sender() ! TransactionFailure("Invalid withdraw amount")
        else if(amount > funds) sender() ! TransactionFailure("Insufficient funds")
        else{
          funds -= amount
          sender() ! TransactionSuccess(s"Successfully withdrew $amount")
        }
      case Statement => sender() ! s"Your balance is $funds"
    }
  }

  object Person{
    case class LiveTheLife(account: ActorRef)
  }
  class Person extends Actor{
    import BankAccount._
    import Person._

    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(90000)
        account ! Withdraw(500)
        account ! Statement
      case message => println(message.toString)
    }
  }


  val account = actorSystem.actorOf(Props[BankAccount], "bankAccount")
  val person = actorSystem.actorOf(Props[Person], "billionaire")

  person ! LiveTheLife(account)
}
