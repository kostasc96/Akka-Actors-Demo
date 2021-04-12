package actors

import actors.ChildActors.CreditCard.{AttachToAccount, CheckStatus}
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActors  extends App {

  val actorSystem = ActorSystem("ParentChildDemo")

  // Actors can create other actors

  object Parent{
    case class CreateChild(name:String)
    case class TellChild(message:String)
  }
  class Parent extends Actor {
    import Parent._

    override def receive: Receive = {
      case CreateChild(name) =>
        println(s"${self.path} creating child")
        // create a new actor right HERE
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))
    }

    def withChild(childRef: ActorRef): Receive = {
      case TellChild(message) => childRef forward message
    }
  }

  class Child extends Actor{
    override def receive: Receive = {
      case message => println(s"${self.path} I got: $message")
    }
  }


  import Parent._
  val parent = actorSystem.actorOf(Props[Parent], "parent")
  parent ! CreateChild("child")
  parent ! TellChild("hey Kid!")

  // actor hierarchies
  // parent -> child -> grandChild
  //        -> child2 ->

  /*
    Guardian actors (top-level)
    - /system = system guardian
    - /user = user-level guardian
    - / = root guardian
   */

  /**
   * Actor selection
   */
  val childSelection = actorSystem.actorSelection("user/parent/child")
  childSelection ! "I found you!"

  /**
   * Danger!
   *
   * NEVER PASS MUTABLE ACTOR STATE, OR THE 'THIS' REFERENCE TO CHILD ACTORS
   */

  object NaiveBankAccount{
    case class Deposit(amount:Int)
    case class WithDraw(amount:Int)
    case object InitializeAccount
  }
  class NaiveBankAccount extends Actor {
    import NaiveBankAccount._
    import CreditCard._

    var amount = 0

    override def receive: Receive = {
      case InitializeAccount =>
        val creditCardRef = context.actorOf(Props[CreditCard])
        creditCardRef ! AttachToAccount(this)
      case Deposit(funds) => deposit(funds)
      case WithDraw(funds) => withdraw(funds)
    }

    def deposit(funds: Int) = {
      println(s"${self.path} depositing $funds on top of $amount")
      amount += funds
    }
    def withdraw(funds:Int) = {
      println(s"${self.path} withdrawing $funds from $amount")
      amount -= funds
    }
  }

  object CreditCard {
    case class AttachToAccount(bankAccount: NaiveBankAccount) // !!
    case object CheckStatus
  }
  class CreditCard extends Actor {

    override def receive: Receive = {
      case AttachToAccount(account) => context.become(attachedTo(account))
    }

    def attachedTo(account: NaiveBankAccount): Receive = {
      case CheckStatus =>
        println(s"${self.path} your message has been processed.")
        // benign
        account.withdraw(1)
    }
  }

  import NaiveBankAccount._
  import CreditCard._

  val bankAccountRef = actorSystem.actorOf(Props[NaiveBankAccount], "account")
  bankAccountRef ! InitializeAccount
  bankAccountRef ! Deposit(100)

  Thread.sleep(500)
  val ccSelection = actorSystem.actorSelection("/user/account/card")
  ccSelection ! CheckStatus

  // WRONG!!!!!!
}
