package actors

import actors.ChangingActorBehavior.Parent.ParentStart
import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChangingActorBehavior extends App {

  object FussyKid {
    case object KidAccept
    case object KidReject
    val HAPPY = "happy"
    val SAD = "sad"
  }
  class FussyKid extends Actor {
    import FussyKid._
    import Parent._
    var state = HAPPY
    override def receive: Receive = {
        case Food(VEGETABLE) => state = SAD
        case Food(CHOCOLATE) => state = HAPPY
        case Ask(message) =>
          if(state == HAPPY) sender() ! KidAccept
          else sender() ! KidReject
    }
  }


  object Parent {
    case class ParentStart(kidRef: ActorRef)
    case class Food(food:String)
    case class Ask(message:String)  // a question
    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"
  }
  class Parent extends Actor {
    import Parent._
    import FussyKid._
    override def receive: Receive = {
      case ParentStart(kidRef) =>
        // test our interaction
        kidRef ! Food(VEGETABLE)
        kidRef ! Ask("do you want to play?")
      case KidAccept => println("Yay, my kid is happy")
      case KidReject => println("My kid is sad, but at least he's healthy!")
    }
  }

  val system = ActorSystem("changingActorBehavior")
  val fussyKid = system.actorOf(Props[FussyKid])
  val parent = system.actorOf(Props[Parent])

  parent ! ParentStart(fussyKid)


  // create a stateless fussy kid
  class StatelessFussyKid extends Actor{
    import FussyKid._
    import Parent._

    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Food(VEGETABLE) => //change my receive handler to sadReceive
        context.become(sadReceive)
      case Food(CHOCOLATE) => // don't do anything, I'm happy with happyReceive
      case Ask(_) => sender() ! KidAccept
    }

    def sadReceive: Receive = {
      case Food(VEGETABLE) => //stay sad
      case Food(CHOCOLATE) => // change my receive handler to happyReceive
        context.become(happyReceive)
      case Ask(_) => sender() ! KidReject
    }
  }

  val statelessFussyKid = system.actorOf(Props[StatelessFussyKid])
  val parent2 = system.actorOf(Props[Parent])

  parent2 ! ParentStart(statelessFussyKid)

  /**
   * Flow with stateless is:
   * kid receives Food(VEGETABLE) -> changes handler to sadReceive
   * kid receives Ask(want to play?)
   * kid replies with sadReceive handler so sends KidReject
   * parent receives KidReject
   */


  // Part 2
  /**
   * Example with stack:
   * context.become()
   *    Food(veg) -> stack.push(sadReceive)
   *    Food(chocolate) -> stack.push(happyReceive)
   *
   *    Stack:
   *      1. happyReceive
   *      2. sadReceive
   *      3. happyReceive
   *
   * Use context.unbecome in order to return to previous state
   */


  class Parent2 extends Actor {
    import Parent._
    import FussyKid._
    override def receive: Receive = {
      case ParentStart(kidRef) =>
        // test our interaction
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Ask("do you want to play?")
      case KidAccept => println("Yay, my kid is happy")
      case KidReject => println("My kid is sad, but at least he's healthy!")
    }
  }

  class StatelessFussyKid2 extends Actor{
    import FussyKid._
    import Parent._

    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Food(VEGETABLE) => //change my receive handler to sadReceive
        context.become(sadReceive, false)  // replaces current handler, pass in false to stack the new handler on top
      case Food(CHOCOLATE) => // don't do anything, I'm happy with happyReceive
      case Ask(_) => sender() ! KidAccept
    }

    def sadReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, false) //stay sad
      case Food(CHOCOLATE) => context.unbecome()  // to return to previous
      case Ask(_) => sender() ! KidReject
    }
  }

  /**
   * new behavior
   * Food(VEGETABLE)
   * Food(VEGETABLE)
   * Food(CHOCOLATE)
   * Food(CHOCOLATE)
   *
   * Stack:
   *   --POPPED OUT-- 1. sadReceive
   *   --POPPED OUT-- 2. sadReceive
   *  1. happyReceive
   */

  val statelessFussyKid2 = system.actorOf(Props[StatelessFussyKid2])
  val parent3 = system.actorOf(Props[Parent2])

  parent3 ! ParentStart(statelessFussyKid2)

  // if stack empty, akka just calls receive

}
