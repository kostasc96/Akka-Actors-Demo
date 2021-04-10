package actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorCapabilities extends App {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "Hi!" => context.sender() ! "Hello,there!"  // context.sender() returns an ActorRef // we can say just sender() // here replying to a message
      case message: String => println(s"[$self] I have received $message")
      case number: Int => println(s"[simple actor] I have received a NUMBER: $number")
      case SpecialMessage(contents) => println(s"[simple actor] I have received something special: $contents")
      case SendMessageToYourself(content) =>
        self ! content   // will trigger message above
      case SayHiTo(ref) => ref ! "Hi!"   // alice is being passed as the sender
      case WirelessPhoneMessage(content, ref) =>
        ref forward (content + "s")  // i keep the original sender of the wireless phone message
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"

  // 1 - messages can be of any type
  // a) messages must be IMMUTABLE (nobody can touch it)
  // b) messages must be SERIALIZABLE
  // in practice use case classes and case objects
  simpleActor ! 42

  case class SpecialMessage(contents:String)
  simpleActor ! SpecialMessage("some special content")

  // actors have information about the context and about themselves
  // context.self === this  (or use self)

  case class SendMessageToYourself(content:String)
  simpleActor ! SendMessageToYourself("I am an actor and I am proud of it")

  // 3 - actors can reply to messages
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)
  alice ! SayHiTo(bob)

  // 4 - dead letters
  alice ! "Hi!"  // reply to "me"

  // 5 - forwarding messages (actors forwarding messages to one another)
  // forwarding = sending a message with the original sender
  case class WirelessPhoneMessage(content:String, ref: ActorRef)
  alice ! WirelessPhoneMessage("Hi!", bob)  // noSender() // alice forwards the message to bob

}
