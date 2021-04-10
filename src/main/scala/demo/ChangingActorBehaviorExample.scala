package demo

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import demo.ActorCapabilitiesExample.{Counter, actorSystem}

object ChangingActorBehaviorExample extends App {

  val actorSystem = ActorSystem("secondDemoActorSystem")

  /**
   * Demo 1
   * recreate the Counter Actor with context.become and NO MUTABLE STATE
   */

  object Counter {
    // messages that actor supports
    case object Increment
    case object Decrement
    case object Print
  }
  class Counter extends Actor{
    import Counter._

    override def receive: Receive = countReceive(0)

    def countReceive(currentCount: Int): Receive = {
      case Increment =>
        println(s"countReceive($currentCount) incrementing")
        context.become(countReceive(currentCount + 1))
      case Decrement =>
        println(s"countReceive($currentCount) decrementing")
        context.become(countReceive(currentCount - 1))
      case Print => println(s"[counter] my current account is $currentCount")
    }
  }

  import Counter._
  val counter = actorSystem.actorOf(Props[Counter], "myCoynter")

  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print




  /**
   * Demo 2
   * simplified voting system
   */

  case class Vote(candidate: String)
  case object VoteStatusRequest   // sent to each citizen
  case class VoteStatusReply(candidate: Option[String])

  class Citizen extends Actor{

    override def receive: Receive = {
      case Vote(c) => context.become(voted(c))
      case VoteStatusRequest => sender() ! VoteStatusReply(None)
    }

    def voted(candidate: String): Receive = {
      case VoteStatusRequest => sender() ! VoteStatusReply(Some(candidate))
    }
  }



  case class AggregateVotes(citizens: Set[ActorRef])

  class VoteAggregator extends Actor{

    override def receive: Receive = awaitingCommand

    def awaitingCommand: Receive = {
      case AggregateVotes(citizens) =>
        citizens.foreach(citizenRef => citizenRef ! VoteStatusRequest)
        context.become(awaitingStatuses(citizens, Map()))
    }

    def awaitingStatuses(stillWaiting:Set[ActorRef], currentStats: Map[String,Int]): Receive = {
      case VoteStatusReply(None) =>
        // a citizen hasn't voted yet
        sender() ! VoteStatusRequest // this might end in an infinite loop
      case VoteStatusReply(Some(candidate)) =>
        val newStillWaiting = stillWaiting - sender()
        val currentVotesOfCandidate = currentStats.getOrElse(candidate, 0)
        val newStats = currentStats + (candidate -> (currentVotesOfCandidate + 1))
        if(newStillWaiting.isEmpty) {
          println(s"[aggregator] poll stats: $newStats")
        }
        else {
          context.become(awaitingStatuses(newStillWaiting, newStats))
        }
    }
  }

  val alice = actorSystem.actorOf(Props[Citizen])
  val bob = actorSystem.actorOf(Props[Citizen])
  val charlie = actorSystem.actorOf(Props[Citizen])
  val daniel = actorSystem.actorOf(Props[Citizen])

  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  charlie ! Vote("Roland")
  daniel ! Vote("Roland")

  val voteAggregator = actorSystem.actorOf(Props[VoteAggregator])
  voteAggregator ! AggregateVotes(Set(alice, bob, charlie, daniel))

  // print status of the votes
  /*
  *   Martin -> 1
  *   Jonas -> 1
  *   Roland -> 2
  * */

}
