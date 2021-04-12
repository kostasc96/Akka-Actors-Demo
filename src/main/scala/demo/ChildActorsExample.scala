package demo

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.testkit.TestActor

object ChildActorsExample extends App {

  val system = ActorSystem("ChildActorsDemo")

  // Distributed word counting

  object WordCounterMaster{
    case class Initialize(nChildren: Int)
    case class WordCountTask(id:Int, text:String)
    case class WordCountReply(id:Int, count:Int)
  }
  class WordCounterMaster extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case Initialize(nChildren) =>
        println("[master] initializing...")
        println("---------------------------")
        val childrenRefs = for(i <- 1 to nChildren) yield context.actorOf(Props[WordCounterWorker],s"wcw_$i")
        context.become(withChildren(childrenRefs,0,0, Map()))
    }

    def withChildren(childrenRefs: Seq[ActorRef], currentChildIndex: Int, currentTaskId: Int, requestMap: Map[Int, ActorRef]): Receive = {
      case text: String =>
        println(s"[master] I have received: $text")
        println(s"I will send it to child $currentChildIndex")
        println("---------------------------")
        val originalSender = sender()
        val task = WordCountTask(currentTaskId, text)
        val childRef = childrenRefs(currentChildIndex)
        childRef ! task
        val nextChildIndex = (currentChildIndex + 1) % childrenRefs.length
        val newTaskId = currentTaskId + 1
        val newRequestMap = requestMap + (currentTaskId -> originalSender)
        context.become(withChildren(childrenRefs, nextChildIndex, newTaskId, newRequestMap))
      case WordCountReply(id, count) =>
        // problem: sender()??
        // solved by:
        println(s"[master] I have received a reply for task id $id with count of words $count")
        println("---------------------------")
        val originalSender = requestMap(id)
        originalSender ! count
        context.become(withChildren(childrenRefs, currentChildIndex, currentTaskId, requestMap - id))
    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case WordCountTask(id, text) =>
        println(s"${self.path} I have received task $id with $text")
        println("---------------------------")
        sender() ! WordCountReply(id, text.split(" ").length)
    }
  }

  /*
      create WordCounterMaster
      send Initialize(10) to WordCounterMaster
      send "Akka is great" to WordCounterMaster, it will send a WordCountTask("...") to one of its children
        child replies with a WordCountReply(3) to the master
        master replies with 3 to the sender.

      requester -> wcm -> wcw
      requester <- wcm <-
   */

  // round robin logic
  // 1,2,3,4,5 and 7 tasks --> 1,2,3,4,5,1,2

  class TestActor extends Actor {
    import WordCounterMaster._

    override def receive: Receive = {
      case "go" =>
        val master = context.actorOf(Props[WordCounterMaster], "master")
        master ! Initialize(3)
        val texts = List("I love Akka", "Scala is very cool", "yes", "I am fine")
        texts.foreach(text => master ! text)
      case count:Int =>
        println("---------------------------")
        println(s"[test actor] I have received a reply: $count")
    }
  }

  val testActor = system.actorOf(Props[TestActor], "testActor")
  testActor ! "go"


}
