package testJava;

import akka.actor.ActorRef;
import akka.actor.Props;

public class TestActorJava {
    private static TestActorScala testActorScala;

    public static void main(String[] args) {

        ActorRef bankAcc = TestActorScala.account();
        ActorRef pers = TestActorScala.person();

        TestActorScala.handler("LiveTheLife",pers, bankAcc);

    }
}
