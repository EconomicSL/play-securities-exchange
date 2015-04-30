import akka.actor.{ActorRef, Props, Actor}


case class FabricatedParent(childProps: Props, proxy: ActorRef) extends Actor {

  val child = context.actorOf(childProps)

  def receive: Receive = {
    case mesg if sender() == child => proxy forward mesg
    case mesg => child forward mesg
  }

}
