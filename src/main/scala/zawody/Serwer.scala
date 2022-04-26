package zawody

import akka.actor.{Actor, ActorPath, ActorSystem, Props}
import akka.event.{Logging, LoggingAdapter}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.GraphPredef._

import java.net.InetSocketAddress
import scala.annotation.tailrec
import scalax.collection.mutable

import scala.collection.mutable.{Queue, StringBuilder}
import scala.language.postfixOps
import scala.util.Random

object Defs {
  var gs = 0  // graphSize
  var tl = 0  // timeLimit
}

class TCPConnectionManager(address: String, port: Int) extends Actor {
  import context.system
  IO(Tcp) ! Bind(self, new InetSocketAddress(address, port))
  val log: LoggingAdapter = Logging(context.system, this)
  val players = new Queue[Tuple2[ActorPath, ActorPath]]()

  override def receive: Receive = {
    case Bound(local) =>
      println(s"Server started on $local")
    case Connected(remote, local) =>
      val handler = context.actorOf(TCPConnectionHandler.props)
      println(s"New connnection: $local -> $remote")
      sender() ! Register(handler)
      println(sender().path.name)
      players.enqueue((handler.path, sender.path))
      if(players.size >= 2) {
        val player1 = players.dequeue()
        val player2 = players.dequeue()
        val elist = GenGraph.create(Defs.gs)
        val token = Random.nextInt(Defs.gs)
        context.actorSelection(player1._1) ! ('A', token, player1._2, player2._1, player2._2, elist)
      }
  }
}

object TCPConnectionManager {
  def props(ip: String, port: Int): Props = Props(classOf[TCPConnectionManager], ip, port)
}

class TCPConnectionHandler extends Actor {
  private var myComp: ActorPath = null
  private var opComp: ActorPath = null
  private var opponent: ActorPath = null
  private var graph: mutable.Graph[Int, UnDiEdge] = null
  private var token = -1
  private var whichPlayerAmI: Char = 'X'
  private var moveNumber = 0
  private var startThinkingTime = 0L

  override def receive: Actor.Receive = {
    case Received(data) =>
      val stopThinkingTime = System.nanoTime
      val decoded = data.utf8String
      val regex = raw"^210\s(\d+){1,7}\s*"r
      val nextNode = decoded match {
        case regex(num) => Option(num.toInt)
        case _ => None
      }
      if(nextNode.isDefined) {
        if(Rules.makeMove(token, nextNode.get, graph)) {
          moveNumber += 1
          token = nextNode.get
          if((stopThinkingTime - startThinkingTime) / 1e9d <= Defs.tl) {
            if(whichPlayerAmI == 'A' && moveNumber == 1) {
              val elist = graph.edges.toList.map(e => (e._1.value, e._2.value))
              context.actorSelection(opponent) ! ('B', token, opComp, self.path, myComp, elist)
            } else { // This is my move. Now, it is my opponent's turn.
              context.actorSelection(opponent) ! token
              if (Rules.possibleMoves(token, graph) == 0) {
                context.actorSelection(myComp) ! Write(ByteString("230\n"))
                println(s"${myComp.name} vs ${opComp.name}, result 1:0")
              }
            }
          } else {
            context.actorSelection(myComp) ! Write(ByteString("241\n"))
            context.actorSelection(opComp) ! Write(ByteString("231\n"))
            println(s"${myComp.name} vs ${opComp.name}, result 0:1")
          }
        } else {
          context.actorSelection(myComp) ! Write(ByteString(s"999 Illegal move (${nextNode.get})\n"))
          context.actorSelection(opComp) ! Write(ByteString(s"999 Your opponent made an illegal move\n"))
          println(s"${myComp.name} vs ${opComp.name}, result 0:1")
        }
      } else {
        println(s"Unrecognized command ${decoded} from ${sender().toString()}")
        context.actorSelection(myComp) ! Write(ByteString(s"999 Unrecognized command\n"))
        context.actorSelection(opComp) ! Write(ByteString(s"999 Your opponent send wrong msg\n"))
        println(s"${myComp.name} vs ${opComp.name}, result 0:1")
      }
    case info: Tuple6[Char, Int, ActorPath, ActorPath, ActorPath, List[(Int, Int)]] =>
      whichPlayerAmI = info._1
      token = info._2
      myComp = info._3
      opponent = info._4
      opComp = info._5
      graph = mutable.Graph.from(0 until Defs.gs, info._6.map(p => p._1 ~ p._2))
      val opis = new StringBuilder(4*graph.size)
      opis.append(s"200 ${graph.nodes.size} ${token} ${graph.edges.size}")
      for (e <- graph.edges) opis.append(s" ${e._1} ${e._2}")
      opis.append("\n")
      context.actorSelection(myComp) ! Write(ByteString(opis.toString()))
      startThinkingTime = System.nanoTime
    case move: Int =>
      Rules.makeMove(token, move, graph)
      token = move
      if (Rules.possibleMoves(token, graph) == 0) {
        context.actorSelection(myComp) ! Write(ByteString("240\n"))
        println(s"${myComp.name} vs ${opComp.name}, result 0:1")
      } else {
        context.actorSelection(myComp) ! Write(ByteString(s"220 ${move}\n"))
        startThinkingTime = System.nanoTime
      }
    case message: ConnectionClosed =>
      println(s"Connection with ${self.path.name} has been closed.")
      context stop self
  }
}

object TCPConnectionHandler {
  def props: Props = Props(classOf[TCPConnectionHandler])
}

object Serwer {
  val help: String = """
  |Usage: java -jar serwer.jar arguments
  |Where the allowed arguments are:
  |  -h | --help          Show help
  |  -i | --ip address    IP address of this server (required)
  |  -p | --port number   Port on listening (required)
  |  -g | --gsize number  The size of a game graph (default = 100 nodes)
  |  -t | --time seconds  Time limit for a single move (default = 2 seconds)
  |""".stripMargin

  def quit(status: Int = 0, message: String = ""): Nothing = {
    if (message.nonEmpty) println(s"ERROR: $message")
    println(help)
    sys.exit(status)
  }

  case class Args(ipAddress: Option[String], portNumber: Option[String],
                  graphSize: Option[String], timeLimit: Option[String])

  def parseArgList(params: Array[String]): Args = {
    @tailrec
    def pa(params2: Seq[String], args: Args): Args = params2 match {
      case Nil => args
      case ("-h" | "--help") +: Nil => quit()
      case ("-i" | "--ip") +: address +: tail =>
        pa(tail, args.copy(ipAddress = Some(address)))
      case ("-p" | "--port") +: number +: tail =>
        pa(tail, args.copy(portNumber = Some(number)))
      case ("-g" | "--gsize") +: gsize +: tail =>
        pa(tail, args.copy(graphSize = Some(gsize)))
      case ("-t" | "--time") +: secs +: tail =>
        pa(tail, args.copy(timeLimit = Some(secs)))
      case _ => quit(1, s"Unrecognized argument ${params2.head}")
    }

    val argz = pa(params.toList, Args(None, None, Option("100"), Option("2")))
    if (argz.ipAddress.isEmpty || argz.portNumber.isEmpty)
      quit(1, "Must specify IP address and a port number.")
    argz
  }

  def main(params: Array[String]): Unit = {
    val argz = parseArgList(params)
    if(argz.graphSize.get.toInt < 32) quit(1, "Graph size should be >= 32.")
    Defs.gs = argz.graphSize.get.toInt
    Defs.tl = argz.timeLimit.get.toInt
    val system = ActorSystem()
    val tcpserver = system.actorOf(TCPConnectionManager.props(
      argz.ipAddress.get, argz.portNumber.get.toInt))
  }
}
