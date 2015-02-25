import com.typesafe.config._

import math._
import scala.swing._
import scala.swing.event._
import de.sciss.osc._
import Implicits._

import akka.actor._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global


abstract class ObjectMusical
abstract class SimpleObjectMusical () extends ObjectMusical
abstract class ComposeObjectMusical () extends ObjectMusical
case class Note (pitch:Int, dur:Int, vel:Int) extends SimpleObjectMusical
case class Rest (dur:Int) extends SimpleObjectMusical
case class Sequential (elements:List[ObjectMusical]) extends ComposeObjectMusical
case class Parallel (elements:List[ObjectMusical]) extends ComposeObjectMusical
case class Poly (elements:List[ObjectMusical]) extends ComposeObjectMusical
case class Voix (numVoix:Int, elements:List[ObjectMusical]) extends ComposeObjectMusical
case class Chord (date:Int, elements:List[ObjectMusical]) extends ComposeObjectMusical


object score {
	// Acteur pour le concert
	class Concert (instrumentists : Array[ActorRef])
	extends Actor {
	  def receive = {
	    case Poly(l) => (
				l.foreach(
					v => {
						v match {
							case Voix (n, l) => {
								println("concert send voice " + n + " to the instrumentist")
								instrumentists(n) ! v
							}
						}
					}
				)
			)
	  }
	}

	// Acteur pour un instrumentiste
	class Instrumentist (port: Int)
	extends Actor {

		val cfg = UDP.Config()
		val c = UDP.Client( localhost -> port, cfg )

		override def preStart {
			cfg.codec = PacketCodec().doublesAsFloats().booleansAsInts()
			c.connect()
		}

	  def receive = {
	    case v@Voix(n,l) => {
				println("instrumentist (port " + port + ") start playing voice " + n)
				val notes = collectNotes (v, 0)
				notes.foreach(n=>context.system.scheduler.scheduleOnce ((n(0)) milliseconds)
				(noteOn (n(1), n(2), n(3), c)))
				context.system.scheduler.scheduleOnce ((duration (v)) milliseconds) (stopObjet (c))
			}
		}
	}

	// Calcule la duree d'un objet musical
	def duration (obj:ObjectMusical):Int =
			obj match {
			case Note(p,d,v) => d
			case Rest(d) => d
			case Sequential (l) => (l.map(duration)).foldLeft(0)(_+_)
			case Parallel (l) => (l.map(duration)).foldLeft(0)(math.max)
			case Poly (l) => (l.map(duration)).foldLeft(0)(math.max)
			case Voix (n,l) => (l.map(duration)).foldLeft(0)(math.max)
			case Chord(d,l) => 999999 // seems to work fine for our purpose
	}

	// Copy un objet musical
	def copy (obj:ObjectMusical):ObjectMusical =
			obj match {
			case Note(p,d,v) => Note(p,d,v)
			case Rest(d) => Rest(d)
			case Sequential (l) => Sequential (l.map(copy))
			case Parallel (l) => Parallel (l.map(copy))
			case Poly (l) => Poly (l.map(copy))
			case Voix (n,l) => Voix (n,l.map(copy))
			case Chord (d,l) => Chord (d,l.map(copy))
	}

	// Compte le nombre de notes d'un objet musical
	def note_count (obj:ObjectMusical):Int =
			obj match {
			case Note(p,d,v) => 1
			case Parallel (l) => (l.map(note_count)).foldLeft(0)(_+_)
			case Sequential (l) => (l.map(note_count)).foldLeft(0)(_+_)
			case Poly (l) => (l.map(note_count)).foldLeft(0)(_+_)
			case Voix (n,l) => (l.map(note_count)).foldLeft(0)(_+_)
			case Chord (d,l) => (l.map(note_count)).foldLeft(0)(_+_)
			case _ => 0
	}

	// Strech un objet musical par un factor fact
	def stretch (obj:ObjectMusical, fact:Double ):ObjectMusical =
			obj match {
			case Note(p,d,v) => Note(p,(d*fact).toInt,v)
			case Rest(d) => Rest((d*fact).toInt)
			case Parallel (l) => Parallel (l.map(stretch (_,fact)))
			case Sequential (l) => Sequential (l.map(stretch (_,fact)))
			case Poly (l) => Poly (l.map(stretch (_,fact)))
			case Voix (n,l) => Voix (n,l.map(stretch (_,fact)))
			case Chord (d,l) => Chord (d,l.map(stretch (_,fact)))
	}


	// Transpose obj de n demitons
	def transpose (obj:ObjectMusical, n:Int ):ObjectMusical =
			obj match {
			case Note(p,d,v) => Note(p+n,d,v)
			case Rest(d) => Rest(d)
			case Parallel (l) => Parallel (l.map(transpose (_,n)))
			case Sequential (l) => Sequential (l.map(transpose (_,n)))
			case Poly (l) => Poly (l.map(transpose (_,n)))
			case Voix (n,l) => Voix (n,l.map(transpose (_,n)))
			case Chord (d,l) => Chord (d,l.map(transpose (_,n)))
	}

	// mirror de obj au tour du center c
	def mirror (obj:ObjectMusical, c:Int ):ObjectMusical =
			obj match {
			case Note(p,d,v) => Note(c - (p - c),d,v)
			case Rest(d) => Rest(d)
			case Parallel (l) => Parallel (l.map(mirror (_,c)))
			case Sequential (l) => Sequential (l.map(mirror (_,c)))
			case Poly (l) => Poly (l.map(mirror (_,c)))
			case Voix (n,l) => Voix (n,l.map(mirror (_,c)))
			case Chord (d,l) => Chord (d,l.map(mirror (_,c)))
	}

	// retrograde un obj
	def retrograde (obj:ObjectMusical):ObjectMusical =
			obj match {
			case Sequential (l) => Sequential (l.reverse.map(retrograde))
			case o => o
	}

	// make a list of n obj
	def mk_list (obj:ObjectMusical, rep:List[ObjectMusical], n:Int):List[ObjectMusical] =
			if (rep.length == n) rep
			else mk_list (obj, obj::rep , n)

	// make a sequential avec n fois obj
	def repeat (obj:ObjectMusical, n:Int):ObjectMusical =
	Sequential (mk_list (obj, Nil, n))

	// make obj en parallele avec lui meme avec un decalage de n ms.
	def canon (obj:ObjectMusical, n:Int):ObjectMusical =
	Parallel (obj::Sequential (Rest(n)::obj::Nil)::Nil)


	//  Met obj1 et obj2 en seqeunce
	def concat (obj1:ObjectMusical, obj2:ObjectMusical):ObjectMusical =
	Sequential (obj1::obj2::Nil)



	def concat_par_tones (obj:ObjectMusical, tone:Int, n:Int):ObjectMusical = {
			var rep:List[ObjectMusical] = List (obj)
					for (i <- 0 to n-1)
						rep = (transpose(obj, i*tone))::rep
						Sequential (rep.reverse)
	}

/*
	//=========================EDITOR
	class ScorePanel (obj:ObjectMusical) extends Panel {
		listenTo(mouse.clicks)
		reactions += {
			case e: MouseClicked => playObject(this.obj)}
			override def paint(g:Graphics2D) {
				drawAt (this.obj, 0, g)
		}
	}


	def openEditor (obj:ObjectMusical): Unit = {
			val frame=new MainFrame (null) {
				title = "PPC Editor"
				contents = new ScorePanel(obj)
				size = new Dimension(500,254)
			}
			frame.visible=true
	}

	def drawAt (obj:ObjectMusical, at:Int, g:Graphics2D): Unit = {
			obj match {
			case Note(p,d,v) => g.fillRect(at/50,254-p*2,d/50,2)
			case Rest(d) => Nil
			case Sequential (l) => {var date = at
			l.foreach(n=>{drawAt(n,date,g); date = date + duration(n)})}
			case Parallel (l) => l.foreach(n=>drawAt(n,at,g))
			case Poly (l) => l.foreach(n=>drawAt(n,at,g)) // looks good
			case Voix (n,l) => l.foreach(n=>drawAt(n,at,g))
			case Chord (d,l) => {var date = d
			l.foreach(n=>{drawAt(n,date+at,g); date = date + duration(n)})} // decalage de d
			}
	}

	//=========================PLAYER
	def playObject (obj:ObjectMusical, port:Int): Unit = {
		println (port)
		val delay = 500
		val cfg = UDP.Config()
		cfg.codec = PacketCodec().doublesAsFloats().booleansAsInts()
		val c = UDP.Client( "127.0.0.1" -> port, cfg )
		c.connect()
		val system = akka.actor.ActorSystem("system")

		val notes = collectNotes (obj, 0)
		notes.foreach(n=>system.scheduler.scheduleOnce ((n(0)) milliseconds)
		(noteOn (n(1), n(2), n(3), c)))
		system.scheduler.scheduleOnce ((duration (obj)) milliseconds) (stopObjet (c))
	}


	//=========================dispatcher to each actor
	def launchObject (obj:ObjectMusical): Unit = {
		var port=3341
		obj match {
				case Poly (l) => (l.foreach(v=> {playObject(v, port) ;port+=1}))
		}
	}
*/

	def noteOn (pitch:Int, vel:Int, dur:Int, c:Client): Unit =
			c ! Message("/noteon", pitch,  vel, dur)

	def stopObjet (c:Client): Unit = c.close()

	def collectNotes (obj:ObjectMusical, at:Int): List [List [Int]] = {
		obj match {
		case Note(p,d,v) => List (at::p::v::d::Nil)
		case Rest(d) => Nil
		case Sequential (l) => {var date = at
		concatListas (l.map(n=>{val rep = collectNotes(n,date);
		date = date + duration(n); rep}))}
		case Parallel (l) => concatListas (l.map(n=>collectNotes(n,at)))
		case Poly (l) => concatListas (l.map(n=>collectNotes(n,at))) // looks ok
		case Voix (num,l) => concatListas (l.map(n=>collectNotes(n,at))) // looks ok too
		case Chord (d,l) => { var date = d
		concatListas (l.map(n=>{val rep = collectNotes(n,at+date);
		date = date + duration(n);rep} ))}
		}
	}

	def concatListas (lst:List [List [List [Int]]]):List [List [Int]] ={
		var rep = List.empty[List [Int]]
				lst.foreach(n=>rep = n:::rep)
				rep
	}


	//////////////////////////////////////////////////

	def main(args: Array[String]): Unit = {

		println("reading application.conf")
		var conf = ConfigFactory.load()

		val nrOfInstrumentists = conf.getInt("culto.nrOfInstrumentists")
		val startingPort = conf.getInt("culto.startingPort")
		val offsetVoice = conf.getInt("culto.offsetVoice")

		// debug purpose
		println("number of instrumentists = " + nrOfInstrumentists)
		println("starting port = " + startingPort) // first port to use for pd connections
		println("offset voice = " + offsetVoice) // index at which the voice start
		println()

	 	val system = ActorSystem("ConcertSystem")

		// spawning the actors for the instrumentists
		var instrumentists:Array[ActorRef] = new Array[ActorRef](nrOfInstrumentists)
		for(i <- 0 until nrOfInstrumentists){
        val instr = system.actorOf(Props(new Instrumentist(startingPort+i)),
					name = "instrument"+i)
				instrumentists(i) = instr
    }

		// spawning the concert
		val concert = system.actorOf(Props(new Concert(instrumentists)),
      name = "concert")


		println("start the concert by sending the motete to the concert actor")
		// sending the motete to the concert
		concert ! motete


		// old stuff for the GUI, but not ported yet...
		// openEditor(motete)
	}



	//////////////////////////////////////////////////
	//       				  poly to load 									//
  //////////////////////////////////////////////////

	// this is motete8.scala
	val motete =
		Poly (List (Voix (0 , List (Chord (0 , List (Note (67, 80, 3857)))
		, Chord (3857 , List (Note (67, 80, 1943)))
		, Chord (5800 , List (Note (69, 80, 2867)))
		, Chord (8667 , List (Note (71, 80, 1333)))
		, Chord (10000 , List (Note (69, 80, 600)))
		, Chord (10600 , List (Note (71, 80, 971)))
		, Chord (11571 , List (Note (72, 80, 1929)))
		, Chord (13500 , List (Note (71, 80, 929)))
		, Chord (14429 , List (Note (69, 80, 946)))
		, Chord (15375 , List (Note (67, 80, 500)))
		, Chord (15875 , List (Note (67, 80, 982)))
		, Chord (16857 , List (Note (72, 80, 476)))
		, Chord (17333 , List (Note (71, 80, 1096)))
		, Chord (18429 , List (Note (69, 80, 2142)))
		, Chord (20571 , List (Note (67, 80, 1096)))
		, Chord (21667 , List (Note (66, 80, 6500)))
		, Chord (28167 , List (Note (67, 80, 2166)))
		, Chord (30333 , List (Note (69, 80, 4334)))
		, Chord (34667 , List (Note (74, 80, 708)))
		, Chord (35375 , List (Note (72, 80, 625)))
		, Chord (36000 , List (Note (71, 80, 1000)))
		, Chord (37167 , List (Note (69, 80, 500)))
		, Chord (37667 , List (Note (67, 80, 458)))
		, Chord (38125 , List (Note (66, 80, 1042)))
		, Chord (39167 , List (Note (67, 80, 1033)))
		, Chord (40600 , List (Note (66, 80, 543)))
		, Chord (41143 , List (Note (64, 80, 429)))
		, Chord (41571 , List (Note (62, 80, 2096)))
		, Chord (44143 , List (Note (71, 80, 429)))
		, Chord (44571 , List (Note (72, 80, 429)))
		, Chord (47375 , List (Note (71, 80, 1125)))
		, Chord (48500 , List (Note (69, 80, 1167)))
		, Chord (49667 , List (Note (67, 80, 2333)))
		, Chord (52000 , List (Note (69, 80, 834)))
		, Chord (52833 , List (Note (71, 80, 1667)))
		, Chord (54500 , List (Note (69, 80, 4929)))
		, Chord (59429 , List (Note (67, 80, 821)))
		, Chord (60250 , List (Note (67, 80, 1625)))
		, Chord (61875 , List (Note (67, 80, 4982)))
		, Chord (66857 , List (Note (69, 80, 7429)))
		, Chord (84167 , List (Note (71, 80, 500)))
		, Chord (84667 , List (Note (69, 80, 619)))
		, Chord (85286 , List (Note (71, 80, 571)))
		, Chord (85857 , List (Note (72, 80, 810)))
		, Chord (86667 , List (Note (71, 80, 833)))
		, Chord (87500 , List (Note (69, 80, 1643)))
		, Chord (89143 , List (Note (67, 80, 4857)))
		, Chord (94000 , List (Note (67, 80, 2429)))
		, Chord (96429 , List (Note (72, 80, 1196)))
		, Chord (97625 , List (Note (71, 80, 2804)))
		, Chord (100429 , List (Note (69, 80, 1446)))
		, Chord (101875 , List (Note (67, 80, 2125)))
		, Chord (104000 , List (Note (66, 80, 3857)))
		, Chord (107857 , List (Note (67, 80, 1893)))
		, Chord (109750 , List (Note (69, 80, 2917)))
		, Chord (112667 , List (Note (74, 80, 1333)))
		, Chord (114000 , List (Note (72, 80, 600)))
		, Chord (114600 , List (Note (71, 80, 2900)))
		, Chord (117500 , List (Note (69, 80, 625)))
		, Chord (118125 , List (Note (67, 80, 375)))
		, Chord (118500 , List (Note (66, 80, 900)))
		, Chord (119400 , List (Note (67, 80, 1457)))
		, Chord (120857 , List (Note (66, 80, 310)))
		, Chord (121167 , List (Note (64, 80, 167)))
		, Chord (121333 , List (Note (62, 80, 9792)))
		, Chord (131125 , List (Note (71, 80, 2125)))
		, Chord (133250 , List (Note (72, 80, 1083)))
		, Chord (134333 , List (Note (71, 80, 1917)))
		, Chord (136250 , List (Note (69, 80, 950)))
		, Chord (137200 , List (Note (67, 80, 1467)))
		, Chord (138667 , List (Note (69, 80, 2762)))
		, Chord (141429 , List (Note (71, 80, 1405)))
		, Chord (142833 , List (Note (69, 80, 1834)))
		, Chord (144667 , List (Note (67, 80, 933)))
		, Chord (145600 , List (Note (67, 80, 2067)))
		, Chord (147667 , List (Note (67, 80, 933)))
		, Chord (148600 , List (Note (69, 80, 400)))
		, Chord (149000 , List (Note (71, 80, 3143)))
		, Chord (152143 , List (Note (69, 80, 1524)))
		, Chord (153667 , List (Note (71, 80, 2333)))
		, Chord (156000 , List (Note (72, 80, 3286)))
		, Chord (159286 , List (Note (71, 80, 1714)))
		, Chord (161000 , List (Note (69, 80, 2429)))
		, Chord (163429 , List (Note (67, 80, 3285)))
		, Chord (166714 , List (Note (67, 80, 1661)))
		, Chord (168375 , List (Note (72, 80, 2482)))
		, Chord (170857 , List (Note (71, 80, 810)))
		, Chord (171667 , List (Note (69, 80, 1666)))
		, Chord (173333 , List (Note (67, 80, 4953)))
		, Chord (178286 , List (Note (66, 80, 6589)))
		, Chord (201625 , List (Note (67, 80, 2804)))
		, Chord (204429 , List (Note (69, 80, 1446)))
		, Chord (205875 , List (Note (74, 80, 2125)))
		, Chord (208000 , List (Note (72, 80, 3857)))
		, Chord (211857 , List (Note (71, 80, 1893)))
		, Chord (213750 , List (Note (69, 80, 2917)))
		, Chord (216667 , List (Note (67, 80, 333)))
		, Chord (217000 , List (Note (66, 80, 625)))
		, Chord (217625 , List (Note (67, 80, 1500)))
		, Chord (219125 , List (Note (66, 80, 125)))
		, Chord (219250 , List (Note (64, 80, 125)))
		, Chord (219375 , List (Note (62, 80, 125)))
		, Chord (219500 , List (Note (71, 80, 2000)))
		, Chord (221500 , List (Note (72, 80, 2833)))
		, Chord (224333 , List (Note (71, 80, 333)))
		, Chord (224667 , List (Note (69, 80, 333)))
		, Chord (225000 , List (Note (67, 80, 250)))
		, Chord (225250 , List (Note (69, 80, 9875)))
		, Chord (235125 , List (Note (71, 80, 1042)))
		, Chord (236167 , List (Note (69, 80, 1083)))
		, Chord (237250 , List (Note (67, 80, 1083)))
		, Chord (238333 , List (Note (67, 80, 500)))
		, Chord (238833 , List (Note (67, 80, 417)))
		, Chord (239250 , List (Note (69, 80, 500)))
		, Chord (239750 , List (Note (71, 80, 750)))
		, Chord (240500 , List (Note (69, 80, 700)))
		, Chord (241200 , List (Note (71, 80, 1467)))
		, Chord (246125 , List (Note (72, 80, 1042)))
		, Chord (247167 , List (Note (71, 80, 1000)))
		, Chord (248167 , List (Note (69, 80, 500)))
		, Chord (248667 , List (Note (67, 80, 933)))
		, Chord (249600 , List (Note (67, 80, 2067)))
		, Chord (251667 , List (Note (72, 80, 458)))
		, Chord (252125 , List (Note (71, 80, 875)))
		, Chord (253000 , List (Note (69, 80, 1000)))
		, Chord (254000 , List (Note (67, 80, 1375)))
		, Chord (255375 , List (Note (66, 80, 4625)))
		, Chord (260000 , List (Note (67, 80, 800)))
		, Chord (260800 , List (Note (69, 80, 1700)))
		, Chord (262500 , List (Note (74, 80, 4929)))
		, Chord (267429 , List (Note (72, 80, 14857)))
		))
		, Voix (1 , List (Chord (0 , List (Note (76, 80, 3857)))
		, Chord (3857 , List (Note (76, 80, 1943)))
		, Chord (5800 , List (Note (76, 80, 2867)))
		, Chord (8667 , List (Note (76, 80, 1333)))
		, Chord (10000 , List (Note (74, 80, 600)))
		, Chord (10600 , List (Note (72, 80, 971)))
		, Chord (11571 , List (Note (71, 80, 1929)))
		, Chord (13500 , List (Note (69, 80, 929)))
		, Chord (14429 , List (Note (67, 80, 429)))
		, Chord (14857 , List (Note (69, 80, 643)))
		, Chord (15500 , List (Note (71, 80, 1214)))
		, Chord (16714 , List (Note (72, 80, 619)))
		, Chord (17333 , List (Note (74, 80, 1096)))
		, Chord (18429 , List (Note (72, 80, 2142)))
		, Chord (20571 , List (Note (71, 80, 1096)))
		, Chord (21667 , List (Note (69, 80, 5762)))
		, Chord (27429 , List (Note (71, 80, 2904)))
		, Chord (30333 , List (Note (72, 80, 4334)))
		, Chord (34667 , List (Note (71, 80, 708)))
		, Chord (35375 , List (Note (69, 80, 625)))
		, Chord (36000 , List (Note (69, 80, 1000)))
		, Chord (37167 , List (Note (67, 80, 500)))
		, Chord (37667 , List (Note (66, 80, 458)))
		, Chord (38125 , List (Note (64, 80, 875)))
		, Chord (39000 , List (Note (62, 80, 875)))
		, Chord (40429 , List (Note (60, 80, 571)))
		, Chord (41000 , List (Note (62, 80, 500)))
		, Chord (41500 , List (Note (64, 80, 1250)))
		, Chord (43500 , List (Note (66, 80, 786)))
		, Chord (44286 , List (Note (67, 80, 714)))
		, Chord (47375 , List (Note (66, 80, 1125)))
		, Chord (48500 , List (Note (64, 80, 1167)))
		, Chord (49667 , List (Note (69, 80, 2333)))
		, Chord (52000 , List (Note (69, 80, 1000)))
		, Chord (53000 , List (Note (69, 80, 1500)))
		, Chord (54500 , List (Note (67, 80, 4929)))
		, Chord (59429 , List (Note (66, 80, 821)))
		, Chord (60250 , List (Note (64, 80, 1625)))
		, Chord (61875 , List (Note (62, 80, 4982)))
		, Chord (66857 , List (Note (67, 80, 7429)))
		, Chord (84200 , List (Note (69, 80, 600)))
		, Chord (84800 , List (Note (71, 80, 486)))
		, Chord (85286 , List (Note (72, 80, 571)))
		, Chord (85857 , List (Note (74, 80, 810)))
		, Chord (86667 , List (Note (76, 80, 833)))
		, Chord (87500 , List (Note (74, 80, 1643)))
		, Chord (89143 , List (Note (72, 80, 4857)))
		, Chord (94000 , List (Note (71, 80, 2429)))
		, Chord (96429 , List (Note (69, 80, 1196)))
		, Chord (97625 , List (Note (71, 80, 2804)))
		, Chord (100429 , List (Note (72, 80, 1446)))
		, Chord (101875 , List (Note (71, 80, 2125)))
		, Chord (104000 , List (Note (69, 80, 3857)))
		, Chord (107857 , List (Note (69, 80, 1893)))
		, Chord (109750 , List (Note (67, 80, 2917)))
		, Chord (112667 , List (Note (66, 80, 1333)))
		, Chord (114000 , List (Note (64, 80, 600)))
		, Chord (114600 , List (Note (62, 80, 2900)))
		, Chord (117500 , List (Note (61, 80, 643)))
		, Chord (118143 , List (Note (62, 80, 286)))
		, Chord (118429 , List (Note (64, 80, 429)))
		, Chord (118857 , List (Note (66, 80, 1857)))
		, Chord (120714 , List (Note (67, 80, 411)))
		, Chord (121125 , List (Note (69, 80, 250)))
		, Chord (121375 , List (Note (67, 80, 9750)))
		, Chord (131125 , List (Note (66, 80, 2125)))
		, Chord (133250 , List (Note (64, 80, 1083)))
		, Chord (134333 , List (Note (76, 80, 1917)))
		, Chord (136250 , List (Note (76, 80, 950)))
		, Chord (137200 , List (Note (76, 80, 1467)))
		, Chord (138667 , List (Note (76, 80, 2762)))
		, Chord (141429 , List (Note (74, 80, 1405)))
		, Chord (142833 , List (Note (72, 80, 1834)))
		, Chord (144667 , List (Note (71, 80, 933)))
		, Chord (145600 , List (Note (69, 80, 1150)))
		, Chord (146750 , List (Note (67, 80, 1536)))
		, Chord (148286 , List (Note (69, 80, 714)))
		, Chord (149000 , List (Note (71, 80, 3143)))
		, Chord (152143 , List (Note (72, 80, 1524)))
		, Chord (153667 , List (Note (74, 80, 2333)))
		, Chord (156000 , List (Note (72, 80, 3286)))
		, Chord (159286 , List (Note (71, 80, 1714)))
		, Chord (161000 , List (Note (69, 80, 2429)))
		, Chord (163429 , List (Note (71, 80, 3285)))
		, Chord (166714 , List (Note (72, 80, 1661)))
		, Chord (168375 , List (Note (71, 80, 2482)))
		, Chord (170857 , List (Note (69, 80, 810)))
		, Chord (171667 , List (Note (69, 80, 1666)))
		, Chord (173333 , List (Note (67, 80, 4953)))
		, Chord (178286 , List (Note (66, 80, 6589)))
		, Chord (201625 , List (Note (64, 80, 2804)))
		, Chord (204429 , List (Note (62, 80, 1446)))
		, Chord (205875 , List (Note (62, 80, 2125)))
		, Chord (208000 , List (Note (62, 80, 3857)))
		, Chord (211857 , List (Note (64, 80, 1893)))
		, Chord (213750 , List (Note (66, 80, 2917)))
		, Chord (216667 , List (Note (67, 80, 333)))
		, Chord (217000 , List (Note (66, 80, 625)))
		, Chord (217625 , List (Note (64, 80, 1500)))
		, Chord (219125 , List (Note (69, 80, 125)))
		, Chord (219250 , List (Note (69, 80, 125)))
		, Chord (219375 , List (Note (69, 80, 125)))
		, Chord (219500 , List (Note (67, 80, 1500)))
		, Chord (221000 , List (Note (66, 80, 3250)))
		, Chord (224250 , List (Note (64, 80, 375)))
		, Chord (224625 , List (Note (62, 80, 375)))
		, Chord (225000 , List (Note (67, 80, 333)))
		, Chord (225333 , List (Note (69, 80, 9792)))
		, Chord (235125 , List (Note (71, 80, 1042)))
		, Chord (236167 , List (Note (72, 80, 1083)))
		, Chord (237250 , List (Note (74, 80, 1083)))
		, Chord (238333 , List (Note (76, 80, 500)))
		, Chord (238833 , List (Note (74, 80, 417)))
		, Chord (239250 , List (Note (72, 80, 500)))
		, Chord (239750 , List (Note (71, 80, 750)))
		, Chord (240500 , List (Note (69, 80, 700)))
		, Chord (241200 , List (Note (71, 80, 1467)))
		, Chord (246125 , List (Note (72, 80, 875)))
		, Chord (247000 , List (Note (71, 80, 857)))
		, Chord (247857 , List (Note (69, 80, 572)))
		, Chord (248429 , List (Note (69, 80, 1171)))
		, Chord (249600 , List (Note (67, 80, 1150)))
		, Chord (250750 , List (Note (66, 80, 750)))
		, Chord (251500 , List (Note (64, 80, 1500)))
		, Chord (253000 , List (Note (62, 80, 1000)))
		, Chord (254000 , List (Note (61, 80, 1375)))
		, Chord (255375 , List (Note (62, 80, 4625)))
		, Chord (260000 , List (Note (64, 80, 834)))
		, Chord (260833 , List (Note (66, 80, 1667)))
		, Chord (262500 , List (Note (67, 80, 4929)))
		, Chord (267429 , List (Note (69, 80, 14857)))
		))
		, Voix (2 , List (Chord (0 , List (Note (55, 80, 3286)))
		, Chord (3286 , List (Note (55, 80, 1714)))
		, Chord (5000 , List (Note (57, 80, 2429)))
		, Chord (7429 , List (Note (59, 80, 3285)))
		, Chord (10714 , List (Note (57, 80, 1661)))
		, Chord (12375 , List (Note (59, 80, 2482)))
		, Chord (14857 , List (Note (60, 80, 810)))
		, Chord (15667 , List (Note (59, 80, 1666)))
		, Chord (17333 , List (Note (57, 80, 4953)))
		, Chord (22286 , List (Note (55, 80, 6589)))
		, Chord (28875 , List (Note (55, 80, 1125)))
		, Chord (30000 , List (Note (60, 80, 1125)))
		, Chord (31125 , List (Note (59, 80, 1075)))
		, Chord (32200 , List (Note (57, 80, 600)))
		, Chord (32800 , List (Note (55, 80, 486)))
		, Chord (33286 , List (Note (54, 80, 571)))
		, Chord (33857 , List (Note (55, 80, 810)))
		, Chord (34667 , List (Note (57, 80, 833)))
		, Chord (35500 , List (Note (62, 80, 1643)))
		, Chord (40000 , List (Note (60, 80, 800)))
		, Chord (40800 , List (Note (59, 80, 800)))
		, Chord (41600 , List (Note (57, 80, 400)))
		, Chord (42000 , List (Note (55, 80, 800)))
		, Chord (42800 , List (Note (54, 80, 1600)))
		, Chord (44400 , List (Note (55, 80, 400)))
		, Chord (44800 , List (Note (54, 80, 825)))
		, Chord (45625 , List (Note (52, 80, 708)))
		, Chord (46333 , List (Note (50, 80, 1417)))
		, Chord (47750 , List (Note (59, 80, 4250)))
		, Chord (52000 , List (Note (60, 80, 1000)))
		, Chord (53000 , List (Note (59, 80, 1875)))
		, Chord (54875 , List (Note (57, 80, 5792)))
		, Chord (60667 , List (Note (55, 80, 2904)))
		, Chord (63571 , List (Note (57, 80, 1429)))
		, Chord (65000 , List (Note (59, 80, 1429)))
		, Chord (66429 , List (Note (57, 80, 1446)))
		, Chord (67875 , List (Note (55, 80, 1458)))
		, Chord (69333 , List (Note (55, 80, 1096)))
		, Chord (70429 , List (Note (55, 80, 1071)))
		, Chord (71500 , List (Note (57, 80, 1071)))
		, Chord (72571 , List (Note (59, 80, 1096)))
		, Chord (73667 , List (Note (57, 80, 1619)))
		, Chord (75286 , List (Note (59, 80, 1589)))
		, Chord (76875 , List (Note (60, 80, 1125)))
		, Chord (78000 , List (Note (59, 80, 3250)))
		, Chord (81250 , List (Note (57, 80, 1083)))
		, Chord (82333 , List (Note (55, 80, 4334)))
		, Chord (86667 , List (Note (55, 80, 2762)))
		, Chord (89429 , List (Note (60, 80, 1405)))
		, Chord (90833 , List (Note (59, 80, 1834)))
		, Chord (92667 , List (Note (57, 80, 933)))
		, Chord (93600 , List (Note (55, 80, 2067)))
		, Chord (95667 , List (Note (54, 80, 933)))
		, Chord (96600 , List (Note (55, 80, 400)))
		, Chord (97000 , List (Note (57, 80, 3143)))
		, Chord (100143 , List (Note (62, 80, 1524)))
		, Chord (101667 , List (Note (60, 80, 2333)))
		, Chord (104000 , List (Note (59, 80, 3286)))
		, Chord (107286 , List (Note (57, 80, 1714)))
		, Chord (109000 , List (Note (55, 80, 2429)))
		, Chord (111429 , List (Note (54, 80, 7428)))
		, Chord (118857 , List (Note (55, 80, 3310)))
		, Chord (122167 , List (Note (54, 80, 1633)))
		, Chord (123800 , List (Note (52, 80, 2486)))
		, Chord (126286 , List (Note (50, 80, 2914)))
		, Chord (129200 , List (Note (59, 80, 1467)))
		, Chord (130667 , List (Note (60, 80, 1458)))
		, Chord (132125 , List (Note (59, 80, 750)))
		, Chord (132875 , List (Note (57, 80, 2250)))
		, Chord (135125 , List (Note (55, 80, 750)))
		, Chord (135875 , List (Note (57, 80, 325)))
		, Chord (136200 , List (Note (59, 80, 2200)))
		, Chord (138400 , List (Note (57, 80, 1100)))
		, Chord (139500 , List (Note (55, 80, 1643)))
		, Chord (141143 , List (Note (55, 80, 2143)))
		, Chord (143286 , List (Note (55, 80, 1089)))
		, Chord (144375 , List (Note (57, 80, 1625)))
		, Chord (146000 , List (Note (59, 80, 800)))
		, Chord (146800 , List (Note (57, 80, 1629)))
		, Chord (148429 , List (Note (59, 80, 821)))
		, Chord (149250 , List (Note (60, 80, 375)))
		, Chord (149625 , List (Note (59, 80, 2804)))
		, Chord (152429 , List (Note (57, 80, 1446)))
		, Chord (153875 , List (Note (55, 80, 2125)))
		, Chord (156000 , List (Note (55, 80, 1000)))
		, Chord (157000 , List (Note (60, 80, 1875)))
		, Chord (158875 , List (Note (59, 80, 5792)))
		, Chord (164667 , List (Note (57, 80, 2904)))
		, Chord (171875 , List (Note (55, 80, 982)))
		, Chord (172857 , List (Note (54, 80, 476)))
		, Chord (186333 , List (Note (55, 80, 1917)))
		, Chord (188250 , List (Note (57, 80, 950)))
		, Chord (189200 , List (Note (62, 80, 1467)))
		, Chord (190667 , List (Note (60, 80, 2762)))
		, Chord (193429 , List (Note (59, 80, 1405)))
		, Chord (194833 , List (Note (57, 80, 917)))
		, Chord (195750 , List (Note (55, 80, 1850)))
		, Chord (197600 , List (Note (54, 80, 2067)))
		, Chord (199667 , List (Note (55, 80, 476)))
		, Chord (200143 , List (Note (54, 80, 857)))
		, Chord (201000 , List (Note (52, 80, 1000)))
		, Chord (202000 , List (Note (50, 80, 1375)))
		, Chord (203375 , List (Note (59, 80, 4625)))
		, Chord (208000 , List (Note (60, 80, 7429)))
		, Chord (215429 , List (Note (59, 80, 821)))
		, Chord (216250 , List (Note (57, 80, 875)))
		, Chord (217125 , List (Note (55, 80, 750)))
		, Chord (217875 , List (Note (57, 80, 1268)))
		, Chord (219143 , List (Note (59, 80, 1232)))
		, Chord (220375 , List (Note (57, 80, 2482)))
		, Chord (225333 , List (Note (55, 80, 1238)))
		, Chord (226571 , List (Note (55, 80, 1229)))
		, Chord (227800 , List (Note (55, 80, 2486)))
		, Chord (230286 , List (Note (57, 80, 714)))
		, Chord (231000 , List (Note (59, 80, 1500)))
		, Chord (232500 , List (Note (57, 80, 333)))
		, Chord (232833 , List (Note (59, 80, 738)))
		, Chord (233571 , List (Note (60, 80, 4429)))
		, Chord (238000 , List (Note (59, 80, 714)))
		, Chord (238714 , List (Note (57, 80, 1453)))
		, Chord (240167 , List (Note (55, 80, 500)))
		, Chord (240667 , List (Note (55, 80, 1167)))
		, Chord (241833 , List (Note (60, 80, 3310)))
		, Chord (245143 , List (Note (59, 80, 23524)))
		, Chord (268667 , List (Note (57, 80, 4533)))
		, Chord (273200 , List (Note (55, 80, 2300)))
		, Chord (275500 , List (Note (54, 80, 3375)))
		, Chord (278875 , List (Note (55, 80, 1125)))
		, Chord (280000 , List (Note (57, 80, 1714)))
		, Chord (281714 , List (Note (59, 80, 572)))
		))
		, Voix (3 , List (Chord (0 , List (Note (64, 80, 3286)))
		, Chord (3286 , List (Note (64, 80, 1714)))
		, Chord (5000 , List (Note (64, 80, 2429)))
		, Chord (7429 , List (Note (64, 80, 3285)))
		, Chord (10714 , List (Note (62, 80, 1661)))
		, Chord (12375 , List (Note (60, 80, 2482)))
		, Chord (14857 , List (Note (59, 80, 810)))
		, Chord (15667 , List (Note (57, 80, 1666)))
		, Chord (17333 , List (Note (55, 80, 4953)))
		, Chord (22286 , List (Note (57, 80, 6589)))
		, Chord (28875 , List (Note (59, 80, 1125)))
		, Chord (30000 , List (Note (60, 80, 1125)))
		, Chord (31125 , List (Note (62, 80, 1075)))
		, Chord (32200 , List (Note (60, 80, 600)))
		, Chord (32800 , List (Note (59, 80, 486)))
		, Chord (33286 , List (Note (57, 80, 571)))
		, Chord (33857 , List (Note (59, 80, 810)))
		, Chord (34667 , List (Note (60, 80, 833)))
		, Chord (35500 , List (Note (59, 80, 1643)))
		, Chord (40000 , List (Note (57, 80, 625)))
		, Chord (40625 , List (Note (57, 80, 708)))
		, Chord (41333 , List (Note (55, 80, 500)))
		, Chord (41833 , List (Note (54, 80, 967)))
		, Chord (42800 , List (Note (52, 80, 867)))
		, Chord (43667 , List (Note (50, 80, 666)))
		, Chord (44333 , List (Note (48, 80, 1292)))
		, Chord (45625 , List (Note (50, 80, 708)))
		, Chord (46333 , List (Note (52, 80, 1417)))
		, Chord (47750 , List (Note (54, 80, 4250)))
		, Chord (52000 , List (Note (55, 80, 1000)))
		, Chord (53000 , List (Note (54, 80, 1875)))
		, Chord (54875 , List (Note (52, 80, 5792)))
		, Chord (60667 , List (Note (57, 80, 2904)))
		, Chord (63571 , List (Note (57, 80, 1429)))
		, Chord (65000 , List (Note (57, 80, 1429)))
		, Chord (66429 , List (Note (55, 80, 1446)))
		, Chord (67875 , List (Note (54, 80, 1458)))
		, Chord (69333 , List (Note (52, 80, 1096)))
		, Chord (70429 , List (Note (50, 80, 1071)))
		, Chord (71500 , List (Note (55, 80, 1071)))
		, Chord (72571 , List (Note (57, 80, 1096)))
		, Chord (73667 , List (Note (59, 80, 1458)))
		, Chord (75125 , List (Note (60, 80, 1446)))
		, Chord (76571 , List (Note (62, 80, 1429)))
		, Chord (78000 , List (Note (64, 80, 2167)))
		, Chord (80167 , List (Note (62, 80, 2166)))
		, Chord (82333 , List (Note (60, 80, 4334)))
		, Chord (86667 , List (Note (59, 80, 2762)))
		, Chord (89429 , List (Note (57, 80, 1405)))
		, Chord (90833 , List (Note (59, 80, 1834)))
		, Chord (92667 , List (Note (60, 80, 933)))
		, Chord (93600 , List (Note (59, 80, 1150)))
		, Chord (94750 , List (Note (57, 80, 1536)))
		, Chord (96286 , List (Note (57, 80, 714)))
		, Chord (97000 , List (Note (55, 80, 3143)))
		, Chord (100143 , List (Note (54, 80, 1524)))
		, Chord (101667 , List (Note (52, 80, 2333)))
		, Chord (104000 , List (Note (50, 80, 3286)))
		, Chord (107286 , List (Note (49, 80, 1714)))
		, Chord (109000 , List (Note (50, 80, 2429)))
		, Chord (111429 , List (Note (52, 80, 7428)))
		, Chord (118857 , List (Note (54, 80, 3310)))
		, Chord (122167 , List (Note (55, 80, 1633)))
		, Chord (123800 , List (Note (57, 80, 2486)))
		, Chord (126286 , List (Note (55, 80, 2914)))
		, Chord (129200 , List (Note (54, 80, 1467)))
		, Chord (130667 , List (Note (52, 80, 1458)))
		, Chord (132125 , List (Note (64, 80, 750)))
		, Chord (132875 , List (Note (64, 80, 1325)))
		, Chord (134200 , List (Note (64, 80, 1300)))
		, Chord (135500 , List (Note (64, 80, 700)))
		, Chord (136200 , List (Note (62, 80, 2200)))
		, Chord (138400 , List (Note (60, 80, 1100)))
		, Chord (139500 , List (Note (59, 80, 1643)))
		, Chord (141143 , List (Note (57, 80, 2143)))
		, Chord (143286 , List (Note (55, 80, 1089)))
		, Chord (144375 , List (Note (57, 80, 1625)))
		, Chord (146000 , List (Note (59, 80, 800)))
		, Chord (146800 , List (Note (60, 80, 867)))
		, Chord (147667 , List (Note (62, 80, 1333)))
		, Chord (149000 , List (Note (60, 80, 625)))
		, Chord (149625 , List (Note (59, 80, 2804)))
		, Chord (152429 , List (Note (57, 80, 1446)))
		, Chord (153875 , List (Note (59, 80, 2125)))
		, Chord (156000 , List (Note (60, 80, 1000)))
		, Chord (157000 , List (Note (59, 80, 1875)))
		, Chord (158875 , List (Note (57, 80, 5792)))
		, Chord (164667 , List (Note (57, 80, 2904)))
		, Chord (171875 , List (Note (55, 80, 982)))
		, Chord (172857 , List (Note (54, 80, 476)))
		, Chord (186333 , List (Note (52, 80, 1917)))
		, Chord (188250 , List (Note (50, 80, 950)))
		, Chord (189200 , List (Note (50, 80, 1467)))
		, Chord (190667 , List (Note (50, 80, 2762)))
		, Chord (193429 , List (Note (52, 80, 1405)))
		, Chord (194833 , List (Note (54, 80, 917)))
		, Chord (195750 , List (Note (55, 80, 1850)))
		, Chord (197600 , List (Note (54, 80, 1150)))
		, Chord (198750 , List (Note (52, 80, 750)))
		, Chord (199500 , List (Note (57, 80, 1625)))
		, Chord (201125 , List (Note (57, 80, 750)))
		, Chord (201875 , List (Note (57, 80, 1500)))
		, Chord (203375 , List (Note (55, 80, 4625)))
		, Chord (208000 , List (Note (54, 80, 7429)))
		, Chord (215429 , List (Note (52, 80, 821)))
		, Chord (216250 , List (Note (50, 80, 875)))
		, Chord (217125 , List (Note (55, 80, 750)))
		, Chord (217875 , List (Note (57, 80, 1268)))
		, Chord (219143 , List (Note (59, 80, 1232)))
		, Chord (220375 , List (Note (60, 80, 2482)))
		, Chord (225333 , List (Note (62, 80, 1238)))
		, Chord (226571 , List (Note (64, 80, 1229)))
		, Chord (227800 , List (Note (62, 80, 2486)))
		, Chord (230286 , List (Note (60, 80, 714)))
		, Chord (231000 , List (Note (59, 80, 1500)))
		, Chord (232500 , List (Note (57, 80, 333)))
		, Chord (232833 , List (Note (59, 80, 738)))
		, Chord (233571 , List (Note (60, 80, 3804)))
		, Chord (237375 , List (Note (59, 80, 911)))
		, Chord (238286 , List (Note (57, 80, 1881)))
		, Chord (240167 , List (Note (57, 80, 500)))
		, Chord (240667 , List (Note (55, 80, 1167)))
		, Chord (241833 , List (Note (54, 80, 3310)))
		, Chord (245143 , List (Note (52, 80, 14857)))
		, Chord (260000 , List (Note (50, 80, 8667)))
		, Chord (268667 , List (Note (49, 80, 1500)))
		, Chord (270167 , List (Note (50, 80, 2262)))
		, Chord (272429 , List (Note (52, 80, 771)))
		, Chord (273200 , List (Note (54, 80, 5086)))
		, Chord (278286 , List (Note (55, 80, 1714)))
		, Chord (280000 , List (Note (57, 80, 2286)))
		))
		, Voix (4 , List (Chord (0 , List (Note (52, 80, 5000)))
		, Chord (5000 , List (Note (52, 80, 2429)))
		, Chord (7429 , List (Note (57, 80, 6571)))
		, Chord (14000 , List (Note (57, 80, 3333)))
		, Chord (17333 , List (Note (55, 80, 1667)))
		, Chord (19000 , List (Note (54, 80, 3286)))
		, Chord (22286 , List (Note (52, 80, 8464)))
		, Chord (30750 , List (Note (59, 80, 2125)))
		, Chord (32875 , List (Note (59, 80, 2125)))
		, Chord (35000 , List (Note (52, 80, 2143)))
		, Chord (39625 , List (Note (59, 80, 2500)))
		, Chord (42125 , List (Note (59, 80, 2446)))
		, Chord (44571 , List (Note (59, 80, 2429)))
		, Chord (47000 , List (Note (57, 80, 5000)))
		, Chord (52000 , List (Note (55, 80, 2500)))
		, Chord (54500 , List (Note (54, 80, 4929)))
		, Chord (59429 , List (Note (52, 80, 9904)))
		, Chord (69333 , List (Note (59, 80, 1667)))
		, Chord (71000 , List (Note (59, 80, 1625)))
		, Chord (72625 , List (Note (59, 80, 1661)))
		, Chord (74286 , List (Note (52, 80, 8464)))
		, Chord (82750 , List (Note (52, 80, 4250)))
		, Chord (87000 , List (Note (52, 80, 2143)))
		, Chord (89143 , List (Note (50, 80, 5732)))
		, Chord (94875 , List (Note (50, 80, 2925)))
		, Chord (104000 , List (Note (52, 80, 5000)))
		, Chord (109000 , List (Note (52, 80, 2429)))
		, Chord (111429 , List (Note (47, 80, 9904)))
		, Chord (121333 , List (Note (52, 80, 3292)))
		, Chord (124625 , List (Note (52, 80, 1661)))
		, Chord (126286 , List (Note (50, 80, 5714)))
		, Chord (132000 , List (Note (50, 80, 2750)))
		, Chord (134750 , List (Note (52, 80, 4250)))
		, Chord (139000 , List (Note (52, 80, 2143)))
		, Chord (141143 , List (Note (47, 80, 4982)))
		, Chord (146125 , List (Note (52, 80, 2446)))
		, Chord (148571 , List (Note (54, 80, 4929)))
		, Chord (153500 , List (Note (52, 80, 2500)))
		, Chord (156000 , List (Note (50, 80, 2500)))
		, Chord (158500 , List (Note (48, 80, 4929)))
		, Chord (163429 , List (Note (47, 80, 9904)))
		, Chord (178286 , List (Note (52, 80, 5714)))
		, Chord (184000 , List (Note (52, 80, 2750)))
		, Chord (186750 , List (Note (57, 80, 4250)))
		, Chord (191000 , List (Note (57, 80, 2143)))
		, Chord (193143 , List (Note (55, 80, 2857)))
		, Chord (196000 , List (Note (54, 80, 5800)))
		, Chord (208000 , List (Note (52, 80, 7429)))
		, Chord (215429 , List (Note (59, 80, 3285)))
		, Chord (218714 , List (Note (59, 80, 3286)))
		, Chord (222000 , List (Note (52, 80, 3333)))
		, Chord (227000 , List (Note (59, 80, 1625)))
		, Chord (228625 , List (Note (59, 80, 1661)))
		, Chord (230286 , List (Note (59, 80, 2839)))
		, Chord (233125 , List (Note (57, 80, 5625)))
		, Chord (238750 , List (Note (55, 80, 2125)))
		, Chord (240875 , List (Note (54, 80, 4268)))
		, Chord (245143 , List (Note (52, 80, 14857)))
		, Chord (260000 , List (Note (43, 80, 7429)))
		, Chord (267429 , List (Note (43, 80, 9904)))
		, Chord (277333 , List (Note (43, 80, 4953)))
		))
		))
}
