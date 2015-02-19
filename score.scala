import com.typesafe.config._

import math._
import scala.swing._
import scala.swing.event._
import de.sciss.osc._
import Implicits._

import akka.actor.Actor
import akka.actor.Props
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

	//=========================EDITOR


	class ScorePanel (obj:ObjectMusical) extends Panel {
		listenTo(mouse.clicks)
	/*	reactions += {
		case e: MouseClicked => playObject(this.obj)}
		override def paint(g:Graphics2D) {
			drawAt (this.obj, 0, g)
		}
		*/
	}


	def openEditor (obj:ObjectMusical): Unit = {
			val frame=new MainFrame (null) {
				title = "PPC Editor"
						contents=new ScorePanel (obj)
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
		var conf = ConfigFactory.load()
				println("The answer is: " + conf.getString("simple-app.answer"))
				//openEditor(cantateBXV345)
				launchObject(cantateBXV345)
	}

	//////////////////////////////////////////////////

	val cantateBXV345 =
			Poly (List (Voix (0 , List (Chord (0 , List (Note (67, 100, 4000)))
					, Chord (4000 , List (Note (69, 100, 1000)))
					, Chord (5000 , List (Note (69, 100, 500)))
					, Chord (5500 , List (Note (70, 100, 250)))
					, Chord (5750 , List (Note (72, 100, 250)))
					, Chord (6000 , List (Note (70, 100, 500)))
					, Chord (6500 , List (Note (67, 100, 500)))
					, Chord (7000 , List (Note (74, 100, 1500)))
					, Chord (8500 , List (Note (72, 100, 250)))
					, Chord (8750 , List (Note (70, 100, 250)))
					, Chord (9000 , List (Note (69, 100, 500)))
					, Chord (9500 , List (Note (67, 100, 500)))
					, Chord (10000 , List (Note (66, 100, 1000)))
					, Chord (11000 , List (Note (69, 100, 1000)))
					, Chord (12000 , List (Note (70, 100, 1000)))
					, Chord (13000 , List (Note (72, 100, 500)))
					, Chord (13500 , List (Note (74, 100, 250)))
					, Chord (13750 , List (Note (75, 100, 250)))
					, Chord (14000 , List (Note (74, 100, 500)))
					, Chord (14500 , List (Note (70, 100, 500)))
					, Chord (15000 , List (Note (77, 100, 1500)))
					, Chord (16500 , List (Note (76, 100, 250)))
					, Chord (16750 , List (Note (74, 100, 250)))
					, Chord (17000 , List (Note (72, 100, 500)))
					, Chord (17500 , List (Note (70, 100, 500)))
					, Chord (18000 , List (Note (69, 100, 1000)))
					, Chord (19000 , List (Note (74, 100, 1000)))
					, Chord (20000 , List (Note (73, 100, 1000)))
					, Chord (21000 , List (Note (74, 100, 1000)))
					, Chord (22000 , List (Note (76, 100, 500)))
					, Chord (22500 , List (Note (77, 100, 250)))
					, Chord (22750 , List (Note (79, 100, 250)))
					, Chord (23000 , List (Note (77, 100, 500)))
					, Chord (23500 , List (Note (76, 100, 500)))
					, Chord (24000 , List (Note (76, 100, 2667)))
					, Chord (26667 , List (Note (74, 100, 1333)))
					, Chord (28000 , List (Note (74, 100, 4000)))
					, Chord (32000 , List (Note (70, 100, 500)))
					, Chord (32500 , List (Note (69, 100, 500)))
					, Chord (33000 , List (Note (71, 100, 500)))
					, Chord (33500 , List (Note (74, 100, 500)))
					, Chord (34000 , List (Note (79, 100, 500)))
					, Chord (34500 , List (Note (75, 100, 500)))
					, Chord (35000 , List (Note (72, 100, 1000)))
					, Chord (36000 , List (Note (74, 100, 500)))
					, Chord (36500 , List (Note (77, 100, 500)))
					, Chord (37000 , List (Note (75, 100, 500)))
					, Chord (37500 , List (Note (74, 100, 500)))
					, Chord (38000 , List (Note (75, 100, 1000)))
					, Chord (39000 , List (Note (69, 100, 1000)))
					, Chord (40000 , List (Note (74, 100, 1000)))
					, Chord (41000 , List (Note (67, 100, 1000)))
					, Chord (42000 , List (Note (72, 100, 500)))
					, Chord (42500 , List (Note (74, 100, 250)))
					, Chord (42750 , List (Note (75, 100, 250)))
					, Chord (43000 , List (Note (74, 100, 500)))
					, Chord (43500 , List (Note (72, 100, 500)))
					, Chord (44000 , List (Note (70, 100, 1333)))
					, Chord (45333 , List (Note (69, 100, 667)))
					, Chord (46000 , List (Note (67, 100, 667)))
					, Chord (46667 , List (Note (67, 100, 1333)))
					))
					, Voix (1 , List (Chord (0 , List (Note (62, 100, 4000)))
							, Chord (4000 , List (Note (63, 100, 1000)))
							, Chord (5000 , List (Note (62, 100, 1000)))
							, Chord (6000 , List (Note (62, 100, 1500)))
							, Chord (7500 , List (Note (62, 100, 500)))
							, Chord (8000 , List (Note (67, 100, 1000)))
							, Chord (9000 , List (Note (66, 100, 500)))
							, Chord (9500 , List (Note (67, 100, 500)))
							, Chord (10000 , List (Note (62, 100, 1000)))
							, Chord (11000 , List (Note (66, 100, 1000)))
							, Chord (12000 , List (Note (67, 100, 1000)))
							, Chord (13000 , List (Note (65, 100, 1000)))
							, Chord (14000 , List (Note (65, 100, 1500)))
							, Chord (15500 , List (Note (65, 100, 500)))
							, Chord (16000 , List (Note (70, 100, 1000)))
							, Chord (17000 , List (Note (72, 100, 500)))
							, Chord (17500 , List (Note (64, 100, 500)))
							, Chord (18000 , List (Note (65, 100, 1000)))
							, Chord (19000 , List (Note (65, 100, 1000)))
							, Chord (20000 , List (Note (67, 100, 1000)))
							, Chord (21000 , List (Note (65, 100, 1000)))
							, Chord (22000 , List (Note (64, 100, 500)))
							, Chord (22500 , List (Note (64, 100, 500)))
							, Chord (23000 , List (Note (62, 100, 500)))
							, Chord (23500 , List (Note (62, 100, 500)))
							, Chord (24000 , List (Note (64, 100, 667)))
							, Chord (24667 , List (Note (65, 100, 667)))
							, Chord (25333 , List (Note (67, 100, 1333)))
							, Chord (26667 , List (Note (66, 100, 1333)))
							, Chord (28000 , List (Note (69, 100, 4000)))
							, Chord (32000 , List (Note (67, 100, 1000)))
							, Chord (33000 , List (Note (67, 100, 1000)))
							, Chord (34000 , List (Note (67, 100, 1500)))
							, Chord (35500 , List (Note (63, 100, 500)))
							, Chord (36000 , List (Note (68, 100, 1000)))
							, Chord (37000 , List (Note (67, 100, 1000)))
							, Chord (38000 , List (Note (67, 100, 1000)))
							, Chord (39000 , List (Note (66, 100, 1000)))
							, Chord (40000 , List (Note (67, 100, 500)))
							, Chord (40500 , List (Note (65, 100, 500)))
							, Chord (41000 , List (Note (64, 100, 1000)))
							, Chord (42000 , List (Note (66, 100, 500)))
							, Chord (42500 , List (Note (67, 100, 500)))
							, Chord (43000 , List (Note (69, 100, 1000)))
							, Chord (44000 , List (Note (69, 100, 667)))
							, Chord (44667 , List (Note (67, 100, 667)))
							, Chord (45333 , List (Note (66, 100, 1333)))
							, Chord (46667 , List (Note (62, 100, 1333)))
							))
							, Voix (2 , List (Chord (0 , List (Note (58, 100, 4000)))
									, Chord (4000 , List (Note (60, 100, 500)))
									, Chord (4500 , List (Note (58, 100, 500)))
									, Chord (5000 , List (Note (57, 100, 1000)))
									, Chord (6000 , List (Note (55, 100, 1500)))
									, Chord (7500 , List (Note (57, 100, 500)))
									, Chord (8000 , List (Note (58, 100, 1000)))
									, Chord (9000 , List (Note (60, 100, 500)))
									, Chord (9500 , List (Note (58, 100, 500)))
									, Chord (10000 , List (Note (57, 100, 1000)))
									, Chord (11000 , List (Note (62, 100, 1000)))
									, Chord (12000 , List (Note (62, 100, 1000)))
									, Chord (13000 , List (Note (60, 100, 1000)))
									, Chord (14000 , List (Note (58, 100, 1500)))
									, Chord (15500 , List (Note (60, 100, 500)))
									, Chord (16000 , List (Note (62, 100, 1000)))
									, Chord (17000 , List (Note (55, 100, 500)))
									, Chord (17500 , List (Note (60, 100, 500)))
									, Chord (18000 , List (Note (60, 100, 1000)))
									, Chord (19000 , List (Note (58, 100, 1000)))
									, Chord (20000 , List (Note (58, 100, 1000)))
									, Chord (21000 , List (Note (57, 100, 1000)))
									, Chord (22000 , List (Note (57, 100, 500)))
									, Chord (22500 , List (Note (57, 100, 500)))
									, Chord (23000 , List (Note (57, 100, 500)))
									, Chord (23500 , List (Note (62, 100, 500)))
									, Chord (24000 , List (Note (62, 100, 667)))
									, Chord (24667 , List (Note (61, 100, 333)))
									, Chord (25000 , List (Note (59, 100, 333)))
									, Chord (25333 , List (Note (61, 100, 1333)))
									, Chord (26667 , List (Note (57, 100, 1333)))
									, Chord (28000 , List (Note (62, 100, 4000)))
									, Chord (32000 , List (Note (62, 100, 1000)))
									, Chord (33000 , List (Note (62, 100, 1000)))
									, Chord (34000 , List (Note (60, 100, 1500)))
									, Chord (35500 , List (Note (60, 100, 500)))
									, Chord (36000 , List (Note (60, 100, 1000)))
									, Chord (37000 , List (Note (59, 100, 1000)))
									, Chord (38000 , List (Note (60, 100, 1000)))
									, Chord (39000 , List (Note (62, 100, 1000)))
									, Chord (40000 , List (Note (62, 100, 1000)))
									, Chord (41000 , List (Note (60, 100, 1000)))
									, Chord (42000 , List (Note (60, 100, 500)))
									, Chord (42500 , List (Note (58, 100, 500)))
									, Chord (43000 , List (Note (57, 100, 500)))
									, Chord (43500 , List (Note (62, 100, 500)))
									, Chord (44000 , List (Note (62, 100, 1333)))
									, Chord (45333 , List (Note (60, 100, 667)))
									, Chord (46000 , List (Note (58, 100, 667)))
									, Chord (46667 , List (Note (58, 100, 1333)))
									))
									, Voix (3 , List (Chord (0 , List (Note (55, 100, 4000)))
											, Chord (4000 , List (Note (55, 100, 1000)))
											, Chord (5000 , List (Note (54, 100, 1000)))
											, Chord (6000 , List (Note (55, 100, 1500)))
											, Chord (7500 , List (Note (53, 100, 500)))
											, Chord (8000 , List (Note (51, 100, 500)))
											, Chord (8500 , List (Note (50, 100, 500)))
											, Chord (9000 , List (Note (51, 100, 500)))
											, Chord (9500 , List (Note (48, 100, 500)))
											, Chord (10000 , List (Note (50, 100, 1000)))
											, Chord (11000 , List (Note (50, 100, 1000)))
											, Chord (12000 , List (Note (55, 100, 1000)))
											, Chord (13000 , List (Note (57, 100, 1000)))
											, Chord (14000 , List (Note (58, 100, 1500)))
											, Chord (15500 , List (Note (57, 100, 500)))
											, Chord (16000 , List (Note (55, 100, 500)))
											, Chord (16500 , List (Note (53, 100, 500)))
											, Chord (17000 , List (Note (52, 100, 500)))
											, Chord (17500 , List (Note (48, 100, 500)))
											, Chord (18000 , List (Note (53, 100, 1000)))
											, Chord (19000 , List (Note (58, 100, 1000)))
											, Chord (20000 , List (Note (52, 100, 1000)))
											, Chord (21000 , List (Note (53, 100, 1000)))
											, Chord (22000 , List (Note (49, 100, 500)))
											, Chord (22500 , List (Note (49, 100, 500)))
											, Chord (23000 , List (Note (50, 100, 500)))
											, Chord (23500 , List (Note (50, 100, 500)))
											, Chord (24000 , List (Note (57, 100, 1333)))
											, Chord (25333 , List (Note (45, 100, 1333)))
											, Chord (26667 , List (Note (50, 100, 1333)))
											, Chord (28000 , List (Note (54, 100, 4000)))
											, Chord (32000 , List (Note (55, 100, 1000)))
											, Chord (33000 , List (Note (53, 100, 1000)))
											, Chord (34000 , List (Note (51, 100, 1500)))
											, Chord (35500 , List (Note (56, 100, 500)))
											, Chord (36000 , List (Note (53, 100, 500)))
											, Chord (36500 , List (Note (50, 100, 500)))
											, Chord (37000 , List (Note (55, 100, 1000)))
											, Chord (38000 , List (Note (48, 100, 1000)))
											, Chord (39000 , List (Note (60, 100, 1000)))
											, Chord (40000 , List (Note (58, 100, 500)))
											, Chord (40500 , List (Note (57, 100, 500)))
											, Chord (41000 , List (Note (58, 100, 500)))
											, Chord (41500 , List (Note (60, 100, 500)))
											, Chord (42000 , List (Note (57, 100, 500)))
											, Chord (42500 , List (Note (55, 100, 500)))
											, Chord (43000 , List (Note (54, 100, 1000)))
											, Chord (44000 , List (Note (55, 100, 1333)))
											, Chord (45333 , List (Note (50, 100, 1333)))
											, Chord (46667 , List (Note (43, 100, 1333)))
											))
					))
}
