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

///////////////////////////// BACH
 val voix1 = Sequential ( List (
  Note (60 , 750 , 106 ) , Note (62 , 250 , 108 ) , Note (63 , 250 , 108 ) ,
  Note (64 , 250 , 109 ) , Note (65 , 250 , 109 ) , Note (66 , 250 , 110 ) ,
  Note (67 , 1000 , 110 ) , Note (68 , 625 , 113 ) ,Note (65 , 125 , 114 ) ,
  Note (61 , 125 , 112 ) , Note (60 , 125 , 112 ) , Note (59 , 500 , 112 ) ,
  Rest (500) , Rest (500) , Note (67 , 1000 , 109 ) ,Note (66 , 1000 , 108 ) ,
  Note (65 , 1000 , 106 ) , Note (64 , 1000 , 106 ) , Note (63 , 1000 , 106 ) ,
  Note (62 , 750 , 106 ) , Note (61 , 250 , 106 ) , Note (58 , 250 , 106 ) ,
  Note (57 , 250 , 106 ) , Note (62 , 500 , 106 ) ,Rest (1000) , Rest (1000) ,
Note (67 , 1000 , 106 ) , Note (65 , 500 , 106 ) , Note (64 , 1000 , 106 )))

val voix2 = Sequential (List (
Rest (125) , Note (48 , 125 , 100 ), Note (51 , 125 , 100 ),
Note (55 , 125 , 100 ),Note (60 , 1000 , 100 ),Note (58 , 250 , 100 ),
Note (57 , 250 , 100 ),Note (58 , 625 , 100 ),Note (52 , 125 , 100 ),
Note (50 , 125 , 100 ),Note (52 , 125 , 100 ),Note (53 , 125 , 100 ),
Note (48 , 125 , 100 ),Note (53 , 125 , 100 ),Note (55 , 125 , 100 ),
Note (56 , 750 , 100 ),Note (56 , 250 , 100 ),Note (55 , 250 , 100 ),
Note (53 , 250 , 100 ),Note (51 , 625 , 100 ),Note (51 , 125 , 100 ),
Note (53 , 125 , 100 ),Note (51 , 125 , 100 ),Note (50 , 250 , 100 ),
Note (48 , 250 , 100 ),Note (49 , 500 , 100 ), Rest (250) ,Note (50 , 250 , 100 ),
Note (51 , 250 , 100 ),Note (50 , 250 , 100 ),Note (48 , 125 , 100 ),
Note (47 , 125 , 100 ),Note (48 , 125 , 100 ),Note (47 , 125 , 100 ),
Note (48 , 125 , 100 ),Note (50 , 125 , 100 ),Note (48 , 125 , 100 ),
Note (46 , 125 , 100 ),Note (45 , 125 , 100 ),Note (43 , 125 , 100 ),
Note (45 , 125 , 100 ),Note (46 , 125 , 100 ),Note (48 , 125 , 100 ),
Note (45 , 125 , 100 ),Note (46 , 125 , 100 ),Note (48 , 125 , 100 ),
Note (50 , 250 , 100 ),Note (60 , 500 , 100 ),Note (58 , 125 , 100 ),
Note (57 , 125 , 100 ),Note (58 , 250 , 100 ),Note (55 , 250 , 100 ),
Note (52 , 250 , 100 ),Note (57 , 125 , 100 ),Note (55 , 125 , 100 ),
Note (54 , 250 , 100 ),Note (55 , 125 , 100 ),Note (57 , 125 , 100 ),
Note (58 , 250 , 100 ), Note (49 , 250 , 100 ),Note (50 , 500 , 100 ),
Rest (500) , Rest (250) , Note (50 , 375 , 100 ),Note (53 , 125 , 100 ),
Note (52 , 125 , 100 ),
Note (50 , 125 , 100 ),Note (49 , 125 , 100 ),Note (50 , 125 , 100 ),
Note (52 , 125 , 100 ),Note (53 , 125 , 100 ),Note (55 , 125 , 100 ),
Note (58 , 125 , 100 ),Note (57 , 125 , 100 ),Note (55 , 125 , 100 )))


def concat_par_tones (obj:ObjectMusical, tone:Int, n:Int):ObjectMusical = {
    var rep:List[ObjectMusical] = List (obj)
    for (i <- 0 to n-1)
      rep = (transpose(obj, i*tone))::rep
    Sequential (rep.reverse)
  }

def canon_Bach ():ObjectMusical = {
    var new_voix = concat (Rest(2000),
                           concat_par_tones (transpose (voix2, 7), 2, 5))
    Parallel (List (
    concat_par_tones (voix1, 2, 5), concat_par_tones (voix2, 2, 5),new_voix))
  }

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
def playObject (obj:ObjectMusical): Unit = {
  val delay = 500
  val cfg = UDP.Config()
  cfg.codec = PacketCodec().doublesAsFloats().booleansAsInts()
  val c = UDP.Client( localhost -> 3341, cfg )
  c.connect()
  val system = akka.actor.ActorSystem("system")

  val notes = collectNotes (obj, 0)
  notes.foreach(n=>system.scheduler.scheduleOnce ((n(0)) milliseconds)
  (noteOn (n(1), n(2), n(3), c)))
  system.scheduler.scheduleOnce ((duration (obj)) milliseconds) (stopObjet (c))

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

val cantateBWV318 =
Poly (List (Voix (0 , List (Chord (0 , List (Note (67, 100, 1000)))
, Chord (1000 , List (Note (67, 100, 500)))
, Chord (1500 , List (Note (69, 100, 500)))
, Chord (2000 , List (Note (71, 100, 1000)))
, Chord (3000 , List (Note (72, 100, 1000)))
, Chord (4000 , List (Note (74, 100, 1000)))
, Chord (5000 , List (Note (76, 100, 1000)))
, Chord (6000 , List (Note (74, 100, 2000)))
, Chord (8000 , List (Note (76, 100, 1000)))
, Chord (9000 , List (Note (78, 100, 1000)))
, Chord (10000 , List (Note (79, 100, 1000)))
, Chord (11000 , List (Note (74, 100, 1000)))
, Chord (12000 , List (Note (71, 100, 1000)))
, Chord (13000 , List (Note (73, 100, 1000)))
, Chord (14000 , List (Note (74, 100, 2000)))
, Chord (16000 , List (Note (72, 100, 1000)))
, Chord (17000 , List (Note (69, 100, 1000)))
, Chord (18000 , List (Note (71, 100, 1000)))
, Chord (19000 , List (Note (72, 100, 1000)))
, Chord (20000 , List (Note (71, 100, 1000)))
, Chord (21000 , List (Note (69, 100, 1000)))
, Chord (22000 , List (Note (67, 100, 2000)))
, Chord (24000 , List (Note (66, 100, 1000)))
, Chord (25000 , List (Note (67, 100, 1000)))
, Chord (26000 , List (Note (69, 100, 1000)))
, Chord (27000 , List (Note (71, 100, 1000)))
, Chord (28000 , List (Note (69, 100, 1500)))
, Chord (29500 , List (Note (67, 100, 500)))
, Chord (30000 , List (Note (66, 100, 1000)))
, Chord (31000 , List (Note (64, 100, 1000)))
, Chord (32000 , List (Note (62, 100, 4000)))
, Chord (36000 , List (Note (67, 100, 1000)))
, Chord (37000 , List (Note (67, 100, 500)))
, Chord (37500 , List (Note (69, 100, 500)))
, Chord (38000 , List (Note (71, 100, 1000)))
, Chord (39000 , List (Note (72, 100, 1000)))
, Chord (40000 , List (Note (74, 100, 1000)))
, Chord (41000 , List (Note (76, 100, 1000)))
, Chord (42000 , List (Note (74, 100, 2000)))
, Chord (44000 , List (Note (67, 100, 1000)))
, Chord (45000 , List (Note (69, 100, 1000)))
, Chord (46000 , List (Note (71, 100, 1000)))
, Chord (47000 , List (Note (72, 100, 1000)))
, Chord (48000 , List (Note (71, 100, 1000)))
, Chord (49000 , List (Note (69, 100, 1000)))
, Chord (50000 , List (Note (67, 100, 2000)))
))
, Voix (1 , List (Chord (0 , List (Note (62, 100, 1000)))
, Chord (1000 , List (Note (64, 100, 500)))
, Chord (1500 , List (Note (66, 100, 500)))
, Chord (2000 , List (Note (67, 100, 1000)))
, Chord (3000 , List (Note (66, 100, 500)))
, Chord (3500 , List (Note (64, 100, 500)))
, Chord (4000 , List (Note (62, 100, 500)))
, Chord (4500 , List (Note (66, 100, 500)))
, Chord (5000 , List (Note (71, 100, 500)))
, Chord (5500 , List (Note (69, 100, 500)))
, Chord (6000 , List (Note (66, 100, 2000)))
, Chord (8000 , List (Note (69, 100, 1000)))
, Chord (9000 , List (Note (69, 100, 1000)))
, Chord (10000 , List (Note (67, 100, 1000)))
, Chord (11000 , List (Note (69, 100, 1000)))
, Chord (12000 , List (Note (67, 100, 500)))
, Chord (12500 , List (Note (66, 100, 500)))
, Chord (13000 , List (Note (64, 100, 1000)))
, Chord (14000 , List (Note (66, 100, 2000)))
, Chord (16000 , List (Note (64, 100, 1000)))
, Chord (17000 , List (Note (62, 100, 1000)))
, Chord (18000 , List (Note (62, 100, 1000)))
, Chord (19000 , List (Note (60, 100, 1000)))
, Chord (20000 , List (Note (62, 100, 1500)))
, Chord (21500 , List (Note (60, 100, 500)))
, Chord (22000 , List (Note (59, 100, 2000)))
, Chord (24000 , List (Note (62, 100, 1000)))
, Chord (25000 , List (Note (61, 100, 500)))
, Chord (25500 , List (Note (59, 100, 500)))
, Chord (26000 , List (Note (57, 100, 1000)))
, Chord (27000 , List (Note (62, 100, 1000)))
, Chord (28000 , List (Note (64, 100, 2500)))
, Chord (30500 , List (Note (62, 100, 1000)))
, Chord (31500 , List (Note (61, 100, 500)))
, Chord (32000 , List (Note (57, 100, 4000)))
, Chord (36000 , List (Note (62, 100, 1000)))
, Chord (37000 , List (Note (64, 100, 500)))
, Chord (37500 , List (Note (66, 100, 500)))
, Chord (38000 , List (Note (67, 100, 1000)))
, Chord (39000 , List (Note (67, 100, 1000)))
, Chord (40000 , List (Note (65, 100, 1000)))
, Chord (41000 , List (Note (64, 100, 500)))
, Chord (41500 , List (Note (66, 100, 500)))
, Chord (42000 , List (Note (67, 100, 2000)))
, Chord (44000 , List (Note (67, 100, 500)))
, Chord (44500 , List (Note (64, 100, 500)))
, Chord (45000 , List (Note (62, 100, 500)))
, Chord (45500 , List (Note (60, 100, 500)))
, Chord (46000 , List (Note (59, 100, 500)))
, Chord (46500 , List (Note (57, 100, 500)))
, Chord (47000 , List (Note (55, 100, 500)))
, Chord (47500 , List (Note (66, 100, 500)))
, Chord (48000 , List (Note (67, 100, 1500)))
, Chord (49500 , List (Note (66, 100, 500)))
, Chord (50000 , List (Note (62, 100, 2000)))
))
, Voix (2 , List (Chord (0 , List (Note (59, 100, 1000)))
, Chord (1000 , List (Note (60, 100, 1000)))
, Chord (2000 , List (Note (62, 100, 1000)))
, Chord (3000 , List (Note (60, 100, 1000)))
, Chord (4000 , List (Note (59, 100, 250)))
, Chord (4250 , List (Note (60, 100, 250)))
, Chord (4500 , List (Note (62, 100, 1000)))
, Chord (5500 , List (Note (61, 100, 500)))
, Chord (6000 , List (Note (62, 100, 2000)))
, Chord (8000 , List (Note (60, 100, 500)))
, Chord (8500 , List (Note (59, 100, 500)))
, Chord (9000 , List (Note (60, 100, 500)))
, Chord (9500 , List (Note (62, 100, 500)))
, Chord (10000 , List (Note (59, 100, 1000)))
, Chord (11000 , List (Note (57, 100, 1000)))
, Chord (12000 , List (Note (62, 100, 1000)))
, Chord (13000 , List (Note (57, 100, 1000)))
, Chord (14000 , List (Note (57, 100, 2000)))
, Chord (16000 , List (Note (55, 100, 1000)))
, Chord (17000 , List (Note (57, 100, 1000)))
, Chord (18000 , List (Note (55, 100, 500)))
, Chord (18500 , List (Note (54, 100, 500)))
, Chord (19000 , List (Note (55, 100, 1000)))
, Chord (20000 , List (Note (55, 100, 1000)))
, Chord (21000 , List (Note (54, 100, 1000)))
, Chord (22000 , List (Note (55, 100, 2000)))
, Chord (24000 , List (Note (57, 100, 1000)))
, Chord (25000 , List (Note (55, 100, 1000)))
, Chord (26000 , List (Note (62, 100, 500)))
, Chord (26500 , List (Note (61, 100, 500)))
, Chord (27000 , List (Note (59, 100, 1000)))
, Chord (28000 , List (Note (52, 100, 1000)))
, Chord (29000 , List (Note (57, 100, 2000)))
, Chord (31000 , List (Note (55, 100, 1000)))
, Chord (32000 , List (Note (54, 100, 4000)))
, Chord (36000 , List (Note (55, 100, 1000)))
, Chord (37000 , List (Note (60, 100, 1000)))
, Chord (38000 , List (Note (62, 100, 1000)))
, Chord (39000 , List (Note (60, 100, 1000)))
, Chord (40000 , List (Note (60, 100, 500)))
, Chord (40500 , List (Note (59, 100, 500)))
, Chord (41000 , List (Note (60, 100, 1000)))
, Chord (42000 , List (Note (59, 100, 2000)))
, Chord (44000 , List (Note (59, 100, 1000)))
, Chord (45000 , List (Note (57, 100, 1000)))
, Chord (46000 , List (Note (62, 100, 1000)))
, Chord (47000 , List (Note (64, 100, 500)))
, Chord (47500 , List (Note (57, 100, 500)))
, Chord (48000 , List (Note (59, 100, 500)))
, Chord (48500 , List (Note (60, 100, 500)))
, Chord (49000 , List (Note (62, 100, 1000)))
, Chord (50000 , List (Note (59, 100, 2000)))
))
, Voix (3 , List (Chord (0 , List (Note (55, 100, 1000)))
, Chord (1000 , List (Note (48, 100, 1000)))
, Chord (2000 , List (Note (43, 100, 500)))
, Chord (2500 , List (Note (55, 100, 500)))
, Chord (3000 , List (Note (57, 100, 1000)))
, Chord (4000 , List (Note (59, 100, 500)))
, Chord (4500 , List (Note (57, 100, 500)))
, Chord (5000 , List (Note (55, 100, 500)))
, Chord (5500 , List (Note (57, 100, 500)))
, Chord (6000 , List (Note (50, 100, 2000)))
, Chord (8000 , List (Note (57, 100, 1000)))
, Chord (9000 , List (Note (50, 100, 1000)))
, Chord (10000 , List (Note (52, 100, 1000)))
, Chord (11000 , List (Note (54, 100, 1000)))
, Chord (12000 , List (Note (55, 100, 1000)))
, Chord (13000 , List (Note (57, 100, 1000)))
, Chord (14000 , List (Note (50, 100, 2000)))
, Chord (16000 , List (Note (52, 100, 1000)))
, Chord (17000 , List (Note (54, 100, 1000)))
, Chord (18000 , List (Note (55, 100, 1000)))
, Chord (19000 , List (Note (52, 100, 1000)))
, Chord (20000 , List (Note (50, 100, 500)))
, Chord (20500 , List (Note (48, 100, 500)))
, Chord (21000 , List (Note (50, 100, 1000)))
, Chord (22000 , List (Note (43, 100, 2000)))
, Chord (24000 , List (Note (50, 100, 1000)))
, Chord (25000 , List (Note (52, 100, 1000)))
, Chord (26000 , List (Note (54, 100, 1000)))
, Chord (27000 , List (Note (55, 100, 1000)))
, Chord (28000 , List (Note (49, 100, 1000)))
, Chord (29000 , List (Note (45, 100, 1000)))
, Chord (30000 , List (Note (50, 100, 1000)))
, Chord (31000 , List (Note (45, 100, 1000)))
, Chord (32000 , List (Note (50, 100, 4000)))
, Chord (36000 , List (Note (47, 100, 1000)))
, Chord (37000 , List (Note (48, 100, 1000)))
, Chord (38000 , List (Note (55, 100, 1000)))
, Chord (39000 , List (Note (52, 100, 1000)))
, Chord (40000 , List (Note (50, 100, 1000)))
, Chord (41000 , List (Note (48, 100, 1000)))
, Chord (42000 , List (Note (55, 100, 2000)))
, Chord (44000 , List (Note (52, 100, 1000)))
, Chord (45000 , List (Note (54, 100, 1000)))
, Chord (46000 , List (Note (55, 100, 1000)))
, Chord (47000 , List (Note (52, 100, 1000)))
, Chord (48000 , List (Note (50, 100, 2000)))
, Chord (50000 , List (Note (55, 100, 2000)))
))
))


//////////////////////////////////////////////////

def main(args: Array[String]): Unit = {

//  val  exemple = Parallel (List (
//  Sequential (List (Note (60,1000,100), Note (63,500,100), Note (61,500,100),
//                     Rest (1000), Note(67,1000,100))),
//  Sequential (List (Note (52,2000,100),Note (55,1000,100), Note (55,1000,100)))))

//  val chordTest = Chord (1000 , List (Note (55, 10000, 2000)))

    val conf = ConfigFactory.load()
    println("The answer is: " + conf.getString("simple-app.answer"))

    openEditor(cantateBWV318)

    }
}
