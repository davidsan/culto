
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

object score {

  // Calcule la duree d'un objet musical
  def duration (obj:ObjectMusical):Int =
obj match {
  case Note(p,d,v) => d
  case Rest(d) => d
  case Sequential (l) => (l.map(duration)).foldLeft(0)(_+_)
  case Parallel (l) => (l.map(duration)).foldLeft(0)(math.max)
}

 // Copy un objet musical
  def copy (obj:ObjectMusical):ObjectMusical =
obj match {
  case Note(p,d,v) => Note(p,d,v)
  case Rest(d) => Rest(d)
  case Sequential (l) => Sequential (l.map(copy))
  case Parallel (l) => Parallel (l.map(copy))
}

// Compte le nombre de notes d'un objet musical
  def note_count (obj:ObjectMusical):Int =
obj match {
  case Note(p,d,v) => 1
  case Parallel (l) => (l.map(note_count)).foldLeft(0)(_+_)
  case Sequential (l) => (l.map(note_count)).foldLeft(0)(_+_)
  case _ => 0
}

// Strech un objet musical par un factor fact
  def stretch (obj:ObjectMusical, fact:Double ):ObjectMusical =
obj match {
  case Note(p,d,v) => Note(p,(d*fact).toInt,v)
  case Rest(d) => Rest((d*fact).toInt)
  case Parallel (l) => Parallel (l.map(stretch (_,fact)))
  case Sequential (l) => Sequential (l.map(stretch (_,fact)))
}


// Transpose obj de n demitons
  def transpose (obj:ObjectMusical, n:Int ):ObjectMusical =
obj match {
  case Note(p,d,v) => Note(p+n,d,v)
  case Rest(d) => Rest(d)
  case Parallel (l) => Parallel (l.map(transpose (_,n)))
  case Sequential (l) => Sequential (l.map(transpose (_,n)))
}

// mirror de obj au tour du center c
  def mirror (obj:ObjectMusical, c:Int ):ObjectMusical =
obj match {
  case Note(p,d,v) => Note(c - (p - c),d,v)
  case Rest(d) => Rest(d)
  case Parallel (l) => Parallel (l.map(mirror (_,c)))
  case Sequential (l) => Sequential (l.map(mirror (_,c)))
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
}
}

//=========================PLAYER
def playObject (obj:ObjectMusical): Unit = {
  val delay = 500
  val cfg = UDP.Config()
  cfg.codec = PacketCodec().doublesAsFloats().booleansAsInts()
  val c = UDP.Client( "127.0.0.1" -> 3341, cfg )
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
}
}

def concatListas (lst:List [List [List [Int]]]):List [List [Int]] ={
  var rep = List.empty[List [Int]]
  lst.foreach(n=>rep = n:::rep)
  rep
}


//////////////////////////////////////////////////

def main(args: Array[String]): Unit = {

//  val  exemple = Parallel (List (
//  Sequential (List (Note (60,1000,100), Note (63,500,100), Note (61,500,100),
//                     Rest (1000), Note(67,1000,100))),
//  Sequential (List (Note (52,2000,100),Note (55,1000,100), Note (55,1000,100)))))
  openEditor(canon_Bach())
  }
}
