package com.example

// Use H2Driver to connect to an H2 database
import slick.driver.H2Driver.api._

import scala.concurrent.ExecutionContext.Implicits.global
import java.io.File
import java.nio.charset.{CodingErrorAction, MalformedInputException}
import java.util.NoSuchElementException

import slick.jdbc.meta.MTable

import scala.collection.immutable.HashMap
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source

/**
  * Created by micas on 09/11/2016.
  */
object Main extends App{
  import com.example.NoteType._
  val songsDir = """C:\UltraSongs\"""
  println("Starting app")
  val db = Database.forConfig("h2mem1")
  val songInfo = SongInfo(title = "LEL", artist = "LUL", mp3 = "GG", bpm = 42.0f, gap = 5500f)
  println(SongsInfo.songsInfo.baseTableRow.create_*.map(_.name))
  //println(SongsInfo.songsInfo.productIterator.toSeq)
  println(songInfo.productIterator.toSeq)
  val setup = DBIO.seq(
    (SongsInfo.songsInfo.schema).create,
    SongsInfo.songsInfo += songInfo
  )
  val setupFuture = db.run(setup)
  val q = for (s <-SongsInfo.songsInfo)
    yield (s.title, s.artist)
  Await.result(setupFuture, Duration.Inf)
  db.run(q.result).foreach(println)
  val songInfoProjection = SongsInfo.songsInfo.baseTableRow.create_*
  val standardFields = songInfoProjection.map(_.name).toList
  val standardFieldsTypes = songInfoProjection.map(_.tpe.structural.getDumpInfo.mainInfo).toList
  val optionalFields = songInfoProjection.map(x => (x.name, x.tpe.structural)).filter(x => x._2.getDumpInfo.mainInfo.contains("Option")).map(_._1).toList
  val requiredFields = standardFields.filterNot(x => optionalFields.contains(x)).toList
  println(standardFieldsTypes)
  println(requiredFields, optionalFields)
  val songParser = new SongParser(songsDir, Some(db))
  songParser.getSongCollection
  val q2 = for (s <-SongsInfo.songsInfo)
    yield (s.title, s.artist)
  db.run(q.result).foreach(println)
  db.run(q2.result).foreach(println)
  db.close()
}

object NoteType extends Enumeration{
  type NoteType = Value
  val NormalNote, GoldenNote, FreestyleNote  = Value
  def fromString(string : String) =
    string match{
      case ":" => NormalNote
      case "*" => GoldenNote
      case "F" => FreestyleNote
      case _ =>
        println("Wrong type of note, replacing with NormalNote")
        NormalNote
    }
}

object LineType extends Enumeration{
  type LineType = Value
  val Note, Blank, Break, End  = Value
}

class SongNote{
  import NoteType._
  var noteStart = 0
  var noteLength = 0
  var noteTone = 0
  var noteSyl = ""
  var noteType = NormalNote
}

class NoteBreak{
  var breakStart = 0
  var breakEnd : Option[Int] = None
}

class SongLine{
  import NoteType._
  var notes : Array[NoteType] = Array()
  var lineBreak = new NoteBreak
}

class SongLyrics {
  var lines : Array[SongLine] = Array()
}

trait Creatable[T <: Creatable[T]] {
  val cs = this.getClass.getConstructors
  def createFromList(params: List[Any]) =
    cs(0).newInstance(params map { _.asInstanceOf[AnyRef] } : _*).asInstanceOf[T]
}
//object SongInfo{
//  val default = SongInfo(title = "", artist = "", mp3 = "", gap=0, bpm=0)
//}

case class SongInfo(id: Option[Long] = None,
                    title: String, artist : String, video: Option[String] = None, mp3 : String, cover : Option[String] = None,
                    year: Option[Int] = None, language : Option[String] = None, path: Option[String] = None,
                    videogap : Option[Float] = None, gap : Float, bpm : Float
                   ) extends Creatable[SongInfo]
//https://github.com/performous/performous/wiki/ultrastar-txt-tags
//https://github.com/ultrastares/usdxworldparty/blob/master/src/base/USong.pas
class SongsInfo(tag: Tag) extends Table[SongInfo](tag, "SONGINFO"){
  def id = column[Long]("id",O.PrimaryKey, O.AutoInc)
  def title = column[String]("TITLE")
  def artist = column[String]("ARTIST")
  def video = column[Option[String]]("VIDEO")
  def mp3 = column[String]("MP3")
  def cover = column[Option[String]]("COVER")
  def year = column[Option[Int]]("YEAR")
  def language = column[Option[String]]("LANGUAGE")
  def path = column[Option[String]]("PATH")
  def videogap = column[Option[Float]]("VIDEOGAP")
  def gap = column[Float]("GAP")
  def bpm = column[Float]("BPM")
  def * = (id.?, title, artist, video, mp3, cover, year, language, path, videogap, gap, bpm) <> (SongInfo.tupled, SongInfo.unapply)
  /*var creator : Option[String] = None
  var edition : Option[String] = None
  var genre : Option[String] = None
  var vocals : Option[String] = None
  var background : Option[String] = None
  var relative : Option[String] = None
  var previewstart : Option[Float] = None
  var duetsingerp1 : Option[String] = None
  var duetsingerp2 : Option[String] = None
  var composer : Option[String] = None
  var medleystartbeat : Option[Float] = None
  var medleyendbeat : Option[Float] = None
  var calcmedley : Option[String] = None
  var start : Option[Float] = None
  var end : Option[Float] = None
  var resolution : Option[Int] = None
  var encoding : Option[String] = None
  var notesgap : Option[Float] = None
  var p1 : Option[String] = None
  var p2 : Option[String] = None*/
  //override def toString = this.getClass.getDeclaredFields.map(x => (x.getName,x.get(this))).mkString(",") //works on 2.12
  //override def toString = this.getClass().getDeclaredFields().mkString("")
  //override def toString = "Artist: %s Title: %s".format(artist,title)
}

object SongsInfo {
  lazy val songsInfo = TableQuery[SongsInfo]
}

/*class SongFullyParsed {
  var songInfo = new SongInfo
  var songLyrics = new SongLyrics
}

class SongCollection {
  var songCollection : Array[SongFullyParsed] = Array()
  def searchSongsByTitle(title : String) =
    for(c <- songCollection if c.songInfo.title.contains(title) ) c
}*/

class SongParser(dirName : String, db : Option[Database] = None){
  var songDir = dirName
  val songInfoProjection = SongsInfo.songsInfo.baseTableRow.create_*
  val standardFields = songInfoProjection.map(_.name).toList
  val standardFieldsTypes = songInfoProjection.map(_.tpe.structural.getDumpInfo.mainInfo).toList
  val optionalFields = songInfoProjection.map(x => (x.name, x.tpe.structural)).filter(x => x._2.getDumpInfo.mainInfo.contains("Option")).map(_._1).toList
  val requiredFields = standardFields.filterNot(x => optionalFields.contains(x)).toList

  def getSongDirectories = {
    val dir = new File(dirName)
    if (dir.exists() && dir.isDirectory){
      //dir.listFiles.(_.isDirectory) //works on 2.12
      dir.listFiles.filter(_.isDirectory)
    }else{
      Array[File]()
    }
  }

  def songTxt(dir : File) =
    dir.listFiles.find(_.getName() == (dir.getName + ".txt"))

  def guessCodec(txt : File) : Option[io.Codec] =
    try{
      Source.fromFile(txt).mkString
      Some(io.Codec.UTF8)
    }catch {
      case _ : MalformedInputException =>
        println("Tried UTF-8, didn't work")
        try{
          Source.fromFile(txt)(io.Codec.ISO8859).mkString
          Some(io.Codec.ISO8859)
        }catch {
          case _ : MalformedInputException =>
            println("Tried ISO8859, didn't work")
            None
        }
    }

  //http://stackoverflow.com/questions/27164267/convert-vector-to-tuple-scala
  def toTuple(seq: Seq[_]): Product = {
    val clz = Class.forName("scala.Tuple" + seq.size)
    clz.getConstructors()(0).newInstance(seq.map(_.asInstanceOf[AnyRef]): _*).asInstanceOf[Product]
  }

//http://stackoverflow.com/questions/1589603/scala-set-a-field-value-reflectively-from-field-name
  implicit def reflector(ref: AnyRef) = new {
    def getV(name: String): Any = ref.getClass.getMethods.find(_.getName == name).get.invoke(ref)
    def setV(name: String, value: Any): Unit = ref.getClass.getMethods.find(_.getName == name + "_$eq").get.invoke(ref, value.asInstanceOf[AnyRef])
  }

  def parseTxt(txt : File, fullyParse : Boolean = false): Unit = {
    val fieldPattern = """#(\w+):(.+)\n?""".r
    val notePattern = """([\:\*F]) (\d+) (\d+) (-?\d+) (.*)\n?""".r
    val breakPattern = """- (\d+)\s?(\d+)?\n?""".r
    val endPattern = """E\n?""".r
    val codec = guessCodec(txt)
    if (codec.isEmpty){
      println("The next song data will probably be incorrect")
    }
    val decoder = codec.getOrElse(io.Codec.UTF8).onMalformedInput(CodingErrorAction.REPLACE)
    var done = false
    //var songInfo = new SongInfo
    var fieldsData = new HashMap[String, String]
    //songInfo.getClass.getDeclaredMethods.foreach(println)
    for (line <- Source.fromFile(txt)(decoder).getLines() if !done){
      line match{
        case fieldPattern(field, value) =>
          val lowerField = field.toLowerCase()
          //try {
            if (standardFields.contains(field)){
              fieldsData = fieldsData + (field -> value)
            } else {
              println("Not standard field", txt.getName, field)
            }
            //songInfo.setV(lowerField, Some(value))
          //}catch {
//            case ex: NoSuchElementException =>
//              println("Not standard field", txt.getName, field)
//          }
          //println(field, value)
        case breakPattern(breakStart, breakEnd) =>
          val songBreak = new NoteBreak
          songBreak.breakStart = breakStart.toInt
          songBreak.breakEnd = if (breakEnd!=null) Some(breakEnd.toInt) else None
          //println("Break", songBreak)
          if (!fullyParse){
            done = true
          }
        case notePattern(noteType,noteStart, noteLength, noteTone, noteSyl) =>
          val songNote = new SongNote
          songNote.noteType = NoteType.fromString(noteType)
          songNote.noteStart = noteStart.toInt
          songNote.noteLength = noteLength.toInt
          songNote.noteTone = noteTone.toInt
          songNote.noteSyl = noteSyl
          //println("Note", songNote)
          if (!fullyParse){
            done = true
          }
        case endPattern() => println("END", line)
        case _ => println("Different thing", txt.getName, line)
      }
    }
    //println(songInfo)
    println(fieldsData)
    //val songInfoData = toTuple(standardFields.map(f => fieldsData.get(f)).toSeq)
    val songInfoData = standardFields.zipWithIndex.map{
      case (f,i) =>
        val requiredType = standardFieldsTypes(i)
        val beforeConversion =
          if(requiredFields.contains(f)){
            fieldsData.getOrElse(f, None)
          }else{
            fieldsData.get(f)
          }
        requiredType match {
          case "Option[Float']" =>
            val casted = beforeConversion.asInstanceOf[Option[String]]
            if (casted.isDefined){
              Some(casted.get.trim.replace(",",".").toFloat)
            }else{
              None
            }
          case "Option[Int']" =>
            val casted = beforeConversion.asInstanceOf[Option[String]]
            if (casted.isDefined){
              Some(casted.get.trim.toInt)
            }else{
              None
            }
          case "Float'" => beforeConversion.toString.replace(",",".").toFloat
          case "Int'" => beforeConversion.toString.toInt
          case _ => beforeConversion
        }
    }

    //val tupleSongInfoData = toTuple(songInfoData)
    //val songInfo = SongInfo.tupled(songInfoData)
    //val songInfo = SongInfo.tupled((None,"Hola","Lola",None,"Thingy",None,None,None,None,None,0,0))
    //val songInfo = SongInfo.tupled(songInfoData)
    //val songInfo = SongInfo.default.createFromList(songInfoData)
    println(songInfoData)
    val songInfoDefault = SongInfo(title = "", artist = "", mp3 = "", gap=0, bpm=0)
    val songInfo = songInfoDefault.createFromList(songInfoData)
    //SongsInfo.songsInfo += songInfoData
    val dbFuture = db.get.run(DBIO.seq(
      SongsInfo.songsInfo += songInfo
    ))
    Await.result(dbFuture, Duration.Inf)

  }

  def getSongCollection = {
    val directories = getSongDirectories
    for (dir <- directories){
      val txt = songTxt(dir)
      if(txt.isDefined){
        parseTxt(txt.get)
      }else{
        println("Directory with inexistent .txt or wrong name", dir)
      }

    }
  }
}