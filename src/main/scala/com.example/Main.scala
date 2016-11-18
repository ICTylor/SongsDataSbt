package com.example

// Use H2Driver to connect to an H2 database
import java.io.File
import java.nio.charset.{CodingErrorAction, MalformedInputException}

import slick.driver.H2Driver.api._

import scala.collection.immutable.HashMap
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.io.Source

/**
  * Created by Damián Perdiz on 09/11/2016.
  */
object Main extends App{
  if (args.length != 1){
    println("Songs directory required as argument")
  }else{
    val songsDir = args(0)
    println("Starting app")
    val db = Database.forConfig("h2mem1")
    val setup = DBIO.seq(
      SongsInfo.songsInfo.schema.create
    )
    val setupFuture = db.run(setup)
    Await.result(setupFuture, Duration.Inf)
    val q = for (s <-SongsInfo.songsInfo)
      yield (s.title, s.artist)
    val songInfoQuery = TableQuery[SongsInfo]
    val qByBpm = songInfoQuery.sortBy(_.bpm).map(s => (s.title, s.bpm))
    val songParser = new SongParser(songsDir, Some(db))
    songParser.populateSongDatabase()
    db.run(q.result).foreach(println)
    db.run(qByBpm.result).foreach(println)
    db.close()
  }
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

class SongNote(noteStart : Int, noteLength : Int, noteTone : Int, noteSyl : String, noteType : NoteType.NoteType = NoteType.NormalNote){
}

class NoteBreak(breakStart : Int, breakEnd : Option[Int]){
}

class SongLine(var notes : List[SongNote],var lineBreak : NoteBreak = new NoteBreak(0,None)){
}

class SongLyrics {
  var lines : List[SongLine] = List()
}

trait Creatable[T <: Creatable[T]] {
  val cs = this.getClass.getConstructors
  def createFromList(params: List[Any]) =
    cs(0).newInstance(params map { _.asInstanceOf[AnyRef] } : _*).asInstanceOf[T]
}

case class SongInfo(id: Option[Long] = None,
                    title: String, artist : String, video: Option[String] = None, mp3 : String, cover : Option[String] = None,
                    year: Option[Int] = None, language : Option[String] = None,
                    videogap : Option[Float] = None, gap : Float, bpm : Float, creator: Option[String] = None,
                    edition: Option[String] = None, genre: Option[String] = None, medleystartbeat : Option[Float] = None,
                    medleyendbeat : Option[Float] = None, start : Option[Float] = None, end : Option[Float] = None,
                    resolution : Option[Int] = None, relative : Option[String] = None, background : Option[String] = None,
                    notesgap : Option[Float] = None
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
  def videogap = column[Option[Float]]("VIDEOGAP")
  def gap = column[Float]("GAP")
  def bpm = column[Float]("BPM")
  def creator = column[Option[String]]("CREATOR")
  def edition = column[Option[String]]("EDITION")
  def genre = column[Option[String]]("GENRE")
  def medleystartbeat = column[Option[Float]]("MEDLEYSTARTBEAT")
  def medleyendbeat = column[Option[Float]]("MEDLEYENDBEAT")
  def start = column[Option[Float]]("START")
  def end = column[Option[Float]]("END")
  def resolution = column[Option[Int]]("RESOLUTION")
  def relative = column[Option[String]]("RELATIVE")
  def background = column[Option[String]]("BACKGROUND")
  def notesgap = column[Option[Float]]("NOTESGAP")
  //def vocals = column[Option[String]]("VOCALS") //Can't add more fields like this, tuple only allows 22 fields
  def * = (id.?, title, artist, video, mp3, cover, year, language, videogap, gap, bpm,
           creator, edition, genre, medleystartbeat, medleyendbeat,
           start, end, resolution, relative, background, notesgap) <> (SongInfo.tupled, SongInfo.unapply)
  /*
  var previewstart : Option[Float] = None
  var duetsingerp1 : Option[String] = None
  var duetsingerp2 : Option[String] = None
  var composer : Option[String] = None
  var calcmedley : Option[String] = None
  var encoding : Option[String] = None
  var p1 : Option[String] = None
  var p2 : Option[String] = None*/
}

object SongsInfo {
  lazy val songsInfo = TableQuery[SongsInfo]
}


class SongParser(dirName : String, db : Option[Database] = None){
  var songDir = dirName
  val songInfoProjection = SongsInfo.songsInfo.baseTableRow.create_*
  val standardFields = songInfoProjection.map(_.name).toList
  val standardFieldsTypes = songInfoProjection.map(_.tpe.structural.getDumpInfo.mainInfo).toList
  val optionalFields = songInfoProjection.map(x => (x.name, x.tpe.structural)).filter(x => x._2.getDumpInfo.mainInfo.contains("Option")).map(_._1).toList
  val requiredFields = standardFields.filterNot(x => optionalFields.contains(x))

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
    dir.listFiles.find(_.getName == (dir.getName + ".txt"))

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

  def parseTxt(txt : File, fullyParse : Boolean = false): (SongInfo, Option[SongLyrics]) = {
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
    var fieldsData = new HashMap[String, String]
    val songLyrics = new SongLyrics
    var songLine = new SongLine(List())

    for (line <- Source.fromFile(txt)(decoder).getLines if !done){
      line match{
        case fieldPattern(field, value) =>
            if (standardFields.contains(field)){
              fieldsData = fieldsData + (field -> value)
            } else {
              println("Not standard field", txt.getName, field)
            }
        case breakPattern(breakStart, breakEnd) =>
          val songBreak = new NoteBreak(breakStart.toInt, if (breakEnd!=null) Some(breakEnd.toInt) else None)
          if (!fullyParse){
            done = true
          }
          songLine.lineBreak = songBreak
          songLyrics.lines = songLyrics.lines ::: List(songLine)
          songLine = new SongLine(List())
        case notePattern(noteType,noteStart, noteLength, noteTone, noteSyl) =>
          val songNote = new SongNote(noteStart = noteStart.toInt, noteLength = noteLength.toInt,
                                      noteTone = noteTone.toInt, noteSyl = noteSyl, noteType = NoteType.fromString(noteType))
          if (!fullyParse){
            done = true
          }
          songLine.notes = songLine.notes ::: List(songNote)
        case endPattern() => println("END", line)
        case _ => println("Different thing", txt.getName, line)
      }
    }
    println(fieldsData)
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
    val songInfoDefault = SongInfo(title = "", artist = "", mp3 = "", gap=0, bpm=0)
    val songInfo = songInfoDefault.createFromList(songInfoData)
    (songInfo, if (fullyParse) Some(songLyrics) else None)
  }

  def submitToDB(songInfo: SongInfo) = {
    db.get.run(DBIO.seq(
      SongsInfo.songsInfo += songInfo
    ))
  }

  def populateSongDatabase() = {
    val directories = getSongDirectories
    for (dir <- directories){
      val txt = songTxt(dir)
      if(txt.isDefined){
        val (songInfo, songLyrics) = parseTxt(txt.get)
        submitToDB(songInfo)
      }else{
        println("Directory with inexistent .txt or wrong name", dir)
      }
    }
  }
}