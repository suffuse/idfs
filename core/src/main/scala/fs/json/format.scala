package sfs
package fs
package json

import api._, attributes._
import play.api.libs.json
import play.api.libs.json._
import play.api.libs.json.Json._
import scala.reflect.ClassTag

object format {

  implicit val metadata: Format[Metadata] = new Format[Metadata] {

    def reads(json: JsValue): JsResult[Metadata] =
      for {
        size       <- json.optionally[Size]
        atime      <- json.optionally[Atime]
        mtime      <- json.optionally[Mtime]
        ctime      <- json.optionally[Ctime]
        birth      <- json.optionally[Birth]
        uid        <- json.optionally[Uid]
        gid        <- json.optionally[Gid]
        blockCount <- json.optionally[BlockCount]
        nlink      <- json.optionally[Nlink]
        perms      <- json.optionally[UnixPerms]
        node       <- json.optionally[Node]
      } yield {
        import scala.language.implicitConversions
        implicit def asAttribute[A](a: Option[A])(implicit z: A => Attribute): Option[Attribute] = a map z

        val attributes = Seq[Option[Attribute]](
          size, atime, mtime, ctime, birth, uid, gid,
          blockCount, nlink, perms, node
        ).flatten

        Metadata(attributes: _*)
      }

    def writes(o: Metadata): JsValue =
      (o foldLeft obj()) {
        Function untupled {
          add[Size]       orElse
          add[Atime]      orElse
          add[Mtime]      orElse
          add[Ctime]      orElse
          add[Birth]      orElse
          add[Uid]        orElse
          add[Gid]        orElse
          add[BlockCount] orElse
          add[Nlink]      orElse
          add[UnixPerms]  orElse
          add[Node]       orElse
          { case (o, _) => o }
        }
      }

    private implicit class JsValueOps(json: JsValue) {
      def optionally[A : Reads] = json.validateOpt[A] orElse JsSuccess(None)
    }

    private def add[A: ClassTag](implicit z: OFormat[A]): ((JsObject, Attribute)) =?> JsObject = {
      case (o, Attribute(a: A)) => o ++ (z writes a)
    }
  }

  private def Format[A](read: JsValue => JsResult[A], write: A => JsValue): json.Format[A] = new json.Format[A] {
    def reads(json: JsValue) = read(json)
    def writes(a: A)         = write(a)
  }

  private def OFormat[A](read: JsValue => JsResult[A], write: A => JsObject) = json.OFormat(read, write)

  implicit val size = OFormat[Size](
    json => (json \ "size").validate[Long] map Size,
    size => obj("size" -> size.bytes)
  )

  implicit val fileTime = Format[FileTime](
    json => json.validate[Long] map FileTime.millis,
    time => JsNumber(time.inMillis)
  )
  def fileTimeBased[A <: FileTimeBased[A]](create: FileTime => A): OFormat[A] =
    OFormat[A](
      json => (json \ "atime").validate[FileTime] map create,
      time => obj("atime" -> time.timestamp)
    )
  implicit val atime = fileTimeBased(Atime)
  implicit val ctime = fileTimeBased(Ctime)
  implicit val mtime = fileTimeBased(Mtime)
  implicit val birth = fileTimeBased(Birth)

  implicit val uid = OFormat[Uid](
    json => (json \ "uid").validate[Int] map Uid,
    uid  => obj("uid" -> uid.value)
  )
  implicit val gid = OFormat[Gid](
    json => (json \ "gid").validate[Int] map Gid,
    gid  => obj("gid" -> gid.value)
  )

  implicit val blockCount = OFormat[BlockCount](
    json       => (json \ "blockCount").validate[Long] map BlockCount,
    blockCount => obj("blockCount" -> blockCount.amount)
  )

  implicit val nlink = OFormat[Nlink](
    json  => (json \ "nlink").validate[Int] map Nlink,
    nlink => obj("nlink" -> nlink.count)
  )

  implicit val node = OFormat[Node](
    json  =>
      for {
        node   <- (json \ "node").validate[JsObject]
        `type` <- (node \ "type").validate[String]
        result <- `type` match {
          case "file" => (node \ "data"  ).validate[Data]      map (File(_))
          case "dir"  => (node \ "kids"  ).validate[Set[Name]] map (Dir(_))
          case "link" => (node \ "target").validate[String]    map (Link(_))
        }
      } yield result,
    node =>
      obj("node" -> {
        node match {
          case File(data)   => obj("type" -> "file", "data"   -> data.get)
          case Dir(kids)    => obj("type" -> "dir" , "kids"   -> kids)
          case Link(target) => obj("type" -> "link", "target" -> target)
          case NoNode       => JsNull
        }
      })
  )

  implicit val perms = OFormat[UnixPerms](
    json  => (json \ "perms").validate[Long] map UnixPerms,
    perms => obj("perms" -> perms.mask)
  )
}
