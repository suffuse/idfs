package suffuse

// https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.7
//
// media-type     = type "/" subtype *( ";" parameter )
// type           = token
// subtype        = token
//
// Parameters MAY follow the type/subtype in the form of attribute/value pairs
// (as defined in section 3.6).
//
// The type, subtype, and parameter attribute names are case-insensitive.
// Parameter values might or might not be case-sensitive, depending on the
// semantics of the parameter name. Linear white space (LWS) MUST NOT be used
// between the type and subtype, nor between an attribute and its value. The
// presence or absence of a parameter might be significant to the processing of a
// media-type, depending on its definition within the media type registry.
//
// Note that some older HTTP applications do not recognize media type parameters.
// When sending data to older HTTP applications, implementations SHOULD only use
// media type parameters when they are required by that type/subtype definition.
//
// Media-type values are registered with the Internet Assigned Number Authority
// (IANA [19]). The media type registration process is outlined in RFC 1590 [17].
// Use of non-registered media types is discouraged.

final case class MediaType(`type`: String, subtype: String, attrs: Map[String, String]) {
  def charset = attrs.getOrElse("charset", "<none>")
  override def toString = s"%s/%s".format(`type`, subtype)
}
object MediaType {
  val NoMediaType = new MediaType("<none>", "<none>", Map())

  val Extract = """^([^/]+)/([^\s;]+)[;]\s*(.*)$""".r

  private def mkPair(s: String) = s indexOf '=' match {
    case n if n >= 0 => (s take n, s drop n + 1)
    case _           => (s, "")
  }
  private def mkMap(attrs: String) = (attrs split ";" map mkPair).toMap

  def apply(str: String): MediaType = str match {
    case Extract(tpe, subtpe, attrs) => new MediaType(tpe, subtpe, mkMap(attrs))
    case _                           => NoMediaType
  }
}
