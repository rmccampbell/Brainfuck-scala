import java.io.{InputStream, PushbackInputStream}

/**
 * InputStream that converts CR LF to LF
 */
class EOLConversionStream(in: InputStream) extends InputStream {
  val pb = new PushbackInputStream(in)

  override def read(): Int = {
    val b = pb.read()
    if (b == '\r') {
      val b2 = pb.read()
      if (b2 == '\n') return b2
      else pb.unread(b2)
    }
    b
  }

  override def close(): Unit = pb.close()
}

object EOLConversionStream {
  lazy val stdin = new EOLConversionStream(System.in)
}
