object Test extends App {
  //val stream = System.in
  val stream = EOLConversionStream.stdin
  var c = stream.read()
  while (c > 0) {
    println(c)
    c = stream.read()
  }
}
