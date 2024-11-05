import scala.annotation.tailrec
import scala.io.Source

case class Brainfuck1(code: IndexedSeq[Char],
                      iptr: Int = 0,
                      leftTape: List[Byte] = Nil,
                      current: Byte = 0,
                      rightTape: Stream[Byte] = Stream.continually(0)) {
  def advance(iptr: Int = iptr, leftTape: List[Byte] = leftTape,
              current: Byte = current, rightTape: Stream[Byte] = rightTape): Brainfuck1 =
    copy(iptr = iptr + 1, leftTape = leftTape, current = current, rightTape = rightTape)

  def right: Brainfuck1 = advance(leftTape = current :: leftTape,
                                  current = rightTape.head,
                                  rightTape = rightTape.tail)

  def left: Brainfuck1 = advance(leftTape = leftTape.tail,
                                 current = leftTape.head,
                                 rightTape = current #:: rightTape)

  def plus: Brainfuck1 = advance(current = (current + 1).toByte)

  def minus: Brainfuck1 = advance(current = (current - 1).toByte)

  def write: Brainfuck1 = {
    System.out.write(current)
    System.out.flush()
    advance()
  }

  def read: Brainfuck1 = {
    val c = EOLConversionStream.stdin.read()
    advance(current = (c max 0).toByte)
  }

  def debug: Brainfuck1 = {
    println(s"${leftTape.reverse} $current $rightTape")
    advance()
  }

  def lBracket: Brainfuck1 =
    advance(iptr = if (current == 0) findBracket(iptr, right = true) else iptr)

  def rBracket: Brainfuck1 =
    advance(iptr = if (current != 0) findBracket(iptr, right = false) else iptr)

  def findBracket(iptr: Int, right: Boolean): Int = {
    val dir = if (right) 1 else -1
    val (open, close) = if (right) ('[', ']') else (']', '[')

    @tailrec def loop(iptr: Int, depth: Int): Int = {
      val depth2 = code(iptr) match {
        case `open` => depth + 1
        case `close` => depth - 1
        case _ => depth
      }
      if (depth2 == 0) iptr
      else loop(iptr + dir, depth2)
    }
    loop(iptr + dir, 1)
  }

  def step: Brainfuck1 = code(iptr) match {
    case '>' => right
    case '<' => left
    case '+' => plus
    case '-' => minus
    case '.' => write
    case ',' => read
    case '[' => lBracket
    case ']' => rBracket
    case '?' => debug
    case _ => advance()
  }

  @tailrec
  final def run(): Brainfuck1 = {
    if (iptr < code.length) step.run() else this
  }
}

object Brainfuck1 extends App {
  val Debug = false

  val input = args.lift(0) match {
    case None | Some("-") => Source.stdin
    case Some(fileName) => Source.fromFile(fileName)
  }
  val valid = "><+-.,[]" ++ (if (Debug) "?" else "")
  val code = input.mkString.filter(valid.toSet)
  Brainfuck1(code).run()
}
