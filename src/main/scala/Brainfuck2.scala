import scala.annotation.tailrec
import scala.io.Source

sealed trait BrainfuckOp
case object Right extends BrainfuckOp
case object Left extends BrainfuckOp
case object Plus extends BrainfuckOp
case object Minus extends BrainfuckOp
case object Dot extends BrainfuckOp
case object Comma extends BrainfuckOp
case class LBracket(target: Int) extends BrainfuckOp
case class RBracket(target: Int) extends BrainfuckOp
case object Debug extends BrainfuckOp

case class Brainfuck2(code: IndexedSeq[BrainfuckOp],
                      iptr: Int = 0,
                      leftTape: List[Byte] = Nil,
                      current: Byte = 0,
                      rightTape: Stream[Byte] = Stream.continually(0)) {
  def advance(iptr: Int = iptr, leftTape: List[Byte] = leftTape,
              current: Byte = current, rightTape: Stream[Byte] = rightTape): Brainfuck2 =
    copy(iptr = iptr + 1, leftTape = leftTape, current = current, rightTape = rightTape)

  def right: Brainfuck2 = advance(leftTape = current :: leftTape,
                                 current = rightTape.head,
                                 rightTape = rightTape.tail)

  def left: Brainfuck2 = advance(leftTape = leftTape.tail,
                                current = leftTape.head,
                                rightTape = current #:: rightTape)

  def plus: Brainfuck2 = advance(current = (current + 1).toByte)

  def minus: Brainfuck2 = advance(current = (current - 1).toByte)

  def write: Brainfuck2 = {
    System.out.write(current)
    System.out.flush()
    advance()
  }

  def read: Brainfuck2 = {
    val c = EOLConversionStream.stdin.read()
    advance(current = (c max 0).toByte)
  }

  def debug: Brainfuck2 = {
    println(s"${leftTape.reverse} $current $rightTape")
    advance()
  }

  def lBracket(target: Int): Brainfuck2 =
    if (current == 0) copy(iptr = target) else advance()

  def rBracket(target: Int): Brainfuck2 =
    if (current != 0) copy(iptr = target) else advance()

  def step: Brainfuck2 = code(iptr) match {
    case Right => right
    case Left => left
    case Plus => plus
    case Minus => minus
    case Dot => write
    case Comma => read
    case LBracket(target) => lBracket(target)
    case RBracket(target) => rBracket(target)
    case Debug => debug
  }

  @tailrec
  final def run(): Brainfuck2 = {
    if (iptr < code.length) step.run() else this
  }
}

object Brainfuck2 {
  def apply(code: Seq[Char]): Brainfuck2 = Brainfuck2(parse(code).toVector)

  def parse(code: Seq[Char]): Seq[BrainfuckOp] = {
    def parseBlock(code: List[Char], startInd: Int = 0, isBlock: Boolean = false):
        (List[BrainfuckOp], Int, List[Char]) = {
      @tailrec
      def parse(code: List[Char], ind: Int, accum: List[BrainfuckOp] = Nil):
          (List[BrainfuckOp], Int, List[Char]) = {
        code match {
          case Nil =>
            if (isBlock) throw new IllegalArgumentException("missing close bracket")
            (accum, ind, Nil)
          case head :: tail => head match {
            case '>' => parse(tail, ind + 1, Right :: accum)
            case '<' => parse(tail, ind + 1, Left:: accum)
            case '+' => parse(tail, ind + 1, Plus :: accum)
            case '-' => parse(tail, ind + 1, Minus :: accum)
            case '.' => parse(tail, ind + 1, Dot :: accum)
            case ',' => parse(tail, ind + 1, Comma :: accum)
            case '?' => parse(tail, ind + 1, Debug :: accum)
            case '[' =>
              val (block, ind2, tail2) = parseBlock(tail, ind + 1, isBlock = true)
              parse(tail2, ind2, block ::: LBracket(ind2) :: accum)
            case ']' =>
              if (!isBlock) throw new IllegalArgumentException("too many close brackets")
              (RBracket(startInd) :: accum, ind + 1, tail)
            case _ => parse(tail, ind, accum)
          }
        }
      }
      parse(code, startInd)
    }
    parseBlock(code.toList)._1.reverse
  }

  def main(args: Array[String]): Unit = {
    val Debug = false

    val input = args.lift(0) match {
      case None | Some("-") => Source.stdin
      case Some(fileName) => Source.fromFile(fileName)
    }
    val valid = "><+-.,[]" ++ (if (Debug) "?" else "")
    val code = input.filter(valid.toSet).toList
    Brainfuck2(code).run()
  }
}
