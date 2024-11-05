import scala.annotation.tailrec
import scala.io.Source

case class Brainfuck3(leftTape: List[Byte] = Nil,
                      current: Byte = 0,
                      rightTape: Stream[Byte] = Stream.continually(0)) {
  import Brainfuck3.BrainfuckFunc

  def right: Brainfuck3 = copy(leftTape = current :: leftTape,
                               current = rightTape.head,
                               rightTape = rightTape.tail)

  def left: Brainfuck3 = copy(leftTape = leftTape.tail,
                              current = leftTape.head,
                              rightTape = current #:: rightTape)

  def plus: Brainfuck3 = copy(current = (current + 1).toByte)

  def minus: Brainfuck3 = copy(current = (current - 1).toByte)

  def write: Brainfuck3 = {
    System.out.write(current)
    System.out.flush()
    this
  }

  def read: Brainfuck3 = {
    val c = EOLConversionStream.stdin.read()
    copy(current = (c max 0).toByte)
  }

  def debug: Brainfuck3 = {
    println(s"${leftTape.reverse} $current $rightTape")
    this
  }

  @tailrec
  final def loop(func: BrainfuckFunc): Brainfuck3 = {
    if (current != 0) func(this).loop(func)
    else this
  }
}

object Brainfuck3 {
  type BrainfuckFunc = Brainfuck3 => Brainfuck3

  def parse(code: Seq[Char]): BrainfuckFunc = {
    def parseBlock(code: List[Char], isBlock: Boolean = false):
        (BrainfuckFunc, List[Char]) = {
      @tailrec
      def parseRest(code: List[Char], prog: BrainfuckFunc = identity):
          (BrainfuckFunc, List[Char]) = {
        code match {
          case Nil =>
            if (isBlock) throw new IllegalArgumentException("missing close bracket")
            (prog, Nil)
          case head :: tail => head match {
            case '>' => parseRest(tail, prog.andThen(_.right))
            case '<' => parseRest(tail, prog.andThen(_.left))
            case '+' => parseRest(tail, prog.andThen(_.plus))
            case '-' => parseRest(tail, prog.andThen(_.minus))
            case '.' => parseRest(tail, prog.andThen(_.write))
            case ',' => parseRest(tail, prog.andThen(_.read))
            case '?' => parseRest(tail, prog.andThen(_.debug))
            case '[' =>
              val (block, tail2) = parseBlock(tail, isBlock = true)
              parseRest(tail2, prog.andThen(_.loop(block)))
            case ']' =>
              if (!isBlock) throw new IllegalArgumentException("too many close brackets")
              (prog, tail)
            case _ => parseRest(tail, prog)
          }
        }
      }
      parseRest(code)
    }
    parseBlock(code.toList)._1
  }

  def run(code: Seq[Char]): Brainfuck3 = parse(code)(Brainfuck3())

  def main(args: Array[String]): Unit = {
    val Debug = false

    val input = args.lift(0) match {
      case None | Some("-") => Source.stdin
      case Some(fileName) => Source.fromFile(fileName)
    }
    val valid = "><+-.,[]" ++ (if (Debug) "?" else "")
    val code = input.filter(valid.toSet).toList
    Brainfuck3.run(code)
  }
}
