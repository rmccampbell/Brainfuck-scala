import cats.data.Kleisli
import cats.effect.IO

import scala.annotation.tailrec
import scala.io.Source

case class Brainfuck4(leftTape: List[Byte] = Nil,
                      current: Byte = 0,
                      rightTape: Stream[Byte] = Stream.continually(0)) {
  import Brainfuck4.BrainfuckIOFunc

  def right: Brainfuck4 = copy(leftTape = current :: leftTape,
                               current = rightTape.head,
                               rightTape = rightTape.tail)

  def left: Brainfuck4 = copy(leftTape = leftTape.tail,
                              current = leftTape.head,
                              rightTape = current #:: rightTape)

  def plus: Brainfuck4 = copy(current = (current + 1).toByte)

  def minus: Brainfuck4 = copy(current = (current - 1).toByte)

  def write: IO[Brainfuck4] = for {
    _ <- IO {System.out.write(current); System.out.flush()}
  } yield this

  def read: IO[Brainfuck4] = for {
    c <- IO {EOLConversionStream.stdin.read()}
  } yield copy(current = (c max 0).toByte)

  def debug: IO[Brainfuck4] = for {
    _ <- IO {println(s"${leftTape.reverse} $current $rightTape")}
  } yield this

  def loop(func: BrainfuckIOFunc): IO[Brainfuck4] = {
    if (current != 0) func(this).flatMap(_.loop(func))
    else IO.pure(this)
  }
}

object Brainfuck4 {
  type BrainfuckIOFunc = Kleisli[IO, Brainfuck4, Brainfuck4]

  def parse(code: Seq[Char]): BrainfuckIOFunc = {
    def parseBlock(code: List[Char], isBlock: Boolean = false):
        (BrainfuckIOFunc, List[Char]) = {
      @tailrec
      def parseRest(code: List[Char], prog: BrainfuckIOFunc = Kleisli(IO.pure)):
          (BrainfuckIOFunc, List[Char]) = {
        code match {
          case Nil =>
            if (isBlock) throw new IllegalArgumentException("missing close bracket")
            (prog, Nil)
          case head :: tail => head match {
            case '>' => parseRest(tail, prog.map(_.right))
            case '<' => parseRest(tail, prog.map(_.left))
            case '+' => parseRest(tail, prog.map(_.plus))
            case '-' => parseRest(tail, prog.map(_.minus))
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

  def run(code: Seq[Char]): IO[Brainfuck4] = parse(code)(Brainfuck4())

  def unsafeRun(code: Seq[Char]): Brainfuck4 = run(code).unsafeRunSync()

  def main(args: Array[String]): Unit = {
    val Debug = false

    val input = args.lift(0) match {
      case None | Some("-") => Source.stdin
      case Some(fileName) => Source.fromFile(fileName)
    }
    val valid = "><+-.,[]" ++ (if (Debug) "?" else "")
    val code = input.filter(valid.toSet).toList
    Brainfuck4.unsafeRun(code)
  }
}
