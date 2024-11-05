import cats._
import cats.data.Kleisli
import cats.effect.IO
import cats.implicits._

val code = "++++++++[>+++++++++++++>+<<-]>.---.+++++++..+++.>++."

Brainfuck1(code).run()

Brainfuck2(code).run()

Brainfuck3.run(code)

Brainfuck4.unsafeRun(code)
