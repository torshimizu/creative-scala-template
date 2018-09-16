import doodle.core._
import doodle.core.Image._
import doodle.syntax._
import doodle.jvm.Java2DFrame._
import doodle.backend.StandardInterpreter._

// To use this example, open the SBT console and type:
//
// Example.image.draw
object Example {
  val circ = circle(10)

  def above(count: Int): Image = {
    count match {
      case 1 => circ
      case n => above(n - 1) above circ
    }
  }

  def below(count: Int): Image = {
    count match {
      case 1 => circ
      case n => below(n - 1) below circ
    }
  }

  def left(count: Int): Image = {
    count match {
      case 1 => circ
      case n => left(n - 1) beside circ
    }
  }

  def right(count: Int): Image = {
    count match {
      case 1 => circ
      case n => circ beside right(n - 1)
    }
  }

  def cross(count: Int): Image = {
      below(count) below (above(count) above (left(count) beside circ beside right(count)))
  }

  def cross2(count: Int): Image = {
    circ beside (circ above cross(count-1) above circ) beside circ
  }
}

object Chessboard {
  val red = Image.square(10) fillColor Color.red
  val black = Image.square(10) fillColor Color.black

  val unit = (red beside black) above (black beside red)


  def chessboard(count: Int): Image = {
    count match {
      case 0 => unit
      case n => (chessboard(n - 1) beside chessboard(n - 1)) above (chessboard(n - 1) beside chessboard(n - 1))
    }
  }
}

object Stri {

  def triangle(width: Double, height: Double): Image = {
    Image.triangle(width, height) lineColor Color.magenta
  }

  def triSet(size: Double): Image = {
    triangle(size, size) above (triangle(size, size) beside triangle(size, size))
  }

  def fractal(count: Int): Image = {
    count match {
      case 0 => triSet(10.0)
      case n => fractal(n - 1) above (fractal(n - 1) beside fractal(n - 1))
    }
  }
}

object Box {
  def aBox(size: Int, deg: Int): Image = {
    Image.rectangle(size, size) fillColor Color.blue.spin(deg.degrees) lineColor Color.magenta.spin(deg.degrees)
  }

  def drawBoxes(num: Int): Image = {
    num match {
      case 0 => Image.empty
      case n => aBox(50, (5 - n) * 10) beside drawBoxes(n - 1)
    }
  }

}
