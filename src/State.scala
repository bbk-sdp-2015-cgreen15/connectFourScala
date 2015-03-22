import java.io.{ FileNotFoundException, PrintWriter, UnsupportedEncodingException }
import State.length0
import scala.beans.BeanProperty

class State(@BeanProperty var player: Player,
            @BeanProperty var board: Board,
            @BeanProperty var lastMove: Move)
  extends Comparable[Any] {

  @BeanProperty
  var children: Array[State] = length0

  @BeanProperty
  var value: Int = 0

  def initializeChildren(): Array[State] = {

    // Call GetPossibleMoves within here 
    
    val opponent: Player = Player.getOther(player)
    val arrayOfMoves: Array[Move] = board.getPossibleMoves(opponent)
    val arrayOfStates: Array[State] = arrayOfMoves.map(move => makeState(opponent, move))
    
    arrayOfStates
  }

  def makeBoard(move: Move): Board = {
    new Board(board, move)
  }

  def makeState(player: Player, move: Move): State = {
    val b = makeBoard(move)
    new State(player, b, move)
  }

  def writeToFile(): Unit = {
    try {
      var writer = new PrintWriter("output.txt", "UTF-8")
      writer.println(this)
      writer.close
    } catch {
      case e @ (_: FileNotFoundException | _: UnsupportedEncodingException) => e.printStackTrace()
    }
  }

  override def toString(): String = {
    println("State.toString printing")
    toStringHelper(0, "")
  }

  override def compareTo(o: Any): Int = {
    0
  }

  private def toStringHelper(d: Int, ind: String): String = {
    var str = ind + player + " to play\n"
    str = str + ind + "Value: " + value + "\n"
    str = str + board.toString(ind) + "\n"
    if (children != null && children.length > 0) {
      str = str + ind + "Children at depth " + (d + 1) + ":\n" + ind +
        "----------------\n"
      for (s <- children) {
        str = str + s.toStringHelper(d + 1, ind + "   ")
      }
    }
    str
  }
}

object State {

  val length0 = Array[State]()
}

