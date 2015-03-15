import java.io.{FileNotFoundException, PrintWriter, UnsupportedEncodingException}

import State.length0

import scala.beans.BeanProperty

class State(@BeanProperty var player: Player, @BeanProperty var board: Board, @BeanProperty var lastMove: Move)
  extends Comparable[Any] {

  @BeanProperty
  var children: Array[State] = length0

  @BeanProperty
  var value: Int = 0

  def initializeChildren() {
    
    // Call GetPossibleMoves within here 
    
    println("Called initializeChildren ")
    // TODO - refactor with Case Statement 
    var opponent: Player = RED
    if (player == RED) {
      opponent = YELLOW
    }

    val arrayOfMoves: Array[Move] = board.getPossibleMoves(opponent)
    val arrayOfStates: Array[State] = arrayOfMoves.map(move => makeState(opponent, move))
    
  }
  
  def makeBoard(move: Move): Board = {
    new Board(board, move)
  }
  
  def makeState  (player: Player, move: Move): State = {
 
    println("spawned new state with move  " + move.player + " col " + move.column)
    val b = makeBoard(move)
    new State(player, b, move)
  }
  
  def writeToFile() {
    // var writer: PrintWriter = _
    try {
       var writer = new PrintWriter("output.txt", "UTF-8")
      writer.println(this)
      writer.close
    } catch {
      case e@(_: FileNotFoundException | _: UnsupportedEncodingException) => e.printStackTrace()
    } // finally {
      // writer.close();
    // }
  }

  override def toString(): String = {
    println("State.toString printing")
    toStringHelper(0, "")
  }

  override def compareTo(o: Any): Int = ???

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

