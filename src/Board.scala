import scala.collection.mutable.ListBuffer

class Board {

  private val FOUR = 4

  private val deltas = Array(Array(1, 0), Array(0, 1), Array(-1, 1), Array(1, 1))

  private val board = Array.ofDim[Player](Board.NUM_ROWS, Board.NUM_COLS)
  
  var move: Move = null
  private var lastCol: Int = 0
  
  def getLastCol : Int = {
    lastCol
  }
  
  def this(b: Board) {
    this()
    for (r <- 0 until Board.NUM_ROWS; c <- 0 until Board.NUM_COLS)
      board(r)(c) = b.board(r)(c)
  }

  def getPlayer(r: Int, c: Int): Player = {
    assert(0 <= r && r < Board.NUM_ROWS && 0 <= c && c < Board.NUM_COLS)
    board(r)(c)
  }

  def this(b: Board, nextMove: Move) {
    this(b)
    this.move = nextMove
    makeMove(nextMove)
  }

  def makeMove(move: Move): Unit = {
    println(" ****  In MakeMove move col is " + move.column)
    lastCol = move.column;
    var filled: Boolean = false;
    for (r <- 5.to(0, -1)) {
      if (getPlayer(r, move.column) == null && !filled) {
        board(r)(move.column) = move.player
        filled = true
      }
    }
  }

  def getTile(row: Int, col: Int): Player = board(row)(col)

  def getPossibleMoves(p: Player): Array[Move] = {

    var possibleMoves = new Array[Move](0)
    var filled: Boolean = false;

    for (c <- 0.to(6)) {
      if (getPlayer(0, c) == null) {
        possibleMoves +:= new Move(p, c)
        println("Added to array at poss " + c)

      }
    }

    possibleMoves

  }

  override def toString(): String = {
    toString("")
  }

  def toString(prefix: String): String = {
    val str = new StringBuilder("")
    for (row <- board) {
      str.append(prefix + "|")
      for (spot <- row) {
        if (spot == null) {
          str.append(" |")
        } else if (spot == RED) {
          str.append("R|")
        } else {
          str.append("Y|")
        }
      }
      str.append("\n")
    }
    str.toString
  }

  def hasConnectFour(): Option[Player] = {
    winLocations().find(loc => loc(0) != null && loc(0) == loc(1) && loc(0) == loc(2) &&
      loc(0) == loc(3))
      .map(_(0))
      .orElse(None)
  }

  def winLocations(): List[Array[Player]] = {
    val locations = ListBuffer[Array[Player]]()
    for (delta <- deltas; r <- 0 until Board.NUM_ROWS; c <- 0 until Board.NUM_COLS) {
      val loc = possibleWin(r, c, delta)
      if (loc != null) {
        locations += loc
      }
    }
    locations.toList
  }

  def possibleWin(r: Int, c: Int, delta: Array[Int]): Array[Player] = {
    val location = Array.ofDim[Player](FOUR)
    for (i <- 0 until FOUR) {
      val newR = r + i * delta(0)
      val newC = c + i * delta(1)
      if (0 <= newR && newR < Board.NUM_ROWS && 0 <= newC && newC < Board.NUM_COLS) {
        location(i) = board(newR)(newC)
      }
    }
    location
  }
}

object Board {
  val NUM_ROWS = 6
  val NUM_COLS = 7

  def apply(b: Board): Board =
    new Board(b)

  def apply(): Board =
    new Board()
}
