class AI(private var player: Player, private var depth: Int) extends Solver {

  override def getMoves(b: Board): Array[Move] = {
    
    
    // Create Game State from Player and Depth
    
    val opponent = Player.getOther(player)
    val state = new State(opponent, b, new Move(opponent, b.getLastCol))
    AI.createGameTree(state, 1 /* depth */)
    
    
    // TODO use miniMax to evaluate State Tree
    // TODO Return array of best moves (hard coded) 
    
    val bestMoves = new Array[Move](1)
    bestMoves(0) = new Move(YELLOW, 5)
    
    bestMoves
  }

  def minimax(s: State) {

    // Run through the tree from the bottom up 
    // to find which moves are best 
    
  }

  def evaluateBoard(b: Board): Int = {
    
    val winner = b.hasConnectFour()
    var value = 0
    if (!winner.isDefined) {
      val locs = b.winLocations()
      for (loc <- locs; p <- loc) {
        value += (if (p == player) 1 else if (p != null) -1 else 0)
      }
    } else {
      var numEmpty = 0
      var r = 0
      while (r < Board.NUM_ROWS) {
        var c = 0
        while (c < Board.NUM_COLS) {
          if (b.getTile(r, c) == null) numEmpty += 1
          c = c + 1
        }
        r = r + 1
      }
      value = (if (winner.get == player) 1 else -1) * 10000 * numEmpty
    }
    value
  }
  
}

object AI {

//    def createGameTreeHelper(s: State, d: Int, n: Node): Node = {
//
//    // s.writeToFile
//
//    if (d != 0) {
//      println("In createGameTree Depth is " + d.toString)
//      val stateList: Array[State] = s.initializeChildren()
//      stateList.map(state => AI.createGameTreeHelper(state, d - 1))
//    } else {
//      println("we're at the bottom")
//      
//    }
//
//  }
//
//  def createGameTree(s: State, d: Int): Unit = {
//
//    var nodeTree = new Node(d, s.lastMove.column)
//    // reset array list of tree nodes 
//    createGameTreeHelper(s, d)
//
//  }
  
  def createGameTreeHelper(s: State, d: Int): Unit = {

    s.writeToFile

    if (d != 0) {
      println("In createGameTree Depth is " + d.toString)
      val stateList: Array[State] = s.initializeChildren()
      stateList.map(state => AI.createGameTreeHelper(state, d - 1))
    } else {
      println("we're at the bottom")
      
    }

  }

  def createGameTree(s: State, d: Int): Unit = {

    var nodeTree = new Node(d, s.lastMove.column)
    // reset array list of tree nodes 
    createGameTreeHelper(s, d)

  }

  def minimax(ai: AI, s: State) {
    ai.minimax(s)
  }
}

